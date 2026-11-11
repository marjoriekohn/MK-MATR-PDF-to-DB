-- =====================================================================================================
-- MATR metadata database, script 8 of 8: SEARCH
-- =====================================================================================================
-- Run after 7_seed.sql, and again whenever 5_permissions.sql is run (that takes back every grant). Can be
-- run again at any time, on a new database or one in use: it changes no catalog rows.
--
-- The dashboard's Questions Explorer finds questions by meaning as well as by their words. Every Question has
-- an embedding: 384 numbers from the gte-small model, made from its words (after its grid's lead-in, for a
-- row of a grid), the question it depends on, and its section's heading, so questions about the same thing
-- lie close together. A search is embedded the same way, and the closest
-- questions are the matches.
--
--   public.question_embedding   one row per Question: the text that is embedded, and its embedding (empty
--                               until made, and emptied when the text changes)
--   triggers on component and placement
--                               after any change, note that the texts may be out of date
--   cron job embed-questions    every 10 seconds: brings the texts up to date, and asks the embed-questions Edge
--                               Function to fill the empty embeddings
--   public.match_questions()    the questions closest to a search's embedding; the search-questions Edge
--                               Function calls it, and the dashboard calls that
--
-- The Edge Functions are in supabase/functions/. On a new project, before running this:
--   1. deploy them:  supabase functions deploy --project-ref <ref>   (supabase/config.toml turns off the JWT
--      check: the functions check the key themselves)
--   2. tell the database where the project is, once:
--        select vault.create_secret('https://<ref>.supabase.co', 'project_url');
-- =====================================================================================================

begin;

create extension if not exists vector with schema extensions;
create extension if not exists pg_net with schema extensions;
create extension if not exists pg_cron;

-- ------ Before semantic search -----------------------------------------------------------------------
-- Questions were grouped by component.normal_text, the words left after stripping "what is your" and the
-- like. Each wording is already one component, and the stripping merged questions that differ ("what is
-- your age?", "what was your age?"), so the dashboard groups by component and finds by meaning instead. A
-- database built before then loses the column here.
alter table public.component drop column if exists normal_text;
drop function if exists private.normal_question(text);

-- ------ The embeddings ---------------------------------------------------------------------------------
create table if not exists public.question_embedding (
  component_id bigint primary key references public.component (component_id) on delete cascade,
  content      text   not null constraint question_embedding_content_not_blank check (btrim(content) <> ''),
  embedding    extensions.vector(384),
  embedded_at  timestamptz -- when the embedding was made: how the cron job sees its calls get somewhere
);
alter table public.question_embedding add column if not exists embedded_at timestamptz; -- a database from before it
create index if not exists question_embedding_embedding_idx
  on public.question_embedding using hnsw (embedding extensions.vector_cosine_ops);

-- One row while the texts may be out of date: the catalog changed since they were last made.
create table if not exists private.question_embedding_stale (
  stale boolean primary key default true constraint question_embedding_stale_one_row check (stale)
);

-- One row: when the cron job last called embed-questions, and how many calls in a row have filled nothing.
create table if not exists private.question_embedding_calls (
  one                    boolean     primary key default true constraint question_embedding_calls_one_row check (one),
  last_called_at         timestamptz,
  calls_without_progress integer     not null default 0
);
insert into private.question_embedding_calls default values on conflict do nothing;

-- The token the database sends the embed-questions Edge Function, which checks it against this secret. Made
-- once per project; nothing outside the database needs to know it.
do $$
begin
  if not exists (select 1 from vault.secrets where name = 'embed_questions_token') then
    perform vault.create_secret(encode(extensions.gen_random_bytes(32), 'hex'), 'embed_questions_token',
                                'Lets the database call the embed-questions Edge Function');
  end if;
end;
$$;

-- ------ What is embedded for each question --------------------------------------------------------------
-- Its words (after a grid's lead-in, as printed), then the question it depends on ("when did you start?"
-- after "do you smoke?"), then its section's heading: the most particular first. For a question printed in
-- several places, each once, sorted, so the same catalog always gives the same text. No labels such as
-- "Section:": a search never has them, and words every text shares only make the texts look alike.
-- Instructions, notes and variables' labels are left out for the same reason: shared by a whole grid or
-- section, they outweighed the question's own words. Cut at 2,000 characters: gte-small reads 512 tokens.
create or replace function private.question_embedding_texts()
returns table (component_id bigint, content text)
language sql
stable
set search_path = ''
as $$
  with recursive
  -- each place a question is printed (origin), and each grid or group holding it, up to its section
  climb as (
    select p.component_id as question_id, p.placement_id as origin, p.parent_placement_id, 0 as depth
    from public.placement p
    join public.component c on c.component_id = p.component_id
    where c.kind = 'Question'
    union all
    select climb.question_id, climb.origin, holder.parent_placement_id, climb.depth + 1
    from climb
    join public.placement holder on holder.placement_id = climb.parent_placement_id
    join public.component hc on hc.component_id = holder.component_id
    where hc.kind in ('Grid', 'Group')
  ),
  -- what holds each step of the climb
  holder as (
    select climb.*, hc.kind as holder_kind, hc.text as holder_text
    from climb
    join public.placement h on h.placement_id = climb.parent_placement_id
    join public.component hc on hc.component_id = h.component_id
  ),
  wording as (
    select climb.question_id,
           string_agg(distinct concat_ws(' ', grid.holder_text, q.text), '. '
                      order by concat_ws(' ', grid.holder_text, q.text)) as words
    from climb
    join public.component q on q.component_id = climb.question_id
    left join holder grid on grid.origin = climb.origin and grid.depth = 0 and grid.holder_kind = 'Grid'
    where climb.depth = 0
    group by climb.question_id
  ),
  parents as (
    select climb.question_id, string_agg(distinct pc.text, '. ' order by pc.text) as words
    from climb
    join public.placement here on here.placement_id = climb.origin
    join public.placement pp on pp.placement_id = here.depends_on_placement_id
    join public.component pc on pc.component_id = pp.component_id
    where climb.depth = 0
    group by climb.question_id
  ),
  sections as (
    select question_id, string_agg(distinct holder_text, '. ' order by holder_text) as words
    from holder
    where holder_kind = 'Section'
    group by question_id
  )
  select q.component_id,
         left(concat_ws('. ', coalesce(wording.words, q.text), parents.words, sections.words), 2000)
  from public.component q
  left join wording  on wording.question_id  = q.component_id
  left join parents  on parents.question_id  = q.component_id
  left join sections on sections.question_id = q.component_id
  where q.kind = 'Question';
$$;

-- Bring the texts up to date: one row per Question, and a text that changed loses its embedding.
create or replace function private.refresh_question_embedding_texts()
returns void
language sql
set search_path = ''
as $$
  delete from public.question_embedding e
  where not exists (select 1 from public.component c where c.component_id = e.component_id and c.kind = 'Question');

  insert into public.question_embedding as e (component_id, content)
  select component_id, content from private.question_embedding_texts()
  on conflict (component_id) do update set content = excluded.content, embedding = null, embedded_at = null
  where e.content is distinct from excluded.content;
$$;

-- ------ Keeping them up to date ----------------------------------------------------------------------------
-- After any change to what the texts are made of. Once per statement, and only a note: a push writes many
-- rows, and the texts are made once, by the cron job.
create or replace function private.question_embeddings_out_of_date()
returns trigger
language plpgsql
security definer -- the site's role writes the catalog but not private tables; a trigger can't be called directly
set search_path = ''
as $$
begin
  insert into private.question_embedding_stale default values on conflict do nothing;
  return null;
end;
$$;

drop trigger if exists question_embeddings_out_of_date on public.component;
create trigger question_embeddings_out_of_date
  after insert or update or delete or truncate on public.component
  for each statement execute function private.question_embeddings_out_of_date();
drop trigger if exists question_embeddings_out_of_date on public.placement;
create trigger question_embeddings_out_of_date
  after insert or update or delete or truncate on public.placement
  for each statement execute function private.question_embeddings_out_of_date();
-- (A database set up before the labels were left out has one on variable too.)
drop trigger if exists question_embeddings_out_of_date on public.variable;

-- Run by the cron job: make the texts again if the catalog changed, then, while any embedding is empty, ask
-- the embed-questions Edge Function to fill some (pg_net sends the call once this commits). A call embeds 4,
-- about all the CPU an Edge Function gets, so every 10 seconds fills about 24 a minute. One call at a time:
-- calls at once share a worker, and all of them stop when it runs out of CPU.
-- A call that fills nothing (the function down, cut off, the model out) leaves them empty, so a later run
-- asks again; after 6 such calls in a row (a minute) it asks only every 5 minutes, and every 10 seconds again
-- once a call fills one. Whether a call got anywhere is read from embedded_at, whatever went wrong.
-- Also keeps a day of the job's own run history.
create or replace function private.embed_questions()
returns void
language plpgsql
set search_path = ''
as $$
declare
  project_url text;
  token       text;
  last_call   private.question_embedding_calls;
  misses      integer;
begin
  delete from cron.job_run_details
  where jobid in (select jobid from cron.job where jobname = 'embed-questions') and end_time < now() - interval '1 day';

  if exists (select 1 from private.question_embedding_stale) then
    delete from private.question_embedding_stale;
    perform private.refresh_question_embedding_texts();
  end if;

  if not exists (select 1 from public.question_embedding where embedding is null) then
    update private.question_embedding_calls set calls_without_progress = 0 where calls_without_progress <> 0;
    return;
  end if;

  -- how many calls in a row have filled nothing, counting the last one (it has had 10 seconds or more)
  select * into last_call from private.question_embedding_calls;
  misses := case
    when last_call.last_called_at is null then 0
    when exists (select 1 from public.question_embedding where embedded_at >= last_call.last_called_at) then 0
    else last_call.calls_without_progress + 1
  end;
  if misses >= 6 and now() - last_call.last_called_at < interval '5 minutes' then
    return; -- backing off; misses isn't kept, so the same call is counted once
  end if;

  select decrypted_secret into project_url from vault.decrypted_secrets where name = 'project_url';
  select decrypted_secret into token from vault.decrypted_secrets where name = 'embed_questions_token';
  if project_url is null then
    raise warning 'Questions can''t be embedded: run select vault.create_secret(''https://<ref>.supabase.co'', ''project_url'');';
    return;
  end if;
  perform net.http_post(
    url := project_url || '/functions/v1/embed-questions',
    headers := jsonb_build_object('Content-Type', 'application/json', 'x-embed-token', token),
    body := '{}'::jsonb,
    timeout_milliseconds := 30000
  );
  update private.question_embedding_calls set last_called_at = now(), calls_without_progress = misses;
end;
$$;

select cron.unschedule(jobid) from cron.job where jobname = 'embed-questions';
select cron.schedule('embed-questions', '10 seconds', 'select private.embed_questions()');

-- ------ Searching -------------------------------------------------------------------------------------------
-- The questions at least match_threshold close (cosine similarity, -1 to 1) to a search's embedding, closest
-- first, at most match_count of them (and never more than 200).
create or replace function public.match_questions(
  query_embedding extensions.vector(384),
  match_threshold double precision default 0.85,
  match_count     integer default 50
)
returns table (component_id bigint, similarity double precision)
language sql
stable
security invoker
set search_path = ''
as $$
  select e.component_id, 1 - (e.embedding operator(extensions.<=>) query_embedding) as similarity
  from public.question_embedding e
  where e.embedding operator(extensions.<=>) query_embedding < 1 - match_threshold
  order by e.embedding operator(extensions.<=>) query_embedding
  limit least(match_count, 200);
$$;

-- ------ Permissions (as in 5_permissions.sql) -------------------------------------------------------------
-- The dashboard reads the embeddings like the rest of the catalog, and searches them; the site's server
-- reads them; only the database and the embed-questions function (as postgres) write them.
alter table public.question_embedding enable row level security;
drop policy if exists "Dashboard can read" on public.question_embedding;
create policy "Dashboard can read" on public.question_embedding for select to anon, authenticated using (true);
revoke all on public.question_embedding from anon, authenticated, service_role;
grant select on public.question_embedding to anon, authenticated, service_role;

revoke all on function public.match_questions(extensions.vector, double precision, integer) from public;
grant execute on function public.match_questions(extensions.vector, double precision, integer)
  to anon, authenticated, service_role;

revoke all on private.question_embedding_stale from public, anon, authenticated, service_role;
revoke all on private.question_embedding_calls from public, anon, authenticated, service_role;
revoke all on function private.question_embedding_texts() from public, anon, authenticated;
revoke all on function private.refresh_question_embedding_texts() from public, anon, authenticated;
revoke all on function private.embed_questions() from public, anon, authenticated;

-- ------ Descriptions (as in 6_descriptions.sql) ----------------------------------------------------------
comment on table public.question_embedding            is 'One row per Question: the text its embedding is made from (its words, after its grid''s lead-in; the question it depends on; its section''s heading), and the embedding. Kept up to date by the database (sql/8_search.sql); never written by hand.';
comment on column public.question_embedding.embedding is '384 numbers from the gte-small model (Supabase Edge Functions). Empty until the embed-questions function makes it, and emptied when the text changes.';
comment on column public.question_embedding.embedded_at is 'When the embed-questions function made the embedding; the cron job reads it to see whether its calls get anywhere.';
comment on function public.match_questions(extensions.vector, double precision, integer) is 'The questions closest in meaning to a search''s embedding (gte-small): for the search-questions Edge Function.';

-- ------ Make the texts for the questions already there -----------------------------------------------------
insert into private.question_embedding_stale default values on conflict do nothing;

commit;
