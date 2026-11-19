-- =====================================================================================================
-- MATR metadata database, script 5 of 8: PERMISSIONS
-- =====================================================================================================
-- Run after 4_functions.sql; can be run again at any time, and then run 8_search.sql again too, since this
-- takes back its grants. Three roles reach the database through the
-- REST API:
--   anon, authenticated   the public dashboard: reads the catalog but not what staff hid, upvotes,
--                         nothing else. A signed-in
--                         staff member (authenticated, with an email in staff) may also read the private
--                         tables.
--   service_role          the site's server: reads and writes every table and calls the functions. It
--                         skips Row Level Security, but still needs GRANTs.
-- Supabase grants all three everything on new tables and functions in public by default, so this takes all
-- of that away first and then grants only what is meant: each role can do what it needs and no more.

begin;

-- ------ Row Level Security on every table: with it on and no policy, a role sees no rows ------------
do $$
declare
  t text;
  p text;
begin
  for t in select c.relname from pg_class c where c.relnamespace = 'public'::regnamespace and c.relkind = 'r' loop
    execute format('alter table public.%I enable row level security', t);
  end loop;
  -- every policy goes, whatever it is called, so the ones below are the only ones
  for t, p in select tablename, policyname from pg_policies where schemaname = 'public' loop
    execute format('drop policy %I on public.%I', p, t);
  end loop;
end;
$$;
-- the staff check as it was before it moved to the private schema (4_functions.sql), now that no policy uses it
drop function if exists public.is_active_staff();

-- ------ Nothing for any of the three roles unless granted below ---------------------------------------
revoke all on all tables    in schema public from anon, authenticated, service_role;
revoke all on all sequences in schema public from anon, authenticated, service_role;
revoke all on all functions in schema public from public, anon, authenticated;
revoke all on schema private from public, anon, authenticated;
revoke all on all functions in schema private from public, anon, authenticated;
alter default privileges in schema public revoke all on tables    from anon, authenticated, service_role;
alter default privileges in schema public revoke all on sequences from anon, authenticated, service_role;
alter default privileges in schema public revoke execute on functions from anon, authenticated;

-- ------ The public dashboard reads the catalog, but not what staff hid ----------------------------------
-- Staff can hide a study, a questionnaire or a wave (its `hidden` column): it stays in this database and on the
-- site, but the dashboard doesn't get it, or anything that is there only because of it. Each table's policy
-- says which rows the dashboard gets. A policy that looks in another table sees only the rows that table's own
-- policy lets through, so hiding a study hides its questionnaires, their waves, questions and variables:
--   study                    not hidden
--   questionnaire            not hidden, and its study is shown
--   wave                     not hidden, and its questionnaire is shown
--   placement                its questionnaire is shown, and it isn't a question that only hidden waves asked
--                            (sections, grids, notes and the like are in no wave, and go with their questionnaire)
--   component                a shown placement prints it (one wording can be printed in several questionnaires)
--   wave_question            its wave is shown
--   variable, placement_tag  its placement is shown
--   response_option, variable_tag, response_option_tag
--                            its variable is shown
--   question_scale           its component is shown
--   study_publication        its study is shown
--   registry, principal_investigator, publication, respondent_type, answer_option, tag, measurement_scale
--                            every row: lists every study shares, which the dashboard shows only beside a
--                            study or question it gets
-- The site's server (service_role) skips policies, so staff still see everything. question_embedding's policy
-- is in 8_search.sql; pdf_file's, below, looks in wave, so a hidden wave's PDFs aren't listed either.

-- The questions that only hidden waves asked. placement's policy needs these, and can't find them itself: it
-- sees only shown waves, so a question only hidden waves asked would look like a section no wave asks. This
-- runs as its owner (postgres), which skips policies, so it sees every wave.
create or replace function private.asked_only_in_hidden_waves()
returns setof bigint
language sql
stable
security definer
set search_path = ''
as $$
  select wq.placement_id
  from public.wave_question wq
  join public.wave w on w.wave_id = wq.wave_id
  group by wq.placement_id
  having bool_and(w.hidden);
$$;
revoke all on function private.asked_only_in_hidden_waves() from public, anon, authenticated;
grant execute on function private.asked_only_in_hidden_waves() to anon, authenticated;
grant usage on schema private to anon; -- to run that function (authenticated has it below, for the staff check)

do $$
declare
  t text;
  shown text;
begin
  for t, shown in
    select * from (values
      ('study',                  'not hidden'),
      ('questionnaire',          'not hidden and study_id in (select study_id from public.study)'),
      ('wave',                   'not hidden and questionnaire_id in (select questionnaire_id from public.questionnaire)'),
      ('placement',              'questionnaire_id in (select questionnaire_id from public.questionnaire)
                                  and placement_id not in (select private.asked_only_in_hidden_waves())'),
      ('component',              'component_id in (select component_id from public.placement)'),
      ('wave_question',          'wave_id in (select wave_id from public.wave)'),
      ('variable',               'placement_id in (select placement_id from public.placement)'),
      ('placement_tag',          'placement_id in (select placement_id from public.placement)'),
      ('response_option',        'variable_key in (select variable_key from public.variable)'),
      ('variable_tag',           'variable_key in (select variable_key from public.variable)'),
      ('response_option_tag',    'variable_key in (select variable_key from public.variable)'),
      ('question_scale',         'component_id in (select component_id from public.component)'),
      ('study_publication',      'study_id in (select study_id from public.study)'),
      ('registry',               'true'),
      ('principal_investigator', 'true'),
      ('publication',            'true'),
      ('respondent_type',        'true'),
      ('answer_option',          'true'),
      ('tag',                    'true'),
      ('measurement_scale',      'true')
    ) as rules (table_name, shown_when)
  loop
    execute format('create policy "Dashboard can read" on public.%I for select to anon, authenticated using (%s)', t, shown);
    execute format('grant select on public.%I to anon, authenticated', t);
  end loop;
end;
$$;

-- ...and the PDFs a wave was extracted from, but no others: the bucket is public, so a file is safe from
-- anyone without its SHA-256 only while nothing lists it. An extraction's PDF stays unlisted until pushed.
-- (Someone signed in has one policy of their own on this table, below: a table read through two policies
-- has both worked out for every row.)
create policy "Dashboard can read a wave's files" on public.pdf_file for select to anon
  using (exists (select 1 from public.wave w where w.pdf_sha256 = pdf_file.pdf_sha256 or w.supplement_sha256 = pdf_file.pdf_sha256));
grant select on public.pdf_file to anon, authenticated;

grant execute on function public.upvote_questionnaire(bigint) to anon, authenticated, service_role;
grant execute on function public.unstar_questionnaire(bigint) to anon, authenticated, service_role;

-- ------ Staff who sign in may read the private tables (the site itself uses service_role) -------------
-- The policies ask private.is_active_staff(), so a signed-in person may use that schema and run that
-- function (and the dashboard's private.asked_only_in_hidden_waves(), above), and nothing else in it. "(select ...)" has it worked out once for a query, not once for each row.
grant usage on schema private to authenticated;
grant execute on function private.is_active_staff() to authenticated;
do $$
declare
  t text;
begin
  foreach t in array array['staff', 'extraction', 'history'] loop
    execute format('grant select on public.%I to authenticated', t);
    execute format('create policy "Staff can read" on public.%I for select to authenticated using ((select private.is_active_staff()))', t);
  end loop;
end;
$$;
-- every PDF for staff; for anyone else signed in, a wave's files, as the dashboard gets
create policy "Staff can read, others a wave's files" on public.pdf_file for select to authenticated
  using (
    (select private.is_active_staff())
    or exists (select 1 from public.wave w where w.pdf_sha256 = pdf_file.pdf_sha256 or w.supplement_sha256 = pdf_file.pdf_sha256)
  );

-- ------ The site's server ------------------------------------------------------------------------------
grant usage on schema private to service_role;
grant select, insert, update, delete on all tables    in schema public to service_role;
grant usage, select                  on all sequences in schema public to service_role;
alter default privileges in schema public grant select, insert, update, delete on tables    to service_role;
alter default privileges in schema public grant usage, select                  on sequences to service_role;

-- The functions the site calls through /rpc/, and the helpers they run as the caller
grant execute on function public.push_extraction(bigint, bigint, boolean) to service_role;
grant execute on function public.apply_change(bigint, jsonb, text)        to service_role;
grant execute on function public.empty_wave(bigint, bigint)               to service_role;
grant execute on function public.reopen_wave(bigint, bigint)              to service_role;
grant execute on function public.empty_questionnaire(bigint, bigint)      to service_role;
grant execute on function public.empty_study(bigint, bigint)              to service_role;
grant execute on function public.empty_registry(bigint, bigint)           to service_role;
grant execute on all functions in schema private to service_role;

-- PostgreSQL itself still lets everyone (PUBLIC) run a new function, and that default can only be changed
-- for every schema at once, so each public function the dashboard mustn't call is revoked by name here.
revoke all on function public.push_extraction(bigint, bigint, boolean) from public, anon, authenticated;
revoke all on function public.apply_change(bigint, jsonb, text)        from public, anon, authenticated;
revoke all on function public.empty_wave(bigint, bigint)               from public, anon, authenticated;
revoke all on function public.reopen_wave(bigint, bigint)              from public, anon, authenticated;
revoke all on function public.empty_questionnaire(bigint, bigint)      from public, anon, authenticated;
revoke all on function public.empty_study(bigint, bigint)              from public, anon, authenticated;
revoke all on function public.empty_registry(bigint, bigint)           from public, anon, authenticated;

-- ------ The PDFs' bucket in Storage ----------------------------------------------------------------------
-- Extraction PDFs (and PDF files of Variable Names and Coding) are kept in Storage as <sha256>.pdf
-- (R/model_storage.R). The bucket is public: anyone can download a file by its address,
-- <project URL>/storage/v1/object/public/extraction-pdfs/<sha256>.pdf, so the dashboard can link a wave's PDF
-- (wave.pdf_sha256 names it; a draft's stays unlisted). Only the site's server writes to it, with the secret
-- key: no policy on storage.objects lets anyone else add, change or remove a file. Up to 50 MB, as the site
-- accepts (shiny.maxRequestSize in global.R), and only PDFs. A project that has the bucket keeps its files.
insert into storage.buckets (id, name, public, file_size_limit, allowed_mime_types)
values ('extraction-pdfs', 'extraction-pdfs', true, 50 * 1024 * 1024, array['application/pdf'])
on conflict (id) do update set public = true, file_size_limit = excluded.file_size_limit, allowed_mime_types = excluded.allowed_mime_types;

commit;

-- the REST API learns the tables and functions
notify pgrst, 'reload schema';
