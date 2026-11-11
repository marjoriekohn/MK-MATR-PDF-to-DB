-- =====================================================================================================
-- MATR metadata database, script 5 of 8: PERMISSIONS
-- =====================================================================================================
-- Run after 4_functions.sql; can be run again at any time, and then run 8_search.sql again too, since this
-- takes back its grants. Three roles reach the database through the
-- REST API:
--   anon, authenticated   the public dashboard: reads the catalog, upvotes, nothing else. A signed-in
--                         staff member (authenticated, with an email in staff) may also read the private
--                         tables.
--   service_role          the site's server: reads and writes every table and calls the functions. It
--                         skips Row Level Security, but still needs GRANTs.
-- Supabase grants anon and authenticated everything on new tables and functions in public by default, so
-- this takes all of that away first and then grants only what is meant.

begin;

-- ------ Row Level Security on every table: with it on and no policy, a role sees no rows ------------
do $$
declare
  t text;
begin
  for t in select c.relname from pg_class c where c.relnamespace = 'public'::regnamespace and c.relkind = 'r' loop
    execute format('alter table public.%I enable row level security', t);
    execute format('drop policy if exists "Dashboard can read" on public.%I', t);
    execute format('drop policy if exists "Dashboard can read a wave''s files" on public.%I', t);
    execute format('drop policy if exists "Staff can read" on public.%I', t);
  end loop;
end;
$$;

-- ------ Nothing for the dashboard roles unless granted below ------------------------------------------
revoke all on all tables    in schema public from anon, authenticated;
revoke all on all sequences in schema public from anon, authenticated;
revoke all on all functions in schema public from public, anon, authenticated;
revoke all on schema private from public, anon, authenticated;
revoke all on all functions in schema private from public, anon, authenticated;
alter default privileges in schema public revoke all on tables    from anon, authenticated;
alter default privileges in schema public revoke all on sequences from anon, authenticated;
alter default privileges in schema public revoke execute on functions from anon, authenticated;

-- ------ The public dashboard reads the catalog ---------------------------------------------------------
do $$
declare
  t text;
  catalog constant text[] := array[
    'registry', 'principal_investigator', 'study', 'respondent_type', 'questionnaire', 'wave',
    'component', 'placement', 'wave_question', 'variable', 'answer_option', 'response_option',
    'tag', 'placement_tag', 'variable_tag', 'response_option_tag', 'measurement_scale', 'question_scale'
  ];
begin
  foreach t in array catalog loop
    execute format('create policy "Dashboard can read" on public.%I for select to anon, authenticated using (true)', t);
    execute format('grant select on public.%I to anon, authenticated', t);
  end loop;
end;
$$;

-- ...and the PDFs a wave was extracted from, but no others: the bucket is public, so a file is safe from
-- anyone without its SHA-256 only while nothing lists it. An extraction's PDF stays unlisted until pushed.
create policy "Dashboard can read a wave's files" on public.pdf_file for select to anon, authenticated
  using (exists (select 1 from public.wave w where w.pdf_sha256 = pdf_file.pdf_sha256 or w.supplement_sha256 = pdf_file.pdf_sha256));
grant select on public.pdf_file to anon, authenticated;

grant execute on function public.upvote_questionnaire(bigint) to anon, authenticated, service_role;

-- ------ Staff who sign in may read the private tables (the site itself uses service_role) -------------
revoke all on function public.is_active_staff() from public, anon;
grant execute on function public.is_active_staff() to authenticated, service_role;
do $$
declare
  t text;
begin
  foreach t in array array['staff', 'extraction', 'history'] loop
    execute format('grant select on public.%I to authenticated', t);
    execute format('create policy "Staff can read" on public.%I for select to authenticated using (public.is_active_staff())', t);
  end loop;
end;
$$;
create policy "Staff can read" on public.pdf_file for select to authenticated using (public.is_active_staff());

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
grant execute on function public.empty_questionnaire(bigint, bigint)      to service_role;
grant execute on function public.empty_study(bigint, bigint)              to service_role;
grant execute on function public.empty_registry(bigint, bigint)           to service_role;
grant execute on all functions in schema private to service_role;

-- PostgreSQL itself still lets everyone (PUBLIC) run a new function, and that default can only be changed
-- for every schema at once, so each public function the dashboard mustn't call is revoked by name here.
revoke all on function public.push_extraction(bigint, bigint, boolean) from public, anon, authenticated;
revoke all on function public.apply_change(bigint, jsonb, text)        from public, anon, authenticated;
revoke all on function public.empty_wave(bigint, bigint)               from public, anon, authenticated;
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
