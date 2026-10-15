-- =====================================================================================================
-- MATR metadata database: remove a draft's "why" (public.draft.note)
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor.
--
-- Make an edit no longer asks "Why are you making this change?", the pop-ups that add a row from an
-- extraction no longer ask why, and an extraction's change request no longer carries a note for the
-- reviewer. All of them were kept in public.draft.note, which nothing else uses, so the column goes.
--
-- ORDER: deploy the site's new code FIRST, then run this. The old code reads and writes draft.note
-- (it selects it on every page that lists drafts), so running this before the new code is deployed
-- breaks those pages. The new code never mentions the column, so it works before and after this runs.
--
-- What depends on draft.note (checked against docs/sql_scripts.sql, 2026-10-14):
--   * private.draft_set_parent() lists 'note' among the columns a caller may set. It names the column
--     in a string, which PostgreSQL does not track as a dependency, so it is replaced below first.
--   * the column's comment (dropped with the column).
--   * no view, index, constraint, trigger, policy or other function refers to it. Step 0 checks again.
--   * the build checks at the end of docs/sql_scripts.sql ("Checks of the database build") pass
--     'note' to create_draft() and save_draft(); after this they are refused ("A draft's note can't be
--     set this way."). Remove 'note' from those calls in docs/sql_scripts.sql (the lines with
--     'note', 'Because' / 'note', 'why' / 'note', null, and the history check of h.note), and from
--     section 1's create table public.draft and section 6's comment on column public.draft.note.
--   * docs/database_tables.md lists the column; regenerate it with tests/database/describe_schema.R.
--
-- The notes already written are lost when the column goes. Step 1 keeps a copy, if wanted.
-- It is one transaction: if any step fails, nothing changes.
-- =====================================================================================================

begin;

-- ------ 0. Check nothing else depends on the column ----------------------------------------------------
-- Views and rules that use it would stop the drop below anyway (no CASCADE); this says which, first.
do $$
declare
  dependents text;
begin
  select string_agg(distinct c.relname, ', ') into dependents
  from pg_depend d
  join pg_rewrite r on r.oid = d.objid
  join pg_class c on c.oid = r.ev_class
  join pg_attribute a on a.attrelid = d.refobjid and a.attnum = d.refobjsubid
  where d.refobjid = 'public.draft'::regclass and a.attname = 'note';
  if dependents is not null then
    raise exception 'Views use public.draft.note (%). Change them first.', dependents;
  end if;

  -- Functions that mention the column by name in their code (other than draft_set_parent, replaced below)
  select string_agg(p.oid::regprocedure::text, ', ') into dependents
  from pg_proc p
  where p.pronamespace in ('public'::regnamespace, 'private'::regnamespace)
    and p.proname <> 'draft_set_parent'
    and p.prosrc ~ '([^_a-z]|^)note([^_a-z]|$)';
  if dependents is not null then
    raise notice 'These functions mention "note"; check they don''t mean draft.note: %', dependents;
  end if;
end;
$$;

-- ------ 1. (Optional) keep a copy of the notes already written ---------------------------------------
-- Uncomment to keep them in the private schema, which the REST API doesn't offer.
-- create table private.draft_note_archive as
--   select draft_id, note, now() as archived_at from public.draft where note is not null;

-- ------ 2. A draft's note can't be set any more ---------------------------------------------------------
-- As in docs/sql_scripts.sql, without 'note' among the columns a caller may set. CREATE OR REPLACE keeps
-- the function's owner and its grant to service_role.
create or replace function private.draft_set_parent(p_draft_id bigint, p_parent jsonb)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  allowed constant text[] := array[
    'status', 'summary', 'requested_reviewer', 'submitted_at',
    'sent_back_by', 'sent_back_at', 'sent_back_note'
  ];
  given   jsonb := coalesce(p_parent, '{}'::jsonb);
  names   text[];
  unknown text;
begin
  select array_agg(k order by k) into names from jsonb_object_keys(given) as k;
  if names is null then
    return;
  end if;
  select k into unknown from unnest(names) as k where k <> all (allowed) limit 1;
  if unknown is not null then
    raise exception 'A draft''s % can''t be set this way.', unknown;
  end if;
  -- Applied is only ever set by applying (apply_draft() and approve_draft())
  if given ->> 'status' = 'Applied' then
    raise exception 'A change is applied by applying it, not by saying it was.';
  end if;
  execute format(
    'update public.draft t set %s from jsonb_populate_record(null::public.draft, $2) r where t.draft_id = $1',
    (select string_agg(format('%1$I = r.%1$I', k), ', ') from unnest(names) k)
  ) using p_draft_id, given;
end;
$$;

-- ------ 3. The column goes -------------------------------------------------------------------------------
-- No CASCADE: if anything still depends on it, this stops and nothing changes. Dropping a column of an
-- applied draft is allowed: draft_guard() is a row trigger on UPDATE and DELETE, not on ALTER TABLE.
alter table public.draft drop column note;

-- ------ 4. Descriptions -----------------------------------------------------------------------------------
-- Where an extraction goes is now picked when its change request is submitted, and it can be a
-- recorded wave that asks no questions yet; "notes" is no longer kept.
comment on column public.extracted_draft.info is
  'Where the extraction goes, picked when its change request is submitted (empty until then): registry_id, study_id, kind ("new_questionnaire", or "new_wave" for a recorded questionnaire), questionnaire_id, name, abbreviation, respondent_type_id, description, wave_choice ("new" or "existing"), wave_id (a recorded wave that asks no questions yet), wave_name, administered_date, respondent_count.';

commit;

-- ------ After it has run ---------------------------------------------------------------------------------
-- These should come back with no rows:
--   select column_name from information_schema.columns where table_schema = 'public' and table_name = 'draft' and column_name = 'note';
--   select 1 from pg_proc where proname = 'draft_set_parent' and prosrc like '%''note''%';
--
-- To undo (the notes themselves come back only from step 1's copy, if it was made):
--   alter table public.draft add column note text;
--   comment on column public.draft.note is 'Why: what a reviewer reads first.';
--   -- and create or replace private.draft_set_parent() with 'note' back in `allowed`, as in docs/sql_scripts.sql.
