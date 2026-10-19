-- =====================================================================================================
-- MATR metadata database: a response option is a choice printed on the form (drop response_option.printed)
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor.
--
-- A choice that isn't printed can't be marked by a respondent, so it isn't a response option and has no
-- code. The site no longer records one: Gemini is told to record only printed choices, the Edit
-- Extraction page has no "Printed" check box, and a lone check box (a "Yes" bubble on its own) is a
-- closed-ended variable with one response option. So every response_option row is printed, and
-- response_option.printed (always true from now on) goes.
--
-- What the database itself needs for that:
--   * Nothing makes a closed-ended variable have two or more response options. That rule (BR99) and the
--     one for option sets (BR106) are checked by the site, not the database (the list at the end of
--     docs/sql_scripts.sql), and the site now asks for one or more. A closed-ended variable with one
--     response option, and an option set with one answer option, are allowed as the tables stand.
--   * Likewise nothing in the database refuses to delete a study's last questionnaire (BR11) or a
--     questionnaire's last wave (BR19); Make an edit no longer refuses either. Deleting a wave still takes
--     the questions it was the only wave to ask (BR69), as before.
--
-- ORDER: deploy the site's new code FIRST, then run this. The old code reads response_option.printed (the
-- edit pages read every column of the table) and writes it (an extraction's push), so running this
-- before the new code is deployed breaks those. The new code never mentions the column, so it works
-- before and after this runs.
--
-- BEFORE RUNNING, CHECK THE PUBLIC DASHBOARD: it reads the MATR tables through the REST API (section 5c
-- of docs/sql_scripts.sql). If it asks for response_option.printed (by name, or with select=*), change it
-- first. Step 0 can only see what is inside the database.
--
-- What depends on response_option.printed (checked against docs/sql_scripts.sql, 2026-10-19):
--   * its default (true), dropped with the column.
--   * no view, index, constraint, trigger, policy or function refers to it. Step 0 checks again.
--   * docs/sql_scripts.sql: remove the line `printed boolean not null default true,` from section 1's
--     create table public.response_option, and in the list at the end change "BR99 closed-ended has >= 2
--     options" to ">= 1 option", "BR106 option set has >= 2 answer options" to ">= 1 answer option", and
--     drop "BR11 study has >= 1 questionnaire" and "BR19 questionnaire has >= 1 wave".
--   * docs/database_tables.md lists the column; regenerate it with tests/database/describe_schema.R.
--   * history: applied_item rows that recorded a value for "printed" stay as they are (they name the
--     column in text), so the Edit history still shows what was done then.
--   * drafts: an extraction draft saved with a choice marked "printed": false still has it in its JSON;
--     the site leaves such a choice out when it opens the draft, and the next save drops it.
--
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
  where d.refobjid = 'public.response_option'::regclass and a.attname = 'printed';
  if dependents is not null then
    raise exception 'Views use public.response_option.printed (%). Change them first.', dependents;
  end if;

  -- Functions that mention the column by name in their code (printed_label doesn't count)
  select string_agg(p.oid::regprocedure::text, ', ') into dependents
  from pg_proc p
  where p.pronamespace in ('public'::regnamespace, 'private'::regnamespace)
    and p.prosrc ~ '([^_a-z]|^)printed([^_a-z]|$)';
  if dependents is not null then
    raise notice 'These functions mention "printed"; check they don''t mean response_option.printed: %', dependents;
  end if;
end;
$$;

-- ------ 1. The response options recorded as not printed ---------------------------------------------------
-- See what there is first (this changes nothing):
--   select v.questionnaire_id, v.matr_id, a.standardized_label, r.code_value
--   from public.response_option r
--   join public.variable v on v.variable_key = r.variable_key
--   join public.answer_option a on a.answer_option_id = r.answer_option_id
--   where not r.printed
--   order by v.questionnaire_id, v.matr_id;
--
-- They are a lone check box's "No" (the box left empty). Deciding what to do with them is up to MATR: the
-- answers already collected may use their codes. To delete them, uncomment the next statement. Their
-- variables keep pointing at the option set they had (e.g. "Yes / No"), which then lists a choice the
-- variable no longer offers; Make an edit can point them at a set of just their printed choices.
-- delete from public.response_option where not printed;

-- ------ 2. Stop if any are left -----------------------------------------------------------------------------
-- Dropping the column would otherwise make them look printed. Nothing has changed if this stops.
do $$
declare
  unprinted bigint;
begin
  select count(*) into unprinted from public.response_option where not printed;
  if unprinted > 0 then
    raise exception '% response options are recorded as not printed. Decide what to do with them first (step 1).', unprinted;
  end if;
end;
$$;

-- ------ 3. The column goes -------------------------------------------------------------------------------
-- No CASCADE: if anything still depends on it, this stops and nothing changes.
alter table public.response_option drop column printed;

-- ------ 4. Descriptions -----------------------------------------------------------------------------------
comment on table public.response_option is
  'One answer option as it was printed for one closed-ended variable. A closed-ended variable has one or more (a lone check box has one).';
-- Where an extraction goes is now always a recorded registry, study, questionnaire and wave: anything that
-- isn't recorded yet is added from the Basic Info form's drop-downs first.
comment on column public.extracted_draft.info is
  'Where the extraction goes, picked on Change Requests after its change request is submitted (empty until then): registry_id, study_id, questionnaire_id and wave_id, a recorded wave that asks no questions yet.';

commit;

-- ------ After it has run ---------------------------------------------------------------------------------
-- This should come back with no rows:
--   select column_name from information_schema.columns
--   where table_schema = 'public' and table_name = 'response_option' and column_name = 'printed';
--
-- To undo (the unprinted response options come back only from a backup, if step 1's delete was run):
--   alter table public.response_option add column printed boolean not null default true;
--   comment on table public.response_option is 'One answer option as it appeared for one closed-ended variable.';
