-- Resets the TEST Supabase project (rqygcjgftmtatzrzcwcf) to a clean starting point.
-- Run it whole in the test project's SQL Editor whenever the test data has piled up. It refuses to run
-- anywhere else, the real database included (step 0).
--
-- What stays:
--   * every staff member but the tests' (named ZZTEST...);
--   * the questionnaires whose abbreviations are in keep_questionnaires (AHP, GNA, MQ and KQ), with everything
--     that belongs to them: their studies, registries and lead investigators, their waves, blocks and
--     questions, variables, choices and counts of respondents, scale items and categories, and the option
--     sets, answer choices, subjects and categories those use;
--   * every component type, measurement scale, respondent type and PDF (pdf_file), but the tests' own;
--   * the drafts, applied or not, with their history (applied_item), when everyone they involve is kept
--     staff and they aren't the tests' own.
-- Everything else in the public tables is removed, the tests' leftover ZZTEST rows included.
-- Not touched: Supabase Auth's users, and the files in Storage.
--
-- The database's guards refuse some of these deletes on purpose (staff are never deleted, an applied
-- draft is kept). The script turns the tables' own triggers off while it deletes and on again after.
-- The foreign keys stay on, so nothing kept can be left pointing at a removed row. Everything is one
-- transaction: if anything fails, nothing has changed.

begin;

-- ------ 0. Only the test project --------------------------------------------------------------------
-- The test project's database cluster has this system identifier; no other database does.
do $$
begin
  if (select system_identifier::text from pg_control_system()) is distinct from '7692131729119822239' then
    raise exception 'This is not the MATR test database, so nothing was removed. This script only resets the test project (rqygcjgftmtatzrzcwcf).';
  end if;
end
$$;

-- ------ 1. What stays ---------------------------------------------------------------------------------
-- (Every keep_ table below holds no nulls: a "not in" list with a null in it would match nothing.)
-- Edit this list to keep other questionnaires.
create temp table keep_questionnaires (abbreviation text) on commit drop;
insert into keep_questionnaires values ('AHP'), ('GNA'), ('MQ'), ('KQ');

create temp table keep_staff on commit drop as
  select staff_id from public.staff where name not ilike 'ZZTEST%' and email not ilike 'zztest%';

create temp table keep_questionnaire on commit drop as
  select questionnaire_id from public.questionnaire
  where abbreviation in (select abbreviation from keep_questionnaires) and name not ilike 'ZZTEST%';

create temp table keep_study on commit drop as
  select study_id from public.questionnaire where questionnaire_id in (select questionnaire_id from keep_questionnaire);

create temp table keep_registry on commit drop as
  select registry_id from public.study where study_id in (select study_id from keep_study);

create temp table keep_wave on commit drop as
  select wave_id from public.wave where questionnaire_id in (select questionnaire_id from keep_questionnaire);

create temp table keep_component on commit drop as
  select distinct component_id from public.questionnaire_component
  where questionnaire_id in (select questionnaire_id from keep_questionnaire);

create temp table keep_variable on commit drop as
  select variable_key from public.variable where questionnaire_id in (select questionnaire_id from keep_questionnaire);

create temp table keep_option_set on commit drop as
  select option_set_id from public.closed_ended
  where variable_key in (select variable_key from keep_variable) and option_set_id is not null
  union
  select expected_option_set_id from public.question_scale
  where component_id in (select component_id from keep_component) and expected_option_set_id is not null;

create temp table keep_answer_option on commit drop as
  select answer_option_id from public.response_option where variable_key in (select variable_key from keep_variable)
  union
  select answer_option_id from public.answer_option_set where option_set_id in (select option_set_id from keep_option_set);

create temp table keep_subject on commit drop as
  select subject_id from public.variable
  where variable_key in (select variable_key from keep_variable) and subject_id is not null;

create temp table keep_category on commit drop as
  select mesh_id from public.question_category where component_id in (select component_id from keep_component);

create temp table keep_investigator on commit drop as
  select principal_investigator_id from public.study where study_id in (select study_id from keep_study)
  union
  select contact_principal_investigator_id from public.study
  where study_id in (select study_id from keep_study) and contact_principal_investigator_id is not null;

-- A draft stays, applied or not, with its history, when everyone it names is kept staff and it isn't the tests'.
create temp table keep_draft on commit drop as
  select d.draft_id from public.draft d
  where coalesce(d.summary, '') not ilike 'ZZTEST%'
    and d.prepared_by in (select staff_id from keep_staff)
    and (d.saved_by is null or d.saved_by in (select staff_id from keep_staff))
    and (d.requested_reviewer is null or d.requested_reviewer in (select staff_id from keep_staff))
    and (d.editing_by is null or d.editing_by in (select staff_id from keep_staff))
    and not exists (
      select 1 from public.draft_review_event e
      where e.draft_id = d.draft_id and e.staff_id not in (select staff_id from keep_staff)
    );

-- ------ 2. The guards off while deleting -----------------------------------------------------------------
do $$
declare t record;
begin
  for t in select tablename from pg_tables where schemaname = 'public' loop
    execute format('alter table public.%I disable trigger user', t.tablename);
  end loop;
end
$$;

-- ------ 3. Removing the rest -------------------------------------------------------------------------------
-- Children before the rows they point at. A draft's details, reading and reviews go with it (on delete
-- cascade), and so do a variable's subtype row, choices and counts of respondents.
delete from public.applied_item where draft_id not in (select draft_id from keep_draft);
delete from public.draft where draft_id not in (select draft_id from keep_draft);

delete from public.response_option_count where wave_id not in (select wave_id from keep_wave);
delete from public.variable where variable_key not in (select variable_key from keep_variable);
delete from public.wave_question where questionnaire_id not in (select questionnaire_id from keep_questionnaire);
delete from public.question_scale where component_id not in (select component_id from keep_component);
delete from public.question_category where component_id not in (select component_id from keep_component);
delete from public.questionnaire_question where questionnaire_id not in (select questionnaire_id from keep_questionnaire);
delete from public.questionnaire_component where questionnaire_id not in (select questionnaire_id from keep_questionnaire);
delete from public.question where component_id not in (select component_id from keep_component);
delete from public.component where component_id not in (select component_id from keep_component);
delete from public.wave where wave_id not in (select wave_id from keep_wave);
delete from public.questionnaire where questionnaire_id not in (select questionnaire_id from keep_questionnaire);
delete from public.study where study_id not in (select study_id from keep_study);
delete from public.principal_investigator where principal_investigator_id not in (select principal_investigator_id from keep_investigator);
delete from public.registry where registry_id not in (select registry_id from keep_registry);
-- the tests' own; the rest stay
delete from public.respondent_type
  where name ilike 'ZZTEST%' and respondent_type_id not in (select respondent_type_id from public.questionnaire);
delete from public.question_scale where scale_id in (select scale_id from public.measurement_scale where name ilike 'ZZTEST%');
delete from public.measurement_scale where name ilike 'ZZTEST%';
delete from public.component_type where name ilike 'ZZTEST%';

delete from public.answer_option_set where option_set_id not in (select option_set_id from keep_option_set);
delete from public.option_set where option_set_id not in (select option_set_id from keep_option_set);
delete from public.answer_option where answer_option_id not in (select answer_option_id from keep_answer_option);
delete from public.subject where subject_id not in (select subject_id from keep_subject);
delete from public.category where mesh_id not in (select mesh_id from keep_category);

delete from public.staff where staff_id not in (select staff_id from keep_staff);

-- ------ 4. The guards on again ----------------------------------------------------------------------------------
do $$
declare t record;
begin
  for t in select tablename from pg_tables where schemaname = 'public' loop
    execute format('alter table public.%I enable trigger user', t.tablename);
  end loop;
end
$$;

commit;

-- ------ 5. What is left ---------------------------------------------------------------------------------------
select 'staff' as table_name, count(*) as row_count from public.staff
union all select 'registry', count(*) from public.registry
union all select 'study', count(*) from public.study
union all select 'principal_investigator', count(*) from public.principal_investigator
union all select 'respondent_type', count(*) from public.respondent_type
union all select 'questionnaire', count(*) from public.questionnaire
union all select 'wave', count(*) from public.wave
union all select 'component', count(*) from public.component
union all select 'question', count(*) from public.question
union all select 'variable', count(*) from public.variable
union all select 'response_option', count(*) from public.response_option
union all select 'option_set', count(*) from public.option_set
union all select 'answer_option', count(*) from public.answer_option
union all select 'subject', count(*) from public.subject
union all select 'category', count(*) from public.category
union all select 'measurement_scale', count(*) from public.measurement_scale
union all select 'component_type', count(*) from public.component_type
union all select 'pdf_file', count(*) from public.pdf_file
union all select 'draft', count(*) from public.draft
union all select 'applied_item', count(*) from public.applied_item;
