-- Resets the TEST Supabase project (rqygcjgftmtatzrzcwcf) to a clean starting point.
-- Run it whole in the test project's SQL Editor whenever the tests' leftovers have piled up (a run that
-- crashed before its cleanup). It refuses to run anywhere else, the real database included (step 0).
--
-- After the seven scripts in sql/ the test project holds the seed (sql/7_seed.sql) and whatever the tests
-- and the extractions staff try there add. This keeps every row that isn't a test's and removes the rest:
--   * the tests' staff (named ZZTEST...), with every extraction that names them and every history row they wrote;
--   * the tests' registries, investigators, respondent types, studies, questionnaires and waves (named
--     ZZTEST...), with everything pushed into them (placements, questions, variables, choices, tags on them);
--   * the tests' measurement scales, tags, answer options and components (named ZZTEST...), and extractions
--     whose summary is;
--   * PDFs (pdf_file) that no extraction or wave names any more, and components no questionnaire prints.
-- Not touched: Supabase Auth's users, and the files in Storage.
--
-- The database's guards refuse two of these deletes on purpose (staff are never deleted, the history is never
-- changed). The script turns those two triggers off while it deletes and on again after. The foreign keys stay
-- on, so nothing kept can be left pointing at a removed row. Everything is one transaction: if anything
-- fails, nothing has changed.

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

-- ------ 1. The tests' staff, with their extractions and history -----------------------------------------
create temp table test_staff on commit drop as
  select staff_id from public.staff where name ilike 'ZZTEST%' or email ilike 'zztest%';

delete from public.extraction
where prepared_by in (select staff_id from test_staff)
   or saved_by in (select staff_id from test_staff)
   or editing_by in (select staff_id from test_staff)
   or summary ilike 'ZZTEST%';

alter table public.history disable trigger history_is_kept;
delete from public.history where staff_id in (select staff_id from test_staff);
alter table public.history enable trigger history_is_kept;

alter table public.staff disable trigger staff_never_deleted;
delete from public.staff where staff_id in (select staff_id from test_staff);
alter table public.staff enable trigger staff_never_deleted;

-- ------ 2. The tests' trees, with what was pushed into them -------------------------------------------------
-- A questionnaire takes its waves, placements, questions, variables, response options and tags with it.
delete from public.wave where name ilike 'ZZTEST%';
delete from public.questionnaire
where name ilike 'ZZTEST%'
   or study_id in (select study_id from public.study where name ilike 'ZZTEST%')
   or study_id in (select s.study_id from public.study s join public.registry r on r.registry_id = s.registry_id where r.name ilike 'ZZTEST%');
delete from public.study
where name ilike 'ZZTEST%'
   or registry_id in (select registry_id from public.registry where name ilike 'ZZTEST%');
delete from public.registry where name ilike 'ZZTEST%';
delete from public.principal_investigator p
where p.name ilike 'ZZTEST%'
  and not exists (select 1 from public.study s where s.principal_investigator_id = p.principal_investigator_id
                                                  or s.contact_principal_investigator_id = p.principal_investigator_id);
delete from public.respondent_type t
where t.name ilike 'ZZTEST%'
  and not exists (select 1 from public.questionnaire q where q.respondent_type_id = t.respondent_type_id);

-- ------ 3. The tests' vocabulary and scales, where nothing kept points at them -----------------------------------
delete from public.question_scale where scale_id in (select scale_id from public.measurement_scale where name ilike 'ZZTEST%');
delete from public.measurement_scale where name ilike 'ZZTEST%';
delete from public.tag t
where t.name ilike 'ZZTEST%'
  and not exists (select 1 from public.placement_tag x where x.tag_id = t.tag_id)
  and not exists (select 1 from public.variable_tag x where x.tag_id = t.tag_id)
  and not exists (select 1 from public.response_option_tag x where x.tag_id = t.tag_id);
delete from public.answer_option a
where a.standardized_label ilike 'ZZTEST%'
  and not exists (select 1 from public.response_option x where x.answer_option_id = a.answer_option_id);
delete from public.component c
where c.text ilike 'ZZTEST%'
  and not exists (select 1 from public.placement x where x.component_id = c.component_id)
  and not exists (select 1 from public.question_scale x where x.component_id = c.component_id);

-- ------ 4. What is left owned by nothing -------------------------------------------------------------------------
select private.forget_components(array(select component_id from public.component));
delete from public.pdf_file f
where not exists (select 1 from public.extraction e where e.pdf_sha256 = f.pdf_sha256)
  and not exists (select 1 from public.wave w where w.pdf_sha256 = f.pdf_sha256 or w.supplement_sha256 = f.pdf_sha256);

commit;

-- ------ 5. What is left ---------------------------------------------------------------------------------------
select 'staff' as table_name, count(*) as row_count from public.staff
union all select 'registry', count(*) from public.registry
union all select 'principal_investigator', count(*) from public.principal_investigator
union all select 'study', count(*) from public.study
union all select 'respondent_type', count(*) from public.respondent_type
union all select 'questionnaire', count(*) from public.questionnaire
union all select 'wave', count(*) from public.wave
union all select 'pdf_file', count(*) from public.pdf_file
union all select 'component', count(*) from public.component
union all select 'placement', count(*) from public.placement
union all select 'wave_question', count(*) from public.wave_question
union all select 'variable', count(*) from public.variable
union all select 'answer_option', count(*) from public.answer_option
union all select 'response_option', count(*) from public.response_option
union all select 'tag', count(*) from public.tag
union all select 'measurement_scale', count(*) from public.measurement_scale
union all select 'extraction', count(*) from public.extraction
union all select 'history', count(*) from public.history;
