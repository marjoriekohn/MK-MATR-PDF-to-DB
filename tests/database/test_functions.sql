-- The TEST project's own functions (rqygcjgftmtatzrzcwcf), which the tests use to make and remove their rows in
-- one request instead of many. Run it whole, once, in the test project's SQL Editor after the seven scripts in
-- sql/, and again whenever it changes. It refuses to run anywhere else, the real database included: these
-- functions get past the database's guards (staff are never deleted, the history is never changed), which the
-- real database must always keep.
--
--   test_forget_staff(p_staff_ids, p_marker, p_remove_staff)
--     Removes every extraction that names staff the tests made (prepared, saved or being edited by them) and
--     every history row they wrote, and with p_remove_staff the staff rows too. The staff are named by id
--     (p_staff_ids) or by the start of their name (p_marker); only staff whose names start with ZZTEST, the
--     tests' marker, are ever touched. Returns counts.
--
--   test_make_tree(p_marker)
--     Adds a registry, a lead investigator, a respondent type, a study, a questionnaire and a wave, named with
--     the marker: what local_tree() makes. Returns the six rows as stored.
--
--   test_forget_tree(p_tree)
--     Removes what test_make_tree() made, given what it returned, with everything a test pushed into it: the
--     questionnaire's waves and placements go with it (on delete cascade), and the components the tree alone
--     printed are forgotten afterwards (private.forget_components).
--
--   test_forget_marked(p_marker)
--     Removes the vocabulary a push adds and the site never deletes, where a test named it with the marker:
--     tags and answer options whose name starts with it (any case), components whose text does, and
--     extractions whose summary does. A row something still points at stays (test-zz_leftovers.R then
--     reports it). Returns counts.
--
-- Callable only with the secret key (service_role), which is what the tests use.

begin;

-- ------ Only the test project --------------------------------------------------------------------
-- The test project's database cluster has this system identifier; no other database does.
do $$
begin
  if (select system_identifier::text from pg_control_system()) is distinct from '7692131729119822239' then
    raise exception 'This is not the MATR test database, so nothing was added. These functions belong only on the test project (rqygcjgftmtatzrzcwcf).';
  end if;
end
$$;

drop function if exists public.test_remove_applied(bigint[]);

-- ------ test_forget_staff ------------------------------------------------------------------------
create or replace function public.test_forget_staff(
  p_staff_ids    bigint[] default null,
  p_marker       text     default null,
  p_remove_staff boolean  default false
)
returns jsonb
language plpgsql
security definer
set search_path = public, pg_temp
as $$
declare
  v_staff       bigint[];
  v_extractions integer;
  v_history     integer;
begin
  if p_marker is not null and p_marker not like 'ZZTEST%' then
    raise exception 'Only the tests'' own staff are removed: their names start with ZZTEST.';
  end if;
  select coalesce(array_agg(staff_id), '{}') into v_staff
  from public.staff
  where name like 'ZZTEST%'
    and (staff_id = any(p_staff_ids) or (p_marker is not null and name like p_marker || '%'));

  delete from public.extraction
  where prepared_by = any(v_staff) or saved_by = any(v_staff) or editing_by = any(v_staff);
  get diagnostics v_extractions = row_count;

  -- the guard that keeps the history is off only inside this transaction
  alter table public.history disable trigger history_is_kept;
  delete from public.history where staff_id = any(v_staff);
  get diagnostics v_history = row_count;
  alter table public.history enable trigger history_is_kept;

  if p_remove_staff and cardinality(v_staff) > 0 then
    alter table public.staff disable trigger staff_never_deleted;
    delete from public.staff where staff_id = any(v_staff);
    alter table public.staff enable trigger staff_never_deleted;
  end if;

  return jsonb_build_object(
    'staff', cardinality(v_staff), 'extractions', v_extractions, 'history', v_history,
    'staff_removed', p_remove_staff and cardinality(v_staff) > 0
  );
end;
$$;

-- ------ test_make_tree ---------------------------------------------------------------------------
create or replace function public.test_make_tree(p_marker text)
returns jsonb
language plpgsql
security definer
set search_path = public, pg_temp
as $$
declare
  v_registry      public.registry;
  v_investigator  public.principal_investigator;
  v_respondent    public.respondent_type;
  v_study         public.study;
  v_questionnaire public.questionnaire;
  v_wave          public.wave;
  v_n             text := floor(random() * 1e9)::bigint::text;
begin
  if p_marker is null or p_marker not like 'ZZTEST%' then
    raise exception 'The tests'' rows are named with their marker, which starts with ZZTEST.';
  end if;
  insert into public.registry (name, abbreviation, participant_count)
    values (p_marker || ' Registry ' || v_n, p_marker || '-' || v_n, 100) returning * into v_registry;
  insert into public.principal_investigator (name, email)
    values (p_marker || ' Investigator ' || v_n, 'pi' || v_n || '@example.test') returning * into v_investigator;
  insert into public.respondent_type (name, minimum_age, maximum_age)
    values (p_marker || ' Respondent Type ' || v_n, 18, 99) returning * into v_respondent;
  insert into public.study (registry_id, principal_investigator_id, name, status)
    values (v_registry.registry_id, v_investigator.principal_investigator_id, p_marker || ' Study ' || v_n, 'Active')
    returning * into v_study;
  insert into public.questionnaire (study_id, respondent_type_id, name, abbreviation)
    values (v_study.study_id, v_respondent.respondent_type_id, p_marker || ' Questionnaire ' || v_n, 'ZZQ')
    returning * into v_questionnaire;
  insert into public.wave (questionnaire_id, name)
    values (v_questionnaire.questionnaire_id, p_marker || ' Wave ' || v_n) returning * into v_wave;
  return jsonb_build_object(
    'registry', to_jsonb(v_registry), 'investigator', to_jsonb(v_investigator),
    'respondent_type', to_jsonb(v_respondent), 'study', to_jsonb(v_study),
    'questionnaire', to_jsonb(v_questionnaire), 'wave', to_jsonb(v_wave)
  );
end;
$$;

-- ------ test_forget_tree -------------------------------------------------------------------------
create or replace function public.test_forget_tree(p_tree jsonb)
returns void
language plpgsql
security definer
set search_path = public, pg_temp
as $$
declare
  v_questionnaire bigint := (p_tree #>> '{questionnaire,questionnaire_id}')::bigint;
  v_printed       bigint[];
begin
  if coalesce(p_tree #>> '{registry,name}', '') not like 'ZZTEST%' then
    raise exception 'Only a tree the tests made is removed: its registry''s name starts with ZZTEST.';
  end if;
  -- every wave of the questionnaire, the tree's own and any a test added; the placements, questions, variables,
  -- response options and tags go with the questionnaire; the components only it printed are forgotten
  v_printed := array(select component_id from public.placement where questionnaire_id = v_questionnaire);
  delete from public.wave where questionnaire_id = v_questionnaire;
  delete from public.questionnaire where questionnaire_id = v_questionnaire;
  delete from public.study where study_id = (p_tree #>> '{study,study_id}')::bigint;
  delete from public.respondent_type where respondent_type_id = (p_tree #>> '{respondent_type,respondent_type_id}')::bigint;
  delete from public.principal_investigator
    where principal_investigator_id = (p_tree #>> '{investigator,principal_investigator_id}')::bigint;
  delete from public.registry where registry_id = (p_tree #>> '{registry,registry_id}')::bigint;
  perform private.forget_components(v_printed);
end;
$$;

-- ------ test_forget_marked -----------------------------------------------------------------------
create or replace function public.test_forget_marked(p_marker text)
returns jsonb
language plpgsql
security definer
set search_path = public, pg_temp
as $$
declare
  v_tags        integer;
  v_options     integer;
  v_components  integer;
  v_extractions integer;
begin
  if p_marker is null or p_marker not ilike 'ZZTEST%' then
    raise exception 'Only the tests'' own rows are removed: they are named with a marker that starts with ZZTEST.';
  end if;

  delete from public.extraction where summary ilike p_marker || '%';
  get diagnostics v_extractions = row_count;

  delete from public.tag t
  where t.name ilike p_marker || '%'
    and not exists (select 1 from public.placement_tag x where x.tag_id = t.tag_id)
    and not exists (select 1 from public.variable_tag x where x.tag_id = t.tag_id)
    and not exists (select 1 from public.response_option_tag x where x.tag_id = t.tag_id);
  get diagnostics v_tags = row_count;

  delete from public.answer_option a
  where a.standardized_label ilike p_marker || '%'
    and not exists (select 1 from public.response_option x where x.answer_option_id = a.answer_option_id);
  get diagnostics v_options = row_count;

  delete from public.component c
  where c.text ilike p_marker || '%'
    and not exists (select 1 from public.placement x where x.component_id = c.component_id)
    and not exists (select 1 from public.question_scale x where x.component_id = c.component_id);
  get diagnostics v_components = row_count;

  return jsonb_build_object(
    'extractions', v_extractions, 'tags', v_tags, 'answer_options', v_options, 'components', v_components
  );
end;
$$;

revoke all on function public.test_forget_staff(bigint[], text, boolean) from public, anon, authenticated;
revoke all on function public.test_make_tree(text) from public, anon, authenticated;
revoke all on function public.test_forget_tree(jsonb) from public, anon, authenticated;
revoke all on function public.test_forget_marked(text) from public, anon, authenticated;
grant execute on function public.test_forget_staff(bigint[], text, boolean) to service_role;
grant execute on function public.test_make_tree(text) to service_role;
grant execute on function public.test_forget_tree(jsonb) to service_role;
grant execute on function public.test_forget_marked(text) to service_role;

commit;

-- The site's API learns of new functions when it reloads.
notify pgrst, 'reload schema';
