-- The TEST project's own functions (rqygcjgftmtatzrzcwcf), which the tests use to make and remove their rows in
-- one request instead of many. Run it whole, once, in the test project's SQL Editor, and again whenever it
-- changes. It refuses to run anywhere else, the real database included: these functions get past the
-- database's guards, which the real database must always keep.
--
--   test_forget_staff(p_staff_ids, p_marker, p_remove_staff)
--     Removes every draft that involves staff the tests made (prepared, saved, asked to review, being edited,
--     or reviewed by them), applied ones too, with their history (applied_item). The database keeps an
--     applied draft for good (draft_guard), so without this each test run left its applied drafts behind.
--     With p_remove_staff it removes those staff too (staff_never_deleted keeps them otherwise).
--     The staff are named by id (p_staff_ids) or by the start of their name (p_marker); only staff whose
--     names start with ZZTEST, the tests' marker, are ever touched.
--
--   test_make_tree(p_marker)
--     Adds a registry, a lead investigator, a respondent type, a study, a questionnaire and a wave, named with
--     the marker: what local_tree() makes. Returns the six rows as stored.
--
--   test_forget_tree(p_tree)
--     Removes what test_make_tree() made, given what it returned.
--
--   test_remove_applied(p_draft_ids)
--     Removes the rows applied drafts added to the live tables, from each draft's list of them (applied_item):
--     the tables that point at others first. Only drafts prepared by the tests' own staff. Returns how many
--     rows went. The drafts and their lists stay; test_forget_staff() removes those.
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
  v_staff  bigint[];
  v_drafts bigint[];
  v_items  integer;
begin
  if p_marker is not null and p_marker not like 'ZZTEST%' then
    raise exception 'Only the tests'' own staff are removed: their names start with ZZTEST.';
  end if;
  select coalesce(array_agg(staff_id), '{}') into v_staff
  from public.staff
  where name like 'ZZTEST%'
    and (staff_id = any(p_staff_ids) or (p_marker is not null and name like p_marker || '%'));

  select coalesce(array_agg(d.draft_id), '{}') into v_drafts
  from public.draft d
  where d.prepared_by = any(v_staff) or d.saved_by = any(v_staff)
     or d.requested_reviewer = any(v_staff) or d.editing_by = any(v_staff)
     or exists (select 1 from public.draft_review_event e where e.draft_id = d.draft_id and e.staff_id = any(v_staff));

  -- A draft's details, reading and reviews go with it (on delete cascade). Its history doesn't, so it goes
  -- first. The guard that keeps an applied draft is off only inside this transaction.
  delete from public.applied_item where draft_id = any(v_drafts);
  get diagnostics v_items = row_count;
  alter table public.draft disable trigger draft_guard;
  delete from public.draft where draft_id = any(v_drafts);
  alter table public.draft enable trigger draft_guard;

  if p_remove_staff and cardinality(v_staff) > 0 then
    alter table public.staff disable trigger staff_never_deleted;
    delete from public.staff where staff_id = any(v_staff);
    alter table public.staff enable trigger staff_never_deleted;
  end if;

  return jsonb_build_object(
    'staff', cardinality(v_staff), 'drafts', cardinality(v_drafts), 'applied_items', v_items,
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
begin
  if coalesce(p_tree #>> '{registry,name}', '') not like 'ZZTEST%' then
    raise exception 'Only a tree the tests made is removed: its registry''s name starts with ZZTEST.';
  end if;
  delete from public.wave where wave_id = (p_tree #>> '{wave,wave_id}')::bigint;
  delete from public.questionnaire where questionnaire_id = (p_tree #>> '{questionnaire,questionnaire_id}')::bigint;
  delete from public.study where study_id = (p_tree #>> '{study,study_id}')::bigint;
  delete from public.respondent_type where respondent_type_id = (p_tree #>> '{respondent_type,respondent_type_id}')::bigint;
  delete from public.principal_investigator
    where principal_investigator_id = (p_tree #>> '{investigator,principal_investigator_id}')::bigint;
  delete from public.registry where registry_id = (p_tree #>> '{registry,registry_id}')::bigint;
end;
$$;

-- ------ test_remove_applied ----------------------------------------------------------------------
create or replace function public.test_remove_applied(p_draft_ids bigint[])
returns integer
language plpgsql
security definer
set search_path = public, pg_temp
as $$
declare
  -- The tables an edit or a push can add to, children before the rows they point at. A variable's subtype row
  -- and its choices (and their counts), and a component's question row, go with it (on delete cascade), and
  -- the database wants a variable and its subtype removed in one statement, so those aren't removed here.
  -- A table that isn't listed goes last.
  v_order    text[] := array[
    'variable', 'answer_option_set', 'option_set', 'answer_option',
    'wave_question', 'questionnaire_question', 'question_category', 'question_scale',
    'questionnaire_component', 'component',
    'wave', 'questionnaire', 'study', 'registry', 'principal_investigator', 'respondent_type', 'subject',
    'category', 'measurement_scale'
  ];
  v_cascaded text[] := array['open_ended', 'closed_ended', 'response_option', 'response_option_count', 'question'];
  v_table    text;
  v_where    text;
  v_rows     integer;
  v_removed  integer := 0;
begin
  if exists (
    select 1 from public.draft d left join public.staff s on s.staff_id = d.prepared_by
    where d.draft_id = any(p_draft_ids) and coalesce(s.name, '') not like 'ZZTEST%'
  ) then
    raise exception 'Only what the tests applied is removed: drafts prepared by staff whose names start with ZZTEST.';
  end if;
  for v_table in
    select t.entity_name
    from (select distinct entity_name from public.applied_item
          where draft_id = any(p_draft_ids) and action = 'Insert') t
    where not t.entity_name = any(v_cascaded)
    order by array_position(v_order, t.entity_name) nulls last, t.entity_name
  loop
    -- an edit's item names one row ({"column": value}); a push's line for a table lists every row it added ([...])
    select string_agg('(' || k.clause || ')', ' or ') into v_where
    from public.applied_item i
    cross join lateral jsonb_array_elements(
      case when jsonb_typeof(i.record_identifier::jsonb) = 'array' then i.record_identifier::jsonb
           else jsonb_build_array(i.record_identifier::jsonb) end
    ) as rk(row_key)
    cross join lateral (
      select string_agg(format('%I = %L', c.key, c.value), ' and ') as clause from jsonb_each_text(rk.row_key) c
    ) k
    where i.draft_id = any(p_draft_ids) and i.action = 'Insert' and i.entity_name = v_table;
    if v_where is not null then
      execute format('delete from public.%I where %s', v_table, v_where);
      get diagnostics v_rows = row_count;
      v_removed := v_removed + v_rows;
    end if;
  end loop;
  return v_removed;
end;
$$;

revoke all on function public.test_forget_staff(bigint[], text, boolean) from public, anon, authenticated;
revoke all on function public.test_make_tree(text) from public, anon, authenticated;
revoke all on function public.test_forget_tree(jsonb) from public, anon, authenticated;
grant execute on function public.test_forget_staff(bigint[], text, boolean) to service_role;
grant execute on function public.test_make_tree(text) to service_role;
grant execute on function public.test_forget_tree(jsonb) to service_role;
revoke all on function public.test_remove_applied(bigint[]) from public, anon, authenticated;
grant execute on function public.test_remove_applied(bigint[]) to service_role;

commit;

-- The site's API learns of new functions when it reloads.
notify pgrst, 'reload schema';
