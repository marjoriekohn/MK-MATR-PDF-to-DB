-- =====================================================================================================
-- MATR metadata database: names are unique whatever their capitals, and two columns nobody uses go
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first, then on the real one. Run 9 to 13 first.
--
-- 1. Names that must be unique are compared ignoring capitals and spaces at either end, as subjects, respondent
--    types and registry abbreviations already are, so "Yes / No" and "yes / no " can't both be there:
--      option_set.name                     option_set_name_unique (was an exact match)
--      measurement_scale.name              measurement_scale_name_unique (was an exact match)
--      measurement_scale.abbreviation      measurement_scale_abbreviation_unique (was an exact match)
--    and these are new:
--      registry.name                       registry_name_unique
--      study.name, within its registry     study_name_unique_in_registry
--      questionnaire.name, within its study           questionnaire_name_unique_in_study
--      questionnaire.abbreviation, within its study   questionnaire_abbreviation_unique_in_study
--    An answer choice (answer_option.standardized_label) needs nothing: it is kept in lower case with single
--    spaces already (answer_option_label_normal, 9_integrity_rules.sql), so its exact match is enough.
--    If two rows already break a rule, this says which and stops; change one (Make an edit) and run it again.
--
-- 2. draft_review_event.revision goes. Nothing shows it. The three functions that wrote it are replaced
--    without it: private.draft_record_submission(), public.send_back_draft() and private.apply_draft_items()
--    (replaced whole; docs/sql/sql_scripts.sql has the same). An approval is still of the revision the
--    approver read: approve_draft() and send_back_draft() refuse when the draft's revision has moved on.
--
-- 3. option_set.description goes. Nothing shows it but the Make an edit form, and it is empty everywhere.
--
-- LOST: the revision of each review (which revision was submitted, sent back or approved) and any option set
-- descriptions. Nothing else changes.
--
-- ORDER: put the site's new code in place first, then run this. The old code reads draft_review_event.revision
-- (every draft page would fail) and offers an option set's description; the new code does neither, and works
-- before and after this runs.
--
-- After it has run:
--   * docs/sql/sql_scripts.sql already builds the same.
--   * docs/database_tables.md lists the columns; regenerate it with tests/database/describe_schema.R.
--
-- It is one transaction: if any step fails, nothing changes.
-- =====================================================================================================

begin;

-- ------ 1. Names unique, ignoring capitals and outer spaces ----------------------------------------------
-- Rows that already have the same name would stop the rule being made: say which, and stop.
do $$
declare
  clash text;
begin
  select string_agg(clash_found, '; ') into clash
  from (
    select format('option set names "%s"', string_agg(name, '" and "' order by option_set_id)) as clash_found
    from public.option_set group by lower(btrim(name)) having count(*) > 1
    union all
    select format('measurement scale names "%s"', string_agg(name, '" and "' order by scale_id))
    from public.measurement_scale group by lower(btrim(name)) having count(*) > 1
    union all
    select format('measurement scale abbreviations "%s"', string_agg(abbreviation, '" and "' order by scale_id))
    from public.measurement_scale group by lower(btrim(abbreviation)) having count(*) > 1
    union all
    select format('registry names "%s"', string_agg(name, '" and "' order by registry_id))
    from public.registry group by lower(btrim(name)) having count(*) > 1
    union all
    select format('study names "%s" in registry %s', string_agg(name, '" and "' order by study_id), registry_id)
    from public.study group by registry_id, lower(btrim(name)) having count(*) > 1
    union all
    select format('questionnaire names "%s" in study %s', string_agg(name, '" and "' order by questionnaire_id), study_id)
    from public.questionnaire group by study_id, lower(btrim(name)) having count(*) > 1
    union all
    select format('questionnaire abbreviations "%s" in study %s', string_agg(abbreviation, '" and "' order by questionnaire_id), study_id)
    from public.questionnaire group by study_id, lower(btrim(abbreviation)) having count(*) > 1
  ) clashes;
  if clash is not null then
    raise exception 'These are the same once capitals and outer spaces are ignored; change one of each (Make an edit), then run this again: %', clash;
  end if;
end;
$$;

-- an exact match before: the rule that ignores capitals takes its name
alter table public.option_set        drop constraint option_set_name_unique;
alter table public.measurement_scale drop constraint measurement_scale_name_unique,
                                     drop constraint measurement_scale_abbreviation_unique;

create unique index option_set_name_unique                    on public.option_set (lower(btrim(name)));              -- BR109
create unique index measurement_scale_name_unique             on public.measurement_scale (lower(btrim(name)));       -- BR78
create unique index measurement_scale_abbreviation_unique     on public.measurement_scale (lower(btrim(abbreviation)));
create unique index registry_name_unique                      on public.registry (lower(btrim(name)));
create unique index study_name_unique_in_registry             on public.study (registry_id, lower(btrim(name)));
create unique index questionnaire_name_unique_in_study        on public.questionnaire (study_id, lower(btrim(name)));
create unique index questionnaire_abbreviation_unique_in_study on public.questionnaire (study_id, lower(btrim(abbreviation)));


-- ------ 2. A review no longer records the revision ----------------------------------------------------------
-- A draft that has just gone to Awaiting Review (from p_was: Draft, Sent Back, or nothing for a new one)
-- was submitted, by p_staff_id.
create or replace function private.draft_record_submission(p_draft_id bigint, p_was text, p_staff_id bigint)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d public.draft%rowtype;
begin
  select * into d from public.draft where draft_id = p_draft_id;
  if d.status = 'Awaiting Review' and p_was is distinct from 'Awaiting Review' then
    insert into public.draft_review_event (draft_id, event, staff_id)
    values (d.draft_id, 'Submitted', p_staff_id);
  end if;
end;
$$;

-- p_revision is the revision the reviewer read. Like any save, it makes the reviewer saved_by and moves
-- saved_at on; the revision stays. Returns the draft's revision.
create or replace function public.send_back_draft(
  p_draft_id bigint,
  p_revision integer,
  p_staff_id bigint,
  p_note     text default null
)
returns integer
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d   public.draft%rowtype;
  who public.staff%rowtype;
begin
  -- the lock makes a send back, a save and an approval of the same request take turns
  select * into d from public.draft where draft_id = p_draft_id for update;
  if not found then
    raise exception 'That is no longer in the database.';
  end if;
  select * into who from public.staff where staff_id = p_staff_id;
  if not found or not who.active then
    raise exception 'Only active staff can send a change back.';
  end if;
  if d.status = 'Applied' then
    raise exception 'That change has already been applied, so it can''t be changed.';
  end if;
  if d.status not in ('Awaiting Review', 'Sent Back') then
    raise exception 'Only a change waiting for review can be sent back.';
  end if;
  if d.revision <> p_revision then
    raise exception 'That was changed after you opened it. Open it again to see the new version.';
  end if;

  insert into public.draft_review_event (draft_id, event, staff_id, note)
  values (d.draft_id, 'Sent Back', p_staff_id, p_note);
  update public.draft
  set status = 'Sent Back', saved_by = p_staff_id, saved_at = now()
  where draft_id = p_draft_id;
  return d.revision;
end;
$$;

-- An edit can change the tables Make an edit changes (R/service_edit_entities.R, and what their changes
-- reach). An extraction only adds rows (Insert), to the tables a questionnaire is made of.
create or replace function private.apply_draft_items(
  d             public.draft,
  p_items       jsonb,
  p_approved_by bigint
)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  editable constant text[] := array[
    'registry', 'principal_investigator', 'study', 'respondent_type', 'questionnaire', 'wave',
    'component_type', 'component', 'questionnaire_component', 'question', 'questionnaire_question',
    'wave_question', 'category', 'question_category', 'answer_option', 'subject', 'option_set', 'answer_option_set',
    'measurement_scale', 'question_scale', 'variable', 'open_ended', 'closed_ended', 'response_option', 'response_option_count'
  ];
  loadable constant text[] := array[
    'questionnaire', 'wave', 'component', 'questionnaire_component', 'question', 'questionnaire_question',
    'wave_question', 'variable', 'open_ended', 'closed_ended', 'option_set', 'answer_option',
    'answer_option_set', 'response_option', 'response_option_count', 'subject'
  ];
  token_pattern constant text := '^new:[a-z_]+:[0-9]+$';

  allowed      text[] := case d.kind when 'Extraction' then loadable else editable end;
  new_ids      jsonb := '{}'::jsonb;   -- 'new:component:1' -> the number the database gave it
  col_cache    jsonb := '{}'::jsonb;   -- table -> its columns

  grp          record;
  attr         jsonb;
  cols         jsonb;                  -- the table's columns: type, whether the database numbers it, whether it holds numbers
  ident        jsonb;                  -- the row's key, with new rows swapped for their numbers
  key          text;
  val          text;
  where_sql    text;
  made_token   text;                   -- an Insert: the new row's name ...
  made_column  text;                   -- ... and the column the database numbers
  col_list     text[];
  val_list     text[];
  set_list     text[];
  new_id       bigint;
  n_rows       integer;
  same         boolean;
  recorded_id  text;
  recorded_val text;
  token        text;
  added_tables text[] := array[]::text[]; -- an extraction's rows, as added: their tables ...
  added_ids    text[] := array[]::text[]; -- ... and their keys
begin
  if jsonb_typeof(p_items) is distinct from 'array' or jsonb_array_length(p_items) = 0 then
    raise exception 'There is nothing in this to apply.';
  end if;

  set constraints all deferred;

  -- Runs of items on the same row and action, in order.
  for grp in
    with items as (
      select (i.value ->> 'item_number')::integer as item_number,
             i.value ->> 'entity_name'            as entity_name,
             i.value ->> 'record_identifier'      as record_identifier,
             i.value ->> 'action'                 as action,
             i.value ->> 'attribute_name'         as attribute_name,
             i.value ->> 'old_value'              as old_value,
             i.value ->> 'new_value'              as new_value
      from jsonb_array_elements(p_items) as i(value)
    ),
    marked as (
      select items.*,
             case when (entity_name, record_identifier, action) is not distinct from
                       (lag(entity_name) over w, lag(record_identifier) over w, lag(action) over w)
                  then 0 else 1 end as starts
      from items
      window w as (order by item_number)
    ),
    numbered as (
      select marked.*, sum(starts) over (order by item_number) as run from marked
    )
    select run, entity_name, record_identifier, action, min(item_number) as first_item,
           jsonb_agg(jsonb_build_object(
             'n', item_number, 'attribute', attribute_name, 'old', old_value, 'new', new_value
           ) order by item_number) as attrs
    from numbered
    group by run, entity_name, record_identifier, action
    order by run
  loop
    if grp.entity_name is null or not (grp.entity_name = any (allowed)) then
      raise exception 'Item %: this change touches a table it can''t change (%).', grp.first_item, coalesce(grp.entity_name, '(none)');
    end if;
    if grp.action is null or grp.action not in ('Insert', 'Update', 'Delete') then
      raise exception 'Item %: the action % isn''t Insert, Update or Delete.', grp.first_item, coalesce(grp.action, '(none)');
    end if;
    if d.kind = 'Extraction' and grp.action <> 'Insert' then
      raise exception 'Item %: an extraction only adds rows.', grp.first_item;
    end if;

    cols := col_cache -> grp.entity_name;
    if cols is null then
      select jsonb_object_agg(a.attname, jsonb_build_object(
               'type', format_type(a.atttypid, a.atttypmod),
               'identity', exists (select 1 from pg_attrdef ad
                                   where ad.adrelid = a.attrelid and ad.adnum = a.attnum
                                     and pg_get_expr(ad.adbin, ad.adrelid) like '%random_id(%'),
               'number', a.atttypid in ('int2'::regtype, 'int4'::regtype, 'int8'::regtype)))
        into cols
      from pg_attribute a
      where a.attrelid = format('public.%I', grp.entity_name)::regclass and a.attnum > 0 and not a.attisdropped;
      col_cache := col_cache || jsonb_build_object(grp.entity_name, cols);
    end if;

    -- The row's key, new rows swapped for their numbers. The one thing an Insert can't swap is the
    -- key the database is about to number.
    ident := '{}'::jsonb;
    made_token := null;
    made_column := null;
    for key, val in select k, v from jsonb_each_text(grp.record_identifier::jsonb) as e(k, v) loop
      if not cols ? key then
        raise exception 'Item %: % has no column %.', grp.first_item, grp.entity_name, key;
      end if;
      if val ~ token_pattern then
        if grp.action = 'Insert' and (cols -> key ->> 'identity')::boolean then
          made_token := val;
          made_column := key;
          continue;
        end if;
        if not new_ids ? val then
          raise exception 'Item %: it mentions % before that row is added.', grp.first_item, val;
        end if;
        val := new_ids ->> val;
      elsif grp.action = 'Insert' and (cols -> key ->> 'identity')::boolean then
        raise exception 'Item %: a new % row can''t choose its own %.', grp.first_item, grp.entity_name, key;
      end if;
      ident := ident || jsonb_build_object(key, val);
    end loop;

    -- Which row, for an Update or a Delete.
    where_sql := (
      select string_agg(format('%I = %L::%s', e.k, e.v, cols -> e.k ->> 'type'), ' and ' order by e.k)
      from jsonb_each_text(ident) as e(k, v)
    );

    if grp.action = 'Insert' then
      col_list := array[]::text[];
      val_list := array[]::text[];
      for key, val in select k, v from jsonb_each_text(ident) as e(k, v) loop
        col_list := col_list || format('%I', key);
        val_list := val_list || format('%L::%s', val, cols -> key ->> 'type');
      end loop;
      for attr in select value from jsonb_array_elements(grp.attrs) loop
        key := attr ->> 'attribute';
        val := attr ->> 'new';
        if key is null or val is null then
          continue;
        end if;
        if not cols ? key then
          raise exception 'Item %: % has no column %.', attr ->> 'n', grp.entity_name, key;
        end if;
        if key = any (array(select jsonb_object_keys(ident))) then
          -- a key column given again as a field (a question's component_id): same value
          continue;
        end if;
        if (cols -> key ->> 'number')::boolean and val ~ token_pattern then
          if not new_ids ? val then
            raise exception 'Item %: it mentions % before that row is added.', attr ->> 'n', val;
          end if;
          val := new_ids ->> val;
        end if;
        col_list := col_list || format('%I', key);
        val_list := val_list || format('%L::%s', val, cols -> key ->> 'type');
      end loop;

      if made_column is not null then
        execute format(
          case when cardinality(col_list) = 0 then 'insert into public.%1$I default values returning %2$I'
               else 'insert into public.%1$I (%3$s) values (%4$s) returning %2$I' end,
          grp.entity_name, made_column, array_to_string(col_list, ', '), array_to_string(val_list, ', ')
        ) into new_id;
        new_ids := new_ids || jsonb_build_object(made_token, new_id);
      else
        execute format('insert into public.%I (%s) values (%s)',
          grp.entity_name, array_to_string(col_list, ', '), array_to_string(val_list, ', '));
      end if;

    else
      -- Update and Delete: the row has to be there, and as it was.
      execute format('select count(*) from public.%I where %s', grp.entity_name, where_sql) into n_rows;
      if n_rows <> 1 then
        raise exception 'Item %: the % row % is no longer in the database, so this change is out of date.',
          grp.first_item, grp.entity_name, grp.record_identifier;
      end if;
      for attr in select value from jsonb_array_elements(grp.attrs) loop
        key := attr ->> 'attribute';
        if key is null then
          continue;
        end if;
        if not cols ? key then
          raise exception 'Item %: % has no column %.', attr ->> 'n', grp.entity_name, key;
        end if;
        execute format('select %I is not distinct from %L::%s from public.%I where %s',
          key, attr ->> 'old', cols -> key ->> 'type', grp.entity_name, where_sql) into same;
        if not same then
          execute format('select coalesce(%I::text, ''(empty)'') from public.%I where %s',
            key, grp.entity_name, where_sql) into val;
          raise exception 'Item %: % of the % row % is now "%", not "%" as when this change was prepared. It is out of date.',
            attr ->> 'n', key, grp.entity_name, grp.record_identifier, val, coalesce(attr ->> 'old', '(empty)');
        end if;
      end loop;

      if grp.action = 'Update' then
        set_list := array[]::text[];
        for attr in select value from jsonb_array_elements(grp.attrs) loop
          key := attr ->> 'attribute';
          if key is null then
            raise exception 'Item %: an update has to say which column.', attr ->> 'n';
          end if;
          val := attr ->> 'new';
          if val ~ token_pattern and (cols -> key ->> 'number')::boolean then
            if not new_ids ? val then
              raise exception 'Item %: it mentions % before that row is added.', attr ->> 'n', val;
            end if;
            val := new_ids ->> val;
          end if;
          set_list := set_list || format('%I = %L::%s', key, val, cols -> key ->> 'type');
        end loop;
        execute format('update public.%I set %s where %s', grp.entity_name, array_to_string(set_list, ', '), where_sql);
      else
        execute format('delete from public.%I where %s', grp.entity_name, where_sql);
      end if;
    end if;

    -- applied_item says what was done, with the numbers the database gave.
    recorded_id := grp.record_identifier;
    for token in select jsonb_object_keys(new_ids) loop
      recorded_id := replace(recorded_id, '"' || token || '"', new_ids ->> token);
    end loop;
    if d.kind = 'Extraction' then
      -- an extraction is recorded by the rows it added, one line for each table (below), not field by field:
      -- what it put in them is in its extraction draft, which is kept
      added_tables := added_tables || grp.entity_name;
      added_ids := added_ids || recorded_id;
      continue;
    end if;
    for attr in select value from jsonb_array_elements(grp.attrs) loop
      key := attr ->> 'attribute';
      recorded_val := attr ->> 'new';
      if recorded_val ~ token_pattern and key is not null and coalesce((cols -> key ->> 'number')::boolean, false)
         and new_ids ? recorded_val then
        recorded_val := new_ids ->> recorded_val;
      end if;
      insert into public.applied_item
        (draft_id, item_number, entity_name, record_identifier, action, attribute_name, old_value, new_value)
      values
        (d.draft_id, (attr ->> 'n')::integer, grp.entity_name, recorded_id, grp.action, key, attr ->> 'old', recorded_val);
    end loop;
  end loop;

  -- An extraction's line for each table it added to: the keys of its rows there, as a JSON array, in the order
  -- they were added (the tables in the order it first added to them).
  if d.kind = 'Extraction' then
    insert into public.applied_item (draft_id, item_number, entity_name, record_identifier, action)
    select d.draft_id, row_number() over (order by min(first_at)), t, '[' || string_agg(id, ',' order by first_at) || ']', 'Insert'
    from (
      select a.t, a.id, min(a.n) as first_at
      from unnest(added_tables, added_ids) with ordinality as a(t, id, n)
      group by a.t, a.id
    ) as added
    group by t;
  end if;

  -- the approval, if it needed one, is recorded with the change (before it is Applied, which takes no more)
  if p_approved_by is not null then
    insert into public.draft_review_event (draft_id, event, staff_id)
    values (d.draft_id, 'Approved', p_approved_by);
  end if;

  update public.draft
  set status = 'Applied', applied_at = now(), saved_at = now()
  where draft_id = d.draft_id;
end;
$$;

alter table public.draft_review_event drop column revision;   -- its check constraint goes with it

comment on table public.draft_review_event is
  'Each time a draft was submitted for review, sent back, or approved: by whom and when. An approval is of the draft''s revision the approver read (approve_draft checks it).';


-- ------ 3. An option set has no description ---------------------------------------------------------------
alter table public.option_set drop column description;

commit;

-- The site's API learns of the changed columns when it reloads.
notify pgrst, 'reload schema';
