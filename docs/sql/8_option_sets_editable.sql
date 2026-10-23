-- =====================================================================================================
-- MATR metadata database: Make an edit can add, change and delete option sets
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one.
--
-- Make an edit now lists Option Sets (a named set of answer choices that closed-ended variables offer),
-- Measurement Scales and their items, and PDF Files (only shown). Measurement scales, their items
-- (question_scale) and which choices are in a set (answer_option_set) could already be changed by an edit;
-- the option set's own row (option_set: its name and description) couldn't, since only an extraction added
-- them. This lets an edit change public.option_set too: private.apply_draft_items() gains it in the list of
-- tables an edit may change. Nothing else about the function changes.
--
-- ORDER: run this before staff use Option Sets on Make an edit. Until it has run, applying an option set's
-- add, change of name or description, or delete is refused ("this change touches a table it can't change
-- (option_set)") and nothing changes; Measurement Scales, Scale Items and PDF Files work either way.
--
-- After it has run, docs/sql/sql_scripts.sql already builds the same.
--
-- Nothing is lost and no row changes. It is one transaction: if any step fails, nothing changes.
-- =====================================================================================================

begin;

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
    'measurement_scale', 'question_scale', 'variable', 'open_ended', 'closed_ended', 'response_option'
  ];
  loadable constant text[] := array[
    'questionnaire', 'wave', 'component', 'questionnaire_component', 'question', 'questionnaire_question',
    'wave_question', 'variable', 'open_ended', 'closed_ended', 'option_set', 'answer_option',
    'answer_option_set', 'response_option', 'subject'
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

  -- the approval, if it needed one, is recorded with the change (before it is Applied, which takes no more)
  if p_approved_by is not null then
    insert into public.draft_review_event (draft_id, event, staff_id, revision)
    values (d.draft_id, 'Approved', p_approved_by, d.revision);
  end if;

  update public.draft
  set status = 'Applied', applied_at = now(), saved_at = now()
  where draft_id = d.draft_id;
end;
$$;

-- ------ Check ----------------------------------------------------------------------------------------
do $$
begin
  assert position('''subject'', ''option_set'', ''answer_option_set''' in
    pg_get_functiondef('private.apply_draft_items(public.draft, jsonb, bigint)'::regprocedure)) > 0,
    'an edit can change option_set';
end;
$$;

commit;
