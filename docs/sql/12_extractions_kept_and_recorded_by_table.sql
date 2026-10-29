-- =====================================================================================================
-- MATR metadata database: applied extractions are kept whole, and recorded in applied_item a table at a time
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one. Run 9, 10 and 11 first.
--
-- 1. An applied extraction's draft is kept: extracted_draft and ai_extracted_draft, with what the AI read
--    (ai_original) and how staff changed it (review_edits), for analysis. The test project ran an earlier
--    9_integrity_rules.sql that deleted them as the extraction was applied: that trigger goes, and
--    draft_detail_guard is again as it was before 9 (an applied draft's details can't be changed or deleted).
--    On the real database, where the current 9 never made the trigger, this part changes nothing.
--
-- 2. An applied extraction was recorded in applied_item field by field: about 6,000 rows for one wave of a
--    questionnaire. Now it has one row for each table it added rows to: record_identifier is a JSON array of
--    the keys of every row it added there (the numbers the database gave them), in the order they were added.
--    What it put in them is in its extraction draft, kept by part 1. An edit is still recorded field by field.
--    private.apply_draft_items() is replaced whole (docs/sql/sql_scripts.sql has the same).
--
-- 3. Extractions applied before this are recorded the same way: their field-by-field rows are replaced by a
--    row for each table.
--
-- LOST: the field-by-field rows of extractions already applied (part 3): the values they held are in the
-- MATR tables, and, for an extraction whose draft is still there, in its draft. The keys of every row each
-- extraction added stay. Nothing else changes. On the test project, the drafts of extractions applied after
-- the earlier 9 had already gone; they can't come back.
--
-- ORDER: run it, then put the site's new code in place (Edit History reads the new rows; the old code shows
-- them as one row a table, keyed by a list).
--
-- It is one transaction: if any step fails, nothing changes. docs/sql/sql_scripts.sql already builds the same.
-- =====================================================================================================

begin;

-- ------ 1. Applied extraction drafts are kept --------------------------------------------------------------
drop trigger if exists draft_applied_extraction_let_go on public.draft;
drop function if exists public.draft_applied_extraction_let_go();

-- The details of an applied draft stay as they were applied: an extraction's too, with what the AI read
-- (ai_original) and how staff changed it (review_edits), kept for analysis.
create or replace function public.draft_detail_guard()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if exists (select 1 from public.draft where draft_id = old.draft_id and status = 'Applied') then
    raise exception 'That change has already been applied, so it can''t be changed.';
  end if;
  if tg_op = 'DELETE' then
    return old;
  end if;
  return new;
end;
$$;

-- ------ 2. An extraction is recorded a table at a time -------------------------------------------------------
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
    insert into public.draft_review_event (draft_id, event, staff_id, revision)
    values (d.draft_id, 'Approved', p_approved_by, d.revision);
  end if;

  update public.draft
  set status = 'Applied', applied_at = now(), saved_at = now()
  where draft_id = d.draft_id;
end;
$$;

comment on table public.applied_item is
  'What an applied draft did: for an edit, one field it changed, with its old and new value; for an extraction, one table it added rows to.';
comment on column public.applied_item.record_identifier is
  'The row''s key as JSON, with the numbers the database gave rows the draft added. For an extraction, a JSON array of the keys of every row it added to the table.';

-- ------ 3. Extractions already applied, recorded the same way ---------------------------------------------------
-- Each row an extraction added, once, at its first item; then a line for each table, in the order it was added to.
create temp table extraction_lines on commit drop as
with added as (
  select a.draft_id, a.entity_name, a.record_identifier, min(a.item_number) as first_at
  from public.applied_item a
  join public.draft d using (draft_id)
  where d.kind = 'Extraction' and a.record_identifier not like '[%'
  group by a.draft_id, a.entity_name, a.record_identifier
)
select draft_id, entity_name,
       '[' || string_agg(record_identifier, ',' order by first_at) || ']' as record_identifier,
       min(first_at) as first_at
from added
group by draft_id, entity_name;

delete from public.applied_item a
using public.draft d
where d.draft_id = a.draft_id and d.kind = 'Extraction' and a.record_identifier not like '[%';

insert into public.applied_item (draft_id, item_number, entity_name, record_identifier, action)
select draft_id, row_number() over (partition by draft_id order by first_at), entity_name, record_identifier, 'Insert'
from extraction_lines;

-- ------ Check ----------------------------------------------------------------------------------------
do $$
begin
  assert to_regprocedure('public.draft_applied_extraction_let_go()') is null, 'an applied extraction''s draft is kept';
  assert position('added_tables' in
    pg_get_functiondef('private.apply_draft_items(public.draft, jsonb, bigint)'::regprocedure)) > 0,
    'an extraction is recorded a table at a time';
  assert not exists (
    select 1 from public.applied_item a join public.draft d using (draft_id)
    where d.kind = 'Extraction' and a.record_identifier not like '[%'
  ), 'every applied extraction is recorded a table at a time';
end;
$$;

commit;
