-- =====================================================================================================
-- MATR metadata database: a delete is kept as one row, and what it deleted goes for good
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first, then on the real one. Run 9 to 14 first.
--
-- Until now an applied delete was recorded field by field in applied_item, every column of every row it removed
-- with its old value, and the same list stayed in its edit_draft.items: deleting the AHP and KQ waves on the test
-- project wrote 14,396 applied_item rows. From now on:
--
-- 1. private.apply_draft_items() records a delete as ONE applied_item row: the row staff picked to delete (the
--    wave, the questionnaire), with no fields. The site names that row in edit_draft.reach ("deleted": its table,
--    its key, and its name, which the Edit History shows: "Deleted Kid Questionnaire Wave 1"); a delete saved before the site did is recorded by the last row it deleted, which is the one
--    picked (rows go children first). The rows that went with it, and what any of them held, aren't kept, and the
--    delete's edit_draft.items is emptied once it is applied. The draft's sentence still says how many rows went
--    ("Deleted the wave “KQ Wave 1” and 2,120 connected records"). Until it is applied a delete keeps its items
--    as before: approving checks every old value against the database (BR37). Adds, updates and extractions are
--    recorded as before. (Replaced whole; docs/sql/sql_scripts.sql has the same.)
--
-- 2. The deletes already applied are cut down the same way: each keeps its last Delete row, numbered 1, and its
--    edit_draft.items is emptied. edit_draft_guard, which stops an applied draft being changed, is switched off for
--    this, inside this transaction only.
--
-- 3. Comments: applied_item, edit_draft.reach and edit_draft.items say the above; draft.requested_reviewer drops
--    what it said about sign-in.
--
-- LOST: the old values of every row the applied deletes removed, and the keys of every row but the one picked.
--
-- ORDER: either. The new site code names the row picked in edit_draft.reach, which the old function ignores; the
-- old site code doesn't, and this function then records the last row deleted. Edit History reads the one row
-- either way.
--
-- After it has run:
--   * docs/sql/sql_scripts.sql already builds the same.
--   * docs/database_tables.md has the comments; regenerate it with tests/database/describe_schema.R.
--
-- It is one transaction: if any step fails, nothing changes.
-- =====================================================================================================

begin;

-- ------ 1. A delete is recorded as the row picked -----------------------------------------------------------
-- Applies a draft's items to the live tables, records what was done in applied_item, records the approval
-- (if it had one) as a draft_review_event, and marks the draft Applied. One transaction: if anything is
-- refused, nothing changes. Called only by apply_draft() and approve_draft(), after they have checked
-- who may.
--
-- An item names a table (entity_name), a row (record_identifier, the key as JSON, e.g.
-- {"component_id":88,"questionnaire_id":3}), an action (Insert, Update or Delete) and a column with
-- its old and new value, as text. Consecutive items of one action on one row are one statement.
--   Insert  a row; its fields are the key and the items' new values. A key the database numbers
--           itself (a random ID, private.random_id()) is given by the database. An item with no column adds a row
--           that is only its key.
--   Update  the row's columns are set; first each old value is checked against the live row (BR37).
--   Delete  the row goes; first each old value is checked against the live row (BR37).
-- A row the change adds has no number yet and is named new:<table>:<n> (e.g. new:component:1)
-- wherever it is mentioned. It is swapped for the number the database gave when the row was inserted.
-- applied_item records the real numbers.
--
-- What applied_item keeps: an edit that adds or updates, each field, with its old and new value; an extraction, a
-- line for each table it added to; an edit that deletes, one line for the row staff picked to delete (named in
-- edit_draft.reach as "deleted"), and nothing of what it or the rows that went with it held. The delete's own list
-- of items is emptied too, so what was deleted is gone for good.
--
-- Constraints that can be deferred are deferred to the end, so rows can be put in order (display
-- orders) and the answer-format check (BR92) looks at the finished change.
--
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
  deleting     boolean := false;          -- an edit that deletes: recorded as the one row picked ...
  picked       jsonb;                     -- ... {"table", "row"}, from edit_draft.reach ...
  last_table   text;                      -- ... or, if the draft doesn't say, the last row deleted
  last_id      text;
begin
  if jsonb_typeof(p_items) is distinct from 'array' or jsonb_array_length(p_items) = 0 then
    raise exception 'There is nothing in this to apply.';
  end if;

  set constraints all deferred;

  if d.kind = 'Edit' then
    select e.edit_kind = 'Delete', e.reach -> 'deleted' into deleting, picked
    from public.edit_draft e where e.draft_id = d.draft_id;
    deleting := coalesce(deleting, false);
  end if;

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
    if deleting then
      -- a delete is recorded by the row picked (below), not field by field
      if grp.action = 'Delete' then
        last_table := grp.entity_name;
        last_id := recorded_id;
      end if;
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

  -- A delete's one line: the row staff picked (the wave, the questionnaire), with nothing it held. The list of
  -- items it was applied from goes too.
  if deleting then
    if coalesce(picked ->> 'table', last_table) is not null then
      insert into public.applied_item (draft_id, item_number, entity_name, record_identifier, action)
      values (d.draft_id, 1, coalesce(picked ->> 'table', last_table), coalesce(picked ->> 'row', last_id), 'Delete');
    end if;
    update public.edit_draft set items = '[]'::jsonb where draft_id = d.draft_id;
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

-- ------ 2. The deletes already applied, cut down the same way ------------------------------------------------
alter table public.edit_draft disable trigger edit_draft_guard;

with deletes as (
  select d.draft_id
  from public.draft d join public.edit_draft e using (draft_id)
  where d.status = 'Applied' and e.edit_kind = 'Delete'
),
kept as (
  -- the last row each deleted: the one picked, since rows go children first
  select distinct on (a.draft_id) a.draft_id, a.item_number
  from public.applied_item a join deletes using (draft_id)
  where a.action = 'Delete'
  order by a.draft_id, a.item_number desc
)
delete from public.applied_item a
using deletes
where a.draft_id = deletes.draft_id
  and not exists (select 1 from kept where kept.draft_id = a.draft_id and kept.item_number = a.item_number);

update public.applied_item a
set item_number = 1, attribute_name = null, old_value = null, new_value = null
from public.edit_draft e, public.draft d
where e.draft_id = a.draft_id and d.draft_id = a.draft_id and d.status = 'Applied' and e.edit_kind = 'Delete';

update public.edit_draft e
set items = '[]'::jsonb
from public.draft d
where d.draft_id = e.draft_id and d.status = 'Applied' and e.edit_kind = 'Delete';

alter table public.edit_draft enable trigger edit_draft_guard;

-- ------ 3. Comments --------------------------------------------------------------------------------------------
comment on table public.applied_item            is 'What an applied draft did: for an edit that adds or updates, one field it changed, with its old and new value; for an extraction, one table it added rows to; for an edit that deletes, the one row staff picked to delete (what it and the rows that went with it held isn''t kept).';
comment on column public.edit_draft.reach is 'For a delete: how many rows go with it (rows_removed), how many are emptied (rows_emptied), and the row staff picked to delete (deleted: {table, row, name}: its key as JSON, and its name as the Edit History shows it), which is all that is kept of it.';
comment on column public.edit_draft.items is
  'The rows and fields it changes, in the order to apply them: item_number, entity_name, record_identifier, action, attribute_name, old_value, new_value. Emptied once a delete is applied.';
comment on column public.draft.requested_reviewer is
  'The staff member the preparer asked to review it. Any other eligible staff member may still approve.';

commit;

notify pgrst, 'reload schema';
