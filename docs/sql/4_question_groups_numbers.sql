-- =====================================================================================================
-- Migration 2026-10-21: question groups, printed and sequential question numbers, new subjects from an
-- extraction, and what the reviewer changed measured against the AI's answer.
--
--   1. component_type "Question Group": a container with no words of its own (a battery, a definition
--      with its question, a question with its follow-up). Sections keep their printed heading.
--   2. wave_question.question_number is now the question's place among the questions (1, 2, 3...,
--      worked out by the site, sections and question groups not counted), still unique in a wave.
--      The number printed beside it goes in the new wave_question.printed_number, which can repeat
--      (each section can start again at 1) or be empty.
--   3. An extraction can add a subject (who a variable is about) that staff typed: private.apply_draft_items()
--      lets an Extraction insert into public.subject.
--   4. ai_extracted_draft.ai_original, once written, can only be replaced while the PDF is read again and
--      before staff have saved a reviewed copy, so what the reviewer changed is always measured against the
--      answer they reviewed.
--   5. Descriptions brought up to date (review_edits is how the reviewed copy differs from ai_original,
--      worked out again at each save; ai_original is the AI's answer as tidied by the reading job).
--
-- Safe to run on a database with data: it adds, and changes no rows. Run it whole, in one transaction.
-- docs/sql/sql_scripts.sql builds the same database from scratch.
-- =====================================================================================================
begin;

-- 1. Question groups --------------------------------------------------------------------------------
insert into public.component_type (name, description) values
  ('Question Group', 'Holds blocks that belong together but have no printed heading: a battery, a definition with its question, a question with its follow-up. Has no text.')
on conflict (name) do nothing;

comment on column public.component.text is 'The printed words. Empty only for a Question Group, which has none (BR42, site).';

-- 2. Printed and sequential question numbers ------------------------------------------------------------
alter table public.wave_question add column if not exists printed_number text;

comment on table public.wave_question is
  'That a wave actually asked a question: its place among the wave''s questions, and the number printed beside it.';
comment on column public.wave_question.question_number is
  'The question''s place among the questions the wave asks (1, 2, 3...), in printed order; sections and question groups aren''t counted. Unique in a wave. Worked out by the site, not typed.';
comment on column public.wave_question.printed_number is
  'The number or label printed beside the question ("1a", "12", "Q7"), as text. Can repeat in a wave (each section can start again at 1). Empty when none is printed.';

-- 3. An extraction can add subjects -------------------------------------------------------------------
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
    'wave_question', 'category', 'question_category', 'answer_option', 'subject', 'answer_option_set',
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

-- 4. ai_original is the answer the reviewer reviewed --------------------------------------------------
create or replace function public.ai_original_guard()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  -- the first answer can always be written; a later one only while the PDF is read again, before
  -- anyone has saved a reviewed copy
  if new.ai_original is distinct from old.ai_original and old.ai_original is not null and (
    old.reading_status not in ('queued', 'sent', 'received')
    or exists (select 1 from public.extracted_draft e where e.draft_id = old.draft_id and e.questionnaire is not null)
  ) then
    raise exception 'What the AI returned is kept as it was read, so what the reviewer changed can be measured. Read the PDF again only before anyone saves the draft.';
  end if;
  return new;
end;
$$;

drop trigger if exists ai_extracted_draft_original_kept on public.ai_extracted_draft;
create trigger ai_extracted_draft_original_kept
  before update of ai_original on public.ai_extracted_draft
  for each row execute function public.ai_original_guard();

-- 5. Descriptions ------------------------------------------------------------------------------------
comment on column public.ai_extracted_draft.ai_original is
  'The questionnaire as the AI returned it, tidied by the reading job (spaces, quotes, trailing full stops on numbers). Written by the reading job before anyone saves the draft, and never changed after, so what the reviewer changed can be measured.';
comment on column public.ai_extracted_draft.review_edits is
  'How the reviewed copy differs from ai_original, worked out again (not added to) each time staff save: [{change, item, ref, where, field, before, after}]. A value changed back is no difference.';

commit;
