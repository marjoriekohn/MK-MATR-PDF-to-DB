-- Answer choices worded one way, and questions that differ only in who they ask about.
-- For a database built before these were added to docs/sql/1_setup.sql (which builds them for a new one).
-- Adds question_stem (and question.question_stem_id), answer_option_alias, the subjects Twin A and Twin B,
-- and lets changes add to the new tables (private.apply_draft_items()). Nothing already there changes.

begin;

alter table public.question add column question_stem_id bigint;

create table public.question_stem (
  question_stem_id bigint primary key default private.random_id('question_stem', 'question_stem_id'),
  text             text   not null
    constraint question_stem_text_not_blank check (btrim(text) <> '')
);

create table public.answer_option_alias (
  alias            text   primary key
    constraint answer_option_alias_normal check (alias = private.normal_answer(alias)),
  answer_option_id bigint not null
);

alter table public.question
  add constraint question_question_stem_id_fkey
    foreign key (question_stem_id) references public.question_stem (question_stem_id) on delete set null;
alter table public.answer_option_alias
  add constraint answer_option_alias_answer_option_id_fkey
    foreign key (answer_option_id) references public.answer_option (answer_option_id) on delete cascade;

create unique index question_stem_text_unique on public.question_stem (btrim(regexp_replace(text, '\s+', ' ', 'g')));
create index answer_option_alias_answer_option_idx on public.answer_option_alias (answer_option_id);
create index question_question_stem_idx            on public.question (question_stem_id);

alter table public.question_stem       enable row level security;
alter table public.answer_option_alias enable row level security;
revoke all on public.question_stem, public.answer_option_alias from anon, authenticated;
create policy "Dashboard can read" on public.question_stem       for select to anon, authenticated using (true);
create policy "Dashboard can read" on public.answer_option_alias for select to anon, authenticated using (true);
grant select on public.question_stem, public.answer_option_alias to anon, authenticated;
grant select, insert, update, delete on public.question_stem, public.answer_option_alias to service_role;

comment on table public.question_stem is 'What questions that differ only in who they ask about share, e.g. "Has [subject] ever repeated a grade?". [subject] is filled in with each variable''s subject when shown.';
comment on column public.question.question_stem_id is 'The stem this question shares with the questions that differ from it only in who they ask about (Twin A, Twin B). Empty when it shares none.';
comment on table public.answer_option is 'One answer choice, stored once for the whole registry. [subject] in its words stands for the variable''s subject ("describes [subject] very well").';
comment on table public.answer_option_alias is 'A wording merged into an answer choice on Make an edit. An extraction that reads it uses the choice instead.';

insert into public.subject (name)
select v.name from (values ('Twin A'), ('Twin B')) as v(name)
where not exists (select 1 from public.subject s where lower(btrim(s.name)) = lower(v.name));

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
    'measurement_scale', 'question_scale', 'variable', 'open_ended', 'closed_ended', 'response_option', 'response_option_count',
    'question_stem', 'answer_option_alias'
  ];
  loadable constant text[] := array[
    'questionnaire', 'wave', 'component', 'questionnaire_component', 'question', 'questionnaire_question',
    'wave_question', 'variable', 'open_ended', 'closed_ended', 'option_set', 'answer_option',
    'answer_option_set', 'response_option', 'response_option_count', 'subject', 'question_stem'
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

  -- An extraction's wave keeps the PDF and the file of Variable Names and Coding it was read from, so they stay
  -- with it after the draft is gone (a CSV or text file by its text)
  if d.kind = 'Extraction' then
    insert into public.pdf_file (pdf_sha256, pdf_bytes)
    select x.supplement ->> 'sha256', (x.supplement ->> 'bytes')::bigint
    from public.extracted_draft x
    where x.draft_id = d.draft_id and x.supplement ->> 'sha256' is not null
    on conflict (pdf_sha256) do nothing;
    insert into public.wave_file as f (wave_id, pdf_sha256, pdf_name, supplement_name, supplement_sha256, supplement_text)
    select w.wave_id, x.pdf_sha256, x.pdf_name, x.supplement ->> 'name', x.supplement ->> 'sha256', x.supplement ->> 'text'
    from public.extracted_draft x
    join public.wave w on w.wave_id = case when x.info ->> 'wave_id' ~ '^[0-9]+$' then (x.info ->> 'wave_id')::bigint end
    where x.draft_id = d.draft_id
    on conflict (wave_id) do update
      set pdf_sha256 = excluded.pdf_sha256, pdf_name = excluded.pdf_name, supplement_name = excluded.supplement_name,
          supplement_sha256 = excluded.supplement_sha256, supplement_text = excluded.supplement_text;
  end if;

  update public.draft
  set status = 'Applied', applied_at = now(), saved_at = now()
  where draft_id = d.draft_id;
end;
$$;


commit;

notify pgrst, 'reload schema';
