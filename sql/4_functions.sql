-- =====================================================================================================
-- MATR metadata database, script 4 of 8: FUNCTIONS
-- =====================================================================================================
-- Run after 3_indexes.sql; can be run again at any time (it replaces every function and trigger).
--
--   4a. Rules a constraint can't express        triggers on the tables
--   4b. Helpers                                 private.*: not reachable through the REST API
--   4c. What the site calls (through /rpc/)     public.push_extraction, reopen_wave, apply_change, empty_*
--   4d. What the dashboard calls                public.upvote_questionnaire, public.unstar_questionnaire
--   4e. Who is staff                            private.is_active_staff, for Row Level Security
--
-- Everything that changes several tables at once is a function, because separate REST calls can't share
-- a transaction. The site saves, claims and discards an extraction draft with plain REST calls on the one
-- extraction table; only pushing it is a function.

begin;

-- ------ A database built before a scale's items lost their numbers ----------------------------------------
-- A question is in a scale or it isn't, in no order (1_tables.sql), so question_scale has no item_number. A
-- database built with the column loses it here, with the check and the unique rule that were on it.
alter table public.question_scale drop column if exists item_number;

-- =====================================================================================================
-- 4a. RULES A CONSTRAINT CAN'T EXPRESS
-- =====================================================================================================

-- A placement sits inside a Section, Grid or Group (or at the top); a Grid holds no Sections, Grids or Groups;
-- a dependency links one Question to an earlier Question.
create or replace function public.placement_rules()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  my_kind     text;
  parent_kind text;
  depend_kind text;
begin
  select kind into my_kind from public.component where component_id = new.component_id;
  if new.parent_placement_id is not null then
    select c.kind into parent_kind
    from public.placement p join public.component c on c.component_id = p.component_id
    where p.placement_id = new.parent_placement_id;
    if parent_kind not in ('Section', 'Grid', 'Group') then
      raise exception 'A % can''t hold other blocks: only a Section, Grid or Group can.', parent_kind;
    end if;
    if parent_kind = 'Grid' and my_kind in ('Section', 'Grid', 'Group') then
      raise exception 'A Grid holds only questions and remarks, not a %.', my_kind;
    end if;
  end if;
  if new.depends_on_placement_id is not null then
    if my_kind <> 'Question' then
      raise exception 'Only a Question can depend on another question; this is a %.', my_kind;
    end if;
    select c.kind into depend_kind
    from public.placement p join public.component c on c.component_id = p.component_id
    where p.placement_id = new.depends_on_placement_id;
    if depend_kind is distinct from 'Question' then
      raise exception 'A question can only depend on another Question, not a %.', coalesce(depend_kind, 'missing block');
    end if;
  end if;
  return new;
end;
$$;

drop trigger if exists placement_rules on public.placement;
create trigger placement_rules
  before insert or update of component_id, parent_placement_id, depends_on_placement_id on public.placement
  for each row execute function public.placement_rules();

-- A block can't be inside itself, however far down.
create or replace function public.placement_no_cycle()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  here  bigint := new.parent_placement_id;
  steps integer := 0;
begin
  while here is not null loop
    if here = new.placement_id then
      raise exception 'A block can''t be inside itself.';
    end if;
    select parent_placement_id into here from public.placement where placement_id = here;
    steps := steps + 1;
    if steps > 1000 then
      raise exception 'The blocks are nested too deep.';
    end if;
  end loop;
  return new;
end;
$$;

drop trigger if exists placement_no_cycle on public.placement;
create trigger placement_no_cycle
  after insert or update of parent_placement_id on public.placement
  for each row execute function public.placement_no_cycle();

-- Only a Question is asked in a wave or records variables.
create or replace function public.placement_is_question()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  the_kind text;
begin
  select c.kind into the_kind
  from public.placement p join public.component c on c.component_id = p.component_id
  where p.placement_id = new.placement_id;
  if the_kind is distinct from 'Question' then
    raise exception 'Only a Question can be asked or have variables; this block is a %.', coalesce(the_kind, 'missing block');
  end if;
  return new;
end;
$$;

drop trigger if exists wave_question_is_question on public.wave_question;
create trigger wave_question_is_question
  before insert or update of placement_id on public.wave_question
  for each row execute function public.placement_is_question();

drop trigger if exists variable_on_question on public.variable;
create trigger variable_on_question
  before insert or update of placement_id on public.variable
  for each row execute function public.placement_is_question();

-- A closed-ended variable has at least one response option, and an open-ended one none. Checked when the
-- transaction ends, so a variable and its options can go in together.
create or replace function public.variable_choices_check()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  the_key  bigint := case when tg_table_name = 'variable' then new.variable_key else coalesce(old.variable_key, new.variable_key) end;
  the_type text;
  n        integer;
begin
  select variable_type into the_type from public.variable where variable_key = the_key;
  if the_type is null then
    return null;   -- the variable went with the change
  end if;
  select count(*) into n from public.response_option where variable_key = the_key;
  if the_type = 'closed_ended' and n = 0 then
    raise exception 'A closed-ended variable needs at least one response option (variable %).', the_key;
  end if;
  if the_type = 'open_ended' and n > 0 then
    raise exception 'An open-ended variable has no response options (variable %).', the_key;
  end if;
  return null;
end;
$$;

drop trigger if exists variable_choices_check on public.variable;
create constraint trigger variable_choices_check
  after insert or update of variable_type on public.variable
  deferrable initially deferred
  for each row execute function public.variable_choices_check();

drop trigger if exists response_option_choices_check on public.response_option;
create constraint trigger response_option_choices_check
  after insert or delete or update of variable_key on public.response_option
  deferrable initially deferred
  for each row execute function public.variable_choices_check();

-- Staff are never deleted: the history names them. Mark them inactive instead.
create or replace function public.staff_never_deleted()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  raise exception 'Staff members are never deleted (the history names them). Set active to false instead.';
end;
$$;

drop trigger if exists staff_never_deleted on public.staff;
create trigger staff_never_deleted
  before delete on public.staff
  for each row execute function public.staff_never_deleted();

-- A save by staff raises the extraction's revision and says when; the reading job's columns don't. Once
-- pushed, what was read and reviewed is kept as it was (its summary and who has it open may still change).
create or replace function public.extraction_touch()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  content_changed boolean :=
    new.questionnaire is distinct from old.questionnaire or new.info is distinct from old.info
    or new.matr_csv is distinct from old.matr_csv or new.supplement is distinct from old.supplement
    or new.pdf_sha256 is distinct from old.pdf_sha256 or new.pdf_name is distinct from old.pdf_name;
  staff_saved boolean := content_changed or new.summary is distinct from old.summary;
begin
  if old.status = 'Pushed' then
    if new.status <> 'Pushed' then
      raise exception 'A pushed extraction stays pushed. Empty its wave and extract again instead.';
    end if;
    if content_changed or new.ai_original is distinct from old.ai_original or new.review_edits is distinct from old.review_edits then
      raise exception 'A pushed extraction can''t be changed: it is the record of what was read and reviewed.';
    end if;
    return new;
  end if;
  if staff_saved then
    new.revision := old.revision + 1;
    new.saved_at := now();
  end if;
  return new;
end;
$$;

drop trigger if exists extraction_touch on public.extraction;
create trigger extraction_touch
  before update on public.extraction
  for each row execute function public.extraction_touch();

-- The history is only ever added to.
create or replace function public.history_is_kept()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  raise exception 'The history is never changed or deleted.';
end;
$$;

drop trigger if exists history_is_kept on public.history;
create trigger history_is_kept
  before update or delete on public.history
  for each row execute function public.history_is_kept();


-- =====================================================================================================
-- 4b. HELPERS (private)
-- =====================================================================================================

-- The staff member a change is made by: must be there and active.
create or replace function private.require_staff(p_staff_id bigint)
returns void
language plpgsql
stable
set search_path = public, pg_temp
as $$
begin
  if not exists (select 1 from public.staff where staff_id = p_staff_id and active) then
    raise exception 'Only an active staff member can change the catalog (staff % is not one).', coalesce(p_staff_id::text, 'unknown');
  end if;
end;
$$;

-- A note of what a push did, kept in the temp table push_log while a push runs (nothing otherwise): what kind
-- of row (p_what), whether it was in the database already (p_reused), which row (p_id, so a row used many times
-- is counted once) and its words (p_name, for the kinds the preview lists by name).
drop function if exists private.push_note(text, boolean);
create or replace function private.push_note(p_what text, p_reused boolean, p_id bigint default null, p_name text default null)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if to_regclass('pg_temp.push_log') is not null then
    insert into pg_temp.push_log (what, reused, id, name) values (p_what, p_reused, p_id, p_name);
  end if;
end;
$$;

-- The tag of a kind and name, added if the list hasn't got it. Names are matched ignoring case and outer
-- spaces; curly quotes are written straight.
create or replace function private.tag_id(p_kind text, p_name text)
returns bigint
language plpgsql
set search_path = public, pg_temp
as $$
declare
  the_name text := private.squish(translate(p_name, E'‘’“”', '''''""'));
  the_id   bigint;
begin
  if the_name is null or the_name = '' then
    return null;
  end if;
  select tag_id into the_id from public.tag where kind = p_kind and lower(btrim(name)) = lower(the_name);
  if the_id is null then
    insert into public.tag (kind, name) values (p_kind, the_name) returning tag_id into the_id;
    perform private.push_note('tag', false, the_id, the_name);
  else
    perform private.push_note('tag', true, the_id, the_name);
  end if;
  return the_id;
end;
$$;

-- The answer option with these words (in their normal form), added if there isn't one.
create or replace function private.answer_option_id(p_label text)
returns bigint
language plpgsql
set search_path = public, pg_temp
as $$
declare
  normal text := private.normal_answer(p_label);
  the_id bigint;
begin
  if normal is null then
    raise exception 'A response option has no words.';
  end if;
  select answer_option_id into the_id from public.answer_option where standardized_label = normal;
  if the_id is null then
    insert into public.answer_option (standardized_label) values (normal) returning answer_option_id into the_id;
    perform private.push_note('answer_option', false, the_id, normal);
  else
    perform private.push_note('answer_option', true, the_id, normal);
  end if;
  return the_id;
end;
$$;

-- The measurement scale a question's `scale` names: the one with that abbreviation, or else that name, ignoring
-- case and outer spaces. A scale the database hasn't got is added, named by its abbreviation until staff give
-- it its full name (Make an edit).
create or replace function private.scale_id(p_scale text)
returns bigint
language plpgsql
set search_path = public, pg_temp
as $$
declare
  the_words text := private.squish(p_scale);
  the_id    bigint;
begin
  if the_words is null or the_words = '' then
    return null;
  end if;
  select scale_id into the_id from public.measurement_scale
  where lower(btrim(abbreviation)) = lower(the_words) or lower(btrim(name)) = lower(the_words)
  order by (lower(btrim(abbreviation)) = lower(the_words)) desc
  limit 1;
  if the_id is null then
    insert into public.measurement_scale (name, abbreviation) values (the_words, the_words) returning scale_id into the_id;
    perform private.push_note('measurement_scale', false, the_id, the_words);
  else
    perform private.push_note('measurement_scale', true, the_id,
                              (select abbreviation from public.measurement_scale where scale_id = the_id));
  end if;
  return the_id;
end;
$$;

-- The tags an extraction's JSON puts on a question (a block), a variable or a response option: its `subject`,
-- `time_frame` and `unit`, where it has them.
create or replace function private.push_tags(p_item jsonb, p_placement_id bigint, p_variable_key bigint, p_answer_option_id bigint)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  field  record;
  the_id bigint;
begin
  for field in select * from (values ('subject', 'subject'), ('time_frame', 'time frame'), ('unit', 'unit')) as f(key, kind) loop
    the_id := private.tag_id(field.kind, p_item ->> field.key);
    if the_id is null then
      continue;
    end if;
    if p_answer_option_id is not null then
      insert into public.response_option_tag (variable_key, answer_option_id, tag_id) values (p_variable_key, p_answer_option_id, the_id)
      on conflict do nothing;
    elsif p_variable_key is not null then
      insert into public.variable_tag (variable_key, tag_id) values (p_variable_key, the_id) on conflict do nothing;
    else
      insert into public.placement_tag (placement_id, tag_id) values (p_placement_id, the_id) on conflict do nothing;
    end if;
  end loop;
end;
$$;

-- A question's variables, from its block's `variables`. A variable whose MATR ID the questionnaire records
-- already (from an earlier wave) is reused if it is this question's; a new one is added after them. A reused
-- variable keeps what it has, unless the push updates a wave reopened for editing (p_update): a variable is the
-- questionnaire's, not a wave's, so staff's corrections to it (its kind, label, kind of answer, tags and choices
-- with their codes) go in for every wave that asks its question, and a variable staff took out of the question goes.
drop function if exists private.push_variables(jsonb, bigint, bigint);
create or replace function private.push_variables(p_block jsonb, p_questionnaire_id bigint, p_placement_id bigint,
                                                  p_update boolean default false)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  words      text := coalesce(p_block ->> 'text', '');
  v          jsonb;
  c          jsonb;
  the_matr   text;
  the_type   text;
  the_data   text;
  the_key    bigint;
  placed_at  bigint;
  next_order integer;
  n_choices  integer;
  the_option bigint;
  the_code   text;
begin
  select coalesce(max(display_order), 0) into next_order from public.variable where placement_id = p_placement_id;
  for v in select value from jsonb_array_elements(coalesce(p_block -> 'variables', '[]'::jsonb)) loop
    the_matr := nullif(btrim(v ->> 'matr_id'), '');
    if the_matr is null then
      raise exception 'A variable of the question "%" has no MATR ID.', words;
    end if;
    the_type := v ->> 'variable_type';
    if the_type is null or the_type not in ('open_ended', 'closed_ended') then
      raise exception 'The variable % of the question "%" is neither open-ended nor closed-ended.', the_matr, words;
    end if;
    select variable_key, placement_id into the_key, placed_at from public.variable
    where questionnaire_id = p_questionnaire_id and matr_id = the_matr;
    if the_key is not null then
      if placed_at <> p_placement_id then
        raise exception 'The MATR ID % is already used by another question of this questionnaire. Give the variable a MATR ID of its own.', the_matr;
      end if;
      perform private.push_note('variable', true, the_key);
      if not p_update then
        continue;
      end if;
    end if;
    the_data := case when the_type = 'open_ended' then nullif(btrim(v ->> 'answer_data_type'), '') end;
    if the_type = 'open_ended' and the_data is null then
      raise exception 'The open-ended variable % of the question "%" doesn''t say what kind of answer it takes (NUMBER, TEXT, DATE or TIME).', the_matr, words;
    end if;
    if the_key is null then
      next_order := next_order + 1;
      insert into public.variable (questionnaire_id, placement_id, matr_id, display_order, label, variable_type, answer_data_type)
      values (p_questionnaire_id, p_placement_id, the_matr, next_order, nullif(private.squish(v ->> 'label'), ''), the_type, the_data)
      returning variable_key into the_key;
      perform private.push_note('variable', false, the_key);
    else
      -- the update of a reopened wave: the variable as staff left it, its tags and choices put in again below
      update public.variable set label = nullif(private.squish(v ->> 'label'), ''), variable_type = the_type, answer_data_type = the_data
      where variable_key = the_key;
      delete from public.variable_tag where variable_key = the_key;
      delete from public.response_option where variable_key = the_key;
    end if;
    perform private.push_tags(v, null, the_key, null);

    if the_type = 'closed_ended' then
      n_choices := 0;
      for c in select value from jsonb_array_elements(coalesce(v -> 'response_options', '[]'::jsonb)) loop
        n_choices := n_choices + 1;
        the_option := private.answer_option_id(c ->> 'answer_option');
        the_code := nullif(btrim(c ->> 'code_value'), '');
        if the_code is null then
          raise exception 'The choice "%" of the variable % has no code.', c ->> 'answer_option', the_matr;
        end if;
        if exists (select 1 from public.response_option where variable_key = the_key and answer_option_id = the_option) then
          raise exception 'The variable % lists the choice "%" twice.', the_matr, c ->> 'answer_option';
        end if;
        insert into public.response_option (variable_key, answer_option_id, display_order, code_value)
        values (the_key, the_option, n_choices, the_code);
        perform private.push_note('response_option', false);
        perform private.push_tags(c, null, the_key, the_option);
      end loop;
      if n_choices = 0 then
        raise exception 'The closed-ended variable % of the question "%" has no choices.', the_matr, words;
      end if;
    end if;
  end loop;
  if p_update then
    -- a variable staff took out of the question goes, for every wave that asks it
    delete from public.variable
    where placement_id = p_placement_id
      and matr_id <> all (array(
        select btrim(x ->> 'matr_id') from jsonb_array_elements(coalesce(p_block -> 'variables', '[]'::jsonb)) as x
      ));
  end if;
end;
$$;

-- One block of an extraction's JSON and everything under it, into the catalog.
-- The component is kept once per kind and words. A placement the questionnaire has already (an earlier wave
-- asked it) with this component under the same parent is reused, once per push; otherwise a new placement goes
-- after the parent's others. A Question is marked asked in the wave with its numbers, and depends on the
-- question its `parent_question` names (by question number, the question's place among the wave's). A Question
-- whose block names a measurement scale (`scale`: one, or several with commas between) has its wording put in
-- each (question_scale): the wording is the scale's item wherever it is printed, so a push only ever adds one;
-- staff take one out on Make an edit.
drop function if exists private.push_block(jsonb, bigint, bigint, bigint);
create or replace function private.push_block(p_block jsonb, p_questionnaire_id bigint, p_wave_id bigint, p_parent_id bigint,
                                              p_update boolean default false)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  the_kind      text := p_block ->> 'component_type';
  the_words     text := nullif(private.squish(p_block ->> 'text'), '');
  the_component bigint;
  the_placement bigint;
  worded        boolean := false; -- the database had this wording (its component) already
  reused        boolean := false; -- the questionnaire printed it here already (its placement)
  depends_on    bigint;
  a_scale       text;
  the_scale     bigint;
  added         integer;
  child         jsonb;
begin
  if the_kind is null then
    raise exception 'A block has no kind (component_type).';
  end if;
  if the_kind = 'Group' then
    the_words := null;
  elsif the_words is null then
    raise exception 'A % has no words.', the_kind;
  end if;

  select component_id into the_component from public.component c
  where c.kind = the_kind and ((the_words is null and c.text is null) or private.squish(c.text) = the_words);
  if the_component is null then
    insert into public.component (kind, text) values (the_kind, the_words) returning component_id into the_component;
    perform private.push_note('component', false, the_component);
  else
    worded := true;
    perform private.push_note('component', true, the_component);
  end if;
  -- a question's wording, by name: the preview says which questions the database has already
  if the_kind = 'Question' then
    perform private.push_note('question', worded, the_component, the_words);
  end if;

  select pl.placement_id into the_placement from public.placement pl
  where pl.questionnaire_id = p_questionnaire_id and pl.component_id = the_component
    and pl.parent_placement_id is not distinct from p_parent_id
    and not exists (select 1 from pg_temp.push_placed s where s.placement_id = pl.placement_id)
  order by pl.display_order
  limit 1;
  if the_placement is null then
    insert into public.placement (questionnaire_id, component_id, parent_placement_id, display_order)
    select p_questionnaire_id, the_component, p_parent_id, coalesce(max(display_order), 0) + 1
    from public.placement where questionnaire_id = p_questionnaire_id and parent_placement_id is not distinct from p_parent_id
    returning placement_id into the_placement;
    perform private.push_note('placement', false, the_placement);
  else
    reused := true;
    perform private.push_note('placement', true, the_placement);
  end if;
  insert into pg_temp.push_placed (placement_id, kind, question_number)
  values (the_placement, the_kind, nullif(btrim(p_block ->> 'question_number'), ''));

  if the_kind = 'Question' then
    if nullif(btrim(p_block ->> 'parent_question'), '') is not null then
      select s.placement_id into depends_on from pg_temp.push_placed s
      where s.kind = 'Question' and s.question_number = btrim(p_block ->> 'parent_question') and s.placement_id <> the_placement
      order by s.seq desc
      limit 1;
      if depends_on is not null and not reused then
        update public.placement set depends_on_placement_id = depends_on where placement_id = the_placement;
      end if;
    end if;
    if reused and p_update then
      -- a question another wave asks too, updated: the question it depends on, and its tags, are as staff left them
      update public.placement set depends_on_placement_id = depends_on where placement_id = the_placement;
      delete from public.placement_tag where placement_id = the_placement;
    end if;
    if nullif(btrim(p_block ->> 'question_number'), '') is null then
      raise exception 'The question "%" has no question number.', the_words;
    end if;
    insert into public.wave_question (questionnaire_id, wave_id, placement_id, question_number, printed_number)
    values (p_questionnaire_id, p_wave_id, the_placement, btrim(p_block ->> 'question_number'), nullif(btrim(p_block ->> 'printed_number'), ''));
    perform private.push_note('wave_question', false);
    perform private.push_variables(p_block, p_questionnaire_id, the_placement, p_update);
    perform private.push_tags(p_block, the_placement, null, null);
    -- its measurement scales: one, or several with commas between ("FTND, CDS-12")
    for a_scale in
      select btrim(named) from regexp_split_to_table(coalesce(p_block ->> 'scale', ''), '[,;]') as named where btrim(named) <> ''
    loop
      the_scale := private.scale_id(a_scale);
      insert into public.question_scale (scale_id, component_id) values (the_scale, the_component) on conflict do nothing;
      get diagnostics added = row_count;
      perform private.push_note('scale_item', added = 0, the_component);
    end loop;
  end if;

  for child in select value from jsonb_array_elements(coalesce(p_block -> 'children', '[]'::jsonb)) loop
    perform private.push_block(child, p_questionnaire_id, p_wave_id, the_placement, p_update);
  end loop;
end;
$$;

-- The push itself; public.push_extraction() wraps it.
create or replace function private.push_run(e public.extraction, p_staff_id bigint)
returns jsonb
language plpgsql
set search_path = public, pg_temp
as $$
declare
  w        public.wave;
  q        public.questionnaire;
  block    jsonb;
  counts   jsonb;
  asked    integer;
  updating boolean;
  printed  bigint[];
  asked_here bigint[]; -- the placements of the questions the wave asked, for an update
begin
  if e.questionnaire is null then
    raise exception 'Nothing has been saved for this extraction yet.';
  end if;
  if coalesce(e.info ->> 'wave_id', '') !~ '^[0-9]+$' then
    raise exception 'Pick the wave this extraction goes into first (Basic Info).';
  end if;
  select * into w from public.wave where wave_id = (e.info ->> 'wave_id')::bigint;
  if not found then
    raise exception 'The wave this extraction was to go into is no longer in the database. Pick another (Basic Info).';
  end if;
  select * into q from public.questionnaire where questionnaire_id = w.questionnaire_id;
  if coalesce(e.info ->> 'questionnaire_id', '') ~ '^[0-9]+$' and (e.info ->> 'questionnaire_id')::bigint <> w.questionnaire_id then
    raise exception 'The wave picked (%) isn''t one of the questionnaire picked. Pick them again (Basic Info).', w.name;
  end if;
  select count(*) into asked from public.wave_question where wave_id = w.wave_id;
  -- an extraction reopened from this wave (public.reopen_wave) replaces what the wave asks
  updating := asked > 0 and coalesce(e.info ->> 'updates_wave_id', '') = w.wave_id::text; -- never null: any other draft is refused
  if asked > 0 and not updating then
    raise exception 'The wave % of % already asks % question(s). Empty it first (Metadata > the wave > Empty), or pick a wave that asks none.', w.name, q.name, asked;
  end if;
  -- the option sets the database has before anything is pushed (or, for an update, removed): see the tally below
  drop table if exists pg_temp.push_sets_before;
  create temp table push_sets_before on commit drop as
  select distinct choices
  from (select array_agg(answer_option_id order by answer_option_id) as choices from public.response_option group by variable_key) as offered;

  if updating then
    -- wordings staff changed: everywhere they are printed where staff chose that (or nothing else prints them),
    -- otherwise only here, as a wording of its own, which the push below finds or adds
    perform private.reword_components(e.questionnaire -> 'components');
    printed := array(select component_id from public.placement where questionnaire_id = q.questionnaire_id);
    asked_here := array(select placement_id from public.wave_question where wave_id = w.wave_id);
    delete from public.wave_question where wave_id = w.wave_id;
    perform private.tidy_placements(q.questionnaire_id, asked_here);
  end if;

  create temp table if not exists push_placed (seq serial, placement_id bigint, kind text, question_number text) on commit drop;
  drop table if exists pg_temp.push_log; -- one made by an earlier version in this session has other columns
  create temp table push_log (seq serial, what text, reused boolean, id bigint, name text) on commit drop;
  truncate pg_temp.push_placed;

  for block in select value from jsonb_array_elements(coalesce(e.questionnaire -> 'components', '[]'::jsonb)) loop
    perform private.push_block(block, q.questionnaire_id, w.wave_id, null, updating);
  end loop;
  if not exists (select 1 from public.wave_question where wave_id = w.wave_id) then
    raise exception 'This extraction has no questions to push.';
  end if;
  if updating then
    -- only now, so a wording the update still prints keeps its component (and its id)
    perform private.forget_components(printed);
  end if;

  -- the wave keeps the files it was read from
  if e.supplement ->> 'sha256' is not null then
    insert into public.pdf_file (pdf_sha256, pdf_bytes)
    values (e.supplement ->> 'sha256', (e.supplement ->> 'bytes')::bigint)
    on conflict (pdf_sha256) do nothing;
  end if;
  update public.wave set
    pdf_sha256        = coalesce(e.pdf_sha256, wave.pdf_sha256),
    pdf_name          = coalesce(e.pdf_name, wave.pdf_name),
    supplement_name   = coalesce(e.supplement ->> 'name', wave.supplement_name),
    supplement_sha256 = case when e.supplement is null then wave.supplement_sha256 else e.supplement ->> 'sha256' end,
    supplement_text   = case when e.supplement is null then wave.supplement_text else e.supplement ->> 'text' end
  where wave_id = w.wave_id;

  update public.extraction set status = 'Pushed', pushed_at = now(), saved_by = p_staff_id, editing_by = null, editing_at = null
  where extraction_id = e.extraction_id;

  -- Each row counts once however often the extraction uses it, and as already there only if this push didn't add
  -- it: "10 tags already in the database" is ten tags, not the 285 places that use them.
  drop table if exists pg_temp.push_tally;
  create temp table push_tally on commit drop as
  select what, coalesce(id::text, 'note ' || seq) as thing, bool_and(reused) as was_there, min(name) as name
  from pg_temp.push_log group by 1, 2;
  -- The option sets the extraction's variables offer: an option set is the choices one variable offers, whatever
  -- their order or their codes ("yes | no" and "no | yes" are one set). It was there already when a variable
  -- offered the same choices before this push began (push_sets_before). Named by its choices in the order a
  -- variable lists them.
  insert into pg_temp.push_tally (what, thing, was_there, name)
  select 'option_set', used.choices::text, exists (select 1 from pg_temp.push_sets_before b where b.choices = used.choices), used.words
  from (
    select offered.choices, min(offered.words) as words
    from (
      select r.variable_key,
             array_agg(r.answer_option_id order by r.answer_option_id) as choices,
             string_agg(a.standardized_label, ' | ' order by r.display_order) as words
      from public.response_option r join public.answer_option a on a.answer_option_id = r.answer_option_id
      group by r.variable_key
    ) as offered
    join pg_temp.push_tally t on t.what = 'variable' and t.thing = offered.variable_key::text
    group by offered.choices
  ) as used;

  select coalesce(jsonb_object_agg(what || case when was_there then '_reused' else '_new' end, n), '{}'::jsonb) into counts
  from (select what, was_there, count(*) as n from pg_temp.push_tally group by what, was_there) as tally;
  counts := counts || jsonb_build_object(
    'extraction_id', e.extraction_id, 'wave_id', w.wave_id, 'questionnaire_id', q.questionnaire_id,
    'questions', (select count(*) from public.wave_question where wave_id = w.wave_id)
  );
  if updating then
    counts := counts || jsonb_build_object('questions_before', asked);
  end if;

  insert into public.history (staff_id, action, entity, record_id, summary, details)
  values (p_staff_id, 'Pushed', 'wave', w.wave_id::text,
          format(case when updating then 'Pushed an edited extraction of %s, %s, in place of what it asked: %s questions, %s new variables.'
                      else 'Pushed the extraction of %s, %s: %s questions, %s new variables.' end,
                 q.name, w.name, counts ->> 'questions', coalesce(counts ->> 'variable_new', '0')),
          counts);
  -- with the names of what the preview lists: the tags, answer choices, option sets and measurement scales it adds
  -- and finds, and the questions the database has already (the history keeps the counts alone)
  return counts || jsonb_build_object('names', (
    select coalesce(jsonb_object_agg(key, list), '{}'::jsonb)
    from (
      select what || case when was_there then '_reused' else '_new' end as key, jsonb_agg(name order by name) as list
      from pg_temp.push_tally
      where name is not null and (what in ('tag', 'answer_option', 'option_set', 'measurement_scale') or (what = 'question' and was_there))
      group by 1
    ) as named
  ));
end;
$$;

-- Forget the components among these that nothing prints any more: a change that unplaced them (a block deleted
-- or reworded, a wave or questionnaire emptied) leaves them owned by nothing. Only the components a change
-- touched are looked at, so a component another change is adding at the same moment is left alone. A question
-- no questionnaire prints is no measurement scale's item either, so its scale items go with it.
create or replace function private.forget_components(p_component_ids bigint[])
returns integer
language plpgsql
set search_path = public, pg_temp
as $$
declare
  n integer;
begin
  if p_component_ids is null or cardinality(p_component_ids) = 0 then
    return 0;
  end if;
  delete from public.question_scale k
  where k.component_id = any (p_component_ids)
    and not exists (select 1 from public.placement p where p.component_id = k.component_id);
  delete from public.component c
  where c.component_id = any (p_component_ids)
    and not exists (select 1 from public.placement p where p.component_id = c.component_id);
  get diagnostics n = row_count;
  return n;
end;
$$;

-- What emptying a wave leaves in its questionnaire: the questions it asked (p_asked, its placements; every question
-- of the questionnaire when not given) that no wave asks any more go; then, once no wave asks the questionnaire
-- anything and it prints no question at all, everything else it printed (its instructions and notes); then the
-- sections, grids and groups left with nothing inside. A question no wave asks that the wave never asked (one kept
-- when its own wave was deleted) stays, with what it is in.
drop function if exists private.tidy_placements(bigint);
create or replace function private.tidy_placements(p_questionnaire_id bigint, p_asked bigint[] default null)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  n integer;
begin
  delete from public.placement p
  using public.component c
  where c.component_id = p.component_id and c.kind = 'Question' and p.questionnaire_id = p_questionnaire_id
    and (p_asked is null or p.placement_id = any (p_asked))
    and not exists (select 1 from public.wave_question wq where wq.placement_id = p.placement_id);
  delete from public.placement p
  where p.questionnaire_id = p_questionnaire_id
    and not exists (select 1 from public.wave_question wq where wq.questionnaire_id = p.questionnaire_id)
    and not exists (
      select 1 from public.placement x join public.component c on c.component_id = x.component_id
      where x.questionnaire_id = p_questionnaire_id and c.kind = 'Question'
    );
  loop
    delete from public.placement p
    using public.component c
    where c.component_id = p.component_id and c.kind in ('Section', 'Grid', 'Group') and p.questionnaire_id = p_questionnaire_id
      and not exists (select 1 from public.placement inside where inside.parent_placement_id = p.placement_id);
    get diagnostics n = row_count;
    exit when n = 0;
  end loop;
end;
$$;

-- What a questionnaire holds, for saying what an emptying removed.
create or replace function private.content_counts(p_questionnaire_ids bigint[])
returns jsonb
language sql
stable
set search_path = public, pg_temp
as $$
  select jsonb_build_object(
    'questions_asked', (select count(*) from public.wave_question where questionnaire_id = any (p_questionnaire_ids)),
    'placements',      (select count(*) from public.placement where questionnaire_id = any (p_questionnaire_ids)),
    'variables',       (select count(*) from public.variable where questionnaire_id = any (p_questionnaire_ids))
  );
$$;

-- The difference between two counts, as what was removed.
create or replace function private.removed(p_before jsonb, p_after jsonb)
returns jsonb
language sql
immutable
set search_path = ''
as $$
  select jsonb_build_object(
    'questions_removed',  (p_before ->> 'questions_asked')::integer - (p_after ->> 'questions_asked')::integer,
    'placements_removed', (p_before ->> 'placements')::integer - (p_after ->> 'placements')::integer,
    'variables_removed',  (p_before ->> 'variables')::integer - (p_after ->> 'variables')::integer
  );
$$;

-- A wording staff changed in an extraction reopened from the catalog (public.reopen_wave: each block names the
-- component it came from, `component_id`). The component itself takes the new words, so every place that prints
-- it does, when staff chose that for the block (`"reword": "everywhere"`) or when nothing else prints it. If the
-- new words are a component already, every place printing the old one prints that one instead. Otherwise the old
-- component is left as it is for the other questionnaires, and the push gives this one the new wording.
-- p_blocks: the extraction's blocks (its `components`, or a block's `children`).
create or replace function private.reword_components(p_blocks jsonb)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  b         jsonb;
  old       public.component;
  new_words text;
  the_same  bigint;
  places    integer;
begin
  for b in select value from jsonb_array_elements(coalesce(p_blocks, '[]'::jsonb)) loop
    new_words := nullif(private.squish(b ->> 'text'), '');
    if coalesce(b ->> 'component_id', '') ~ '^[0-9]+$' and new_words is not null then
      select * into old from public.component where component_id = (b ->> 'component_id')::bigint;
      if found and old.kind = b ->> 'component_type' and private.squish(old.text) is distinct from new_words then
        select count(*) into places from public.placement where component_id = old.component_id;
        if b ->> 'reword' = 'everywhere' or places <= 1 then
          select component_id into the_same from public.component
          where kind = old.kind and private.squish(text) = new_words and component_id <> old.component_id;
          if the_same is null then
            update public.component set text = new_words where component_id = old.component_id;
          else
            update public.placement set component_id = the_same where component_id = old.component_id;
            perform private.forget_components(array[old.component_id]);
          end if;
        end if;
      end if;
    end if;
    perform private.reword_components(b -> 'children');
  end loop;
end;
$$;

-- The one tag of a kind on a question's placement or, with p_variable_key, on a variable (the first by name
-- if it has several).
create or replace function private.tag_name(p_kind text, p_placement_id bigint, p_variable_key bigint)
returns text
language sql
stable
set search_path = public, pg_temp
as $$
  select min(t.name) from public.tag t
  where t.kind = p_kind and case
    when p_variable_key is not null then exists (select 1 from public.variable_tag vt where vt.variable_key = p_variable_key and vt.tag_id = t.tag_id)
    else exists (select 1 from public.placement_tag pt where pt.placement_id = p_placement_id and pt.tag_id = t.tag_id)
  end;
$$;

-- Whether a placement, or anything inside it, is a question: one the wave asks, with p_wave_id, or any at all.
create or replace function private.holds_question(p_placement_id bigint, p_wave_id bigint default null)
returns boolean
language sql
stable
set search_path = public, pg_temp
as $$
  with recursive inside as (
    select p.placement_id, p.component_id from public.placement p where p.placement_id = p_placement_id
    union all
    select p.placement_id, p.component_id from public.placement p join inside i on p.parent_placement_id = i.placement_id
  )
  select exists (
    select 1 from inside i join public.component c on c.component_id = i.component_id
    where c.kind = 'Question'
      and (p_wave_id is null or exists (select 1 from public.wave_question wq where wq.wave_id = p_wave_id and wq.placement_id = i.placement_id))
  );
$$;

-- What a wave asks, as the blocks of an extraction's JSON (docs/database_design.md): the questionnaire's
-- placements under p_parent_id in order, each naming its component (`component_id`), a question with its numbers
-- in the wave, the question it depends on, its tags, its measurement scale and its variables with their choices
-- and codes. Left out:
-- a question the wave doesn't ask (another wave's), and a section, grid or group whose questions are all other
-- waves'. Every block is marked checked: it was reviewed before it was pushed.
create or replace function private.wave_blocks(p_wave_id bigint, p_questionnaire_id bigint, p_parent_id bigint)
returns jsonb
language plpgsql
stable
set search_path = public, pg_temp
as $$
declare
  blocks jsonb := '[]'::jsonb;
  pl     record;
  asked  record;
  block  jsonb;
begin
  for pl in
    select p.placement_id, p.depends_on_placement_id, c.component_id, c.kind, c.text
    from public.placement p join public.component c on c.component_id = p.component_id
    where p.questionnaire_id = p_questionnaire_id and p.parent_placement_id is not distinct from p_parent_id
    order by p.display_order, p.placement_id
  loop
    block := jsonb_strip_nulls(jsonb_build_object('component_type', pl.kind, 'text', pl.text, 'component_id', pl.component_id, 'checked', true));
    if pl.kind = 'Question' then
      select wq.question_number, wq.printed_number into asked from public.wave_question wq
      where wq.wave_id = p_wave_id and wq.placement_id = pl.placement_id;
      continue when not found;
      block := block || jsonb_strip_nulls(jsonb_build_object(
        'question_number', asked.question_number,
        'printed_number', asked.printed_number,
        'parent_question', (select d.question_number from public.wave_question d
                            where d.wave_id = p_wave_id and d.placement_id = pl.depends_on_placement_id),
        'subject', private.tag_name('subject', pl.placement_id, null),
        'time_frame', private.tag_name('time frame', pl.placement_id, null),
        -- the scales its wording is an item of, by abbreviation ("CDS-12, FTND")
        'scale', (select string_agg(m.abbreviation, ', ' order by lower(m.abbreviation))
                  from public.question_scale k join public.measurement_scale m on m.scale_id = k.scale_id
                  where k.component_id = pl.component_id)
      )) || jsonb_build_object('variables', (
        select coalesce(jsonb_agg(jsonb_strip_nulls(jsonb_build_object(
          'variable_type', v.variable_type, 'matr_id', v.matr_id, 'label', v.label, 'answer_data_type', v.answer_data_type,
          'subject', private.tag_name('subject', null, v.variable_key),
          'time_frame', private.tag_name('time frame', null, v.variable_key),
          'unit', private.tag_name('unit', null, v.variable_key),
          'response_options', (
            select jsonb_agg(jsonb_build_object('answer_option', a.standardized_label, 'code_value', r.code_value) order by r.display_order)
            from public.response_option r join public.answer_option a on a.answer_option_id = r.answer_option_id
            where r.variable_key = v.variable_key
          )
        )) order by v.display_order, v.variable_key), '[]'::jsonb)
        from public.variable v where v.placement_id = pl.placement_id
      ));
    elsif pl.kind in ('Section', 'Grid', 'Group') then
      continue when private.holds_question(pl.placement_id) and not private.holds_question(pl.placement_id, p_wave_id);
      block := block || jsonb_build_object('children', private.wave_blocks(p_wave_id, p_questionnaire_id, pl.placement_id));
    end if;
    blocks := blocks || jsonb_build_array(block);
  end loop;
  return blocks;
end;
$$;


-- =====================================================================================================
-- 4c. WHAT THE SITE CALLS
-- =====================================================================================================

-- Push an extraction into the catalog: its blocks, questions, variables and response options go in, its wave
-- is marked as asking the questions, and the extraction is marked Pushed. The wave must ask nothing yet
-- (empty it first if it was extracted before), unless the extraction was reopened from that wave
-- (public.reopen_wave): then it takes the place of what the wave asks, in this one transaction. Returns what
-- was done, as counts (with `questions_before` for such an update).
-- With p_dry_run, does all of it and then undoes it, returning the counts: what the Edit Extraction page
-- shows before staff push.
create or replace function public.push_extraction(p_extraction_id bigint, p_staff_id bigint, p_dry_run boolean default false)
returns jsonb
language plpgsql
set search_path = public, pg_temp
as $$
declare
  e      public.extraction;
  result jsonb;
begin
  perform private.require_staff(p_staff_id);
  select * into e from public.extraction where extraction_id = p_extraction_id;
  if not found then
    raise exception 'That extraction is no longer in the database.';
  end if;
  if e.status <> 'Draft' then
    raise exception 'This extraction was pushed already, on %.', to_char(e.pushed_at, 'YYYY-MM-DD');
  end if;
  if not p_dry_run then
    return private.push_run(e, p_staff_id);
  end if;
  begin
    result := private.push_run(e, p_staff_id);
    raise exception using errcode = 'P0901', message = result::text;
  exception
    when sqlstate 'P0901' then
      return sqlerrm::jsonb;   -- the push was undone with the error; its counts come back
  end;
end;
$$;

-- Bring what a wave asks back into Edit Extraction: a new Draft extraction holding the wave's questions as the
-- catalog has them now (private.wave_blocks), going into the same wave, with the wave's PDF beside it. Its
-- info says which wave it came from (`updates_wave_id`), so pushing it replaces what that wave asks instead of
-- being refused (private.push_run); until then the catalog is as it was. The extraction the wave was first pushed
-- from stays as the record of what was read and reviewed. Asked for again while that draft is open, it gives
-- the same draft. Returns the draft's extraction_id.
-- When the extraction the wave was last pushed from was read by the AI, the draft is AI-Powered too and carries
-- that reading (ai_original, with its model and prompt version; its reading is 'ready', since there is nothing
-- to read), and its info names that extraction (`reopened_from`): the site matches the draft's blocks to the
-- reading through the copy that was pushed, so a block can still be put back as the AI read it. A wave last
-- pushed from an extraction typed by hand gives a Manual draft, as before.
create or replace function public.reopen_wave(p_wave_id bigint, p_staff_id bigint)
returns bigint
language plpgsql
set search_path = public, pg_temp
as $$
declare
  w      public.wave;
  q      public.questionnaire;
  src    public.extraction;   -- the extraction the wave was last pushed from, if there is one
  by_ai  boolean;
  the_id bigint;
begin
  perform private.require_staff(p_staff_id);
  select * into w from public.wave where wave_id = p_wave_id;
  if not found then
    raise exception 'That wave is no longer in the database.';
  end if;
  select * into q from public.questionnaire where questionnaire_id = w.questionnaire_id;
  if not exists (select 1 from public.wave_question where wave_id = p_wave_id) then
    raise exception 'The wave % of % asks no questions yet, so there is nothing to edit. Start a New Extraction for it instead.', w.name, q.name;
  end if;
  select extraction_id into the_id from public.extraction
  where status = 'Draft' and info ->> 'updates_wave_id' = p_wave_id::text
  order by created_at
  limit 1;
  if the_id is not null then
    return the_id;
  end if;
  select * into src from public.extraction
  where status = 'Pushed' and info ->> 'wave_id' = p_wave_id::text
  order by pushed_at desc, extraction_id desc
  limit 1;
  -- (an AI-Powered extraction has a PDF: extraction_ai_has_pdf_and_reading)
  by_ai := coalesce(src.method = 'AI-Powered' and src.ai_original is not null and w.pdf_sha256 is not null, false);
  insert into public.extraction (method, status, prepared_by, saved_by, info, questionnaire, pdf_sha256, pdf_name,
                                 ai_original, reading_status, model, prompt_version)
  values (
    case when by_ai then 'AI-Powered' else 'Manual' end, 'Draft', p_staff_id, p_staff_id,
    jsonb_strip_nulls(jsonb_build_object(
      'registry_id', (select s.registry_id::text from public.study s where s.study_id = q.study_id),
      'study_id', q.study_id::text, 'questionnaire_id', q.questionnaire_id::text, 'wave_id', w.wave_id::text,
      'updates_wave_id', w.wave_id::text,
      'reopened_from', case when by_ai then src.extraction_id::text end
    )),
    jsonb_build_object(
      'schema_version', 2,
      'questionnaire', jsonb_strip_nulls(jsonb_build_object('name', q.name, 'abbreviation', q.abbreviation)),
      'components', private.wave_blocks(w.wave_id, q.questionnaire_id, null)
    ),
    w.pdf_sha256, w.pdf_name,
    case when by_ai then src.ai_original end, case when by_ai then 'ready' end,
    case when by_ai then src.model end, case when by_ai then src.prompt_version end
  )
  returning extraction_id into the_id;
  return the_id;
end;
$$;

-- Apply an edit made on Make an edit: a list of items, each one field of one row to insert, update or
-- delete, applied in order in one transaction and written to the history as one change.
-- p_items: a JSON array of {item_number, entity_name, record_identifier, action, attribute_name, old_value,
--   new_value}. record_identifier is the row's key as a JSON object in text ({"wave_id": 123}). A row being
--   added names its new key as a token, "new:<table>:<n>", which later items may use for the number the
--   database gave it. An update's old_value must match what the row holds, so a stale edit is refused.
-- p_summary: the change in a sentence, for the history.
-- Returns {"history_id", "new_ids": {token: id}}.
create or replace function public.apply_change(p_staff_id bigint, p_items jsonb, p_summary text default null)
returns jsonb
language plpgsql
set search_path = public, pg_temp
as $$
declare
  editable constant text[] := array[
    'registry', 'principal_investigator', 'study', 'publication', 'study_publication', 'respondent_type',
    'questionnaire', 'wave', 'pdf_file',
    'component', 'placement', 'wave_question', 'variable', 'answer_option', 'response_option',
    'tag', 'placement_tag', 'variable_tag', 'response_option_tag', 'measurement_scale', 'question_scale'
  ];
  token_pattern constant text := '^new:[a-z_]+:[0-9]+$';

  new_ids      jsonb := '{}'::jsonb;   -- 'new:component:1' -> the number the database gave it
  col_cache    jsonb := '{}'::jsonb;   -- table -> its columns
  applied      jsonb := '[]'::jsonb;   -- the items as applied, tokens swapped for numbers
  grp          record;
  attr         jsonb;
  cols         jsonb;
  ident        jsonb;
  key          text;
  val          text;
  where_sql    text;
  made_token   text;
  made_column  text;
  col_list     text[];
  val_list     text[];
  set_list     text[];
  new_id       bigint;
  n_rows       integer;
  same         boolean;
  recorded_id  text;
  token        text;
  first_entity text;
  first_id     text;
  the_action   text;
  the_history  bigint;
  unplaced     bigint[] := array[]::bigint[];   -- the components of placements this change deletes or changes
  old_component bigint;
  taken        bigint[];                         -- the components of the placements a delete takes with it
begin
  perform private.require_staff(p_staff_id);
  if jsonb_typeof(p_items) is distinct from 'array' or jsonb_array_length(p_items) = 0 then
    raise exception 'There is nothing in this change to apply.';
  end if;
  -- for the rest of the transaction (a SET CONSTRAINTS can't be undone at the end of a function): the site
  -- calls this through the REST API, each call a transaction of its own
  set constraints all deferred;

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
           jsonb_agg(jsonb_build_object('n', item_number, 'attribute', attribute_name, 'old', old_value, 'new', new_value)
                     order by item_number) as attrs
    from numbered
    group by run, entity_name, record_identifier, action
    order by run
  loop
    if grp.entity_name is null or not (grp.entity_name = any (editable)) then
      raise exception 'Item %: this change touches a table it can''t change (%).', grp.first_item, coalesce(grp.entity_name, '(none)');
    end if;
    if grp.action is null or grp.action not in ('Insert', 'Update', 'Delete') then
      raise exception 'Item %: the action % isn''t Insert, Update or Delete.', grp.first_item, coalesce(grp.action, '(none)');
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
      where a.attrelid = format('public.%I', grp.entity_name)::regclass and a.attnum > 0 and not a.attisdropped
        and a.attgenerated = '';
      col_cache := col_cache || jsonb_build_object(grp.entity_name, cols);
    end if;

    -- the row's key, new rows swapped for their numbers; the one thing an Insert can't swap is the key the
    -- database is about to number
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
        if key is null or val is null or ident ? key then
          continue;
        end if;
        if not cols ? key then
          raise exception 'Item %: % has no column %.', attr ->> 'n', grp.entity_name, key;
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
      -- Update and Delete: the row has to be there, and as it was
      execute format('select count(*) from public.%I where %s', grp.entity_name, where_sql) into n_rows;
      if n_rows <> 1 then
        raise exception 'Item %: the % row % is no longer in the database, so this change is out of date.',
          grp.first_item, grp.entity_name, grp.record_identifier;
      end if;
      -- a block deleted, reworded or moved may leave its component printed nowhere (looked at when the change ends)
      if grp.entity_name in ('placement', 'question_scale') then
        execute format('select component_id from public.%I where %s', grp.entity_name, where_sql) into old_component;
        unplaced := unplaced || old_component;
      end if;
      -- and so may what a delete takes with it: the blocks inside a deleted section, every block of a deleted questionnaire
      if grp.action = 'Delete' and grp.entity_name in ('placement', 'questionnaire') then
        execute format(
          'with recursive inside as (
             select p.placement_id, p.component_id from public.placement p where p.%1$I in (select %1$I from public.%2$I where %3$s)
             union all
             select p.placement_id, p.component_id from public.placement p join inside i on p.parent_placement_id = i.placement_id
           ) select array_agg(component_id) from inside',
          case grp.entity_name when 'placement' then 'placement_id' else 'questionnaire_id' end, grp.entity_name, where_sql
        ) into taken;
        unplaced := unplaced || coalesce(taken, array[]::bigint[]);
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
          execute format('select coalesce(%I::text, ''(empty)'') from public.%I where %s', key, grp.entity_name, where_sql) into val;
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

    -- what was done, with the numbers the database gave
    recorded_id := grp.record_identifier;
    for token in select jsonb_object_keys(new_ids) loop
      recorded_id := replace(recorded_id, '"' || token || '"', new_ids ->> token);
    end loop;
    if first_entity is null then
      first_entity := grp.entity_name;
      first_id := recorded_id;
    end if;
    applied := applied || (
      select coalesce(jsonb_agg(jsonb_build_object(
        'item_number', a.value ->> 'n', 'entity_name', grp.entity_name, 'record_identifier', recorded_id, 'action', grp.action,
        'attribute_name', a.value ->> 'attribute', 'old_value', a.value ->> 'old',
        'new_value', case when (a.value ->> 'new') ~ token_pattern and new_ids ? (a.value ->> 'new') then new_ids ->> (a.value ->> 'new') else a.value ->> 'new' end
      ) order by (a.value ->> 'n')::integer), '[]'::jsonb)
      from jsonb_array_elements(grp.attrs) as a(value)
    );
  end loop;

  perform private.forget_components(unplaced);

  select case when bool_or(i.value ->> 'action' = 'Delete') then 'Deleted'
              when bool_and(i.value ->> 'action' = 'Insert') then 'Added'
              else 'Updated' end
    into the_action
  from jsonb_array_elements(applied) as i(value);
  insert into public.history (staff_id, action, entity, record_id, summary, details)
  values (p_staff_id, the_action, first_entity, first_id, coalesce(nullif(btrim(p_summary), ''), 'Changed the catalog on Make an edit.'), applied)
  returning history_id into the_history;

  return jsonb_build_object('history_id', the_history, 'new_ids', new_ids);
end;
$$;

-- Empty a wave: it stays, but asks no questions. The questions it alone asked go from its questionnaire,
-- with their variables, and what that leaves owned by nothing (sections left empty, components no
-- questionnaire prints). The wave keeps the files it was read from, so Re-extract can read them again.
-- Returns what was removed, as counts.
create or replace function public.empty_wave(p_wave_id bigint, p_staff_id bigint)
returns jsonb
language plpgsql
set search_path = public, pg_temp
as $$
declare
  w       public.wave;
  q       public.questionnaire;
  printed bigint[];
  asked   bigint[];   -- the placements of the questions it asked
  before  jsonb;
  gone    jsonb;
begin
  perform private.require_staff(p_staff_id);
  select * into w from public.wave where wave_id = p_wave_id;
  if not found then
    raise exception 'That wave is no longer in the database.';
  end if;
  select * into q from public.questionnaire where questionnaire_id = w.questionnaire_id;
  before := private.content_counts(array[q.questionnaire_id]);
  printed := array(select component_id from public.placement where questionnaire_id = q.questionnaire_id);
  asked := array(select placement_id from public.wave_question where wave_id = p_wave_id);
  delete from public.wave_question where wave_id = p_wave_id;
  perform private.tidy_placements(q.questionnaire_id, asked);
  perform private.forget_components(printed);
  gone := private.removed(before, private.content_counts(array[q.questionnaire_id]));
  insert into public.history (staff_id, action, entity, record_id, summary, details)
  values (p_staff_id, 'Emptied', 'wave', w.wave_id::text,
          format('Emptied %s, %s: %s questions removed.', q.name, w.name, gone ->> 'questions_removed'), gone);
  return gone;
end;
$$;

-- Empty a questionnaire: it and its waves stay, with nothing printed or asked. Returns counts.
create or replace function public.empty_questionnaire(p_questionnaire_id bigint, p_staff_id bigint)
returns jsonb
language plpgsql
set search_path = public, pg_temp
as $$
declare
  q       public.questionnaire;
  printed bigint[];
  before  jsonb;
  gone    jsonb;
begin
  perform private.require_staff(p_staff_id);
  select * into q from public.questionnaire where questionnaire_id = p_questionnaire_id;
  if not found then
    raise exception 'That questionnaire is no longer in the database.';
  end if;
  before := private.content_counts(array[q.questionnaire_id]);
  printed := array(select component_id from public.placement where questionnaire_id = q.questionnaire_id);
  delete from public.placement where questionnaire_id = q.questionnaire_id;
  perform private.forget_components(printed);
  gone := private.removed(before, private.content_counts(array[q.questionnaire_id]));
  insert into public.history (staff_id, action, entity, record_id, summary, details)
  values (p_staff_id, 'Emptied', 'questionnaire', q.questionnaire_id::text,
          format('Emptied %s: %s questions removed.', q.name, gone ->> 'questions_removed'), gone);
  return gone;
end;
$$;

-- Empty a study: it, its questionnaires and their waves stay, with nothing printed or asked. Returns counts.
create or replace function public.empty_study(p_study_id bigint, p_staff_id bigint)
returns jsonb
language plpgsql
set search_path = public, pg_temp
as $$
declare
  s       public.study;
  qs      bigint[];
  printed bigint[];
  before  jsonb;
  gone    jsonb;
begin
  perform private.require_staff(p_staff_id);
  select * into s from public.study where study_id = p_study_id;
  if not found then
    raise exception 'That study is no longer in the database.';
  end if;
  qs := array(select questionnaire_id from public.questionnaire where study_id = s.study_id);
  before := private.content_counts(qs);
  printed := array(select component_id from public.placement where questionnaire_id = any (qs));
  delete from public.placement where questionnaire_id = any (qs);
  perform private.forget_components(printed);
  gone := private.removed(before, private.content_counts(qs));
  insert into public.history (staff_id, action, entity, record_id, summary, details)
  values (p_staff_id, 'Emptied', 'study', s.study_id::text,
          format('Emptied the study %s: %s questions removed from %s questionnaire(s).', s.name, gone ->> 'questions_removed', cardinality(qs)), gone);
  return gone;
end;
$$;

-- Empty a registry: it, its studies, questionnaires and waves stay, with nothing printed or asked. Returns counts.
create or replace function public.empty_registry(p_registry_id bigint, p_staff_id bigint)
returns jsonb
language plpgsql
set search_path = public, pg_temp
as $$
declare
  r       public.registry;
  qs      bigint[];
  printed bigint[];
  before  jsonb;
  gone    jsonb;
begin
  perform private.require_staff(p_staff_id);
  select * into r from public.registry where registry_id = p_registry_id;
  if not found then
    raise exception 'That registry is no longer in the database.';
  end if;
  qs := array(select q.questionnaire_id from public.questionnaire q join public.study s using (study_id) where s.registry_id = r.registry_id);
  before := private.content_counts(qs);
  printed := array(select component_id from public.placement where questionnaire_id = any (qs));
  delete from public.placement where questionnaire_id = any (qs);
  perform private.forget_components(printed);
  gone := private.removed(before, private.content_counts(qs));
  insert into public.history (staff_id, action, entity, record_id, summary, details)
  values (p_staff_id, 'Emptied', 'registry', r.registry_id::text,
          format('Emptied the registry %s: %s questions removed from %s questionnaire(s).', r.name, gone ->> 'questions_removed', cardinality(qs)), gone);
  return gone;
end;
$$;


-- =====================================================================================================
-- 4d. WHAT THE DASHBOARD CALLS
-- =====================================================================================================

-- One more upvote for a questionnaire. Runs as its owner, since the dashboard roles can't update the table.
create or replace function public.upvote_questionnaire(p_questionnaire_id bigint)
returns integer
language plpgsql
security definer
set search_path = public, pg_temp
as $$
declare
  n integer;
begin
  update public.questionnaire set upvote_count = upvote_count + 1
  where questionnaire_id = p_questionnaire_id
  returning upvote_count into n;
  if n is null then
    raise exception 'There is no questionnaire %.', p_questionnaire_id;
  end if;
  return n;
end;
$$;

-- One upvote fewer, when a dashboard visitor takes back their star. Never below zero. Runs as its owner, like
-- upvote_questionnaire().
create or replace function public.unstar_questionnaire(p_questionnaire_id bigint)
returns integer
language plpgsql
security definer
set search_path = public, pg_temp
as $$
declare
  n integer;
begin
  update public.questionnaire set upvote_count = greatest(upvote_count - 1, 0)
  where questionnaire_id = p_questionnaire_id
  returning upvote_count into n;
  if n is null then
    raise exception 'There is no questionnaire %.', p_questionnaire_id;
  end if;
  return n;
end;
$$;


-- =====================================================================================================
-- 4e. WHO IS STAFF
-- =====================================================================================================

-- Whether the signed-in person (auth.jwt() email) is an active row of staff: for the row policies on the
-- private tables (5_permissions.sql). staff.email matches ignoring case (staff_email_unique). It runs as its
-- owner, to read staff whoever asks, so it is kept out of the schema the REST API offers: nobody can call
-- it through /rpc/, only the policies use it. (It was public.is_active_staff(); 5_permissions.sql removes that.)
create or replace function private.is_active_staff()
returns boolean
language sql
stable
security definer
set search_path = ''
as $$
  select exists (
    select 1 from public.staff s
    where s.active and lower(btrim(s.email)) = lower(coalesce(auth.jwt() ->> 'email', ''))
  );
$$;

commit;
