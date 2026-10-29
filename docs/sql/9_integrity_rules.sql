-- =====================================================================================================
-- MATR metadata database: integrity rules, normalized answer choices, Groups, respondents' counts, and
-- extraction drafts that go once they are discarded
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one.
--
-- What it does, in order (each part is a section below):
--   A. Question Group is renamed Group (the component_type row; the site and the prompt call it Group now).
--   B. Answer choices are kept normalized: lower case, straight quotes, one space between words, no question
--      mark, and no full stop or colon at the end. Existing rows are normalized first; a check keeps it so.
--   C. Columns that held what isn't normalized data, or nothing, go: response_option.printed_label (the words
--      as printed: a choice is its standard wording) and variable.respondent_count (never written; a count
--      belongs to a choice in a wave, part D).
--   D. response_option_count: how many of a wave's respondents gave each answer, typed on Edit Extraction
--      and added with the extraction. private.apply_draft_items() may add (and an edit change) its rows.
--   E. Extraction drafts: a discarded one is deleted (public.discard_draft()). An applied one is kept whole,
--      with what the AI read and how staff changed it. A PDF no draft uses any more loses its pdf_file row (the
--      site removes the file from Storage). extracted_draft.supplement holds the optional file of MATR IDs and
--      codes.
--   F. The audit's rules (docs/database_audit.md, 23 Oct 2026), enforced by the database as well as the site:
--      M1 a closed-ended variable's choices are its option set's; M2 a component is a Question exactly when
--      it has a question row; M3 a parent is a Section or a Group, and a Section is never inside a Group;
--      M4 only a Group has no words, and it has none; M5 + D1 no duplicate investigators (email), respondent
--      types or registry abbreviations (ignoring case and outer spaces); M6 required names can't be blank;
--      M7 a study's lead investigator never changes, and staff are never deleted; M8 pdf_file's comment.
--
-- ORDER: run it, then put the site's new code in place (it calls Group "Group", reads
-- extracted_draft.supplement and calls discard_draft()). The old site still works with it except for
-- choices' printed words (gone) and Question Group (renamed).
--
-- LOST: response_option.printed_label and variable.respondent_count, with whatever they held. Answer choices
-- are rewritten in lower case. Nothing else changes. The public dashboard reads the metadata tables: if it
-- names printed_label or respondent_count, or 'Question Group', change it too.
--
-- It is one transaction: if any step fails, nothing changes. The checks at the end are in a block that is
-- rolled back, so they leave nothing. docs/sql/sql_scripts.sql already builds the same.
-- =====================================================================================================

begin;

-- ------ A. Question Group is Group ----------------------------------------------------------------------
update public.component_type
set name = 'Group',
    description = 'Holds blocks that belong together but have no printed heading: a battery, a definition with its question, a question with its follow-up. Has no text.'
where name = 'Question Group';

comment on column public.component.text is 'The printed words. Empty only for a Group, which has none (BR42).';


-- ------ B. Answer choices, normalized --------------------------------------------------------------------
-- The site's normal_answer() (R/service_draft.R) does the same.
create function private.normal_answer(p_text text)
returns text
language sql
immutable
set search_path = ''
as $$
  select nullif(btrim(regexp_replace(
    lower(btrim(regexp_replace(
      translate(replace(p_text, '?', ''), E'‘’“”', '''''""'),
      '\s+', ' ', 'g'
    ))),
    '[.:[:space:]]+$', ''
  )), '');
$$;

-- Two choices that become the same words would break answer_option_label_unique: say which, and stop.
do $$
declare
  clash text;
begin
  select string_agg(format('"%s"', labels), '; ') into clash
  from (
    select string_agg(standardized_label, '" and "' order by answer_option_id) as labels
    from public.answer_option
    group by private.normal_answer(standardized_label)
    having count(*) > 1
  ) same;
  if clash is not null then
    raise exception 'These answer choices are the same once normalized; merge them first (Make an edit), then run this again: %', clash;
  end if;
  if exists (select 1 from public.answer_option where private.normal_answer(standardized_label) is null) then
    raise exception 'An answer choice is blank once normalized (only punctuation); change it first, then run this again.';
  end if;
end;
$$;

update public.answer_option
set standardized_label = private.normal_answer(standardized_label)
where standardized_label is distinct from private.normal_answer(standardized_label);

alter table public.answer_option
  add constraint answer_option_label_normal check (standardized_label = private.normal_answer(standardized_label));

comment on column public.answer_option.standardized_label is
  'The answer in standard wording, kept normalized: lower case, straight quotes, one space between words, no question mark, no full stop or colon at the end.';


-- ------ C. Columns that go ---------------------------------------------------------------------------------
alter table public.response_option drop column printed_label;
alter table public.variable drop column respondent_count;


-- ------ D. How many of a wave's respondents gave each answer ---------------------------------------------
create table public.response_option_count (
  wave_id          bigint  not null,
  variable_key     bigint  not null,
  answer_option_id bigint  not null,
  respondent_count integer not null
    constraint response_option_count_not_negative check (respondent_count >= 0),
  primary key (wave_id, variable_key, answer_option_id)
);

alter table public.response_option_count
  add constraint response_option_count_wave_id_fkey
    foreign key (wave_id) references public.wave (wave_id) on delete cascade,
  add constraint response_option_count_response_option_fkey
    foreign key (variable_key, answer_option_id) references public.response_option (variable_key, answer_option_id) on delete cascade;

create index response_option_count_choice_idx on public.response_option_count (variable_key, answer_option_id);

-- The wave asks the variable's question: it is in the variable's questionnaire, and asks that question.
-- Checked when the transaction ends, so an extraction can add the count before or after the wave's question.
create function public.response_option_count_in_wave()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if not exists (select 1 from public.response_option_count where wave_id = new.wave_id
                 and variable_key = new.variable_key and answer_option_id = new.answer_option_id) then
    return null; -- added and removed in the same change
  end if;
  if not exists (
    select 1 from public.variable v
    join public.wave_question wq
      on wq.questionnaire_id = v.questionnaire_id and wq.component_id = v.component_id and wq.wave_id = new.wave_id
    where v.variable_key = new.variable_key
  ) then
    raise exception 'A count of respondents is for a wave that asks the variable''s question.';
  end if;
  return null;
end;
$$;

create constraint trigger response_option_count_in_wave
  after insert or update on public.response_option_count
  deferrable initially deferred
  for each row execute function public.response_option_count_in_wave();

alter table public.response_option_count enable row level security;
create policy "Dashboard can read" on public.response_option_count for select to anon, authenticated using (true);
grant select on public.response_option_count to anon, authenticated;
grant select, insert, update, delete on public.response_option_count to service_role;

comment on table public.response_option_count is
  'How many of a wave''s respondents gave one answer (response option) of one variable. Typed on Edit Extraction for the extraction''s wave.';

-- An extraction may add these rows, and an edit change them: private.apply_draft_items() lists the tables
-- each may touch. The function is otherwise as it is; it is changed where it stands, so this needn't repeat it.
do $$
declare
  def text := pg_get_functiondef('private.apply_draft_items(public.draft, jsonb, bigint)'::regprocedure);
  changed text;
begin
  changed := replace(def, '''closed_ended'', ''response_option''', '''closed_ended'', ''response_option'', ''response_option_count''');
  changed := replace(changed, '''answer_option_set'', ''response_option'', ''subject''', '''answer_option_set'', ''response_option'', ''response_option_count'', ''subject''');
  if changed = def or position('response_option_count' in changed) = 0
     or (length(changed) - length(def)) <> 2 * length(', ''response_option_count''') then
    raise exception 'private.apply_draft_items() isn''t as expected (migration 8): its lists of tables weren''t found.';
  end if;
  execute changed;
end;
$$;


-- ------ E. Extraction drafts go once they are discarded ---------------------------------------------------
-- The optional file of MATR IDs and codes: {name, sha256, bytes} for a PDF (kept in Storage like the
-- questionnaire's, by its hash) or {name, text} for a CSV or text file.
alter table public.extracted_draft
  add column supplement jsonb,
  add constraint extracted_draft_supplement_is_an_object check (supplement is null or jsonb_typeof(supplement) = 'object');

comment on column public.extracted_draft.supplement is
  'The optional file of MATR IDs and codes Gemini reads after the questionnaire: {name, sha256, bytes} for a PDF (in Storage as <sha256>.pdf), or {name, text} for a CSV or text file.';

-- (An applied extraction draft is kept whole, as every applied draft is: draft_detail_guard stays as it was.)

-- A PDF no draft uses any more (its draft was discarded) goes too (its file in Storage is removed by the site).
create function public.extracted_draft_pdf_let_go()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if old.pdf_sha256 is not null
     and not exists (select 1 from public.extracted_draft where pdf_sha256 = old.pdf_sha256) then
    delete from public.pdf_file where pdf_sha256 = old.pdf_sha256;
  end if;
  return null;
end;
$$;

create trigger extracted_draft_pdf_let_go
  after delete on public.extracted_draft
  for each row execute function public.extracted_draft_pdf_let_go();

-- Discarding an extraction deletes it, with its details and review events. Nobody approves it; only the
-- person who prepared it can, and not once it is applied.
create function public.discard_draft(p_draft_id bigint, p_staff_id bigint)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d public.draft%rowtype;
begin
  select * into d from public.draft where draft_id = p_draft_id for update;
  if not found then
    raise exception 'That is no longer in the database.';
  end if;
  if d.kind <> 'Extraction' then
    raise exception 'Only an extraction is discarded this way; an edit is withdrawn.';
  end if;
  if d.status = 'Applied' then
    raise exception 'That has been applied, so it can''t be discarded.';
  end if;
  if p_staff_id is distinct from d.prepared_by then
    raise exception 'Only the person who prepared this draft can discard it.';
  end if;
  delete from public.draft where draft_id = p_draft_id;
end;
$$;

revoke all on function public.discard_draft(bigint, bigint) from public;
grant execute on function public.discard_draft(bigint, bigint) to service_role;

-- Extractions that were discarded before this (Withdrawn) can go too. Uncomment to delete them; check first
-- that none is a draft you keep for reference (select draft_id, summary from public.draft where kind =
-- 'Extraction' and status = 'Withdrawn'). Their PDFs' files stay in Storage.
-- delete from public.draft where kind = 'Extraction' and status = 'Withdrawn';


-- ------ F. The audit's rules --------------------------------------------------------------------------------
-- Each is checked when the transaction ends (a deferred constraint trigger), so a change can add or move
-- rows in any order; private.apply_draft_items() defers constraints already. A broken rule refuses the
-- whole change with a sentence.

-- M1 (BR102): a closed-ended variable's choices (response_option) are exactly its option set's
-- (answer_option_set).
create function private.choices_match_set(p_variable_keys bigint[])
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_matr text;
begin
  select v.matr_id into v_matr
  from public.closed_ended c
  join public.variable v using (variable_key)
  where c.variable_key = any (p_variable_keys)
    and (
      exists (select 1 from public.response_option r where r.variable_key = c.variable_key
              and not exists (select 1 from public.answer_option_set s
                              where s.option_set_id = c.option_set_id and s.answer_option_id = r.answer_option_id))
      or exists (select 1 from public.answer_option_set s where s.option_set_id = c.option_set_id
                 and not exists (select 1 from public.response_option r
                                 where r.variable_key = c.variable_key and r.answer_option_id = s.answer_option_id))
    )
  limit 1;
  if found then
    raise exception 'The choices of variable % aren''t the same as its option set''s. A variable offers exactly its set''s choices.', v_matr;
  end if;
end;
$$;

create function public.choices_match_set_check()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_keys bigint[];
begin
  if tg_table_name = 'answer_option_set' then
    select array_agg(c.variable_key) into v_keys from public.closed_ended c
    where (tg_op <> 'DELETE' and c.option_set_id = new.option_set_id)
       or (tg_op <> 'INSERT' and c.option_set_id = old.option_set_id);
  elsif tg_op = 'DELETE' then
    v_keys := array[old.variable_key];
  elsif tg_op = 'UPDATE' then
    v_keys := array[new.variable_key, old.variable_key];
  else
    v_keys := array[new.variable_key];
  end if;
  if v_keys is not null then
    perform private.choices_match_set(v_keys);
  end if;
  return null;
end;
$$;

create constraint trigger response_option_matches_set
  after insert or update or delete on public.response_option
  deferrable initially deferred
  for each row execute function public.choices_match_set_check();
create constraint trigger closed_ended_matches_set
  after insert or update on public.closed_ended
  deferrable initially deferred
  for each row execute function public.choices_match_set_check();
create constraint trigger answer_option_set_matches_variables
  after insert or update or delete on public.answer_option_set
  deferrable initially deferred
  for each row execute function public.choices_match_set_check();

-- M2 (BR45), M3 (BR53) and M4 (BR42): what a block of each kind may be, by the kind's name. The site relies on
-- the same three names (Section, Question, Group) and Make an edit won't rename or delete them.
create function private.component_kind(p_component_id bigint)
returns text
language sql
stable
set search_path = public, pg_temp
as $$
  select t.name from public.component c join public.component_type t using (component_type_id)
  where c.component_id = p_component_id;
$$;

-- M2: a component is a Question exactly when it has a question row. M4: its words are empty exactly when it
-- is a Group.
create function public.component_kind_check()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_id   bigint := case when tg_op = 'DELETE' then old.component_id else new.component_id end;
  v_kind text   := private.component_kind(v_id);
  v_text text;
begin
  if v_kind is null then
    return null; -- the component is gone (a question row can't outlive it: its foreign key)
  end if;
  if (v_kind = 'Question') <> exists (select 1 from public.question where component_id = v_id) then
    raise exception 'Component % is %: a component has a question row exactly when it is a Question.', v_id, v_kind;
  end if;
  select text into v_text from public.component where component_id = v_id;
  if (v_kind = 'Group') <> (coalesce(btrim(v_text), '') = '') then
    raise exception 'Component % is %: only a Group has no words, and a Group has none.', v_id, v_kind;
  end if;
  return null;
end;
$$;

create constraint trigger component_kind_rules
  after insert or update on public.component
  deferrable initially deferred
  for each row execute function public.component_kind_check();
create constraint trigger question_is_a_question
  after insert or delete on public.question
  deferrable initially deferred
  for each row execute function public.component_kind_check();

-- M3: where a questionnaire places a block, its parent is a Section or a Group, and a Section is never inside
-- a Group.
create function private.placement_check(p_questionnaire_id bigint, p_component_id bigint)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_parent      bigint;
  v_parent_kind text;
begin
  select parent_component_id into v_parent from public.questionnaire_component
  where questionnaire_id = p_questionnaire_id and component_id = p_component_id;
  if not found or v_parent is null then
    return;
  end if;
  v_parent_kind := private.component_kind(v_parent);
  if v_parent_kind is distinct from 'Section' and v_parent_kind is distinct from 'Group' then
    raise exception 'Component % is inside a %: only a Section or a Group holds other blocks.', p_component_id, coalesce(v_parent_kind, 'block that is gone');
  end if;
  if v_parent_kind = 'Group' and private.component_kind(p_component_id) = 'Section' then
    raise exception 'Section % is inside a Group: a Section is never inside a Group.', p_component_id;
  end if;
end;
$$;

create function public.questionnaire_component_parent_check()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  perform private.placement_check(new.questionnaire_id, new.component_id);
  return null;
end;
$$;

create constraint trigger questionnaire_component_parent_kind
  after insert or update on public.questionnaire_component
  deferrable initially deferred
  for each row execute function public.questionnaire_component_parent_check();

-- ... and a block whose kind changes is checked where it is placed, and where blocks sit inside it.
create function public.component_placements_check()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  p record;
begin
  for p in
    select questionnaire_id, component_id from public.questionnaire_component
    where component_id = new.component_id or parent_component_id = new.component_id
  loop
    perform private.placement_check(p.questionnaire_id, p.component_id);
  end loop;
  return null;
end;
$$;

create constraint trigger component_kind_placements
  after update of component_type_id on public.component
  deferrable initially deferred
  for each row execute function public.component_placements_check();

-- M5 and D1: one row per person, respondent type and registry abbreviation, ignoring case and outer spaces.
create unique index principal_investigator_email_unique on public.principal_investigator (lower(btrim(email)));
create unique index respondent_type_name_unique         on public.respondent_type (lower(btrim(name)));
create unique index registry_abbreviation_unique        on public.registry (lower(btrim(abbreviation)));

-- M6: required names can't be blank (not null lets '' and '   ' through).
alter table public.registry               add constraint registry_name_not_blank                 check (btrim(name) <> ''),
                                          add constraint registry_abbreviation_not_blank         check (btrim(abbreviation) <> '');
alter table public.principal_investigator add constraint principal_investigator_name_not_blank   check (btrim(name) <> ''),
                                          add constraint principal_investigator_email_not_blank  check (btrim(email) <> '');
alter table public.study                  add constraint study_name_not_blank                    check (btrim(name) <> '');
alter table public.respondent_type        add constraint respondent_type_name_not_blank          check (btrim(name) <> '');
alter table public.questionnaire          add constraint questionnaire_name_not_blank            check (btrim(name) <> ''),
                                          add constraint questionnaire_abbreviation_not_blank    check (btrim(abbreviation) <> '');
alter table public.wave                   add constraint wave_name_not_blank                     check (btrim(name) <> '');
alter table public.component_type         add constraint component_type_name_not_blank          check (btrim(name) <> '');
alter table public.option_set             add constraint option_set_name_not_blank               check (btrim(name) <> '');
alter table public.measurement_scale      add constraint measurement_scale_name_not_blank        check (btrim(name) <> ''),
                                          add constraint measurement_scale_abbreviation_not_blank check (btrim(abbreviation) <> '');
alter table public.category               add constraint category_term_not_blank                 check (btrim(term) <> '');
alter table public.question_scale         add constraint question_scale_item_number_not_blank    check (btrim(item_number) <> '');
alter table public.variable               add constraint variable_matr_id_not_blank              check (btrim(matr_id) <> '');
alter table public.staff                  add constraint staff_name_not_blank                    check (btrim(name) <> ''),
                                          add constraint staff_email_not_blank                   check (btrim(email) <> '');
-- (answer_option.standardized_label: answer_option_label_normal, part B, refuses a blank too)

-- M7 (BR5): a study's lead investigator never changes.
create function public.study_principal_investigator_immutable()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if new.principal_investigator_id <> old.principal_investigator_id then
    raise exception 'A study''s lead investigator can''t be changed. The contact investigator can.';
  end if;
  return new;
end;
$$;

create trigger study_principal_investigator_immutable
  before update of principal_investigator_id on public.study
  for each row execute function public.study_principal_investigator_immutable();

-- M7 (BR23): staff are never deleted; someone who leaves is set inactive.
create function public.staff_never_deleted()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  raise exception 'Staff are never deleted: set % inactive instead.', old.name;
end;
$$;

create trigger staff_never_deleted
  before delete on public.staff
  for each row execute function public.staff_never_deleted();

-- M8: where the PDF table's comment says the files are.
comment on table public.pdf_file is
  'A PDF uploaded for an extraction, once per file (by its SHA-256): its pages and size. The file itself is in Storage, bucket extraction-pdfs, as <hash>.pdf (docs/sql/3_pdf_storage.sql). It goes when no extraction draft uses it any more.';

-- The trigger functions above are for the database itself; nobody calls them over /rpc/.
revoke all on function public.response_option_count_in_wave()         from public, anon, authenticated;
revoke all on function public.extracted_draft_pdf_let_go()            from public, anon, authenticated;
revoke all on function public.choices_match_set_check()               from public, anon, authenticated;
revoke all on function public.component_kind_check()                  from public, anon, authenticated;
revoke all on function public.questionnaire_component_parent_check()  from public, anon, authenticated;
revoke all on function public.component_placements_check()            from public, anon, authenticated;
revoke all on function public.study_principal_investigator_immutable() from public, anon, authenticated;
revoke all on function public.staff_never_deleted()                   from public, anon, authenticated;
revoke all on function private.normal_answer(text)                     from public, anon, authenticated;
revoke all on function private.choices_match_set(bigint[])             from public, anon, authenticated;
revoke all on function private.component_kind(bigint)                  from public, anon, authenticated;
revoke all on function private.placement_check(bigint, bigint)         from public, anon, authenticated;
-- the site's server runs apply_draft_items() as itself, and it fires these
grant execute on function private.normal_answer(text)          to service_role;
grant execute on function private.choices_match_set(bigint[])  to service_role;
grant execute on function private.component_kind(bigint)       to service_role;
grant execute on function private.placement_check(bigint, bigint) to service_role;

-- ------ Check ----------------------------------------------------------------------------------------
do $$
begin
  assert exists (select 1 from public.component_type where name = 'Group'), 'Group is a kind of block';
  assert not exists (select 1 from public.component_type where name = 'Question Group'), 'Question Group is gone';
  assert private.normal_answer(E'  Don’t KNOW?  ') = 'don''t know', 'answer choices are normalized';
  assert private.normal_answer('Yes.') = 'yes' and private.normal_answer(' : ') is null, '... and blank is nothing';
  assert private.normal_answer('Yes. .') = private.normal_answer(private.normal_answer('Yes. .')), '... once is enough';
  assert not exists (select 1 from information_schema.columns where table_schema = 'public'
    and ((table_name = 'response_option' and column_name = 'printed_label')
      or (table_name = 'variable' and column_name = 'respondent_count'))), 'the dropped columns are gone';
  assert position('response_option_count' in
    pg_get_functiondef('private.apply_draft_items(public.draft, jsonb, bigint)'::regprocedure)) > 0,
    'an extraction can add respondents'' counts';
  assert exists (select 1 from information_schema.columns where table_schema = 'public'
    and table_name = 'extracted_draft' and column_name = 'supplement'), 'an extraction can have a file of MATR IDs and codes';
  assert to_regprocedure('public.discard_draft(bigint, bigint)') is not null, 'a draft can be discarded';
  assert (select count(*) from pg_trigger where not tgisinternal and tgname in (
    'response_option_matches_set', 'closed_ended_matches_set', 'answer_option_set_matches_variables',
    'component_kind_rules', 'question_is_a_question', 'questionnaire_component_parent_kind',
    'component_kind_placements', 'study_principal_investigator_immutable', 'staff_never_deleted',
    'extracted_draft_pdf_let_go', 'response_option_count_in_wave')) = 11,
    'every trigger is there';
end;
$$;

commit;

-- ------ Rollback ----------------------------------------------------------------------------------------
-- To take the rules away again (the dropped columns, and the old words of answer choices, can't come back):
--   drop trigger response_option_matches_set on public.response_option;
--   drop trigger closed_ended_matches_set on public.closed_ended;
--   drop trigger answer_option_set_matches_variables on public.answer_option_set;
--   drop trigger component_kind_rules on public.component;
--   drop trigger question_is_a_question on public.question;
--   drop trigger questionnaire_component_parent_kind on public.questionnaire_component;
--   drop trigger component_kind_placements on public.component;
--   drop trigger study_principal_investigator_immutable on public.study;
--   drop trigger staff_never_deleted on public.staff;
--   drop index public.principal_investigator_email_unique, public.respondent_type_name_unique, public.registry_abbreviation_unique;
--   (and each *_not_blank check: alter table ... drop constraint ...)
