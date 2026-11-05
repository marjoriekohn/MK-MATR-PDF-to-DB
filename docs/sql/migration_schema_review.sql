-- The schema review of November 2026, for a database built before it was added to docs/sql/1_setup.sql (which
-- builds all of it for a new one). On the test project (rqygcjgftmtatzrzcwcf), run this instead of 1_setup.sql:
-- 1_setup.sql can't put back answer_option_set's rows without the order this adds.
--
--   1. An investigator may have no email, but a study's contact has one.
--   2. The PDFs' bucket is public, and the dashboard reads which files each wave was extracted from.
--   3. Deleting a wave or a questionnaire takes what only it owns, and what is left owned by nothing; what
--      anything else still uses stays.
--   4. An option set's name is no longer typed: it is its choices, in their order (public.name(option_set)).
--
-- One transaction: if any statement fails, nothing changes. Run it once; a second run stops at its first
-- statement (the bucket aside, nothing it makes can be made twice).

begin;

-- ====== 1. Investigators' emails ==========================================================================

alter table public.principal_investigator alter column email drop not null;

create function public.contact_has_email()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_name text;
begin
  select p.name into v_name
  from public.study s join public.principal_investigator p on p.principal_investigator_id = s.contact_principal_investigator_id
  where p.email is null
    and case when tg_table_name = 'study' then s.study_id = new.study_id
             else p.principal_investigator_id = new.principal_investigator_id end
  limit 1;
  if found then
    raise exception '% has no email, so they can''t be a study''s contact.', v_name;
  end if;
  return null;
end;
$$;

create constraint trigger contact_has_email
  after insert or update of contact_principal_investigator_id on public.study
  deferrable initially deferred
  for each row execute function public.contact_has_email();
create constraint trigger contact_keeps_email
  after update of email on public.principal_investigator
  deferrable initially deferred
  for each row execute function public.contact_has_email();
revoke all on function public.contact_has_email() from public, anon, authenticated;

comment on column public.study.contact_principal_investigator_id is 'Who to contact about the study. Empty when not known; someone with an email.';
comment on column public.principal_investigator.email is 'Unique, ignoring case and outer spaces. Empty when not known, but a study''s contact has one.';

-- ====== 2. Public files, named by their wave =============================================================

update storage.buckets set public = true where id = 'extraction-pdfs';

-- the PDF a wave was extracted from goes with it (or when it is extracted again from another file), once
-- nothing else uses it
create function public.pdf_file_let_go()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  was jsonb := to_jsonb(old);   -- an extracted_draft or a wave_file row: each names its files its own way
  sha text;
begin
  foreach sha in array array_remove(array[was ->> 'pdf_sha256', was -> 'supplement' ->> 'sha256', was ->> 'supplement_sha256'], null) loop
    if not exists (select 1 from public.extracted_draft where pdf_sha256 = sha or supplement ->> 'sha256' = sha)
       and not exists (select 1 from public.wave_file where pdf_sha256 = sha or supplement_sha256 = sha) then
      delete from public.pdf_file where pdf_sha256 = sha;
    end if;
  end loop;
  return null;
end;
$$;
drop trigger extracted_draft_pdf_let_go on public.extracted_draft;
drop function public.extracted_draft_pdf_let_go();
create trigger extracted_draft_pdf_let_go
  after delete on public.extracted_draft
  for each row execute function public.pdf_file_let_go();
create trigger wave_file_pdf_let_go
  after delete or update of pdf_sha256, supplement_sha256 on public.wave_file
  for each row execute function public.pdf_file_let_go();
revoke all on function public.pdf_file_let_go() from public, anon, authenticated;

create policy "Dashboard can read" on public.wave_file for select to anon, authenticated using (true);
grant select on public.wave_file to anon, authenticated;
create policy "Dashboard can read a wave's files" on public.pdf_file for select to anon, authenticated
  using (exists (select 1 from public.wave_file f
                 where f.pdf_sha256 = pdf_file.pdf_sha256 or f.supplement_sha256 = pdf_file.pdf_sha256));
grant select on public.pdf_file to anon, authenticated;

comment on table public.pdf_file is 'A PDF uploaded for an extraction (the questionnaire, or its file of Variable Names and Coding), once per file (by its SHA-256): its pages and size. The file itself is in Storage, bucket extraction-pdfs, as <hash>.pdf. Kept while a wave was extracted from it (wave_file) or an extraction draft uses it; the dashboard sees only a wave''s.';
comment on table public.wave_file is
  'The PDF and the file of Variable Names and Coding a wave''s extraction was read from, written when the extraction is applied. Goes with the wave (and the files with it, when nothing else uses them); the dashboard reads it.';

-- ====== 3. What a wave and a questionnaire own goes with them ============================================
-- (as docs/sql/1_setup.sql, section 2 and section 4 "What nothing owns", explain)

alter table public.wave
  drop constraint wave_questionnaire_id_fkey,
  add constraint wave_questionnaire_id_fkey
    foreign key (questionnaire_id) references public.questionnaire (questionnaire_id) on delete cascade;
alter table public.questionnaire_component
  drop constraint questionnaire_component_questionnaire_id_fkey,
  add constraint questionnaire_component_questionnaire_id_fkey
    foreign key (questionnaire_id) references public.questionnaire (questionnaire_id) on delete cascade,
  drop constraint questionnaire_component_parent_fk,
  add constraint questionnaire_component_parent_fk
    foreign key (questionnaire_id, parent_component_id)
    references public.questionnaire_component (questionnaire_id, component_id) on delete cascade;
alter table public.questionnaire_question
  drop constraint questionnaire_question_component_fk,
  add constraint questionnaire_question_component_fk
    foreign key (questionnaire_id, component_id)
    references public.questionnaire_component (questionnaire_id, component_id) on delete cascade;
alter table public.wave_question
  drop constraint wave_question_wave_fk,
  add constraint wave_question_wave_fk
    foreign key (questionnaire_id, wave_id) references public.wave (questionnaire_id, wave_id) on delete cascade,
  drop constraint wave_question_question_fk,
  add constraint wave_question_question_fk
    foreign key (questionnaire_id, component_id)
    references public.questionnaire_question (questionnaire_id, component_id) on delete cascade;
alter table public.variable
  drop constraint variable_questionnaire_question_fk,
  add constraint variable_questionnaire_question_fk
    foreign key (questionnaire_id, component_id)
    references public.questionnaire_question (questionnaire_id, component_id) on delete cascade;
alter table public.question_category
  drop constraint question_category_component_id_fkey,
  add constraint question_category_component_id_fkey
    foreign key (component_id) references public.question (component_id) on delete cascade;

-- A component no questionnaire places any more. Its question row goes with it, and that row's MeSH tags.
create function public.forget_unplaced_component()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  delete from public.component c
  where c.component_id = old.component_id
    and not exists (select 1 from public.questionnaire_component p where p.component_id = c.component_id)
    and not exists (select 1 from public.question_scale s where s.component_id = c.component_id)
    and not exists (select 1 from public.questionnaire_component p where p.skip_to_component_id = c.component_id)
    and not exists (select 1 from public.response_option r where r.skip_to_component_id = c.component_id)
    and not exists (select 1 from public.open_ended o where o.skip_to_component_id = c.component_id);
  return null;
end;
$$;

create constraint trigger component_left_unplaced
  after delete or update of component_id on public.questionnaire_component
  deferrable initially deferred
  for each row execute function public.forget_unplaced_component();

-- A question its questionnaire asks in no wave (BR69): its placement goes, and with it its settings and
-- variables (and then the component, if nothing else places it). A follow-up that goes may leave the question
-- above it unasked and named by no one, so that one is looked at next, and so on up.
create function public.forget_unasked_question()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_question bigint := old.component_id;
  v_parent   bigint;
begin
  while v_question is not null loop
    select q.parent_question_id into v_parent from public.questionnaire_question q
    where q.questionnaire_id = old.questionnaire_id and q.component_id = v_question;
    delete from public.questionnaire_component p
    where p.questionnaire_id = old.questionnaire_id and p.component_id = v_question
      and exists (select 1 from public.questionnaire_question q
                  where q.questionnaire_id = p.questionnaire_id and q.component_id = p.component_id)
      and not exists (select 1 from public.wave_question w
                      where w.questionnaire_id = p.questionnaire_id and w.component_id = p.component_id)
      and not exists (select 1 from public.questionnaire_question f
                      where f.questionnaire_id = p.questionnaire_id and f.parent_question_id = p.component_id);
    exit when not found;
    v_question := v_parent;
  end loop;
  return null;
end;
$$;

create constraint trigger question_left_unasked
  after delete or update of wave_id, component_id on public.wave_question
  deferrable initially deferred
  for each row execute function public.forget_unasked_question();

-- A stem no question has.
create function public.forget_unused_stem()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  delete from public.question_stem t
  where t.question_stem_id = old.question_stem_id
    and not exists (select 1 from public.question q where q.question_stem_id = t.question_stem_id);
  return null;
end;
$$;

create constraint trigger question_stem_left_unused
  after delete or update of question_stem_id on public.question
  deferrable initially deferred
  for each row execute function public.forget_unused_stem();

revoke all on function public.forget_unplaced_component() from public, anon, authenticated;
revoke all on function public.forget_unasked_question()   from public, anon, authenticated;
revoke all on function public.forget_unused_stem()        from public, anon, authenticated;

-- ====== 4. An option set's name is its choices ===========================================================

-- Each set's choices take the order its name listed them in. A name cut short ("...") or changed by hand misses
-- some: those follow, in the order the set's first variable offers them, then in the order they were added.
alter table public.answer_option_set add column display_order integer;
update public.answer_option_set m
set display_order = o.n
from (
  select x.option_set_id, x.answer_option_id,
         row_number() over (
           partition by x.option_set_id
           order by nullif(strpos(' / ' || lower(s.name) || ' / ', ' / ' || a.standardized_label || ' / '), 0) nulls last,
                    (select r.display_order from public.closed_ended c
                     join public.response_option r on r.variable_key = c.variable_key and r.answer_option_id = x.answer_option_id
                     where c.option_set_id = x.option_set_id order by c.variable_key limit 1) nulls last,
                    x.answer_option_id
         ) as n
  from public.answer_option_set x
  join public.option_set s on s.option_set_id = x.option_set_id
  join public.answer_option a on a.answer_option_id = x.answer_option_id
) o
where m.option_set_id = o.option_set_id and m.answer_option_id = o.answer_option_id;
-- The update queued answer_option_set's deferred check (answer_option_set_matches_variables), and a table can't be
-- altered while it has one waiting: run it now (the choices didn't change, so it passes).
set constraints answer_option_set_matches_variables immediate;
alter table public.answer_option_set
  alter column display_order set not null,
  add constraint answer_option_set_display_order_check check (display_order > 0),
  add constraint answer_option_set_display_order_unique unique (option_set_id, display_order) deferrable initially immediate;

-- One set for each combination of choices: two sets with the same ones stop the migration here (merge them on
-- Make an edit first, then run it again).
do $$
declare
  twins text;
begin
  with choices as (
    select s.option_set_id, s.name,
           array(select answer_option_id from public.answer_option_set where option_set_id = s.option_set_id order by 1) as ids
    from public.option_set s
  )
  select string_agg(names, '; ') into twins
  from (select string_agg(name, ' = ' order by option_set_id) as names from choices group by ids having count(*) > 1) d;
  if twins is not null then
    raise exception 'These option sets have exactly the same choices, so they would be one set: %.', twins;
  end if;
end;
$$;

-- what the old names said that the new ones won't, for the record
do $$
declare
  n integer;
begin
  select count(*) into n
  from public.option_set s
  where lower(btrim(s.name)) is distinct from (
    select string_agg(a.standardized_label, ' / ' order by m.display_order)
    from public.answer_option_set m join public.answer_option a using (answer_option_id)
    where m.option_set_id = s.option_set_id);
  raise notice '% option set name(s) were not exactly their choices (cut short, renamed or numbered); they are now.', n;
end;
$$;

alter table public.option_set drop column name;   -- with its index (option_set_name_unique) and check

-- An option set's name: its choices in its order, "never / sometimes / often". The REST API offers it as the
-- set's column name (a computed field: select=option_set_id,name), so the dashboard and the site read it as
-- before; SQL reads it as s.name or public.name(s).
create function public.name(s public.option_set)
returns text
language sql
stable
set search_path = ''
as $$
  select string_agg(a.standardized_label, ' / ' order by m.display_order)
  from public.answer_option_set m join public.answer_option a on a.answer_option_id = m.answer_option_id
  where m.option_set_id = s.option_set_id;
$$;

-- BR109: one set for each combination of choices, whatever their order: a set is its choices. A set with no
-- choices (on its way in or out) is not compared.
create function public.option_set_choices_unique()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_set   bigint := case when tg_op = 'DELETE' then old.option_set_id else new.option_set_id end;
  v_other public.option_set;
begin
  select o.* into v_other
  from public.option_set o
  where o.option_set_id <> v_set
    and exists (select 1 from public.answer_option_set where option_set_id = v_set)
    and array(select answer_option_id from public.answer_option_set where option_set_id = o.option_set_id order by 1)
      = array(select answer_option_id from public.answer_option_set where option_set_id = v_set order by 1)
  limit 1;
  if found then
    raise exception 'The option set "%" already has exactly these choices. A set is its choices, so there is one of each.',
      public.name(v_other);
  end if;
  return null;
end;
$$;

create constraint trigger option_set_choices_unique
  after insert or update or delete on public.answer_option_set
  deferrable initially deferred
  for each row execute function public.option_set_choices_unique();
revoke all on function public.option_set_choices_unique() from public, anon, authenticated;
revoke all on function public.name(public.option_set) from public;
grant execute on function public.name(public.option_set) to anon, authenticated, service_role;

comment on table public.option_set is 'A set of answer options, one for each combination; each variable offering it codes and orders them its own way. Its name (public.name(option_set), read as a column) is its choices in their order.';
comment on table public.answer_option_set is 'The answer options of an option set, in the order its name lists them.';


-- ====== Last: what nothing owns already ==================================================================
-- After every alter table above: a table can't be altered while row changes have its deferred checks waiting.
-- What nothing owns already (left by deletes and rewordings before this) goes now, by part 3's rule.
do $$
declare
  n         integer;
  questions integer := 0;
  components integer;
  stems     integer;
begin
  loop   -- a follow-up that goes can leave the question above it named by no one
    delete from public.questionnaire_component p
    using public.questionnaire_question q
    where q.questionnaire_id = p.questionnaire_id and q.component_id = p.component_id
      and not exists (select 1 from public.wave_question w
                      where w.questionnaire_id = p.questionnaire_id and w.component_id = p.component_id)
      and not exists (select 1 from public.questionnaire_question f
                      where f.questionnaire_id = p.questionnaire_id and f.parent_question_id = p.component_id);
    get diagnostics n = row_count;
    questions := questions + n;
    exit when n = 0;
  end loop;
  delete from public.component c
  where not exists (select 1 from public.questionnaire_component p where p.component_id = c.component_id)
    and not exists (select 1 from public.question_scale s where s.component_id = c.component_id)
    and not exists (select 1 from public.questionnaire_component p where p.skip_to_component_id = c.component_id)
    and not exists (select 1 from public.response_option r where r.skip_to_component_id = c.component_id)
    and not exists (select 1 from public.open_ended o where o.skip_to_component_id = c.component_id);
  get diagnostics components = row_count;
  delete from public.question_stem t
  where not exists (select 1 from public.question q where q.question_stem_id = t.question_stem_id);
  get diagnostics stems = row_count;
  raise notice 'Removed what nothing owned: % question(s) asked in no wave, % component(s) printed nowhere, % unused stem(s).',
    questions, components, stems;
end;
$$;

commit;

notify pgrst, 'reload schema';
