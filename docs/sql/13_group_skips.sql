-- =====================================================================================================
-- MATR metadata database: a group of questions can skip, on the answers to all of them (or any)
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one. Run 9_integrity_rules.sql first.
--
-- A choice or a write-in can skip (6_skip_links.sql, 7_more_skips.sql), but some routing hangs on the answers to
-- several questions at once: "If you answered No to items 22a-d, go to Question 28." That is the group
-- holding 22a-d skipping, so it is kept where the questionnaire places the group (questionnaire_component),
-- in the same columns a write-in's skip has, and one more:
--
--   skip_condition        the answer that skips ("No")
--   skip_when             'every': the skip happens when every question in the group gets that answer;
--                         'any': when any of them does
--   skip_instruction      the routing as printed ("Go to Question 28")
--   skip_to_component_id  where it sends respondents (a question, section or group), or
--   skip_to_end           the end of the questionnaire
--
-- Only a Group skips this way: placement_check (9_integrity_rules.sql, M3), which runs as each transaction
-- ends wherever a block is placed or changes kind, now also refuses a skip on any other kind of block.
--
-- ORDER: run this before the site's new code pushes a draft with a group that skips. The site only sends
-- these columns for such a group, so other drafts push the same before and after this runs.
--
-- After it has run:
--   * docs/sql/sql_scripts.sql already builds the same.
--   * docs/database_tables.md lists the columns; regenerate it with tests/database/describe_schema.R.
--
-- Nothing is lost and no row changes. It is one transaction: if any step fails, nothing changes.
-- =====================================================================================================

begin;

alter table public.questionnaire_component
  add column if not exists skip_condition text,
  add column if not exists skip_when text,
  add column if not exists skip_instruction text,
  add column if not exists skip_to_component_id bigint,
  add column if not exists skip_to_end boolean not null default false;

alter table public.questionnaire_component
  drop constraint if exists questionnaire_component_skip_when_check,
  add constraint questionnaire_component_skip_when_check check (skip_when in ('every', 'any')),
  -- a skip goes to one place: a component, or the end
  drop constraint if exists questionnaire_component_skip_to_one_place,
  add constraint questionnaire_component_skip_to_one_place check (not (skip_to_end and skip_to_component_id is not null)),
  drop constraint if exists questionnaire_component_skip_to_component_id_fkey,
  add constraint questionnaire_component_skip_to_component_id_fkey
    foreign key (skip_to_component_id) references public.component (component_id);

create index if not exists questionnaire_component_skip_to_idx on public.questionnaire_component (skip_to_component_id);

-- M3, and now: only a Group skips
create or replace function private.placement_check(p_questionnaire_id bigint, p_component_id bigint)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_parent      bigint;
  v_parent_kind text;
  v_skips       boolean;
begin
  select parent_component_id,
         skip_condition is not null or skip_when is not null or skip_instruction is not null
           or skip_to_component_id is not null or skip_to_end
    into v_parent, v_skips
  from public.questionnaire_component
  where questionnaire_id = p_questionnaire_id and component_id = p_component_id;
  if not found then
    return;
  end if;
  if v_skips and private.component_kind(p_component_id) is distinct from 'Group' then
    raise exception 'Component % skips, but only a Group skips on the answers to the questions in it.', p_component_id;
  end if;
  if v_parent is null then
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

comment on column public.questionnaire_component.skip_condition is
  'For a Group that skips: the answer that skips, e.g. No for "If you answered No to items 22a-d, go to Question 28". Empty when it doesn''t skip.';
comment on column public.questionnaire_component.skip_when is
  'For a Group that skips: every (the skip happens when every question in the group gets the answer) or any (when any of them does).';
comment on column public.questionnaire_component.skip_instruction is
  'For a Group that skips: the routing as printed, e.g. "Go to Question 28".';
comment on column public.questionnaire_component.skip_to_component_id is
  'For a Group that skips: where it sends respondents, the question, section or group it names. Empty when nothing is linked, or when it ends the questionnaire (skip_to_end).';
comment on column public.questionnaire_component.skip_to_end is
  'For a Group that skips: it sends respondents to the end of the questionnaire.';

-- ------ Check ----------------------------------------------------------------------------------------
do $$
declare
  q bigint;
  grp bigint;
  item bigint;
begin
  assert (select count(*) from information_schema.columns where table_schema = 'public' and table_name = 'questionnaire_component'
          and column_name in ('skip_condition', 'skip_when', 'skip_instruction', 'skip_to_component_id', 'skip_to_end')) = 5,
    'a placement has the five skip columns';
  -- a group can skip; a question can't (tried, then undone)
  select qc.questionnaire_id, qc.component_id into q, grp
  from public.questionnaire_component qc join public.component c using (component_id)
  join public.component_type t using (component_type_id) where t.name = 'Group' limit 1;
  if grp is not null then
    update public.questionnaire_component set skip_condition = 'no', skip_when = 'every', skip_to_end = true
    where questionnaire_id = q and component_id = grp;
    perform private.placement_check(q, grp);
    update public.questionnaire_component set skip_condition = null, skip_when = null, skip_to_end = false
    where questionnaire_id = q and component_id = grp;
  end if;
  select qc.questionnaire_id, qc.component_id into q, item
  from public.questionnaire_component qc join public.question using (component_id) limit 1;
  if item is not null then
    begin
      update public.questionnaire_component set skip_condition = 'no' where questionnaire_id = q and component_id = item;
      perform private.placement_check(q, item);
      raise exception 'a question was let skip';
    exception when raise_exception then
      if sqlerrm = 'a question was let skip' then raise; end if;
    end;
  end if;
end;
$$;

commit;
