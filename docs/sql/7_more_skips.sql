-- =====================================================================================================
-- MATR metadata database: write-ins can skip too, and a skip can end the questionnaire
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one.
--
-- 6_skip_links.sql linked a choice's skip instruction to the component it sends respondents to. This adds:
--
--   * response_option.skip_to_end: the choice sends respondents to the end of the questionnaire ("If no,
--     you have finished", or "Go to Section 5" where Section 5 is only the closing words). It has no
--     component to point at, so skip_to_component_id stays empty.
--   * The same routing for a write-in (open_ended), for an answer it names: "How many times have you been
--     pregnant? If none, go to Question 57." is skip_condition "None", skip_instruction "Go to Question 57"
--     (as printed), and skip_to_component_id the question it sends respondents to (or skip_to_end).
--
-- ORDER: run this before the site's new code pushes a draft that uses them. The site only sends these
-- columns when a choice or write-in has them, so drafts without them push the same before and after this
-- runs; a draft with a write-in skip or an "End of Questionnaire" fails to push until it has run.
--
-- After it has run:
--   * docs/sql/sql_scripts.sql already builds the same (open_ended and response_option, and their descriptions).
--   * docs/database_tables.md lists the columns; regenerate it with tests/database/describe_schema.R.
--
-- Nothing is lost. It is one transaction: if any step fails, nothing changes.
-- =====================================================================================================

begin;

alter table public.response_option
  add column if not exists skip_to_end boolean not null default false;

alter table public.open_ended
  add column if not exists skip_condition text,
  add column if not exists skip_instruction text,
  add column if not exists skip_to_component_id bigint,
  add column if not exists skip_to_end boolean not null default false;

alter table public.open_ended
  drop constraint if exists open_ended_skip_to_component_id_fkey;
alter table public.open_ended
  add constraint open_ended_skip_to_component_id_fkey
  foreign key (skip_to_component_id) references public.component (component_id);

-- a skip goes to one place: a component, or the end
alter table public.response_option
  drop constraint if exists response_option_skip_to_one_place;
alter table public.response_option
  add constraint response_option_skip_to_one_place check (not (skip_to_end and skip_to_component_id is not null));
alter table public.open_ended
  drop constraint if exists open_ended_skip_to_one_place;
alter table public.open_ended
  add constraint open_ended_skip_to_one_place check (not (skip_to_end and skip_to_component_id is not null));

create index if not exists open_ended_skip_to_idx on public.open_ended (skip_to_component_id);

comment on column public.response_option.skip_to_component_id is
  'Where the skip instruction sends respondents: the question, section or question group it names. Empty when nothing is linked, or when it ends the questionnaire (skip_to_end).';
comment on column public.response_option.skip_to_end is
  'The skip instruction sends respondents to the end of the questionnaire.';
comment on column public.open_ended.skip_condition is
  'The answer that skips, e.g. None for "If none, go to Question 57". Empty when the write-in has no skip.';
comment on column public.open_ended.skip_instruction is
  'The routing printed for the write-in, e.g. "Go to Question 57".';
comment on column public.open_ended.skip_to_component_id is
  'Where the skip sends respondents: the question, section or question group it names. Empty when nothing is linked, or when it ends the questionnaire (skip_to_end).';
comment on column public.open_ended.skip_to_end is
  'The skip sends respondents to the end of the questionnaire.';

commit;
