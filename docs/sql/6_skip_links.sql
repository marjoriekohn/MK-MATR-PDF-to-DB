-- =====================================================================================================
-- MATR metadata database: a choice's skip instruction points at where it sends respondents
--
-- FOR REVIEW. NOT RUN BY THE SITE OR ITS TESTS. Run it by hand, once, in the Supabase SQL editor: on the
-- test project first (the tests need it), then on the real one.
--
-- A choice can carry routing printed beside it ("Go to Question 32"). The words stay in
-- response_option.skip_instruction, as printed. The new response_option.skip_to_component_id says which
-- component that is: the question (or the section) respondents are sent to, so the link survives
-- renumbering and the dashboard can follow it. It is empty where nothing is linked ("End of survey").
--
-- ORDER: run this FIRST, then deploy the site's new code. The new code reads and writes the column, so
-- the Edit Extraction push and Make an edit fail until this has run. The old code never mentions it, so
-- it works before and after this runs.
--
-- After it has run:
--   * docs/sql/sql_scripts.sql already builds the same (response_option, its index and its description).
--   * docs/database_tables.md lists the columns; regenerate it with tests/database/describe_schema.R.
--
-- Nothing is lost. It is one transaction: if any step fails, nothing changes.
-- =====================================================================================================

begin;

alter table public.response_option
  add column if not exists skip_to_component_id bigint;

alter table public.response_option
  drop constraint if exists response_option_skip_to_component_id_fkey;
alter table public.response_option
  add constraint response_option_skip_to_component_id_fkey
  foreign key (skip_to_component_id) references public.component (component_id);

create index if not exists response_option_skip_to_idx on public.response_option (skip_to_component_id);

comment on column public.response_option.skip_to_component_id is
  'Where the skip instruction sends respondents: the question (or section) it names. Empty when nothing is linked.';

commit;
