-- =====================================================================================================
-- MATR metadata database: the whole database in one script (Supabase, PostgreSQL 15+)
--
-- Builds every table, rule, function and permission the site and the public dashboard use, on a
-- Supabase project: a new one, or one to start over. Part 0 first removes whatever tables and functions
-- are there, so the build starts clean, and the last part puts back these tables' rows:
--
--   KEPT:  staff, component_type, subject, time_frame, principal_investigator, respondent_type, registry, study,
--          questionnaire, wave, pdf_file and wave_file, with their IDs. They are copied to a schema of their own
--          (matr_keep) before anything is removed, and back once the database is built.
--   LOST:  every other row (what the questionnaires print, their variables, drafts, their history, ...).
--
-- On the TEST project (rqygcjgftmtatzrzcwcf) nothing is lost: every table's rows are kept and put back,
-- so the questionnaires extracted there stay as test data. tests/database/reset_test_database.sql trims
-- it to them. Run tests/database/test_functions.sql again afterwards: its functions go with public.
--
-- Part 0 refuses to run if any other table holds a row, so it can't empty a database that is in use.
-- Rows left from trying the site out can be cleared anyway, on purpose: see the top of part 0. Once
-- real data is in, change the database with migrations instead of running this again.
--
-- The PDFs stay: their files in Storage (bucket extraction-pdfs) aren't touched, and their rows
-- (pdf_file) are kept, and so is what says which wave was extracted from which (wave_file). Sign-in accounts (Authentication > Users) are not touched.
--
--   0. Clear the schemas           keep the tables above; remove public and private
--   1. Create tables               columns, primary keys, unique and check constraints
--   2. Add references              foreign keys
--   3. Indexes                     for foreign keys and lookups not already covered by a key
--   4. Triggers and functions      rules a constraint can't express, and the functions the site calls
--   5. Security & permissions      Row Level Security, policies and grants
--   6. Descriptions                comments shown in the Supabase table editor
--   7. Starting data               component types and subjects
--   Rules the site enforces instead
--   Checks of the database build   (rolled back: they leave nothing behind)
--   Staff access                   what a signed-in staff member may read
--   Put back the kept rows         the tables above (every table, on the test project), from matr_keep
--
-- Each part is its own transaction: if a statement fails, its part makes nothing, and the parts before
-- it stay. If a part fails, fix the cause and run the whole file again. The kept rows stay in matr_keep
-- until they are back: a run that finds matr_keep there uses the rows in it, and doesn't copy again.
--
-- On a new project there are no staff to keep: add them afterwards (public.staff, one row per person,
-- with the email they sign in with). The database knows a signed-in person as staff by that email
-- (public.is_active_staff()).
--
-- Business rules (BRn) are numbered as in the design.
-- =====================================================================================================


-- =====================================================================================================
-- 0. CLEAR THE SCHEMAS (KEEPING STAFF, THE STARTING LISTS, REGISTRIES, STUDIES, QUESTIONNAIRES, WAVES AND PDFS)
-- =====================================================================================================
-- Copies the kept tables' rows to matr_keep, then removes the schemas public and private with everything
-- in them, and makes public again with the permissions a new Supabase project starts with (the build
-- expects a new project, and takes the permissions it doesn't want away again).
--
-- If the other tables hold only rows you mean to throw away (left from trying the site out), and you
-- have checked which project this is, uncomment the next line to clear them anyway. EVERY ROW OF THEM
-- IS LOST.
-- set matr.clear_with_data = 'yes';
--
-- On the TEST project every table's rows are kept, unless the next line is uncommented: then the test project
-- is rebuilt like any other, keeping only the tables listed at the top (its extracted questionnaires, drafts
-- and history are lost), for a change to the tables that the old rows wouldn't fit.
-- set matr.test_keep_only_lists = 'yes';

begin;

-- Nothing else may hold data, unless clearing it was asked for
do $$
declare
  t       regclass;
  holding text[] := array[]::text[];
  has_row boolean;
begin
  -- the test project keeps every row (the next block), so nothing is lost there, unless asked to keep only the lists
  if (select system_identifier::text from pg_control_system()) = '7692131729119822239' then
    if coalesce(current_setting('matr.test_keep_only_lists', true), '') = 'yes' then
      raise notice 'This is the MATR test database: keeping only the listed tables, as matr.test_keep_only_lists asks.';
    else
      raise notice 'This is the MATR test database: keeping every table''s rows.';
    end if;
    return;
  end if;
  for t in
    select c.oid::regclass from pg_class c
    where c.relnamespace in (select oid from pg_namespace where nspname in ('public', 'private'))
      and c.relkind in ('r', 'p')
      and not (c.relnamespace = 'public'::regnamespace
               and c.relname in ('staff', 'component_type', 'subject', 'time_frame', 'principal_investigator', 'respondent_type',
                                 'registry', 'study', 'questionnaire', 'wave', 'pdf_file', 'wave_file'))   -- kept
    order by 1
  loop
    execute format('select exists (select 1 from %s)', t) into has_row;
    if has_row then
      holding := holding || t::text;
    end if;
  end loop;
  if cardinality(holding) > 0 then
    if coalesce(current_setting('matr.clear_with_data', true), '') = 'yes' then
      raise notice 'Clearing tables that held data, as matr.clear_with_data asks: %.', array_to_string(holding, ', ');
    else
      raise exception 'Refusing to clear the database: these tables hold data: %. If those rows are only from trying the site out, see the top of part 0.',
        array_to_string(holding, ', ');
    end if;
  end if;
end;
$$;

-- The kept rows, each as JSON, so they come back whatever columns the old tables had. On the test
-- project, every table in public.
do $$
declare
  t    text;
  n    bigint;
  kept text[] := array['staff', 'component_type', 'subject', 'time_frame', 'principal_investigator', 'respondent_type',
                       'registry', 'study', 'questionnaire', 'wave', 'pdf_file', 'wave_file'];
begin
  if to_regnamespace('matr_keep') is not null then
    raise notice 'matr_keep is already there (an earlier run stopped before putting its rows back): keeping those rows.';
    return;
  end if;
  if (select system_identifier::text from pg_control_system()) = '7692131729119822239'
     and coalesce(current_setting('matr.test_keep_only_lists', true), '') <> 'yes' then
    kept := kept || array(select c.relname::text from pg_class c
                          where c.relnamespace = 'public'::regnamespace and c.relkind in ('r', 'p')
                            and c.relname <> all (kept)
                          order by 1);
  end if;
  create schema matr_keep;
  foreach t in array kept loop
    if to_regclass('public.' || t) is not null then
      execute format('create table matr_keep.%I as select to_jsonb(r) as kept from public.%I r', t, t);
      execute format('select count(*) from matr_keep.%I', t) into n;
      raise notice 'Keeping % row(s) of %.', n, t;
    end if;
  end loop;
end;
$$;

drop schema if exists private cascade;
drop schema if exists public cascade;

-- public as a new Supabase project has it
create schema public;
grant usage on schema public to postgres, anon, authenticated, service_role;
grant all privileges on all tables    in schema public to postgres, anon, authenticated, service_role;
grant all privileges on all functions in schema public to postgres, anon, authenticated, service_role;
grant all privileges on all sequences in schema public to postgres, anon, authenticated, service_role;
alter default privileges in schema public grant all on tables    to postgres, anon, authenticated, service_role;
alter default privileges in schema public grant all on functions to postgres, anon, authenticated, service_role;
alter default privileges in schema public grant all on sequences to postgres, anon, authenticated, service_role;

commit;


-- =====================================================================================================
-- THE BUILD (sections 1-7): every table, rule, function and permission the site and the public
-- dashboard use, in one transaction.
-- =====================================================================================================

begin;

-- Internal functions the site's public functions call, but the REST API must not offer.
create schema private;

-- Every table's own ID is a random 9-digit number (100000000 to 999999999), picked by the database
-- when a row is added: e.g. registry_id 482019375. It tries again in the rare case the number is
-- already taken. It is the column's default, so it is made here, before the tables.
-- (9 digits: short enough to read and type, and far below the 2^53 that R and JSON hold exactly.)
create function private.random_id(p_table text, p_column text)
returns bigint
language plpgsql
volatile
set search_path = ''
as $$
declare
  candidate bigint;
  taken     boolean;
begin
  loop
    candidate := 100000000 + floor(random() * 900000000)::bigint;
    execute format('select exists (select 1 from public.%I where %I = $1)', p_table, p_column)
      into taken using candidate;
    if not taken then
      return candidate;
    end if;
  end loop;
end;
$$;

-- An answer choice as it is kept (answer_option_label_normal): lower case, straight quotes, one space between
-- words, no question mark, and no full stop or colon at the end. The site's normal_answer() (R/service_draft.R)
-- does the same, and writes some wordings one way too (a range of numbers as "1-2", "hrs" as "hours"), so what
-- it gives always passes this check.
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


-- =====================================================================================================
-- 1. CREATE TABLES
-- =====================================================================================================

-- ------ Registries and studies -----------------------------------------------------------------------

create table public.registry (
  registry_id        bigint  primary key default private.random_id('registry', 'registry_id'),
  name               text    not null,
  abbreviation       text    not null,
  participant_count  integer
    constraint registry_participant_count_check check (participant_count >= 0),
  description        text
);

create table public.principal_investigator (
  principal_investigator_id bigint primary key default private.random_id('principal_investigator', 'principal_investigator_id'),
  name   text not null,
  email  text             -- may be empty, but not for a study's contact (contact_has_email, section 4)
);

create table public.study (
  study_id                          bigint primary key default private.random_id('study', 'study_id'),
  registry_id                       bigint not null,                     -- BR1, BR2
  principal_investigator_id         bigint,                              -- BR3, BR4 (optional)
  contact_principal_investigator_id bigint,                              -- BR6-BR9
  name              text not null,
  status            text
    constraint study_status_check check (status in ('Active', 'Ended')),
  description       text,
  methods           text,
  acknowledgements  text
);

-- ------ Questionnaires and waves ---------------------------------------------------------------------

create table public.respondent_type (
  respondent_type_id bigint  primary key default private.random_id('respondent_type', 'respondent_type_id'),
  name         text    not null,
  minimum_age  integer
    constraint respondent_type_minimum_age_check check (minimum_age >= 0),
  maximum_age  integer
    constraint respondent_type_maximum_age_check check (maximum_age <= 120),
  description  text,
  constraint respondent_type_age_range check (minimum_age <= maximum_age)   -- BR15
);

-- A name and an abbreviation are each unique within a study, ignoring case and outer spaces
-- (questionnaire_name_unique_in_study, questionnaire_abbreviation_unique_in_study, section 3).
create table public.questionnaire (
  questionnaire_id   bigint  primary key default private.random_id('questionnaire', 'questionnaire_id'),
  study_id           bigint  not null,                                   -- BR10
  respondent_type_id bigint  not null,                                   -- BR12, BR13
  name               text    not null,
  abbreviation       text    not null,
  upvote_count       integer not null default 0                          -- BR16
    constraint questionnaire_upvote_count_nonnegative check (upvote_count >= 0),
  -- the total across all its waves, for when the waves' own counts aren't known; the waves' counts
  -- can't add up to more (wave_counts_within_questionnaire, section 4)
  participant_count  integer
    constraint questionnaire_participant_count_check check (participant_count >= 0),
  description        text
);

create table public.wave (
  wave_id           bigint  primary key default private.random_id('wave', 'wave_id'),
  questionnaire_id  bigint  not null,                                    -- BR18
  name              text    not null,
  respondent_count  integer
    constraint wave_respondent_count_check check (respondent_count >= 0),
  administered_date date,
  constraint wave_name_unique_in_questionnaire unique (questionnaire_id, name),   -- BR21
  -- the target of wave_question's foreign key, so a wave and a question share one questionnaire (BR70)
  constraint wave_questionnaire_wave_key unique (questionnaire_id, wave_id)
);

-- ------ What is printed ------------------------------------------------------------------------------

create table public.component_type (
  component_type_id bigint primary key default private.random_id('component_type', 'component_type_id'),
  name              text   not null
    constraint component_type_name_unique unique,                        -- BR41
  description       text
);

-- A component is a wording, kept once: one for each kind and words (but for runs of spaces), however many
-- questionnaires print it and however often (component_words_unique, section 3). A Group has no words, so there
-- is one Group, placed wherever a questionnaire holds blocks together.
create table public.component (
  component_id      bigint primary key default private.random_id('component', 'component_id'),
  component_type_id bigint not null,                                     -- BR39, BR40
  text              text                                                 -- empty only for Groups (BR42: component_kind_rules)
);

-- QUESTION is a subtype of COMPONENT (BR43, BR44). Its one attribute of its own is the stem it shares with
-- the questions that differ from it only in who they ask about.
create table public.question (
  component_id     bigint primary key,
  question_stem_id bigint
);

-- What questions that differ only in who they ask about have in common: "Has [subject] ever repeated a grade?"
-- for "Has Twin A ever repeated a grade?" and "Has Twin B ever repeated a grade?". Each question keeps its own
-- printed words, number and variables; [subject] is filled in with a variable's subject when the stem is shown.
-- So do questions that differ only in when they ask about ("... in your lifetime?", "... during the last 12
-- months?"): "on how many occasions have you smoked cigarettes [time frame]?", filled in with a variable's time frame.
-- One stem for each wording, but for runs of spaces: question_stem_text_unique, section 3.
create table public.question_stem (
  question_stem_id bigint primary key default private.random_id('question_stem', 'question_stem_id'),
  text             text   not null
    constraint question_stem_text_not_blank check (btrim(text) <> '')
);

-- Where a questionnaire prints a component: a placement. A questionnaire can print the same component more than
-- once (an instruction repeated before each battery, the same item in Twin A's part and Twin B's), so a placement
-- has a number of its own, and everything about a place in the questionnaire (what holds it, its order, a
-- question's number, follow-ups and variables, where a skip sends respondents) names the placement, not the
-- component. The component is the wording, shared by every place that prints it.
create table public.questionnaire_component (
  placement_id        bigint  primary key default private.random_id('questionnaire_component', 'placement_id'),
  questionnaire_id    bigint  not null,                                  -- BR46, BR47
  component_id        bigint  not null,                                  -- BR48, BR49
  parent_placement_id bigint,            -- the Section or Group it is in, in the same questionnaire
  display_order       integer not null
    constraint questionnaire_component_display_order_check check (display_order > 0),
  -- a Group can skip on the answers to the questions in it ("If you answered No to items 22a-d, go to Question 28")
  skip_condition      text,              -- the answer that skips ("No")
  skip_when           text               -- every question in the group gets it, or any
    constraint questionnaire_component_skip_when_check check (skip_when in ('every', 'any')),
  skip_instruction    text,              -- the routing printed for it
  skip_to_placement_id bigint,           -- where it sends respondents
  skip_to_end         boolean not null default false, -- or to the end of the questionnaire
  constraint questionnaire_component_skip_to_one_place check (not (skip_to_end and skip_to_placement_id is not null)),
  -- what the foreign keys below name: a placement and its questionnaire (and component)
  constraint questionnaire_component_in_questionnaire unique (questionnaire_id, placement_id),
  constraint questionnaire_component_places unique (placement_id, questionnaire_id, component_id),
  -- BR54, the direct case (a trigger catches longer cycles)
  constraint questionnaire_component_not_own_parent check (parent_placement_id <> placement_id),
  -- BR55: display order unique among siblings; NULLS NOT DISTINCT makes the top level one group.
  -- Deferrable so a change can move components past each other.
  constraint questionnaire_component_display_order_unique
    unique nulls not distinct (questionnaire_id, parent_placement_id, display_order)
    deferrable initially immediate
);

-- A placement of a question: its settings there. Keyed by the placement; questionnaire_id and component_id say
-- which questionnaire and question it is (the foreign key keeps them the placement's).
create table public.questionnaire_question (
  placement_id                bigint  primary key,
  questionnaire_id            bigint  not null,
  component_id                bigint  not null,
  parent_question_placement_id bigint, -- the question it follows, placed in the same questionnaire
  self_contained              boolean not null default true,
  condition                   text,
  constraint questionnaire_question_in_questionnaire unique (questionnaire_id, placement_id),
  -- BR63, the direct case (a trigger catches longer cycles)
  constraint questionnaire_question_not_own_parent check (parent_question_placement_id <> placement_id)
);

create table public.wave_question (
  questionnaire_id bigint not null,
  wave_id          bigint not null,
  placement_id     bigint not null,
  question_number  text   not null,      -- its place among the wave's questions (1, 2, 3...), worked out by the site
  printed_number   text,                 -- the number printed beside it (1a, 12, Q7); can repeat, or be empty
  primary key (wave_id, placement_id),
  -- BR71: question number (its place) unique within a wave (deferrable for renumbering)
  constraint wave_question_number_unique unique (wave_id, question_number) deferrable initially immediate
);

-- ------ Answers --------------------------------------------------------------------------------------

create table public.subject (
  subject_id bigint primary key default private.random_id('subject', 'subject_id'),
  name       text   not null
    constraint subject_name_not_blank check (btrim(name) <> '')
    -- straight quotes only, as the site writes a reading's ("Respondent's Siblings", never "Respondent’s
    -- Siblings"), so a name read either way is the one name (R/service_draft.R, straight_quotes())
    constraint subject_name_straight_quotes check (name !~ '[‘’“”]')
  -- one subject for each name, ignoring case and outer spaces: subject_name_unique, section 3
);

-- When a variable's answer is about, written one way (R/service_draft.R, normal_time_frame()): "in your lifetime",
-- "in the past 12 months", "in the past 30 days". The forms print each in many ways ("during the last 12 months",
-- "over the past twelve months"); keeping one wording lets the dashboard find every past-30-day question across the
-- registries. It belongs to the variable, as one question often has a variable for each time frame, and it fills
-- [time frame] in a question stem ("on how many occasions have you smoked cigarettes [time frame]?").
create table public.time_frame (
  time_frame_id bigint primary key default private.random_id('time_frame', 'time_frame_id'),
  name          text   not null
    constraint time_frame_name_written_one_way check (btrim(name) <> '' and name = lower(btrim(name)))
  -- one time frame for each name: time_frame_name_unique, section 3
);

-- variable_key is deliberately NOT called variable_id: matr_id holds what MATR staff call the
-- "variable ID" (e.g. AH25LF).
create table public.variable (
  variable_key     bigint  primary key default private.random_id('variable', 'variable_key'),
  questionnaire_id bigint  not null,
  placement_id     bigint  not null,     -- the placement of the question it records an answer to
  matr_id          text    not null,
  display_order    integer not null
    constraint variable_display_order_check check (display_order > 0),
  label            text,
  subject_id       bigint,
  time_frame_id    bigint,               -- when its answer is about; empty when the question names no time
  -- BR93: matr_id unique within its questionnaire, may repeat across questionnaires
  constraint variable_matr_id_unique_in_questionnaire unique (questionnaire_id, matr_id),
  -- BR94: display order unique within its questionnaire question
  constraint variable_display_order_unique
    unique (placement_id, display_order) deferrable initially immediate
);

create table public.open_ended (
  variable_key     bigint primary key,                                   -- BR88, BR89
  answer_data_type text   not null                                       -- BR95
    constraint open_ended_answer_data_type_check check (answer_data_type in ('NUMBER', 'TEXT', 'DATE', 'TIME')),
  skip_condition   text,                 -- the answer that skips ("None")
  skip_instruction text,                 -- the routing printed for it
  skip_to_placement_id bigint,           -- where it sends respondents
  skip_to_end      boolean not null default false, -- or to the end of the questionnaire
  constraint open_ended_skip_to_one_place check (not (skip_to_end and skip_to_placement_id is not null))
);

-- A set is its choices (answer_option_set), one set for each combination of them (BR109:
-- option_set_choices_unique, section 4). Its name is its choices in their order, worked out when it is read
-- (public.name(option_set), section 4), so nobody types one and it can't fall out of step with them.
create table public.option_set (
  option_set_id bigint primary key default private.random_id('option_set', 'option_set_id')
);

create table public.closed_ended (
  variable_key  bigint primary key,                                      -- BR90, BR91
  option_set_id bigint not null                                          -- BR96, BR97
);

create table public.answer_option (
  answer_option_id   bigint primary key default private.random_id('answer_option', 'answer_option_id'),
  standardized_label text   not null
    constraint answer_option_label_unique unique                         -- BR110
    constraint answer_option_label_normal check (standardized_label = private.normal_answer(standardized_label))
);

-- Wordings merged into an answer choice on Make an edit: an extraction that reads one of them uses the choice
-- instead of adding it again ("1 to 2" for "1-2"). An alias isn't a choice's wording itself (the site checks).
create table public.answer_option_alias (
  alias            text   primary key
    constraint answer_option_alias_normal check (alias = private.normal_answer(alias)),
  answer_option_id bigint not null
);

create table public.answer_option_set (
  option_set_id    bigint  not null,                                     -- BR105
  answer_option_id bigint  not null,                                     -- BR107, BR108
  display_order    integer not null                                      -- the order its name lists them in
    constraint answer_option_set_display_order_check check (display_order > 0),
  primary key (option_set_id, answer_option_id),
  -- deferrable so a change can swap one choice for another in its place
  constraint answer_option_set_display_order_unique
    unique (option_set_id, display_order) deferrable initially immediate
);

create table public.response_option (
  variable_key     bigint  not null,                                     -- BR98
  answer_option_id bigint  not null,                                     -- BR100, BR101
  display_order    integer not null
    constraint response_option_display_order_check check (display_order > 0),
  code_value       text    not null,     -- text: codes may be 1, 0, -9, A ...
  skip_instruction text,
  skip_to_placement_id bigint,           -- where the skip instruction sends respondents
  skip_to_end      boolean not null default false, -- or to the end of the questionnaire
  constraint response_option_skip_to_one_place check (not (skip_to_end and skip_to_placement_id is not null)),
  primary key (variable_key, answer_option_id),
  -- BR103 (and BR104: the same answer option may carry different codes on different variables)
  constraint response_option_display_order_unique
    unique (variable_key, display_order) deferrable initially immediate,
  constraint response_option_code_value_unique
    unique (variable_key, code_value) deferrable initially immediate
);

-- How many of a wave's respondents gave each answer, typed on Edit Extraction for the extraction's wave.
create table public.response_option_count (
  wave_id          bigint  not null,
  variable_key     bigint  not null,
  answer_option_id bigint  not null,
  respondent_count integer not null
    constraint response_option_count_not_negative check (respondent_count >= 0),
  primary key (wave_id, variable_key, answer_option_id)
);

-- ------ Classifying questions ------------------------------------------------------------------------

create table public.category (
  mesh_id    text primary key                                            -- BR72
    constraint category_mesh_id_format check (mesh_id ~ '^[A-Z][0-9]{6,9}$'),
  term       text not null,
  scope_note text
);

create table public.question_category (
  component_id bigint not null,                                          -- BR73, BR74
  mesh_id      text   not null,                                          -- BR75, BR76
  primary key (component_id, mesh_id)                                    -- BR77
);

create table public.measurement_scale (
  scale_id     bigint primary key default private.random_id('measurement_scale', 'scale_id'),
  name         text   not null,                                         -- unique, ignoring case and outer spaces (BR78): section 3
  abbreviation text   not null,                                         -- the same
  version      text,
  description  text,
  citation     text
);

create table public.question_scale (
  scale_id               bigint not null,                                -- BR79
  component_id           bigint not null,                                -- BR81, BR82
  expected_option_set_id bigint,                                         -- BR83, BR84
  item_number            text   not null,  -- text: published items can be numbered 56a, 56b
  primary key (scale_id, component_id),
  constraint question_scale_item_number_unique unique (scale_id, item_number)   -- BR85
);

-- ------ Staff, drafts and the history of changes (private to the site) --------------------------------

create table public.staff (
  staff_id bigint  primary key default private.random_id('staff', 'staff_id'),
  name     text    not null,
  email    text    not null,             -- unique, ignoring case (BR22): staff_email_unique, section 3
  active   boolean not null default true,
  role     text    not null default 'STAFF'
    constraint staff_role_check check (role in ('STAFF', 'ADMIN'))
);

-- A change to the MATR data, from the first save until it is applied, and afterwards the record of it.
-- The Edit history page reads applied drafts, e.g.
--   select d.*, coalesce(e.edit_kind, 'Add') as kind          -- an extraction only adds
--   from public.draft d left join public.edit_draft e using (draft_id)
--   where d.status = 'Applied' order by d.applied_at desc;
create table public.draft (
  draft_id           bigint  primary key default private.random_id('draft', 'draft_id'),
  kind               text    not null
    constraint draft_kind_check check (kind in ('Extraction', 'Edit')),
  status             text    not null default 'Draft'
    constraint draft_status_check check (status in ('Draft', 'Awaiting Review', 'Sent Back', 'Applied', 'Withdrawn')),
  prepared_by        bigint  not null,
  saved_by           bigint  not null,
  requested_reviewer bigint,
  revision           integer not null default 1
    constraint draft_revision_check check (revision > 0),
  summary            text,
  created_at         timestamptz not null default now(),
  saved_at           timestamptz not null default now(),
  applied_at         timestamptz,
  editing_by         bigint,
  editing_at         timestamptz,
  -- the target of the subtype tables' foreign keys, so a subtype row can only join a draft of its kind
  constraint draft_id_kind_unique unique (draft_id, kind),
  constraint draft_reviewer_is_someone_else check (requested_reviewer is null or requested_reviewer <> prepared_by),
  -- an applied draft says when, and only an applied one does
  constraint draft_applied_says_when check ((status = 'Applied') = (applied_at is not null)),
  -- someone working on it says since when (claim_draft())
  constraint draft_editing_says_when check ((editing_by is null) = (editing_at is null))
);

-- A subtype holds kind as well, fixed to its one value, so its foreign key (draft_id, kind) can only
-- match a draft of that kind.
create table public.edit_draft (
  draft_id  bigint primary key,
  kind      text   not null default 'Edit'
    constraint edit_draft_kind_check check (kind = 'Edit'),
  edit_kind text   not null
    constraint edit_draft_edit_kind_check check (edit_kind in ('Add', 'Update', 'Delete')),
  form      jsonb  not null default '{}'::jsonb,
  reach     jsonb,
  items     jsonb  not null default '[]'::jsonb,
  constraint edit_draft_items_are_a_list check (jsonb_typeof(items) = 'array')
);

create table public.extracted_draft (
  draft_id           bigint primary key,
  kind               text   not null default 'Extraction'
    constraint extracted_draft_kind_check check (kind = 'Extraction'),
  method             text   not null
    constraint extracted_draft_method_check check (method in ('AI-Powered', 'Manual')),
  info               jsonb  not null default '{}'::jsonb,
  matr_csv           jsonb,
  questionnaire      jsonb,
  pdf_name           text,
  pdf_sha256         text,
  supplement         jsonb,  -- the optional file of MATR IDs and codes: {name, sha256, bytes} or {name, text}
  constraint extracted_draft_info_is_an_object check (jsonb_typeof(info) = 'object'),
  constraint extracted_draft_supplement_is_an_object check (supplement is null or jsonb_typeof(supplement) = 'object'),
  -- the target of ai_extracted_draft's foreign key, so its row can only join an AI-Powered extraction
  constraint extracted_draft_id_method_unique unique (draft_id, method),
  -- an AI-Powered extraction always has a PDF to read (and its reading: extracted_draft_has_reading)
  constraint extracted_draft_ai_has_pdf check (method <> 'AI-Powered' or pdf_sha256 is not null)
);

-- What an AI-Powered extraction has and a Manual one doesn't. Like edit_draft and extracted_draft, it
-- holds method as well, fixed to its one value, so its foreign key (draft_id, method) can only match an
-- AI-Powered extraction.
create table public.ai_extracted_draft (
  draft_id           bigint primary key,
  method             text   not null default 'AI-Powered'
    constraint ai_extracted_draft_method_check check (method = 'AI-Powered'),
  ai_original        jsonb,
  review_edits       jsonb  not null default '[]'::jsonb,
  reading_status     text   not null
    constraint ai_extracted_draft_reading_status_check
    check (reading_status in ('queued', 'sent', 'received', 'assigning', 'ready', 'failed')),
  reading_started    timestamptz,
  reading_claimed_at timestamptz,
  reading_pid        integer,
  reading_error      text,
  model              text,
  prompt_version     text,
  constraint ai_extracted_draft_edits_are_a_list check (jsonb_typeof(review_edits) = 'array')
);

-- A PDF uploaded for an extraction, once per file, by its SHA-256. Its pages and size are facts about
-- the file, not the draft (the same file uploaded twice is one file; the name it was uploaded under is
-- the draft's: extracted_draft.pdf_name).
create table public.pdf_file (
  pdf_sha256 text    primary key,
  pdf_pages  integer
    constraint pdf_file_pdf_pages_check check (pdf_pages > 0),
  pdf_bytes  bigint
    constraint pdf_file_pdf_bytes_check check (pdf_bytes >= 0)
);

-- The PDF and the file of Variable Names and Coding a wave's extraction was read from, written when the
-- extraction is applied (private.apply_draft_items()): a PDF by its pdf_file row, a CSV or text file by its
-- text. A table of its own, not columns of wave, because the public dashboard reads all of wave and this is
-- the staff's record, like the drafts.
create table public.wave_file (
  wave_id           bigint primary key,
  pdf_sha256        text,
  pdf_name          text,
  supplement_name   text,
  supplement_sha256 text,
  supplement_text   text,
  constraint wave_file_supplement_is_one_file check (supplement_sha256 is null or supplement_text is null)
);

-- Each time a draft is submitted for review, sent back, or approved: who, when, and the
-- reviewer's note for a send back. draft itself says only where it is now (status, requested_reviewer).
create table public.draft_review_event (
  draft_review_event_id bigint      primary key default private.random_id('draft_review_event', 'draft_review_event_id'),
  draft_id              bigint      not null,
  event                 text        not null
    constraint draft_review_event_event_check check (event in ('Submitted', 'Sent Back', 'Approved')),
  staff_id              bigint      not null,
  note                  text,
  occurred_at           timestamptz not null default now(),
  -- only a send back has a note: what the reviewer asks to be changed
  constraint draft_review_event_note_is_a_send_back check (note is null or event = 'Sent Back')
);
-- One field an applied draft changed. Written only by private.apply_draft_items(), with the real
-- numbers of rows the draft added. record_identifier is text because the row it names may sit in any
-- table, including tables with composite keys (BR37 is checked by apply_draft_items()).
create table public.applied_item (
  draft_id          bigint  not null,                                    -- BR33, BR34
  item_number       integer not null
    constraint applied_item_item_number_check check (item_number > 0),
  entity_name       text    not null,
  record_identifier text    not null,
  action            text    not null
    constraint applied_item_action_check check (action in ('Insert', 'Update', 'Delete')),
  attribute_name    text,
  old_value         text,
  new_value         text,
  primary key (draft_id, item_number)
);


-- =====================================================================================================
-- 2. ADD REFERENCES
-- =====================================================================================================
-- None cascade unless noted: deleting a row that others point at is refused.
--
-- What a questionnaire or a wave owns goes with it (on delete cascade): a questionnaire's waves and its
-- placements of components (what holds a placement takes it along), and through those its questions' settings,
-- wave numbers and variables (with their
-- choices and counts); a wave's record of the questions it asked. What is shared stays: a component, an answer
-- choice, an option set, a subject, a scale. Deleting a shared row that something still uses is refused. What a
-- delete leaves owned by nothing (a component no questionnaire prints, a question no wave asks, a stem no
-- question has) goes when the change ends, unless something still names it (section 4, "What nothing owns").
-- Above a questionnaire nothing cascades: a study or a registry that still holds anything can't be deleted.
-- R/service_edit_relations.R lists the same, for what the site shows before a delete.

alter table public.study
  add constraint study_registry_id_fkey
    foreign key (registry_id) references public.registry (registry_id),
  add constraint study_principal_investigator_id_fkey
    foreign key (principal_investigator_id) references public.principal_investigator (principal_investigator_id),
  add constraint study_contact_principal_investigator_id_fkey
    foreign key (contact_principal_investigator_id) references public.principal_investigator (principal_investigator_id);

alter table public.questionnaire
  add constraint questionnaire_study_id_fkey
    foreign key (study_id) references public.study (study_id),
  add constraint questionnaire_respondent_type_id_fkey
    foreign key (respondent_type_id) references public.respondent_type (respondent_type_id);

alter table public.wave
  add constraint wave_questionnaire_id_fkey
    foreign key (questionnaire_id) references public.questionnaire (questionnaire_id) on delete cascade;

alter table public.component
  add constraint component_component_type_id_fkey
    foreign key (component_type_id) references public.component_type (component_type_id);

alter table public.question
  add constraint question_component_id_fkey                               -- goes with its component (M2)
    foreign key (component_id) references public.component (component_id) on delete cascade;

alter table public.questionnaire_component
  add constraint questionnaire_component_questionnaire_id_fkey
    foreign key (questionnaire_id) references public.questionnaire (questionnaire_id) on delete cascade,
  add constraint questionnaire_component_component_id_fkey
    foreign key (component_id) references public.component (component_id),
  -- BR50-BR52: the parent is placed in the SAME questionnaire, because questionnaire_id is half of the key; what
  -- is inside a section or a group goes with it
  add constraint questionnaire_component_parent_fk
    foreign key (questionnaire_id, parent_placement_id)
    references public.questionnaire_component (questionnaire_id, placement_id) on delete cascade,
  -- a group skips to a place in its own questionnaire. A place a skip names can't be deleted while it does (Make an
  -- edit empties the skip first, keeping its printed words); a whole questionnaire goes with its skips, which is
  -- why this is checked when the change ends, once the cascade has taken them too.
  add constraint questionnaire_component_skip_to_fk
    foreign key (questionnaire_id, skip_to_placement_id)
    references public.questionnaire_component (questionnaire_id, placement_id) deferrable initially deferred;

alter table public.questionnaire_question
  add constraint questionnaire_question_component_id_fkey                -- BR58, BR59
    foreign key (component_id) references public.question (component_id),
  -- BR56, BR57: a questionnaire question IS a placement (of that question, in that questionnaire); its settings
  -- go with it, and follow it to new words (a placement pointed at another question)
  add constraint questionnaire_question_placement_fk
    foreign key (placement_id, questionnaire_id, component_id)
    references public.questionnaire_component (placement_id, questionnaire_id, component_id)
    on update cascade on delete cascade,
  -- BR60-BR62: the question it follows is placed in the SAME questionnaire
  add constraint questionnaire_question_parent_fk
    foreign key (questionnaire_id, parent_question_placement_id)
    references public.questionnaire_question (questionnaire_id, placement_id);

alter table public.wave_question
  -- BR66, BR67, BR70: the wave and the question share one questionnaire_id
  add constraint wave_question_wave_fk
    foreign key (questionnaire_id, wave_id) references public.wave (questionnaire_id, wave_id) on delete cascade,
  add constraint wave_question_question_fk                               -- BR68, BR70
    foreign key (questionnaire_id, placement_id)
    references public.questionnaire_question (questionnaire_id, placement_id) on delete cascade;

alter table public.variable
  -- BR86: a variable records an answer for exactly one questionnaire question (one placement of it)
  add constraint variable_questionnaire_question_fk
    foreign key (questionnaire_id, placement_id)
    references public.questionnaire_question (questionnaire_id, placement_id) on delete cascade,
  add constraint variable_subject_fk
    foreign key (subject_id) references public.subject (subject_id),
  add constraint variable_time_frame_fk
    foreign key (time_frame_id) references public.time_frame (time_frame_id);

alter table public.open_ended
  add constraint open_ended_variable_key_fkey
    foreign key (variable_key) references public.variable (variable_key) on delete cascade,
  add constraint open_ended_skip_to_placement_id_fkey                    -- as a group's skip, above
    foreign key (skip_to_placement_id) references public.questionnaire_component (placement_id) deferrable initially deferred;

alter table public.closed_ended
  add constraint closed_ended_variable_key_fkey
    foreign key (variable_key) references public.variable (variable_key) on delete cascade,
  add constraint closed_ended_option_set_id_fkey
    foreign key (option_set_id) references public.option_set (option_set_id);

alter table public.question
  add constraint question_question_stem_id_fkey
    foreign key (question_stem_id) references public.question_stem (question_stem_id) on delete set null;

-- A choice's merged wordings go with it.
alter table public.answer_option_alias
  add constraint answer_option_alias_answer_option_id_fkey
    foreign key (answer_option_id) references public.answer_option (answer_option_id) on delete cascade;

alter table public.answer_option_set
  add constraint answer_option_set_option_set_id_fkey
    foreign key (option_set_id) references public.option_set (option_set_id),
  add constraint answer_option_set_answer_option_id_fkey
    foreign key (answer_option_id) references public.answer_option (answer_option_id);

alter table public.response_option
  add constraint response_option_variable_key_fkey
    foreign key (variable_key) references public.closed_ended (variable_key) on delete cascade,
  add constraint response_option_answer_option_id_fkey
    foreign key (answer_option_id) references public.answer_option (answer_option_id),
  add constraint response_option_skip_to_placement_id_fkey               -- as a group's skip, above
    foreign key (skip_to_placement_id) references public.questionnaire_component (placement_id) deferrable initially deferred;

alter table public.response_option_count
  add constraint response_option_count_wave_id_fkey
    foreign key (wave_id) references public.wave (wave_id) on delete cascade,
  add constraint response_option_count_response_option_fkey
    foreign key (variable_key, answer_option_id) references public.response_option (variable_key, answer_option_id) on delete cascade;

-- A question's MeSH tags go with it.
alter table public.question_category
  add constraint question_category_component_id_fkey
    foreign key (component_id) references public.question (component_id) on delete cascade,
  add constraint question_category_mesh_id_fkey
    foreign key (mesh_id) references public.category (mesh_id);

alter table public.question_scale
  add constraint question_scale_scale_id_fkey
    foreign key (scale_id) references public.measurement_scale (scale_id),
  add constraint question_scale_component_id_fkey
    foreign key (component_id) references public.question (component_id),
  add constraint question_scale_expected_option_set_id_fkey
    foreign key (expected_option_set_id) references public.option_set (option_set_id);

alter table public.draft
  add constraint draft_prepared_by_fkey        foreign key (prepared_by)        references public.staff (staff_id),
  add constraint draft_saved_by_fkey           foreign key (saved_by)           references public.staff (staff_id),
  add constraint draft_requested_reviewer_fkey foreign key (requested_reviewer) references public.staff (staff_id),
  add constraint draft_editing_by_fkey         foreign key (editing_by)         references public.staff (staff_id);

alter table public.edit_draft
  add constraint edit_draft_draft_fkey
    foreign key (draft_id, kind) references public.draft (draft_id, kind) on delete cascade;

alter table public.extracted_draft
  add constraint extracted_draft_draft_fkey
    foreign key (draft_id, kind) references public.draft (draft_id, kind) on delete cascade,
  add constraint extracted_draft_pdf_sha256_fkey
    foreign key (pdf_sha256) references public.pdf_file (pdf_sha256);

-- A wave's files go with the wave.
alter table public.wave_file
  add constraint wave_file_wave_id_fkey
    foreign key (wave_id) references public.wave (wave_id) on delete cascade,
  add constraint wave_file_pdf_sha256_fkey
    foreign key (pdf_sha256) references public.pdf_file (pdf_sha256),
  add constraint wave_file_supplement_sha256_fkey
    foreign key (supplement_sha256) references public.pdf_file (pdf_sha256);

alter table public.ai_extracted_draft
  add constraint ai_extracted_draft_extraction_fkey
    foreign key (draft_id, method) references public.extracted_draft (draft_id, method) on delete cascade;

-- The events go with a draft that is deleted (one that isn't applied: draft_guard keeps those).
alter table public.draft_review_event
  add constraint draft_review_event_draft_id_fkey
    foreign key (draft_id) references public.draft (draft_id) on delete cascade,
  add constraint draft_review_event_staff_id_fkey
    foreign key (staff_id) references public.staff (staff_id);

-- No cascade: an applied draft can't be deleted (draft_guard), so its items stay with it.
alter table public.applied_item
  add constraint applied_item_draft_id_fkey
    foreign key (draft_id) references public.draft (draft_id);


-- =====================================================================================================
-- 3. INDEXES
-- =====================================================================================================
-- Primary keys and unique constraints already make an index, and an index also serves any search on
-- its first columns. So a foreign key only needs one here when no key starts with its columns.

-- Uniqueness that ignores case or spaces (an index, as a unique constraint can't hold an expression, so an
-- insert's ON CONFLICT names the expression as its target, not a constraint)
create unique index staff_email_unique  on public.staff   (lower(email));          -- BR22
create unique index subject_name_unique on public.subject (lower(btrim(name)));
create unique index time_frame_name_unique on public.time_frame (name);
create unique index question_stem_text_unique on public.question_stem (btrim(regexp_replace(text, '[[:space:]]+', ' ', 'g')));
-- one component for each kind and words (but for runs of spaces), and one Group (the one kind with no words)
create unique index component_words_unique on public.component (component_type_id, btrim(regexp_replace(text, '[[:space:]]+', ' ', 'g')))
  where text is not null;
create unique index component_one_group on public.component (component_type_id) where text is null;
create unique index measurement_scale_name_unique          on public.measurement_scale (lower(btrim(name)));       -- BR78
create unique index measurement_scale_abbreviation_unique  on public.measurement_scale (lower(btrim(abbreviation)));
create unique index registry_name_unique                   on public.registry (lower(btrim(name)));
create unique index study_name_unique_in_registry          on public.study (registry_id, lower(btrim(name)));
create unique index questionnaire_name_unique_in_study     on public.questionnaire (study_id, lower(btrim(name)));
create unique index questionnaire_abbreviation_unique_in_study on public.questionnaire (study_id, lower(btrim(abbreviation)));

-- Foreign keys
create index study_registry_id_idx                 on public.study (registry_id);
create index study_principal_investigator_id_idx   on public.study (principal_investigator_id);
create index study_contact_pi_id_idx               on public.study (contact_principal_investigator_id);
create index questionnaire_study_id_idx            on public.questionnaire (study_id);
create index questionnaire_respondent_type_idx     on public.questionnaire (respondent_type_id);
create index component_component_type_id_idx       on public.component (component_type_id);
create index questionnaire_component_component_idx on public.questionnaire_component (component_id);
create index questionnaire_component_skip_to_idx  on public.questionnaire_component (skip_to_placement_id);
create index questionnaire_question_component_idx  on public.questionnaire_question (component_id);
create index questionnaire_question_parent_idx     on public.questionnaire_question (questionnaire_id, parent_question_placement_id);
create index wave_question_question_idx            on public.wave_question (questionnaire_id, placement_id);
create index variable_placement_idx                on public.variable (questionnaire_id, placement_id);
create index variable_subject_id_idx               on public.variable (subject_id);
create index variable_time_frame_id_idx            on public.variable (time_frame_id);
create index closed_ended_option_set_id_idx        on public.closed_ended (option_set_id);
create index answer_option_set_answer_option_idx   on public.answer_option_set (answer_option_id);
create index answer_option_alias_answer_option_idx on public.answer_option_alias (answer_option_id);
create index question_question_stem_idx            on public.question (question_stem_id);
create index response_option_answer_option_idx     on public.response_option (answer_option_id);
create index response_option_skip_to_idx           on public.response_option (skip_to_placement_id);
create index response_option_count_choice_idx      on public.response_option_count (variable_key, answer_option_id);
create index open_ended_skip_to_idx                on public.open_ended (skip_to_placement_id);
create index question_category_mesh_id_idx         on public.question_category (mesh_id);
create index question_scale_component_id_idx       on public.question_scale (component_id);
create index question_scale_expected_set_idx       on public.question_scale (expected_option_set_id);
create index draft_prepared_by_idx                 on public.draft (prepared_by);
create index draft_saved_by_idx                    on public.draft (saved_by);
create index draft_requested_reviewer_idx          on public.draft (requested_reviewer);
create index draft_editing_by_idx                  on public.draft (editing_by);
create index extracted_draft_pdf_sha256_idx        on public.extracted_draft (pdf_sha256);
create index wave_file_pdf_sha256_idx              on public.wave_file (pdf_sha256);
create index wave_file_supplement_sha256_idx       on public.wave_file (supplement_sha256);
create index draft_review_event_staff_idx          on public.draft_review_event (staff_id);

-- Lookups: the site lists drafts by kind and status
create index draft_kind_status_idx on public.draft (kind, status);
-- and a draft's reviews in order
create index draft_review_event_draft_idx on public.draft_review_event (draft_id, occurred_at);

-- BR26: one approval, at most, for each draft
create unique index draft_review_event_one_approval on public.draft_review_event (draft_id) where event = 'Approved';


-- =====================================================================================================
-- 4. TRIGGERS AND FUNCTIONS
-- =====================================================================================================

-- ------ 4a. Rules a constraint can't express ---------------------------------------------------------

-- BR20: a wave cannot be moved to another questionnaire.
create function public.wave_questionnaire_immutable()
returns trigger
language plpgsql
set search_path = ''
as $$
begin
  if new.questionnaire_id is distinct from old.questionnaire_id then
    raise exception 'A wave cannot be moved to another questionnaire (BR20)'
      using errcode = 'check_violation';
  end if;
  return new;
end;
$$;

create trigger wave_questionnaire_immutable
  before update of questionnaire_id on public.wave
  for each row execute function public.wave_questionnaire_immutable();

-- BR54: a placement is not its own ancestor.
create function public.questionnaire_component_no_cycle()
returns trigger
language plpgsql
set search_path = ''
as $$
begin
  if new.parent_placement_id is null then
    return new;
  end if;
  if exists (
    with recursive ancestors (placement_id) as (
      select new.parent_placement_id
      union
      select qc.parent_placement_id
      from public.questionnaire_component qc
      join ancestors a on qc.placement_id = a.placement_id
      where qc.parent_placement_id is not null
    )
    select 1 from ancestors where placement_id = new.placement_id
  ) then
    raise exception 'Placement % would become its own ancestor in questionnaire % (BR54)',
      new.placement_id, new.questionnaire_id
      using errcode = 'check_violation';
  end if;
  return new;
end;
$$;

create trigger questionnaire_component_no_cycle
  before insert or update of parent_placement_id on public.questionnaire_component
  for each row execute function public.questionnaire_component_no_cycle();

-- BR63: a placed question does not follow itself through the questions it follows.
create function public.questionnaire_question_no_cycle()
returns trigger
language plpgsql
set search_path = ''
as $$
begin
  if new.parent_question_placement_id is null then
    return new;
  end if;
  if exists (
    with recursive ancestors (placement_id) as (
      select new.parent_question_placement_id
      union
      select qq.parent_question_placement_id
      from public.questionnaire_question qq
      join ancestors a on qq.placement_id = a.placement_id
      where qq.parent_question_placement_id is not null
    )
    select 1 from ancestors where placement_id = new.placement_id
  ) then
    raise exception 'Question placement % would become its own ancestor in questionnaire % (BR63)',
      new.placement_id, new.questionnaire_id
      using errcode = 'check_violation';
  end if;
  return new;
end;
$$;

create trigger questionnaire_question_no_cycle
  before insert or update of parent_question_placement_id on public.questionnaire_question
  for each row execute function public.questionnaire_question_no_cycle();

-- BR93: matr_id does not change once recorded.
create function public.variable_matr_id_immutable()
returns trigger
language plpgsql
set search_path = ''
as $$
begin
  if new.matr_id is distinct from old.matr_id then
    raise exception 'The MATR ID of variable % cannot change (BR93)', old.variable_key
      using errcode = 'check_violation';
  end if;
  return new;
end;
$$;

create trigger variable_matr_id_immutable
  before update of matr_id on public.variable
  for each row execute function public.variable_matr_id_immutable();

-- BR92: every variable is exactly one of OPEN_ENDED or CLOSED_ENDED. Checked at COMMIT, so the
-- variable and its subtype row are added in one transaction.
create function public.variable_answer_format_check()
returns trigger
language plpgsql
set search_path = ''
as $$
declare
  k bigint;
  n integer;
begin
  if tg_op = 'DELETE' then
    k := old.variable_key;
  else
    k := new.variable_key;
  end if;
  -- nothing to check if the variable itself is gone
  if not exists (select 1 from public.variable where variable_key = k) then
    return null;
  end if;
  select (select count(*) from public.open_ended   where variable_key = k)
       + (select count(*) from public.closed_ended where variable_key = k)
    into n;
  if n <> 1 then
    raise exception 'Variable % must be exactly one of OPEN_ENDED or CLOSED_ENDED; found % (BR92)', k, n
      using errcode = 'check_violation';
  end if;
  return null;
end;
$$;

create constraint trigger variable_answer_format_check
  after insert on public.variable
  deferrable initially deferred
  for each row execute function public.variable_answer_format_check();

create constraint trigger open_ended_answer_format_check
  after insert or update or delete on public.open_ended
  deferrable initially deferred
  for each row execute function public.variable_answer_format_check();

create constraint trigger closed_ended_answer_format_check
  after insert or update or delete on public.closed_ended
  deferrable initially deferred
  for each row execute function public.variable_answer_format_check();

-- What can't happen to a draft, whoever asks. The site checks first and says it kindly; this keeps a
-- mistake in the site, or a person in the SQL editor, from breaking the rule. An applied draft is the
-- record of a change made to the MATR data, so it stays as it was applied: never changed or deleted.
create function public.draft_guard()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if tg_op = 'DELETE' then
    if old.status = 'Applied' then
      raise exception 'That change has been applied and is the record of it, so it can''t be deleted.';
    end if;
    return old;
  end if;
  if old.status = 'Applied' then
    raise exception 'That change has already been applied, so it can''t be changed.';
  end if;
  if old.status = 'Withdrawn' and new.status <> 'Withdrawn' then
    raise exception 'That was withdrawn, so it can''t be changed.';
  end if;
  if new.kind <> old.kind or new.prepared_by <> old.prepared_by or new.created_at <> old.created_at then
    raise exception 'Who prepared a draft, when, and what kind it is can''t be changed.';
  end if;
  if new.status = 'Draft' and old.status <> 'Draft' then
    raise exception 'A draft that was sent for review can''t go back to being a plain draft.';
  end if;
  if new.status = 'Sent Back' and old.status not in ('Awaiting Review', 'Sent Back') then
    raise exception 'Only a change waiting for review can be sent back.';
  end if;
  return new;
end;
$$;

create trigger draft_guard
  before update or delete on public.draft
  for each row execute function public.draft_guard();

-- The details of an applied draft stay as they were applied: an extraction's too, with what the AI read
-- (ai_original) and how staff changed it (review_edits), kept for analysis.
create function public.draft_detail_guard()
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

create trigger edit_draft_guard
  before update or delete on public.edit_draft
  for each row execute function public.draft_detail_guard();

create trigger extracted_draft_guard
  before update or delete on public.extracted_draft
  for each row execute function public.draft_detail_guard();

create trigger ai_extracted_draft_guard
  before update or delete on public.ai_extracted_draft
  for each row execute function public.draft_detail_guard();

-- What the AI returned is written by the reading job while it reads, before anyone saves the draft, and
-- never changed after: what the reviewer changed is measured against it.
create function public.ai_original_guard()
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

create trigger ai_extracted_draft_original_kept
  before update of ai_original on public.ai_extracted_draft
  for each row execute function public.ai_original_guard();

-- Every draft has its details. Checked when the transaction ends, so create_draft() can add the
-- draft and then its details.
create function public.draft_has_details()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  -- a draft made and deleted in the same transaction needs nothing
  if not exists (select 1 from public.draft where draft_id = new.draft_id) then
    return null;
  end if;
  if new.kind = 'Extraction' and not exists (select 1 from public.extracted_draft where draft_id = new.draft_id) then
    raise exception 'An extraction needs its details (extracted_draft).';
  elsif new.kind = 'Edit' and not exists (select 1 from public.edit_draft where draft_id = new.draft_id) then
    raise exception 'An edit needs its details (edit_draft).';
  end if;
  return null;
end;
$$;

create constraint trigger draft_has_details
  after insert on public.draft
  deferrable initially deferred
  for each row execute function public.draft_has_details();

-- Every AI-Powered extraction has its reading (ai_extracted_draft). Checked when the transaction ends,
-- so create_draft() can add the extraction and then its reading. Run when an extraction is added or its
-- method changes, and when a reading is deleted.
create function public.extracted_draft_has_reading()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_id bigint := case when tg_op = 'DELETE' then old.draft_id else new.draft_id end;
begin
  if exists (select 1 from public.extracted_draft x where x.draft_id = v_id and x.method = 'AI-Powered')
     and not exists (select 1 from public.ai_extracted_draft a where a.draft_id = v_id) then
    raise exception 'An AI-Powered extraction needs its reading (ai_extracted_draft).';
  end if;
  return null;
end;
$$;

create constraint trigger extracted_draft_has_reading
  after insert or update of method on public.extracted_draft
  deferrable initially deferred
  for each row execute function public.extracted_draft_has_reading();

create constraint trigger ai_extracted_draft_kept
  after delete on public.ai_extracted_draft
  deferrable initially deferred
  for each row execute function public.extracted_draft_has_reading();

-- A PDF's pages and size are facts about the file: once known, they don't change.
create function public.pdf_file_guard()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if new.pdf_sha256 <> old.pdf_sha256
     or (old.pdf_pages is not null and new.pdf_pages is distinct from old.pdf_pages)
     or (old.pdf_bytes is not null and new.pdf_bytes is distinct from old.pdf_bytes) then
    raise exception 'A PDF''s hash, pages and size are facts about the file, so they can''t be changed.';
  end if;
  return new;
end;
$$;

create trigger pdf_file_guard
  before update on public.pdf_file
  for each row execute function public.pdf_file_guard();

-- A draft's reviews are the record of what happened to it: added, never changed, and deleted only with
-- the draft (one that isn't applied). An applied draft gets no more. An approval is from someone other
-- than the person who prepared the change and the person who made its last revision (BR27).
create function public.draft_review_event_guard()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d public.draft%rowtype;
begin
  if tg_op = 'UPDATE' then
    raise exception 'A review of a change is the record of what happened, so it can''t be changed.';
  end if;
  if tg_op = 'DELETE' then
    -- a cascade from deleting the draft: the draft is gone by now
    if exists (select 1 from public.draft where draft_id = old.draft_id) then
      raise exception 'A review of a change is the record of what happened, so it goes only with its draft.';
    end if;
    return old;
  end if;
  select * into d from public.draft where draft_id = new.draft_id;
  if not found then
    return new; -- the foreign key says so
  end if;
  if d.status = 'Applied' then
    raise exception 'That change has already been applied, so it can''t be changed.';
  end if;
  if new.event = 'Approved' and (new.staff_id = d.prepared_by or new.staff_id = d.saved_by) then
    raise exception 'An approval is from someone other than the person who prepared the change and the person who made its last revision.';
  end if;
  return new;
end;
$$;

create trigger draft_review_event_guard
  before insert or update or delete on public.draft_review_event
  for each row execute function public.draft_review_event_guard();

-- An approval belongs to an applied draft: approve_draft() records it and applies the change together.
-- Checked when the transaction ends, since the draft is marked Applied after its approval is recorded.
create function public.draft_approval_applies()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if new.event = 'Approved'
     and exists (select 1 from public.draft where draft_id = new.draft_id and status <> 'Applied') then
    raise exception 'An approval goes with applying the change: approve_draft() does both.';
  end if;
  return null;
end;
$$;

create constraint trigger draft_approval_applies
  after insert on public.draft_review_event
  deferrable initially deferred
  for each row execute function public.draft_approval_applies();

-- A draft that is sent back says who sent it back: it has a Sent Back event. Checked when the
-- transaction ends, so send_back_draft() can record the event and change the status in either order.
create function public.draft_sent_back_says_who()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if new.status = 'Sent Back'
     and exists (select 1 from public.draft where draft_id = new.draft_id and status = 'Sent Back')
     and not exists (select 1 from public.draft_review_event e
                     where e.draft_id = new.draft_id and e.event = 'Sent Back') then
    raise exception 'A change that is sent back says who sent it back: send_back_draft() records it.';
  end if;
  return null;
end;
$$;

create constraint trigger draft_sent_back_says_who
  after insert or update of status on public.draft
  deferrable initially deferred
  for each row execute function public.draft_sent_back_says_who();

-- ------ 4b. Internal helpers (schema private: the REST API can't call them) ---------------------------

-- Sets one detail row of a draft from JSON holding only the columns to set: a column not mentioned keeps
-- its default (p_new) or its value. A JSON null sets the column to null.
create function private.draft_set_row(p_table text, p_draft_id bigint, p_values jsonb, p_new boolean)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  given   jsonb := coalesce(p_values, '{}'::jsonb) - 'draft_id' - 'kind';
  names   text[];
  unknown text;
begin
  if p_table not in ('edit_draft', 'extracted_draft', 'ai_extracted_draft') then
    raise exception 'A draft''s details are in edit_draft, extracted_draft or ai_extracted_draft, not %.', p_table;
  end if;
  -- ai_extracted_draft's method is fixed, as kind is
  if p_table = 'ai_extracted_draft' then
    given := given - 'method';
  end if;
  select array_agg(k order by k) into names from jsonb_object_keys(given) as k;
  if names is null then
    if p_new then
      execute format('insert into public.%I (draft_id) values ($1)', p_table) using p_draft_id;
    end if;
    return;
  end if;
  select k into unknown from unnest(names) as k
  where not exists (
    select 1 from pg_attribute a
    where a.attrelid = format('public.%I', p_table)::regclass and a.attname = k and a.attnum > 0 and not a.attisdropped
  ) limit 1;
  if unknown is not null then
    raise exception '% has no column %.', p_table, unknown;
  end if;
  if p_new then
    execute format(
      'insert into public.%1$I (draft_id, %2$s) select $1, %3$s from jsonb_populate_record(null::public.%1$I, $2) r',
      p_table,
      (select string_agg(format('%I', k), ', ') from unnest(names) k),
      (select string_agg(format('r.%I', k), ', ') from unnest(names) k)
    ) using p_draft_id, given;
  else
    execute format(
      'update public.%1$I t set %2$s from jsonb_populate_record(null::public.%1$I, $2) r where t.draft_id = $1',
      p_table,
      (select string_agg(format('%1$I = r.%1$I', k), ', ') from unnest(names) k)
    ) using p_draft_id, given;
  end if;
end;
$$;

-- Sets a draft's details from JSON holding only the columns to set (see draft_set_row()). An
-- extraction's details are in three tables, and the JSON names columns of any of them:
--   pdf_pages, pdf_bytes        pdf_file, for the PDF pdf_sha256 names (a known file keeps what it has;
--                               what it didn't know yet is filled in)
--   the reading's columns       ai_extracted_draft (an AI-Powered extraction only)
--   the rest                    extracted_draft
create function private.draft_set_details(p_table text, p_draft_id bigint, p_detail jsonb, p_new boolean)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  given    jsonb := coalesce(p_detail, '{}'::jsonb) - 'draft_id' - 'kind';
  reading  jsonb;
  v_method text;
begin
  if p_table not in ('edit_draft', 'extracted_draft') then
    raise exception 'A draft''s details are in edit_draft or extracted_draft, not %.', p_table;
  end if;
  if p_table = 'edit_draft' then
    perform private.draft_set_row('edit_draft', p_draft_id, given, p_new);
    return;
  end if;

  -- the PDF first, so extracted_draft can point at it
  if given ->> 'pdf_sha256' is not null then
    insert into public.pdf_file as f (pdf_sha256, pdf_pages, pdf_bytes)
    values (given ->> 'pdf_sha256', (given ->> 'pdf_pages')::integer, (given ->> 'pdf_bytes')::bigint)
    on conflict (pdf_sha256) do update
      set pdf_pages = coalesce(f.pdf_pages, excluded.pdf_pages),
          pdf_bytes = coalesce(f.pdf_bytes, excluded.pdf_bytes);
  end if;
  -- a PDF of Variable Names and Coding (supplement) is kept the same way
  if jsonb_typeof(given -> 'supplement') = 'object' and given -> 'supplement' ->> 'sha256' is not null then
    insert into public.pdf_file as f (pdf_sha256, pdf_bytes)
    values (given -> 'supplement' ->> 'sha256', (given -> 'supplement' ->> 'bytes')::bigint)
    on conflict (pdf_sha256) do update
      set pdf_bytes = coalesce(f.pdf_bytes, excluded.pdf_bytes);
  end if;
  given := given - 'pdf_pages' - 'pdf_bytes';

  select coalesce(jsonb_object_agg(e.key, e.value), '{}'::jsonb) into reading
  from jsonb_each(given) as e
  where e.key in (
    select a.attname from pg_attribute a
    where a.attrelid = 'public.ai_extracted_draft'::regclass and a.attnum > 0 and not a.attisdropped
      and a.attname not in ('draft_id', 'method')
  );
  given := given - array(select jsonb_object_keys(reading));

  perform private.draft_set_row('extracted_draft', p_draft_id, given, p_new);
  select x.method into v_method from public.extracted_draft x where x.draft_id = p_draft_id;
  if v_method = 'AI-Powered' then
    if p_new or reading <> '{}'::jsonb then
      perform private.draft_set_row('ai_extracted_draft', p_draft_id, reading, p_new);
    end if;
  elsif exists (select 1 from jsonb_each(reading) as e where e.value <> 'null'::jsonb) then
    raise exception 'A Manual extraction has no reading and nothing from an AI.';
  end if;
end;
$$;

-- Sets the columns of draft a caller may set. The rest (who prepared it, its revision, who saved it)
-- create_draft() and save_draft() keep themselves; when it was submitted, sent back or approved is a
-- draft_review_event.
create function private.draft_set_parent(p_draft_id bigint, p_parent jsonb)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  allowed constant text[] := array['status', 'summary', 'requested_reviewer'];
  given   jsonb := coalesce(p_parent, '{}'::jsonb);
  names   text[];
  unknown text;
begin
  select array_agg(k order by k) into names from jsonb_object_keys(given) as k;
  if names is null then
    return;
  end if;
  select k into unknown from unnest(names) as k where k <> all (allowed) limit 1;
  if unknown is not null then
    raise exception 'A draft''s % can''t be set this way.', unknown;
  end if;
  -- Applied is only ever set by applying (apply_draft() and approve_draft())
  if given ->> 'status' = 'Applied' then
    raise exception 'A change is applied by applying it, not by saying it was.';
  end if;
  -- and Sent Back by sending it back, which says who and why
  if given ->> 'status' = 'Sent Back' then
    raise exception 'A change request is sent back with send_back_draft(), which records who sent it back and why.';
  end if;
  execute format(
    'update public.draft t set %s from jsonb_populate_record(null::public.draft, $2) r where t.draft_id = $1',
    (select string_agg(format('%1$I = r.%1$I', k), ', ') from unnest(names) k)
  ) using p_draft_id, given;
end;
$$;

-- A draft that has just gone to Awaiting Review (from p_was: Draft, Sent Back, or nothing for a new one)
-- was submitted, by p_staff_id.
create function private.draft_record_submission(p_draft_id bigint, p_was text, p_staff_id bigint)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d public.draft%rowtype;
begin
  select * into d from public.draft where draft_id = p_draft_id;
  if d.status = 'Awaiting Review' and p_was is distinct from 'Awaiting Review' then
    insert into public.draft_review_event (draft_id, event, staff_id)
    values (d.draft_id, 'Submitted', p_staff_id);
  end if;
end;
$$;

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
-- line for each table it added to (and one for the questions it reused and linked, below); an edit that deletes, one line for the row staff picked to delete (named in
-- edit_draft.reach as "deleted"), and nothing of what it or the rows that went with it held. The delete's own list
-- of items is emptied too, so what was deleted is gone for good.
--
-- Constraints that can be deferred are deferred to the end, so rows can be put in order (display
-- orders) and the answer-format check (BR92) looks at the finished change.
--
-- An edit can change the tables Make an edit changes (R/service_edit_entities_*.R, and what their changes
-- reach). An extraction only adds rows (Insert), to the tables a questionnaire is made of, with one exception: a
-- question it reuses (one another questionnaire prints with the same words) that has no stem yet gets the stem the
-- extraction links it with (an Update of question.question_stem_id from empty), so it stays linked.
create function private.apply_draft_items(
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
    'wave_question', 'category', 'question_category', 'answer_option', 'subject', 'time_frame', 'option_set', 'answer_option_set',
    'measurement_scale', 'question_scale', 'variable', 'open_ended', 'closed_ended', 'response_option', 'response_option_count',
    'question_stem', 'answer_option_alias'
  ];
  loadable constant text[] := array[
    'questionnaire', 'wave', 'component', 'questionnaire_component', 'question', 'questionnaire_question',
    'wave_question', 'variable', 'open_ended', 'closed_ended', 'option_set', 'answer_option',
    'answer_option_set', 'response_option', 'response_option_count', 'subject', 'time_frame', 'question_stem'
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
  added_ids    text[] := array[]::text[]; -- ... and their keys ...
  added_acts   text[] := array[]::text[]; -- ... and whether they were added or (a reused question's stem) updated
  deleting     boolean := false;          -- an edit that deletes: recorded as the one row picked ...
  picked       jsonb;                     -- ... {"table", "row"}, from edit_draft.reach ...
  last_table   text;                      -- ... or, if the draft doesn't say, the last row deleted
  last_id      text;
begin
  if jsonb_typeof(p_items) is distinct from 'array' or jsonb_array_length(p_items) = 0 then
    raise exception 'There is nothing in this to apply.';
  end if;

  -- For the rest of the transaction, not just this function: SET CONSTRAINTS can't be undone at the end of a
  -- function. The site calls apply_draft() and approve_draft() through the REST API, each in a transaction of its
  -- own, so it ends with them. Anything that calls this inside a longer transaction runs what follows with every
  -- deferrable constraint deferred, as the build's checks do (they set one immediate again where they need it).
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
    if d.kind = 'Extraction' and grp.action <> 'Insert' and not (
      grp.action = 'Update' and grp.entity_name = 'question'
      and not exists (select 1 from jsonb_array_elements(grp.attrs) a
                      where a.value ->> 'attribute' is distinct from 'question_stem_id' or a.value ->> 'old' is not null)
    ) then
      raise exception 'Item %: an extraction only adds rows, and links a question it reuses that has no stem yet.', grp.first_item;
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
      -- an extraction is recorded by the rows it added (or linked), one line for each table (below), not field by
      -- field: what it put in them is in its extraction draft, which is kept
      added_tables := added_tables || grp.entity_name;
      added_ids := added_ids || recorded_id;
      added_acts := added_acts || grp.action;
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
  -- they were added (the tables in the order it first added to them); and a line of the questions it reused and
  -- linked (Update), if any.
  if d.kind = 'Extraction' then
    insert into public.applied_item (draft_id, item_number, entity_name, record_identifier, action)
    select d.draft_id, row_number() over (order by min(first_at)), t, '[' || string_agg(id, ',' order by first_at) || ']', act
    from (
      select a.t, a.act, a.id, min(a.n) as first_at
      from unnest(added_tables, added_ids, added_acts) with ordinality as a(t, id, act, n)
      group by a.t, a.act, a.id
    ) as added
    group by t, act;
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

-- ------ 4c. Functions the site and the dashboard call (through /rpc/) --------------------------------
-- Everything that changes several tables at once is a function, because separate REST calls can't
-- share a transaction.
--
-- The rules (decided 2026-10-07):
--   * An add or an update is applied at once, for STAFF and ADMIN.
--   * A delete, or pushing an extraction, needs one approval when a STAFF member prepared it; an ADMIN
--     never needs approval.
--   * The approver is an active staff member other than the person who prepared the request and other
--     than whoever last changed it (an approver who edited a request has made it their own).
--   * Approving applies: the approval and the change go in together, or neither does.
--   * A change after the request was sent raises its revision; the approver approves the revision
--     they read.
--   * A reviewer may send a request back with a note instead (send_back_draft()). Each submission, send
--     back and approval is kept as a draft_review_event.

-- Whether a draft needs an approval before it is applied.
create function public.draft_needs_approval(p_draft_id bigint)
returns boolean
language sql
stable
set search_path = public, pg_temp
as $$
  select s.role = 'STAFF' and (d.kind = 'Extraction' or e.edit_kind = 'Delete')
  from public.draft d
  join public.staff s on s.staff_id = d.prepared_by
  left join public.edit_draft e on e.draft_id = d.draft_id
  where d.draft_id = p_draft_id;
$$;

-- Adds a draft and its details together. Returns the draft_id.
create function public.create_draft(
  p_kind        text,
  p_prepared_by bigint,
  p_parent      jsonb default '{}'::jsonb,
  p_detail      jsonb default '{}'::jsonb
)
returns bigint
language plpgsql
set search_path = public, pg_temp
as $$
declare
  who    public.staff%rowtype;
  new_id bigint;
begin
  if p_kind is null or p_kind not in ('Extraction', 'Edit') then
    raise exception 'A draft is an Extraction or an Edit.';
  end if;
  select * into who from public.staff where staff_id = p_prepared_by;
  if not found or not who.active then
    raise exception 'Only active staff can prepare a draft.';
  end if;
  if (coalesce(p_parent, '{}'::jsonb) ->> 'requested_reviewer') is not null then
    perform 1 from public.staff where staff_id = (p_parent ->> 'requested_reviewer')::bigint and active;
    if not found then
      raise exception 'Only active staff can be asked to review.';
    end if;
  end if;

  -- a creation time given by the caller is kept (a draft opened on a page keeps the time it was started)
  insert into public.draft (kind, prepared_by, saved_by, created_at)
  values (p_kind, p_prepared_by, p_prepared_by, coalesce((p_parent ->> 'created_at')::timestamptz, now()))
  returning draft_id into new_id;
  perform private.draft_set_parent(new_id, coalesce(p_parent, '{}'::jsonb) - 'created_at');
  perform private.draft_set_details(case p_kind when 'Extraction' then 'extracted_draft' else 'edit_draft' end,
                                    new_id, p_detail, true);
  -- a draft sent for review as it is made (an edit that needs approval) was submitted as well
  perform private.draft_record_submission(new_id, null, p_prepared_by);
  return new_id;
end;
$$;

-- Saves a change to a draft and its details together. Returns the draft's revision after it.
--   p_staff_id           who saves it (becomes saved_by; and who submitted it, if this sends it for review)
--   p_expected_revision  the revision the person was looking at; if it has moved on the save is
--                        refused, so nobody overwrites a version they haven't seen
--   p_new_revision       false for a change that isn't to the content (withdrawing)
create function public.save_draft(
  p_draft_id          bigint,
  p_staff_id          bigint,
  p_parent            jsonb   default '{}'::jsonb,
  p_detail            jsonb   default '{}'::jsonb,
  p_expected_revision integer default null,
  p_new_revision      boolean default true
)
returns integer
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d       public.draft%rowtype;
  who     public.staff%rowtype;
  new_rev integer;
begin
  -- the lock makes a save and an approval of the same draft take turns
  select * into d from public.draft where draft_id = p_draft_id for update;
  if not found then
    raise exception 'That is no longer in the database.';
  end if;
  select * into who from public.staff where staff_id = p_staff_id;
  if not found or not who.active then
    raise exception 'Only active staff can change a draft.';
  end if;
  if p_expected_revision is not null and p_expected_revision <> d.revision then
    raise exception 'That was changed after you opened it. Open it again to see the new version.';
  end if;
  if (coalesce(p_parent, '{}'::jsonb) ->> 'requested_reviewer') is not null then
    perform 1 from public.staff where staff_id = (p_parent ->> 'requested_reviewer')::bigint and active;
    if not found then
      raise exception 'Only active staff can be asked to review.';
    end if;
  end if;

  new_rev := d.revision + case when p_new_revision then 1 else 0 end;
  update public.draft
  set revision = new_rev, saved_by = p_staff_id, saved_at = now()
  where draft_id = p_draft_id;
  perform private.draft_set_parent(p_draft_id, p_parent);
  if coalesce(p_detail, '{}'::jsonb) <> '{}'::jsonb then
    perform private.draft_set_details(case d.kind when 'Extraction' then 'extracted_draft' else 'edit_draft' end,
                                      p_draft_id, p_detail, false);
  end if;
  perform private.draft_record_submission(p_draft_id, d.status, p_staff_id);
  return new_rev;
end;
$$;

-- A reviewer sends a change request back to the person who prepared it, saying what needs changing.
-- p_revision is the revision the reviewer read. Like any save, it makes the reviewer saved_by and moves
-- saved_at on; the revision stays. Returns the draft's revision.
create function public.send_back_draft(
  p_draft_id bigint,
  p_revision integer,
  p_staff_id bigint,
  p_note     text default null
)
returns integer
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d   public.draft%rowtype;
  who public.staff%rowtype;
begin
  -- the lock makes a send back, a save and an approval of the same request take turns
  select * into d from public.draft where draft_id = p_draft_id for update;
  if not found then
    raise exception 'That is no longer in the database.';
  end if;
  select * into who from public.staff where staff_id = p_staff_id;
  if not found or not who.active then
    raise exception 'Only active staff can send a change back.';
  end if;
  if d.status = 'Applied' then
    raise exception 'That change has already been applied, so it can''t be changed.';
  end if;
  if d.status not in ('Awaiting Review', 'Sent Back') then
    raise exception 'Only a change waiting for review can be sent back.';
  end if;
  if d.revision <> p_revision then
    raise exception 'That was changed after you opened it. Open it again to see the new version.';
  end if;

  insert into public.draft_review_event (draft_id, event, staff_id, note)
  values (d.draft_id, 'Sent Back', p_staff_id, p_note);
  update public.draft
  set status = 'Sent Back', saved_by = p_staff_id, saved_at = now()
  where draft_id = p_draft_id;
  return d.revision;
end;
$$;

-- The person who prepared a change applies it, when it needs no approval. For an extraction, p_items
-- are the rows to add. Returns the draft_id.
create function public.apply_draft(
  p_draft_id bigint,
  p_staff_id bigint,
  p_items    jsonb default null
)
returns bigint
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d     public.draft%rowtype;
  who   public.staff%rowtype;
  items jsonb;
begin
  select * into d from public.draft where draft_id = p_draft_id for update;
  if not found then
    raise exception 'That is no longer in the database.';
  end if;
  select * into who from public.staff where staff_id = p_staff_id;
  if not found or not who.active then
    raise exception 'Only active staff can apply a change.';
  end if;
  if d.status = 'Applied' then
    raise exception 'That change has already been applied.';
  end if;
  if d.status = 'Withdrawn' then
    raise exception 'That was withdrawn, so it can''t be applied.';
  end if;
  if p_staff_id <> d.prepared_by then
    raise exception 'Only the person who prepared a change can apply it without approval. Someone else approves it instead.';
  end if;
  if public.draft_needs_approval(d.draft_id) then
    raise exception 'This needs approval from another staff member before it is applied.';
  end if;

  if d.kind = 'Edit' then
    select e.items into items from public.edit_draft e where e.draft_id = d.draft_id;
  else
    items := p_items;
  end if;
  perform private.apply_draft_items(d, items, null);
  return d.draft_id;
end;
$$;

-- Another staff member approves a change request and it is applied, in one step. p_revision is the
-- revision the approver read. For an extraction, p_items are the rows to add. The approval is recorded
-- as an Approved draft_review_event. Returns the draft_id.
create function public.approve_draft(
  p_draft_id bigint,
  p_revision integer,
  p_staff_id bigint,
  p_items    jsonb default null
)
returns bigint
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d     public.draft%rowtype;
  who   public.staff%rowtype;
  items jsonb;
begin
  -- the lock makes a save and an approval of the same request take turns
  select * into d from public.draft where draft_id = p_draft_id for update;
  if not found then
    raise exception 'That is no longer in the database.';
  end if;
  select * into who from public.staff where staff_id = p_staff_id;
  if not found or not who.active then
    raise exception 'Only active staff can approve a change.';
  end if;
  if d.status <> 'Awaiting Review' then
    raise exception 'That is not waiting for approval (it is %).', lower(d.status);
  end if;
  if d.revision <> p_revision then
    raise exception 'That was changed after you opened it. Read it again before approving.';
  end if;
  if p_staff_id = d.prepared_by then
    raise exception 'You prepared this, so someone else has to approve it.';
  end if;
  if p_staff_id = d.saved_by then
    raise exception 'You made the latest changes to this, so someone else has to approve it.';
  end if;

  if d.kind = 'Edit' then
    select e.items into items from public.edit_draft e where e.draft_id = d.draft_id;
  else
    items := p_items;
  end if;
  perform private.apply_draft_items(d, items, p_staff_id);
  return d.draft_id;
end;
$$;

-- One person at a time works on a draft on the Edit Extraction page. Opening it there claims it; the page
-- renews the claim every minute while they are active, and lets it go when they leave, close the page or
-- have done nothing for a while. A claim not renewed for p_stale_seconds has lapsed (a closed laptop, a
-- lost connection), so nobody is kept out by a page that is gone; p_take_over takes it from whoever has
-- it. Claiming never raises the revision or moves saved_at: it isn't a change to the content.
create function public.claim_draft(
  p_draft_id      bigint,
  p_staff_id      bigint,
  p_take_over     boolean default false,
  p_stale_seconds integer default 180
)
returns jsonb
language plpgsql
set search_path = public, pg_temp
as $$
declare
  d public.draft%rowtype;
begin
  -- the lock makes two claims of the same draft take turns, so only one of them gets it
  select * into d from public.draft where draft_id = p_draft_id for update;
  if not found then
    raise exception 'That is no longer in the database.';
  end if;
  perform 1 from public.staff where staff_id = p_staff_id and active;
  if not found then
    raise exception 'Only active staff can change a draft.';
  end if;
  if d.status in ('Applied', 'Withdrawn') then
    raise exception 'That is no longer being prepared, so it can''t be worked on.';
  end if;

  if p_take_over or d.editing_by is null or d.editing_by = p_staff_id
     or d.editing_at < now() - make_interval(secs => p_stale_seconds) then
    update public.draft set editing_by = p_staff_id, editing_at = now()
    where draft_id = p_draft_id
    returning editing_by, editing_at into d.editing_by, d.editing_at;
    return jsonb_build_object(
      'claimed', true, 'editing_by', d.editing_by, 'editing_at', d.editing_at, 'revision', d.revision,
      'editing_name', (select name from public.staff where staff_id = d.editing_by)
    );
  end if;
  return jsonb_build_object(
    'claimed', false, 'editing_by', d.editing_by, 'editing_at', d.editing_at, 'revision', d.revision,
    'editing_name', (select name from public.staff where staff_id = d.editing_by)
  );
end;
$$;

create function public.release_draft(p_draft_id bigint, p_staff_id bigint)
returns boolean
language plpgsql
set search_path = public, pg_temp
as $$
begin
  -- an applied draft can't be changed (draft_guard), and nobody can claim one, so there is nothing to let go
  update public.draft set editing_by = null, editing_at = null
  where draft_id = p_draft_id and editing_by = p_staff_id and status <> 'Applied';
  return found;
end;
$$;

-- BR16: the one write the public dashboard may make. Anonymous: nothing about the voter is stored.
-- Once-per-session limiting (BR17) is up to the dashboard. Runs with its owner's rights, because the
-- dashboard can't update questionnaire itself.
create function public.upvote_questionnaire(p_questionnaire_id bigint)
returns integer
language sql
security definer
set search_path = ''
as $$
  update public.questionnaire
     set upvote_count = upvote_count + 1
   where questionnaire_id = p_questionnaire_id
  returning upvote_count;
$$;


-- ------ 4d. Respondents' counts, extraction drafts that go, and the audit's rules ----------------------

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
      on wq.placement_id = v.placement_id and wq.wave_id = new.wave_id
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

-- A PDF (the questionnaire's, or the file of Variable Names and Coding) that no draft and no wave uses
-- any more goes too: its draft was discarded, or its wave deleted or extracted again from another file (its
-- file in Storage is removed by the site). One a draft or a wave still uses stays.
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

-- A questionnaire's participant count is the total across all its waves, so the waves' own counts (where
-- known) can't add up to more. Checked when the transaction ends, so a change can set both.
create function public.wave_counts_within_questionnaire()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  q     public.questionnaire%rowtype;
  waves bigint;
begin
  select * into q from public.questionnaire where questionnaire_id = new.questionnaire_id;
  if not found or q.participant_count is null then
    return null;
  end if;
  select coalesce(sum(respondent_count), 0) into waves from public.wave where questionnaire_id = q.questionnaire_id;
  if waves > q.participant_count then
    raise exception 'The waves of % have % respondents between them, more than its % participants in all.',
      q.name, waves, q.participant_count;
  end if;
  return null;
end;
$$;

create constraint trigger wave_counts_within_questionnaire
  after insert or update of respondent_count, questionnaire_id on public.wave
  deferrable initially deferred
  for each row execute function public.wave_counts_within_questionnaire();

-- (the same function: new.questionnaire_id is the questionnaire's own)
create constraint trigger questionnaire_count_covers_waves
  after insert or update of participant_count on public.questionnaire
  deferrable initially deferred
  for each row execute function public.wave_counts_within_questionnaire();

create trigger extracted_draft_pdf_let_go
  after delete on public.extracted_draft
  for each row execute function public.pdf_file_let_go();
create trigger wave_file_pdf_let_go
  after delete or update of pdf_sha256, supplement_sha256 on public.wave_file
  for each row execute function public.pdf_file_let_go();

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

-- M2 (BR45), M3 (BR53) and M4 (BR42): what a block of each kind may be, by the kind's name. The site relies on
-- the same four names (Section, Question, Grid, Group) and Make an edit won't rename or delete them.
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

-- M3: where a questionnaire places a block, its parent is a Section, a Grid or a Group. A Section is never
-- inside a Group or a Grid, and a Grid holds only its questions with their instructions and definitions: no
-- Section, Grid or Group. Only a Group or a Grid skips (on the answers to the questions in it). The site
-- keeps the same rules in can_hold() (R/service_draft.R).
create function private.placement_check(p_placement_id bigint)
returns void
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_component   bigint;
  v_parent      bigint;
  v_kind        text;
  v_parent_kind text;
  v_skips       boolean;
begin
  select component_id, parent_placement_id,
         skip_condition is not null or skip_when is not null or skip_instruction is not null
           or skip_to_placement_id is not null or skip_to_end
    into v_component, v_parent, v_skips
  from public.questionnaire_component
  where placement_id = p_placement_id;
  if not found then
    return;
  end if;
  v_kind := private.component_kind(v_component);
  if v_skips and v_kind is distinct from 'Group' and v_kind is distinct from 'Grid' then
    raise exception 'Placement % skips, but only a Group or a Grid skips on the answers to the questions in it.', p_placement_id;
  end if;
  if v_parent is null then
    return;
  end if;
  select private.component_kind(component_id) into v_parent_kind
  from public.questionnaire_component where placement_id = v_parent;
  if v_parent_kind is null or v_parent_kind not in ('Section', 'Grid', 'Group') then
    raise exception 'Placement % is inside a %: only a Section, a Grid or a Group holds other blocks.', p_placement_id, coalesce(v_parent_kind, 'block that is gone');
  end if;
  if v_parent_kind in ('Group', 'Grid') and v_kind = 'Section' then
    raise exception 'Section placement % is inside a %: a Section is never inside a Group or a Grid.', p_placement_id, v_parent_kind;
  end if;
  if v_parent_kind = 'Grid' and v_kind in ('Grid', 'Group') then
    raise exception '% placement % is inside a Grid: a Grid holds only its questions, with their instructions and definitions.', v_kind, p_placement_id;
  end if;
end;
$$;

create function public.questionnaire_component_parent_check()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  perform private.placement_check(new.placement_id);
  return null;
end;
$$;

create constraint trigger questionnaire_component_parent_kind
  after insert or update on public.questionnaire_component
  deferrable initially deferred
  for each row execute function public.questionnaire_component_parent_check();

-- ... and a block whose kind changes is checked wherever it is placed, and where blocks sit inside it.
create function public.component_placements_check()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  p record;
begin
  for p in
    select c.placement_id from public.questionnaire_component c
    where c.component_id = new.component_id
       or c.parent_placement_id in (select placement_id from public.questionnaire_component where component_id = new.component_id)
  loop
    perform private.placement_check(p.placement_id);
  end loop;
  return null;
end;
$$;

create constraint trigger component_kind_placements
  after update of component_type_id on public.component
  deferrable initially deferred
  for each row execute function public.component_placements_check();

-- ------ What nothing owns ----------------------------------------------------------------------------------
-- A delete can leave a shared row that nothing owns any more: a component no questionnaire prints, a question
-- no wave asks, a stem no question has. Each goes when the change ends (deferred, so a change can take a block
-- out and put it back, or move it), unless something still names it: a component that a measurement scale
-- lists stays, and so does a question placement that follow-up questions name as theirs or a skip sends
-- respondents to.
-- None of them refuses anything. R/service_edit_relations.R shows staff the same before a delete.

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
    and not exists (select 1 from public.question_scale s where s.component_id = c.component_id);
  return null;
end;
$$;

create constraint trigger component_left_unplaced
  after delete or update of component_id on public.questionnaire_component
  deferrable initially deferred
  for each row execute function public.forget_unplaced_component();

-- A question placement no wave asks (BR69): it goes, and with it its settings and variables (and then the
-- component, if nothing else places it). A follow-up that goes may leave the question it follows unasked and
-- followed by none, so that one is looked at next, and so on up.
create function public.forget_unasked_question()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_question bigint := old.placement_id;
  v_parent   bigint;
begin
  while v_question is not null loop
    select q.parent_question_placement_id into v_parent from public.questionnaire_question q
    where q.placement_id = v_question;
    delete from public.questionnaire_component p
    where p.placement_id = v_question
      and exists (select 1 from public.questionnaire_question q where q.placement_id = p.placement_id)
      and not exists (select 1 from public.wave_question w where w.placement_id = p.placement_id)
      and not exists (select 1 from public.questionnaire_question f where f.parent_question_placement_id = p.placement_id)
      and not exists (select 1 from public.questionnaire_component g where g.skip_to_placement_id = p.placement_id)
      and not exists (select 1 from public.response_option r where r.skip_to_placement_id = p.placement_id)
      and not exists (select 1 from public.open_ended o where o.skip_to_placement_id = p.placement_id);
    exit when not found;
    v_question := v_parent;
  end loop;
  return null;
end;
$$;

create constraint trigger question_left_unasked
  after delete or update of wave_id, placement_id on public.wave_question
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
alter table public.measurement_scale      add constraint measurement_scale_name_not_blank        check (btrim(name) <> ''),
                                          add constraint measurement_scale_abbreviation_not_blank check (btrim(abbreviation) <> '');
alter table public.category               add constraint category_term_not_blank                 check (btrim(term) <> '');
alter table public.question_scale         add constraint question_scale_item_number_not_blank    check (btrim(item_number) <> '');
alter table public.variable               add constraint variable_matr_id_not_blank              check (btrim(matr_id) <> '');
alter table public.staff                  add constraint staff_name_not_blank                    check (btrim(name) <> ''),
                                          add constraint staff_email_not_blank                   check (btrim(email) <> '');
-- (answer_option.standardized_label: answer_option_label_normal, part B, refuses a blank too)

-- M7 (BR5): a study's lead investigator never changes once it is recorded (a study added without one
-- can be given one later).
create function public.study_principal_investigator_immutable()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
begin
  if old.principal_investigator_id is not null
     and new.principal_investigator_id is distinct from old.principal_investigator_id then
    raise exception 'A study''s lead investigator can''t be changed. The contact investigator can.';
  end if;
  return new;
end;
$$;

create trigger study_principal_investigator_immutable
  before update of principal_investigator_id on public.study
  for each row execute function public.study_principal_investigator_immutable();

-- M8: an investigator may have no email, but a study's contact is someone who can be written to. Checked when the
-- transaction ends, so a change can give someone an email and make them the contact in either order.
create function public.contact_has_email()
returns trigger
language plpgsql
set search_path = public, pg_temp
as $$
declare
  v_name text;
begin
  -- one query for each table: PL/pgSQL looks up every new.<field> a query names, so a study's field can't be named
  -- when an investigator changes
  if tg_table_name = 'study' then
    select p.name into v_name
    from public.study s join public.principal_investigator p on p.principal_investigator_id = s.contact_principal_investigator_id
    where p.email is null and s.study_id = new.study_id
    limit 1;
  else
    select p.name into v_name
    from public.study s join public.principal_investigator p on p.principal_investigator_id = s.contact_principal_investigator_id
    where p.email is null and p.principal_investigator_id = new.principal_investigator_id
    limit 1;
  end if;
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

-- The trigger functions above are for the database itself; nobody calls them over /rpc/.
revoke all on function public.response_option_count_in_wave()         from public, anon, authenticated;
revoke all on function public.pdf_file_let_go()                      from public, anon, authenticated;
revoke all on function public.wave_counts_within_questionnaire()      from public, anon, authenticated;
revoke all on function public.choices_match_set_check()               from public, anon, authenticated;
revoke all on function public.component_kind_check()                  from public, anon, authenticated;
revoke all on function public.questionnaire_component_parent_check()  from public, anon, authenticated;
revoke all on function public.component_placements_check()            from public, anon, authenticated;
revoke all on function public.study_principal_investigator_immutable() from public, anon, authenticated;
revoke all on function public.contact_has_email()                     from public, anon, authenticated;
revoke all on function public.forget_unplaced_component()             from public, anon, authenticated;
revoke all on function public.forget_unasked_question()               from public, anon, authenticated;
revoke all on function public.forget_unused_stem()                    from public, anon, authenticated;
revoke all on function public.option_set_choices_unique()             from public, anon, authenticated;
revoke all on function public.staff_never_deleted()                   from public, anon, authenticated;
revoke all on function private.normal_answer(text)                     from public, anon, authenticated;
revoke all on function private.choices_match_set(bigint[])             from public, anon, authenticated;
revoke all on function private.component_kind(bigint)                  from public, anon, authenticated;
revoke all on function private.placement_check(bigint)                 from public, anon, authenticated;
-- the site's server runs apply_draft_items() as itself, and it fires these
grant execute on function private.normal_answer(text)          to service_role;
grant execute on function private.choices_match_set(bigint[])  to service_role;
grant execute on function private.component_kind(bigint)       to service_role;
grant execute on function private.placement_check(bigint)       to service_role;


-- =====================================================================================================
-- 5. SECURITY & PERMISSIONS
-- =====================================================================================================
-- Three roles reach the database through the REST API:
--   anon, authenticated   the public dashboard: reads the MATR tables, upvotes, nothing else
--   service_role          the site's server: reads and writes every table, calls the draft functions.
--                         It skips Row Level Security, but still needs GRANTs.
-- Supabase grants anon and authenticated everything on new tables and functions in public by default,
-- so this section takes all of that away first and then grants only what is meant.

-- ------ 5a. Row Level Security on every table --------------------------------------------------------
-- With RLS on and no policy, anon and authenticated see no rows even if a grant slips through later.
do $$
declare
  t text;
begin
  for t in select c.relname from pg_class c
           where c.relnamespace = 'public'::regnamespace and c.relkind = 'r' loop
    execute format('alter table public.%I enable row level security', t);
  end loop;
end;
$$;

-- ------ 5b. Nothing for the dashboard roles unless granted below --------------------------------------
revoke all on all tables    in schema public from anon, authenticated;
revoke all on all sequences in schema public from anon, authenticated;
revoke all on all functions in schema public from public, anon, authenticated;
revoke all on schema private from public, anon, authenticated;
revoke all on all functions in schema private from public, anon, authenticated;

-- ...and the same for tables and functions made later by the role running this (postgres), so a new
-- table is private until it is shared on purpose.
alter default privileges in schema public revoke all on tables    from anon, authenticated;
alter default privileges in schema public revoke all on sequences from anon, authenticated;
alter default privileges in schema public revoke execute on functions from anon, authenticated;
-- PostgreSQL itself still lets everyone (PUBLIC) run a new function, and that default can only be
-- changed for every schema at once (extensions' functions included), so it is left alone. When you
-- add a function to public, follow it with: revoke all on function public.<name>(<args>) from public;

-- ------ 5c. The public dashboard reads the MATR tables (not staff, drafts or history) ----------------
do $$
declare
  t text;
  dashboard_tables constant text[] := array[
    'registry', 'principal_investigator', 'study', 'respondent_type', 'questionnaire', 'wave',
    'component_type', 'component', 'question', 'questionnaire_component', 'questionnaire_question',
    'wave_question', 'subject', 'time_frame', 'variable', 'open_ended', 'closed_ended', 'option_set',
    'answer_option', 'answer_option_set', 'response_option', 'response_option_count', 'category', 'question_category',
    'measurement_scale', 'question_scale', 'question_stem', 'answer_option_alias', 'wave_file'
  ];
begin
  foreach t in array dashboard_tables loop
    execute format('create policy "Dashboard can read" on public.%I for select to anon, authenticated using (true)', t);
    execute format('grant select on public.%I to anon, authenticated', t);
  end loop;
end;
$$;

-- ...and the PDFs a wave was extracted from, but no others: the bucket is public, so a file is safe from
-- anyone without its SHA-256 only while nothing lists it. A draft's PDF stays unlisted until its wave has it.
create policy "Dashboard can read a wave's files" on public.pdf_file for select to anon, authenticated
  using (exists (select 1 from public.wave_file f
                 where f.pdf_sha256 = pdf_file.pdf_sha256 or f.supplement_sha256 = pdf_file.pdf_sha256));
grant select on public.pdf_file to anon, authenticated;

grant execute on function public.upvote_questionnaire(bigint) to anon, authenticated;
-- an option set's name, read as its column
revoke all on function public.name(public.option_set) from public;
grant execute on function public.name(public.option_set) to anon, authenticated, service_role;

-- ------ 5d. The site's server ------------------------------------------------------------------------
grant select, insert, update, delete on all tables    in schema public to service_role;
grant usage, select                  on all sequences in schema public to service_role;
alter default privileges in schema public grant select, insert, update, delete on tables    to service_role;
alter default privileges in schema public grant usage, select                  on sequences to service_role;

grant execute on function public.draft_needs_approval(bigint)                                to service_role;
grant execute on function public.create_draft(text, bigint, jsonb, jsonb)                    to service_role;
grant execute on function public.save_draft(bigint, bigint, jsonb, jsonb, integer, boolean)  to service_role;
grant execute on function public.apply_draft(bigint, bigint, jsonb)                          to service_role;
grant execute on function public.approve_draft(bigint, integer, bigint, jsonb)              to service_role;
grant execute on function public.send_back_draft(bigint, integer, bigint, text)              to service_role;
grant execute on function public.claim_draft(bigint, bigint, boolean, integer)               to service_role;
grant execute on function public.release_draft(bigint, bigint)                               to service_role;
grant execute on function public.discard_draft(bigint, bigint)                               to service_role;
revoke all on function public.claim_draft(bigint, bigint, boolean, integer) from public;
revoke all on function public.release_draft(bigint, bigint)                 from public;
grant execute on function public.upvote_questionnaire(bigint)                                to service_role;

-- The public functions above run as the caller, so the caller needs the helpers too. "private" isn't
-- among the schemas the REST API exposes, so these can't be called over /rpc/.
grant usage on schema private to service_role;
grant execute on function private.random_id(text, text)                              to service_role;
grant execute on function private.draft_set_details(text, bigint, jsonb, boolean)    to service_role;
grant execute on function private.draft_set_row(text, bigint, jsonb, boolean)        to service_role;
grant execute on function private.draft_record_submission(bigint, text, bigint)      to service_role;
grant execute on function private.draft_set_parent(bigint, jsonb)                    to service_role;
grant execute on function private.apply_draft_items(public.draft, jsonb, bigint)       to service_role;


-- =====================================================================================================
-- 6. DESCRIPTIONS
-- =====================================================================================================

-- ------ Tables ---------------------------------------------------------------------------------------
comment on table public.registry                is 'Outermost container; every study, questionnaire and variable rolls up to one.';
comment on table public.principal_investigator  is 'A researcher who leads a study or is its contact.';
comment on table public.study                   is 'One research project run under a registry.';
comment on table public.respondent_type         is 'The population a questionnaire was written for, bounded by age.';
comment on table public.questionnaire           is 'What stays true across every administration of an instrument.';
comment on table public.wave                    is 'One round of data collection for a questionnaire.';
comment on table public.component_type          is 'Kind of building block: Section, Question, Instruction, ...';
comment on table public.component               is 'One piece of printed material, held once for the whole registry: one for each kind and wording, however many times it is printed. The one Group is shared by every group.';
comment on table public.question                is 'Subtype of component that requests one or more answers.';
comment on table public.question_stem           is 'What questions that differ only in who or when they ask about share, e.g. "Has [subject] ever repeated a grade?", "on how many occasions have you smoked cigarettes [time frame]?". [subject] and [time frame] are filled in with each variable''s subject and time frame when shown.';
comment on column public.question.question_stem_id is 'The stem this question shares with the questions that differ from it only in who they ask about (Twin A, Twin B). Empty when it shares none.';
comment on table public.questionnaire_component is 'A placement: one place where a questionnaire printed a component, with what holds it and its order. A questionnaire can print a component more than once; each place is a placement of its own.';
comment on table public.questionnaire_question  is 'A placement of a question: its settings there. Variables attach here.';
comment on table public.wave_question           is 'That a wave actually asked a question: its place among the wave''s questions, and the number printed beside it.';
comment on table public.subject                 is 'Who a variable can be about. Kept by staff (Make an edit, kind Subject); read by the dashboard.';
comment on table public.time_frame              is 'When a variable''s answer can be about, written one way, e.g. "in the past 12 months". Fills [time frame] in a question stem; read by the dashboard.';
comment on table public.variable                is 'One place an answer is recorded; one column in the response database.';
comment on table public.open_ended              is 'Variable the respondent writes.';
comment on table public.closed_ended            is 'Variable chosen from an option set.';
comment on table public.option_set              is 'A set of answer options, one for each combination; each variable offering it codes and orders them its own way. Its name (public.name(option_set), read as a column) is its choices in their order.';
comment on table public.answer_option           is 'One answer choice, stored once for the whole registry. [subject] in its words stands for the variable''s subject ("describes [subject] very well").';
comment on table public.answer_option_alias     is 'A wording merged into an answer choice on Make an edit. An extraction that reads it uses the choice instead.';
comment on table public.response_option_count   is 'How many of a wave''s respondents gave one answer (response option) of one variable. Typed on Edit Extraction for the extraction''s wave.';
comment on table public.answer_option_set       is 'The answer options of an option set, in the order its name lists them.';
comment on table public.response_option         is 'One answer option as it was printed for one closed-ended variable. A closed-ended variable has one or more (a lone check box has one).';
comment on table public.category                is 'One MeSH term, keyed by its NLM unique identifier.';
comment on table public.question_category       is 'Links questions to MeSH categories.';
comment on table public.measurement_scale       is 'A published, validated set of questions scored together.';
comment on table public.question_scale          is 'A question''s membership and item number in a measurement scale.';
comment on table public.staff                   is 'MATR employee who prepares and reviews metadata. Never deleted; marked inactive.';
comment on table public.draft                   is 'A change to the MATR data: an extraction of a questionnaire (extracted_draft) or an edit (edit_draft). While staff work on it, a draft; once Applied, the record of what was done, by whom, and who approved it (draft_review_event).';
comment on table public.edit_draft              is 'The details of a draft that is an edit made on Make an edit.';
comment on table public.extracted_draft         is 'The details of a draft that is an extraction of a questionnaire: typed by hand (Manual) or read from a PDF by an AI (AI-Powered, with its ai_extracted_draft).';
comment on table public.ai_extracted_draft      is 'What an AI-Powered extraction has and a Manual one doesn''t: the reading of its PDF, what the AI returned, and what the reviewer changed in it.';
comment on table public.pdf_file                is 'A PDF uploaded for an extraction (the questionnaire, or its file of Variable Names and Coding), once per file (by its SHA-256): its pages and size. The file itself is in Storage, bucket extraction-pdfs, as <hash>.pdf. Kept while a wave was extracted from it (wave_file) or an extraction draft uses it; the dashboard sees only a wave''s.';
comment on column public.answer_option.standardized_label is
  'The answer in standard wording, kept normalized: lower case, straight quotes, one space between words, no question mark, no full stop or colon at the end.';
comment on column public.extracted_draft.supplement is
  'The optional file of MATR IDs and codes Gemini reads after the questionnaire: {name, sha256, bytes} for a PDF (in Storage as <sha256>.pdf), or {name, text} for a CSV or text file.';
comment on table public.draft_review_event      is 'Each time a draft was submitted for review, sent back, or approved: by whom and when. An approval is of the draft''s revision the approver read (approve_draft checks it).';
comment on table public.applied_item            is 'What an applied draft did: for an edit that adds or updates, one field it changed, with its old and new value; for an extraction, one table it added rows to (or, as an Update, the questions it reused and linked with others); for an edit that deletes, the one row staff picked to delete (what it and the rows that went with it held isn''t kept).';

-- ------ Columns --------------------------------------------------------------------------------------
comment on column public.study.status is 'Active: the study is ongoing. Ended: all data collections are finished. Empty when not known.';
comment on column public.study.principal_investigator_id is 'The lead investigator. Empty when not known; never changes once recorded.';
comment on column public.study.contact_principal_investigator_id is 'Who to contact about the study. Empty when not known; someone with an email.';
comment on column public.principal_investigator.email is 'Unique, ignoring case and outer spaces. Empty when not known, but a study''s contact has one.';
comment on column public.questionnaire.participant_count is
  'How many took it, across all its waves, for when the waves'' own counts aren''t known. The waves'' counts can''t add up to more.';
comment on table public.wave_file is
  'The PDF and the file of Variable Names and Coding a wave''s extraction was read from, written when the extraction is applied. Goes with the wave (and the files with it, when nothing else uses them); the dashboard reads it.';
comment on column public.wave_file.pdf_sha256 is 'The questionnaire''s PDF (pdf_file).';
comment on column public.wave_file.pdf_name is 'That PDF''s name, as uploaded.';
comment on column public.wave_file.supplement_name is 'The name of the file of Variable Names and Coding the extraction was given, if any.';
comment on column public.wave_file.supplement_sha256 is 'That file, when it is a PDF (pdf_file).';
comment on column public.wave_file.supplement_text is 'That file''s text, when it is a CSV or text file.';
comment on column public.questionnaire_component.placement_id is 'System key: this place in the questionnaire.';
comment on column public.questionnaire_component.parent_placement_id is 'The placement of the Section or Group it is in. Empty at the top level.';
comment on column public.questionnaire_component.display_order is 'Position among the placements sharing the same parent, counting from 1.';
comment on column public.questionnaire_question.parent_question_placement_id is 'The placement of the question it follows (a follow-up, or one reached by routing). Empty when it follows none.';
comment on column public.variable.placement_id is 'The placement of the question whose answer it records.';
comment on column public.questionnaire_component.skip_condition is
  'For a Group that skips: the answer that skips, e.g. No for "If you answered No to items 22a-d, go to Question 28". Empty when it doesn''t skip.';
comment on column public.questionnaire_component.skip_when is
  'For a Group that skips: every (the skip happens when every question in the group gets the answer) or any (when any of them does).';
comment on column public.questionnaire_component.skip_instruction is
  'For a Group that skips: the routing as printed, e.g. "Go to Question 28".';
comment on column public.questionnaire_component.skip_to_placement_id is
  'For a Group that skips: where it sends respondents, the question, section or group it names. Empty when nothing is linked, or when it ends the questionnaire (skip_to_end).';
comment on column public.questionnaire_component.skip_to_end is
  'For a Group that skips: it sends respondents to the end of the questionnaire.';
comment on column public.variable.variable_key is 'System key. Deliberately not "variable_id"; see matr_id.';
comment on column public.variable.matr_id is 'What MATR staff call the variable ID, e.g. AH25LF. Unique within a questionnaire; never changes.';
comment on column public.variable.subject_id is 'Who the variable is about (public.subject). Null when not known.';
comment on column public.variable.time_frame_id is 'When the variable''s answer is about (public.time_frame), e.g. "in the past 30 days". Null when its question names no time.';
comment on column public.response_option.code_value is 'Stored value for this option in the response data, e.g. 1 for Yes.';
comment on column public.response_option.skip_to_placement_id is
  'Where the skip instruction sends respondents: the question, section or question group it names. Empty when nothing is linked, or when it ends the questionnaire (skip_to_end).';
comment on column public.response_option.skip_to_end is
  'The skip instruction sends respondents to the end of the questionnaire.';
comment on column public.open_ended.skip_condition is
  'The answer that skips, e.g. None for "If none, go to Question 57". Empty when the write-in has no skip.';
comment on column public.open_ended.skip_instruction is
  'The routing printed for the write-in, e.g. "Go to Question 57".';
comment on column public.open_ended.skip_to_placement_id is
  'Where the skip sends respondents: the question, section or question group it names. Empty when nothing is linked, or when it ends the questionnaire (skip_to_end).';
comment on column public.open_ended.skip_to_end is
  'The skip sends respondents to the end of the questionnaire.';

comment on column public.staff.role is
  'STAFF: deleting, or pushing an extraction, needs one approval from another staff member. ADMIN: never needs approval.';

comment on column public.draft.kind is 'Extraction or Edit; says which subtype table holds the rest.';
comment on column public.draft.status is
  'Draft (being prepared), Awaiting Review (a change request), Sent Back (a reviewer returned it), Applied (in the MATR data) or Withdrawn (the preparer pulled it). When it was submitted, sent back or approved is in draft_review_event.';
comment on column public.draft.prepared_by is 'Who started it. Never changes.';
comment on column public.draft.saved_by is 'Who saved it last (or sent it back). An approver can''t be this person.';
comment on column public.draft.requested_reviewer is
  'The staff member the preparer asked to review it. Any other eligible staff member may still approve.';
comment on column public.draft.revision is 'Rises every time the content is saved. An approval is of the revision the approver read.';
comment on column public.draft.summary is 'One line saying what it comes to; a change request is listed by it.';
comment on column public.draft.saved_at is 'When staff last saved it. The reading job''s progress does not change it.';

comment on column public.edit_draft.edit_kind is 'What the preparer chose: Add, Update or Delete.';
comment on column public.edit_draft.form is 'The sentence and the form as typed, to open it again: {action, entity, scope, target, values}.';
comment on column public.edit_draft.reach is 'For a delete: how many rows go with it (rows_removed), how many are emptied (rows_emptied), and the row staff picked to delete (deleted: {table, row, name}: its key as JSON, and its name as the Edit History shows it), which is all that is kept of it.';
comment on column public.edit_draft.items is
  'The rows and fields it changes, in the order to apply them: item_number, entity_name, record_identifier, action, attribute_name, old_value, new_value. Emptied once a delete is applied.';

comment on column public.extracted_draft.method is
  'AI-Powered: Gemini read the PDF (its reading is in ai_extracted_draft). Manual: staff typed it.';
comment on column public.extracted_draft.info is
  'Where the extraction goes, picked on Change Requests after its change request is submitted (empty until then): registry_id, study_id, questionnaire_id and wave_id, a recorded wave that asks no questions yet.';
comment on column public.extracted_draft.matr_csv is 'The uploaded CSV of MATR IDs: {name, text}.';
comment on column public.extracted_draft.questionnaire is
  'The questionnaire as staff last saved it (blocks, variables, choices). Null until they save, then the reviewed copy.';
comment on column public.extracted_draft.pdf_sha256 is 'The PDF (pdf_file), if there is one. An AI-Powered extraction always has one.';
comment on column public.extracted_draft.pdf_name is 'The PDF''s name as uploaded with this draft (the same file can be uploaded under two names).';

comment on column public.ai_extracted_draft.method is 'Always AI-Powered: with draft_id, it can only point at an AI-Powered extraction.';
comment on column public.ai_extracted_draft.ai_original is
  'The questionnaire as the AI returned it, tidied by the reading job (spaces, quotes, trailing full stops on numbers). Written by the reading job before anyone saves the draft, and never changed after, so what the reviewer changed can be measured.';
comment on column public.ai_extracted_draft.review_edits is
  'How the reviewed copy differs from ai_original, worked out again (not added to) each time staff save: [{change, item, ref, where, field, before, after}]. A value changed back is no difference.';
comment on column public.component.text is 'The printed words. Empty only for a Group, which has none (BR42).';
comment on column public.wave_question.question_number is
  'The question''s place among the questions the wave asks (1, 2, 3...), in printed order; sections and question groups aren''t counted. Unique in a wave. Worked out by the site, not typed.';
comment on column public.wave_question.printed_number is
  'The number or label printed beside the question ("1a", "12", "Q7"), as text. Can repeat in a wave (each section can start again at 1). Empty when none is printed.';
comment on column public.ai_extracted_draft.reading_status is 'How far the reading job has got: queued, sent, received, ready or failed.';
comment on column public.ai_extracted_draft.reading_started is 'When this reading was started. A job started for an earlier one saves nothing.';
comment on column public.ai_extracted_draft.reading_claimed_at is
  'When a copy of the site claimed this draft for reading. Set only while null, so one job runs.';
comment on column public.ai_extracted_draft.model is
  'The Gemini model that read it, as Gemini named it. With prompt_version, says what the reviewer''s changes measure.';
comment on column public.ai_extracted_draft.prompt_version is
  'The first 12 characters of the SHA-256 of the instructions Gemini was given (the prompt and the lookup lists).';

comment on column public.pdf_file.pdf_pages is 'Empty when the pages couldn''t be counted. Once known, never changes.';
comment on column public.pdf_file.pdf_bytes is 'Once known, never changes.';

comment on column public.draft_review_event.event is
  'Submitted (sent for review, by the person who saved it then), Sent Back (a reviewer returned it, with a note) or Approved (and applied, in the same step; at most one).';
comment on column public.draft_review_event.note is 'Sent Back only: what the reviewer asks to be changed.';

comment on column public.draft.applied_at is 'When it was applied to the MATR data. Empty until then.';
comment on column public.draft.editing_by is
  'Who has it open on the Edit Extraction page, if anyone (claim_draft()). Empty when nobody does.';
comment on column public.draft.editing_at is
  'When their page last renewed the claim. One not renewed for a few minutes has lapsed, and anyone may claim the draft.';
comment on column public.applied_item.record_identifier is 'The row''s key as JSON, with the numbers the database gave rows the draft added. For an extraction, a JSON array of the keys of every row it added to the table.';
comment on column public.applied_item.action is 'What happened to this one row: Insert, Update or Delete.';

-- ------ Functions ------------------------------------------------------------------------------------
comment on function public.draft_needs_approval(bigint) is
  'Whether the draft needs one approval before it is applied: a STAFF member''s delete, or a STAFF member''s extraction. An ADMIN''s never does.';
comment on function public.create_draft(text, bigint, jsonb, jsonb) is
  'Adds a draft and its details (extracted_draft or edit_draft) together. Returns the draft_id.';
comment on function public.save_draft(bigint, bigint, jsonb, jsonb, integer, boolean) is
  'Saves a change to a draft and its details together, as the given staff member, and returns its revision. Refused if the revision the caller read has moved on. Sending it for review records a Submitted event.';
comment on function public.apply_draft(bigint, bigint, jsonb) is
  'The person who prepared a change applies it, when it needs no approval (an add or an update, or anything an ADMIN prepared). Returns the draft_id.';
comment on function public.approve_draft(bigint, integer, bigint, jsonb) is
  'Another staff member approves a change request and it is applied, in one transaction; the approval is an Approved event. Returns the draft_id.';
comment on function public.send_back_draft(bigint, integer, bigint, text) is
  'A reviewer sends a change request back to its preparer, with a note: records a Sent Back event and sets the status. Refused if the revision the reviewer read has moved on. Returns the revision.';
comment on function public.claim_draft(bigint, bigint, boolean, integer) is
  'Claims a draft for one staff member to work on, or renews their claim. Refused (claimed false) while someone else''s claim was renewed within p_stale_seconds, unless p_take_over. Never raises the revision or moves saved_at. Returns {claimed, editing_by, editing_name, editing_at, revision}.';
comment on function public.release_draft(bigint, bigint) is
  'Lets go of a staff member''s claim on a draft, if they still have it. Returns whether they did.';
comment on function public.upvote_questionnaire(bigint) is
  'Adds one to a questionnaire''s upvote count (BR16). The only write the public dashboard can make.';
comment on function private.random_id(text, text) is
  'Internal: a random 9-digit number not yet used in that table''s column. The default of every table''s own ID.';
comment on function private.draft_set_details(text, bigint, jsonb, boolean) is
  'Internal: sets a draft''s details from JSON, an extraction''s in extracted_draft, ai_extracted_draft and pdf_file. Called by create_draft() and save_draft().';
comment on function private.draft_set_row(text, bigint, jsonb, boolean) is
  'Internal: sets one detail row of a draft from JSON. Called by draft_set_details().';
comment on function private.draft_record_submission(bigint, text, bigint) is
  'Internal: records a Submitted event for a draft that has just gone to Awaiting Review. Called by create_draft() and save_draft().';
comment on function private.draft_set_parent(bigint, jsonb) is
  'Internal: sets the columns of draft a caller may set. Called by create_draft() and save_draft().';
comment on function private.apply_draft_items(public.draft, jsonb, bigint) is
  'Internal: applies the items of a draft in one transaction, records them in applied_item and the approval as an event. Called by apply_draft() and approve_draft() after they have checked who may.';


-- =====================================================================================================
-- 7. STARTING DATA
-- =====================================================================================================

-- Component types identified from the first six questionnaires.
insert into public.component_type (name, description) values
  ('Section',            'Contains other components; holds the printed heading, including any section number.'),
  ('Question',           'Requests one or more answers; has variables recording them.'),
  ('Introduction',       'Opening text that introduces the questionnaire or a section.'),
  ('Instruction',        'Printed directions telling the respondent how to answer.'),
  ('Definition',         'Printed explanation of a term used in the questionnaire.'),
  ('Interviewer Script', 'Text an interviewer reads aloud or follows.'),
  ('Note',               'Any other printed remark.'),
  ('Group',              'Holds blocks that belong together but have no printed heading: a battery, a definition with its question, a question with its follow-up. Has no text, so there is one Group component, placed wherever a questionnaire groups blocks.'),
  ('Grid',               'A lead-in its questions share, printed once above them (a table of rows, or a stem with several items): "In the past month, how often did you...". Holds those questions, each with only its own words, and any instruction or definition among them; a question''s full wording is the lead-in followed by its words. Questions that are whole without a lead-in go in a Group instead.');

-- Who a variable can be about; staff keep this list on Make an edit.
insert into public.subject (name) values
  ('Respondent'), ('Respondent''s Family'), ('Respondent''s Children'), ('Respondent''s Siblings'), ('Twin A'), ('Twin B');

-- When a variable can be about, as normal_time_frame() writes them; an extraction adds any other it finds.
insert into public.time_frame (name) values
  ('in your lifetime'), ('in the past year'), ('in the past 12 months'), ('in the past 6 months'), ('in the past month'),
  ('in the past 30 days'), ('in the past 4 weeks'), ('in the past 2 weeks'), ('in the past week');

-- The PDFs' bucket: the extractions' PDFs (and PDF files of Variable Names and Coding) are kept in Storage, as
-- <sha256>.pdf (R/model_storage.R). Public: anyone can download a file by its address,
-- <project URL>/storage/v1/object/public/extraction-pdfs/<sha256>.pdf, so the dashboard can link a wave's PDF
-- (wave_file names it). Only the site's server writes to it, with the secret key: no policy on storage.objects
-- lets anyone else add, change or remove a file. Up to 50 MB, as the site accepts (shiny.maxRequestSize in
-- global.R), and only PDFs. A project that has it keeps its files, and the bucket is made public, so this
-- statement can also be run by itself.
insert into storage.buckets (id, name, public, file_size_limit, allowed_mime_types)
values ('extraction-pdfs', 'extraction-pdfs', true, 50 * 1024 * 1024, array['application/pdf'])
on conflict (id) do update set public = true;

commit;

-- =====================================================================================================
-- Rules the site and the dashboard enforce, not this script:
--   BR17 one upvote per session         BR38 every write recorded (true of changes made through
--                                            drafts: an applied draft and its applied_item rows)
--   BR64 non-self-contained question names a parent
--   BR65 non-self-contained hidden from standalone search
--   BR69 questionnaire question asked in >= 1 wave
--   BR80 scale has >= 1 question        BR87 questionnaire question has >= 1 variable
--   BR99 closed-ended has >= 1 option   BR106 option set has >= 1 answer option
-- BR5, BR23, BR42, BR45, BR53 and BR102 are the database's too (section 4d).
-- BR11 (a study has a questionnaire) and BR19 (a questionnaire has a wave) are no longer enforced:
-- a study's last questionnaire and a questionnaire's last wave can be deleted.
-- BR28, BR29, BR32, BR35 and BR36 described the old Wave Load / two-approver design; the draft
-- functions in section 4c now apply the rules of 2026-10-07 instead.
-- =====================================================================================================


-- =====================================================================================================
-- CHECKS OF THE DATABASE BUILD: the permissions, the draft tables and their functions create_draft(),
-- save_draft(), send_back_draft(), apply_draft(), approve_draft(), claim_draft(), release_draft(), the
-- review events they record, and what applying does to the MATR tables, the applied draft and applied_item.
-- =====================================================================================================
--
-- Everything runs in one transaction that is rolled back at the end, and it makes up its own rows
-- (registry "ZZ Test Registry"...), so nothing stays.
-- It stops at the first failed check and says which, and the last thing it shows is
-- "All checks passed.". It has no psql commands, so the Management API can run it as it stands.

begin;

-- ------ Helpers --------------------------------------------------------------------------

-- Runs `statement` and checks that it fails with a message matching `pattern`, and that it
-- leaves nothing behind (the failed part of a transaction is rolled back).
create function pg_temp.expect_error(statement text, pattern text) returns void
language plpgsql as $$
begin
  begin
    execute statement;
  exception when others then
    if sqlerrm !~ pattern then
      raise exception 'Expected an error matching "%", got: %', pattern, sqlerrm;
    end if;
    return;
  end;
  raise exception 'Expected an error matching "%", but it went through: %', pattern, statement;
end;
$$;

-- An edit by this person, with these items. Returns its id.
create function pg_temp.edit(prepared_by bigint, action text, items jsonb, status text default 'Awaiting Review') returns bigint
language sql as $$
  select public.create_draft(
    'Edit', prepared_by,
    jsonb_build_object('status', status, 'summary', 'Test edit'),
    jsonb_build_object('edit_kind', initcap(action), 'form', jsonb_build_object('action', action), 'items', items));
$$;

-- One item, as the site writes it: the row's key as compact JSON, values as text.
create function pg_temp.item(n int, entity text, key jsonb, action text, attribute text, old_value text, new_value text) returns jsonb
language sql as $$
  select jsonb_build_object('item_number', n, 'entity_name', entity,
                            'record_identifier', replace(replace(key::text, ': ', ':'), ', ', ','),
                            'action', action, 'attribute_name', attribute, 'old_value', old_value, 'new_value', new_value);
$$;

create temp table ctx (name text primary key, id bigint);
create function pg_temp.id(n text) returns bigint language sql as $$ select id from ctx where name = n; $$;

-- ------ Data of its own --------------------------------------------------------------------
insert into public.staff (name, email, active, role) values
  ('Test Ana', 'ana@drafts.test', true, 'STAFF'),
  ('Test Ben', 'ben@drafts.test', true, 'STAFF'),
  ('Test Cy', 'cy@drafts.test', true, 'STAFF'),
  ('Test Dee', 'dee@drafts.test', false, 'STAFF'),
  ('Test Ada', 'ada@drafts.test', true, 'ADMIN');
insert into ctx select 'ana', staff_id from public.staff where email = 'ana@drafts.test';
insert into ctx select 'ben', staff_id from public.staff where email = 'ben@drafts.test';
insert into ctx select 'cy', staff_id from public.staff where email = 'cy@drafts.test';
insert into ctx select 'dee', staff_id from public.staff where email = 'dee@drafts.test';
insert into ctx select 'ada', staff_id from public.staff where email = 'ada@drafts.test';

insert into public.registry (name, abbreviation, participant_count) values ('ZZ Test Registry', 'ZZT', 10);
insert into ctx select 'registry', registry_id from public.registry where name = 'ZZ Test Registry';
insert into public.principal_investigator (name, email) values ('ZZ Test PI', 'pi@drafts.test');
insert into ctx select 'pi', principal_investigator_id from public.principal_investigator where email = 'pi@drafts.test';
insert into public.study (registry_id, principal_investigator_id, name, status)
  select pg_temp.id('registry'), pg_temp.id('pi'), 'ZZ Test Study', 'Active';
insert into ctx select 'study', study_id from public.study where name = 'ZZ Test Study';
insert into public.respondent_type (name, minimum_age, maximum_age) values ('ZZ Test Adults', 18, 99);
insert into ctx select 'respondent_type', respondent_type_id from public.respondent_type where name = 'ZZ Test Adults';
insert into public.questionnaire (study_id, respondent_type_id, name, abbreviation)
  select pg_temp.id('study'), pg_temp.id('respondent_type'), 'ZZ Test Questionnaire', 'ZZQ';
insert into ctx select 'questionnaire', questionnaire_id from public.questionnaire where name = 'ZZ Test Questionnaire';
insert into public.wave (questionnaire_id, name, respondent_count) select pg_temp.id('questionnaire'), 'Wave 1', 100;
insert into public.wave (questionnaire_id, name, respondent_count) select pg_temp.id('questionnaire'), 'Wave 2', 90;
insert into ctx select 'wave1', wave_id from public.wave where questionnaire_id = pg_temp.id('questionnaire') and name = 'Wave 1';
insert into ctx select 'wave2', wave_id from public.wave where questionnaire_id = pg_temp.id('questionnaire') and name = 'Wave 2';
-- The real Section type (section 7): only a Section or a Group may hold other blocks (M3)
insert into ctx select 'section_type', component_type_id from public.component_type where name = 'Section';
with made as (
  insert into public.component (component_type_id, text) select pg_temp.id('section_type'), 'Section A' returning component_id
)
insert into ctx select 'section_a', component_id from made;
with placed as (
  insert into public.questionnaire_component (questionnaire_id, component_id, display_order)
    select pg_temp.id('questionnaire'), pg_temp.id('section_a'), 1 returning placement_id
)
insert into ctx select 'section_a_place', placement_id from placed;

-- ------ The migrations themselves ---------------------------------------------------------
do $$
begin
  -- staff roles
  assert (select data_type from information_schema.columns where table_name = 'staff' and column_name = 'role') = 'text', 'staff.role';
  assert (select role from public.staff where staff_id = pg_temp.id('ana')) = 'STAFF', 'the default role is STAFF';
  assert (select role from public.staff where staff_id = pg_temp.id('ada')) = 'ADMIN', 'an ADMIN can be made';
  perform pg_temp.expect_error('insert into public.staff (name, email, role) values (''X'', ''x@drafts.test'', ''BOSS'')', 'staff_role_check');
  perform pg_temp.expect_error('insert into public.staff (name, email, role) values (''X'', ''x@drafts.test'', null)', 'null value');
  -- study status
  perform pg_temp.expect_error(format('update public.study set status = ''Ongoing'' where study_id = %s', pg_temp.id('study')), 'study_status_check');
  update public.study set status = 'Ended' where study_id = pg_temp.id('study');
  update public.study set status = 'Active' where study_id = pg_temp.id('study');
  -- the old designs are gone, and so are the columns the migrations up to 2026-10-20 removed
  assert not exists (select 1 from information_schema.columns where table_schema = 'public' and (
           (table_name = 'draft' and column_name in ('note', 'sent_back_by', 'sent_back_at', 'sent_back_note',
                                                     'submitted_at', 'approved_by', 'approved_at'))
        or (table_name = 'extracted_draft' and column_name in ('ai_original', 'review_edits', 'reading_status',
              'reading_started', 'reading_claimed_at', 'reading_pid', 'reading_error', 'model', 'prompt_version',
              'pdf_pages', 'pdf_bytes'))
        or (table_name = 'response_option' and column_name in ('printed', 'printed_label'))
        or (table_name = 'variable' and column_name = 'respondent_count'))), 'no column the migrations removed';
  assert to_regclass('public.pending_draft') is null and to_regclass('public.pending_draft_approval') is null, 'pending_draft is gone';
  assert to_regclass('public.draft_approval') is null and to_regclass('public.write_history') is null
     and to_regclass('public.write_history_item') is null, 'the applied draft is the history: no separate tables';
  assert to_regprocedure('public.apply_pending_edit(bigint)') is null, 'and its functions';
  -- every table's own ID is a random 9-digit number the database picks
  assert (select count(*) from pg_attrdef ad join pg_class c on c.oid = ad.adrelid
          where c.relnamespace = 'public'::regnamespace and pg_get_expr(ad.adbin, ad.adrelid) like '%random_id(%') = 19, 'nineteen tables number their rows (a placement, a question stem and a time frame are three of them)';
  assert not exists (select 1 from information_schema.columns where table_schema = 'public' and is_identity = 'YES'), 'none counts 1, 2, 3';
  assert (select bool_and(staff_id between 100000000 and 999999999) from public.staff), 'staff IDs have 9 digits';
  assert (select bool_and(component_type_id between 100000000 and 999999999) from public.component_type), 'and so do the starting component types';
  assert pg_temp.id('registry') between 100000000 and 999999999 and pg_temp.id('wave1') between 100000000 and 999999999, 'and the rows made here';
  -- the helpers are out of the REST API's reach
  assert to_regprocedure('public.apply_draft_items(public.draft, jsonb, bigint)') is null, 'apply_draft_items is not in public';
  assert to_regprocedure('private.apply_draft_items(public.draft, jsonb, bigint)') is not null, 'it is in private';
  assert to_regprocedure('public.draft_set_details(text, bigint, jsonb, boolean)') is null
     and to_regprocedure('public.draft_set_parent(bigint, jsonb)') is null, 'and so are the draft_set_ helpers';
  -- the dashboard reads subject, and can't change it
  assert exists (select 1 from information_schema.columns where table_name = 'variable' and column_name = 'subject_id'), 'variable.subject_id';
  assert has_table_privilege('anon', 'public.subject', 'select'), 'the dashboard reads subject';
  assert not has_table_privilege('anon', 'public.subject', 'insert, update, delete'), 'but can''t change it';
  assert (select count(*) from pg_policies where tablename = 'subject' and policyname = 'Dashboard can read') = 1, 'with the one policy the other tables have';
  -- a subject's name has straight quotes
  perform pg_temp.expect_error($q$insert into public.subject (name) values ('ZZ Respondent’s Cousin')$q$, 'subject_name_straight_quotes');
  -- and time frames the same way
  assert exists (select 1 from information_schema.columns where table_name = 'variable' and column_name = 'time_frame_id'), 'variable.time_frame_id';
  assert has_table_privilege('anon', 'public.time_frame', 'select'), 'the dashboard reads time_frame';
  assert not has_table_privilege('anon', 'public.time_frame', 'insert, update, delete'), 'but can''t change it';
end;
$$;

-- ------ Who can see what ---------------------------------------------------------------------
do $$
declare t text;
begin
  foreach t in array array['staff', 'applied_item', 'draft', 'extracted_draft', 'edit_draft',
                           'ai_extracted_draft', 'draft_review_event'] loop
    assert not has_table_privilege('anon', 'public.' || t, 'select, insert, update, delete'), t || ' is private to anon';
    assert not has_table_privilege('authenticated', 'public.' || t, 'select, insert, update, delete'), t || ' is private to signed-in users';
    assert has_table_privilege('service_role', 'public.' || t, 'select, insert, update, delete'), t || ' is the site''s';
    assert (select relrowsecurity from pg_class where oid = ('public.' || t)::regclass), t || ' has Row Level Security on';
  end loop;
  assert has_function_privilege('service_role', 'private.random_id(text, text)', 'execute'), 'the site may number new rows';

  assert has_function_privilege('service_role', 'public.create_draft(text, bigint, jsonb, jsonb)', 'execute'), 'the site may create';
  assert has_function_privilege('service_role', 'public.save_draft(bigint, bigint, jsonb, jsonb, integer, boolean)', 'execute'), 'save';
  assert has_function_privilege('service_role', 'public.apply_draft(bigint, bigint, jsonb)', 'execute'), 'apply';
  assert has_function_privilege('service_role', 'public.approve_draft(bigint, integer, bigint, jsonb)', 'execute'), 'approve';
  assert has_function_privilege('service_role', 'public.send_back_draft(bigint, integer, bigint, text)', 'execute'), 'and send back';
  assert has_function_privilege('service_role', 'public.claim_draft(bigint, bigint, boolean, integer)', 'execute')
     and has_function_privilege('service_role', 'public.release_draft(bigint, bigint)', 'execute'), 'claim and let go';
  assert not has_function_privilege('anon', 'public.claim_draft(bigint, bigint, boolean, integer)', 'execute')
     and not has_function_privilege('authenticated', 'public.release_draft(bigint, bigint)', 'execute'), 'the dashboard can''t claim';
  assert not has_function_privilege('anon', 'public.send_back_draft(bigint, integer, bigint, text)', 'execute')
     and not has_function_privilege('authenticated', 'public.send_back_draft(bigint, integer, bigint, text)', 'execute'), 'the dashboard can''t send back';
  -- the dashboard reads every MATR table, with one policy each, and writes none
  foreach t in array array['registry', 'principal_investigator', 'study', 'respondent_type', 'questionnaire', 'wave',
    'component_type', 'component', 'question', 'questionnaire_component', 'questionnaire_question', 'wave_question',
    'subject', 'time_frame', 'variable', 'open_ended', 'closed_ended', 'option_set', 'answer_option', 'answer_option_set',
    'response_option', 'response_option_count', 'category', 'question_category', 'measurement_scale', 'question_scale',
    'question_stem', 'answer_option_alias', 'wave_file', 'pdf_file'] loop
    assert has_table_privilege('anon', 'public.' || t, 'select'), t || ' is readable by the dashboard';
    assert not has_table_privilege('anon', 'public.' || t, 'insert, update, delete, truncate'), t || ' is read-only to the dashboard';
    assert (select count(*) from pg_policies where schemaname = 'public' and tablename = t) = 1, t || ' has its one policy';
    assert has_table_privilege('service_role', 'public.' || t, 'select, insert, update, delete'), t || ' is the site''s to change';
  end loop;
  assert (select count(*) from pg_class where relnamespace = 'public'::regnamespace and relkind = 'r') = 37, 'thirty-seven tables';
  assert not exists (select 1 from pg_class where relnamespace = 'public'::regnamespace and relkind = 'r' and not relrowsecurity), 'RLS on all of them';
  assert (select public from storage.buckets where id = 'extraction-pdfs'), 'the PDFs'' bucket is public';
  assert not exists (select 1 from pg_policies where schemaname = 'storage' and tablename = 'objects'
                     and roles && array['anon', 'authenticated', 'public']::name[] and cmd <> 'SELECT'
                     and (qual like '%extraction-pdfs%' or with_check like '%extraction-pdfs%')), 'and only the site writes to it';
  assert has_function_privilege('anon', 'public.upvote_questionnaire(bigint)', 'execute'), 'the dashboard may upvote';
  assert not has_function_privilege('anon', 'public.draft_needs_approval(bigint)', 'execute'), 'but not ask about drafts';
  assert not has_schema_privilege('anon', 'private', 'usage') and not has_schema_privilege('authenticated', 'private', 'usage'), 'private is private';
  assert has_function_privilege('service_role', 'private.apply_draft_items(public.draft, jsonb, bigint)', 'execute'), 'the site''s functions can use the helpers';

  -- a table made later is private until it is shared on purpose
  create table public.zz_later (id integer);
  assert not has_table_privilege('anon', 'public.zz_later', 'select'), 'a new table is hidden from the dashboard';
  assert has_table_privilege('service_role', 'public.zz_later', 'select, insert, update, delete'), 'and open to the site';
  drop table public.zz_later;

  set local role anon;
  perform pg_temp.expect_error('select public.apply_draft(1, 1)', 'permission denied');
  perform pg_temp.expect_error('select private.apply_draft_items(null::public.draft, ''[]''::jsonb, null)', 'permission denied');
  perform pg_temp.expect_error('select * from public.staff', 'permission denied');
  perform pg_temp.expect_error('insert into public.subject (name) values (''x'')', 'permission denied');
  perform pg_temp.expect_error('select public.approve_draft(1, 1, 1)', 'permission denied');
  perform pg_temp.expect_error('select public.create_draft(''Edit'', 1)', 'permission denied');
  perform pg_temp.expect_error('select * from public.draft', 'permission denied');
  set local role authenticated;
  perform pg_temp.expect_error('select public.apply_draft(1, 1)', 'permission denied');
  perform pg_temp.expect_error('select public.save_draft(1, 1)', 'permission denied');
  reset role;
end;
$$;

-- ------ create_draft and the draft tables ----------------------------------------------------------
do $$
declare
  e bigint;
  x bigint;
begin
  e := pg_temp.edit(pg_temp.id('ana'), 'update', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave2')), 'Update', 'name', 'Wave 2', 'Round 2')), 'Draft');
  assert (select status from public.draft where draft_id = e) = 'Draft', 'a new draft is a draft';
  assert (select revision from public.draft where draft_id = e) = 1, 'at revision 1';
  assert (select saved_by from public.draft where draft_id = e) = pg_temp.id('ana'), 'saved by whoever made it';
  assert (select edit_kind from public.edit_draft where draft_id = e) = 'Update', 'edit_kind from the sentence';
  assert (select jsonb_array_length(items) from public.edit_draft where draft_id = e) = 1, 'items kept';

  -- an AI-Powered extraction: p_detail names columns of extracted_draft, ai_extracted_draft and pdf_file alike
  x := public.create_draft('Extraction', pg_temp.id('ana'), '{}'::jsonb, jsonb_build_object(
    'method', 'AI-Powered', 'reading_status', 'queued', 'pdf_sha256', 'abc', 'pdf_name', 'a.pdf',
    'pdf_pages', 3, 'pdf_bytes', 1000, 'info', jsonb_build_object('wave_id', 1)));
  assert (select method from public.extracted_draft where draft_id = x) = 'AI-Powered', 'an extraction''s details';
  assert (select pdf_name from public.extracted_draft where draft_id = x) = 'a.pdf', 'the name it was uploaded under';
  assert (select reading_status from public.ai_extracted_draft where draft_id = x) = 'queued', 'its reading';
  assert (select review_edits from public.ai_extracted_draft where draft_id = x) = '[]'::jsonb, 'no edits yet';
  assert (select ai_original from public.ai_extracted_draft where draft_id = x) is null, 'and no AI answer until the job saves one';
  assert (select pdf_pages = 3 and pdf_bytes = 1000 from public.pdf_file where pdf_sha256 = 'abc'), 'the PDF, once';
  -- the reading job saves its progress the same way
  perform public.save_draft(x, pg_temp.id('ana'), '{}'::jsonb,
    jsonb_build_object('reading_status', 'ready', 'ai_original', jsonb_build_object('blocks', '[]'::jsonb), 'model', 'm'), null, false);
  assert (select reading_status = 'ready' and model = 'm' and ai_original is not null from public.ai_extracted_draft where draft_id = x), 'the reading moved on';
  -- the same file again: one pdf_file row, and what it knew stays
  perform public.create_draft('Extraction', pg_temp.id('ben'), '{}'::jsonb, jsonb_build_object(
    'method', 'AI-Powered', 'reading_status', 'queued', 'pdf_sha256', 'abc', 'pdf_name', 'b.pdf', 'pdf_pages', 4));
  assert (select count(*) from public.pdf_file where pdf_sha256 = 'abc') = 1
     and (select pdf_pages from public.pdf_file where pdf_sha256 = 'abc') = 3, 'a known PDF keeps what it has';
  perform pg_temp.expect_error('update public.pdf_file set pdf_pages = 5 where pdf_sha256 = ''abc''', 'facts about the file');
  -- a Manual one
  x := public.create_draft('Extraction', pg_temp.id('ana'), '{}'::jsonb, jsonb_build_object('method', 'Manual'));
  assert not exists (select 1 from public.ai_extracted_draft where draft_id = x), 'a manual extraction has no reading';

  -- what it refuses
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{}''::jsonb, ''{"edit_kind":"Add"}''::jsonb)', pg_temp.id('dee')), 'Only active staff can prepare');
  perform pg_temp.expect_error('select public.create_draft(''Edit'', 0, ''{}''::jsonb, ''{"edit_kind":"Add"}''::jsonb)', 'Only active staff can prepare');
  perform pg_temp.expect_error(format('select public.create_draft(''Thing'', %s)', pg_temp.id('ana')), 'Extraction or an Edit');
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{}''::jsonb, ''{"edit_kind":"Add","nope":1}''::jsonb)', pg_temp.id('ana')), 'edit_draft has no column nope');
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{"owner":1}''::jsonb, ''{"edit_kind":"Add"}''::jsonb)', pg_temp.id('ana')), 'can''t be set this way');
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{}''::jsonb, ''{"edit_kind":"Fix"}''::jsonb)', pg_temp.id('ana')), 'edit_kind_check');
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{}''::jsonb, ''{}''::jsonb)', pg_temp.id('ana')), 'null value');
  perform pg_temp.expect_error(format('select public.create_draft(''Extraction'', %s, ''{}''::jsonb, ''{"method":"AI-Powered"}''::jsonb)', pg_temp.id('ana')), 'extracted_draft_ai_has_pdf');
  perform pg_temp.expect_error(format('select public.create_draft(''Extraction'', %s, ''{}''::jsonb, ''{"method":"AI-Powered","pdf_sha256":"abc"}''::jsonb)', pg_temp.id('ana')), 'reading_status.*null value|null value.*reading_status');
  perform pg_temp.expect_error(format('select public.create_draft(''Extraction'', %s, ''{}''::jsonb, ''{"method":"Manual","reading_status":"queued"}''::jsonb)', pg_temp.id('ana')), 'A Manual extraction has no reading');
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{"requested_reviewer":%s}''::jsonb, ''{"edit_kind":"Add"}''::jsonb)', pg_temp.id('ana'), pg_temp.id('ana')), 'draft_reviewer_is_someone_else');
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{"requested_reviewer":%s}''::jsonb, ''{"edit_kind":"Add"}''::jsonb)', pg_temp.id('ana'), pg_temp.id('dee')), 'Only active staff can be asked');
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{"status":"Applied"}''::jsonb, ''{"edit_kind":"Add"}''::jsonb)', pg_temp.id('ana')), 'applied by applying it');
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{"note":"why"}''::jsonb, ''{"edit_kind":"Add"}''::jsonb)', pg_temp.id('ana')), 'note can''t be set this way');
  perform pg_temp.expect_error(format('select public.create_draft(''Edit'', %s, ''{"submitted_at":"2026-01-01"}''::jsonb, ''{"edit_kind":"Add"}''::jsonb)', pg_temp.id('ana')), 'submitted_at can''t be set this way');

  -- one made as a change request was submitted, by whoever made it
  e := pg_temp.edit(pg_temp.id('ana'), 'delete', '[]'::jsonb);
  assert (select count(*) from public.draft_review_event where draft_id = e and event = 'Submitted' and staff_id = pg_temp.id('ana')) = 1, 'a Submitted event';
  e := pg_temp.edit(pg_temp.id('ana'), 'add', '[]'::jsonb, 'Draft');
  assert not exists (select 1 from public.draft_review_event where draft_id = e), 'a plain draft has none';

  -- a draft with no details can't be left behind (checked when the transaction ends, so it is run here as that would)
  begin
    insert into public.draft (kind, prepared_by, saved_by) values ('Edit', pg_temp.id('ana'), pg_temp.id('ana'));
    set constraints draft_has_details immediate;
    raise exception 'went through';
  exception when others then
    assert sqlerrm like '%needs its details%', 'a draft with no details is refused: ' || sqlerrm;
  end;
end;
$$;

-- a subtype row can only join a draft of its own kind
do $$
declare e bigint := pg_temp.edit(pg_temp.id('ana'), 'add', '[]'::jsonb, 'Draft');
begin
  perform pg_temp.expect_error(format('insert into public.extracted_draft (draft_id, method) values (%s, ''Manual'')', e), 'extracted_draft_draft_fkey');
end;
$$;

-- ------ save_draft -------------------------------------------------------------------------------------
do $$
declare
  e bigint := pg_temp.edit(pg_temp.id('ana'), 'update', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave2')), 'Update', 'name', 'Wave 2', 'Round 2')), 'Draft');
  r integer;
begin
  r := public.save_draft(e, pg_temp.id('ana'), jsonb_build_object('summary', 'Reworded'),
                         jsonb_build_object('form', jsonb_build_object('action', 'update', 'note', 'x')), 1);
  assert r = 2, 'saving raises the revision';
  assert (select summary from public.draft where draft_id = e) = 'Reworded', 'the parent changed';
  assert (select form ->> 'note' from public.edit_draft where draft_id = e) = 'x', 'and the details';
  assert (select jsonb_array_length(items) from public.edit_draft where draft_id = e) = 1, 'a column not mentioned keeps its value';
  -- someone else saves: it is theirs now
  r := public.save_draft(e, pg_temp.id('ben'), '{}'::jsonb, jsonb_build_object('reach', jsonb_build_object('rows_removed', 0)), 2);
  assert r = 3 and (select saved_by from public.draft where draft_id = e) = pg_temp.id('ben'), 'saved_by follows the last save';
  assert (select prepared_by from public.draft where draft_id = e) = pg_temp.id('ana'), 'prepared_by never moves';
  -- a change that is not to the content
  r := public.save_draft(e, pg_temp.id('ana'), '{}'::jsonb, '{}'::jsonb, null, false);
  assert r = 3, 'a save that is not to the content leaves the revision';
  -- a null clears a column
  perform public.save_draft(e, pg_temp.id('ana'), jsonb_build_object('summary', null), '{}'::jsonb, null, false);
  assert (select summary from public.draft where draft_id = e) is null, 'null clears';

  perform pg_temp.expect_error(format('select public.save_draft(%s, %s, ''{}''::jsonb, ''{}''::jsonb, 1)', e, pg_temp.id('ana')), 'changed after you opened it');
  perform pg_temp.expect_error(format('select public.save_draft(%s, %s)', e, pg_temp.id('dee')), 'Only active staff');
  perform pg_temp.expect_error(format('select public.save_draft(0, %s)', pg_temp.id('ana')), 'no longer in the database');
  perform pg_temp.expect_error(format('select public.save_draft(%s, %s, ''{}''::jsonb, ''{"nope":1}''::jsonb)', e, pg_temp.id('ana')), 'edit_draft has no column nope');
  perform pg_temp.expect_error(format('select public.save_draft(%s, %s, ''{"status":"Applied"}''::jsonb)', e, pg_temp.id('ana')), 'applied by applying it');
  perform pg_temp.expect_error(format('select public.save_draft(%s, %s, ''{"requested_reviewer":%s}''::jsonb)', e, pg_temp.id('ana'), pg_temp.id('dee')), 'Only active staff can be asked');
end;
$$;

-- statuses move one way
do $$
declare
  e bigint := pg_temp.edit(pg_temp.id('ana'), 'delete', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave1')), 'Delete', 'name', 'Wave 1', null)));
begin
  -- a change waiting for review can't go back to a plain draft
  perform pg_temp.expect_error(format('select public.save_draft(%s, %s, ''{"status":"Draft"}''::jsonb)', e, pg_temp.id('ana')), 'can''t go back to being a plain draft');
  -- sending back is send_back_draft()'s, which says who and why
  perform pg_temp.expect_error(format('select public.save_draft(%s, %s, ''{"status":"Sent Back"}''::jsonb)', e, pg_temp.id('ben')), 'send_back_draft');
  perform pg_temp.expect_error(format('select public.send_back_draft(%s, 2, %s, ''x'')', e, pg_temp.id('ben')), 'changed after you opened it');
  perform pg_temp.expect_error(format('select public.send_back_draft(%s, 1, %s, ''x'')', e, pg_temp.id('dee')), 'Only active staff');
  perform pg_temp.expect_error(format('select public.send_back_draft(0, 1, %s, ''x'')', pg_temp.id('ben')), 'no longer in the database');
  assert public.send_back_draft(e, 1, pg_temp.id('ben'), 'Which wave?') = 1, 'send_back_draft returns the revision';
  assert (select status from public.draft where draft_id = e) = 'Sent Back', 'sent back';
  assert (select note from public.draft_review_event where draft_id = e and event = 'Sent Back') = 'Which wave?', 'with the reviewer''s note';
  assert (select staff_id from public.draft_review_event where draft_id = e and event = 'Sent Back') = pg_temp.id('ben'), 'and who';
  assert (select saved_by from public.draft where draft_id = e) = pg_temp.id('ben'), 'the reviewer saved it last';
  assert (select revision from public.draft where draft_id = e) = 1, 'sending back is not a change to the content';
  -- only a send back has a note
  perform pg_temp.expect_error(format('insert into public.draft_review_event (draft_id, event, staff_id, note) values (%s, ''Submitted'', %s, ''x'')', e, pg_temp.id('ana')), 'draft_review_event_note_is_a_send_back');
  -- and a draft can't be marked Sent Back without one (checked when the transaction ends, so it is run here as that would)
  declare
    other bigint := pg_temp.edit(pg_temp.id('ana'), 'delete', '[]'::jsonb);
  begin
    begin
      update public.draft set status = 'Sent Back' where draft_id = other;
      set constraints draft_sent_back_says_who immediate;
      raise exception 'went through';
    exception when others then
      assert sqlerrm like '%says who sent it back%', 'a send back with no event is refused: ' || sqlerrm;
    end;
  end;
  -- a sent-back change is not waiting for approval
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s)', e, pg_temp.id('cy')), 'not waiting for approval \(it is sent back\)');
  -- the preparer fixes it and sends it again
  perform public.save_draft(e, pg_temp.id('ana'), jsonb_build_object('status', 'Awaiting Review'), '{}'::jsonb, 1);
  assert (select status from public.draft where draft_id = e) = 'Awaiting Review' and (select revision from public.draft where draft_id = e) = 2, 'sent again, as a new revision';
  assert (select array_agg(event order by event) from public.draft_review_event where draft_id = e)
       = array['Sent Back', 'Submitted', 'Submitted'], 'every submission and send back is kept';
  -- the preparer pulls it
  perform public.save_draft(e, pg_temp.id('ana'), jsonb_build_object('status', 'Withdrawn'), '{}'::jsonb, null, false);
  perform pg_temp.expect_error(format('select public.save_draft(%s, %s, ''{"status":"Awaiting Review"}''::jsonb)', e, pg_temp.id('ana')), 'withdrawn, so it can''t be changed');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 2, %s)', e, pg_temp.id('cy')), 'not waiting for approval \(it is withdrawn\)');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'withdrawn');
  perform pg_temp.expect_error(format('select public.send_back_draft(%s, 2, %s)', e, pg_temp.id('ben')), 'Only a change waiting for review can be sent back');
  -- only a change waiting for review can be sent back
  e := pg_temp.edit(pg_temp.id('ana'), 'add', '[]'::jsonb, 'Draft');
  perform pg_temp.expect_error(format('select public.send_back_draft(%s, 1, %s)', e, pg_temp.id('ben')), 'Only a change waiting for review can be sent back');
end;
$$;

-- ------ The rule: who needs approval ----------------------------------------------------------------------
do $$
declare
  upd bigint := pg_temp.edit(pg_temp.id('ana'), 'update', '[]'::jsonb, 'Draft');
  add bigint := pg_temp.edit(pg_temp.id('ana'), 'add', '[]'::jsonb, 'Draft');
  del bigint := pg_temp.edit(pg_temp.id('ana'), 'delete', '[]'::jsonb, 'Draft');
  adel bigint := pg_temp.edit(pg_temp.id('ada'), 'delete', '[]'::jsonb, 'Draft');
  ext bigint := public.create_draft('Extraction', pg_temp.id('ana'), '{}'::jsonb, jsonb_build_object('method', 'Manual'));
  aext bigint := public.create_draft('Extraction', pg_temp.id('ada'), '{}'::jsonb, jsonb_build_object('method', 'Manual'));
begin
  assert not public.draft_needs_approval(add), 'STAFF: an add needs none';
  assert not public.draft_needs_approval(upd), 'STAFF: an update needs none';
  assert public.draft_needs_approval(del), 'STAFF: a delete needs one';
  assert public.draft_needs_approval(ext), 'STAFF: pushing an extraction needs one';
  assert not public.draft_needs_approval(adel), 'ADMIN: a delete needs none';
  assert not public.draft_needs_approval(aext), 'ADMIN: an extraction needs none';
end;
$$;

-- ------ apply_draft: an update, at once -----------------------------------------------------------------
do $$
declare
  e bigint := pg_temp.edit(pg_temp.id('ana'), 'update', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave2')), 'Update', 'name', 'Wave 2', 'Round 2'),
    pg_temp.item(2, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave2')), 'Update', 'respondent_count', '90', null)), 'Draft');
  made bigint;
  h public.draft%rowtype;
begin
  -- only the person who prepared it
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ben')), 'Only the person who prepared');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('dee')), 'Only active staff');
  made := public.apply_draft(e, pg_temp.id('ana'));

  assert (select name from public.wave where wave_id = pg_temp.id('wave2')) = 'Round 2', 'the name changed';
  assert (select respondent_count from public.wave where wave_id = pg_temp.id('wave2')) is null, 'and the count was emptied, in the same statement';
  assert made = e, 'apply_draft returns the draft';
  select * into h from public.draft where draft_id = e;
  assert h.status = 'Applied' and h.applied_at is not null, 'history: applied, and when';
  assert h.prepared_by = pg_temp.id('ana') and not exists (select 1 from public.draft_review_event where draft_id = e and event = 'Approved'), 'history: no approver';
  assert h.summary = 'Test edit', 'history: summary';
  assert (select count(*) from public.applied_item where draft_id = made) = 2, 'history: both items';
  assert (select new_value from public.applied_item where draft_id = made and item_number = 1) = 'Round 2', 'history: new value';
  assert (select old_value from public.applied_item where draft_id = made and item_number = 2) = '90', 'history: old value';
  assert (select status from public.draft where draft_id = e) = 'Applied', 'the draft is applied';

  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'already been applied');
  perform pg_temp.expect_error(format('select public.save_draft(%s, %s)', e, pg_temp.id('ana')), 'already been applied');
  perform pg_temp.expect_error(format('update public.edit_draft set reach = ''{}'' where draft_id = %s', e), 'already been applied');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s)', e, pg_temp.id('cy')), 'not waiting for approval \(it is applied\)');
end;
$$;

-- ------ apply_draft: an add, with rows that have no number yet ------------------------------------------------
do $$
declare
  e bigint := pg_temp.edit(pg_temp.id('ben'), 'add', jsonb_build_array(
    pg_temp.item(1, 'component', jsonb_build_object('component_id', 'new:component:1'), 'Insert', 'component_type_id', null, pg_temp.id('section_type')::text),
    pg_temp.item(2, 'component', jsonb_build_object('component_id', 'new:component:1'), 'Insert', 'text', null, 'Section B'),
    pg_temp.item(3, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:1'), 'Insert', 'questionnaire_id', null, pg_temp.id('questionnaire')::text),
    pg_temp.item(4, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:1'), 'Insert', 'component_id', null, 'new:component:1'),
    pg_temp.item(5, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:1'), 'Insert', 'display_order', null, '2'),
    pg_temp.item(6, 'subject', jsonb_build_object('subject_id', 'new:subject:1'), 'Insert', 'name', null, 'ZZ Test Subject')), 'Draft');
  made bigint;
  new_component bigint;
  new_place bigint;
begin
  made := public.apply_draft(e, pg_temp.id('ben'));
  select component_id into new_component from public.component where text = 'Section B';
  assert new_component between 100000000 and 999999999, 'the component was added, with a random 9-digit ID';
  select placement_id into new_place from public.questionnaire_component
  where questionnaire_id = pg_temp.id('questionnaire') and component_id = new_component and display_order = 2;
  assert new_place is not null, 'the placement points at the number the database gave the new component';
  assert (select count(*) from public.subject where name = 'ZZ Test Subject') = 1, 'the subject was added';
  assert (select edit_kind from public.edit_draft where draft_id = made) = 'Add', 'kind Add';
  assert (select record_identifier from public.applied_item where draft_id = made and item_number = 3)
    = format('{"placement_id":%s}', new_place), 'history: the real number in the key';
  assert not exists (select 1 from public.applied_item where draft_id = made and record_identifier like '%new:%'), 'no placeholder is left';
  insert into ctx values ('section_b', new_component), ('section_b_place', new_place);
end;
$$;

-- ------ apply_draft: a change that points an existing row at a new one ---------------------------------------
-- Like changing a section's words: its placement is pointed at a new component; what is inside stays inside it,
-- and the old wording, printed nowhere else now, goes.
do $$
declare
  edit bigint;
  child bigint;
  child_place bigint;
  made bigint;
  new_component bigint;
begin
  insert into public.component (component_type_id, text) values (pg_temp.id('section_type'), 'Child') returning component_id into child;
  insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order)
    values (pg_temp.id('questionnaire'), child, pg_temp.id('section_b_place'), 1) returning placement_id into child_place;
  edit := pg_temp.edit(pg_temp.id('cy'), 'update', jsonb_build_array(
    pg_temp.item(1, 'component', jsonb_build_object('component_id', 'new:component:1'), 'Insert', 'component_type_id', null, pg_temp.id('section_type')::text),
    pg_temp.item(2, 'component', jsonb_build_object('component_id', 'new:component:1'), 'Insert', 'text', null, 'Section B, reworded'),
    pg_temp.item(3, 'questionnaire_component', jsonb_build_object('placement_id', pg_temp.id('section_b_place')), 'Update', 'component_id', pg_temp.id('section_b')::text, 'new:component:1')),
    'Draft');
  made := public.apply_draft(edit, pg_temp.id('cy'));
  set constraints component_left_unplaced immediate;
  set constraints component_left_unplaced deferred;

  select component_id into new_component from public.component where text = 'Section B, reworded';
  assert (select component_id from public.questionnaire_component where placement_id = pg_temp.id('section_b_place')) = new_component,
    'the placement now prints the new wording';
  assert (select parent_placement_id from public.questionnaire_component where placement_id = child_place) = pg_temp.id('section_b_place'),
    'and what was inside it is still inside it';
  assert not exists (select 1 from public.component where component_id = pg_temp.id('section_b')), 'the old wording, printed nowhere, is gone';
  assert (select new_value from public.applied_item where draft_id = made and item_number = 3) = new_component::text, 'history: the real number as the new value';
end;
$$;

-- ------ A STAFF delete: a change request, one approval, then it is applied ----------------------------------------
do $$
declare
  e bigint := pg_temp.edit(pg_temp.id('ana'), 'delete', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave1')), 'Delete', 'questionnaire_id', pg_temp.id('questionnaire')::text, null),
    pg_temp.item(2, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave1')), 'Delete', 'name', 'Wave 1', null),
    pg_temp.item(3, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave1')), 'Delete', 'respondent_count', '100', null),
    pg_temp.item(4, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave1')), 'Delete', 'administered_date', null, null)));
  made bigint;
begin
  -- not without approval, not even by its preparer
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'needs approval from another staff member');
  assert exists (select 1 from public.wave where wave_id = pg_temp.id('wave1')), 'nothing was deleted';
  -- who may approve
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s)', e, pg_temp.id('ana')), 'You prepared this');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s)', e, pg_temp.id('dee')), 'Only active staff');
  perform pg_temp.expect_error(format('select public.approve_draft(0, 1, %s)', pg_temp.id('ben')), 'no longer in the database');
  -- what was read must be what is approved
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 2, %s)', e, pg_temp.id('ben')), 'changed after you opened it');
  -- someone who changed it has made it theirs
  perform public.save_draft(e, pg_temp.id('ben'), '{}'::jsonb, '{}'::jsonb, 1);
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 2, %s)', e, pg_temp.id('ben')), 'You made the latest changes');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 2, %s)', e, pg_temp.id('ana')), 'You prepared this');
  assert not exists (select 1 from public.draft_review_event where draft_id = e and event = 'Approved')
     and not exists (select 1 from public.applied_item where draft_id = e), 'refusals leave no approval behind';

  -- Cy approves, and it is applied in the same step
  made := public.approve_draft(e, 2, pg_temp.id('cy'));
  assert not exists (select 1 from public.wave where wave_id = pg_temp.id('wave1')), 'the wave is gone';
  assert (select status from public.draft where draft_id = e) = 'Applied', 'the request is applied';
  assert (select revision from public.draft where draft_id = e) = 2, 'the draft keeps the revision that was approved';
  assert made = e, 'approve_draft returns the draft';
  assert (select staff_id from public.draft_review_event where draft_id = made and event = 'Approved') = pg_temp.id('cy'), 'history: the approver';
  assert (select prepared_by from public.draft where draft_id = made) = pg_temp.id('ana'), 'history: the preparer';
  assert (select applied_at is not null from public.draft where draft_id = made)
     and exists (select 1 from public.draft_review_event where draft_id = made and event = 'Approved'), 'history: when';
  -- a delete keeps one line, the row deleted, and nothing it held: not even the items it was applied from
  assert (select count(*) from public.applied_item where draft_id = made) = 1, 'history: one line for a delete';
  assert (select entity_name || ' ' || record_identifier || ' ' || action || ' ' || coalesce(attribute_name, old_value, '(nothing)')
          from public.applied_item where draft_id = made) = format('wave {"wave_id":%s} Delete (nothing)', pg_temp.id('wave1')),
    'history: the wave, and nothing it held';
  assert (select items from public.edit_draft where draft_id = made) = '[]'::jsonb, 'the delete''s items are let go';
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 2, %s)', e, pg_temp.id('ben')), 'not waiting for approval \(it is applied\)');
end;
$$;

-- an ADMIN's delete needs no approval; an ADMIN may approve a STAFF member's
do $$
declare
  e bigint;
  made bigint;
  wid bigint;
begin
  insert into public.wave (questionnaire_id, name) values (pg_temp.id('questionnaire'), 'Wave 3') returning wave_id into wid;
  e := pg_temp.edit(pg_temp.id('ada'), 'delete', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', wid), 'Delete', 'name', 'Wave 3', null)), 'Draft');
  -- the row picked, as the site names it
  update public.edit_draft set reach = jsonb_build_object('rows_removed', 0, 'rows_emptied', 0,
    'deleted', jsonb_build_object('table', 'wave', 'row', format('{"wave_id":%s}', wid))) where draft_id = e;
  made := public.apply_draft(e, pg_temp.id('ada'));
  assert not exists (select 1 from public.wave where wave_id = wid), 'the ADMIN deleted it at once';
  assert (select entity_name || ' ' || record_identifier from public.applied_item where draft_id = made) = format('wave {"wave_id":%s}', wid),
    'history: the row picked';
  assert not exists (select 1 from public.draft_review_event where draft_id = made and event = 'Approved'), 'with no approver';

  insert into public.wave (questionnaire_id, name) values (pg_temp.id('questionnaire'), 'Wave 4') returning wave_id into wid;
  e := pg_temp.edit(pg_temp.id('ben'), 'delete', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', wid), 'Delete', 'name', 'Wave 4', null)));
  made := public.approve_draft(e, 1, pg_temp.id('ada'));
  assert not exists (select 1 from public.wave where wave_id = wid), 'an ADMIN approved the STAFF member''s delete';
  assert (select staff_id from public.draft_review_event where draft_id = made and event = 'Approved') = pg_temp.id('ada'), 'history: the ADMIN approved';
end;
$$;

-- ------ apply: the rest of what it checks -----------------------------------------------------------------------
do $$
declare
  e bigint;
begin
  -- an update with the wrong old value is refused as a whole and leaves nothing behind
  e := pg_temp.edit(pg_temp.id('ana'), 'update', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave2')), 'Update', 'name', 'Wave 2', 'Other')), 'Draft');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'is now "Round 2", not "Wave 2"');
  assert not exists (select 1 from public.applied_item where draft_id = e), 'a refused change writes no history';
  assert (select status from public.draft where draft_id = e) = 'Draft', 'and stays a draft';

  -- an edit with nothing in it
  e := pg_temp.edit(pg_temp.id('ana'), 'update', '[]'::jsonb, 'Draft');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'nothing in this to apply');

  -- a table that edits can't change
  e := pg_temp.edit(pg_temp.id('ana'), 'update', jsonb_build_array(
    pg_temp.item(1, 'staff', jsonb_build_object('staff_id', pg_temp.id('dee')), 'Update', 'active', 'false', 'true')), 'Draft');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'can''t change \(staff\)');
  assert (select active from public.staff where staff_id = pg_temp.id('dee')) = false, 'staff untouched';

  -- a column that isn't there
  e := pg_temp.edit(pg_temp.id('ana'), 'update', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave2')), 'Update', 'nope', 'x', 'y')), 'Draft');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'wave has no column nope');

  -- a row that isn't there any more
  e := pg_temp.edit(pg_temp.id('ana'), 'update', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave1')), 'Update', 'name', 'Wave 1', 'Other')), 'Draft');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'no longer in the database, so this change is out of date');

  -- a new row can't pick its own ID: the database picks a random one
  e := pg_temp.edit(pg_temp.id('ana'), 'add', jsonb_build_array(
    pg_temp.item(1, 'subject', jsonb_build_object('subject_id', 7), 'Insert', 'name', null, 'ZZ Seven')), 'Draft');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'can''t choose its own subject_id');

  -- a new row mentioned before it is added
  e := pg_temp.edit(pg_temp.id('ana'), 'add', jsonb_build_array(
    pg_temp.item(1, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:1'), 'Insert', 'component_id', null, 'new:component:9')), 'Draft');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'before that row is added');

  -- a rule of the database (a wave name can't repeat) stops the whole change
  e := pg_temp.edit(pg_temp.id('ana'), 'add', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', 'new:wave:1'), 'Insert', 'questionnaire_id', null, pg_temp.id('questionnaire')::text),
    pg_temp.item(2, 'wave', jsonb_build_object('wave_id', 'new:wave:1'), 'Insert', 'name', null, 'Round 2')), 'Draft');
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s)', e, pg_temp.id('ana')), 'wave_name_unique_in_questionnaire');
  assert (select count(*) from public.wave where questionnaire_id = pg_temp.id('questionnaire') and name = 'Round 2') = 1, 'no second Round 2';
end;
$$;

-- ------ An extraction: only adds, to the tables a questionnaire is made of ----------------------------------------
do $$
declare
  items jsonb;
  e bigint;
  made bigint;
  q bigint;
  w bigint;
  v bigint;
begin
  -- a questionnaire, its wave, one section holding one question with one write-in variable
  items := jsonb_build_array(
    pg_temp.item(1, 'questionnaire', jsonb_build_object('questionnaire_id', 'new:questionnaire:1'), 'Insert', 'study_id', null, pg_temp.id('study')::text),
    pg_temp.item(2, 'questionnaire', jsonb_build_object('questionnaire_id', 'new:questionnaire:1'), 'Insert', 'respondent_type_id', null, pg_temp.id('respondent_type')::text),
    pg_temp.item(3, 'questionnaire', jsonb_build_object('questionnaire_id', 'new:questionnaire:1'), 'Insert', 'name', null, 'ZZ Pushed Questionnaire'),
    pg_temp.item(4, 'questionnaire', jsonb_build_object('questionnaire_id', 'new:questionnaire:1'), 'Insert', 'abbreviation', null, 'ZZP'),
    pg_temp.item(5, 'wave', jsonb_build_object('wave_id', 'new:wave:1'), 'Insert', 'questionnaire_id', null, 'new:questionnaire:1'),
    pg_temp.item(6, 'wave', jsonb_build_object('wave_id', 'new:wave:1'), 'Insert', 'name', null, 'Wave 1'),
    pg_temp.item(7, 'component', jsonb_build_object('component_id', 'new:component:1'), 'Insert', 'component_type_id', null, pg_temp.id('section_type')::text),
    pg_temp.item(8, 'component', jsonb_build_object('component_id', 'new:component:1'), 'Insert', 'text', null, 'Pushed section'),
    pg_temp.item(9, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:1'), 'Insert', 'questionnaire_id', null, 'new:questionnaire:1'),
    pg_temp.item(10, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:1'), 'Insert', 'component_id', null, 'new:component:1'),
    pg_temp.item(11, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:1'), 'Insert', 'display_order', null, '1'),
    pg_temp.item(12, 'component', jsonb_build_object('component_id', 'new:component:2'), 'Insert', 'component_type_id', null, (select component_type_id from public.component_type where name = 'Question')::text),
    pg_temp.item(13, 'component', jsonb_build_object('component_id', 'new:component:2'), 'Insert', 'text', null, 'Pushed question?'),
    pg_temp.item(14, 'question', jsonb_build_object('component_id', 'new:component:2'), 'Insert', 'component_id', null, 'new:component:2'),
    pg_temp.item(15, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:2'), 'Insert', 'questionnaire_id', null, 'new:questionnaire:1'),
    pg_temp.item(16, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:2'), 'Insert', 'component_id', null, 'new:component:2'),
    pg_temp.item(17, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:2'), 'Insert', 'parent_placement_id', null, 'new:questionnaire_component:1'),
    pg_temp.item(18, 'questionnaire_component', jsonb_build_object('placement_id', 'new:questionnaire_component:2'), 'Insert', 'display_order', null, '1'),
    pg_temp.item(19, 'questionnaire_question', jsonb_build_object('placement_id', 'new:questionnaire_component:2'), 'Insert', 'questionnaire_id', null, 'new:questionnaire:1'),
    pg_temp.item(20, 'questionnaire_question', jsonb_build_object('placement_id', 'new:questionnaire_component:2'), 'Insert', 'component_id', null, 'new:component:2'),
    pg_temp.item(21, 'questionnaire_question', jsonb_build_object('placement_id', 'new:questionnaire_component:2'), 'Insert', 'self_contained', null, 'true'),
    pg_temp.item(22, 'wave_question', jsonb_build_object('placement_id', 'new:questionnaire_component:2', 'wave_id', 'new:wave:1'), 'Insert', 'questionnaire_id', null, 'new:questionnaire:1'),
    pg_temp.item(23, 'wave_question', jsonb_build_object('placement_id', 'new:questionnaire_component:2', 'wave_id', 'new:wave:1'), 'Insert', 'question_number', null, '1'),
    pg_temp.item(24, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'questionnaire_id', null, 'new:questionnaire:1'),
    pg_temp.item(25, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'placement_id', null, 'new:questionnaire_component:2'),
    pg_temp.item(26, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'matr_id', null, 'ZZP1'),
    pg_temp.item(27, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'display_order', null, '1'),
    pg_temp.item(28, 'open_ended', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'answer_data_type', null, 'TEXT'));

  -- an extraction by a STAFF member needs approval; the rows come with the approval
  e := public.create_draft('Extraction', pg_temp.id('ana'), jsonb_build_object('status', 'Awaiting Review', 'summary', 'Push ZZP'), jsonb_build_object('method', 'Manual'));
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s, ''[]''::jsonb)', e, pg_temp.id('ana')), 'needs approval from another staff member');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s)', e, pg_temp.id('ben')), 'nothing in this to apply');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s, %L::jsonb)', e, pg_temp.id('ben'),
    jsonb_build_array(pg_temp.item(1, 'registry', jsonb_build_object('registry_id', 'new:registry:1'), 'Insert', 'name', null, 'Not allowed'))), 'touches a table it can''t change \(registry\)');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s, %L::jsonb)', e, pg_temp.id('ben'),
    jsonb_build_array(pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave2')), 'Update', 'name', 'Round 2', 'No'))), 'only adds rows');
  -- a question it reuses can be linked, but only one that has no stem yet
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s, %L::jsonb)', e, pg_temp.id('ben'),
    jsonb_build_array(pg_temp.item(1, 'question', jsonb_build_object('component_id', 1), 'Update', 'question_stem_id', '5', '6'))), 'only adds rows');
  made := public.approve_draft(e, 1, pg_temp.id('ben'), items);

  select questionnaire_id into q from public.questionnaire where name = 'ZZ Pushed Questionnaire';
  select wave_id into w from public.wave where questionnaire_id = q and name = 'Wave 1';
  assert q is not null and w is not null, 'the questionnaire and its wave were added';
  assert (select count(*) from public.questionnaire_component where questionnaire_id = q) = 2, 'the section and the question are placed';
  assert (select p.component_id from public.questionnaire_component qc join public.component c using (component_id)
          join public.questionnaire_component p on p.placement_id = qc.parent_placement_id
          where qc.questionnaire_id = q and c.text = 'Pushed question?')
       = (select component_id from public.component where text = 'Pushed section'), 'the question sits in the section';
  assert (select question_number from public.wave_question where wave_id = w) = '1', 'the wave asked it';
  select variable_key into v from public.variable where questionnaire_id = q and matr_id = 'ZZP1';
  assert v is not null and exists (select 1 from public.open_ended where variable_key = v and answer_data_type = 'TEXT'), 'the variable is write-in';
  assert (select kind from public.draft where draft_id = made) = 'Extraction', 'the history shows an extraction';
  assert (select staff_id from public.draft_review_event where draft_id = made and event = 'Approved') = pg_temp.id('ben'), 'approved by Ben';
  -- the history has a line for each table it added to, with the keys of the rows it added there
  assert (select count(*) from public.applied_item where draft_id = made) = 9, 'a line for each table';
  assert (select string_agg(entity_name, ',' order by item_number) from public.applied_item where draft_id = made)
    = 'questionnaire,wave,component,questionnaire_component,question,questionnaire_question,wave_question,variable,open_ended',
    'in the order it added to them';
  assert (select jsonb_array_length(record_identifier::jsonb) from public.applied_item where draft_id = made and entity_name = 'component') = 2,
    'each row once';
  assert (select record_identifier::jsonb -> 0 ->> 'variable_key' from public.applied_item where draft_id = made and entity_name = 'variable') = v::text,
    'with the numbers the database gave';
  assert not exists (select 1 from public.applied_item where draft_id = made and record_identifier like '%new:%'), 'no placeholder is left';
  assert (select status from public.draft where draft_id = e) = 'Applied', 'the extraction is applied';
  assert exists (select 1 from public.extracted_draft where draft_id = e), 'and its extraction draft is kept, for analysis';
  perform pg_temp.expect_error(format('delete from public.extracted_draft where draft_id = %s', e), 'already been applied');

  -- an ADMIN pushes without approval (the same rows again would repeat the wave's name: use other names)
  e := public.create_draft('Extraction', pg_temp.id('ada'), jsonb_build_object('summary', 'Push ZZP2'), jsonb_build_object('method', 'Manual'));
  made := public.apply_draft(e, pg_temp.id('ada'), jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', 'new:wave:1'), 'Insert', 'questionnaire_id', null, q::text),
    pg_temp.item(2, 'wave', jsonb_build_object('wave_id', 'new:wave:1'), 'Insert', 'name', null, 'Wave 2')));
  assert exists (select 1 from public.wave where questionnaire_id = q and name = 'Wave 2'), 'the ADMIN''s push went in';
  assert not exists (select 1 from public.draft_review_event where draft_id = made and event = 'Approved'), 'with no approval';

  -- BR92: a variable must be open- or closed-ended; the whole push is refused if not. That check is
  -- deferred to the end of the transaction, so it is run here as the end of one would run it.
  e := public.create_draft('Extraction', pg_temp.id('ada'), '{}'::jsonb, jsonb_build_object('method', 'Manual'));
  begin
    perform public.apply_draft(e, pg_temp.id('ada'), jsonb_build_array(
      pg_temp.item(1, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'questionnaire_id', null, q::text),
      pg_temp.item(2, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'placement_id', null,
        (select qc.placement_id from public.questionnaire_component qc join public.component c using (component_id)
         where qc.questionnaire_id = q and c.text = 'Pushed question?')::text),
      pg_temp.item(3, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'matr_id', null, 'ZZP9'),
      pg_temp.item(4, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'display_order', null, '9')));
    set constraints all immediate;
    raise exception 'went through';
  exception when others then
    assert sqlerrm like '%exactly one of OPEN_ENDED or CLOSED_ENDED%', 'a variable with no answer format is refused: ' || sqlerrm;
  end;
  assert not exists (select 1 from public.variable where matr_id = 'ZZP9'), 'and nothing was left';
end;
$$;

-- ------ The applied draft is the record: its rules ----------------------------------------------------------------
do $$
declare
  e  bigint;
  ap bigint;
begin
  -- an applied draft
  e := pg_temp.edit(pg_temp.id('ana'), 'update', jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave2')), 'Update', 'respondent_count', null, '5')), 'Draft');
  perform public.apply_draft(e, pg_temp.id('ana'));

  -- it can't be deleted or changed, nor can its details
  perform pg_temp.expect_error(format('delete from public.draft where draft_id = %s', e), 'can''t be deleted');
  perform pg_temp.expect_error(format('delete from public.edit_draft where draft_id = %s', e), 'already been applied');
  perform pg_temp.expect_error(format('update public.draft set summary = ''x'' where draft_id = %s', e), 'already been applied');
  -- nor can its reviews, and it gets no more
  perform pg_temp.expect_error(format('insert into public.draft_review_event (draft_id, event, staff_id) values (%s, ''Submitted'', %s)', e, pg_temp.id('ana')), 'already been applied');
  assert (select count(*) from public.applied_item where draft_id = e) = 1, 'its items are still there';

  -- a draft that was never applied can be deleted, with its details
  ap := pg_temp.edit(pg_temp.id('ana'), 'add', '[]'::jsonb, 'Draft');
  delete from public.draft where draft_id = ap;
  assert not exists (select 1 from public.edit_draft where draft_id = ap), 'its details went with it';

  -- the constraints, written straight to the table (as a mistake in the SQL editor would)
  ap := pg_temp.edit(pg_temp.id('ana'), 'add', '[]'::jsonb, 'Draft');
  -- Applied needs a time, and a time needs Applied
  perform pg_temp.expect_error(format('update public.draft set status = ''Applied'' where draft_id = %s', ap), 'draft_applied_says_when');
  perform pg_temp.expect_error(format('update public.draft set applied_at = now() where draft_id = %s', ap), 'draft_applied_says_when');
  -- an approval is from someone other than the preparer and whoever saved it last (BR27)
  perform pg_temp.expect_error(format('insert into public.draft_review_event (draft_id, event, staff_id) values (%s, ''Approved'', %s)', ap, pg_temp.id('ana')), 'someone other than');
  perform public.save_draft(ap, pg_temp.id('cy'));
  perform pg_temp.expect_error(format('insert into public.draft_review_event (draft_id, event, staff_id) values (%s, ''Approved'', %s)', ap, pg_temp.id('cy')), 'someone other than');
  -- and goes with applying the change (checked when the transaction ends, so it is run here as that would)
  begin
    insert into public.draft_review_event (draft_id, event, staff_id) values (ap, 'Approved', pg_temp.id('ben'));
    set constraints draft_approval_applies immediate;
    raise exception 'went through';
  exception when others then
    assert sqlerrm like '%goes with applying the change%', 'an approval of a draft that isn''t applied is refused: ' || sqlerrm;
  end;
  -- a review is the record of what happened: never changed, and deleted only with its draft
  insert into public.draft_review_event (draft_id, event, staff_id) values (ap, 'Submitted', pg_temp.id('cy'));
  perform pg_temp.expect_error(format('update public.draft_review_event set note = null where draft_id = %s', ap), 'can''t be changed');
  perform pg_temp.expect_error(format('delete from public.draft_review_event where draft_id = %s', ap), 'goes only with its draft');
  delete from public.draft where draft_id = ap;
  assert not exists (select 1 from public.draft_review_event where draft_id = ap), 'a draft that was never applied takes its reviews with it';

  -- an item belongs to a draft, is numbered once, and says what happened to its row
  perform pg_temp.expect_error('insert into public.applied_item (draft_id, item_number, entity_name, record_identifier, action) values (0, 1, ''wave'', ''{}'', ''Update'')', 'applied_item_draft_id_fkey');
  perform pg_temp.expect_error(format('insert into public.applied_item (draft_id, item_number, entity_name, record_identifier, action) values (%s, 1, ''wave'', ''{}'', ''Update'')', e), 'applied_item_pkey');
  perform pg_temp.expect_error(format('insert into public.applied_item (draft_id, item_number, entity_name, record_identifier, action) values (%s, 2, ''wave'', ''{}'', ''Add'')', e), 'applied_item_action_check');
end;
$$;

-- ------ One person at a time works on a draft --------------------------------------------------
do $$
declare
  d     bigint;
  claim jsonb;
begin
  d := pg_temp.edit(pg_temp.id('ana'), 'add', '[]'::jsonb, 'Draft');
  claim := public.claim_draft(d, pg_temp.id('ana'));
  assert (claim ->> 'claimed')::boolean and (claim ->> 'editing_by')::bigint = pg_temp.id('ana'), 'the first claim gets it';
  assert claim ->> 'editing_name' = 'Test Ana' and (claim ->> 'revision')::int = 1, 'and says who has it, and its revision';
  claim := public.claim_draft(d, pg_temp.id('ben'));
  assert not (claim ->> 'claimed')::boolean and (claim ->> 'editing_by')::bigint = pg_temp.id('ana'), 'a second person is refused, and told who has it';
  assert (public.claim_draft(d, pg_temp.id('ana')) ->> 'claimed')::boolean, 'who has it renews it';
  update public.draft set editing_at = now() - interval '10 minutes' where draft_id = d;
  assert not (public.claim_draft(d, pg_temp.id('ben'), false, 3600) ->> 'claimed')::boolean, 'a claim lasts as long as the caller says';
  assert (public.claim_draft(d, pg_temp.id('ben')) ->> 'claimed')::boolean, 'a lapsed claim can be taken';
  assert not (public.claim_draft(d, pg_temp.id('ana')) ->> 'claimed')::boolean, 'and is then the new person''s';
  assert (public.claim_draft(d, pg_temp.id('ana'), true) ->> 'claimed')::boolean, 'anyone can take over';
  perform pg_temp.expect_error(format('select public.claim_draft(%s, %s)', d, pg_temp.id('dee')), 'Only active staff');
  perform pg_temp.expect_error('select public.claim_draft(0, ' || pg_temp.id('ana') || ')', 'no longer in the database');
  perform pg_temp.expect_error(format('update public.draft set editing_at = null where draft_id = %s', d), 'draft_editing_says_when');
  assert not public.release_draft(d, pg_temp.id('ben')), 'only who has it lets it go';
  assert public.release_draft(d, pg_temp.id('ana')), 'who has it lets it go';
  assert (select editing_by is null and editing_at is null and revision = 1 and saved_by = pg_temp.id('ana')
          from public.draft where draft_id = d), 'nobody has it, and claiming changed nothing else';
  -- a draft set aside can't be worked on
  perform public.save_draft(d, pg_temp.id('ana'), '{"status": "Withdrawn"}'::jsonb, '{}'::jsonb, null, false);
  perform pg_temp.expect_error(format('select public.claim_draft(%s, %s)', d, pg_temp.id('ana')), 'no longer being prepared');
end;
$$;

-- A study needs no lead investigator or status, and one recorded later never changes. A questionnaire's
-- participant count covers its waves'. An applied extraction's wave keeps its PDF and its file of
-- Variable Names and Coding, and a discarded draft doesn't take them with it.
do $$
declare
  st bigint;
  other bigint;
  no_email bigint;
  e bigint;
  w1 bigint;
  w2 bigint;
begin
  insert into public.study (registry_id, name) values (pg_temp.id('registry'), 'ZZ Study With No Lead') returning study_id into st;
  update public.study set principal_investigator_id = pg_temp.id('pi') where study_id = st;
  insert into public.principal_investigator (name, email) values ('ZZ Other PI', 'other@drafts.test')
    returning principal_investigator_id into other;
  perform pg_temp.expect_error(format('update public.study set principal_investigator_id = %s where study_id = %s', other, st), 'lead investigator can''t be changed');
  perform pg_temp.expect_error(format('update public.study set principal_investigator_id = null where study_id = %s', st), 'lead investigator can''t be changed');

  -- an investigator needs no email, but a study's contact does (checked when the transaction ends)
  insert into public.principal_investigator (name) values ('ZZ No Email PI') returning principal_investigator_id into no_email;
  insert into public.study (registry_id, principal_investigator_id, name) values (pg_temp.id('registry'), no_email, 'ZZ Led Without Email');
  begin
    update public.study set contact_principal_investigator_id = no_email where study_id = st;
    set constraints contact_has_email immediate;
    raise exception 'went through';
  exception when others then
    assert sqlerrm = 'ZZ No Email PI has no email, so they can''t be a study''s contact.', 'a contact needs an email: ' || sqlerrm;
  end;
  update public.study set contact_principal_investigator_id = other where study_id = st;
  begin
    update public.principal_investigator set email = null where principal_investigator_id = other;
    set constraints contact_keeps_email immediate;
    raise exception 'went through';
  exception when others then
    assert sqlerrm = 'ZZ Other PI has no email, so they can''t be a study''s contact.', 'a contact keeps their email: ' || sqlerrm;
  end;
  -- given an email and made the contact in one change, in either order
  set constraints contact_has_email, contact_keeps_email deferred;
  update public.study set contact_principal_investigator_id = no_email where study_id = st;
  update public.principal_investigator set email = 'noemail@drafts.test' where principal_investigator_id = no_email;
  set constraints contact_has_email, contact_keeps_email immediate;
  set constraints contact_has_email, contact_keeps_email deferred;

  -- two waves of 100 and 90 respondents, and the questionnaire's other waves none (earlier checks changed
  -- and deleted the test waves). The rule is checked when the transaction ends, so it is run here as that would.
  update public.wave set respondent_count = null where questionnaire_id = pg_temp.id('questionnaire');
  insert into public.wave (questionnaire_id, name, respondent_count) values (pg_temp.id('questionnaire'), 'ZZ Counted 1', 100)
    returning wave_id into w1;
  insert into public.wave (questionnaire_id, name, respondent_count) values (pg_temp.id('questionnaire'), 'ZZ Counted 2', 90)
    returning wave_id into w2;
  begin
    update public.questionnaire set participant_count = 150 where questionnaire_id = pg_temp.id('questionnaire');
    set constraints questionnaire_count_covers_waves immediate;
    raise exception 'went through';
  exception when others then
    assert sqlerrm like '%190 respondents between them, more than its 150 participants%', 'a total below the waves'' is refused: ' || sqlerrm;
  end;
  update public.questionnaire set participant_count = 200 where questionnaire_id = pg_temp.id('questionnaire');
  set constraints questionnaire_count_covers_waves immediate;
  set constraints questionnaire_count_covers_waves deferred;
  begin
    update public.wave set respondent_count = 120 where wave_id = w1;
    set constraints wave_counts_within_questionnaire immediate;
    raise exception 'went through';
  exception when others then
    assert sqlerrm like '%210 respondents between them%', 'waves that add up to more are refused: ' || sqlerrm;
  end;
  update public.wave set respondent_count = null where wave_id = w1;
  set constraints wave_counts_within_questionnaire immediate;
  set constraints wave_counts_within_questionnaire deferred;

  e := public.create_draft('Extraction', pg_temp.id('ada'), jsonb_build_object('summary', 'Files'), jsonb_build_object(
    'method', 'Manual', 'pdf_sha256', 'zzfiles', 'pdf_name', 'q.pdf', 'pdf_bytes', 20,
    'supplement', jsonb_build_object('name', 'codes.pdf', 'sha256', 'zzcodes', 'bytes', 10),
    'info', jsonb_build_object('wave_id', w1::text)));
  assert (select pdf_bytes from public.pdf_file where pdf_sha256 = 'zzcodes') = 10, 'the file of Variable Names and Coding has a pdf_file row';
  perform public.apply_draft(e, pg_temp.id('ada'), jsonb_build_array(
    pg_temp.item(1, 'wave', jsonb_build_object('wave_id', 'new:wave:1'), 'Insert', 'questionnaire_id', null, pg_temp.id('questionnaire')::text),
    pg_temp.item(2, 'wave', jsonb_build_object('wave_id', 'new:wave:1'), 'Insert', 'name', null, 'Wave Files')));
  assert (select pdf_sha256 = 'zzfiles' and pdf_name = 'q.pdf' and supplement_sha256 = 'zzcodes' and supplement_name = 'codes.pdf'
                 and supplement_text is null
          from public.wave_file where wave_id = w1), 'the wave keeps the files it was extracted from';
  e := public.create_draft('Extraction', pg_temp.id('ada'), '{}'::jsonb, jsonb_build_object(
    'method', 'Manual', 'pdf_sha256', 'zzfiles', 'supplement', jsonb_build_object('name', 'codes.pdf', 'sha256', 'zzcodes')));
  perform public.discard_draft(e, pg_temp.id('ada'));
  assert (select count(*) from public.pdf_file where pdf_sha256 in ('zzfiles', 'zzcodes')) = 2, 'discarding a draft leaves the files a wave uses';
  e := public.create_draft('Extraction', pg_temp.id('ada'), '{}'::jsonb, jsonb_build_object(
    'method', 'Manual', 'pdf_sha256', 'zzalone', 'supplement', jsonb_build_object('name', 'c.pdf', 'sha256', 'zzalone2')));
  perform public.discard_draft(e, pg_temp.id('ada'));
  assert not exists (select 1 from public.pdf_file where pdf_sha256 in ('zzalone', 'zzalone2')), 'and takes the ones nothing else uses';

  -- the dashboard sees a wave's files, and no draft's
  e := public.create_draft('Extraction', pg_temp.id('ada'), '{}'::jsonb, jsonb_build_object('method', 'Manual', 'pdf_sha256', 'zzdraftonly'));
  set local role anon;
  assert (select count(*) from public.pdf_file where pdf_sha256 in ('zzfiles', 'zzcodes')) = 2, 'the dashboard sees a wave''s files';
  assert not exists (select 1 from public.pdf_file where pdf_sha256 = 'zzdraftonly'), 'but not a draft''s';
  assert (select pdf_sha256 from public.wave_file where wave_id = w1) = 'zzfiles', 'and which file is the wave''s';
  reset role;
  perform public.discard_draft(e, pg_temp.id('ada'));

  -- a deleted wave takes its files once nothing else names them. The extraction it was read from was applied, and an
  -- applied extraction is kept as the record of what it was read from, so its files stay.
  delete from public.wave where wave_id = w1;
  assert (select count(*) from public.pdf_file where pdf_sha256 in ('zzfiles', 'zzcodes')) = 2, 'files the applied extraction names stay';
  -- a wave whose files nothing else names takes them with it
  declare
    w3 bigint;
  begin
    insert into public.pdf_file (pdf_sha256) values ('zzwaveonly');
    insert into public.wave (questionnaire_id, name) values (pg_temp.id('questionnaire'), 'Wave Only Files') returning wave_id into w3;
    insert into public.wave_file (wave_id, pdf_sha256, pdf_name) values (w3, 'zzwaveonly', 'w.pdf');
    delete from public.wave where wave_id = w3;
    assert not exists (select 1 from public.pdf_file where pdf_sha256 = 'zzwaveonly'), 'one nothing else uses goes with the wave';
  end;
end;
$$;

-- Deleting a wave, then its questionnaire: what only it owns goes, and what nothing owns afterwards; what anything
-- else still uses stays. Last, because the clean-up runs when a change ends, and here it is run by hand: it then
-- also clears what the checks above left without an owner.
create function pg_temp.place_question(q bigint, parent bigint, n int, words text, stem bigint default null,
                                       follows bigint default null) returns bigint
language plpgsql as $$
declare
  c bigint;
begin
  insert into public.component (component_type_id, text)
    select component_type_id, words from public.component_type where name = 'Question' returning component_id into c;
  insert into public.question (component_id, question_stem_id) values (c, stem);
  return pg_temp.place_again(q, parent, n, c, follows);
end;
$$;
-- places question c in questionnaire q (again); parent and follows are placements; returns the new placement
create function pg_temp.place_again(q bigint, parent bigint, n int, c bigint, follows bigint default null) returns bigint
language plpgsql as $$
declare
  p bigint;
begin
  insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order)
    values (q, c, parent, n) returning placement_id into p;
  insert into public.questionnaire_question (placement_id, questionnaire_id, component_id, parent_question_placement_id, self_contained)
    values (p, q, c, follows, follows is null);
  return p;
end;
$$;
create function pg_temp.ask(q bigint, w bigint, p bigint, n text) returns void
language sql as $$
  insert into public.wave_question (questionnaire_id, wave_id, placement_id, question_number) values (q, w, p, n);
$$;
create function pg_temp.answer(q bigint, p bigint, matr text, skip_to bigint default null) returns bigint
language sql as $$
  with v as (
    insert into public.variable (questionnaire_id, placement_id, matr_id, display_order) values (q, p, matr, 1) returning variable_key
  )
  insert into public.open_ended (variable_key, answer_data_type, skip_to_placement_id)
  select variable_key, 'TEXT', skip_to from v returning variable_key;
$$;
create function pg_temp.component_of(p bigint) returns bigint
language sql as $$ select component_id from public.questionnaire_component where placement_id = p $$;
create function pg_temp.tidy() returns void
language plpgsql as $$
begin
  set constraints question_left_unasked, component_left_unplaced, question_stem_left_unused immediate;
  set constraints question_left_unasked, component_left_unplaced, question_stem_left_unused deferred;
end;
$$;

-- M3 and M4 with a Grid: it has words (its lead-in), holds questions with their instructions and definitions,
-- and skips like a Group; a Section, Grid or Group never goes inside it, and a Section never inside a Group.
-- Everything here is undone at the end, so the checks after it see the data as it was.
do $$
declare
  q bigint := pg_temp.id('questionnaire');
  kinds jsonb;
  grid_c bigint; grid bigint; asked bigint; told_c bigint; group_c bigint; grp bigint; inner_grid_c bigint;
begin
  begin
    select jsonb_object_agg(name, component_type_id) into kinds from public.component_type;
    assert kinds ? 'Grid', 'there is a Grid kind of block';
    -- M4 waits for the change to end (a question's component goes in before its question row), so ask it now
    begin
      insert into public.component (component_type_id, text) values ((kinds ->> 'Grid')::bigint, null);
      set constraints component_kind_rules immediate;
      raise exception 'A Grid with no words went through';
    exception when others then
      if sqlerrm !~ 'only a Group has no words' then
        raise;
      end if;
    end;
    set constraints component_kind_rules deferred;
    set constraints questionnaire_component_parent_kind immediate;

    insert into public.component (component_type_id, text) values ((kinds ->> 'Grid')::bigint, 'ZZ In the past month, how often did you')
      returning component_id into grid_c;
    insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order, skip_to_end)
      values (q, grid_c, pg_temp.id('section_a_place'), 90, true) returning placement_id into grid;
    asked := pg_temp.place_question(q, grid, 1, 'ZZ feel tired?');
    insert into public.component (component_type_id, text) values ((kinds ->> 'Instruction')::bigint, 'ZZ Tick one box per row.')
      returning component_id into told_c;
    insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order)
      values (q, told_c, grid, 2);
    assert (select count(*) from public.questionnaire_component where parent_placement_id = grid) = 2,
      'a grid holds its questions and instructions, and skips';

    perform pg_temp.expect_error(format(
      'insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order) values (%s, %s, %s, 3)',
      q, pg_temp.id('section_a'), grid),
      'a Section is never inside a Group or a Grid');
    insert into public.component (component_type_id, text) values ((kinds ->> 'Grid')::bigint, 'ZZ How much do you agree that')
      returning component_id into inner_grid_c;
    perform pg_temp.expect_error(format(
      'insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order) values (%s, %s, %s, 3)',
      q, inner_grid_c, grid),
      'Grid placement .* is inside a Grid');
    select component_id into group_c from public.component where component_type_id = (kinds ->> 'Group')::bigint;
    if group_c is null then
      insert into public.component (component_type_id, text) values ((kinds ->> 'Group')::bigint, null) returning component_id into group_c;
    end if;
    perform pg_temp.expect_error(format(
      'insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order) values (%s, %s, %s, 3)',
      q, group_c, grid),
      'Group placement .* is inside a Grid');
    perform pg_temp.expect_error(format(
      'update public.questionnaire_component set skip_to_end = true where questionnaire_id = %s and component_id = %s', q, told_c),
      'only a Group or a Grid skips');

    insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order)
      values (q, group_c, pg_temp.id('section_a_place'), 91) returning placement_id into grp;
    insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order)
      values (q, grid_c, grp, 1);
    assert (select count(*) from public.questionnaire_component where component_id = grid_c) = 2,
      'a grid can be inside a group, and placed twice';
    perform pg_temp.expect_error(format(
      'insert into public.questionnaire_component (questionnaire_id, component_id, parent_placement_id, display_order) values (%s, %s, %s, 2)',
      q, pg_temp.id('section_a'), grp),
      'a Section is never inside a Group or a Grid');
    raise exception 'zz grid checks done';
  exception when raise_exception then
    if sqlerrm <> 'zz grid checks done' then
      raise;
    end if;
  end;
  set constraints questionnaire_component_parent_kind deferred;
end;
$$;

do $$
declare
  q bigint; other bigint; w1 bigint; w2 bigint; ow bigint;
  sec bigint; once bigint; twice bigint; parent bigint; follow bigint; parent2 bigint; follow2 bigint;
  shared bigint; shared_there bigint; scaled bigint; skipped_to bigint; again bigint;
  once_c bigint; twice_c bigint; parent2_c bigint; follow2_c bigint; shared_c bigint; scaled_c bigint; sec_c bigint;
  stem_alone bigint; stem_shared bigint; scale bigint;
begin
  insert into public.questionnaire (study_id, respondent_type_id, name, abbreviation)
    select pg_temp.id('study'), pg_temp.id('respondent_type'), 'ZZ Owned', 'ZZO' returning questionnaire_id into q;
  insert into public.questionnaire (study_id, respondent_type_id, name, abbreviation)
    select pg_temp.id('study'), pg_temp.id('respondent_type'), 'ZZ Neighbour', 'ZZN' returning questionnaire_id into other;
  insert into public.wave (questionnaire_id, name) values (q, 'W1') returning wave_id into w1;
  insert into public.wave (questionnaire_id, name) values (q, 'W2') returning wave_id into w2;
  insert into public.wave (questionnaire_id, name) values (other, 'W1') returning wave_id into ow;
  insert into public.question_stem (text) values ('ZZ alone [subject]?') returning question_stem_id into stem_alone;
  insert into public.question_stem (text) values ('ZZ shared [subject]?') returning question_stem_id into stem_shared;
  insert into public.category (mesh_id, term) values ('Z9999999', 'ZZ term');
  insert into public.measurement_scale (name, abbreviation) values ('ZZ Scale', 'ZZS') returning scale_id into scale;

  insert into public.component (component_type_id, text) values (pg_temp.id('section_type'), 'ZZ Owned section') returning component_id into sec_c;
  insert into public.questionnaire_component (questionnaire_id, component_id, display_order) values (q, sec_c, 1) returning placement_id into sec;
  once       := pg_temp.place_question(q, sec, 1, 'ZZ Asked once?', stem_alone);
  twice      := pg_temp.place_question(q, sec, 2, 'ZZ Asked twice?', stem_shared);
  parent     := pg_temp.place_question(q, sec, 3, 'ZZ Parent?');
  follow     := pg_temp.place_question(q, sec, 4, 'ZZ Follow-up?', null, parent);
  shared     := pg_temp.place_question(q, sec, 5, 'ZZ Shared?', stem_shared);
  scaled     := pg_temp.place_question(q, sec, 6, 'ZZ In a scale?');
  skipped_to := pg_temp.place_question(q, sec, 7, 'ZZ Skipped to?');
  parent2    := pg_temp.place_question(q, sec, 8, 'ZZ Parent kept?');
  follow2    := pg_temp.place_question(q, sec, 9, 'ZZ Follow-up kept?', null, parent2);
  once_c := pg_temp.component_of(once);     twice_c := pg_temp.component_of(twice);     shared_c := pg_temp.component_of(shared);
  scaled_c := pg_temp.component_of(scaled); parent2_c := pg_temp.component_of(parent2); follow2_c := pg_temp.component_of(follow2);
  shared_there := pg_temp.place_again(other, null, 1, shared_c);
  insert into public.question_category (component_id, mesh_id) values (once_c, 'Z9999999');
  insert into public.question_scale (scale_id, component_id, item_number) values (scale, scaled_c, '1');
  perform pg_temp.ask(q, w1, once, '1');   perform pg_temp.answer(q, once, 'ZZO1');
  perform pg_temp.ask(q, w1, twice, '2');  perform pg_temp.ask(q, w2, twice, '1');
  perform pg_temp.answer(q, twice, 'ZZO2', skipped_to);   -- skips to a place in its own questionnaire
  perform pg_temp.ask(q, w2, parent2, '6'); perform pg_temp.ask(q, w2, follow2, '7');
  perform pg_temp.ask(q, w1, parent, '3'); perform pg_temp.ask(q, w2, follow, '2');
  perform pg_temp.ask(q, w2, shared, '3'); perform pg_temp.ask(q, w2, scaled, '4'); perform pg_temp.ask(q, w2, skipped_to, '5');
  perform pg_temp.ask(other, ow, shared_there, '1');
  perform pg_temp.answer(other, shared_there, 'ZZN1');
  perform pg_temp.tidy();
  assert (select count(*) from public.questionnaire_component where questionnaire_id = q) = 10, 'nothing goes before a delete';

  -- the same question printed twice in one questionnaire: one component, two placements, each with its own
  -- number and variables
  again := pg_temp.place_again(q, sec, 10, shared_c);
  perform pg_temp.ask(q, w2, again, '8');
  perform pg_temp.answer(q, again, 'ZZO3');
  assert (select count(*) from public.questionnaire_component where questionnaire_id = q and component_id = shared_c) = 2,
    'a questionnaire can print a component twice';
  assert (select placement_id from public.variable where questionnaire_id = q and matr_id = 'ZZO3') = again,
    'each placement has its own variables';
  -- one component for each kind and words (but for runs of spaces)
  perform pg_temp.expect_error(
    'insert into public.component (component_type_id, text) select component_type_id, ''ZZ  Shared?'' from public.component_type where name = ''Question''',
    'component_words_unique');
  delete from public.wave_question where placement_id = again;
  perform pg_temp.tidy();
  assert not exists (select 1 from public.questionnaire_component where placement_id = again)
     and exists (select 1 from public.questionnaire_component where placement_id = shared), 'a placement no wave asks goes; the other stays';

  -- a wave: what it asked goes, and a question asked in no other wave goes with it
  delete from public.wave where wave_id = w1;
  assert not exists (select 1 from public.wave_question where wave_id = w1), 'the wave''s questions go with it';
  perform pg_temp.tidy();
  assert not exists (select 1 from public.questionnaire_component where placement_id = once), 'a question asked only in it goes';
  assert not exists (select 1 from public.variable where matr_id = 'ZZO1' and questionnaire_id = q), 'with its variables';
  assert not exists (select 1 from public.component where component_id = once_c), 'and its component, printed nowhere else';
  assert not exists (select 1 from public.question_category where component_id = once_c), 'with its MeSH tags';
  assert not exists (select 1 from public.question_stem where question_stem_id = stem_alone), 'and its stem, which no other question has';
  assert exists (select 1 from public.category where mesh_id = 'Z9999999'), 'the MeSH category itself stays';
  assert exists (select 1 from public.wave_question where wave_id = w2 and placement_id = twice), 'a question another wave asks stays';
  assert exists (select 1 from public.variable where matr_id = 'ZZO2'), 'with its variables';
  assert exists (select 1 from public.questionnaire_question where placement_id = parent),
    'a question a follow-up names stays, though no wave asks it now';

  -- the follow-up goes, and then the question above it, which nothing names any more
  delete from public.wave_question where wave_id = w2 and placement_id = follow;
  perform pg_temp.tidy();
  assert not exists (select 1 from public.questionnaire_component where placement_id in (follow, parent)),
    'a follow-up asked nowhere goes, and so does the question above it';

  -- the questionnaire: its waves and everything placed in it, follow-ups and what they follow too
  delete from public.questionnaire where questionnaire_id = q;
  assert not exists (select 1 from public.wave where questionnaire_id = q), 'its waves go with it';
  assert not exists (select 1 from public.questionnaire_component where questionnaire_id = q)
     and not exists (select 1 from public.questionnaire_question where questionnaire_id = q)
     and not exists (select 1 from public.wave_question where questionnaire_id = q)
     and not exists (select 1 from public.variable where questionnaire_id = q), 'and everything placed in it';
  perform pg_temp.tidy();
  assert not exists (select 1 from public.component where component_id in (sec_c, twice_c, parent2_c, follow2_c)),
    'components only it printed go, though one of its own variables skipped inside it';
  assert exists (select 1 from public.component where component_id = shared_c)
     and exists (select 1 from public.questionnaire_question where questionnaire_id = other and component_id = shared_c)
     and exists (select 1 from public.variable where matr_id = 'ZZN1' and questionnaire_id = other), 'one another questionnaire prints stays, as it was there';
  assert exists (select 1 from public.question_stem where question_stem_id = stem_shared), 'and so does the stem it has';
  assert exists (select 1 from public.question_scale where component_id = scaled_c), 'one a measurement scale lists stays in the scale';
  assert exists (select 1 from public.measurement_scale where scale_id = scale), 'and the shared lists stay';

  -- a shared row something uses can't be deleted
  perform pg_temp.expect_error(format('delete from public.component where component_id = %s', shared_c), 'violates foreign key');
  perform pg_temp.expect_error(format('delete from public.study where study_id = %s', pg_temp.id('study')), 'violates foreign key');
end;
$$;

-- An option set's name is its choices in its order, and there is one set for each combination of choices.
do $$
declare
  never bigint; sometimes bigint; often bigint; always bigint;
  s1 bigint; s2 bigint;
begin
  insert into public.answer_option (standardized_label) values ('zz never') returning answer_option_id into never;
  insert into public.answer_option (standardized_label) values ('zz sometimes') returning answer_option_id into sometimes;
  insert into public.answer_option (standardized_label) values ('zz often') returning answer_option_id into often;
  insert into public.answer_option (standardized_label) values ('zz always') returning answer_option_id into always;
  insert into public.option_set default values returning option_set_id into s1;
  insert into public.answer_option_set (option_set_id, answer_option_id, display_order)
    values (s1, often, 3), (s1, never, 1), (s1, sometimes, 2);
  set constraints option_set_choices_unique immediate;
  set constraints option_set_choices_unique deferred;
  assert (select s.name from public.option_set s where s.option_set_id = s1) = 'zz never / zz sometimes / zz often',
    'a set''s name is its choices, in its order';
  assert not exists (select 1 from information_schema.columns where table_name = 'option_set' and column_name = 'name'),
    'and nobody types it';
  set local role anon;
  assert (select public.name(s) from public.option_set s where s.option_set_id = s1) = 'zz never / zz sometimes / zz often',
    'the dashboard reads it';
  reset role;

  -- the same choices in another order are the same set
  begin
    insert into public.option_set default values returning option_set_id into s2;
    insert into public.answer_option_set (option_set_id, answer_option_id, display_order)
      values (s2, often, 1), (s2, sometimes, 2), (s2, never, 3);
    set constraints option_set_choices_unique immediate;
    raise exception 'went through';
  exception when others then
    assert sqlerrm = 'The option set "zz never / zz sometimes / zz often" already has exactly these choices. A set is its choices, so there is one of each.',
      'a second set with the same choices is refused: ' || sqlerrm;
  end;
  -- one more choice is another set
  set constraints option_set_choices_unique deferred;
  insert into public.option_set default values returning option_set_id into s2;
  insert into public.answer_option_set (option_set_id, answer_option_id, display_order)
    values (s2, never, 1), (s2, sometimes, 2), (s2, often, 3), (s2, always, 4);
  set constraints option_set_choices_unique immediate;
  set constraints option_set_choices_unique deferred;
  -- a place in the order holds one choice (checked at once here: apply_draft(), run by the checks above, deferred
  -- every constraint for the rest of the transaction)
  set constraints answer_option_set_display_order_unique immediate;
  perform pg_temp.expect_error(format('insert into public.answer_option_set (option_set_id, answer_option_id, display_order) values (%s, %s, 1)', s1, always),
    'answer_option_set_display_order_unique');
  set constraints answer_option_set_display_order_unique deferred;
end;
$$;

rollback;
select 'All checks passed.' as result;


-- =====================================================================================================
-- STAFF ACCESS: what a signed-in staff member may read directly from the database.
-- =====================================================================================================
--
-- `auth.jwt()` is the signed-in person's token; its `email` is the account's email, which `staff.email`
-- matches ignoring case (staff_email_unique). Adding `staff.auth_user_id uuid` later, and matching on
-- `auth.uid()` instead, would survive an email change.

begin;

create or replace function public.is_active_staff()
returns boolean
language sql
stable
security definer
set search_path = public, pg_temp
as $$
  select exists (
    select 1 from public.staff s
    where s.active and lower(s.email) = lower(coalesce(auth.jwt() ->> 'email', ''))
  );
$$;

revoke all on function public.is_active_staff() from public, anon;
grant execute on function public.is_active_staff() to authenticated, service_role;

comment on function public.is_active_staff() is
  'Whether the signed-in person (auth.jwt() email) is an active row of staff. For the row policies on the private tables.';

-- Read only, and only for staff. (Row Level Security is already on for every table in public.)
do $$
declare
  t text;
begin
  foreach t in array array['staff', 'draft', 'edit_draft', 'extracted_draft', 'ai_extracted_draft', 'pdf_file',
                           'wave_file', 'draft_review_event', 'applied_item'] loop
    execute format('grant select on public.%I to authenticated', t);
    execute format('revoke insert, update, delete on public.%I from authenticated', t);
    execute format('revoke all on public.%I from anon', t);
    execute format('drop policy if exists "Staff can read" on public.%I', t);
    execute format('create policy "Staff can read" on public.%I for select to authenticated using (public.is_active_staff())', t);
  end loop;
end;
$$;

commit;

-- =====================================================================================================
-- PUT BACK THE KEPT ROWS: staff, component_type, subject, time_frame, principal_investigator, respondent_type,
-- registry, study, questionnaire, wave, pdf_file and wave_file, as part 0 saved them in matr_keep
-- =====================================================================================================
-- After the checks, which expect a clean build. Rows come back with their IDs, each table after the ones
-- it points at. A table with rows kept has exactly those (they replace its starting rows, so a starting
-- component type or subject that was deleted stays deleted); a table with none kept keeps the starting
-- rows. The other tables have no starting rows. A column the old table didn't have is left empty
-- (or its default). The waves come back asking no questions: what the questionnaires print is not
-- kept. On the test project every other table was kept too, and comes back after these, each after the
-- tables it points at, with its own triggers off while its rows go in (the rows were already checked
-- when they were first made; the foreign keys stay on). matr_keep goes once the rows are back. If this part fails, nothing in it happens and matr_keep stays: fix the cause and run the
-- whole file again.

begin;

do $$
declare
  n     bigint;
  t     text;
  rest  text[];
  ready text[];
  keys  text[];
  cols  text;
  vals  text;
begin
  if to_regnamespace('matr_keep') is null then
    return;   -- nothing was kept (a new project)
  end if;

  if to_regclass('matr_keep.component_type') is not null then
    if exists (select 1 from matr_keep.component_type) then
      -- the kinds the site is built around come back even if the kept list predates one (Grid)
      create temp table built_in_kind on commit drop as
        select name, description from public.component_type where name in ('Section', 'Question', 'Grid', 'Group');
      delete from public.component_type;
      insert into public.component_type (component_type_id, name, description)
      select (kept ->> 'component_type_id')::bigint, kept ->> 'name', kept ->> 'description'
      from matr_keep.component_type;
      get diagnostics n = row_count;
      raise notice 'Put back % component type(s).', n;
      insert into public.component_type (name, description)
      select b.name, b.description from built_in_kind b
      where not exists (select 1 from public.component_type t where t.name = b.name);
      get diagnostics n = row_count;
      if n > 0 then
        raise notice 'Added % built-in component type(s) the kept list did not have.', n;
      end if;
    end if;
  end if;

  if to_regclass('matr_keep.subject') is not null then
    if exists (select 1 from matr_keep.subject) then
      delete from public.subject;
      -- with straight quotes: a name kept from before the rule had them curly
      insert into public.subject (subject_id, name)
      select (kept ->> 'subject_id')::bigint, translate(kept ->> 'name', '‘’“”', '''''""')
      from matr_keep.subject;
      get diagnostics n = row_count;
      raise notice 'Put back % subject(s).', n;
    end if;
  end if;

  if to_regclass('matr_keep.time_frame') is not null then
    if exists (select 1 from matr_keep.time_frame) then
      delete from public.time_frame;
      insert into public.time_frame (time_frame_id, name)
      select (kept ->> 'time_frame_id')::bigint, kept ->> 'name'
      from matr_keep.time_frame;
      get diagnostics n = row_count;
      raise notice 'Put back % time frame(s).', n;
    end if;
  end if;

  if to_regclass('matr_keep.staff') is not null then
    insert into public.staff (staff_id, name, email, active, role)
    select (kept ->> 'staff_id')::bigint, kept ->> 'name', kept ->> 'email',
           coalesce((kept ->> 'active')::boolean, true), coalesce(kept ->> 'role', 'STAFF')
    from matr_keep.staff;
    get diagnostics n = row_count;
    raise notice 'Put back % staff member(s).', n;
  end if;

  if to_regclass('matr_keep.principal_investigator') is not null then
    insert into public.principal_investigator (principal_investigator_id, name, email)
    select (kept ->> 'principal_investigator_id')::bigint, kept ->> 'name', kept ->> 'email'
    from matr_keep.principal_investigator;
    get diagnostics n = row_count;
    raise notice 'Put back % investigator(s).', n;
  end if;

  if to_regclass('matr_keep.respondent_type') is not null then
    insert into public.respondent_type (respondent_type_id, name, minimum_age, maximum_age, description)
    select (kept ->> 'respondent_type_id')::bigint, kept ->> 'name', (kept ->> 'minimum_age')::integer,
           (kept ->> 'maximum_age')::integer, kept ->> 'description'
    from matr_keep.respondent_type;
    get diagnostics n = row_count;
    raise notice 'Put back % respondent type(s).', n;
  end if;

  if to_regclass('matr_keep.pdf_file') is not null then
    insert into public.pdf_file (pdf_sha256, pdf_pages, pdf_bytes)
    select kept ->> 'pdf_sha256', (kept ->> 'pdf_pages')::integer, (kept ->> 'pdf_bytes')::bigint
    from matr_keep.pdf_file;
    get diagnostics n = row_count;
    raise notice 'Put back % PDF(s).', n;
  end if;

  if to_regclass('matr_keep.registry') is not null then
    insert into public.registry (registry_id, name, abbreviation, participant_count, description)
    select (kept ->> 'registry_id')::bigint, kept ->> 'name', kept ->> 'abbreviation',
           (kept ->> 'participant_count')::integer, kept ->> 'description'
    from matr_keep.registry;
    get diagnostics n = row_count;
    raise notice 'Put back % registr(y/ies).', n;
  end if;

  if to_regclass('matr_keep.study') is not null then
    insert into public.study (study_id, registry_id, principal_investigator_id, contact_principal_investigator_id,
                              name, status, description, methods, acknowledgements)
    select (kept ->> 'study_id')::bigint, (kept ->> 'registry_id')::bigint, (kept ->> 'principal_investigator_id')::bigint,
           (kept ->> 'contact_principal_investigator_id')::bigint, kept ->> 'name', kept ->> 'status',
           kept ->> 'description', kept ->> 'methods', kept ->> 'acknowledgements'
    from matr_keep.study;
    get diagnostics n = row_count;
    raise notice 'Put back % stud(y/ies).', n;
  end if;

  if to_regclass('matr_keep.questionnaire') is not null then
    insert into public.questionnaire (questionnaire_id, study_id, respondent_type_id, name, abbreviation,
                                      upvote_count, participant_count, description)
    select (kept ->> 'questionnaire_id')::bigint, (kept ->> 'study_id')::bigint, (kept ->> 'respondent_type_id')::bigint,
           kept ->> 'name', kept ->> 'abbreviation', coalesce((kept ->> 'upvote_count')::integer, 0),
           (kept ->> 'participant_count')::integer, kept ->> 'description'
    from matr_keep.questionnaire;
    get diagnostics n = row_count;
    raise notice 'Put back % questionnaire(s).', n;
  end if;

  if to_regclass('matr_keep.wave') is not null then
    insert into public.wave (wave_id, questionnaire_id, name, respondent_count, administered_date)
    select (kept ->> 'wave_id')::bigint, (kept ->> 'questionnaire_id')::bigint, kept ->> 'name',
           (kept ->> 'respondent_count')::integer, (kept ->> 'administered_date')::date
    from matr_keep.wave;
    get diagnostics n = row_count;
    raise notice 'Put back % wave(s).', n;
  end if;

  if to_regclass('matr_keep.wave_file') is not null then
    insert into public.wave_file (wave_id, pdf_sha256, pdf_name, supplement_name, supplement_sha256, supplement_text)
    select (kept ->> 'wave_id')::bigint, kept ->> 'pdf_sha256', kept ->> 'pdf_name', kept ->> 'supplement_name',
           kept ->> 'supplement_sha256', kept ->> 'supplement_text'
    from matr_keep.wave_file;
    get diagnostics n = row_count;
    raise notice 'Put back % wave''s file record(s).', n;
  end if;

  -- The test project's other tables, each after the ones it points at
  rest := array(select c.relname::text from pg_class c
                where c.relnamespace = 'matr_keep'::regnamespace and c.relkind = 'r'
                  and c.relname <> all (array['staff', 'component_type', 'subject', 'time_frame', 'principal_investigator',
                                              'respondent_type', 'registry', 'study', 'questionnaire', 'wave',
                                              'pdf_file', 'wave_file'])
                order by 1);
  foreach t in array rest loop
    if to_regclass('public.' || t) is null then
      execute format('select count(*) from matr_keep.%I', t) into n;
      raise notice 'Not putting back % row(s) of %: the database has no such table now.', n, t;
      rest := array_remove(rest, t);
    end if;
  end loop;
  while cardinality(rest) > 0 loop
    ready := array(select r from unnest(rest) r
                   where not exists (select 1 from pg_constraint f, unnest(rest) u
                                     where f.contype = 'f' and f.conrelid = ('public.' || r)::regclass
                                       and u <> r and f.confrelid = ('public.' || u)::regclass)
                   order by 1);
    if cardinality(ready) = 0 then
      raise exception 'Can''t put back %: they point at each other, so none can go first.', array_to_string(rest, ', ');
    end if;
    foreach t in array ready loop
      -- the columns the kept rows have and the new table still has
      execute format('select array(select distinct jsonb_object_keys(kept) from matr_keep.%I)', t) into keys;
      select string_agg(format('%I', a.attname), ', ' order by a.attnum),
             string_agg(format('r.%I', a.attname), ', ' order by a.attnum)
      into cols, vals
      from pg_attribute a
      where a.attrelid = ('public.' || t)::regclass and a.attnum > 0 and not a.attisdropped
        and a.attgenerated = '' and a.attname = any (keys);
      n := 0;
      if cols is not null then
        execute format('alter table public.%I disable trigger user', t);
        execute format('insert into public.%I (%s) select %s from matr_keep.%I k cross join lateral jsonb_populate_record(null::public.%I, k.kept) r',
                       t, cols, vals, t, t);
        get diagnostics n = row_count;
        execute format('alter table public.%I enable trigger user', t);
      end if;
      raise notice 'Put back % row(s) of %.', n, t;
    end loop;
    rest := array(select r from unnest(rest) r where r <> all (ready));
  end loop;

  drop schema matr_keep cascade;
end;
$$;

commit;


-- the REST API learns the new tables and functions
notify pgrst, 'reload schema';
