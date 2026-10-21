-- =====================================================================================================
-- MATR metadata database: the whole database in one script (Supabase, PostgreSQL 15+)
--
-- Builds every table, rule, function and permission the site and the public dashboard use, on a
-- Supabase project: a new one, or one to start over. Part 0 first removes whatever tables and functions
-- are there, so the build starts clean, and the last part puts back three tables' rows:
--
--   KEPT:  staff, component_type and subject, with their IDs. They are copied to a schema of their
--          own (matr_keep) before anything is removed, and back once the database is built.
--   LOST:  every other row (registries, studies, questionnaires, drafts, their history, ...).
--
-- Part 0 refuses to run if any other table holds a row, so it can't empty a database that is in use.
-- Rows left from trying the site out can be cleared anyway, on purpose: see the top of part 0. Once
-- real data is in, change the database with migrations instead of running this again.
--
-- Sign-in accounts (Authentication > Users) and Storage are not touched.
--
--   0. Clear the schemas           keep staff, component_type and subject; remove public and private
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
--   Put back the kept rows         staff, component_type and subject, from matr_keep
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
-- 0. CLEAR THE SCHEMAS (KEEPING STAFF, COMPONENT TYPES AND SUBJECTS)
-- =====================================================================================================
-- Copies the kept tables' rows to matr_keep, then removes the schemas public and private with everything
-- in them, and makes public again with the permissions a new Supabase project starts with (the build
-- expects a new project, and takes the permissions it doesn't want away again).
--
-- If the other tables hold only rows you mean to throw away (left from trying the site out), and you
-- have checked which project this is, uncomment the next line to clear them anyway. EVERY ROW OF THEM
-- IS LOST.
set matr.clear_with_data = 'yes';

begin;

-- Nothing else may hold data, unless clearing it was asked for
do $$
declare
  t       regclass;
  holding text[] := array[]::text[];
  has_row boolean;
begin
  for t in
    select c.oid::regclass from pg_class c
    where c.relnamespace in (select oid from pg_namespace where nspname in ('public', 'private'))
      and c.relkind in ('r', 'p')
      and not (c.relnamespace = 'public'::regnamespace and c.relname in ('staff', 'component_type', 'subject'))   -- kept
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

-- The kept rows, each as JSON, so they come back whatever columns the old tables had
do $$
declare
  t text;
  n bigint;
begin
  if to_regnamespace('matr_keep') is not null then
    raise notice 'matr_keep is already there (an earlier run stopped before putting its rows back): keeping those rows.';
    return;
  end if;
  create schema matr_keep;
  foreach t in array array['staff', 'component_type', 'subject'] loop
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
  email  text not null
);

create table public.study (
  study_id                          bigint primary key default private.random_id('study', 'study_id'),
  registry_id                       bigint not null,                     -- BR1, BR2
  principal_investigator_id         bigint not null,                     -- BR3, BR4
  contact_principal_investigator_id bigint,                              -- BR6-BR9
  name              text not null,
  status            text not null
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

-- BR14: no uniqueness on name or abbreviation; questionnaire_id is the only identifier.
create table public.questionnaire (
  questionnaire_id   bigint  primary key default private.random_id('questionnaire', 'questionnaire_id'),
  study_id           bigint  not null,                                   -- BR10
  respondent_type_id bigint  not null,                                   -- BR12, BR13
  name               text    not null,
  abbreviation       text    not null,
  upvote_count       integer not null default 0                          -- BR16
    constraint questionnaire_upvote_count_nonnegative check (upvote_count >= 0),
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

create table public.component (
  component_id      bigint primary key default private.random_id('component', 'component_id'),
  component_type_id bigint not null,                                     -- BR39, BR40
  text              text                                                 -- empty only for Sections (BR42, site)
);

-- QUESTION is a subtype of COMPONENT with no attributes of its own (BR43, BR44).
create table public.question (
  component_id bigint primary key
);

create table public.questionnaire_component (
  questionnaire_id    bigint  not null,                                  -- BR46, BR47
  component_id        bigint  not null,                                  -- BR48, BR49
  parent_component_id bigint,
  display_order       integer not null
    constraint questionnaire_component_display_order_check check (display_order > 0),
  primary key (questionnaire_id, component_id),
  -- BR54, the direct case (a trigger catches longer cycles)
  constraint questionnaire_component_not_own_parent check (parent_component_id <> component_id),
  -- BR55: display order unique among siblings; NULLS NOT DISTINCT makes the top level one group.
  -- Deferrable so a change can move components past each other.
  constraint questionnaire_component_display_order_unique
    unique nulls not distinct (questionnaire_id, parent_component_id, display_order)
    deferrable initially immediate
);

create table public.questionnaire_question (
  questionnaire_id   bigint  not null,
  component_id       bigint  not null,
  parent_question_id bigint,
  self_contained     boolean not null default true,
  condition          text,
  primary key (questionnaire_id, component_id),
  -- BR63, the direct case (a trigger catches longer cycles)
  constraint questionnaire_question_not_own_parent check (parent_question_id <> component_id)
);

create table public.wave_question (
  questionnaire_id bigint not null,
  wave_id          bigint not null,
  component_id     bigint not null,
  question_number  text   not null,      -- text: printed numbers contain letters (1a, 1d)
  primary key (questionnaire_id, wave_id, component_id),
  -- BR71: question number unique within a wave (deferrable for renumbering)
  constraint wave_question_number_unique unique (wave_id, question_number) deferrable initially immediate
);

-- ------ Answers --------------------------------------------------------------------------------------

create table public.subject (
  subject_id bigint primary key default private.random_id('subject', 'subject_id'),
  name       text   not null
    constraint subject_name_not_blank check (btrim(name) <> '')
  -- one subject for each name, ignoring case and outer spaces: subject_name_unique, section 3
);

-- variable_key is deliberately NOT called variable_id: matr_id holds what MATR staff call the
-- "variable ID" (e.g. AH25LF).
create table public.variable (
  variable_key     bigint  primary key default private.random_id('variable', 'variable_key'),
  questionnaire_id bigint  not null,
  component_id     bigint  not null,
  matr_id          text    not null,
  display_order    integer not null
    constraint variable_display_order_check check (display_order > 0),
  label            text,
  respondent_count integer
    constraint variable_respondent_count_check check (respondent_count >= 0),
  subject_id       bigint,
  -- BR93: matr_id unique within its questionnaire, may repeat across questionnaires
  constraint variable_matr_id_unique_in_questionnaire unique (questionnaire_id, matr_id),
  -- BR94: display order unique within its questionnaire question
  constraint variable_display_order_unique
    unique (questionnaire_id, component_id, display_order) deferrable initially immediate
);

create table public.open_ended (
  variable_key     bigint primary key,                                   -- BR88, BR89
  answer_data_type text   not null                                       -- BR95
    constraint open_ended_answer_data_type_check check (answer_data_type in ('NUMBER', 'TEXT', 'DATE', 'TIME'))
);

create table public.option_set (
  option_set_id bigint primary key default private.random_id('option_set', 'option_set_id'),
  name          text   not null
    constraint option_set_name_unique unique,                            -- BR109
  description   text
);

create table public.closed_ended (
  variable_key  bigint primary key,                                      -- BR90, BR91
  option_set_id bigint not null                                          -- BR96, BR97
);

create table public.answer_option (
  answer_option_id   bigint primary key default private.random_id('answer_option', 'answer_option_id'),
  standardized_label text   not null
    constraint answer_option_label_unique unique                         -- BR110
);

create table public.answer_option_set (
  option_set_id    bigint not null,                                      -- BR105
  answer_option_id bigint not null,                                      -- BR107, BR108
  primary key (option_set_id, answer_option_id)
);

create table public.response_option (
  variable_key     bigint  not null,                                     -- BR98
  answer_option_id bigint  not null,                                     -- BR100, BR101
  display_order    integer not null
    constraint response_option_display_order_check check (display_order > 0),
  code_value       text    not null,     -- text: codes may be 1, 0, -9, A ...
  printed_label    text,
  skip_instruction text,
  primary key (variable_key, answer_option_id),
  -- BR103 (and BR104: the same answer option may carry different codes on different variables)
  constraint response_option_display_order_unique
    unique (variable_key, display_order) deferrable initially immediate,
  constraint response_option_code_value_unique
    unique (variable_key, code_value) deferrable initially immediate
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
  name         text   not null
    constraint measurement_scale_name_unique unique,                     -- BR78
  abbreviation text   not null
    constraint measurement_scale_abbreviation_unique unique,             -- BR78
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
  constraint extracted_draft_info_is_an_object check (jsonb_typeof(info) = 'object'),
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
    check (reading_status in ('queued', 'sent', 'received', 'ready', 'failed')),
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

-- Each time a draft is submitted for review, sent back, or approved: who, which revision, when, and the
-- reviewer's note for a send back. draft itself says only where it is now (status, requested_reviewer).
create table public.draft_review_event (
  draft_review_event_id bigint      primary key default private.random_id('draft_review_event', 'draft_review_event_id'),
  draft_id              bigint      not null,
  event                 text        not null
    constraint draft_review_event_event_check check (event in ('Submitted', 'Sent Back', 'Approved')),
  staff_id              bigint      not null,
  revision              integer     not null
    constraint draft_review_event_revision_check check (revision > 0),
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
    foreign key (questionnaire_id) references public.questionnaire (questionnaire_id);

alter table public.component
  add constraint component_component_type_id_fkey
    foreign key (component_type_id) references public.component_type (component_type_id);

alter table public.question
  add constraint question_component_id_fkey
    foreign key (component_id) references public.component (component_id);

alter table public.questionnaire_component
  add constraint questionnaire_component_questionnaire_id_fkey
    foreign key (questionnaire_id) references public.questionnaire (questionnaire_id),
  add constraint questionnaire_component_component_id_fkey
    foreign key (component_id) references public.component (component_id),
  -- BR50-BR52: the parent is in the SAME questionnaire, because questionnaire_id is half of the key
  add constraint questionnaire_component_parent_fk
    foreign key (questionnaire_id, parent_component_id)
    references public.questionnaire_component (questionnaire_id, component_id);

alter table public.questionnaire_question
  add constraint questionnaire_question_component_id_fkey                -- BR58, BR59
    foreign key (component_id) references public.question (component_id),
  -- BR56, BR57: a questionnaire question IS a questionnaire component
  add constraint questionnaire_question_component_fk
    foreign key (questionnaire_id, component_id)
    references public.questionnaire_component (questionnaire_id, component_id),
  -- BR60-BR62: the parent question is in the SAME questionnaire
  add constraint questionnaire_question_parent_fk
    foreign key (questionnaire_id, parent_question_id)
    references public.questionnaire_question (questionnaire_id, component_id);

alter table public.wave_question
  -- BR66, BR67, BR70: the wave and the question share one questionnaire_id
  add constraint wave_question_wave_fk
    foreign key (questionnaire_id, wave_id) references public.wave (questionnaire_id, wave_id),
  add constraint wave_question_question_fk                               -- BR68, BR70
    foreign key (questionnaire_id, component_id)
    references public.questionnaire_question (questionnaire_id, component_id);

alter table public.variable
  -- BR86: a variable records an answer for exactly one questionnaire question
  add constraint variable_questionnaire_question_fk
    foreign key (questionnaire_id, component_id)
    references public.questionnaire_question (questionnaire_id, component_id),
  add constraint variable_subject_fk
    foreign key (subject_id) references public.subject (subject_id);

alter table public.open_ended
  add constraint open_ended_variable_key_fkey
    foreign key (variable_key) references public.variable (variable_key) on delete cascade;

alter table public.closed_ended
  add constraint closed_ended_variable_key_fkey
    foreign key (variable_key) references public.variable (variable_key) on delete cascade,
  add constraint closed_ended_option_set_id_fkey
    foreign key (option_set_id) references public.option_set (option_set_id);

alter table public.answer_option_set
  add constraint answer_option_set_option_set_id_fkey
    foreign key (option_set_id) references public.option_set (option_set_id),
  add constraint answer_option_set_answer_option_id_fkey
    foreign key (answer_option_id) references public.answer_option (answer_option_id);

alter table public.response_option
  add constraint response_option_variable_key_fkey
    foreign key (variable_key) references public.closed_ended (variable_key) on delete cascade,
  add constraint response_option_answer_option_id_fkey
    foreign key (answer_option_id) references public.answer_option (answer_option_id);

alter table public.question_category
  add constraint question_category_component_id_fkey
    foreign key (component_id) references public.question (component_id),
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

-- Uniqueness that ignores case or spaces (an index, as a unique constraint can't hold an expression)
create unique index staff_email_unique  on public.staff   (lower(email));          -- BR22
create unique index subject_name_unique on public.subject (lower(btrim(name)));

-- Foreign keys
create index study_registry_id_idx                 on public.study (registry_id);
create index study_principal_investigator_id_idx   on public.study (principal_investigator_id);
create index study_contact_pi_id_idx               on public.study (contact_principal_investigator_id);
create index questionnaire_study_id_idx            on public.questionnaire (study_id);
create index questionnaire_respondent_type_idx     on public.questionnaire (respondent_type_id);
create index component_component_type_id_idx       on public.component (component_type_id);
create index questionnaire_component_component_idx on public.questionnaire_component (component_id);
create index questionnaire_question_component_idx  on public.questionnaire_question (component_id);
create index questionnaire_question_parent_idx     on public.questionnaire_question (questionnaire_id, parent_question_id);
create index wave_question_question_idx            on public.wave_question (questionnaire_id, component_id);
create index variable_subject_id_idx               on public.variable (subject_id);
create index closed_ended_option_set_id_idx        on public.closed_ended (option_set_id);
create index answer_option_set_answer_option_idx   on public.answer_option_set (answer_option_id);
create index response_option_answer_option_idx     on public.response_option (answer_option_id);
create index question_category_mesh_id_idx         on public.question_category (mesh_id);
create index question_scale_component_id_idx       on public.question_scale (component_id);
create index question_scale_expected_set_idx       on public.question_scale (expected_option_set_id);
create index draft_prepared_by_idx                 on public.draft (prepared_by);
create index draft_saved_by_idx                    on public.draft (saved_by);
create index draft_requested_reviewer_idx          on public.draft (requested_reviewer);
create index draft_editing_by_idx                  on public.draft (editing_by);
create index extracted_draft_pdf_sha256_idx        on public.extracted_draft (pdf_sha256);
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

-- BR54: a questionnaire component is not its own ancestor.
create function public.questionnaire_component_no_cycle()
returns trigger
language plpgsql
set search_path = ''
as $$
begin
  if new.parent_component_id is null then
    return new;
  end if;
  if exists (
    with recursive ancestors (component_id) as (
      select new.parent_component_id
      union
      select qc.parent_component_id
      from public.questionnaire_component qc
      join ancestors a on qc.component_id = a.component_id
      where qc.questionnaire_id = new.questionnaire_id
        and qc.parent_component_id is not null
    )
    select 1 from ancestors where component_id = new.component_id
  ) then
    raise exception 'Component % would become its own ancestor in questionnaire % (BR54)',
      new.component_id, new.questionnaire_id
      using errcode = 'check_violation';
  end if;
  return new;
end;
$$;

create trigger questionnaire_component_no_cycle
  before insert or update of parent_component_id on public.questionnaire_component
  for each row execute function public.questionnaire_component_no_cycle();

-- BR63: a questionnaire question is not its own ancestor through the parent chain.
create function public.questionnaire_question_no_cycle()
returns trigger
language plpgsql
set search_path = ''
as $$
begin
  if new.parent_question_id is null then
    return new;
  end if;
  if exists (
    with recursive ancestors (component_id) as (
      select new.parent_question_id
      union
      select qq.parent_question_id
      from public.questionnaire_question qq
      join ancestors a on qq.component_id = a.component_id
      where qq.questionnaire_id = new.questionnaire_id
        and qq.parent_question_id is not null
    )
    select 1 from ancestors where component_id = new.component_id
  ) then
    raise exception 'Question % would become its own ancestor in questionnaire % (BR63)',
      new.component_id, new.questionnaire_id
      using errcode = 'check_violation';
  end if;
  return new;
end;
$$;

create trigger questionnaire_question_no_cycle
  before insert or update of parent_question_id on public.questionnaire_question
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

-- The details of an applied draft stay as they were applied.
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
-- was submitted, by p_staff_id, at its current revision.
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
    insert into public.draft_review_event (draft_id, event, staff_id, revision)
    values (d.draft_id, 'Submitted', p_staff_id, d.revision);
  end if;
end;
$$;

-- Applies a draft's items to the live tables, records each field in applied_item, records the approval
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
-- Constraints that can be deferred are deferred to the end, so rows can be put in order (display
-- orders) and the answer-format check (BR92) looks at the finished change.
--
-- An edit can change the tables Make an edit knows (R/model_table.R; a test checks the list against
-- that file). An extraction only adds rows (Insert), to the tables a questionnaire is made of.
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
    'wave_question', 'category', 'question_category', 'answer_option', 'subject', 'answer_option_set',
    'measurement_scale', 'question_scale', 'variable', 'open_ended', 'closed_ended', 'response_option'
  ];
  loadable constant text[] := array[
    'questionnaire', 'wave', 'component', 'questionnaire_component', 'question', 'questionnaire_question',
    'wave_question', 'variable', 'open_ended', 'closed_ended', 'option_set', 'answer_option',
    'answer_option_set', 'response_option'
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

  insert into public.draft_review_event (draft_id, event, staff_id, revision, note)
  values (d.draft_id, 'Sent Back', p_staff_id, d.revision, p_note);
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
    'wave_question', 'subject', 'variable', 'open_ended', 'closed_ended', 'option_set',
    'answer_option', 'answer_option_set', 'response_option', 'category', 'question_category',
    'measurement_scale', 'question_scale'
  ];
begin
  foreach t in array dashboard_tables loop
    execute format('create policy "Dashboard can read" on public.%I for select to anon, authenticated using (true)', t);
    execute format('grant select on public.%I to anon, authenticated', t);
  end loop;
end;
$$;

grant execute on function public.upvote_questionnaire(bigint) to anon, authenticated;

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
comment on table public.component               is 'One piece of printed material, held once for the whole registry.';
comment on table public.question                is 'Subtype of component that requests one or more answers.';
comment on table public.questionnaire_component is 'Where a questionnaire printed a component: parent and display order.';
comment on table public.questionnaire_question  is 'A question as printed in one questionnaire; variables attach here.';
comment on table public.wave_question           is 'That a wave actually asked a question, with its printed number in that wave.';
comment on table public.subject                 is 'Who a variable can be about. Kept by staff (Make an edit, kind Subject); read by the dashboard.';
comment on table public.variable                is 'One place an answer is recorded; one column in the response database.';
comment on table public.open_ended              is 'Variable the respondent writes.';
comment on table public.closed_ended            is 'Variable chosen from an option set.';
comment on table public.option_set              is 'Named collection of answer options, independent of order and coding.';
comment on table public.answer_option           is 'One answer choice, stored once for the whole registry.';
comment on table public.answer_option_set       is 'Links answer options to option sets.';
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
comment on table public.pdf_file                is 'A PDF uploaded for an extraction, once per file (by its SHA-256): its pages and size. The file is kept in uploads/pdfs/<hash>.pdf until Storage is used.';
comment on table public.draft_review_event      is 'Each time a draft was submitted for review, sent back, or approved: by whom, at which revision, and when.';
comment on table public.applied_item            is 'One field an applied draft changed, with its old and new value.';

-- ------ Columns --------------------------------------------------------------------------------------
comment on column public.study.status is 'Active: the study is ongoing. Ended: all data collections are finished.';
comment on column public.questionnaire_component.display_order is 'Position among components sharing the same parent, counting from 1.';
comment on column public.variable.variable_key is 'System key. Deliberately not "variable_id"; see matr_id.';
comment on column public.variable.matr_id is 'What MATR staff call the variable ID, e.g. AH25LF. Unique within a questionnaire; never changes.';
comment on column public.variable.subject_id is 'Who the variable is about (public.subject). Null when not known.';
comment on column public.response_option.code_value is 'Stored value for this option in the response data, e.g. 1 for Yes.';

comment on column public.staff.role is
  'STAFF: deleting, or pushing an extraction, needs one approval from another staff member. ADMIN: never needs approval.';

comment on column public.draft.kind is 'Extraction or Edit; says which subtype table holds the rest.';
comment on column public.draft.status is
  'Draft (being prepared), Awaiting Review (a change request), Sent Back (a reviewer returned it), Applied (in the MATR data) or Withdrawn (the preparer pulled it). When it was submitted, sent back or approved is in draft_review_event.';
comment on column public.draft.prepared_by is 'Who started it. Never changes.';
comment on column public.draft.saved_by is 'Who saved it last (or sent it back). An approver can''t be this person.';
comment on column public.draft.requested_reviewer is
  'The staff member the preparer asked to review it (they are emailed a link once there is a login). Any other eligible staff member may still approve.';
comment on column public.draft.revision is 'Rises every time the content is saved. An approval is of the revision the approver read.';
comment on column public.draft.summary is 'One line saying what it comes to; a change request is listed by it.';
comment on column public.draft.saved_at is 'When staff last saved it. The reading job''s progress does not change it.';

comment on column public.edit_draft.edit_kind is 'What the preparer chose: Add, Update or Delete.';
comment on column public.edit_draft.form is 'The sentence and the form as typed, to open it again: {action, entity, scope, target, values}.';
comment on column public.edit_draft.reach is 'For a delete: how many rows go with it, and how many are emptied.';
comment on column public.edit_draft.items is
  'The rows and fields it changes, in the order to apply them: item_number, entity_name, record_identifier, action, attribute_name, old_value, new_value.';

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
  'The questionnaire exactly as the AI returned it. Written once by the reading job and never changed, so what the reviewer changed can be measured.';
comment on column public.ai_extracted_draft.review_edits is
  'The reviewer''s changes to what the AI returned, added to each time they save: [{at, by, by_name, change, item, where, field, before, after}].';
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
comment on column public.draft_review_event.revision is
  'The revision submitted, sent back or approved.';
comment on column public.draft_review_event.note is 'Sent Back only: what the reviewer asks to be changed.';

comment on column public.draft.applied_at is 'When it was applied to the MATR data. Empty until then.';
comment on column public.draft.editing_by is
  'Who has it open on the Edit Extraction page, if anyone (claim_draft()). Empty when nobody does.';
comment on column public.draft.editing_at is
  'When their page last renewed the claim. One not renewed for a few minutes has lapsed, and anyone may claim the draft.';
comment on column public.applied_item.record_identifier is 'The row''s key as JSON, with the numbers the database gave rows the draft added.';
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
  ('Note',               'Any other printed remark.');

-- Who a variable can be about; staff keep this list on Make an edit.
insert into public.subject (name) values
  ('Self'), ('Respondent''s Family'), ('Respondent''s Children'), ('Respondent''s Siblings');

commit;

-- =====================================================================================================
-- Rules the site and the dashboard enforce, not this script:
--   BR5  lead PI never changes          BR17 one upvote per session
--   BR23 staff never deleted            BR38 every write recorded (true of changes made through
--                                            drafts: an applied draft and its applied_item rows)
--   BR42 only Sections may have no text BR45 Question-type components have a QUESTION row
--   BR53 a parent is a Section          BR64 non-self-contained question names a parent
--   BR65 non-self-contained hidden from standalone search
--   BR69 questionnaire question asked in >= 1 wave
--   BR80 scale has >= 1 question        BR87 questionnaire question has >= 1 variable
--   BR99 closed-ended has >= 1 option   BR102 response options match the option set
--   BR106 option set has >= 1 answer option
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
insert into public.component_type (name) values ('ZZ Section') on conflict do nothing;
insert into ctx select 'section_type', component_type_id from public.component_type where name = 'ZZ Section';
insert into public.component (component_type_id, text) select pg_temp.id('section_type'), 'Section A';
insert into ctx select 'section_a', component_id from public.component where text = 'Section A' and component_type_id = pg_temp.id('section_type');
insert into public.questionnaire_component (questionnaire_id, component_id, display_order)
  select pg_temp.id('questionnaire'), pg_temp.id('section_a'), 1;

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
        or (table_name = 'response_option' and column_name = 'printed'))), 'no column the migrations removed';
  assert to_regclass('public.pending_draft') is null and to_regclass('public.pending_draft_approval') is null, 'pending_draft is gone';
  assert to_regclass('public.draft_approval') is null and to_regclass('public.write_history') is null
     and to_regclass('public.write_history_item') is null, 'the applied draft is the history: no separate tables';
  assert to_regprocedure('public.apply_pending_edit(bigint)') is null, 'and its functions';
  -- every table's own ID is a random 9-digit number the database picks
  assert (select count(*) from pg_attrdef ad join pg_class c on c.oid = ad.adrelid
          where c.relnamespace = 'public'::regnamespace and pg_get_expr(ad.adbin, ad.adrelid) like '%random_id(%') = 16, 'sixteen tables number their rows';
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
end;
$$;

-- ------ Who can see what ---------------------------------------------------------------------
do $$
declare t text;
begin
  foreach t in array array['staff', 'applied_item', 'draft', 'extracted_draft', 'edit_draft',
                           'ai_extracted_draft', 'pdf_file', 'draft_review_event'] loop
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
    'subject', 'variable', 'open_ended', 'closed_ended', 'option_set', 'answer_option', 'answer_option_set',
    'response_option', 'category', 'question_category', 'measurement_scale', 'question_scale'] loop
    assert has_table_privilege('anon', 'public.' || t, 'select'), t || ' is readable by the dashboard';
    assert not has_table_privilege('anon', 'public.' || t, 'insert, update, delete, truncate'), t || ' is read-only to the dashboard';
    assert (select count(*) from pg_policies where schemaname = 'public' and tablename = t) = 1, t || ' has its one policy';
    assert has_table_privilege('service_role', 'public.' || t, 'select, insert, update, delete'), t || ' is the site''s to change';
  end loop;
  assert (select count(*) from pg_class where relnamespace = 'public'::regnamespace and relkind = 'r') = 32, 'thirty-two tables';
  assert not exists (select 1 from pg_class where relnamespace = 'public'::regnamespace and relkind = 'r' and not relrowsecurity), 'RLS on all of them';
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
  assert (select count(*) from public.draft_review_event where draft_id = e and event = 'Submitted' and staff_id = pg_temp.id('ana') and revision = 1) = 1, 'a Submitted event';
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
  perform pg_temp.expect_error(format('insert into public.draft_review_event (draft_id, event, staff_id, revision, note) values (%s, ''Submitted'', %s, 1, ''x'')', e, pg_temp.id('ana')), 'draft_review_event_note_is_a_send_back');
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
  assert (select array_agg(event || ' ' || revision order by event || ' ' || revision) from public.draft_review_event where draft_id = e)
       = array['Sent Back 1', 'Submitted 1', 'Submitted 2'], 'every submission and send back is kept';
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
    pg_temp.item(3, 'questionnaire_component', jsonb_build_object('component_id', 'new:component:1', 'questionnaire_id', pg_temp.id('questionnaire')), 'Insert', 'display_order', null, '2'),
    pg_temp.item(4, 'subject', jsonb_build_object('subject_id', 'new:subject:1'), 'Insert', 'name', null, 'ZZ Test Subject')), 'Draft');
  made bigint;
  new_component bigint;
begin
  made := public.apply_draft(e, pg_temp.id('ben'));
  select component_id into new_component from public.component where text = 'Section B';
  assert new_component between 100000000 and 999999999, 'the component was added, with a random 9-digit ID';
  assert (select count(*) from public.questionnaire_component where questionnaire_id = pg_temp.id('questionnaire') and component_id = new_component and display_order = 2) = 1,
    'the placement points at the number the database gave the new component';
  assert (select count(*) from public.subject where name = 'ZZ Test Subject') = 1, 'the subject was added';
  assert (select edit_kind from public.edit_draft where draft_id = made) = 'Add', 'kind Add';
  assert (select record_identifier from public.applied_item where draft_id = made and item_number = 3)
    = format('{"component_id":%s,"questionnaire_id":%s}', new_component, pg_temp.id('questionnaire')), 'history: the real number in the key';
  assert not exists (select 1 from public.applied_item where draft_id = made and record_identifier like '%new:%'), 'no placeholder is left';
  insert into ctx values ('section_b', new_component);
end;
$$;

-- ------ apply_draft: a change that points an existing row at a new one ---------------------------------------
-- Like changing a section's words: a new component takes the old one's place, what was inside is moved to it.
do $$
declare
  edit bigint;
  child bigint;
  made bigint;
  new_component bigint;
begin
  insert into public.component (component_type_id, text) values (pg_temp.id('section_type'), 'Child') returning component_id into child;
  insert into public.questionnaire_component (questionnaire_id, component_id, parent_component_id, display_order)
    values (pg_temp.id('questionnaire'), child, pg_temp.id('section_b'), 1);
  edit := pg_temp.edit(pg_temp.id('cy'), 'update', jsonb_build_array(
    pg_temp.item(1, 'component', jsonb_build_object('component_id', 'new:component:1'), 'Insert', 'component_type_id', null, pg_temp.id('section_type')::text),
    pg_temp.item(2, 'component', jsonb_build_object('component_id', 'new:component:1'), 'Insert', 'text', null, 'Section B, reworded'),
    pg_temp.item(3, 'questionnaire_component', jsonb_build_object('component_id', 'new:component:1', 'questionnaire_id', pg_temp.id('questionnaire')), 'Insert', 'display_order', null, '2'),
    pg_temp.item(4, 'questionnaire_component', jsonb_build_object('component_id', child, 'questionnaire_id', pg_temp.id('questionnaire')), 'Update', 'parent_component_id', pg_temp.id('section_b')::text, 'new:component:1'),
    pg_temp.item(5, 'questionnaire_component', jsonb_build_object('component_id', pg_temp.id('section_b'), 'questionnaire_id', pg_temp.id('questionnaire')), 'Delete', 'parent_component_id', null, null),
    pg_temp.item(6, 'questionnaire_component', jsonb_build_object('component_id', pg_temp.id('section_b'), 'questionnaire_id', pg_temp.id('questionnaire')), 'Delete', 'display_order', '2', null)),
    'Draft');
  -- both rows hold display order 2 for a moment; the unique constraint is deferred until the change is done
  made := public.apply_draft(edit, pg_temp.id('cy'));

  select component_id into new_component from public.component where text = 'Section B, reworded';
  assert (select parent_component_id from public.questionnaire_component where questionnaire_id = pg_temp.id('questionnaire') and component_id = child) = new_component,
    'the child now points at the new component';
  assert not exists (select 1 from public.questionnaire_component where component_id = pg_temp.id('section_b')), 'the old placement is gone';
  assert (select new_value from public.applied_item where draft_id = made and item_number = 4) = new_component::text, 'history: the real number as the new value';
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
  assert (select revision from public.draft_review_event where draft_id = made and event = 'Approved') = 2, 'of the revision read';
  assert (select prepared_by from public.draft where draft_id = made) = pg_temp.id('ana'), 'history: the preparer';
  assert (select applied_at is not null from public.draft where draft_id = made)
     and exists (select 1 from public.draft_review_event where draft_id = made and event = 'Approved'), 'history: when';
  assert (select count(*) from public.applied_item where draft_id = made) = 4, 'history: every field';
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
  made := public.apply_draft(e, pg_temp.id('ada'));
  assert not exists (select 1 from public.wave where wave_id = wid), 'the ADMIN deleted it at once';
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
    pg_temp.item(1, 'questionnaire_component', jsonb_build_object('component_id', 'new:component:9', 'questionnaire_id', pg_temp.id('questionnaire')), 'Insert', 'display_order', null, '5')), 'Draft');
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
    pg_temp.item(9, 'questionnaire_component', jsonb_build_object('component_id', 'new:component:1', 'questionnaire_id', 'new:questionnaire:1'), 'Insert', 'display_order', null, '1'),
    pg_temp.item(10, 'component', jsonb_build_object('component_id', 'new:component:2'), 'Insert', 'component_type_id', null, (select component_type_id from public.component_type where name = 'Question')::text),
    pg_temp.item(11, 'component', jsonb_build_object('component_id', 'new:component:2'), 'Insert', 'text', null, 'Pushed question?'),
    pg_temp.item(12, 'question', jsonb_build_object('component_id', 'new:component:2'), 'Insert', 'component_id', null, 'new:component:2'),
    pg_temp.item(13, 'questionnaire_component', jsonb_build_object('component_id', 'new:component:2', 'questionnaire_id', 'new:questionnaire:1'), 'Insert', 'parent_component_id', null, 'new:component:1'),
    pg_temp.item(14, 'questionnaire_component', jsonb_build_object('component_id', 'new:component:2', 'questionnaire_id', 'new:questionnaire:1'), 'Insert', 'display_order', null, '1'),
    pg_temp.item(15, 'questionnaire_question', jsonb_build_object('component_id', 'new:component:2', 'questionnaire_id', 'new:questionnaire:1'), 'Insert', 'self_contained', null, 'true'),
    pg_temp.item(16, 'wave_question', jsonb_build_object('component_id', 'new:component:2', 'questionnaire_id', 'new:questionnaire:1', 'wave_id', 'new:wave:1'), 'Insert', 'question_number', null, '1'),
    pg_temp.item(17, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'questionnaire_id', null, 'new:questionnaire:1'),
    pg_temp.item(18, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'component_id', null, 'new:component:2'),
    pg_temp.item(19, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'matr_id', null, 'ZZP1'),
    pg_temp.item(20, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'display_order', null, '1'),
    pg_temp.item(21, 'open_ended', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'answer_data_type', null, 'TEXT'));

  -- an extraction by a STAFF member needs approval; the rows come with the approval
  e := public.create_draft('Extraction', pg_temp.id('ana'), jsonb_build_object('status', 'Awaiting Review', 'summary', 'Push ZZP'), jsonb_build_object('method', 'Manual'));
  perform pg_temp.expect_error(format('select public.apply_draft(%s, %s, ''[]''::jsonb)', e, pg_temp.id('ana')), 'needs approval from another staff member');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s)', e, pg_temp.id('ben')), 'nothing in this to apply');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s, %L::jsonb)', e, pg_temp.id('ben'),
    jsonb_build_array(pg_temp.item(1, 'registry', jsonb_build_object('registry_id', 'new:registry:1'), 'Insert', 'name', null, 'Not allowed'))), 'touches a table it can''t change \(registry\)');
  perform pg_temp.expect_error(format('select public.approve_draft(%s, 1, %s, %L::jsonb)', e, pg_temp.id('ben'),
    jsonb_build_array(pg_temp.item(1, 'wave', jsonb_build_object('wave_id', pg_temp.id('wave2')), 'Update', 'name', 'Round 2', 'No'))), 'only adds rows');
  made := public.approve_draft(e, 1, pg_temp.id('ben'), items);

  select questionnaire_id into q from public.questionnaire where name = 'ZZ Pushed Questionnaire';
  select wave_id into w from public.wave where questionnaire_id = q and name = 'Wave 1';
  assert q is not null and w is not null, 'the questionnaire and its wave were added';
  assert (select count(*) from public.questionnaire_component where questionnaire_id = q) = 2, 'the section and the question are placed';
  assert (select parent_component_id from public.questionnaire_component qc join public.component c using (component_id) where qc.questionnaire_id = q and c.text = 'Pushed question?')
       = (select component_id from public.component where text = 'Pushed section'), 'the question sits in the section';
  assert (select question_number from public.wave_question where wave_id = w) = '1', 'the wave asked it';
  select variable_key into v from public.variable where questionnaire_id = q and matr_id = 'ZZP1';
  assert v is not null and exists (select 1 from public.open_ended where variable_key = v and answer_data_type = 'TEXT'), 'the variable is write-in';
  assert (select kind from public.draft where draft_id = made) = 'Extraction', 'the history shows an extraction';
  assert (select staff_id from public.draft_review_event where draft_id = made and event = 'Approved') = pg_temp.id('ben'), 'approved by Ben';
  assert (select count(*) from public.applied_item where draft_id = made) = 21, 'every field is in the history';
  assert not exists (select 1 from public.applied_item where draft_id = made and (record_identifier like '%new:%' or new_value like 'new:%')), 'with the real numbers';
  assert (select status from public.draft where draft_id = e) = 'Applied', 'the extraction is applied';

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
      pg_temp.item(2, 'variable', jsonb_build_object('variable_key', 'new:variable:1'), 'Insert', 'component_id', null, (select component_id from public.component where text = 'Pushed question?')::text),
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
  perform pg_temp.expect_error(format('insert into public.draft_review_event (draft_id, event, staff_id, revision) values (%s, ''Submitted'', %s, 1)', e, pg_temp.id('ana')), 'already been applied');
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
  perform pg_temp.expect_error(format('insert into public.draft_review_event (draft_id, event, staff_id, revision) values (%s, ''Approved'', %s, 1)', ap, pg_temp.id('ana')), 'someone other than');
  perform public.save_draft(ap, pg_temp.id('cy'));
  perform pg_temp.expect_error(format('insert into public.draft_review_event (draft_id, event, staff_id, revision) values (%s, ''Approved'', %s, 2)', ap, pg_temp.id('cy')), 'someone other than');
  -- and goes with applying the change (checked when the transaction ends, so it is run here as that would)
  begin
    insert into public.draft_review_event (draft_id, event, staff_id, revision) values (ap, 'Approved', pg_temp.id('ben'), 2);
    set constraints draft_approval_applies immediate;
    raise exception 'went through';
  exception when others then
    assert sqlerrm like '%goes with applying the change%', 'an approval of a draft that isn''t applied is refused: ' || sqlerrm;
  end;
  -- a review is the record of what happened: never changed, and deleted only with its draft
  insert into public.draft_review_event (draft_id, event, staff_id, revision) values (ap, 'Submitted', pg_temp.id('cy'), 2);
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
                           'draft_review_event', 'applied_item'] loop
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
-- PUT BACK THE KEPT ROWS: staff, component_type and subject, as part 0 saved them in matr_keep
-- =====================================================================================================
-- After the checks, which expect a clean build. Rows come back with their IDs. A table with rows kept
-- has exactly those (they replace its starting rows, so a starting component type or subject that was
-- deleted stays deleted); a table with none kept keeps the starting rows. matr_keep goes once the rows
-- are back. If this part fails, nothing in it happens and matr_keep stays: fix the cause and run the
-- whole file again.

begin;

do $$
declare
  n bigint;
begin
  if to_regnamespace('matr_keep') is null then
    return;   -- nothing was kept (a new project)
  end if;

  if to_regclass('matr_keep.component_type') is not null then
    if exists (select 1 from matr_keep.component_type) then
      delete from public.component_type;
      insert into public.component_type (component_type_id, name, description)
      select (kept ->> 'component_type_id')::bigint, kept ->> 'name', kept ->> 'description'
      from matr_keep.component_type;
      get diagnostics n = row_count;
      raise notice 'Put back % component type(s).', n;
    end if;
  end if;

  if to_regclass('matr_keep.subject') is not null then
    if exists (select 1 from matr_keep.subject) then
      delete from public.subject;
      insert into public.subject (subject_id, name)
      select (kept ->> 'subject_id')::bigint, kept ->> 'name'
      from matr_keep.subject;
      get diagnostics n = row_count;
      raise notice 'Put back % subject(s).', n;
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

  drop schema matr_keep cascade;
end;
$$;

commit;


-- the REST API learns the new tables and functions
notify pgrst, 'reload schema';
