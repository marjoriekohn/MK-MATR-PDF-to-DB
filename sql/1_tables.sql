-- =====================================================================================================
-- MATR metadata database, script 1 of 8: TABLES
-- =====================================================================================================
-- The eight scripts in this folder build the whole database on a Supabase project, in order, each run
-- whole in the SQL Editor:
--
--   1_tables.sql        start over, then the tables: columns, primary keys, checks
--   2_references.sql    foreign keys
--   3_indexes.sql       unique rules that ignore case or spaces, and indexes for lookups
--   4_functions.sql     the rules a constraint can't express, and the functions the site calls
--   5_permissions.sql   Row Level Security and grants: what the dashboard, staff and the site may do
--   6_descriptions.sql  comments shown in the Supabase table editor
--   7_seed.sql          the starting rows: registries, studies, questionnaires, waves, lists, staff
--   8_search.sql        the dashboard's search by meaning: an embedding of every question, kept up to date
--
-- Each script is one transaction: if a statement fails, that script changes nothing, and the ones before
-- it stay. Fix the cause and run that script again. Scripts 3 to 8 can be run again on their own at any
-- time (they replace what they made before); 1 and 2 start over.
--
-- WHAT THIS SCRIPT DOES FIRST: it removes the schemas public and private with everything in them, so
-- the build starts clean. EVERY ROW IS LOST, the extracted questionnaires included. Sign-in accounts
-- (Authentication > Users) and the PDFs in Storage are not touched. To protect a database in use, it
-- refuses to run while a questionnaire has content (placements) unless the next line is uncommented:
set matr.start_over = 'yes';
--
-- The data model, in one paragraph. A REGISTRY holds STUDIES; a study holds QUESTIONNAIRES (forms); a
-- questionnaire was given in WAVES (rounds of data collection). What a questionnaire prints is kept once
-- per wording as a COMPONENT (a Section heading, a Question, an Instruction...), and a PLACEMENT says where
-- a questionnaire prints it: inside which Section, Grid or Group, in what order, and, for a question, which
-- earlier question it depends on ("When did you start?" depends on "Do you smoke?"). WAVE_QUESTION says
-- which questions a wave actually asked, with their numbers. A question's answers are recorded in
-- VARIABLES (one per MATR ID, the column in the response data): open-ended (a number, text, date or time
-- the respondent writes) or closed-ended (chosen from its RESPONSE_OPTIONS, each an ANSWER_OPTION kept once
-- for the whole registry, with the code it was recorded as). TAGS (a subject such as "twin", a
-- time frame such as "past 30 days", a unit such as "pounds") can be put on a question, a variable
-- or a response option. The dashboard finds questions by meaning, through an embedding of each
-- question (8_search.sql). Staff work in EXTRACTIONS (one row
-- each, with the questionnaire as JSON until it is pushed) and everything they change is written to HISTORY.
-- =====================================================================================================

begin;

do $$
declare
  t        text;
  has_rows boolean;
begin
  -- content lives in placement (this schema) or questionnaire_component (the schema before it)
  foreach t in array array['placement', 'questionnaire_component'] loop
    if to_regclass('public.' || t) is not null then
      execute format('select exists (select 1 from public.%I)', t) into has_rows;
      if has_rows and coalesce(current_setting('matr.start_over', true), '') <> 'yes' then
        raise exception 'Refusing to start over: this database holds questionnaire content (%). If you mean to lose it, uncomment "set matr.start_over" at the top of 1_tables.sql.', t;
      end if;
    end if;
  end loop;
end;
$$;
drop schema if exists private cascade;
drop schema if exists public cascade;

-- public as a new Supabase project has it (5_permissions.sql takes back what the dashboard roles mustn't have)
create schema public;
grant usage on schema public to postgres, anon, authenticated, service_role;
grant all privileges on all tables    in schema public to postgres, anon, authenticated, service_role;
grant all privileges on all functions in schema public to postgres, anon, authenticated, service_role;
grant all privileges on all sequences in schema public to postgres, anon, authenticated, service_role;
alter default privileges in schema public grant all on tables    to postgres, anon, authenticated, service_role;
alter default privileges in schema public grant all on functions to postgres, anon, authenticated, service_role;
alter default privileges in schema public grant all on sequences to postgres, anon, authenticated, service_role;

-- Helpers the tables' defaults and checks use. The REST API doesn't expose this schema.
create schema private;

-- Every table's own ID is a random 9-digit number (100000000 to 999999999), picked when a row is added:
-- short enough to read and type, and far below the 2^53 that R and JSON hold exactly.
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

-- Words with one space between them and none at the ends.
create function private.squish(p_text text)
returns text
language sql
immutable
set search_path = ''
as $$
  select btrim(regexp_replace(p_text, '\s+', ' ', 'g'));
$$;

-- An answer choice as it is kept (answer_option.standardized_label): lower case, straight quotes, one space
-- between words, no question mark, and no full stop or colon at the end. The site's normal_answer()
-- (R/service_draft.R) does the same and more (writes "1 to 2" as "1-2", "hrs" as "hours"), so what it gives
-- always passes this check.
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
-- THE CATALOG: what the public dashboard reads
-- =====================================================================================================

-- ------ Registries and studies -----------------------------------------------------------------------

create table public.registry (
  registry_id        bigint  primary key default private.random_id('registry', 'registry_id'),
  name               text    not null constraint registry_name_not_blank check (btrim(name) <> ''),
  abbreviation       text    not null constraint registry_abbreviation_not_blank check (btrim(abbreviation) <> ''),
  participant_count  integer constraint registry_participant_count_check check (participant_count >= 0),
  description        text
);

create table public.principal_investigator (
  principal_investigator_id bigint primary key default private.random_id('principal_investigator', 'principal_investigator_id'),
  name   text not null constraint principal_investigator_name_not_blank check (btrim(name) <> ''),
  email  text
);

create table public.study (
  study_id                          bigint primary key default private.random_id('study', 'study_id'),
  registry_id                       bigint not null,
  principal_investigator_id         bigint,
  contact_principal_investigator_id bigint,
  name              text not null constraint study_name_not_blank check (btrim(name) <> ''),
  status            text constraint study_status_check check (status in ('Active', 'Ended')),
  description       text,
  methods           text,
  acknowledgements  text
);

-- ------ Questionnaires and waves ---------------------------------------------------------------------

create table public.respondent_type (
  respondent_type_id bigint  primary key default private.random_id('respondent_type', 'respondent_type_id'),
  name         text    not null constraint respondent_type_name_not_blank check (btrim(name) <> ''),
  minimum_age  integer constraint respondent_type_minimum_age_check check (minimum_age >= 0),
  maximum_age  integer constraint respondent_type_maximum_age_check check (maximum_age <= 120),
  description  text,
  constraint respondent_type_age_range check (minimum_age <= maximum_age)
);

-- A name and an abbreviation are each unique within a study, ignoring case and outer spaces (3_indexes.sql).
create table public.questionnaire (
  questionnaire_id   bigint  primary key default private.random_id('questionnaire', 'questionnaire_id'),
  study_id           bigint  not null,
  respondent_type_id bigint  not null,
  name               text    not null constraint questionnaire_name_not_blank check (btrim(name) <> ''),
  abbreviation       text    not null constraint questionnaire_abbreviation_not_blank check (btrim(abbreviation) <> ''),
  upvote_count       integer not null default 0 constraint questionnaire_upvote_count_nonnegative check (upvote_count >= 0),
  participant_count  integer constraint questionnaire_participant_count_check check (participant_count >= 0),
  description        text
);

-- A wave keeps the files its questions were extracted from: the PDF (by its pdf_file row; the file itself is
-- in Storage) and the optional file of MATR IDs and codes, a PDF (supplement_sha256) or a CSV or text file
-- kept as its text (supplement_text). Written when an extraction is pushed; kept when the wave is emptied, so
-- Re-extract can read the PDF again.
create table public.wave (
  wave_id           bigint  primary key default private.random_id('wave', 'wave_id'),
  questionnaire_id  bigint  not null,
  name              text    not null constraint wave_name_not_blank check (btrim(name) <> ''),
  respondent_count  integer constraint wave_respondent_count_check check (respondent_count >= 0),
  administered_date date,
  pdf_sha256        text,
  pdf_name          text,
  supplement_name   text,
  supplement_sha256 text,
  supplement_text   text,
  constraint wave_name_unique_in_questionnaire unique (questionnaire_id, name),
  constraint wave_supplement_is_one_file check (supplement_sha256 is null or supplement_text is null),
  -- the target of wave_question's foreign key, so a wave and a question share one questionnaire
  constraint wave_questionnaire_wave_key unique (questionnaire_id, wave_id)
);

-- A PDF uploaded to the site, once per file, by its SHA-256: the file is in Storage (bucket extraction-pdfs)
-- as <sha256>.pdf. Its pages and size are facts about the file; the name it was uploaded under belongs to
-- the extraction or wave that uses it.
create table public.pdf_file (
  pdf_sha256 text    primary key constraint pdf_file_sha256_format check (pdf_sha256 ~ '^[0-9a-f]{64}$'),
  pdf_pages  integer constraint pdf_file_pdf_pages_check check (pdf_pages > 0),
  pdf_bytes  bigint  constraint pdf_file_pdf_bytes_check check (pdf_bytes >= 0)
);

-- ------ What is printed ------------------------------------------------------------------------------

-- One wording, kept once however many questionnaires print it and however often (3_indexes.sql: one row
-- for each kind and words, but for runs of spaces). A Group has no words, so there is one Group, placed
-- wherever a questionnaire holds blocks together. kind:
--   Section       holds other blocks under a printed heading (with any section number)
--   Group         holds blocks that belong together with no heading: a battery, a question with its follow-up
--   Grid          holds Questions that finish a lead-in it prints ("In the past week I have ...")
--   Question      asks for one or more answers; has variables
--   Instruction   tells the respondent how to answer
--   Introduction  opens the questionnaire or a section
--   Definition    explains a term
--   Note          any other printed remark
--   Interviewer Script  what an interviewer reads aloud or follows
create table public.component (
  component_id bigint primary key default private.random_id('component', 'component_id'),
  kind         text   not null
    constraint component_kind_check check (kind in ('Section', 'Group', 'Grid', 'Question', 'Instruction', 'Introduction',
                                                    'Definition', 'Note', 'Interviewer Script')),
  text         text   constraint component_text_not_blank check (text is null or btrim(text) <> ''),
  constraint component_group_has_no_text check ((kind = 'Group') = (text is null))
);

-- Where a questionnaire prints a component. A questionnaire can print the same component more than once
-- (an instruction repeated before each battery, the same item in Twin A's part and Twin B's), so each place
-- is a placement of its own, and everything about a place in the questionnaire (what holds it, its order,
-- what it depends on, its numbers in each wave, its variables) names the placement. The component is the
-- wording, shared by every place that prints it.
--   parent_placement_id      the Section, Grid or Group it is inside, in the same questionnaire (empty at the top)
--   depends_on_placement_id  for a Question: the earlier Question it only makes sense after ("When did you
--                            start?" after "Do you smoke?"); empty for a question that stands on its own
create table public.placement (
  placement_id            bigint  primary key default private.random_id('placement', 'placement_id'),
  questionnaire_id        bigint  not null,
  component_id            bigint  not null,
  parent_placement_id     bigint,
  display_order           integer not null constraint placement_display_order_check check (display_order > 0),
  depends_on_placement_id bigint,
  constraint placement_in_questionnaire unique (questionnaire_id, placement_id),
  constraint placement_not_own_parent check (parent_placement_id <> placement_id),
  constraint placement_not_own_dependency check (depends_on_placement_id <> placement_id),
  -- display order unique among siblings; NULLS NOT DISTINCT makes the top level one group. Deferrable so a
  -- change can move blocks past each other.
  constraint placement_display_order_unique
    unique nulls not distinct (questionnaire_id, parent_placement_id, display_order) deferrable initially immediate
);

-- That a wave asked a question: its place among the wave's questions (question_number: 1, 2, 3..., worked
-- out by the site) and the number printed beside it (printed_number: 1a, 12, Q7; can repeat, or be empty).
create table public.wave_question (
  questionnaire_id bigint not null,
  wave_id          bigint not null,
  placement_id     bigint not null,
  question_number  text   not null constraint wave_question_number_not_blank check (btrim(question_number) <> ''),
  printed_number   text,
  primary key (wave_id, placement_id),
  constraint wave_question_number_unique unique (wave_id, question_number) deferrable initially immediate
);

-- ------ Answers --------------------------------------------------------------------------------------

-- One place an answer is recorded: one column in the response data. variable_key is deliberately not called
-- variable_id: matr_id holds what MATR staff call the "variable ID" (AH25LF), unique within a questionnaire.
--   variable_type     open_ended (the respondent writes it) or closed_ended (chosen from its response options)
--   answer_data_type  what an open-ended answer is: NUMBER, TEXT, DATE or TIME; empty for closed-ended
--   label             what tells a question's variables apart ("Twin A", "child a", a check box's words)
create table public.variable (
  variable_key     bigint  primary key default private.random_id('variable', 'variable_key'),
  questionnaire_id bigint  not null,
  placement_id     bigint  not null,
  matr_id          text    not null constraint variable_matr_id_not_blank check (btrim(matr_id) <> ''),
  display_order    integer not null constraint variable_display_order_check check (display_order > 0),
  label            text,
  variable_type    text    not null constraint variable_type_check check (variable_type in ('open_ended', 'closed_ended')),
  answer_data_type text    constraint variable_answer_data_type_check check (answer_data_type in ('NUMBER', 'TEXT', 'DATE', 'TIME')),
  constraint variable_open_ended_has_data_type check ((variable_type = 'open_ended') = (answer_data_type is not null)),
  constraint variable_matr_id_unique_in_questionnaire unique (questionnaire_id, matr_id),
  constraint variable_display_order_unique unique (placement_id, display_order) deferrable initially immediate
);

-- One answer choice, stored once for the whole registry, in its normal form (private.normal_answer).
create table public.answer_option (
  answer_option_id   bigint primary key default private.random_id('answer_option', 'answer_option_id'),
  standardized_label text   not null
    constraint answer_option_label_unique unique
    constraint answer_option_label_normal check (standardized_label = private.normal_answer(standardized_label))
);

-- One answer option as a closed-ended variable printed it: its order, and the code it was recorded as
-- (text: codes may be 1, 0, -9, A...). A closed-ended variable has one or more (a lone check box has one).
create table public.response_option (
  variable_key     bigint  not null,
  answer_option_id bigint  not null,
  display_order    integer not null constraint response_option_display_order_check check (display_order > 0),
  code_value       text    not null,
  primary key (variable_key, answer_option_id),
  constraint response_option_display_order_unique unique (variable_key, display_order) deferrable initially immediate,
  constraint response_option_code_value_unique unique (variable_key, code_value) deferrable initially immediate
);

-- ------ Tags -----------------------------------------------------------------------------------------

-- A word the dashboard can filter by, put on a question (placement_tag), a variable (variable_tag) or a
-- response option (response_option_tag). One list for each kind:
--   subject     who the answer is about, the relation alone in lower case (R/service_draft.R, normal_subject()): "twin", "children"
--   time frame  when it is about, written one way (R/service_draft.R, normal_time_frame()): "past 30 days", "lifetime"
--   unit        what an open-ended number is in, written one way (normal_unit()): "pounds", "years"
-- One tag for each kind and name, ignoring case and outer spaces (3_indexes.sql). Straight quotes only, as
-- the site writes them, so a name typed either way is the one name.
create table public.tag (
  tag_id bigint primary key default private.random_id('tag', 'tag_id'),
  kind   text   not null constraint tag_kind_check check (kind in ('subject', 'time frame', 'unit')),
  name   text   not null
    constraint tag_name_not_blank check (btrim(name) <> '')
    constraint tag_name_straight_quotes check (name !~ '[‘’“”]')
);

create table public.placement_tag (
  placement_id bigint not null,
  tag_id       bigint not null,
  primary key (placement_id, tag_id)
);

create table public.variable_tag (
  variable_key bigint not null,
  tag_id       bigint not null,
  primary key (variable_key, tag_id)
);

create table public.response_option_tag (
  variable_key     bigint not null,
  answer_option_id bigint not null,
  tag_id           bigint not null,
  primary key (variable_key, answer_option_id, tag_id)
);

-- ------ Measurement scales ----------------------------------------------------------------------------

-- A published, validated set of questions scored together. Name and abbreviation unique, ignoring case
-- and outer spaces (3_indexes.sql).
create table public.measurement_scale (
  scale_id     bigint primary key default private.random_id('measurement_scale', 'scale_id'),
  name         text   not null constraint measurement_scale_name_not_blank check (btrim(name) <> ''),
  abbreviation text   not null constraint measurement_scale_abbreviation_not_blank check (btrim(abbreviation) <> ''),
  version      text,
  description  text,
  citation     text
);

-- A question's membership and item number in a scale (text: published items can be numbered 56a, 56b).
create table public.question_scale (
  scale_id     bigint not null,
  component_id bigint not null,
  item_number  text   not null constraint question_scale_item_number_not_blank check (btrim(item_number) <> ''),
  primary key (scale_id, component_id),
  constraint question_scale_item_number_unique unique (scale_id, item_number)
);


-- =====================================================================================================
-- THE STAFF'S WORK: private to the site
-- =====================================================================================================

-- MATR employee who extracts and edits metadata. Never deleted; marked inactive. The database knows a
-- signed-in person as staff by the email they sign in with (public.is_active_staff(), 4_functions.sql).
create table public.staff (
  staff_id bigint  primary key default private.random_id('staff', 'staff_id'),
  name     text    not null constraint staff_name_not_blank check (btrim(name) <> ''),
  email    text    not null constraint staff_email_not_blank check (btrim(email) <> ''),
  active   boolean not null default true,
  role     text    not null default 'STAFF' constraint staff_role_check check (role in ('STAFF', 'ADMIN'))
);

-- One extraction of a questionnaire, from the first save until it is pushed into the catalog, and afterwards
-- the record of what was read and what staff changed (the AI Monitor page reads pushed ones).
--   method          AI-Powered (a PDF read by Gemini; reading_* say how far that has got) or Manual (typed)
--   status          Draft while staff work on it, Pushed once public.push_extraction() has written it
--   info            where it goes: {"registry_id", "study_id", "questionnaire_id", "wave_id"}, each the ID as
--                   text or "" while not picked (a form's state, so JSON, not columns)
--   questionnaire   the questionnaire as staff last saved it (the Edit Extraction page's JSON); empty until
--                   they first save, when the AI's reading (ai_original) is shown instead
--   ai_original     what the AI returned, written once by the reading job and never changed
--   review_edits    how the saved copy differs from ai_original, worked out by the site at each save
--   matr_csv        the MATR IDs and codes read from the supplement, if any
--   pdf_*           the questionnaire PDF (pdf_file has its pages and size); AI-Powered always has one
--   supplement      the optional file of MATR IDs and codes: {name, sha256, bytes} for a PDF, {name, text} for a
--                   CSV or text file
--   revision        goes up at each save by staff (extraction_touch, 4_functions.sql), so a save made from a
--                   stale copy is noticed
--   editing_by/at   who has the draft open on Edit Extraction, since when
create table public.extraction (
  extraction_id      bigint      primary key default private.random_id('extraction', 'extraction_id'),
  method             text        not null constraint extraction_method_check check (method in ('AI-Powered', 'Manual')),
  status             text        not null default 'Draft' constraint extraction_status_check check (status in ('Draft', 'Pushed')),
  prepared_by        bigint      not null,
  saved_by           bigint      not null,
  revision           integer     not null default 1 constraint extraction_revision_check check (revision > 0),
  summary            text,
  info               jsonb       not null default '{}'::jsonb constraint extraction_info_is_an_object check (jsonb_typeof(info) = 'object'),
  matr_csv           jsonb,
  questionnaire      jsonb       constraint extraction_questionnaire_is_an_object check (questionnaire is null or jsonb_typeof(questionnaire) = 'object'),
  pdf_sha256         text,
  pdf_name           text,
  supplement         jsonb       constraint extraction_supplement_is_an_object check (supplement is null or jsonb_typeof(supplement) = 'object'),
  ai_original        jsonb,
  review_edits       jsonb       not null default '[]'::jsonb constraint extraction_edits_are_a_list check (jsonb_typeof(review_edits) = 'array'),
  reading_status     text        constraint extraction_reading_status_check
                                 check (reading_status in ('queued', 'sent', 'received', 'assigning', 'ready', 'failed')),
  reading_started    timestamptz,
  reading_claimed_at timestamptz,
  reading_pid        integer,
  reading_error      text,
  model              text,
  prompt_version     text,
  created_at         timestamptz not null default now(),
  saved_at           timestamptz not null default now(),
  pushed_at          timestamptz,
  editing_by         bigint,
  editing_at         timestamptz,
  constraint extraction_pushed_says_when check ((status = 'Pushed') = (pushed_at is not null)),
  constraint extraction_editing_says_when check ((editing_by is null) = (editing_at is null)),
  constraint extraction_ai_has_pdf_and_reading check (method <> 'AI-Powered' or (pdf_sha256 is not null and reading_status is not null)),
  constraint extraction_manual_has_no_reading check (method = 'AI-Powered' or reading_status is null)
);

-- Everything staff changed in the catalog, one row per change, written by the functions that change it
-- (4_functions.sql) and never edited: the Edit History page.
--   action     Pushed (an extraction), Emptied (a wave, questionnaire, study or registry), Added, Updated,
--              Deleted (one row and what went with it, on Make an edit)
--   entity     the table of the row the change was about; record_id its key, as text
--   summary    the change in a sentence, as the site said it
--   details    what was done: an extraction's counts and extraction_id, an edit's fields with old and new
--              values, an emptying's counts
create table public.history (
  history_id  bigint      primary key default private.random_id('history', 'history_id'),
  staff_id    bigint      not null,
  occurred_at timestamptz not null default now(),
  action      text        not null constraint history_action_check check (action in ('Pushed', 'Emptied', 'Added', 'Updated', 'Deleted')),
  entity      text        not null,
  record_id   text,
  summary     text        not null,
  details     jsonb
);

commit;
