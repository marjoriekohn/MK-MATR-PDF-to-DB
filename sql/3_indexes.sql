-- =====================================================================================================
-- MATR metadata database, script 3 of 8: INDEXES
-- =====================================================================================================
-- Run after 2_references.sql; can be run again at any time. Primary keys and unique constraints already
-- make an index, and an index also serves any search on its first columns, so a foreign key only needs one
-- here when no key starts with its columns.

begin;

-- ------ Uniqueness that ignores case or spaces -------------------------------------------------------
-- (An index, as a unique constraint can't hold an expression.)
drop index if exists public.staff_email_unique;
drop index if exists public.principal_investigator_email_unique;
drop index if exists public.registry_name_unique;
drop index if exists public.registry_abbreviation_unique;
drop index if exists public.respondent_type_name_unique;
drop index if exists public.study_name_unique_in_registry;
drop index if exists public.questionnaire_name_unique_in_study;
drop index if exists public.questionnaire_abbreviation_unique_in_study;
drop index if exists public.component_words_unique;
drop index if exists public.component_one_group;
drop index if exists public.tag_name_unique_in_kind;
drop index if exists public.measurement_scale_name_unique;
drop index if exists public.measurement_scale_abbreviation_unique;

create unique index staff_email_unique                         on public.staff (lower(btrim(email)));
create unique index principal_investigator_email_unique        on public.principal_investigator (lower(btrim(email)));
create unique index registry_name_unique                       on public.registry (lower(btrim(name)));
create unique index registry_abbreviation_unique               on public.registry (lower(btrim(abbreviation)));
create unique index respondent_type_name_unique                on public.respondent_type (lower(btrim(name)));
create unique index study_name_unique_in_registry              on public.study (registry_id, lower(btrim(name)));
create unique index questionnaire_name_unique_in_study         on public.questionnaire (study_id, lower(btrim(name)));
create unique index questionnaire_abbreviation_unique_in_study on public.questionnaire (study_id, lower(btrim(abbreviation)));
-- one component for each kind and words (but for runs of spaces), and one Group (the one kind with no words)
create unique index component_words_unique                     on public.component (kind, private.squish(text)) where text is not null;
create unique index component_one_group                        on public.component (kind) where text is null;
create unique index tag_name_unique_in_kind                    on public.tag (kind, lower(btrim(name)));
create unique index measurement_scale_name_unique              on public.measurement_scale (lower(btrim(name)));
create unique index measurement_scale_abbreviation_unique      on public.measurement_scale (lower(btrim(abbreviation)));

-- ------ Lookups ---------------------------------------------------------------------------------------

-- ------ Foreign keys ----------------------------------------------------------------------------------
drop index if exists public.study_registry_id_idx;
drop index if exists public.study_principal_investigator_id_idx;
drop index if exists public.study_contact_pi_id_idx;
drop index if exists public.questionnaire_study_id_idx;
drop index if exists public.questionnaire_respondent_type_idx;
drop index if exists public.wave_pdf_sha256_idx;
drop index if exists public.wave_supplement_sha256_idx;
drop index if exists public.placement_component_idx;
drop index if exists public.placement_parent_idx;
drop index if exists public.placement_depends_on_idx;
drop index if exists public.wave_question_placement_idx;
drop index if exists public.variable_placement_idx;
drop index if exists public.response_option_answer_option_idx;
drop index if exists public.placement_tag_tag_idx;
drop index if exists public.variable_tag_tag_idx;
drop index if exists public.response_option_tag_tag_idx;
drop index if exists public.question_scale_component_idx;
drop index if exists public.extraction_prepared_by_idx;
drop index if exists public.extraction_saved_by_idx;
drop index if exists public.extraction_editing_by_idx;
drop index if exists public.extraction_pdf_sha256_idx;
drop index if exists public.extraction_status_idx;
drop index if exists public.history_staff_id_idx;
drop index if exists public.history_occurred_at_idx;

create index study_registry_id_idx               on public.study (registry_id);
create index study_principal_investigator_id_idx on public.study (principal_investigator_id);
create index study_contact_pi_id_idx             on public.study (contact_principal_investigator_id);
create index questionnaire_study_id_idx          on public.questionnaire (study_id);
create index questionnaire_respondent_type_idx   on public.questionnaire (respondent_type_id);
create index wave_pdf_sha256_idx                 on public.wave (pdf_sha256);
create index wave_supplement_sha256_idx          on public.wave (supplement_sha256);
create index placement_component_idx             on public.placement (component_id);
create index placement_parent_idx                on public.placement (questionnaire_id, parent_placement_id);
create index placement_depends_on_idx            on public.placement (questionnaire_id, depends_on_placement_id);
create index wave_question_placement_idx         on public.wave_question (questionnaire_id, placement_id);
create index variable_placement_idx              on public.variable (questionnaire_id, placement_id);
create index response_option_answer_option_idx   on public.response_option (answer_option_id);
create index placement_tag_tag_idx               on public.placement_tag (tag_id);
create index variable_tag_tag_idx                on public.variable_tag (tag_id);
create index response_option_tag_tag_idx         on public.response_option_tag (tag_id);
create index question_scale_component_idx        on public.question_scale (component_id);
create index extraction_prepared_by_idx          on public.extraction (prepared_by);
create index extraction_saved_by_idx             on public.extraction (saved_by);
create index extraction_editing_by_idx           on public.extraction (editing_by);
create index extraction_pdf_sha256_idx           on public.extraction (pdf_sha256);
create index extraction_status_idx               on public.extraction (status, saved_at desc);
create index history_staff_id_idx                on public.history (staff_id);
create index history_occurred_at_idx             on public.history (occurred_at desc);

commit;
