-- =====================================================================================================
-- MATR metadata database, script 2 of 8: REFERENCES (foreign keys)
-- =====================================================================================================
-- Run after 1_tables.sql. Nothing cascades unless noted: deleting a row that others point at is refused.
--
-- What a questionnaire owns goes with it (on delete cascade): its waves, its placements (what holds a
-- placement takes it along) and through those its questions' numbers in each wave, its variables, their
-- response options and tags. What is shared stays: a component, an answer option, a tag, a scale. Deleting
-- a shared row that something still uses is refused. A component no questionnaire prints any more is
-- forgotten by private.forget_components() (4_functions.sql) when the change that unplaced it ends, unless a scale names it.
-- Above a questionnaire nothing cascades: a study or a registry that still holds anything can't be deleted;
-- empty it first (public.empty_study(), public.empty_registry()).

begin;

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
    foreign key (questionnaire_id) references public.questionnaire (questionnaire_id) on delete cascade,
  add constraint wave_pdf_sha256_fkey
    foreign key (pdf_sha256) references public.pdf_file (pdf_sha256),
  add constraint wave_supplement_sha256_fkey
    foreign key (supplement_sha256) references public.pdf_file (pdf_sha256);

alter table public.placement
  add constraint placement_questionnaire_id_fkey
    foreign key (questionnaire_id) references public.questionnaire (questionnaire_id) on delete cascade,
  add constraint placement_component_id_fkey
    foreign key (component_id) references public.component (component_id),
  -- the parent is placed in the SAME questionnaire, because questionnaire_id is half of the key; what is inside
  -- a section, grid or group goes with it
  add constraint placement_parent_fkey
    foreign key (questionnaire_id, parent_placement_id)
    references public.placement (questionnaire_id, placement_id) on delete cascade,
  -- the question it depends on is in the same questionnaire; a question whose parent is deleted stands on its own
  add constraint placement_depends_on_fkey
    foreign key (questionnaire_id, depends_on_placement_id)
    references public.placement (questionnaire_id, placement_id) on delete set null (depends_on_placement_id);

alter table public.wave_question
  -- the wave and the question share one questionnaire_id
  add constraint wave_question_wave_fkey
    foreign key (questionnaire_id, wave_id) references public.wave (questionnaire_id, wave_id) on delete cascade,
  add constraint wave_question_placement_fkey
    foreign key (questionnaire_id, placement_id) references public.placement (questionnaire_id, placement_id) on delete cascade;

alter table public.variable
  add constraint variable_placement_fkey
    foreign key (questionnaire_id, placement_id) references public.placement (questionnaire_id, placement_id) on delete cascade;

alter table public.response_option
  add constraint response_option_variable_key_fkey
    foreign key (variable_key) references public.variable (variable_key) on delete cascade,
  add constraint response_option_answer_option_id_fkey
    foreign key (answer_option_id) references public.answer_option (answer_option_id);

alter table public.placement_tag
  add constraint placement_tag_placement_id_fkey
    foreign key (placement_id) references public.placement (placement_id) on delete cascade,
  add constraint placement_tag_tag_id_fkey
    foreign key (tag_id) references public.tag (tag_id);

alter table public.variable_tag
  add constraint variable_tag_variable_key_fkey
    foreign key (variable_key) references public.variable (variable_key) on delete cascade,
  add constraint variable_tag_tag_id_fkey
    foreign key (tag_id) references public.tag (tag_id);

alter table public.response_option_tag
  add constraint response_option_tag_response_option_fkey
    foreign key (variable_key, answer_option_id) references public.response_option (variable_key, answer_option_id) on delete cascade,
  add constraint response_option_tag_tag_id_fkey
    foreign key (tag_id) references public.tag (tag_id);

alter table public.question_scale
  add constraint question_scale_scale_id_fkey
    foreign key (scale_id) references public.measurement_scale (scale_id),
  add constraint question_scale_component_id_fkey
    foreign key (component_id) references public.component (component_id);

alter table public.extraction
  add constraint extraction_prepared_by_fkey foreign key (prepared_by) references public.staff (staff_id),
  add constraint extraction_saved_by_fkey    foreign key (saved_by)    references public.staff (staff_id),
  add constraint extraction_editing_by_fkey  foreign key (editing_by)  references public.staff (staff_id),
  add constraint extraction_pdf_sha256_fkey  foreign key (pdf_sha256)  references public.pdf_file (pdf_sha256);

alter table public.history
  add constraint history_staff_id_fkey foreign key (staff_id) references public.staff (staff_id);

commit;
