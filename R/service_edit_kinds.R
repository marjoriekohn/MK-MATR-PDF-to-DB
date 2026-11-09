# Every kind of thing staff can change on Make an edit, by the key the page names it with. Each kind is defined in
# its group's file (service_edit_entities_*.R); this file is loaded after them (the site loads R/ in alphabetical
# order), since the list needs every one.

# ------ All of them -------------------------------------------------------------------------------------------
edit_entities <- list(
  registry = registry_entity,
  investigator = investigator_entity,
  study = study_entity,
  respondent_type = respondent_type_entity,
  questionnaire = questionnaire_entity,
  wave = wave_entity,
  component = component_entity,
  placement = placement_entity,
  answer_option = answer_option_entity,
  tag = tag_entity,
  measurement_scale = measurement_scale_entity,
  question_scale = question_scale_entity,
  pdf_file = pdf_file_entity
)
edit_entities <- Map(function(entity, key) c(list(key = key), entity), edit_entities, names(edit_entities))
