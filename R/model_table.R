# ------ The live tables, read whole ------------------------------------------------
# The edit pages need to see what a change reaches, so they read whole tables
# rather than the few columns the extraction pages pick. Each table is listed
# here with its columns and what kind of value each holds, and the columns
# that identify a row (`key`). `connection` is the MATR database
# (model_database.R).
#
# Column kinds:
#   id      a row's ID, or a reference to another row's: a whole number
#   number  any other number (ages, counts, display order)
#   text    words; question numbers and MATR IDs are text too, since they can
#           hold letters (2a, AH25LF)
#   date    a day, as YYYY-MM-DD
#   flag    TRUE or FALSE

database_tables <- list(
  registry = list(
    key = "registry_id", order_by = "name",
    columns = c(
      registry_id = "id", name = "text", abbreviation = "text",
      participant_count = "number", description = "text"
    )
  ),
  principal_investigator = list(
    key = "principal_investigator_id", order_by = "name",
    columns = c(principal_investigator_id = "id", name = "text", email = "text")
  ),
  study = list(
    key = "study_id", order_by = "name",
    columns = c(
      study_id = "id", registry_id = "id", principal_investigator_id = "id",
      contact_principal_investigator_id = "id", name = "text", status = "text",
      description = "text", methods = "text", acknowledgements = "text"
    )
  ),
  respondent_type = list(
    key = "respondent_type_id", order_by = c("minimum_age", "name"),
    columns = c(
      respondent_type_id = "id", name = "text", minimum_age = "number",
      maximum_age = "number", description = "text"
    )
  ),
  questionnaire = list(
    key = "questionnaire_id", order_by = "name",
    columns = c(
      questionnaire_id = "id", study_id = "id", respondent_type_id = "id",
      name = "text", abbreviation = "text", description = "text"
    )
  ),
  wave = list(
    key = "wave_id", order_by = "wave_id",
    columns = c(
      wave_id = "id", questionnaire_id = "id", name = "text",
      respondent_count = "number", administered_date = "date"
    )
  ),
  component_type = list(
    key = "component_type_id", order_by = "component_type_id",
    columns = c(component_type_id = "id", name = "text", description = "text")
  ),
  component = list(
    key = "component_id", order_by = "component_id",
    columns = c(component_id = "id", component_type_id = "id", text = "text")
  ),
  questionnaire_component = list(
    key = c("questionnaire_id", "component_id"), order_by = c("questionnaire_id", "component_id"),
    columns = c(
      questionnaire_id = "id", component_id = "id", parent_component_id = "id",
      display_order = "number"
    )
  ),
  question = list(
    key = "component_id", order_by = "component_id",
    columns = c(component_id = "id")
  ),
  questionnaire_question = list(
    key = c("questionnaire_id", "component_id"), order_by = c("questionnaire_id", "component_id"),
    columns = c(
      questionnaire_id = "id", component_id = "id", parent_question_id = "id",
      self_contained = "flag", condition = "text"
    )
  ),
  wave_question = list(
    key = c("questionnaire_id", "wave_id", "component_id"),
    order_by = c("questionnaire_id", "wave_id", "component_id"),
    columns = c(
      questionnaire_id = "id", wave_id = "id", component_id = "id",
      question_number = "text"
    )
  ),
  category = list(
    key = "mesh_id", order_by = "term",
    columns = c(mesh_id = "text", term = "text")
  ),
  question_category = list(
    key = c("component_id", "mesh_id"), order_by = c("component_id", "mesh_id"),
    columns = c(component_id = "id", mesh_id = "text")
  ),
  answer_option = list(
    key = "answer_option_id", order_by = "standardized_label",
    columns = c(answer_option_id = "id", standardized_label = "text")
  ),
  subject = list(
    key = "subject_id", order_by = "name",
    columns = c(subject_id = "id", name = "text")
  ),
  answer_option_set = list(
    key = c("option_set_id", "answer_option_id"), order_by = c("option_set_id", "answer_option_id"),
    columns = c(option_set_id = "id", answer_option_id = "id")
  ),
  measurement_scale = list(
    key = "scale_id", order_by = "name",
    columns = c(
      scale_id = "id", name = "text", abbreviation = "text", version = "text",
      description = "text", citation = "text"
    )
  ),
  question_scale = list(
    key = c("scale_id", "component_id"), order_by = c("scale_id", "component_id"),
    columns = c(
      scale_id = "id", component_id = "id", expected_option_set_id = "id",
      item_number = "text"
    )
  ),
  variable = list(
    key = "variable_key", order_by = "variable_key",
    columns = c(
      variable_key = "id", questionnaire_id = "id", component_id = "id",
      matr_id = "text", display_order = "number", label = "text",
      respondent_count = "number", subject_id = "id"
    )
  ),
  open_ended = list(
    key = "variable_key", order_by = "variable_key",
    columns = c(variable_key = "id", answer_data_type = "text")
  ),
  closed_ended = list(
    key = "variable_key", order_by = "variable_key",
    columns = c(variable_key = "id", option_set_id = "id")
  ),
  response_option = list(
    key = c("variable_key", "answer_option_id"), order_by = c("variable_key", "answer_option_id"),
    columns = c(
      variable_key = "id", answer_option_id = "id", display_order = "number",
      code_value = "text", printed = "flag", printed_label = "text",
      skip_instruction = "text"
    )
  )
)

#' Read a whole table as the edit pages see it.
#'
#' Every row of `table`, with every column listed in `database_tables`, as the kinds
#' described above (ids and numbers as numbers, flags as logical, the rest as text).
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param table One of `names(database_tables)`.
#' @param filters Only the rows that match, as for `database_model_get()`; the pages read whole tables.
#' @return A data frame; an empty table comes back with its columns and no rows. An
#'   error if the pages don't know the table.
table_model_get <- function(connection, table, filters = list()) {
  spec <- database_tables[[table]]
  if (is.null(spec)) stop("The edit pages don't know a table called ", table, ".", call. = FALSE)
  rows <- database_model_get(connection, table, names(spec$columns), order_by = spec$order_by, filters = filters)
  for (column in names(spec$columns)) {
    rows[[column]] <- switch(spec$columns[[column]],
                             id = ,
                             number = as.numeric(rows[[column]]),
                             flag = as.logical(rows[[column]]),
                             as.character(rows[[column]])
    )
  }
  rows
}
