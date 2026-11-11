# ------ The catalog's tables, read whole ------------------------------------------------
# The Metadata page needs to see what a change reaches, so it reads whole tables
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
#
# sql/1_tables.sql builds the tables, and docs/database_design.md says why they look the way they do.

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
      name = "text", abbreviation = "text", participant_count = "number", description = "text"
    )
  ),
  # A wave keeps the files it was extracted from, so Re-extract can read them again (pushed with the extraction;
  # not typed on a form).
  wave = list(
    key = "wave_id", order_by = "wave_id",
    columns = c(
      wave_id = "id", questionnaire_id = "id", name = "text",
      respondent_count = "number", administered_date = "date",
      pdf_sha256 = "text", pdf_name = "text", supplement_name = "text", supplement_sha256 = "text", supplement_text = "text"
    )
  ),
  # The PDFs uploaded to the site, once each by their SHA-256: Make an edit lists them (and only shows them;
  # nothing about a PDF can change).
  pdf_file = list(
    key = "pdf_sha256", order_by = "pdf_sha256",
    columns = c(pdf_sha256 = "text", pdf_pages = "number", pdf_bytes = "number")
  ),
  # One wording, kept once however many questionnaires print it.
  component = list(
    key = "component_id", order_by = "component_id",
    columns = c(component_id = "id", kind = "text", text = "text")
  ),
  placement = list(
    key = "placement_id", order_by = c("questionnaire_id", "placement_id"),
    columns = c(
      placement_id = "id", questionnaire_id = "id", component_id = "id", parent_placement_id = "id",
      display_order = "number", depends_on_placement_id = "id"
    )
  ),
  wave_question = list(
    key = c("wave_id", "placement_id"),
    order_by = c("questionnaire_id", "wave_id", "placement_id"),
    columns = c(
      questionnaire_id = "id", wave_id = "id", placement_id = "id",
      question_number = "text", printed_number = "text"
    )
  ),
  variable = list(
    key = "variable_key", order_by = "variable_key",
    columns = c(
      variable_key = "id", questionnaire_id = "id", placement_id = "id",
      matr_id = "text", display_order = "number", label = "text",
      variable_type = "text", answer_data_type = "text"
    )
  ),
  answer_option = list(
    key = "answer_option_id", order_by = "standardized_label",
    columns = c(answer_option_id = "id", standardized_label = "text")
  ),
  response_option = list(
    key = c("variable_key", "answer_option_id"), order_by = c("variable_key", "answer_option_id"),
    columns = c(variable_key = "id", answer_option_id = "id", display_order = "number", code_value = "text")
  ),
  tag = list(
    key = "tag_id", order_by = c("kind", "name"),
    columns = c(tag_id = "id", kind = "text", name = "text")
  ),
  placement_tag = list(
    key = c("placement_id", "tag_id"), order_by = c("placement_id", "tag_id"),
    columns = c(placement_id = "id", tag_id = "id")
  ),
  variable_tag = list(
    key = c("variable_key", "tag_id"), order_by = c("variable_key", "tag_id"),
    columns = c(variable_key = "id", tag_id = "id")
  ),
  response_option_tag = list(
    key = c("variable_key", "answer_option_id", "tag_id"), order_by = c("variable_key", "answer_option_id", "tag_id"),
    columns = c(variable_key = "id", answer_option_id = "id", tag_id = "id")
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
    columns = c(scale_id = "id", component_id = "id", item_number = "text")
  )
)

# What is read of a table: its columns.
table_read_columns <- function(spec) spec$columns

# Read a whole table as the Metadata page sees it.
# Every row of `table`, with every column listed in `database_tables`, as the kinds
# described above (ids and numbers as numbers, the rest as text).
# table: One of `names(database_tables)`.
# filters: Only the rows that match, as for `database_model_get()`; the page reads whole tables.
# Returns: A data frame; an empty table comes back with its columns and no rows. An
#   error if the page doesn't know the table.
table_model_get <- function(connection, table, filters = list()) {
  spec <- database_tables[[table]]
  if (is.null(spec)) stop("The Metadata page doesn't know a table called ", table, ".", call. = FALSE)
  kinds <- table_read_columns(spec)
  rows <- database_model_get(connection, table, names(kinds), order_by = spec$order_by, filters = filters)
  for (column in names(kinds)) {
    rows[[column]] <- switch(kinds[[column]],
      id = ,
      number = as.numeric(rows[[column]]),
      as.character(rows[[column]])
    )
  }
  rows
}
