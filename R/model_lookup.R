# ------ Lookup tables ---------------------------------------------------------------
# Short lists staff can add rows to without a schema change, so spelling stays
# consistent. Each is a table in the MATR database (`connection`, as in
# model_database.R), read for the Edit Extraction page's pick lists and the reading
# instructions.

# The kinds of block a questionnaire holds.
# Returns: A data frame with `component_type_id` and `name`.
lookup_model_get_component_types <- function(connection) {
  database_model_get(connection, "component_type", c("component_type_id", "name"), order_by = "component_type_id")
}

# Who a variable can be about, the respondent ("Respondent") included.
# Returns: A data frame with `subject_id` and `name`.
lookup_model_get_subjects <- function(connection) {
  database_model_get(connection, "subject", c("subject_id", "name"), order_by = "subject_id")
}

# When a variable can be about: "in your lifetime", "in the past 12 months" and so on.
# Returns: A data frame with `time_frame_id` and `name`.
lookup_model_get_time_frames <- function(connection) {
  database_model_get(connection, "time_frame", c("time_frame_id", "name"), order_by = "name")
}

# The standard answer choices.
# Returns: A data frame with `answer_option_id` and `standardized_label`.
lookup_model_get_answer_options <- function(connection) {
  database_model_get(
    connection, "answer_option", c("answer_option_id", "standardized_label"),
    order_by = "answer_option_id"
  )
}

# The wordings merged into answer choices on Make an edit.
# Returns: A data frame with `alias` and `answer_option_id`.
lookup_model_get_answer_option_aliases <- function(connection) {
  database_model_get(connection, "answer_option_alias", c("alias", "answer_option_id"), order_by = "alias")
}
