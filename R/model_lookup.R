# ------ Lookup tables ---------------------------------------------------------------
# Short lists staff can add rows to without a schema change, so spelling stays
# consistent. Each is a table in the MATR database (`connection`, as in
# model_database.R), read for the Edit Extraction page's pick lists and the reading
# instructions.

#' The kinds of block a questionnaire holds.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame with `component_type_id` and `name`.
lookup_model_get_component_types <- function(connection) {
  database_model_get(connection, "component_type", c("component_type_id", "name"), order_by = "component_type_id")
}

#' Who a variable can be about.
#'
#' There is no `subject` table yet; it's on the to-do list (`docs/PROJECT_STATUS.md`).
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame with `subject_id` and `name`.
lookup_model_get_subjects <- function(connection) {
  database_model_get(connection, "subject", c("subject_id", "name"), order_by = "subject_id")
}

#' The standard answer choices.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame with `answer_option_id` and `standardized_label`.
lookup_model_get_answer_options <- function(connection) {
  database_model_get(
    connection, "answer_option", c("answer_option_id", "standardized_label"),
    order_by = "answer_option_id"
  )
}
