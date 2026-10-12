# Questionnaires and the tables around them: the registries that hold everything
# and the studies in them, who leads each study, each questionnaire's respondent type,
# and its waves. `connection` is the MATR database (model_database.R).

#' Every registry.
#'
#' The outermost container; every study rolls up to a registry.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame with `registry_id`, `name` and `abbreviation`, by name.
registry_model_get_all <- function(connection) {
  database_model_get(connection, "registry", c("registry_id", "name", "abbreviation"), order_by = "name")
}

#' Every questionnaire.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame with `questionnaire_id`, `study_id`, `respondent_type_id`, `name` and `abbreviation`.
questionnaire_model_get_all <- function(connection) {
  database_model_get(
    connection, "questionnaire",
    c("questionnaire_id", "study_id", "respondent_type_id", "name", "abbreviation"),
    order_by = "questionnaire_id"
  )
}

#' Every wave.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame with `wave_id`, `questionnaire_id`, `name`, `administered_date` and `respondent_count`.
wave_model_get_all <- function(connection) {
  database_model_get(
    connection, "wave",
    c("wave_id", "questionnaire_id", "name", "administered_date", "respondent_count"),
    order_by = "wave_id"
  )
}

#' Every study.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame with `study_id`, `registry_id`, `principal_investigator_id` and `name`, by name.
study_model_get_all <- function(connection) {
  database_model_get(
    connection, "study",
    c("study_id", "registry_id", "principal_investigator_id", "name"),
    order_by = "name"
  )
}

#' Every principal investigator.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame with `principal_investigator_id` and `name`.
principal_investigator_model_get_all <- function(connection) {
  database_model_get(connection, "principal_investigator", c("principal_investigator_id", "name"))
}

#' Every respondent type.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame with `respondent_type_id`, `name`, `minimum_age` and `maximum_age`, by name.
respondent_type_model_get_all <- function(connection) {
  database_model_get(
    connection, "respondent_type",
    c("respondent_type_id", "name", "minimum_age", "maximum_age"),
    order_by = "name"
  )
}
