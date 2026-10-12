# ------ Questionnaires already recorded ------------------------------------------
# What the extraction pages need to know about the live tables: the registries
# and their studies, each study's questionnaires and their waves, and who a
# questionnaire can be for. It's read when a session starts (and again after staff
# add something from a drop-down), and the drop-downs and the checks on what staff
# type use this copy, so typing never waits on the database.

#' The live tables the extraction pages pick from and check against.
#'
#' Read when a session starts, and again after staff add something from a drop-down, so typing
#' never waits on the database.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of data frames: `registries`, `studies` (with `principal_investigator`),
#'   `questionnaires` (with `respondent_type` and `n_waves`), `waves` and `respondent_types`.
questionnaire_service_get_catalog <- function(connection) {
  questionnaire_service_catalog(list(
    registries = registry_model_get_all(connection),
    studies = study_model_get_all(connection),
    investigators = principal_investigator_model_get_all(connection),
    questionnaires = questionnaire_model_get_all(connection),
    waves = wave_model_get_all(connection),
    respondent_types = respondent_type_model_get_all(connection)
  ))
}

#' Join the tables the catalog is made of.
#'
#' Each study gets its investigator's name, and each questionnaire its respondent type's name and
#' how many waves it has.
#'
#' @param tables A list of data frames, as the models read them: `registries`, `studies`,
#'   `investigators`, `questionnaires`, `waves` and `respondent_types`.
#' @return The catalog; see `questionnaire_service_get_catalog()`.
questionnaire_service_catalog <- function(tables) {
  studies <- tables$studies
  studies$principal_investigator <- tables$investigators$name[
    match(studies$principal_investigator_id, tables$investigators$principal_investigator_id)
  ]
  questionnaires <- tables$questionnaires
  questionnaires$respondent_type <- tables$respondent_types$name[
    match(questionnaires$respondent_type_id, tables$respondent_types$respondent_type_id)
  ]
  questionnaires$n_waves <- vapply(
    questionnaires$questionnaire_id, function(id) sum(tables$waves$questionnaire_id == id), integer(1)
  )
  list(
    registries = tables$registries,
    studies = studies,
    questionnaires = questionnaires,
    waves = tables$waves,
    respondent_types = tables$respondent_types
  )
}

#' One questionnaire from the catalog.
#'
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param questionnaire_id The id.
#' @return A one-row data frame, or `NULL` if there is not exactly one.
questionnaire_service_get <- function(catalog, questionnaire_id) {
  questionnaires <- catalog$questionnaires
  questionnaire <- questionnaires[questionnaires$questionnaire_id %in% questionnaire_id, ]
  if (nrow(questionnaire) == 1) questionnaire
}

#' One study from the catalog.
#'
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param study_id The id.
#' @return A one-row data frame, or `NULL` if there is not exactly one.
questionnaire_service_get_study <- function(catalog, study_id) {
  study <- catalog$studies[catalog$studies$study_id %in% study_id, ]
  if (nrow(study) == 1) study
}

# ------ Drop-down choices -------------------------------------------------------------
# Named vectors: the names are shown, and the values, the rows' IDs, are saved.
# With no rows there are no labels (recycle0), rather than one made of the
# words around them.

#' Registries for a drop-down.
#'
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @return A named character vector: the names are shown, and the values, the ids, are
#'   saved. Empty if there are none.
questionnaire_service_registry_choices <- function(catalog) {
  registries <- catalog$registries
  stats::setNames(as.character(registries$registry_id), registries$name)
}

#' A registry's studies for a drop-down, each with who leads it.
#'
#' For example "Adult Health Study (Dana Reyes)".
#'
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param registry_id The registry's id.
#' @return A named character vector, as `questionnaire_service_registry_choices()`.
questionnaire_service_study_choices <- function(catalog, registry_id) {
  studies <- catalog$studies[catalog$studies$registry_id %in% registry_id, ]
  labels <- ifelse(
    is.na(studies$principal_investigator),
    studies$name,
    paste0(studies$name, " (", studies$principal_investigator, ")", recycle0 = TRUE)
  )
  stats::setNames(as.character(studies$study_id), labels)
}

#' Respondent types, with their ages, for a drop-down.
#'
#' For example "Child (5-17)", "Adult (18+)" if there is no upper age, or "Parent of Twins (any age)"
#' if there are no ages.
#'
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @return A named character vector, as `questionnaire_service_registry_choices()`.
questionnaire_service_respondent_type_choices <- function(catalog) {
  types <- catalog$respondent_types
  ages <- age_range_text(types$minimum_age, types$maximum_age)
  stats::setNames(as.character(types$respondent_type_id), paste0(types$name, " (", ages, ")", recycle0 = TRUE))
}

#' A study's questionnaires for a drop-down.
#'
#' Names can repeat, so each choice also shows its respondent type and how many waves it has,
#' for example "Adult Health and Personality (AHP) · Adult · 2 waves so far".
#'
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param study_id The study's id.
#' @return A named character vector, as `questionnaire_service_registry_choices()`.
questionnaire_service_choices <- function(catalog, study_id) {
  questionnaires <- catalog$questionnaires[catalog$questionnaires$study_id %in% study_id, ]
  questionnaires <- questionnaires[order(questionnaires$name, questionnaires$abbreviation), ]
  labels <- paste0(
    questionnaires$name, " (", questionnaires$abbreviation, ") · ",
    questionnaires$respondent_type, " · ",
    questionnaires$n_waves, ifelse(questionnaires$n_waves == 1, " wave", " waves"), " so far",
    recycle0 = TRUE
  )
  stats::setNames(as.character(questionnaires$questionnaire_id), labels)
}
