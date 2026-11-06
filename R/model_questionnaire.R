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

#' The questions one questionnaire's waves ask.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param questionnaire_id The questionnaire's id.
#' @return A data frame with `wave_id` and `component_id`, one row per question a wave asks.
wave_question_model_get <- function(connection, questionnaire_id) {
  database_model_get(
    connection, "wave_question", c("wave_id", "placement_id"),
    order_by = c("wave_id", "placement_id"), filters = list(questionnaire_id = database_eq(questionnaire_id))
  )
}

#' Every study, with its lead investigator's name, its questionnaires and their waves, in one request.
#'
#' Every questionnaire is in a study and every wave in a questionnaire, so this is all of each, without a
#' request for each table.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of data frames: `studies` (`study_id`, `registry_id`, `principal_investigator_id` and `name`,
#'   by name), `investigators` (`principal_investigator_id` and `name`, only those who lead a study),
#'   `questionnaires` (`questionnaire_id`, `study_id`, `respondent_type_id`, `name` and `abbreviation`) and
#'   `waves` (`wave_id`, `questionnaire_id`, `name`, `administered_date` and `respondent_count`), each by id.
study_model_get_tree <- function(connection) {
  columns <- list(
    studies = c("study_id", "registry_id", "principal_investigator_id", "name"),
    questionnaires = c("questionnaire_id", "study_id", "respondent_type_id", "name", "abbreviation"),
    waves = c("wave_id", "questionnaire_id", "name", "administered_date", "respondent_count")
  )
  select <- sprintf(
    # a study names two investigators (who leads it, and who to contact), so its foreign key says which
    # (docs/sql/1_setup.sql names it; the column alone is no hint, as the investigator's own is called the same)
    "%s,investigator:principal_investigator!study_principal_investigator_id_fkey(name),questionnaire(%s,wave(%s))",
    paste(columns$studies, collapse = ","), paste(columns$questionnaires, collapse = ","), paste(columns$waves, collapse = ",")
  )
  studies <- database_model_select(connection, "study", select, order_by = "name")
  questionnaires <- unlist(lapply(studies, function(study) study$questionnaire), recursive = FALSE)
  waves <- unlist(lapply(questionnaires, function(questionnaire) questionnaire$wave), recursive = FALSE)
  led <- Filter(function(study) !is.null(study$principal_investigator_id), studies)
  investigators <- unique(data_frame_of(
    lapply(led, function(study) list(principal_investigator_id = study$principal_investigator_id, name = study$investigator$name)),
    c("principal_investigator_id", "name")
  ))
  questionnaires <- data_frame_of(questionnaires, columns$questionnaires)
  waves <- data_frame_of(waves, columns$waves)
  in_order <- function(rows, by) {
    rows <- rows[order(rows[[by]]), , drop = FALSE]
    rownames(rows) <- NULL
    rows
  }
  list(
    studies = data_frame_of(studies, columns$studies),
    investigators = in_order(investigators, "principal_investigator_id"),
    questionnaires = in_order(questionnaires, "questionnaire_id"),
    waves = in_order(waves, "wave_id")
  )
}

# Rows read as lists (database_model_select()) as a data frame of `columns`, each as database_model_get() would
# give it: a missing value is NA, and a column of nothing but missing values is logical.
data_frame_of <- function(rows, columns) {
  if (length(rows) == 0) {
    return(no_rows(columns))
  }
  values <- lapply(stats::setNames(columns, columns), function(column) unlist(lapply(rows, function(row) row[[column]] %||% NA)))
  as.data.frame(values, stringsAsFactors = FALSE)
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
