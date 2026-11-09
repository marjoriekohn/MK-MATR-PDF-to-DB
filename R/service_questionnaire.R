# ------ Questionnaires already recorded ------------------------------------------
# What the extraction pages need to know about the live tables: the registries
# and their studies, each study's questionnaires and their waves, and who a
# questionnaire can be for. It's read when a session starts (and again after staff
# add something from a drop-down), and the drop-downs and the checks on what staff
# type use this copy, so typing never waits on the database.

# The live tables the extraction pages pick from and check against.
# Read when a session starts, and again after staff add something from a drop-down, so typing
# never waits on the database.
# Returns: A list of data frames: `registries`, `studies` (with `principal_investigator`),
#   `questionnaires` (with `respondent_type` and `n_waves`), `waves` and `respondent_types`.
questionnaire_service_get_catalog <- function(connection) {
  # three requests: the studies come with their investigators, questionnaires and waves
  questionnaire_service_catalog(c(
    list(registries = registry_model_get_all(connection)),
    study_model_get_tree(connection),
    list(respondent_types = respondent_type_model_get_all(connection))
  ))
}

# Join the tables the catalog is made of.
# Each study gets its investigator's name, and each questionnaire its respondent type's name and
# how many waves it has.
# tables: A list of data frames, as the models read them: `registries`, `studies`,
#   `investigators`, `questionnaires`, `waves` and `respondent_types`.
# Returns: The catalog; see `questionnaire_service_get_catalog()`.
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

# One questionnaire from the catalog.
# questionnaire_id: The id.
# Returns: A one-row data frame, or `NULL` if there is not exactly one.
questionnaire_service_get <- function(catalog, questionnaire_id) {
  questionnaires <- catalog$questionnaires
  questionnaire <- questionnaires[questionnaires$questionnaire_id %in% questionnaire_id, ]
  if (nrow(questionnaire) == 1) questionnaire
}

# One study from the catalog.
# study_id: The id.
# Returns: A one-row data frame, or `NULL` if there is not exactly one.
questionnaire_service_get_study <- function(catalog, study_id) {
  study <- catalog$studies[catalog$studies$study_id %in% study_id, ]
  if (nrow(study) == 1) study
}

# How many questions each of a questionnaire's waves asks.
# questionnaire_id: The questionnaire's id; blank gives none.
# Returns: A named integer vector: the count by wave id. A wave that asks nothing isn't in it.
questionnaire_service_wave_question_counts <- function(connection, questionnaire_id) {
  if (!grepl("^[0-9]+$", questionnaire_id %||% "")) {
    return(integer())
  }
  asked <- wave_question_model_get(connection, questionnaire_id)
  counts <- table(as.character(asked$wave_id))
  stats::setNames(as.integer(counts), names(counts))
}

# ------ Drop-down choices -------------------------------------------------------------
# Named vectors: the names are shown, and the values, the rows' IDs, are saved.
# With no rows there are no labels (recycle0), rather than one made of the
# words around them.

# Registries for a drop-down.
# Returns: A named character vector: the names are shown, and the values, the ids, are
#   saved. Empty if there are none.
questionnaire_service_registry_choices <- function(catalog) {
  registries <- catalog$registries
  stats::setNames(as.character(registries$registry_id), registries$name)
}

# A registry's studies for a drop-down, each with who leads it.
# For example "Adult Health Study (Dana Reyes)".
# registry_id: The registry's id.
questionnaire_service_study_choices <- function(catalog, registry_id) {
  studies <- catalog$studies[catalog$studies$registry_id %in% registry_id, ]
  labels <- ifelse(
    is.na(studies$principal_investigator),
    studies$name,
    paste0(studies$name, " (", studies$principal_investigator, ")", recycle0 = TRUE)
  )
  stats::setNames(as.character(studies$study_id), labels)
}

# A questionnaire's waves, for a drop-down, each saying whether it asks questions yet.
# An extraction can only go into a wave that asks none (a wave added on Make an edit before its questionnaire
# was extracted, or one emptied there): one that asks questions already is offered so staff see why it won't
# do, and told to empty it first.
# question_counts: From `questionnaire_service_wave_question_counts()`.
questionnaire_service_wave_choices <- function(catalog, questionnaire_id, question_counts = integer()) {
  waves <- catalog$waves[catalog$waves$questionnaire_id %in% questionnaire_id, , drop = FALSE]
  n <- question_counts[as.character(waves$wave_id)]
  n[is.na(n)] <- 0L
  dates <- ifelse(is.na(waves$administered_date), "", paste0(" · given ", format(as.Date(waves$administered_date), "%m/%d/%Y")))
  asks <- ifelse(n == 0, " · no questions yet", paste0(" · asks ", vapply(n, plural, character(1), "question"), ": empty it first"))
  stats::setNames(as.character(waves$wave_id), paste0(waves$name, dates, asks, recycle0 = TRUE))
}

# A study's questionnaires for a drop-down.
# Names can repeat, so each choice also shows its respondent type and how many waves it has,
# for example "Adult Health and Personality (AHP) · Adult · 2 waves so far".
# study_id: The study's id.
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
