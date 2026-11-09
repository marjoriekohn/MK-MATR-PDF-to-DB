# Removes the rows a crashed test run left in the database.
# From the project folder:
#   Rscript tests/database/clean_test_rows.R
#
# Every test names the rows it makes with a marker ("ZZTEST0712ab ..."), and removes them when it ends; a run
# that crashed first leaves them. This removes every marked row, in three requests to the test project's own
# functions (tests/database/test_functions.sql) and one per marked tree: the tests' staff with their extractions
# and history, the tests' trees (registry to wave) with everything pushed into them, and the vocabulary a push
# adds (tags, answer options, components) and extractions named with the marker. It only works on the test
# project or a local database.

log_info <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
  flush.console()
}

# ------ Loading -----------------------------------------------------------------------------------------------
if (file.exists(".env")) readRenviron(".env")
suppressMessages({
  library(shiny)
  library(testthat)
})
for (path in sort(list.files("R", "\\.[Rr]$", full.names = TRUE))) sys.source(path, envir = globalenv())
source("tests/testthat/helper-00_realdb.R")
connection <- test_database()
log_info("Test database: ", connection$url)

# ------ The tests' staff, with their extractions and history -----------------------------------------------------
forgot <- test_forget_staff(marker = "ZZTEST", remove_staff = TRUE)
log_info(sprintf("Removed %d staff members, %d extractions and %d rows of history", forgot$staff, forgot$extractions, forgot$history))

# ------ The tests' trees, with what was pushed into them ------------------------------------------------------------
# test_forget_tree() takes what test_make_tree() returned: the rows, found again here by their marked names. A
# questionnaire's placements, questions and variables go with it; the components only it printed go afterwards.
marked <- function(table, columns) {
  database_model_select(connection, table, columns, filters = list(name = "ilike.ZZTEST*"))
}
registries <- marked("registry", "registry_id,name")
log_info(sprintf("Found %d marked registries", length(registries)))
for (registry in registries) {
  studies <- database_model_select(connection, "study", "study_id,principal_investigator_id", filters = list(registry_id = database_eq(registry$registry_id)))
  for (study in studies) {
    questionnaires <- database_model_select(connection, "questionnaire", "questionnaire_id,respondent_type_id", filters = list(study_id = database_eq(study$study_id)))
    for (questionnaire in questionnaires) {
      database_model_call(connection, "test_forget_tree", list(p_tree = list(
        registry = registry,
        study = list(study_id = study$study_id),
        questionnaire = list(questionnaire_id = questionnaire$questionnaire_id),
        # the respondent type and investigator go only if nothing else uses them: those a test made alone
        respondent_type = list(respondent_type_id = if (length(questionnaires) == 1) questionnaire$respondent_type_id),
        investigator = list(principal_investigator_id = if (length(studies) == 1) study$principal_investigator_id)
      )))
    }
    if (length(questionnaires) == 0) test_delete("study", list(study_id = database_eq(study$study_id)))
  }
  if (length(studies) == 0) test_delete("registry", list(registry_id = database_eq(registry$registry_id)))
}
# marked rows a test made on their own, outside a tree, where nothing points at them any more
for (spec in list(
  c("wave", "wave_id"), c("questionnaire", "questionnaire_id"), c("study", "study_id"), c("registry", "registry_id"),
  c("principal_investigator", "principal_investigator_id"), c("respondent_type", "respondent_type_id"),
  c("measurement_scale", "scale_id")
)) {
  rows <- marked(spec[1], spec[2])
  ids <- vapply(rows, function(row) as.numeric(row[[spec[2]]]), numeric(1))
  if (spec[1] == "measurement_scale") test_delete_in("question_scale", "scale_id", ids)
  test_delete_in(spec[1], spec[2], ids)
  if (length(ids) > 0) log_info(sprintf("Removed %d rows of %s", length(ids), spec[1]))
}

# ------ The vocabulary pushes added, and marked extractions ---------------------------------------------------------
gone <- test_forget_marked("ZZTEST")
log_info(sprintf(
  "Removed %d tags, %d answer options, %d components and %d extractions named with the marker",
  gone$tags, gone$answer_options, gone$components, gone$extractions
))
log_info("Done. test-zz_leftovers.R says whether anything is still there.")
