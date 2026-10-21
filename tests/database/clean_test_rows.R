# Removes the rows a crashed test run left in the database.
# From the project folder:
#   Rscript tests/database/clean_test_rows.R
#
# Every test names the rows it makes with a marker ("ZZTEST0712ab ...").
# This script removes marked rows and their dependencies.
# It only works on the test project or a local database.

# --- Progress logging -------------------------------------------------------

log_info <- function(message) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), message))
  flush.console()
}

log_step <- function(number, total, message) {
  log_info(sprintf("[%d/%d] %s", number, total, message))
}

removed <- function(what, n) {
  log_info(sprintf("  Removed %d %s", n, what))
}

ids_of <- function(table, key, column) {
  log_info(sprintf("  Searching %s for ZZTEST rows...", table))
  ids <- database_model_get(
    connection, table, key,
    order_by = key,
    filters = like(column)
  )[[key]]
  log_info(sprintf("  Found %d matching rows", length(ids)))
  ids
}

like <- function(column) {
  stats::setNames(list("like.ZZTEST*"), column)
}

start_time <- Sys.time()
total_steps <- 7L

log_info("Starting test database cleanup")

# --- Step 1: Load environment -----------------------------------------------

log_step(1, total_steps, "Loading environment and R dependencies")

if (file.exists(".env")) {
  readRenviron(".env")
  log_info("  Loaded .env")
} else {
  log_info("  No .env file found")
}

suppressMessages({
  library(shiny)
  library(testthat)
})

log_info("  R packages loaded")

paths <- sort(list.files("R", "\\.[Rr]$", full.names = TRUE))
log_info(sprintf("  Loading %d project R files...", length(paths)))

for (path in paths) {
  sys.source(path, envir = globalenv())
}

log_info("  Project R files loaded")

source("tests/testthat/helper-00_realdb.R")
log_info("  Database test helpers loaded")

# --- Step 2: Connect to test database ---------------------------------------

log_step(2, total_steps, "Connecting to test database")

connection <- test_database()
log_info("  Test database connection established and validated")

# --- Step 3: Remove staff ---------------------------------------------------

log_step(3, total_steps, "Cleaning up staff and associated records")

staff <- ids_of("staff", "staff_id", "name")

for (i in seq_along(staff)) {
  log_info(sprintf("  Deleting staff %d/%d...", i, length(staff)))
  test_remove_staff(staff[[i]])
}

removed("staff members (with their drafts and history)", length(staff))

# --- Step 4: Remove questionnaires -----------------------------------------

log_step(4, total_steps, "Cleaning up questionnaires and dependencies")

questionnaires <- ids_of(
  "questionnaire", "questionnaire_id", "name"
)

for (i in seq_along(questionnaires)) {
  id <- questionnaires[[i]]

  log_info(sprintf(
    "  Processing questionnaire %d/%d...",
    i, length(questionnaires)
  ))

  by_q <- list(questionnaire_id = database_eq(id))

  for (table in c(
    "variable",
    "wave_question",
    "questionnaire_question",
    "questionnaire_component",
    "wave"
  )) {
    log_info(sprintf("    Deleting related %s rows...", table))
    test_delete(table, by_q)
  }
}

log_info("  Deleting questionnaire records...")
test_delete_in("questionnaire", "questionnaire_id", questionnaires)

removed(
  "questionnaires (with their waves, blocks and variables)",
  length(questionnaires)
)

# --- Step 5: Remove answer option sets --------------------------------------

log_step(5, total_steps, "Cleaning up answer option sets")

options <- ids_of(
  "answer_option", "answer_option_id", "standardized_label"
)

sets <- ids_of("option_set", "option_set_id", "name")

log_info("  Deleting answer-option associations...")
test_delete_in("answer_option_set", "answer_option_id", options)

log_info("  Deleting option-set associations...")
test_delete_in("answer_option_set", "option_set_id", sets)

log_info("  Deleting option sets...")
test_delete_in("option_set", "option_set_id", sets)

removed("sets of choices", length(sets))

# --- Step 6: Remove remaining marked rows ----------------------------------

log_step(6, total_steps, "Cleaning up remaining marked records")

specs <- list(
  c("study", "study_id", "name"),
  c("registry", "registry_id", "name"),
  c("principal_investigator", "principal_investigator_id", "name"),
  c("respondent_type", "respondent_type_id", "name"),
  c("subject", "subject_id", "name"),
  c("category", "mesh_id", "term"),
  c("answer_option", "answer_option_id", "standardized_label"),
  c("measurement_scale", "scale_id", "name")
)

for (i in seq_along(specs)) {
  spec <- specs[[i]]

  log_info(sprintf(
    "  Processing table %d/%d: %s",
    i, length(specs), spec[1]
  ))

  ids <- ids_of(spec[1], spec[2], spec[3])

  if (length(ids) > 0) {
    log_info(sprintf("  Deleting %d rows...", length(ids)))
  }

  test_delete_in(spec[1], spec[2], ids)
  removed(paste0("rows of ", spec[1]), length(ids))
}

# --- Step 7: Remove components ----------------------------------------------

log_step(7, total_steps, "Cleaning up components and dependencies")

components <- ids_of("component", "component_id", "text")

component_tables <- c(
  "question_category",
  "question_scale",
  "questionnaire_question",
  "questionnaire_component",
  "wave_question",
  "variable",
  "question"
)

for (i in seq_along(component_tables)) {
  table <- component_tables[[i]]

  log_info(sprintf(
    "  Cleaning dependency %d/%d: %s",
    i, length(component_tables), table
  ))

  test_delete_in(table, "component_id", components)
}

log_info("  Deleting component records...")
test_delete_in("component", "component_id", components)

removed("components", length(components))

# --- Complete ---------------------------------------------------------------

elapsed <- as.numeric(
  difftime(Sys.time(), start_time, units = "secs")
)

log_info("--------------------------------------------------")
log_info("Test database cleanup completed successfully")
log_info(sprintf("Total execution time: %.2f seconds", elapsed))
log_info("--------------------------------------------------")
