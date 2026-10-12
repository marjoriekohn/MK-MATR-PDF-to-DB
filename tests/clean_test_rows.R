# # Removes the rows a crashed test run left in the database. From the project folder:
# #   Rscript tests/tools/clean_test_rows.R
# # Every test names the rows it makes with a marker ("ZZTEST0712ab ..."); this finds the rows with such
# # a name and removes them and what hangs off them. It refuses to run unless SUPABASE_URL is the dev
# # project (supabase/dev_project.R) or a database on this computer, and never touches a row whose name
# # doesn't start with ZZTEST.
#
# if (file.exists(".env")) readRenviron(".env")
# suppressMessages({
#   library(shiny)
#   library(testthat)
# })
# for (path in sort(list.files("R", "\\.[Rr]$", full.names = TRUE))) sys.source(path, envir = globalenv())
# source("tests/testthat/helper-00_realdb.R")
#
# connection <- test_connection # checks the address
# like <- function(column) stats::setNames(list("like.ZZTEST*"), column)
# ids_of <- function(table, key, column) {
#   database_model_get(connection, table, key, order_by = key, filters = like(column))[[key]]
# }
# removed <- function(what, n) if (n > 0) cat(sprintf("removed %d %s\n", n, what))
#
# # the staff, with their drafts and history
# staff <- ids_of("staff", "staff_id", "name")
# for (id in staff) test_remove_staff(id)
# removed("staff members (with their drafts and history)", length(staff))
#
# # a marked questionnaire's waves, variables and the rows that hang off them, then the questionnaire
# questionnaires <- ids_of("questionnaire", "questionnaire_id", "name")
# for (id in questionnaires) {
#   by_q <- list(questionnaire_id = database_eq(id))
#   # (a variable's subtype row and choices go with it)
#   for (table in c("variable", "wave_question", "questionnaire_question", "questionnaire_component", "wave")) {
#     test_delete(table, by_q)
#   }
# }
# test_delete_in("questionnaire", "questionnaire_id", questionnaires)
# removed("questionnaires (with their waves, blocks and variables)", length(questionnaires))
#
# # marked answer choices, and the sets they make up (a set's name is made of its choices' words)
# options <- ids_of("answer_option", "answer_option_id", "standardized_label")
# sets <- ids_of("option_set", "option_set_id", "name")
# test_delete_in("answer_option_set", "answer_option_id", options)
# test_delete_in("answer_option_set", "option_set_id", sets)
# test_delete_in("option_set", "option_set_id", sets)
# removed("sets of choices", length(sets))
#
# # the rest, children first
# for (spec in list(
#   c("study", "study_id", "name"), c("registry", "registry_id", "name"),
#   c("principal_investigator", "principal_investigator_id", "name"),
#   c("respondent_type", "respondent_type_id", "name"), c("subject", "subject_id", "name"),
#   c("category", "mesh_id", "term"), c("answer_option", "answer_option_id", "standardized_label"),
#   c("measurement_scale", "scale_id", "name")
# )) {
#   ids <- ids_of(spec[1], spec[2], spec[3])
#   test_delete_in(spec[1], spec[2], ids)
#   removed(paste0("rows of ", spec[1]), length(ids))
# }
# components <- ids_of("component", "component_id", "text")
# for (table in c("question_category", "question_scale", "questionnaire_question", "questionnaire_component", "wave_question", "variable", "question")) {
#   test_delete_in(table, "component_id", components)
# }
# test_delete_in("component", "component_id", components)
# removed("components", length(components))
# cat("done\n")
