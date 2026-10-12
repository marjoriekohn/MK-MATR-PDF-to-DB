# # The edit pages read whole tables. Each is listed in database_tables with its columns and what kind of
# # value each holds; this checks the list against the real database.
#
# test_that("every table the edit pages read is in the database, with every column listed", {
#   description <- database_model_describe(test_connection)$definitions
#   for (table in names(database_tables)) {
#     expect_true(table %in% names(description), info = paste("the database has no table", table))
#     expect_true(
#       all(names(database_tables[[table]]$columns) %in% names(description[[table]]$properties)),
#       info = paste("a column of", table, "isn't in the database")
#     )
#     expect_true(all(database_tables[[table]]$key %in% names(database_tables[[table]]$columns)), info = table)
#   }
# })
#
# test_that("columns come back as the kinds the table lists, and a null is NA", {
#   type <- local_respondent_type(minimum_age = 5, maximum_age = NULL)
#   rows <- table_model_get(test_connection, "respondent_type", list(name = database_eq(type$name)))
#   expect_equal(nrow(rows), 1)
#   expect_named(rows, c("respondent_type_id", "name", "minimum_age", "maximum_age", "description"))
#   expect_type(rows$respondent_type_id, "double")
#   expect_type(rows$minimum_age, "double")
#   expect_equal(rows$minimum_age, 5)
#   expect_type(rows$name, "character")
#   expect_true(is.na(rows$maximum_age))
#   expect_true(is.na(rows$description))
# })
#
# test_that("flags come back as logical and numbers in text columns stay text", {
#   questions <- table_model_get(test_connection, "questionnaire_question", list(questionnaire_id = database_eq(0)))
#   expect_type(questions$self_contained, "logical")
#   expect_type(questions$component_id, "double")
#   numbers <- table_model_get(test_connection, "wave_question", list(wave_id = database_eq(0)))
#   expect_type(numbers$question_number, "character")
#   expect_type(table_model_get(test_connection, "category", list(mesh_id = database_eq("none")))$mesh_id, "character")
# })
#
# test_that("a table the pages don't know is a sentence", {
#   expect_error(table_model_get(test_connection, "staff"), "don't know a table called staff")
# })
#
# test_that("a table with no matching rows comes back with its columns and the right kinds", {
#   rows <- table_model_get(test_connection, "questionnaire_question", list(questionnaire_id = database_eq(0)))
#   expect_equal(nrow(rows), 0)
#   expect_named(rows, c("questionnaire_id", "component_id", "parent_question_id", "self_contained", "condition"))
#   expect_type(rows$self_contained, "logical")
#   expect_type(rows$component_id, "double")
# })
#
# test_that("rows come back in the order of the table's key columns, a subject by its name", {
#   marked <- list(name = paste0("like.", test_marker, "*"))
#   local_row("subject", list(name = paste(test_marker, "B subject")), "subject_id")
#   local_row("subject", list(name = paste(test_marker, "A subject")), "subject_id")
#   subjects <- table_model_get(test_connection, "subject", marked)
#   expect_equal(subjects$name, paste(test_marker, c("A", "B"), "subject"))
#   expect_equal(database_tables$subject$key, "subject_id")
# })
#
# test_that("a table is read whole, every page of it, in the order of its name", {
#   made <- list()
#   for (letter in c("C", "A", "B")) made[[letter]] <- local_registry(name = paste(test_marker, letter, "whole"))
#   registries <- table_model_get(test_connection, "registry")
#   expect_true(all(vapply(made, function(row) row$registry_id, numeric(1)) %in% registries$registry_id))
#   mine <- registries[startsWith(registries$name, test_marker), ]
#   expect_equal(mine$name, paste(test_marker, c("A", "B", "C"), "whole"))
#   expect_equal(anyDuplicated(registries$registry_id), 0)
# })
