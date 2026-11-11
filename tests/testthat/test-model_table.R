# The Metadata page reads whole tables. Each is listed in database_tables with its columns and what kind of value
# each holds; these check it against the real database. (That every listed table and column is in the database is
# checked in test-model_database.R, with the one read of the database's description.)

test_that("columns come back as the kinds the table lists, and a null is NA", {
  type <- local_respondent_type(minimum_age = 5, maximum_age = NULL)
  rows <- table_model_get(test_connection, "respondent_type", list(name = database_eq(type$name)))
  expect_equal(nrow(rows), 1)
  expect_named(rows, c("respondent_type_id", "name", "minimum_age", "maximum_age", "description"))
  expect_type(rows$respondent_type_id, "double")
  expect_type(rows$minimum_age, "double")
  expect_equal(rows$minimum_age, 5)
  expect_type(rows$name, "character")
  expect_true(is.na(rows$maximum_age))
  expect_true(is.na(rows$description))
})

test_that("numbers in text columns stay text", {
  numbers <- table_model_get(test_connection, "wave_question", list(wave_id = database_eq(0)))
  expect_type(numbers$question_number, "character")
  expect_type(numbers$placement_id, "double")
  expect_type(table_model_get(test_connection, "pdf_file", list(pdf_sha256 = database_eq("none")))$pdf_sha256, "character")
  words <- paste("what is your", tolower(test_name("height")))
  question <- local_row("component", list(kind = "Question", text = words), "component_id")
  rows <- table_model_get(test_connection, "component", list(component_id = database_eq(question$component_id)))
  expect_named(rows, c("component_id", "kind", "text"))
  expect_equal(rows$text, words) # kept as printed
})

test_that("a table the page doesn't know is a sentence", {
  expect_error(table_model_get(test_connection, "staff"), "doesn't know a table called staff")
  expect_error(table_model_get(test_connection, "component_type"), "doesn't know a table called component_type")
})

test_that("a table with no matching rows comes back with its columns and the right kinds", {
  rows <- table_model_get(test_connection, "placement", list(questionnaire_id = database_eq(0)))
  expect_equal(nrow(rows), 0)
  expect_named(rows, c("placement_id", "questionnaire_id", "component_id", "parent_placement_id", "display_order", "depends_on_placement_id"))
  expect_type(rows$display_order, "double")
  expect_type(rows$component_id, "double")
})

test_that("rows come back in the table's order: tags by kind, then name", {
  marked <- list(name = paste0("like.", test_marker, "*"))
  local_row("tag", list(kind = "unit", name = paste(test_marker, "A unit")), "tag_id")
  local_row("tag", list(kind = "subject", name = paste(test_marker, "B subject")), "tag_id")
  local_row("tag", list(kind = "subject", name = paste(test_marker, "A subject")), "tag_id")
  tags <- table_model_get(test_connection, "tag", marked)
  expect_equal(tags$name, paste(test_marker, c("A subject", "B subject", "A unit")))
  expect_equal(database_tables$tag$key, "tag_id")
})

test_that("a table is read whole, every page of it, in the order of its name", {
  made <- list()
  for (letter in c("C", "A", "B")) made[[letter]] <- local_registry(name = paste(test_marker, letter, "whole"))
  registries <- table_model_get(test_connection, "registry")
  expect_true(all(vapply(made, function(row) row$registry_id, numeric(1)) %in% registries$registry_id))
  mine <- registries[startsWith(registries$name, test_marker), ]
  expect_equal(mine$name, paste(test_marker, c("A", "B", "C"), "whole"))
  expect_equal(anyDuplicated(registries$registry_id), 0)
})
