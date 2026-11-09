# The Edit History model: the history table, which the database writes as staff change the catalog. The rows
# are made here as the database's functions make them (a test of those is tests/database/), so what is tested
# is reading them back.

test_that("a change is in the history with who made it, when, and what it was", {
  me <- local_staff()
  summary <- paste("Changed the name of the respondent type", test_name("Parents"))
  saved <- local_history(me, "Updated", summary, entity = "respondent_type", record_id = "123")

  record <- history_model_get_all(test_connection)[[as.character(saved$history_id)]]
  expect_equal(record$id, as.character(saved$history_id))
  expect_equal(record$action, "Updated")
  expect_equal(record$entity, "respondent_type")
  expect_equal(record$record_id, "123")
  expect_equal(record$summary, summary)
  expect_equal(record$staff, me$name)
  expect_lt(abs(as.numeric(difftime(Sys.time(), parse_time(record$occurred), units = "mins"))), 5)
  expect_null(record$details) # the list leaves the details out; history_model_get() reads them
})

test_that("the newest come first, the newest few can be read alone, and the count is of every change", {
  me <- local_staff()
  before <- history_model_count(test_connection)
  first <- local_history(me, "Added", paste("Added", test_name("First")))
  second <- local_history(me, "Deleted", paste("Deleted", test_name("Second")))
  expect_gte(history_model_count(test_connection), before + 2L) # other runs of the tests add rows meanwhile
  all <- history_model_get_all(test_connection)
  expect_lt(match(as.character(second$history_id), names(all)), match(as.character(first$history_id), names(all)))
  newest <- history_model_get_all(test_connection, most = 2)
  expect_length(newest, 2)
  expect_equal(names(newest), utils::head(names(all), 2))
})

test_that("one change comes with its details: an edit's items as a table, in the order they were applied", {
  me <- local_staff()
  items <- list(
    list(item_number = 1, entity_name = "respondent_type", record_identifier = '{"respondent_type_id":5}', action = "Update", attribute_name = "name", old_value = "Parents", new_value = "Guardians"),
    list(item_number = 2, entity_name = "respondent_type", record_identifier = '{"respondent_type_id":5}', action = "Update", attribute_name = "maximum_age", old_value = "99", new_value = NULL)
  )
  saved <- local_history(me, "Updated", details = items)
  record <- history_model_get(test_connection, as.character(saved$history_id))
  expect_equal(record$id, as.character(saved$history_id))
  expect_length(record$details, 2)
  table <- history_model_get_items(test_connection, as.character(saved$history_id))
  expect_named(table, history_item_columns)
  expect_equal(table$item_number, 1:2)
  expect_equal(table$attribute_name, c("name", "maximum_age"))
  expect_equal(table$old_value, c("Parents", "99"))
  expect_equal(table$new_value, c("Guardians", NA))
  expect_equal(unique(table$record_identifier), '{"respondent_type_id":5}')
})

test_that("a push's details are counts, which make no items table", {
  me <- local_staff()
  counts <- list(questions = 12, variable_new = 30, component_reused = 2, extraction_id = 99, wave_id = 7)
  saved <- local_history(me, "Pushed", "Pushed the extraction of Kid Questionnaire, Wave 1: 12 questions, 30 new variables.", entity = "wave", record_id = "7", details = counts)
  record <- history_model_get(test_connection, as.character(saved$history_id))
  expect_equal(record$action, "Pushed")
  expect_equal(record$details$questions, 12)
  expect_equal(record$details$variable_new, 30)
  expect_equal(nrow(history_model_get_items(test_connection, as.character(saved$history_id))), 0)
})

test_that("details that aren't items are an empty table with the items' columns", {
  for (details in list(NULL, list(questions = 3), list())) {
    table <- history_model_items(details)
    expect_equal(nrow(table), 0)
    expect_named(table, history_item_columns)
  }
  table <- history_model_items(list(list(item_number = "1", entity_name = "wave", action = "Insert")))
  expect_equal(table$item_number, 1L)
  expect_true(is.na(table$old_value))
})

test_that("only digits ask for a change, and one that isn't there says so", {
  expect_error(history_model_get(test_connection, "1 or 1=1"), "no change with that number")
  expect_error(history_model_get(test_connection, NULL), "no change with that number")
  expect_error(history_model_get(test_connection, "0"), "no longer in the history")
})

test_that("a database that can't be reached gives a sentence", {
  expect_error(history_model_get_all(list(url = "http://127.0.0.1:1", key = "sb_secret_x")), "Couldn't reach the MATR database")
})
