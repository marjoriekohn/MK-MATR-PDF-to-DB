# The Edit History service: which changes to show, who made each and when, and what each did. Records are made
# here as history_model_get_all() gives them; only the test of what a change did reads the database.

sample_history_record <- function(id, action, summary, staff, occurred = "2026-10-02T15:30:00+00:00") {
  list(id = id, action = action, entity = "respondent_type", record_id = "1", summary = summary, staff = staff, occurred = occurred)
}
h <- list(ids = c("105", "104", "103", "102", "101"), admin = "Alice Admin", me = "Maggie Staff")
h$records <- stats::setNames(list(
  sample_history_record("105", "Pushed", "Pushed the extraction of Kid Questionnaire, Wave 1: 40 questions, 60 new variables.", h$me),
  sample_history_record("104", "Emptied", "Emptied Kid Questionnaire, Wave 1: 40 questions removed.", h$me),
  sample_history_record("103", "Deleted", "Deleted the respondent type “Parents renamed”", h$admin),
  sample_history_record("102", "Updated", "Changed the name of the respondent type “Parents”", h$admin),
  sample_history_record("101", "Added", "Added the respondent type “Parents”", h$admin)
), h$ids)

test_that("a kind of change narrows the list, and 'all' keeps every kind", {
  expect_equal(names(history_service_filter(h$records, "all")), h$ids)
  expect_equal(names(history_service_filter(h$records, "Pushed")), h$ids[1])
  expect_equal(names(history_service_filter(h$records, "Emptied")), h$ids[2])
  expect_equal(names(history_service_filter(h$records, "Deleted")), h$ids[3])
  expect_equal(names(history_service_filter(h$records, "Updated")), h$ids[4])
  expect_equal(names(history_service_filter(h$records, "Added")), h$ids[5])
  expect_equal(names(history_service_filter(h$records, "something else")), h$ids)
})

test_that("every word typed has to be in the sentence or the name of who made it, ignoring capitals", {
  kept <- function(...) names(history_service_filter(h$records, ...))
  expect_equal(kept("all", "CHANGED"), h$ids[4]) # the sentence
  expect_equal(kept("all", "kid questionnaire"), h$ids[1:2])
  expect_equal(kept("all", "alice"), h$ids[3:5]) # who made three
  expect_equal(kept("Pushed", "maggie"), h$ids[1])
  expect_length(history_service_filter(h$records, "all", "no such thing"), 0)
  expect_equal(kept("Updated", "  "), h$ids[4])
  expect_equal(kept("all", NULL), h$ids)
})

test_that("who made a change, and when, read as words", {
  expect_equal(history_service_who(h$records[[1]]), "by Maggie Staff")
  expect_equal(history_service_who(list(staff = NA_character_)), "by someone no longer listed")
  record <- h$records[[1]]
  withr::local_timezone("UTC")
  occurred <- parse_time(record$occurred)
  expect_match(history_service_when(record, occurred + 60), "^Today at [0-9]{1,2}:[0-9]{2} [AP]M$")
  expect_equal(history_service_when(record, occurred + 24 * 3600), "Yesterday")
  expect_match(history_service_when(record, occurred + 30 * 24 * 3600), "^On [A-Z][a-z]{2} [0-9]{1,2}$")
})

test_that("a push's or an emptying's counts are lines in words, questions first and the ids left out", {
  lines <- history_service_count_lines(list(
    variable_new = 1200, component_reused = 3, questions = 40, extraction_id = 99, wave_id = 7, questionnaire_id = 2
  ))
  expect_equal(lines, c("Questions: 40", "New variables: 1,200", "Components reused: 3"))
  expect_equal(
    history_service_count_lines(list(questions_removed = 40, placements_removed = 52, variables_removed = 60)),
    c("Questions removed: 40", "Placements removed: 52", "Variables removed: 60")
  )
  expect_length(history_service_count_lines(NULL), 0)
  expect_length(history_service_count_lines(list(list(item_number = 1))), 0) # items, not counts
})

test_that("what a change did is its rows, grouped, with the counts, or a push's counts", {
  me <- local_staff()
  updated <- local_history(me, "Updated", "Changed the name of the respondent type “Parents”", details = list(
    list(item_number = 1, entity_name = "respondent_type", record_identifier = '{"respondent_type_id":5}', action = "Update", attribute_name = "name", old_value = "Parents", new_value = "Guardians")
  ))
  pushed <- local_history(me, "Pushed", "Pushed the extraction of Kid Questionnaire, Wave 1: 2 questions, 3 new variables.", entity = "wave", details = list(
    questions = 2, variable_new = 3, extraction_id = 99
  ))

  changes <- history_service_changes(test_connection, as.character(updated$history_id))
  expect_equal(changes$record$action, "Updated")
  expect_length(changes$groups, 1)
  expect_equal(changes$groups[[1]]$table, "respondent_type")
  expect_equal(changes$groups[[1]]$row, "respondent_type_id 5")
  expect_equal(changes$groups[[1]]$fields$column, "name")
  expect_equal(changes$counts, list(added = 0L, changed = 1L, removed = 0L, tables = 1L))
  expect_equal(changes$text, "Changes 1 row, in 1 table.")
  expect_null(changes$lines)

  push <- history_service_changes(test_connection, as.character(pushed$history_id))
  expect_equal(push$record$action, "Pushed")
  expect_equal(push$lines, c("Questions: 2", "New variables: 3"))
  expect_null(push$groups)
})
