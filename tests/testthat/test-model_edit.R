# Applying a change (the database's apply_change) and emptying (empty_wave and the rest), against the real database.

history_row <- function(history_id) {
  rows <- database_model_select(test_connection, "history", "history_id,staff_id,action,entity,record_id,summary,details", filters = list(history_id = database_eq(history_id)))
  if (length(rows) == 1) rows[[1]]
}

test_that("a change is applied in one go, numbered by the database, and written to the history once", {
  me <- local_staff()
  name <- test_name("Applied type")
  items <- edit_number_items(rbind(
    edit_item("respondent_type", '{"respondent_type_id":"new:respondent_type:1"}', "Insert", "name", NA, name),
    edit_item("respondent_type", '{"respondent_type_id":"new:respondent_type:1"}', "Insert", "minimum_age", NA, 5)
  ))
  applied <- edit_model_apply(test_connection, items, paste0("Added the respondent type “", name, "”"), me)
  local_added_rows(applied$new_ids)

  expect_match(applied$history_id, "^[0-9]+$")
  expect_named(applied$new_ids, "new:respondent_type:1")
  rows <- table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(applied$new_ids[[1]])))
  expect_equal(rows$name, name)
  expect_equal(rows$minimum_age, 5)
  entry <- history_row(applied$history_id)
  expect_equal(entry$action, "Added")
  expect_equal(entry$entity, "respondent_type")
  expect_equal(entry$staff_id, me$staff_id)
  expect_equal(entry$summary, paste0("Added the respondent type “", name, "”"))
  expect_equal(entry$record_id, sprintf('{"respondent_type_id":%s}', key_text(applied$new_ids[[1]])))
  expect_equal(length(entry$details), 2) # the items as applied
})

test_that("an update whose old value is stale is refused with a sentence, and nothing changes", {
  me <- local_staff()
  type <- local_respondent_type()
  stale <- edit_number_items(edit_item("respondent_type", edit_identifier(list(respondent_type_id = type$respondent_type_id)), "Update", "name", "Not its name", "New name"))
  expect_error(edit_model_apply(test_connection, stale, "Changed it", me), "out of date")
  expect_equal(table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(type$respondent_type_id)))$name, type$name)
  fresh <- edit_number_items(edit_item("respondent_type", edit_identifier(list(respondent_type_id = type$respondent_type_id)), "Update", "name", type$name, paste(type$name, "renamed")))
  applied <- edit_model_apply(test_connection, fresh, "Changed it", me)
  expect_equal(history_row(applied$history_id)$action, "Updated")
  expect_equal(table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(type$respondent_type_id)))$name, paste(type$name, "renamed"))
})

test_that("only an active staff member can apply a change, and an empty change is refused before it is sent", {
  gone <- local_staff(active = FALSE)
  items <- edit_number_items(edit_item("tag", '{"tag_id":"new:tag:1"}', "Insert", "name", NA, test_name("Tag")))
  expect_error(edit_model_apply(test_connection, items, "Added a tag", gone), "active staff member")
  expect_error(edit_service_apply(test_connection, list(items = edit_number_items(NULL), summary = "Nothing"), gone), "nothing in this change")
})

test_that("what a wave, questionnaire, study or registry holds is counted, and emptying one removes it and says so", {
  me <- local_staff()
  tree <- local_tree()
  expect_equal(edit_model_contents(test_connection, "wave", tree$wave$wave_id), list(questions = 0L))
  for (what in c("questionnaire", "study", "registry")) {
    counts <- edit_model_contents(test_connection, what, tree[[what]][[paste0(what, "_id")]])
    expect_equal(counts, list(questions = 0L, placements = 0L, variables = 0L), info = what)
  }
  expect_equal(edit_service_empty_preview(test_connection, "wave", tree$wave$wave_id), "It asks no questions, so there is nothing to remove.")
  expect_equal(edit_service_empty_preview(test_connection, "study", tree$study$study_id), "Nothing is printed or asked in it, so there is nothing to remove.")
  # a block in the questionnaire counts, and goes when the questionnaire is emptied
  section <- local_row("component", list(kind = "Section", text = tolower(test_name("section"))), "component_id")
  placed <- database_model_insert(test_connection, "placement", list(questionnaire_id = tree$questionnaire$questionnaire_id, component_id = section$component_id, display_order = 1))
  withr::defer(try(test_delete("placement", list(placement_id = database_eq(placed$placement_id))), silent = TRUE))
  expect_equal(edit_model_contents(test_connection, "questionnaire", tree$questionnaire$questionnaire_id)$placements, 1L)
  expect_equal(edit_service_empty_preview(test_connection, "questionnaire", tree$questionnaire$questionnaire_id), "It holds 1 block and 0 variables, asked as 0 questions in its waves.")
  gone <- edit_model_empty(test_connection, "questionnaire", tree$questionnaire$questionnaire_id, me)
  expect_equal(gone[c("questions_removed", "placements_removed", "variables_removed")], list(questions_removed = 0, placements_removed = 1, variables_removed = 0))
  expect_equal(nrow(table_model_get(test_connection, "placement", list(questionnaire_id = database_eq(tree$questionnaire$questionnaire_id)))), 0)
  # the questionnaire and its wave stay
  expect_equal(nrow(table_model_get(test_connection, "wave", list(wave_id = database_eq(tree$wave$wave_id)))), 1)
  expect_equal(edit_service_empty(test_connection, "wave", tree$wave$wave_id, me), "Removed 0 questions, 0 blocks and 0 variables.")
  expect_error(edit_model_empty(test_connection, "component", 1, me), "Only a registry, study, questionnaire or wave")
  expect_error(edit_model_empty(test_connection, "wave", 0, me), "no longer in the database")
})
