# The Edit History model: the applied changes (drafts with the status Applied) and the fields each
# changed (applied_item). The edits are made by applying them, as Make an edit does, so the history is
# what the database wrote.

test_that("an applied edit is in the history, with the people behind it and the times", {
  admin <- local_staff("ADMIN")
  type <- local_respondent_type()
  new_name <- test_name("Renamed")
  id <- local_applied_edit(admin, "respondent_type", "update", type$respondent_type_id, respondent_type__name = new_name)

  record <- history_model_get_all(test_connection)[[as.character(id)]]
  expect_equal(record$id, as.character(id))
  expect_equal(record$kind, "Update")
  expect_equal(record$status, "Applied")
  expect_equal(record$summary, paste0("Changed the name of the respondent type “", type$name, "”"))
  expect_equal(record$prepared_by, admin$name)
  expect_true(is.na(record$approved_by)) # it needed no approval
  expect_false(record$approval_required)
  expect_lt(abs(as.numeric(difftime(Sys.time(), parse_time(record$applied), units = "mins"))), 5)
  expect_match(record$prepared, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$")
  expect_null(record$approved)
  expect_equal(record$source, "Edit")
  expect_equal(id, edit_model_get(test_connection, id)$id) # the record is the draft itself
})

test_that("an edit someone approved says who, and that it needed approval", {
  me <- local_staff()
  approver <- local_staff()
  id <- local_approved_delete(me, approver)
  record <- history_model_get_all(test_connection)[[as.character(id)]]
  expect_equal(record$kind, "Delete")
  expect_equal(record$prepared_by, me$name)
  expect_equal(record$approved_by, approver$name)
  expect_true(record$approval_required)
  expect_match(record$approved, "^[0-9]{4}-")
})

test_that("a draft that isn't applied yet isn't in the history, and the count is of applied ones", {
  me <- local_staff()
  waiting <- local_edit(me, status = "Awaiting Review")
  before <- history_model_count(test_connection)
  expect_false(waiting$id %in% names(history_model_get_all(test_connection)))
  admin <- local_staff("ADMIN")
  id <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("Counted"))
  expect_equal(history_model_count(test_connection), before + 1L)
  expect_true(as.character(id) %in% names(history_model_get_all(test_connection)))
})

test_that("the edits come newest first", {
  admin <- local_staff("ADMIN")
  first <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("First"))
  second <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("Second"))
  records <- history_model_get_all(test_connection)
  ours <- names(records)[names(records) %in% as.character(c(first, second))]
  expect_equal(ours, as.character(c(second, first)))
})

test_that("an edit from before it had a sentence has empty words where it has none", {
  # the history keeps old edits as they were; a row with no summary reads as empty text
  record <- history_model_record(list(draft_id = 1, kind = "Edit", status = "Applied", edit = list(edit_kind = "Update")))
  expect_equal(record$summary, "")
  expect_null(record$note) # no reason is kept
  expect_equal(record$kind, "Update")
  expect_false(record$approval_required) # nobody approved it, so none was needed
  expect_true(is.na(record$approved_by))
  expect_null(record$applied)

  # an extraction that was pushed is an Add, whether its kind comes back as one row or a list of one
  approval <- list(event = "Approved", occurred_at = "2026-10-02T10:00:00+00:00", person = list(name = "Alex"))
  pushed <- history_model_record(list(draft_id = 2, kind = "Extraction", status = "Applied", edit = list(), reviews = list(approval)))
  expect_equal(pushed$kind, "Add")
  expect_equal(pushed$source, "Extraction")
  expect_equal(pushed$approved_by, "Alex")
  expect_true(pushed$approval_required)
  as_list <- history_model_record(list(draft_id = 3, kind = "Edit", status = "Applied", edit = list(list(edit_kind = "Delete"))))
  expect_equal(as_list$kind, "Delete")
})

test_that("the fields an edit changed come back in the order they were applied", {
  admin <- local_staff("ADMIN")
  type <- local_respondent_type(minimum_age = 18, maximum_age = 99)
  id <- local_applied_edit(
    admin, "respondent_type", "update", type$respondent_type_id,
    respondent_type__name = test_name("Changed"), respondent_type__maximum_age = "64"
  )
  items <- history_model_get_items(test_connection, as.character(id))
  expect_named(items, edit_item_columns)
  expect_equal(items$item_number, seq_len(nrow(items)))
  expect_setequal(items$attribute_name, c("name", "maximum_age"))
  expect_equal(items$old_value[items$attribute_name == "maximum_age"], "99")
  expect_equal(items$new_value[items$attribute_name == "maximum_age"], "64")
  expect_equal(unique(items$record_identifier), sprintf('{"respondent_type_id":%d}', type$respondent_type_id))
  expect_equal(unique(items$action), "Update")
})

test_that("an added row is listed with the number the database gave it", {
  admin <- local_staff("ADMIN")
  name <- test_name("Added")
  id <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = name)
  items <- history_model_get_items(test_connection, as.character(id))
  expect_equal(unique(items$action), "Insert")
  expect_true(all(is.na(items$old_value)))
  expect_equal(items$new_value[items$attribute_name == "name"], name)
  expect_match(items$record_identifier[1], '^\\{"respondent_type_id":[0-9]+\\}$')
})

test_that("an edit with no items is an empty table, and only digits ask for an edit", {
  expect_equal(nrow(history_model_get_items(test_connection, "0")), 0)
  expect_error(history_model_get_items(test_connection, "1 or 1=1"), "no edit with that number")
  expect_error(history_model_get_items(test_connection, NULL), "no edit with that number")
})

test_that("a database that can't be reached gives a sentence", {
  expect_error(history_model_get_all(list(url = "http://127.0.0.1:1", key = "sb_secret_x")), "Couldn't reach the MATR database")
})
