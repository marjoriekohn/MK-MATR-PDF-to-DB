# The Edit History service: which edits to show, who was behind each, and what each changed. What each changed is
# what the database wrote for edits applied as Make an edit applies them.

# Four edits as history_service_get_all() gives them, newest first: one a STAFF member's delete approved by a
# colleague, then an ADMIN's add of a respondent type, an update of it and a delete of it. Only the test of what an
# edit changed needs edits the database applied; the rest only read the records, so they are made up here.
sample_history_record <- function(id, kind, summary, prepared_by, approved_by = NA_character_, deleted = NA_character_) {
  list(
    id = id, kind = kind, source = edit_draft_kind, status = "Applied", summary = summary,
    prepared_by = prepared_by, approved_by = approved_by, approval_required = !is.na(approved_by), deleted = deleted,
    prepared = "2026-10-02T15:00:00Z", approved = if (!is.na(approved_by)) "2026-10-02T15:20:00Z", applied = "2026-10-02T15:30:00Z"
  )
}
h <- list(
  ids = c("104", "103", "102", "101"),
  admin = list(name = "Alice Admin"), me = list(name = "Maggie Staff"), approver = list(name = "Alex Approver")
)
h$records <- stats::setNames(list(
  sample_history_record("104", "Delete", "Deleted the respondent type “Teachers”", h$me$name, h$approver$name, deleted = "Teachers"),
  sample_history_record("103", "Delete", "Deleted the respondent type “Parents renamed”", h$admin$name, deleted = "Parents renamed"),
  sample_history_record("102", "Update", "Changed the name of the respondent type “Parents”", h$admin$name),
  sample_history_record("101", "Add", "Added the respondent type “Parents”", h$admin$name)
), h$ids)

test_that("a kind narrows the edits, and 'all' keeps every kind", {
  expect_equal(names(history_service_filter(h$records, "all")), h$ids)
  expect_equal(names(history_service_filter(h$records, "Update")), h$ids[3])
  expect_equal(names(history_service_filter(h$records, "Add")), h$ids[4])
  expect_equal(names(history_service_filter(h$records, "Delete")), h$ids[1:2])
  expect_equal(names(history_service_filter(h$records, "something else")), h$ids)
})

test_that("every word typed has to be in the sentence or a name, ignoring capitals", {
  kept <- function(...) names(history_service_filter(h$records, ...))
  expect_equal(kept("all", "CHANGED"), h$ids[3]) # the sentence
  expect_equal(kept("all", "renamed"), h$ids[2]) # the name the row had when it was deleted
  expect_equal(kept("all", "alex"), h$ids[1]) # the approver
  expect_equal(kept("all", "alice"), h$ids[2:4]) # who prepared three
  expect_length(history_service_filter(h$records, "all", "no such thing"), 0)
  expect_equal(kept("Update", "  "), h$ids[3])
  expect_equal(kept("all", NULL), h$ids)
})

test_that("a delete says what was deleted, by the name it had", {
  wave <- list(kind = "Delete", deleted = "Kid Questionnaire Wave 1", summary = "Deleted the wave “KQ Wave 1” and 2,120 connected records")
  expect_equal(history_service_changes_text(wave), "Deleted Kid Questionnaire Wave 1")
  # one deleted before its name was kept: its sentence, without the count
  wave$deleted <- NA_character_
  expect_equal(history_service_changes_text(wave), "Deleted the wave “KQ Wave 1”")
  expect_equal(history_service_changes_text(list(kind = "Update"), list(added = 0, changed = 1, removed = 0, tables = 1)), "Changes 1 row, in 1 table.")
})

test_that("who was behind an edit reads as a sentence", {
  expect_equal(history_service_people(h$records[[h$ids[1]]]), paste0("prepared by ", h$me$name, " and approved by ", h$approver$name))
  expect_equal(history_service_people(h$records[[h$ids[3]]]), paste0("made by ", h$admin$name, ", applied at once (no approval needed)"))
  expect_equal(history_service_people(list(prepared_by = NA_character_, approved_by = NA_character_, approval_required = TRUE)), "prepared by someone no longer listed")
  expect_equal(history_service_people(list(prepared_by = "Maggie Kohn", approved_by = NA_character_, approval_required = TRUE)), "prepared by Maggie Kohn")
})

test_that("when it was applied is in words, and an edit not applied says where it is", {
  record <- h$records[[h$ids[3]]]
  withr::local_timezone("UTC")
  applied <- parse_time(record$applied)
  expect_match(history_service_when(record, applied + 60), "^Applied today at [0-9]{1,2}:[0-9]{2} [AP]M$")
  expect_equal(history_service_when(record, applied + 24 * 3600), "Applied yesterday")
  expect_match(history_service_when(record, applied + 30 * 24 * 3600), "^Applied on [A-Z][a-z]{2} [0-9]{1,2}$")
  expect_equal(history_service_when(list(applied = NULL, status = "Approved")), "Approved")
})

test_that("what an edit changed is its rows, grouped, with the counts", {
  # an ADMIN's add of a respondent type, an update of it and a delete of it, applied as Make an edit applies them
  admin <- local_staff("ADMIN")
  name <- test_name("Parents")
  added <- as.character(local_applied_edit(admin, "respondent_type", "add", respondent_type__name = name))
  key <- unname(database_model_get(test_connection, "respondent_type", "respondent_type_id", filters = list(name = database_eq(name)))[[1]])
  updated <- as.character(local_applied_edit(admin, "respondent_type", "update", key, respondent_type__name = paste(name, "renamed")))
  deleted <- as.character(local_applied_edit(admin, "respondent_type", "delete", key))
  records <- history_service_get_all(test_connection)

  changes <- history_service_changes(test_connection, updated)
  expect_length(changes$groups, 1)
  expect_equal(changes$groups[[1]]$table, "respondent_type")
  expect_equal(changes$groups[[1]]$fields$column, "name")
  expect_equal(changes$counts, list(added = 0, changed = 1, removed = 0, tables = 1))
  expect_equal(history_service_changes_text(records[[updated]], changes$counts), "Changes 1 row, in 1 table.")
  expect_match(records[[updated]]$summary, "^Changed the ")

  expect_equal(history_service_changes(test_connection, added)$counts$added, 1)
  # a delete keeps only the row picked, and nothing it held (private.apply_draft_items())
  removed <- history_service_changes(test_connection, deleted)
  expect_length(removed$groups, 1)
  expect_equal(removed$groups[[1]][c("table", "action")], list(table = "respondent_type", action = "Delete"))
  expect_match(removed$groups[[1]]$row, "^respondent_type_id [0-9]+$")
  expect_equal(nrow(removed$groups[[1]]$fields), 0)
  expect_equal(records[[deleted]]$deleted, paste(name, "renamed")) # its name when it was deleted
  expect_equal(history_service_changes_text(records[[deleted]], removed$counts), paste("Deleted", paste(name, "renamed")))
  expect_equal(records[[deleted]]$kind, "Delete")
  expect_false(records[[deleted]]$approval_required)
})
