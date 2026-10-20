# The proposals staff make on Make an edit, kept in the database as a draft of kind Edit with its details.

# A wave with two values a rename can change, and the proposal that renames it (as `staff` made it).
# wave_rename <- function(staff, status = "Draft", env = parent.frame()) {
#   tree <- local_tree(env = env)
#   wave <- local_row("wave", list(questionnaire_id = tree$questionnaire$questionnaire_id, name = test_name("Wave"), respondent_count = 1210), "wave_id", env)
#   entity <- edit_entities$wave
#   snap <- edit_service_load(test_connection, entity)
#   key <- as.character(wave$wave_id)
#   form <- form_for("wave", "update", key, snap = snap, wave__name = "Round 2", wave__respondent_count = "")
#   form_record <- list(action = "update", entity = "wave", scope = "", target = key, values = list(wave__name = "Round 2", wave__respondent_count = ""))
#   list(
#     wave = wave,
#     record = edit_service_record(staff, form_record, form_items(form), status, now = as.POSIXct("2026-10-02 15:30:12", tz = "UTC"))
#   )
# }
#
# test_that("a proposal is saved, numbered by the database, and read back as it was", {
#   me <- local_staff()
#   made <- wave_rename(me)
#   expect_null(made$record$id)
#   saved <- edit_model_save(test_connection, made$record, me$staff_id)
#   withr::defer(test_delete("draft", list(draft_id = database_eq(saved$id))))
#
#   expect_match(saved$id, "^[0-9]+$")
#   expect_equal(saved$prepared_by, me$name)
#   expect_equal(saved$prepared_by_id, me$staff_id)
#   expect_equal(saved$saved_by_id, me$staff_id)
#   expect_equal(saved$kind, "Edit")
#   expect_equal(saved$edit_kind, "Update")
#   expect_equal(saved$status, "Draft")
#   expect_equal(saved$revision, 1)
#   back <- edit_model_get(test_connection, saved$id)
#   expect_equal(back, saved)
#   expect_equal(back$summary, made$record$summary)
#   expect_equal(back$started, "2026-10-02T15:30:12Z") # the time it was started, as given
#   expect_null(back$submitted)
#   expect_equal(back$form$values$wave__name, "Round 2")
#   expect_equal(back$form$values$wave__respondent_count, "")
#   expect_named(back$items, edit_item_columns)
#   expect_equal(back$items$attribute_name, c("name", "respondent_count"))
#   expect_equal(back$items$old_value[2], "1210")
#   expect_equal(back$items$new_value[1], "Round 2")
#   expect_equal(back$items$new_value[2], NA_character_)
#   expect_equal(back$items$record_identifier, rep(sprintf('{"wave_id":%d}', made$wave$wave_id), 2))
#   expect_equal(back$items$item_number, 1:2)
# })
#
# test_that("saving again changes the proposal, raises its revision, and doesn't add another", {
#   me <- local_staff()
#   made <- wave_rename(me)
#   saved <- edit_model_save(test_connection, made$record, me$staff_id)
#   withr::defer(test_delete("draft", list(draft_id = database_eq(saved$id))))
#
#   sent <- utils::modifyList(saved, list(status = "Awaiting Review", submitted = "2026-10-02T16:00:00Z", expected_revision = saved$revision))
#   again <- edit_model_save(test_connection, sent, me$staff_id)
#   expect_equal(again$id, saved$id)
#   expect_equal(again$status, "Awaiting Review")
#   expect_equal(again$submitted, "2026-10-02T16:00:00Z")
#   expect_equal(again$revision, 2)
#   expect_equal(length(Filter(function(record) identical(record$id, saved$id), edit_model_get_all(test_connection))), 1)
#
#   # someone opened version 1 and saves it after version 2
#   stale <- utils::modifyList(saved, list(expected_revision = 1))
#   expect_error(edit_model_save(test_connection, stale, me$staff_id), "changed after you opened it")
#   # a change that isn't to the content keeps the revision
#   expect_equal(edit_model_save(test_connection, again, me$staff_id, new_revision = FALSE)$revision, 2)
# })
#
# test_that("the person who last saved a proposal is not who prepared it, when someone else saves", {
#   me <- local_staff()
#   colleague <- local_staff()
#   saved <- local_edit(me)
#   edited <- edit_model_save(test_connection, utils::modifyList(saved, list(summary = "A better summary")), colleague$staff_id)
#   expect_equal(edited$prepared_by_id, me$staff_id)
#   expect_equal(edited$saved_by_id, colleague$staff_id)
#   expect_equal(edited$saved_by, colleague$name)
#   expect_equal(edited$summary, "A better summary")
# })
#
# test_that("a proposal that isn't in the database any more says so", {
#   me <- local_staff()
#   made <- wave_rename(me)
#   gone <- made$record
#   gone$id <- "0"
#   expect_error(edit_model_save(test_connection, gone, me$staff_id), "no longer in the database")
#   expect_error(edit_model_get(test_connection, "0"), "no longer in the database")
#   expect_error(edit_model_get(test_connection, "1 or 1=1"), "no draft with that number")
# })
#
# test_that("every proposal is read, newest first, and extractions are not among them", {
#   me <- local_staff()
#   first <- local_edit(me)
#   second <- local_edit(me)
#   extraction <- local_extraction(me)
#   records <- edit_service_get_all(test_connection)
#   ours <- names(records)[names(records) %in% c(first$id, second$id)]
#   expect_equal(ours, c(second$id, first$id))
#   expect_false(extraction$id %in% names(records))
#   expect_true(all(vapply(records, function(record) record$kind, character(1)) == "Edit"))
# })

test_that("only open proposals are listed, and only the preparer's if asked", {
  records <- list(
    a = list(status = "Draft", prepared_by_id = 1L), b = list(status = "Awaiting Review", prepared_by_id = 2L),
    c = list(status = "Withdrawn", prepared_by_id = 1L), d = list(status = "Applied", prepared_by_id = 1L),
    e = list(status = "Sent Back", prepared_by_id = 1L)
  )
  expect_named(edit_service_open(records), c("a", "b", "e"))
  expect_named(edit_service_open(records, prepared_by_id = 1L), c("a", "e"))
  expect_named(edit_service_open(records, status = "Draft"), "a")
})

# test_that("discarding a draft keeps it, as Withdrawn", {
#   me <- local_staff()
#   saved <- local_edit(me)
#   edit_service_withdraw(test_connection, saved$id, me)
#   back <- edit_model_get(test_connection, saved$id)
#   expect_equal(back$status, "Withdrawn")
#   expect_equal(back$revision, saved$revision) # withdrawing isn't a change to the content
#   expect_equal(nrow(back$items), nrow(saved$items))
# })
#
# test_that("a delete's reach is saved with the proposal", {
#   me <- local_staff()
#   tree <- local_tree()
#   entity <- edit_entities$questionnaire
#   snap <- edit_service_load(test_connection, entity)
#   key <- as.character(tree$questionnaire$questionnaire_id)
#   built <- edit_service_build(entity, "delete", snap, edit_service_row(entity, snap, key))
#   record <- edit_service_record(me, list(action = "delete", entity = "questionnaire", scope = "", target = key, values = list()), built, "Draft")
#   expect_equal(record$reach$rows_removed, built$plan$reach$total)
#   saved <- edit_model_save(test_connection, record, me$staff_id)
#   withr::defer(test_delete("draft", list(draft_id = database_eq(saved$id))))
#   expect_equal(edit_model_get(test_connection, saved$id)$reach$rows_removed, built$plan$reach$total)
#   expect_equal(saved$edit_kind, "Delete")
# })
#
# test_that("an unfinished form is saved as typed, without items", {
#   me <- local_staff()
#   built <- list(summary = "Unfinished: update a wave", items = NULL)
#   record <- edit_service_record(me, list(action = "update", entity = "wave", values = list(wave__name = "R")), built, "Draft")
#   saved <- edit_model_save(test_connection, record, me$staff_id)
#   withr::defer(test_delete("draft", list(draft_id = database_eq(saved$id))))
#   back <- edit_model_get(test_connection, saved$id)
#   expect_equal(NROW(back$items), 0)
#   expect_equal(back$form$values$wave__name, "R")
# })
#
test_that("the kind of an edit is what the form was made from", {
  expect_equal(edit_model_kind(list(action = "add")), "Add")
  expect_equal(edit_model_kind(list(action = "update")), "Update")
  expect_equal(edit_model_kind(list(action = "delete")), "Delete")
  expect_equal(edit_service_kind_of("delete"), "Delete")
})
#
# test_that("a request for a reviewer is kept with the proposal", {
#   me <- local_staff()
#   colleague <- local_staff()
#   record <- sample_edit(me, local_respondent_type(), status = "Awaiting Review")
#   record$requested_reviewer <- list(id = colleague$staff_id)
#   record$submitted <- "2026-10-02T16:00:00Z"
#   saved <- edit_model_save(test_connection, record, me$staff_id)
#   withr::defer(test_delete("draft", list(draft_id = database_eq(saved$id))))
#   expect_equal(saved$requested_reviewer, list(id = colleague$staff_id, name = colleague$name))
# })
#
