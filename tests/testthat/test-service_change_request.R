# The rules of Change Requests as the site works them out (the database enforces them again, see
# test-model_change_request.R): what needs approval, what a person can do about a request, and the
# requests a person sees.

staff_member <- list(staff_id = 1L, name = "Maggie Kohn", role = "STAFF")
admin_member <- list(staff_id = 3L, name = "Ada Admin", role = "ADMIN")

test_that("only a STAFF member's delete, or push of an extraction, needs approval", {
  needs <- change_request_service_needs_approval
  expect_false(needs(staff_member, edit_draft_kind, "Add"))
  expect_false(needs(staff_member, edit_draft_kind, "Update"))
  expect_true(needs(staff_member, edit_draft_kind, "Delete"))
  expect_true(needs(staff_member, extraction_kind))
  # an ADMIN never does
  expect_false(needs(admin_member, edit_draft_kind, "Delete"))
  expect_false(needs(admin_member, extraction_kind))
  expect_false(needs(admin_member, edit_draft_kind, "Add"))
})

test_that("a person can approve what someone else prepared and last changed, and withdraw only their own", {
  request <- list(prepared_by_id = 2L, saved_by_id = 2L, requested_reviewer = NULL)
  state <- change_request_service_state(request, staff_member)
  expect_equal(state, list(mine = FALSE, changed_by_me = FALSE, asked_of_me = FALSE, can_approve = TRUE, can_send_back = TRUE, can_withdraw = FALSE))

  mine <- change_request_service_state(list(prepared_by_id = 1L, saved_by_id = 1L), staff_member)
  expect_true(mine$mine)
  expect_false(mine$can_approve)
  expect_false(mine$can_send_back)
  expect_true(mine$can_withdraw)

  # someone else prepared it but this person made the latest changes: it has become theirs to explain
  changed <- change_request_service_state(list(prepared_by_id = 2L, saved_by_id = 1L), staff_member)
  expect_true(changed$changed_by_me)
  expect_false(changed$can_approve)
  expect_true(changed$can_send_back)
  expect_false(changed$can_withdraw)

  asked <- change_request_service_state(list(prepared_by_id = 2L, saved_by_id = 2L, requested_reviewer = list(id = 1L, name = "Maggie Kohn")), staff_member)
  expect_true(asked$asked_of_me)
  expect_true(asked$can_approve)
})

test_that("the requests a person sees: those they can approve (the ones asked of them first), their own, or all", {
  record <- function(id, prepared_by, saved_by = prepared_by, asked = NULL) {
    list(id = id, prepared_by_id = prepared_by, saved_by_id = saved_by, requested_reviewer = if (!is.null(asked)) list(id = asked))
  }
  records <- list(a = record("a", 1L), b = record("b", 2L), c = record("c", 2L, asked = 1L), d = record("d", 2L, saved_by = 1L))
  expect_named(change_request_service_filter(records, staff_member, "me"), c("c", "b")) # not a (mine), not d (I changed it)
  expect_named(change_request_service_filter(records, staff_member, "mine"), "a")
  expect_named(change_request_service_filter(records, staff_member, "everyone"), c("a", "b", "c", "d"))
  expect_named(change_request_service_filter(records, staff_member), c("a", "b", "c", "d"))
  expect_length(change_request_service_filter(list(), staff_member, "me"), 0)
})

# test_that("the kind of request is as staff chose it, and what an edit changes is its rows", {
#   me <- local_staff()
#   request <- local_edit(me, status = "Awaiting Review")
#   expect_equal(change_request_service_kind(request), "Delete")
#   expect_equal(change_request_service_kind(list(kind = "Edit", edit_kind = "Add")), "Add")
#   expect_equal(change_request_service_kind(list(kind = "Edit")), "Edit")
#   expect_equal(change_request_service_kind(list(kind = extraction_kind)), "Extraction")
#   changes <- change_request_service_changes(request)
#   expect_length(changes$groups, 1)
#   expect_equal(changes$groups[[1]]$table, "respondent_type")
#   expect_equal(changes$counts$removed, 1)
#   expect_equal(change_request_service_changes(list(items = NULL))$groups, list())
# })
#
# test_that("the waiting requests come longest waiting first, edits and extractions together", {
#   me <- local_staff()
#   later <- local_edit(me, status = "Awaiting Review", now = as.POSIXct("2026-10-03 10:00:00", tz = "UTC"))
#   earlier <- local_edit(me, status = "Awaiting Review", now = as.POSIXct("2026-10-02 10:00:00", tz = "UTC"))
#   waiting <- change_request_service_waiting(test_connection)
#   ours <- names(waiting)[names(waiting) %in% c(earlier$id, later$id)]
#   expect_equal(ours, c(earlier$id, later$id))
# })
#
# test_that("approving applies the request as the person read it, and a changed one is refused with a sentence", {
#   me <- local_staff()
#   approver <- local_staff()
#   type <- local_respondent_type()
#   request <- local_edit(me, type, status = "Awaiting Review")
#
#   # the request changed after the approver opened it: what they read isn't what would be applied
#   newer <- edit_model_save(test_connection, utils::modifyList(request, list(note = "new reason", expected_revision = request$revision)), me$staff_id)
#   expect_error(change_request_service_approve(test_connection, request, approver), "changed after you opened it")
#   history_id <- change_request_service_approve(test_connection, newer, approver)
#   local_applied(history_id)
#   expect_equal(history_model_get_all(test_connection)[[history_id]]$approved_by, approver$name)
#   expect_equal(nrow(table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(type$respondent_type_id)))), 0)
# })
#
# test_that("a request is sent back with a note that can't be blank, and withdrawn only by the person who prepared it", {
#   me <- local_staff()
#   reviewer <- local_staff()
#   request <- local_edit(me, status = "Awaiting Review")
#   expect_error(change_request_service_send_back(test_connection, request, reviewer, "   "), "Say what needs changing")
#   expect_error(change_request_service_send_back(test_connection, request, reviewer, NULL), "Say what needs changing")
#   change_request_service_send_back(test_connection, request, reviewer, "  Check the ages  ")
#   expect_equal(edit_model_get(test_connection, request$id)$sent_back$note, "Check the ages")
#
#   other <- local_edit(me, status = "Awaiting Review")
#   expect_error(change_request_service_withdraw(test_connection, other, reviewer), "Only the person who prepared this can withdraw it")
#   change_request_service_withdraw(test_connection, other, me)
#   expect_equal(edit_model_get(test_connection, other$id)$status, "Withdrawn")
# })
#
# test_that("a database that can't be reached gives a signature of its own, so polling carries on", {
#   first <- change_request_service_signature(list(url = "http://127.0.0.1:1", key = "sb_secret_x"))
#   expect_match(first, "^unreachable")
#   expect_false(grepl("^unreachable", change_request_service_signature(test_connection)))
# })
