# # What the database does with a change request, called through /rpc/: approve (and so apply), apply at once,
# # send back and withdraw. The database enforces the rules whoever asks, so these are checked against it.
# # supabase/tests/drafts_and_apply.sql checks the same rules, and what applying does to every table, in SQL.
#
# test_that("a request waits for review with its status, who prepared it and when it was sent", {
#   me <- local_staff()
#   request <- local_edit(me, status = "Awaiting Review")
#   waiting <- change_request_model_waiting(test_connection)
#   expect_true(request$id %in% names(waiting))
#   expect_equal(waiting[[request$id]]$status, "Awaiting Review")
#   expect_equal(waiting[[request$id]]$prepared_by_id, me$staff_id)
#   expect_equal(waiting[[request$id]]$kind, "Edit")
#   expect_match(waiting[[request$id]]$submitted, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
#
#   plain <- local_edit(me, status = "Draft") # not sent for review: not waiting
#   expect_false(plain$id %in% names(change_request_model_waiting(test_connection)))
# })
#
# test_that("edits and extractions wait together, and a withdrawn one stops waiting", {
#   me <- local_staff()
#   request <- local_edit(me, status = "Awaiting Review")
#   extraction <- local_read_extraction(me)
#   record <- extraction_model_get(test_connection, extraction$id)
#   extraction_model_save(test_connection, utils::modifyList(record, list(status = "Awaiting Review", submitted = format_time(Sys.time()))), c("status", "submitted"), me$staff_id)
#   waiting <- change_request_model_waiting(test_connection)
#   expect_true(all(c(request$id, extraction$id) %in% names(waiting)))
#   expect_equal(waiting[[extraction$id]]$kind, "Extraction")
#
#   change_request_model_withdraw(test_connection, request$id, me$staff_id)
#   expect_false(request$id %in% names(change_request_model_waiting(test_connection)))
# })
#
# test_that("one approval from someone else applies a change, in one step", {
#   me <- local_staff()
#   approver <- local_staff()
#   type <- local_respondent_type()
#   request <- local_edit(me, type, status = "Awaiting Review")
#   history_id <- change_request_model_approve(test_connection, request$id, request$revision, approver$staff_id)
#   local_applied(history_id)
#
#   expect_match(history_id, "^[0-9]+$")
#   expect_equal(nrow(table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(type$respondent_type_id)))), 0)
#   expect_equal(edit_model_get(test_connection, request$id)$status, "Applied")
#   entry <- history_model_get_all(test_connection)[[history_id]]
#   expect_equal(entry$approved_by, approver$name)
#   expect_equal(entry$prepared_by, me$name)
#   expect_true(entry$approval_required)
#   # the approval is recorded on the draft itself, together with when it was applied
#   applied <- database_model_select(
#     test_connection, "draft", c("draft_id", "approved_by", "approved_at", "applied_at"),
#     filters = list(draft_id = database_eq(request$id))
#   )[[1]]
#   expect_equal(applied$approved_by, approver$staff_id)
#   expect_false(is.null(applied$approved_at))
#   expect_false(is.null(applied$applied_at))
#   expect_equal(history_id, request$id) # an applied draft is its own record
# })
#
# test_that("nobody approves their own request, or one whose latest change is theirs", {
#   me <- local_staff()
#   colleague <- local_staff()
#   request <- local_edit(me, status = "Awaiting Review")
#   expect_error(change_request_model_approve(test_connection, request$id, request$revision, me$staff_id), "You prepared this, so someone else has to approve it")
#
#   # a colleague changes it: the latest changes are theirs now, so they can't approve it either
#   changed <- edit_model_save(test_connection, utils::modifyList(request, list(summary = "A better summary", expected_revision = request$revision)), colleague$staff_id)
#   expect_error(
#     change_request_model_approve(test_connection, changed$id, changed$revision, colleague$staff_id),
#     "You made the latest changes to this, so someone else has to approve it"
#   )
#   # the person who prepared it still can't
#   expect_error(change_request_model_approve(test_connection, changed$id, changed$revision, me$staff_id), "You prepared this")
#   expect_equal(edit_model_get(test_connection, request$id)$status, "Awaiting Review")
# })
#
# test_that("only active staff approve, and only what is waiting, and only the version they read", {
#   me <- local_staff()
#   approver <- local_staff()
#   former <- local_staff(active = FALSE)
#   request <- local_edit(me, status = "Awaiting Review")
#   expect_error(change_request_model_approve(test_connection, request$id, request$revision, former$staff_id), "Only active staff can approve a change")
#   expect_error(change_request_model_approve(test_connection, request$id, request$revision + 1, approver$staff_id), "changed after you opened it. Read it again before approving")
#   expect_error(change_request_model_approve(test_connection, "0", 1, approver$staff_id), "no longer in the database")
#
#   plain <- local_edit(me, status = "Draft")
#   expect_error(change_request_model_approve(test_connection, plain$id, plain$revision, approver$staff_id), "not waiting for approval \\(it is draft\\)")
#   expect_equal(edit_model_get(test_connection, plain$id)$status, "Draft")
# })
#
# test_that("an ADMIN applies their own delete at once, and a STAFF member's needs approval", {
#   admin <- local_staff("ADMIN")
#   me <- local_staff()
#   type <- local_respondent_type()
#   mine <- local_edit(admin, type)
#   history_id <- change_request_model_apply(test_connection, mine$id, admin$staff_id)
#   local_applied(history_id)
#   entry <- history_model_get_all(test_connection)[[history_id]]
#   expect_false(entry$approval_required)
#   expect_true(is.na(entry$approved_by))
#
#   other <- local_edit(me, local_respondent_type())
#   expect_error(change_request_model_apply(test_connection, other$id, me$staff_id), "needs approval from another staff member before it is applied")
#   expect_error(change_request_model_apply(test_connection, other$id, admin$staff_id), "Only the person who prepared a change can apply it without approval")
# })
#
# test_that("a request is sent back with a note, without a new revision, and only while it waits", {
#   me <- local_staff()
#   reviewer <- local_staff()
#   request <- local_edit(me, status = "Awaiting Review")
#   change_request_model_send_back(test_connection, request$id, request$revision, reviewer$staff_id, "Check the ages first", now = as.POSIXct("2026-10-05 09:00:00", tz = "UTC"))
#   back <- edit_model_get(test_connection, request$id)
#   expect_equal(back$status, "Sent Back")
#   expect_equal(back$revision, request$revision)
#   expect_equal(back$sent_back, list(by = reviewer$name, by_id = reviewer$staff_id, date = "2026-10-05T09:00:00Z", note = "Check the ages first"))
#   expect_false(request$id %in% names(change_request_model_waiting(test_connection)))
#
#   plain <- local_edit(me, status = "Draft")
#   expect_error(change_request_model_send_back(test_connection, plain$id, plain$revision, reviewer$staff_id, "no"), "Only a change waiting for review can be sent back")
#   stale <- local_edit(me, status = "Awaiting Review")
#   expect_error(change_request_model_send_back(test_connection, stale$id, stale$revision + 1, reviewer$staff_id, "no"), "changed after you opened it")
# })
#
# test_that("a request is withdrawn and kept, and can't be approved afterwards", {
#   me <- local_staff()
#   approver <- local_staff()
#   request <- local_edit(me, status = "Awaiting Review")
#   change_request_model_withdraw(test_connection, request$id, me$staff_id)
#   expect_equal(edit_model_get(test_connection, request$id)$status, "Withdrawn")
#   expect_error(change_request_model_approve(test_connection, request$id, request$revision, approver$staff_id), "not waiting for approval \\(it is withdrawn\\)")
# })
#
# test_that("something already applied can't be applied or changed again", {
#   me <- local_staff()
#   admin <- local_staff("ADMIN")
#   request <- local_edit(admin)
#   history_id <- change_request_model_apply(test_connection, request$id, admin$staff_id)
#   local_applied(history_id)
#   expect_error(change_request_model_apply(test_connection, request$id, admin$staff_id), "already been applied")
#   expect_error(draft_row_model_save(test_connection, request$id, me$staff_id, list(summary = "x")), "already been applied, so it can't be changed")
#   expect_error(change_request_model_withdraw(test_connection, request$id, admin$staff_id), "already been applied")
# })
