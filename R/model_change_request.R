# ------ Change Requests ------------------------------------------------------------
# A change request is a draft (model_draft_row.R) that has been sent for review: an edit that
# needs approval (a STAFF member's delete) or an extraction a STAFF member submitted. It waits
# with the status "Awaiting Review" until one staff member other than the person who prepared
# or last changed it approves it, which applies it to the MATR metadata database, or sends it back to the
# preparer, or until the preparer withdraws it.
#
# Approving, applying and sending back are functions of the database, called through /rpc/,
# because each changes several tables at once (an approval or a send back is a
# DRAFT_REVIEW_EVENT row) and separate requests can't share a transaction
# (docs/migrations/20261020_split_draft_details.sql). The functions check the rules
# themselves; the site checks them only to know which buttons to offer.

change_request_waiting_status <- "Awaiting Review"

#' The change requests waiting for review.
#'
#' Edits and extractions together.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of records (see `edit_model_get_all()` and `extraction_model_get_all()`; the
#'   record's `kind` says which), named by id.
change_request_model_waiting <- function(connection) {
  c(
    edit_model_records(connection, change_request_waiting_status),
    extraction_model_records(connection, change_request_waiting_status)
  )
}

#' Approve a change request, which applies it.
#'
#' The database checks that the request is waiting for approval, that it is still the revision
#' the person read, and that the person is active and is neither who prepared it nor who last
#' changed it. The approval and the change go in together, or neither does.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The request's id; digits only.
#' @param revision The revision the person was reading.
#' @param staff_id Who approves.
#' @param items For an extraction, the rows to add (see `extraction_service_push_items()`).
#' @return The id of the new row in the history, as text. An error with the database's own
#'   sentence if it refused, e.g. because a row changed since the request was prepared.
change_request_model_approve <- function(connection, id, revision, staff_id, items = NULL) {
  arguments <- list(
    p_draft_id = draft_row_number(id), p_revision = as.integer(revision), p_staff_id = as.numeric(staff_id)
  )
  if (!is.null(items)) arguments$p_items <- items
  as.character(database_model_call(connection, "approve_draft", arguments))
}

#' Apply a change that needs no approval.
#'
#' Only the person who prepared it can, and only an add or an update, or anything an ADMIN
#' prepared; the database refuses the rest.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id; digits only.
#' @param staff_id Who applies it.
#' @param items For an extraction, the rows to add.
#' @return The id of the new row in the history, as text. An error with the database's own
#'   sentence if it refused.
change_request_model_apply <- function(connection, id, staff_id, items = NULL) {
  arguments <- list(p_draft_id = draft_row_number(id), p_staff_id = as.numeric(staff_id))
  if (!is.null(items)) arguments$p_items <- items
  as.character(database_model_call(connection, "apply_draft", arguments))
}

#' Send a change request back to the person who prepared it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The request's id.
#' @param revision The revision the person was reading.
#' @param staff_id Who sends it back.
#' @param note What needs changing.
#' @return Called for its effect. The database records who sent it back, when, and the note. An
#'   error with the database's sentence if it refused, e.g. because the request was changed after
#'   the person opened it.
change_request_model_send_back <- function(connection, id, revision, staff_id, note) {
  database_model_call(connection, "send_back_draft", list(
    p_draft_id = draft_row_number(id), p_revision = as.integer(revision),
    p_staff_id = as.numeric(staff_id), p_note = note
  ))
  invisible()
}

#' Withdraw a draft or a change request.
#'
#' The record is kept, as Withdrawn.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param staff_id Who withdraws it.
#' @return Called for its effect.
change_request_model_withdraw <- function(connection, id, staff_id) {
  draft_row_model_save(connection, id, staff_id, parent = list(status = "Withdrawn"), new_revision = FALSE)
  invisible()
}
