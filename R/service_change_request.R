# ------ Change Requests ---------------------------------------------------------------
# The rules (docs/PROJECT_STATUS.md, "New Decisions", decided 2026-10-07):
#
#   Add or update something (Make an edit, or a new item made from a drop-down)   applied at once
#   Delete anything                              STAFF: a change request.  ADMIN: applied at once.
#   Push an extraction to the database           STAFF: a change request.  ADMIN: applied at once.
#
# A change request needs one approval, from an active staff member other than the person who
# prepared it and the person who last changed it (an approver who edited a request has made it
# their own). Approving applies it, in one step. The preparer can ask a particular person to
# review it; any other eligible staff member can still approve it. A reviewer can send it back
# to the preparer with a note, and the preparer can withdraw it.
#
# The database enforces all of this (model_change_request.R); this works out what staff can do
# next, so the pages offer the right buttons.
#
# A "request" is a record of a draft that was sent for review: an edit (see edit_model_get_all())
# or an extraction (see extraction_model_get_all()); its `kind` says which.

#' Whether a change needs approval before it is applied.
#'
#' Only a STAFF member's delete, or a STAFF member's push of an extraction. An add or an
#' update never does, and neither does anything an ADMIN does.
#'
#' @param me Who is making the change: `list(staff_id, name, role)`.
#' @param kind `"Edit"` or `"Extraction"`.
#' @param edit_kind For an edit, `"Add"`, `"Update"` or `"Delete"`.
#' @return `TRUE` or `FALSE`.
change_request_service_needs_approval <- function(me, kind, edit_kind = NULL) {
  !identical(me$role, "ADMIN") && (identical(kind, extraction_kind) || identical(edit_kind, "Delete"))
}

#' The requests waiting for review, longest waiting first.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of requests named by id; empty if nothing is waiting.
change_request_service_waiting <- function(connection) {
  records <- change_request_model_waiting(connection)
  waited <- vapply(records, function(record) as.numeric(parse_time(record$submitted %||% record$saved)), numeric(1))
  records[order(waited)]
}

#' A summary that changes whenever something waiting changes.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A string; compare it with an earlier one. A database that can't be reached gives one
#'   of its own, so the page keeps polling and notices when it is back.
change_request_service_signature <- function(connection) {
  tryCatch(change_request_model_signature(connection), error = function(problem) paste("unreachable", Sys.time()))
}

#' Where a waiting request stands for one person.
#'
#' @param record A request.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @return A list: `mine` (this person prepared it), `changed_by_me` (they made its latest
#'   revision, though someone else prepared it), `asked_of_me` (the preparer asked them to
#'   review it), `can_approve`, `can_send_back` and `can_withdraw`.
change_request_service_state <- function(record, me) {
  mine <- draft_row_service_is_mine(record, me)
  changed_by_me <- !mine && same_staff(record$saved_by_id, me$staff_id)
  list(
    mine = mine,
    changed_by_me = changed_by_me,
    asked_of_me = same_staff(record$requested_reviewer$id, me$staff_id),
    can_approve = !mine && !changed_by_me,
    can_send_back = !mine,
    can_withdraw = mine
  )
}

#' The waiting requests a person has something to do about.
#'
#' @param records From `change_request_service_waiting()`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param whose `"me"` keeps those this person can approve, with the ones they were asked to
#'   review first; `"mine"` keeps those they prepared; anything else keeps them all.
#' @return The matching requests.
change_request_service_filter <- function(records, me, whose = "everyone") {
  states <- lapply(records, change_request_service_state, me = me)
  if (identical(whose, "me")) {
    keep <- vapply(states, function(state) state$can_approve, logical(1))
    asked <- vapply(states[keep], function(state) state$asked_of_me, logical(1))
    return(records[keep][order(!asked)])
  }
  if (identical(whose, "mine")) {
    return(records[vapply(states, function(state) state$mine, logical(1))])
  }
  records
}

#' The kind of request, in a word.
#'
#' @param record A request.
#' @return `"Add"`, `"Update"` or `"Delete"` for an edit; `"Extraction"` for an extraction.
change_request_service_kind <- function(record) {
  if (identical(record$kind, extraction_kind)) {
    return("Extraction")
  }
  record$edit_kind %||% "Edit"
}

#' The rows an edit would touch, and how many of each kind.
#'
#' @param record An edit request.
#' @return A list: `groups` (see `change_service_groups()`) and `counts` (see `change_service_counts()`).
change_request_service_changes <- function(record) {
  groups <- change_service_groups(record$items)
  list(groups = groups, counts = change_service_counts(groups))
}

#' Approve a request, which applies it.
#'
#' An extraction's rows are worked out now, from the questionnaire as it is saved and the MATR
#' data as it is now, so a reviewer approves what they read and what would go in today.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The request as the person read it: its `id` and `revision` say what they approve.
#' @param me Who is approving: `list(staff_id, name, role)`.
#' @return The id of its row in the history. An error with a sentence if the database refused,
#'   e.g. because the request was changed after the person opened it.
change_request_service_approve <- function(connection, record, me) {
  items <- if (identical(record$kind, extraction_kind)) extraction_service_push_items(connection, record)
  change_request_model_approve(connection, record$id, record$revision, me$staff_id, items)
}

#' Send a request back to the person who prepared it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The request as the person read it.
#' @param me Who is sending it back: `list(staff_id, name, role)`.
#' @param note What needs changing; it can't be blank.
#' @return Called for its effect.
change_request_service_send_back <- function(connection, record, me, note) {
  note <- trimws(note %||% "")
  if (!nzchar(note)) stop("Say what needs changing, so they know what to fix.", call. = FALSE)
  change_request_model_send_back(connection, record$id, record$revision, me$staff_id, note)
}

#' Withdraw a request, or a draft, the person prepared.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The request or draft.
#' @param me Who withdraws it: `list(staff_id, name, role)`; it has to be the person who prepared it.
#' @return Called for its effect.
change_request_service_withdraw <- function(connection, record, me) {
  if (!draft_row_service_is_mine(record, me)) {
    stop("Only the person who prepared this can withdraw it.", call. = FALSE)
  }
  change_request_model_withdraw(connection, record$id, me$staff_id)
}
