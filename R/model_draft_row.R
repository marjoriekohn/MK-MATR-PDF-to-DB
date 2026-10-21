# ------ Drafts: the shared rows and their details --------------------------------------
# What staff are still working on before it reaches the live tables is a DRAFT: one row
# of the columns both kinds share,
#   status, prepared_by, saved_by, requested_reviewer (STAFF rows), revision, summary,
#   created_at, saved_at, editing_by (a STAFF row), editing_at
# and one row of a table for its kind:
#   EXTRACTED_DRAFT     an extraction of a questionnaire (kind "Extraction"); an AI-Powered one
#                       also has an AI_EXTRACTED_DRAFT row, and its PDF is a PDF_FILE row
#   EDIT_DRAFT          an edit made on Make an edit (kind "Edit")
# Each time it is submitted for review, sent back or approved is a DRAFT_REVIEW_EVENT row: who,
# which revision, when, and a send back's note. A draft is read with its events.
# extraction_model_* and edit_model_* turn the rows into the records the services work
# with. Anything that changes several tables at once is a function of the database called
# through /rpc/ (create_draft, save_draft, send_back_draft, apply_draft, approve_draft;
# docs/migrations/20261020_split_draft_details.sql), because separate requests can't
# share a transaction. Changes to one table (the reading job's progress) are plain requests.
# Who is working on a draft (editing_by, since editing_at) is claimed and let go through
# claim_draft and release_draft (docs/migrations/20261020_draft_editing_claim.sql), so two
# people asking at once can't both get it.

draft_table <- "draft"
extracted_draft_table <- "extracted_draft"
ai_extracted_draft_table <- "ai_extracted_draft"
edit_draft_table <- "edit_draft"

# The shared columns, the names of the people they point at, and the draft's reviews.
draft_columns <- c(
  "draft_id", "kind", "status", "prepared_by", "saved_by", "requested_reviewer", "revision", "summary",
  "created_at", "saved_at", "editing_by", "editing_at",
  "preparer:staff!prepared_by(name)", "saver:staff!saved_by(name)", "editor:staff!editing_by(name)",
  "reviewer:staff!requested_reviewer(name)",
  "reviews:draft_review_event(event,staff_id,revision,note,occurred_at,person:staff(name))"
)

# The statuses of a draft that is finished with: applied to the MATR metadata database, or set aside. A list
# of drafts leaves them out: every page that lists drafts shows only those still being prepared or waiting,
# and the finished ones only grow in number (the database keeps every applied draft as its record).
draft_closed_statuses <- c("Applied", "Withdrawn")

# The drafts of one kind still being prepared or waiting, newest saved first; only those with this status,
# if given. (One finished with is read on its own: draft_row_model_get().)
draft_row_model_list <- function(connection, kind, status = NULL) {
  database_model_select(
    connection, draft_table, draft_columns,
    order_by = c("saved_at.desc", "draft_id.desc"),
    filters = c(
      list(kind = database_eq(kind)),
      list(status = if (!is.null(status)) database_eq(status) else paste0("not.", database_in(draft_closed_statuses)))
    )
  )
}

#' One draft's shared row.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id: digits only.
#' @return The row as a list, or `NULL` if there is no such draft.
draft_row_model_get <- function(connection, id) {
  rows <- database_model_select(connection, draft_table, draft_columns, filters = draft_row_filter(id))
  if (length(rows) > 0) rows[[1]]
}

#' The detail rows of some drafts.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param table `extracted_draft_table` or `edit_draft_table`.
#' @param ids The drafts' ids; digits only. None reads none.
#' @param columns The columns to read; `draft_id` is always among them.
#' @return A list of rows (lists), named by draft id.
draft_row_model_details <- function(connection, table, ids, columns) {
  if (length(ids) == 0) {
    return(list())
  }
  rows <- database_model_select(
    connection, table, union("draft_id", columns), order_by = "draft_id",
    filters = list(draft_id = database_in(ids))
  )
  stats::setNames(rows, vapply(rows, function(row) as.character(row$draft_id), character(1)))
}

#' Add a draft and its details.
#'
#' One database function, so a draft never exists without its details.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param kind `"Extraction"` or `"Edit"`.
#' @param prepared_by The staff id of the person starting it.
#' @param parent The shared columns to set, as a named list (`status`, `summary`,
#'   `requested_reviewer`, `created_at`).
#' @param detail The kind's own columns to set, as a named list.
#' @return The new draft's id, as text.
draft_row_model_create <- function(connection, kind, prepared_by, parent = list(), detail = list()) {
  as.character(database_model_call(connection, "create_draft", list(
    p_kind = kind, p_prepared_by = as.numeric(prepared_by),
    p_parent = draft_row_object(parent), p_detail = draft_row_object(detail)
  )))
}

#' Save a change to a draft and its details together.
#'
#' Only the columns named are written. The database raises the revision unless told not to,
#' and records who saved it, and who submitted it if the status becomes Awaiting Review. For an
#' extraction, `detail` may name columns of extracted_draft, ai_extracted_draft and pdf_file
#' (`pdf_pages`, `pdf_bytes`): the database puts each in its table.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param staff_id The staff id of the person saving.
#' @param parent,detail Named lists of columns to their new values; a `NULL` value empties the column.
#' @param expected_revision The revision the person was looking at; the save is refused if
#'   the draft has moved on. `NULL` skips the check.
#' @param new_revision `FALSE` for a change that isn't to the content (withdrawing).
#' @return The revision after the save. An error with the database's sentence if it refused.
draft_row_model_save <- function(connection, id, staff_id, parent = list(), detail = list(),
                                 expected_revision = NULL, new_revision = TRUE) {
  arguments <- list(
    p_draft_id = draft_row_number(id), p_staff_id = as.numeric(staff_id),
    p_parent = draft_row_object(parent), p_detail = draft_row_object(detail),
    p_new_revision = new_revision
  )
  if (!is.null(expected_revision)) arguments$p_expected_revision <- as.integer(expected_revision)
  as.integer(database_model_call(connection, "save_draft", arguments))
}

#' Claim a draft for one person to work on, or renew their claim.
#'
#' The database gives it to them if nobody has it, they have it already, or whoever has it hasn't
#' renewed their claim for `stale_seconds` (their page is gone). `take_over` takes it whoever has it.
#' Nothing here raises the revision.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param staff_id The staff id of the person claiming it.
#' @param take_over Whether to take it from whoever has it.
#' @param stale_seconds How long a claim lasts without being renewed.
#' @return A list: `claimed` (whether this person has it now), and who does have it: `by_id`, `by`
#'   (their name) and `at` (when they last renewed it); and the draft's `revision`. An error with the
#'   database's sentence if it refused (no such draft, or one finished with).
draft_row_model_claim <- function(connection, id, staff_id, take_over = FALSE, stale_seconds = 180) {
  claim <- database_model_call(connection, "claim_draft", list(
    p_draft_id = draft_row_number(id), p_staff_id = as.numeric(staff_id),
    p_take_over = isTRUE(take_over), p_stale_seconds = as.integer(stale_seconds)
  ))
  list(
    claimed = isTRUE(claim$claimed), by_id = claim$editing_by, by = claim$editing_name,
    at = database_time(claim$editing_at), revision = claim$revision
  )
}

#' Let go of a person's claim on a draft, if they still have it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param staff_id The staff id of the person letting it go.
#' @return Whether they had it.
draft_row_model_release <- function(connection, id, staff_id) {
  isTRUE(database_model_call(connection, "release_draft", list(
    p_draft_id = draft_row_number(id), p_staff_id = as.numeric(staff_id)
  )))
}

#' Change columns of a detail row, only where it still matches.
#'
#' For the reading job, which changes its own columns while staff change others. Nothing here
#' raises the revision.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param table `extracted_draft_table` or `edit_draft_table`.
#' @param id The draft's id.
#' @param changes A named list of columns to their new values.
#' @param where More filters the row must match, such as the reading it was started for.
#' @param select The columns to send back.
#' @return The row as it is now, or `NULL` if no row matched, so the change didn't apply.
draft_row_model_update_detail <- function(connection, table, id, changes, where = list(), select = "draft_id") {
  rows <- database_model_update(
    connection, table, changes, filters = c(draft_row_filter(id), where), select = select
  )
  if (length(rows) > 0) rows[[1]]
}

# Ids are numbers, and only numbers go into a filter.
draft_row_filter <- function(id) {
  list(draft_id = database_eq(draft_row_number(id)))
}

draft_row_number <- function(id) {
  if (!grepl("^[0-9]+$", id %||% "")) stop("There's no draft with that number.", call. = FALSE)
  as.numeric(id)
}

# A named list sent as a JSON object, even when it is empty.
draft_row_object <- function(values) {
  if (length(values) == 0) structure(list(), names = character()) else values
}

#' A time from the database as the app writes times.
#'
#' @param text A timestamp as Postgres sends it, e.g. `"2026-09-29T14:03:22.5+00:00"`.
#' @return The same moment in UTC as `format_time()` writes it, or `NULL` if `text` is `NULL`.
database_time <- function(text) {
  if (!is.null(text)) format_time(parse_time(text))
}

# The person a draft's preparer asked to review it: id and name, or NULL.
draft_row_person <- function(id, person) {
  if (!is.null(id)) list(id = id, name = person$name)
}

# Who has the draft open to work on, and when their page last renewed the claim: or NULL. The claim
# may have lapsed since; see draft_row_service_editor().
draft_row_editing <- function(row) {
  if (!is.null(row$editing_by)) list(by_id = row$editing_by, by = row$editor$name, at = database_time(row$editing_at))
}

# A one-to-one row embedded in another: the row, or NULL. PostgREST sends it as an object, or as a
# list of one when the key it follows isn't the embedded table's own.
draft_row_one <- function(embedded) {
  if (is.list(embedded) && is.null(names(embedded))) embedded <- if (length(embedded) > 0) embedded[[1]]
  embedded
}

# The latest of a draft's reviews of one kind ("Submitted", "Sent Back" or "Approved"), or NULL.
draft_row_latest <- function(row, event) {
  reviews <- Filter(function(review) identical(review$event, event), row$reviews %||% list())
  if (length(reviews) == 0) {
    return(NULL)
  }
  times <- vapply(reviews, function(review) as.numeric(parse_time(review$occurred_at)), numeric(1))
  reviews[[order(times, decreasing = TRUE)[1]]]
}

# When the draft was last sent for review, or NULL.
draft_row_submitted <- function(row) {
  database_time(draft_row_latest(row, "Submitted")$occurred_at)
}

# Who sent the draft back, when and why: only while it is sent back.
draft_row_sent_back <- function(row) {
  if (identical(row$status, "Sent Back")) {
    sent <- draft_row_latest(row, "Sent Back")
    if (!is.null(sent)) {
      list(by = sent$person$name, by_id = sent$staff_id, date = database_time(sent$occurred_at), note = sent$note)
    }
  }
}
