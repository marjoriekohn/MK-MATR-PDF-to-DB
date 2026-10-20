# ------ Drafts: the shared rows and their details --------------------------------------
# What staff are still working on before it reaches the live tables is a DRAFT: one row
# of the columns both kinds share,
#   status, prepared_by, saved_by, requested_reviewer (STAFF rows), revision, summary,
#   sent_back_by / _at / _note, created_at, saved_at, submitted_at
# and one row of a table for its kind:
#   EXTRACTED_DRAFT  an extraction of a questionnaire (kind "Extraction")
#   EDIT_DRAFT       an edit made on Make an edit (kind "Edit")
# extraction_model_* and edit_model_* turn the two rows into the records the services work
# with. Anything that changes several tables at once is a function of the database called
# through /rpc/ (create_draft, save_draft, apply_draft, approve_draft;
# supabase/migrations/20261007120000_drafts_entity.sql), because separate requests can't
# share a transaction. Changes to one table (the reading job's progress) are plain requests.

draft_table <- "draft"
extracted_draft_table <- "extracted_draft"
edit_draft_table <- "edit_draft"

# The shared columns, and the names of the people they point at.
draft_columns <- c(
  "draft_id", "kind", "status", "prepared_by", "saved_by", "requested_reviewer", "revision", "summary",
  "sent_back_by", "sent_back_at", "sent_back_note", "created_at", "saved_at", "submitted_at",
  "preparer:staff!prepared_by(name)", "saver:staff!saved_by(name)",
  "reviewer:staff!requested_reviewer(name)", "sender:staff!sent_back_by(name)"
)

# The drafts of one kind, newest saved first; only those with this status, if given.
draft_row_model_list <- function(connection, kind, status = NULL) {
  database_model_select(
    connection, draft_table, draft_columns,
    order_by = c("saved_at.desc", "draft_id.desc"),
    filters = c(list(kind = database_eq(kind)), if (!is.null(status)) list(status = database_eq(status)))
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
#' and records who saved it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param staff_id The staff id of the person saving.
#' @param parent,detail Named lists of columns to their new values; a `NULL` value empties the column.
#' @param expected_revision The revision the person was looking at; the save is refused if
#'   the draft has moved on. `NULL` skips the check.
#' @param new_revision `FALSE` for a change that isn't to the content (sending back, withdrawing).
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

# The person a draft was sent back by, or asked to review it: id and name, or NULL.
draft_row_person <- function(id, person) {
  if (!is.null(id)) list(id = id, name = person$name)
}

# What the shared row says about being sent back: only while it is.
draft_row_sent_back <- function(row) {
  if (identical(row$status, "Sent Back")) {
    list(by = row$sender$name, by_id = row$sent_back_by, date = database_time(row$sent_back_at), note = row$sent_back_note)
  }
}
