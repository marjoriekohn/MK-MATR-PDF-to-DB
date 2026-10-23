# ------ Edit History ---------------------------------------------------------------
# An applied change is a DRAFT row with the status Applied (model_draft_row.R): what it was in a
# sentence, who prepared it, the one person who approved it (its Approved DRAFT_REVIEW_EVENT, or
# none: an add or an update, or anything an ADMIN did, needs no approval), and when it was
# applied. APPLIED_ITEM has
# one row for each field it changed, with the old and new value (the same items the draft held,
# with the numbers the database gave new rows). Only apply_draft() and approve_draft() in the
# database write them; the site only reads. (Until 2026-10-09 these were the write_history and
# write_history_item tables; the database now keeps the record on the draft itself.)
#
# A record's fields:
#   id                 the draft's id, as text
#   kind               Add, Update or Delete (an extraction is an Add)
#   source             "Edit" or "Extraction"
#   status, summary
#   prepared_by        the preparer's name (NA if they are no longer listed)
#   approved_by        the approver's name; NA if it needed no approval
#   approval_required  whether someone had to approve it
#   prepared, approved, applied   times as the app writes them (`approved` is NULL if nobody did)

history_table <- "draft"
history_item_table <- "applied_item"
history_applied_status <- "Applied"

history_columns <- c(
  "draft_id", "kind", "status", "summary",
  "created_at", "applied_at",
  "preparer:staff!prepared_by(name)",
  "reviews:draft_review_event(event,occurred_at,person:staff(name))",
  "edit:edit_draft(edit_kind)"
)

#' Every applied change, newest first.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of records, named by id; empty if nothing has been applied yet.
history_model_get_all <- function(connection) {
  rows <- database_model_select(
    connection, history_table, history_columns,
    order_by = c("applied_at.desc", "draft_id.desc"),
    filters = list(status = database_eq(history_applied_status))
  )
  records <- lapply(rows, history_model_record)
  stats::setNames(records, vapply(records, function(record) record$id, character(1)))
}

#' The fields one applied change changed.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The applied draft's id: digits only.
#' @return A data frame with `item_number`, `entity_name`, `record_identifier`, `action`,
#'   `attribute_name`, `old_value` and `new_value`, in the order they were applied.
history_model_get_items <- function(connection, id) {
  if (!grepl("^[0-9]+$", id %||% "")) stop("There's no edit with that number.", call. = FALSE)
  database_model_get(
    connection, history_item_table, edit_item_columns,
    order_by = "item_number", filters = list(draft_id = database_eq(id))
  )
}

#' The number of applied changes.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A whole number.
history_model_count <- function(connection) {
  database_model_count(connection, history_table, filters = list(status = database_eq(history_applied_status)))
}

history_model_record <- function(row) {
  approval <- draft_row_latest(row, "Approved")
  approver <- approval$person$name
  edit <- draft_row_one(row$edit)
  list(
    id = as.character(row$draft_id),
    kind = if (identical(row$kind, extraction_kind)) "Add" else edit$edit_kind %||% "Edit",
    source = row$kind %||% edit_draft_kind,
    status = row$status,
    summary = row$summary %||% "",
    prepared_by = as.character(row$preparer$name %||% NA),
    approved_by = as.character(approver %||% NA),
    approval_required = !is.null(approval),
    prepared = database_time(row$created_at),
    approved = database_time(approval$occurred_at),
    applied = database_time(row$applied_at)
  )
}
