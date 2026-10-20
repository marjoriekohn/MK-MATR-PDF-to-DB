# ------ Edit proposals -------------------------------------------------------------
# One record per change staff make on the Make an edit page, saying who, what it comes
# to in a sentence, how to open it again on the form, and the items to apply. Each is a
# DRAFT row of kind "Edit" with an EDIT_DRAFT row (model_draft_row.R). Applying it (at once for
# an add or an update, after one approval for a STAFF member's delete) makes it a
# WRITE_HISTORY row with its WRITE_HISTORY_ITEM rows. `status` is Draft until it's sent for
# review, then Awaiting Review; Sent Back if a reviewer returned it; Withdrawn if the
# preparer discards it; Applied once it is in the MATR metadata database.
#
# A record's fields and where they are kept (draft / edit_draft):
#   id, status, summary                            draft
#   prepared_by (name), prepared_by_id, started    draft (created_at)
#   saved, saved_by (name), saved_by_id, revision  draft
#   submitted                                      draft (submitted_at)
#   requested_reviewer = list(id, name)            draft (requested_reviewer)
#   sent_back = list(by, by_id, date, note)        draft (sent_back_*), only while Sent Back
#   edit_kind ("Add", "Update" or "Delete")        edit_draft
#   form, reach, items                             edit_draft
#
# Items are read back as a data frame (item_number, entity_name,
# record_identifier, action, attribute_name, old_value, new_value), empty
# values as NA.

edit_draft_kind <- "Edit"

edit_item_columns <- c(
  "item_number", "entity_name", "record_identifier", "action",
  "attribute_name", "old_value", "new_value"
)

#' Every edit proposal still being prepared or waiting for approval (not applied or withdrawn).
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of proposal records, newest saved first, named by id; see the top of this
#'   file for what a record holds.
edit_model_get_all <- function(connection) {
  edit_model_records(connection)
}

# The records of the edit drafts, only those with this status if given.
edit_model_records <- function(connection, status = NULL) {
  parents <- draft_row_model_list(connection, edit_draft_kind, status)
  ids <- vapply(parents, function(row) as.character(row$draft_id), character(1))
  details <- draft_row_model_details(connection, edit_draft_table, ids, c("edit_kind", "form", "reach", "items"))
  records <- lapply(Filter(function(row) !is.null(details[[as.character(row$draft_id)]]), parents), function(row) {
    edit_model_record(row, details[[as.character(row$draft_id)]])
  })
  stats::setNames(records, vapply(records, function(record) record$id, character(1)))
}

#' One edit proposal.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The proposal's id.
#' @return The record, as in `edit_model_get_all()`. An error if there is none.
edit_model_get <- function(connection, id) {
  gone <- "That edit is no longer in the database."
  parent <- draft_row_model_get(connection, id)
  if (is.null(parent) || !identical(parent$kind, edit_draft_kind)) stop(gone, call. = FALSE)
  detail <- draft_row_model_details(connection, edit_draft_table, id, c("edit_kind", "form", "reach", "items"))[[as.character(id)]]
  if (is.null(detail)) stop(gone, call. = FALSE)
  edit_model_record(parent, detail)
}

#' Save an edit proposal.
#'
#' Added if it has no id yet; otherwise the row it names is changed, as the person saving it.
#' The database numbers a new one and raises the revision of a changed one.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record A proposal record, as in `edit_model_get_all()`; `id` is `NULL` for a new one.
#'   `expected_revision`, if it has one, is the revision the person was looking at: the save is
#'   refused if someone else has saved since.
#' @param staff_id The staff id of the person saving.
#' @param new_revision `FALSE` for a change that isn't to the content (sending back, withdrawing).
#' @return The record as saved, with its id. An error with the database's sentence if it refused.
edit_model_save <- function(connection, record, staff_id = record$prepared_by_id, new_revision = TRUE) {
  parent <- list(
    status = record$status, summary = record$summary, submitted_at = record$submitted
  )
  parent["requested_reviewer"] <- list(record$requested_reviewer$id)
  detail <- list(
    edit_kind = record$edit_kind %||% edit_model_kind(record$form),
    form = record$form,
    items = if (is.null(record$items)) list() else record$items
  )
  detail["reach"] <- list(record$reach)
  if (is.null(record$id)) {
    parent$created_at <- record$started
    id <- draft_row_model_create(connection, edit_draft_kind, record$prepared_by_id, parent, detail)
  } else {
    id <- record$id
    draft_row_model_save(connection, id, staff_id, parent, detail, record$expected_revision, new_revision)
  }
  edit_model_get(connection, id)
}

# "Add", "Update" or "Delete", from the sentence the form was made from.
edit_model_kind <- function(form) {
  action <- form$action %||% ""
  paste0(toupper(substr(action, 1, 1)), substring(action, 2))
}

edit_model_record <- function(parent, detail) {
  # The form and items are read back the way staff's page made them: values by
  # name, items as a data frame.
  content <- jsonlite::fromJSON(
    jsonlite::toJSON(
      list(form = detail$form, reach = detail$reach, items = detail$items),
      auto_unbox = TRUE, null = "null"
    ),
    simplifyVector = TRUE
  )
  items <- content$items
  if (is.data.frame(items) && all(edit_item_columns %in% names(items))) items <- items[edit_item_columns]
  list(
    id = as.character(parent$draft_id),
    kind = edit_draft_kind,
    edit_kind = detail$edit_kind,
    status = parent$status,
    prepared_by = parent$preparer$name,
    prepared_by_id = parent$prepared_by,
    started = database_time(parent$created_at),
    saved = database_time(parent$saved_at),
    saved_by = parent$saver$name,
    saved_by_id = parent$saved_by,
    submitted = database_time(parent$submitted_at),
    revision = parent$revision,
    summary = parent$summary,
    requested_reviewer = draft_row_person(parent$requested_reviewer, parent$reviewer),
    sent_back = draft_row_sent_back(parent),
    form = content$form,
    reach = content$reach,
    items = items
  )
}
