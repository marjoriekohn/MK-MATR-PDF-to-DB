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
# which revision, when, and a send back's note. A draft is read with its events, and with its details:
# one request for one draft or a list of them (draft_row_model_records(), draft_row_model_one()).
# extraction_model_* and edit_model_* turn the rows into the records the services work
# with. Anything that changes several tables at once is a function of the database called
# through /rpc/ (create_draft, save_draft, send_back_draft, apply_draft, approve_draft;
# docs/sql/1_setup.sql, section 4c), because separate requests can't
# share a transaction. Changes to one table (the reading job's progress) are plain requests.
# Who is working on a draft (editing_by, since editing_at) is claimed and let go through
# claim_draft and release_draft (docs/sql/1_setup.sql, section 4c), so two
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
  "reviews:draft_review_event(event,staff_id,note,occurred_at,person:staff(name))"
)

# The statuses of a draft that is finished with: applied to the MATR metadata database, or set aside. A list
# of drafts leaves them out: every page that lists drafts shows only those still being prepared or waiting,
# and the finished ones only grow in number (the database keeps every applied draft as its record).
draft_closed_statuses <- c("Applied", "Withdrawn")

# The records of the drafts of one kind still being prepared or waiting, read with their details in one request.
# A draft whose detail row is missing is left out. (One finished with is read on its own: draft_row_model_one().)
# kind: `"Extraction"` or `"Edit"`.
# table: The kind's detail table: `extracted_draft_table` or `edit_draft_table`.
# columns: The columns of it to read.
# record: A function of a draft's shared row and its detail row, giving its record.
# Returns: A list of records, newest saved first, named by id.
draft_row_model_records <- function(connection, kind, table, columns, record, status = NULL) {
  rows <- database_model_select(
    connection, draft_table, c(draft_columns, draft_row_detail(table, columns)),
    order_by = c("saved_at.desc", "draft_id.desc"),
    filters = c(
      list(kind = database_eq(kind)),
      list(status = if (!is.null(status)) database_eq(status) else paste0("not.", database_in(draft_closed_statuses)))
    )
  )
  rows <- Filter(function(row) !is.null(draft_row_one(row$detail)), rows)
  records <- lapply(rows, function(row) record(row, draft_row_one(row$detail)))
  stats::setNames(records, vapply(records, function(found) found$id, character(1)))
}

# One draft's record, read with its details in one request.
# id: The draft's id: digits only.
# gone: What to say if there is no such draft of this kind.
# Returns: The record. An error saying `gone` if there is none.
draft_row_model_one <- function(connection, id, kind, table, columns, record, gone) {
  rows <- database_model_select(
    connection, draft_table, c(draft_columns, draft_row_detail(table, columns)),
    filters = c(draft_row_filter(id), list(kind = database_eq(kind)))
  )
  detail <- if (length(rows) > 0) draft_row_one(rows[[1]]$detail)
  if (is.null(detail)) stop(gone, call. = FALSE)
  record(rows[[1]], detail)
}

# A draft's detail row as the read of its shared row embeds it: `detail`.
draft_row_detail <- function(table, columns) sprintf("detail:%s(%s)", table, paste(columns, collapse = ","))

# What every draft's record has, from its shared row; see the tops of model_edit.R and model_extraction.R.
draft_row_record <- function(parent) {
  list(
    id = as.character(parent$draft_id),
    status = parent$status,
    summary = parent$summary,
    prepared_by = parent$preparer$name,
    prepared_by_id = parent$prepared_by,
    started = database_time(parent$created_at),
    saved = database_time(parent$saved_at),
    saved_by = parent$saver$name,
    saved_by_id = parent$saved_by,
    revision = parent$revision,
    submitted = draft_row_submitted(parent),
    submitted_exact = draft_row_submitted_exact(parent),
    requested_reviewer = draft_row_person(parent$requested_reviewer, parent$reviewer),
    sent_back = draft_row_sent_back(parent)
  )
}

# The detail rows of some drafts.
# table: `extracted_draft_table` or `edit_draft_table`.
# ids: The drafts' ids; digits only. None reads none.
# columns: The columns to read; `draft_id` is always among them.
# Returns: A list of rows (lists), named by draft id.
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

# Add a draft and its details.
# One database function, so a draft never exists without its details.
# kind: `"Extraction"` or `"Edit"`.
# prepared_by: The staff id of the person starting it.
# parent: The shared columns to set, as a named list (`status`, `summary`,
#   `requested_reviewer`, `created_at`).
# detail: The kind's own columns to set, as a named list.
# Returns: The new draft's id, as text.
draft_row_model_create <- function(connection, kind, prepared_by, parent = list(), detail = list()) {
  as.character(database_model_call(connection, "create_draft", list(
    p_kind = kind, p_prepared_by = as.numeric(prepared_by),
    p_parent = draft_row_object(parent), p_detail = draft_row_object(detail)
  )))
}

# Save a change to a draft and its details together.
# Only the columns named are written. The database raises the revision unless told not to,
# and records who saved it, and who submitted it if the status becomes Awaiting Review. For an
# extraction, `detail` may name columns of extracted_draft, ai_extracted_draft and pdf_file
# (`pdf_pages`, `pdf_bytes`): the database puts each in its table.
# parent,detail: Named lists of columns to their new values; a `NULL` value empties the column.
# expected_revision: The revision the person was looking at; the save is refused if
#   the draft has moved on. `NULL` skips the check.
# new_revision: `FALSE` for a change that isn't to the content (withdrawing).
# Returns: The revision after the save. An error with the database's sentence if it refused.
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

# Claim a draft for one person to work on, or renew their claim.
# The database gives it to them if nobody has it, they have it already, or whoever has it hasn't
# renewed their claim for `stale_seconds` (their page is gone). `take_over` takes it whoever has it.
# Nothing here raises the revision.
# staff_id: The staff id of the person claiming it.
# take_over: Whether to take it from whoever has it.
# stale_seconds: How long a claim lasts without being renewed.
# Returns: A list: `claimed` (whether this person has it now), and who does have it: `by_id`, `by`
#   (their name) and `at` (when they last renewed it); and the draft's `revision`. An error with the
#   database's sentence if it refused (no such draft, or one finished with).
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

# Let go of a person's claim on a draft, if they still have it.
# staff_id: The staff id of the person letting it go.
# Returns: Whether they had it.
draft_row_model_release <- function(connection, id, staff_id) {
  isTRUE(database_model_call(connection, "release_draft", list(
    p_draft_id = draft_row_number(id), p_staff_id = as.numeric(staff_id)
  )))
}

# Change columns of a detail row, only where it still matches.
# For the reading job, which changes its own columns while staff change others. Nothing here
# raises the revision.
# table: `extracted_draft_table` or `edit_draft_table`.
# changes: A named list of columns to their new values.
# where: More filters the row must match, such as the reading it was started for.
# select: The columns to send back.
# Returns: The row as it is now, or `NULL` if no row matched, so the change didn't apply.
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

# A time from the database as the app writes times.
# text: A timestamp as Postgres sends it, e.g. `"2026-09-29T14:03:22.5+00:00"`.
# Returns: The same moment in UTC as `format_time()` writes it, or `NULL` if `text` is `NULL`.
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

# The same, as seconds with the fractions the database keeps (`submitted` is to the second), or NULL: two
# requests sent in the same second still come in the order they were sent.
draft_row_submitted_exact <- function(row) {
  sent <- draft_row_latest(row, "Submitted")$occurred_at
  if (!is.null(sent)) as.numeric(parse_time(sent))
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
