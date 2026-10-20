# ------ Extraction drafts -----------------------------------------------------------
# One record per extraction of a questionnaire, saying who started it, how it was made
# (read from a PDF by an AI, or typed by hand), how far reading it has got, and what staff
# typed about it. Each is a DRAFT row of kind "Extraction" with an EXTRACTED_DRAFT row
# (model_draft_row.R); this turns the two into the record the services work with, and a
# changed record back into the columns to save.
#
# A record's fields and where they are kept (draft / extracted_draft):
#   id, status, summary                            draft
#   kind (always "Extraction")                     draft, so a list of requests can tell it from an edit
#   prepared_by (name), prepared_by_id, started    draft (created_at)
#   saved, saved_by (name), saved_by_id, revision  draft
#   requested_reviewer = list(id, name)            draft (requested_reviewer)
#   sent_back = list(by, by_id, date, note)        draft (sent_back_*), only while Sent Back
#   method ("AI-Powered" or "Manual")              extracted_draft
#   pdf = list(name, path, pages, bytes, sha256)   pdf_name, pdf_sha256, pdf_pages, pdf_bytes
#   extraction_status, reading_started, reading_claimed_at, reading_pid, error,
#   model, prompt_version                          reading_*, model, prompt_version
#   info, matr_csv                                 info, matr_csv
# The questionnaire itself, what the AI returned and what the reviewer changed aren't in the
# record; they are big. See model_draft.R.
#
# The PDFs aren't in the database. Each upload is kept in `uploads/pdfs/` (the
# option matr.pdf_dir changes it) under its SHA-256, only so the reading job can
# open it and the Edit Extraction page can show it; the record keeps its name, size and hash.
# (Supabase Storage is on the to-do list.)

extraction_kind <- "Extraction"
extraction_ai <- "AI-Powered"
extraction_manual <- "Manual"

# Record field -> column of extracted_draft, for the fields that are one column each.
extraction_detail_columns <- c(
  method = "method", info = "info", matr_csv = "matr_csv", questionnaire = "questionnaire",
  extraction_status = "reading_status", reading_started = "reading_started",
  reading_claimed_at = "reading_claimed_at", reading_pid = "reading_pid", error = "reading_error",
  model = "model", prompt_version = "prompt_version"
)
# ... and of draft.
extraction_parent_columns <- c(status = "status", summary = "summary", submitted = "submitted_at")
# The fields of a record the reading job owns: it saves these, and staff never do.
extraction_reading_fields <- c(
  "extraction_status", "reading_started", "reading_claimed_at", "reading_pid", "error", "model",
  "prompt_version"
)

# The columns of extracted_draft a list of drafts reads (not the questionnaire, the AI's answer or the edits).
extraction_list_columns <- c(
  setdiff(unname(extraction_detail_columns), "questionnaire"), "pdf_name", "pdf_sha256", "pdf_pages", "pdf_bytes"
)

# ------ Reading --------------------------------------------------------------------
#' Every extraction draft still being prepared or waiting for approval (not applied or discarded).
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of records, newest saved first, named by id; see the top of this file for
#'   what a record holds. The questionnaire itself isn't in it; see `draft_model_get()`.
extraction_model_get_all <- function(connection) {
  extraction_model_records(connection)
}

# The records of the extraction drafts, only those with this status if given.
extraction_model_records <- function(connection, status = NULL) {
  parents <- draft_row_model_list(connection, extraction_kind, status)
  ids <- vapply(parents, function(row) as.character(row$draft_id), character(1))
  details <- draft_row_model_details(connection, extracted_draft_table, ids, extraction_list_columns)
  records <- lapply(Filter(function(row) !is.null(details[[as.character(row$draft_id)]]), parents), function(row) {
    extraction_model_record(row, details[[as.character(row$draft_id)]])
  })
  stats::setNames(records, vapply(records, function(record) record$id, character(1)))
}

#' One extraction draft.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @return The record, as in `extraction_model_get_all()`. An error if there is none.
extraction_model_get <- function(connection, id) {
  gone <- "That draft is no longer in the database."
  parent <- draft_row_model_get(connection, id)
  if (is.null(parent) || !identical(parent$kind, extraction_kind)) stop(gone, call. = FALSE)
  detail <- draft_row_model_details(connection, extracted_draft_table, id, extraction_list_columns)[[as.character(id)]]
  if (is.null(detail)) stop(gone, call. = FALSE)
  extraction_model_record(parent, detail)
}

extraction_model_record <- function(parent, detail) {
  list(
    id = as.character(parent$draft_id),
    kind = extraction_kind,
    status = parent$status,
    summary = parent$summary,
    prepared_by = parent$preparer$name,
    prepared_by_id = parent$prepared_by,
    started = database_time(parent$created_at),
    saved = database_time(parent$saved_at),
    saved_by = parent$saver$name,
    saved_by_id = parent$saved_by,
    revision = parent$revision,
    submitted = database_time(parent$submitted_at),
    requested_reviewer = draft_row_person(parent$requested_reviewer, parent$reviewer),
    sent_back = draft_row_sent_back(parent),
    method = detail$method,
    pdf = list(
      name = detail$pdf_name,
      path = if (!is.null(detail$pdf_sha256)) extraction_model_pdf_path(detail$pdf_sha256),
      pages = detail$pdf_pages,
      bytes = detail$pdf_bytes,
      sha256 = detail$pdf_sha256
    ),
    extraction_status = detail$reading_status,
    reading_started = database_time(detail$reading_started),
    reading_claimed_at = database_time(detail$reading_claimed_at),
    reading_pid = detail$reading_pid,
    error = detail$reading_error,
    model = detail$model,
    prompt_version = detail$prompt_version,
    info = detail$info,
    matr_csv = detail$matr_csv
  )
}

# ------ Writing --------------------------------------------------------------------
#' Add an extraction draft.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record A record as in `extraction_model_get_all()`, without an `id`; `prepared_by_id`
#'   says who is starting it.
#' @return The record as saved, with its id.
extraction_model_create <- function(connection, record) {
  rows <- extraction_model_rows(record, setdiff(names(record), "requested_reviewer"))
  rows$parent$created_at <- record$started
  if (!is.null(record$requested_reviewer)) rows$parent$requested_reviewer <- record$requested_reviewer$id
  id <- draft_row_model_create(connection, extraction_kind, record$prepared_by_id, rows$parent, rows$detail)
  extraction_model_get(connection, id)
}

#' Save some fields of an extraction draft, as a person.
#'
#' Only the columns of the fields named in `changed` are written, so what the reading
#' job and staff change at the same moment doesn't undo each other. The database records
#' who saved it and, unless told not to, raises the revision.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The whole record, including its `id`.
#' @param changed The names of the fields to save.
#' @param staff_id The staff id of the person saving.
#' @param expected_revision The revision the person was looking at, or `NULL` not to check.
#' @param new_revision `FALSE` for a change that isn't to the content.
#' @return The record as saved. An error with the database's sentence if it refused.
extraction_model_save <- function(connection, record, changed, staff_id, expected_revision = NULL, new_revision = TRUE) {
  rows <- extraction_model_rows(record, setdiff(changed, "requested_reviewer"))
  if ("requested_reviewer" %in% changed) rows$parent["requested_reviewer"] <- list(record$requested_reviewer$id)
  if ("sent_back" %in% changed) {
    sent <- record$sent_back
    rows$parent["sent_back_by"] <- list(sent$by_id)
    rows$parent["sent_back_at"] <- list(sent$date)
    rows$parent["sent_back_note"] <- list(sent$note)
  }
  draft_row_model_save(connection, record$id, staff_id, rows$parent, rows$detail, expected_revision, new_revision)
  extraction_model_get(connection, record$id)
}

#' Save the reading job's progress.
#'
#' Only the reading's own columns, and only while the draft is still being read for the
#' reading it was started for, so what the job and staff change at the same moment doesn't
#' undo each other, and a job that was overtaken by a new PDF saves nothing.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param changes A named list of record fields (`extraction_status`, `error`, `model`,
#'   `prompt_version`, `reading_pid`) to their new values.
#' @param reading_started The reading's start time the job was started for.
#' @return `TRUE` if saved, `FALSE` if the draft no longer matches and nothing was.
extraction_model_save_reading <- function(connection, id, changes, reading_started) {
  not_its_own <- setdiff(names(changes), extraction_reading_fields)
  if (length(not_its_own) > 0) stop("The reading job can't save ", paste(not_its_own, collapse = ", "), ".", call. = FALSE)
  columns <- unname(extraction_detail_columns[names(changes)])
  saved <- draft_row_model_update_detail(
    connection, extracted_draft_table, id, stats::setNames(changes, columns),
    where = list(reading_started = database_eq(reading_started))
  )
  !is.null(saved)
}

# The columns to write for the fields in `changed`, split by the table that holds them.
extraction_model_rows <- function(record, changed) {
  parent <- list()
  detail <- list()
  for (field in intersect(changed, names(extraction_parent_columns))) {
    parent[extraction_parent_columns[[field]]] <- list(record[[field]])
  }
  for (field in intersect(changed, names(extraction_detail_columns))) {
    detail[extraction_detail_columns[[field]]] <- list(record[[field]])
  }
  if ("pdf" %in% changed) {
    detail[c("pdf_name", "pdf_sha256", "pdf_pages", "pdf_bytes")] <- list(
      record$pdf$name, record$pdf$sha256, record$pdf$pages, record$pdf$bytes
    )
  }
  list(parent = parent, detail = detail)
}

#' Claim a draft for reading.
#'
#' The database does this in one step, so two copies of the site never both start a job.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param reading_started The reading's start time, as the draft records it.
#' @param now The time to record as the claim.
#' @return `TRUE` if the draft was free, `FALSE` if another copy claimed it first (or
#'   the reading has since restarted).
extraction_model_claim_reading <- function(connection, id, reading_started, now = Sys.time()) {
  claimed <- draft_row_model_update_detail(
    connection, extracted_draft_table, id, list(reading_claimed_at = format_time(now)),
    where = list(reading_started = database_eq(reading_started), reading_claimed_at = "is.null")
  )
  !is.null(claimed)
}

# ------ The PDFs --------------------------------------------------------------------
extraction_model_pdf_dir <- function() getOption("matr.pdf_dir", "uploads/pdfs")

extraction_model_pdf_path <- function(sha256) {
  file.path(extraction_model_pdf_dir(), paste0(sha256, ".pdf"))
}

#' Keep a copy of an uploaded PDF.
#'
#' The same file is only ever stored once, named by its hash, in `uploads/pdfs/` (the
#' option `matr.pdf_dir` changes the folder).
#'
#' @param upload_path Where the upload is now.
#' @param sha256 Its SHA-256, from `extraction_model_sha256()`.
#' @return Where the copy is.
extraction_model_save_pdf <- function(upload_path, sha256) {
  dir.create(extraction_model_pdf_dir(), recursive = TRUE, showWarnings = FALSE)
  path <- extraction_model_pdf_path(sha256)
  if (!file.exists(path)) file.copy(upload_path, path)
  path
}

#' The SHA-256 of a file.
#'
#' It names the file's copy in the cache and says it's the same PDF.
#'
#' @param path The file.
#' @return The hash, as a string.
extraction_model_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

#' The text of an uploaded CSV.
#'
#' @param path The file.
#' @return Its lines joined by newlines, read as UTF-8.
extraction_model_read_csv <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# ------ Reading a PDF's first bytes ----------------------------------------------
#' Whether a file starts like a PDF.
#'
#' @param path The file.
#' @return `TRUE` if it begins `%PDF-`.
extraction_model_is_pdf <- function(path) {
  identical(readBin(path, "raw", 5), charToRaw("%PDF-"))
}

#' Count a PDF's pages.
#'
#' Counts the page objects the PDF lists in plain view. PDFs that pack them into
#' compressed streams show none.
#'
#' @param path The PDF.
#' @return The count, or `NULL` (unknown, not 0) if none were found.
extraction_model_page_count <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  n_pages <- length(grepRaw("/Type[[:space:]]*/Page[^s]", bytes, all = TRUE))
  if (n_pages > 0) n_pages
}
