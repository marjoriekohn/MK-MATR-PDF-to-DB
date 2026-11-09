# ------ Extractions -----------------------------------------------------------------
# One record per extraction of a questionnaire, saying who started it, how it was made
# (read from a PDF by an AI, or typed by hand), how far reading it has got, where it goes and
# what staff typed about it. Each is one row of EXTRACTION (docs/database_design.md); this turns
# a row into the record the services work with, and a changed record back into the columns to
# save. Everything here is a plain request on the table (model_database.R); only the push is a
# function of the database (service_extraction_push.R).
#
# A record's fields and the columns they come from:
#   id                                             extraction_id, as text
#   kind (always "Extraction")                     so a list of records can say what it holds
#   status ("Draft" or "Pushed"), summary, revision
#   prepared_by (name), prepared_by_id, started    prepared_by (a STAFF row), created_at
#   saved, saved_by (name), saved_by_id            saved_at, saved_by
#   pushed                                         pushed_at, once the extraction is Pushed
#   editing = list(by_id, by, at)                  editing_by, editing_at, while someone has claimed it
#   method ("AI-Powered" or "Manual")
#   pdf = list(name, path, pages, bytes, sha256)   pdf_name, pdf_sha256; pdf_file (pdf_pages, pdf_bytes)
#   extraction_status, reading_started, reading_claimed_at, reading_pid, error,
#   model, prompt_version                          reading_status, reading_*, reading_error, model, prompt_version;
#                                                  NULL for a Manual extraction
#   info, matr_csv
#   supplement = list(name, sha256): the optional file of MATR IDs and codes read after the questionnaire
#                (service_reading.R), only named (sha256 for a PDF); saved as list(name, sha256, bytes) for a PDF
#                or list(name, text) for a CSV or text file
# The questionnaire itself (`questionnaire`), what the AI returned (`ai_original`) and how the reviewed copy
# differs from it (`review_edits`) aren't in the record; they are big, and read on their own
# (extraction_model_questionnaire()).
#
# Saving. A save by staff names the columns to write and the revision the person was looking at: the
# request only changes the row while it is still at that revision (`revision=eq.<n>` is the lock), and the
# database's extraction_touch trigger raises the revision and moves saved_at when the content changes. A
# save of a stale copy changes nothing, and is an error with a sentence. The reading job writes its own
# columns, only while the row is still being read for the reading it was started for (`reading_started`).
# Who is working on the draft (editing_by, editing_at) is claimed and let go the same way: a claim only
# goes through while nobody has it, this person has it, or whoever has it hasn't renewed it lately.
#
# The PDFs aren't in the database: the record keeps their name, size and hash. Each upload is kept
# in the project's Storage (the bucket `storage_pdf_bucket`, model_storage.R) under its SHA-256, so
# every copy of the site can reach it, and on this server's disk in `uploads/pdfs/` (the option
# matr.pdf_dir changes it), where the reading job opens it and the pages show it from. A server that
# hasn't got it (a hosted site loses its disk when it restarts) fetches it from Storage first
# (extraction_model_local_pdf()). Its pages and size are a PDF_FILE row, which the extraction points at.

extraction_table <- "extraction"
extraction_kind <- "Extraction"
extraction_ai <- "AI-Powered"
extraction_manual <- "Manual"

# Record field -> column, for the fields that are one column each.
extraction_columns <- c(
  status = "status", summary = "summary", method = "method", info = "info", matr_csv = "matr_csv",
  supplement = "supplement", extraction_status = "reading_status", reading_started = "reading_started",
  reading_claimed_at = "reading_claimed_at", reading_pid = "reading_pid", error = "reading_error",
  model = "model", prompt_version = "prompt_version"
)
# The fields of a record the reading job owns: it saves these, and staff never do.
extraction_reading_fields <- c(
  "extraction_status", "reading_started", "reading_claimed_at", "reading_pid", "error", "model", "prompt_version"
)

# What a list of records reads: everything but the questionnaire, the AI's answer and the edits. The file of
# MATR IDs and codes is only named in a list (a CSV's text can be long, and lists are read every few seconds
# while a draft is read); extraction_model_supplement() reads all of it.
extraction_record_select <- c(
  "extraction_id", "status", "summary", "method", "revision", "prepared_by", "saved_by",
  "created_at", "saved_at", "pushed_at", "editing_by", "editing_at", "info", "matr_csv",
  "pdf_name", "pdf_sha256", "pdf:pdf_file(pdf_pages,pdf_bytes)",
  "supplement_name:supplement->>name", "supplement_sha256:supplement->>sha256",
  unname(extraction_columns[extraction_reading_fields]),
  "preparer:staff!prepared_by(name)", "saver:staff!saved_by(name)", "editor:staff!editing_by(name)"
)

# ------ Reading --------------------------------------------------------------------
# Every extraction with a status, newest saved first: the drafts by default, since the pushed ones only
# grow in number (the AI Monitor reads those on its own).
# status: `"Draft"` or `"Pushed"`; `NULL` for all of them.
# Returns: A list of records, newest saved first, named by id; see the top of this file for
#   what a record holds.
extraction_model_get_all <- function(connection, status = "Draft") {
  rows <- database_model_select(
    connection, extraction_table, extraction_record_select,
    order_by = c("saved_at.desc", "extraction_id.desc"),
    filters = if (!is.null(status)) list(status = database_eq(status))
  )
  records <- lapply(rows, extraction_model_record)
  stats::setNames(records, vapply(records, function(record) record$id, character(1)))
}

# One extraction.
# Returns: The record, as in `extraction_model_get_all()`. An error if there is none.
extraction_model_get <- function(connection, id) {
  row <- extraction_model_select_one(connection, id, extraction_record_select)
  if (is.null(row)) stop(extraction_gone, call. = FALSE)
  extraction_model_record(row)
}

extraction_gone <- "That draft is no longer in the database."

# One row's `select`, or NULL if there is no such extraction.
extraction_model_select_one <- function(connection, id, select) {
  rows <- database_model_select(connection, extraction_table, select, filters = extraction_filter(id))
  if (length(rows) > 0) rows[[1]]
}

extraction_model_record <- function(row) {
  pdf <- embedded_one(row$pdf)
  list(
    id = as.character(row$extraction_id),
    kind = extraction_kind,
    status = row$status,
    summary = row$summary,
    prepared_by = row$preparer$name,
    prepared_by_id = row$prepared_by,
    started = database_time(row$created_at),
    saved = database_time(row$saved_at),
    saved_by = row$saver$name,
    saved_by_id = row$saved_by,
    pushed = database_time(row$pushed_at),
    revision = row$revision,
    editing = if (!is.null(row$editing_by)) list(by_id = row$editing_by, by = row$editor$name, at = database_time(row$editing_at)),
    method = row$method,
    pdf = list(
      name = row$pdf_name,
      path = if (!is.null(row$pdf_sha256)) extraction_model_pdf_path(row$pdf_sha256),
      pages = pdf$pdf_pages,
      bytes = pdf$pdf_bytes,
      sha256 = row$pdf_sha256
    ),
    extraction_status = row$reading_status,
    reading_started = database_time(row$reading_started),
    reading_claimed_at = database_time(row$reading_claimed_at),
    reading_pid = row$reading_pid,
    error = row$reading_error,
    model = row$model,
    prompt_version = row$prompt_version,
    info = row$info,
    matr_csv = row$matr_csv,
    supplement = if (!is.null(row$supplement_name)) list(name = row$supplement_name, sha256 = row$supplement_sha256)
  )
}

# A one-to-one row embedded in another: the row, or NULL. PostgREST sends it as an object, or as a
# list of one when the key it follows isn't the embedded table's own.
embedded_one <- function(embedded) {
  if (is.list(embedded) && is.null(names(embedded))) embedded <- if (length(embedded) > 0) embedded[[1]]
  embedded
}

# A time from the database as the app writes times.
# text: A timestamp as Postgres sends it, e.g. `"2026-09-29T14:03:22.5+00:00"`.
# Returns: The same moment in UTC as `format_time()` writes it, or `NULL` if `text` is `NULL`.
database_time <- function(text) {
  if (!is.null(text)) format_time(parse_time(text))
}

# ------ Writing --------------------------------------------------------------------
# Add an extraction.
# record: A record as in `extraction_model_get_all()`, without an `id`; `prepared_by_id`
#   says who is starting it, and `started` when.
# Returns: The record as the database stored it.
extraction_model_create <- function(connection, record) {
  row <- extraction_model_row(record, names(record))
  row$prepared_by <- database_number(record$prepared_by_id)
  row$saved_by <- database_number(record$prepared_by_id)
  if (!is.null(record$started)) row$created_at <- record$started
  if (!is.null(record$saved)) row$saved_at <- record$saved
  stored <- database_model_insert(connection, extraction_table, row, select = "extraction_id")
  extraction_model_get(connection, stored$extraction_id)
}

# Save some fields of an extraction, as a person.
# Only the columns of the fields named in `changed` are written, so what the reading
# job and staff change at the same moment doesn't undo each other. The database raises the
# revision when the content changes.
# record: The whole record, including its `id`.
# changed: The names of the fields to save.
# staff_id: Who is saving.
# expected_revision: The revision the person was looking at, or `NULL` not to check.
# Returns: The record as saved. An error with a sentence if the extraction is gone, or someone saved it
#   after `expected_revision`.
extraction_model_save <- function(connection, record, changed, staff_id, expected_revision = NULL) {
  row <- extraction_model_row(record, changed)
  row$saved_by <- database_number(staff_id)
  extraction_model_patch(connection, record$id, row, expected_revision, select = "extraction_id")
  extraction_model_get(connection, record$id)
}

# Change a row's columns, as a person: only while the row is still at `expected_revision`, if given.
# Returns: The row's `select` after the change. An error with a sentence if the extraction is gone, or
#   someone saved it since.
extraction_model_patch <- function(connection, id, changes, expected_revision = NULL, select = "revision") {
  filters <- extraction_filter(id)
  if (!is.null(expected_revision)) filters$revision <- database_eq(as.integer(expected_revision))
  rows <- database_model_update(connection, extraction_table, changes, filters, select = select)
  if (length(rows) == 0) {
    if (is.null(extraction_model_select_one(connection, id, "extraction_id"))) stop(extraction_gone, call. = FALSE)
    stop("This draft was changed after you opened it. Open it again to see the new version.", call. = FALSE)
  }
  rows[[1]]
}

# The columns to write for the fields in `changed`.
# A PDF's pages and size are a PDF_FILE row the extraction points at, kept first.
extraction_model_row <- function(record, changed) {
  row <- list()
  for (field in intersect(changed, names(extraction_columns))) {
    row[extraction_columns[[field]]] <- list(record[[field]])
  }
  if ("pdf" %in% changed) {
    row[c("pdf_name", "pdf_sha256")] <- list(record$pdf$name, record$pdf$sha256)
  }
  if ("questionnaire" %in% changed) row["questionnaire"] <- list(record$questionnaire)
  row
}

# Save the reading job's progress.
# Only the reading's own columns, and only while the draft is still being read for the reading it was
# started for, so what the job and staff change at the same moment doesn't undo each other, and a job
# that was overtaken by "Try reading again" saves nothing.
# changes: A named list of record fields (`extraction_status`, `error`, `model`,
#   `prompt_version`, `reading_pid`) to their new values.
# Returns: `TRUE` if saved, `FALSE` if the draft no longer matches and nothing was.
extraction_model_save_reading <- function(connection, id, changes, reading_started) {
  not_its_own <- setdiff(names(changes), extraction_reading_fields)
  if (length(not_its_own) > 0) stop("The reading job can't save ", paste(not_its_own, collapse = ", "), ".", call. = FALSE)
  columns <- unname(extraction_columns[names(changes)])
  extraction_model_update_reading(connection, id, stats::setNames(changes, columns), reading_started)
}

# Save what the reading job returned.
# It is only kept if the draft is still being read for `reading_started`.
# json: The questionnaire JSON the job built, as nested lists.
# Returns: Whether it was kept.
extraction_model_save_reading_result <- function(connection, id, json, reading_started) {
  extraction_model_update_reading(connection, id, list(ai_original = json), reading_started)
}

# Claim a draft for reading.
# The database does this in one step, so two copies of the site never both start a job.
# now: The time to record as the claim.
# Returns: `TRUE` if the draft was free, `FALSE` if another copy claimed it first (or
#   the reading has since restarted).
extraction_model_claim_reading <- function(connection, id, reading_started, now = Sys.time()) {
  extraction_model_update_reading(
    connection, id, list(reading_claimed_at = format_time(now)), reading_started,
    where = list(reading_claimed_at = "is.null")
  )
}

# Change columns of a row, only while it is still being read for `reading_started` (and matches `where`).
# Nothing here raises the revision. Returns whether the row matched.
extraction_model_update_reading <- function(connection, id, changes, reading_started, where = list()) {
  rows <- database_model_update(
    connection, extraction_table, changes,
    filters = c(extraction_filter(id), list(reading_started = database_eq(reading_started)), where),
    select = "extraction_id"
  )
  length(rows) > 0
}

# ------ The questionnaire ---------------------------------------------------------------
# Two big jsonb columns, kept apart from the rest of the record:
#   questionnaire the reviewed copy staff save; empty until they first save
#   ai_original   the questionnaire JSON the AI returned (after reading_service_tidy()), written once
#                 by the reading job and never changed afterwards (AI-Powered extractions only)
# and review_edits, how the reviewed copy differs from what the AI returned, worked out again and
# replaced each time staff save (see service_draft_record.R).

# A draft's reviewed copy and what the AI returned, read at once.
# Returns: A list: `questionnaire` (the copy staff last saved, or `NULL`) and `original` (the
#   questionnaire JSON the AI returned, or `NULL`). An error if there is no such extraction.
extraction_model_questionnaire <- function(connection, id) {
  row <- extraction_model_select_one(connection, id, c("questionnaire", "ai_original"))
  if (is.null(row)) stop(extraction_gone, call. = FALSE)
  list(questionnaire = row$questionnaire, original = row$ai_original)
}

# Save the questionnaire as staff reviewed it.
# questionnaire: The questionnaire JSON, as nested lists.
# review_edits: How the copy differs from what the AI returned (an AI-Powered extraction), as a
#   list that replaces what was kept, or `NULL` to leave it as it is.
# expected_revision: The revision the person was looking at; the save is refused if someone
#   saved the draft since. `NULL` skips the check.
# Returns: The draft's revision after the save, invisibly. An error if there is no such extraction, or
#   it has moved on from `expected_revision`.
extraction_model_save_questionnaire <- function(connection, id, questionnaire, staff_id, review_edits = NULL, expected_revision = NULL) {
  changes <- list(questionnaire = questionnaire, saved_by = database_number(staff_id))
  if (!is.null(review_edits)) changes$review_edits <- review_edits
  invisible(extraction_model_patch(connection, id, changes, expected_revision)$revision)
}

# Put staff's reviewed copy aside, so the draft opens as the AI read it: for a draft that is read again.
# How the copy differed from what the AI returned goes with it.
# Returns: The draft's revision after it, invisibly. An error if there is no such extraction, or it has moved on.
extraction_model_clear_questionnaire <- function(connection, id, staff_id, expected_revision = NULL) {
  changes <- list(questionnaire = NULL, review_edits = list(), saved_by = database_number(staff_id))
  invisible(extraction_model_patch(connection, id, changes, expected_revision)$revision)
}

# A draft's whole file of MATR IDs and codes (a record only names it).
# Returns: `list(name, sha256, bytes)` for a PDF, `list(name, text)` for text, or `NULL` for none.
extraction_model_supplement <- function(connection, id) {
  extraction_model_select_one(connection, id, "supplement")$supplement
}

# ------ One person at a time -------------------------------------------------------------
extraction_claim_select <- c("revision", "status", "editing_by", "editing_at", "editor:staff!editing_by(name)")

# Claim a draft for one person to work on, or renew their claim.
# They get it if nobody has it, they have it already, or whoever has it hasn't renewed their claim for
# `stale_seconds` (their page is gone). `take_over` takes it whoever has it. One request, so two people
# asking at once can't both get it. Nothing here raises the revision.
# staff_id: The staff id of the person claiming it.
# take_over: Whether to take it from whoever has it.
# stale_seconds: How long a claim lasts without being renewed.
# Returns: A list: `claimed` (whether this person has it now), and who does have it: `by_id`, `by`
#   (their name) and `at` (when they last renewed it); and the draft's `revision`. An error with a
#   sentence if the extraction is gone or pushed.
extraction_model_claim <- function(connection, id, staff_id, take_over = FALSE, stale_seconds = 180, now = Sys.time()) {
  filters <- c(extraction_filter(id), list(status = database_eq("Draft")))
  if (!take_over) {
    filters$or <- sprintf(
      "(editing_by.is.null,editing_by.eq.%s,editing_at.lt.%s)",
      database_number(staff_id), format_time(now - stale_seconds)
    )
  }
  rows <- database_model_update(
    connection, extraction_table, list(editing_by = database_number(staff_id), editing_at = format_time(now)),
    filters, select = extraction_claim_select
  )
  claimed <- length(rows) > 0
  row <- if (claimed) rows[[1]] else extraction_model_select_one(connection, id, extraction_claim_select)
  if (is.null(row)) stop(extraction_gone, call. = FALSE)
  if (!identical(row$status, "Draft")) stop("This extraction was pushed already, so it can't be worked on.", call. = FALSE)
  list(
    claimed = claimed, by_id = row$editing_by, by = row$editor$name,
    at = database_time(row$editing_at), revision = row$revision
  )
}

# Let go of a person's claim on a draft, if they still have it.
# staff_id: The staff id of the person letting it go.
# Returns: Whether they had it.
extraction_model_release <- function(connection, id, staff_id) {
  rows <- database_model_update(
    connection, extraction_table, list(editing_by = NULL, editing_at = NULL),
    filters = c(extraction_filter(id), list(editing_by = database_eq(database_number(staff_id)))),
    select = "extraction_id"
  )
  length(rows) > 0
}

# ------ Discarding ---------------------------------------------------------------------
# Delete a draft. A pushed extraction stays: it is the record of what was read and reviewed.
# Returns: Called for its effect. An error with a sentence if it is gone or pushed.
extraction_model_discard <- function(connection, id) {
  rows <- database_model_delete(
    connection, extraction_table, c(extraction_filter(id), list(status = database_eq("Draft"))),
    select = "extraction_id"
  )
  if (length(rows) == 0) {
    if (is.null(extraction_model_select_one(connection, id, "extraction_id"))) stop(extraction_gone, call. = FALSE)
    stop("This extraction was pushed already, so it is kept as the record of what was done.", call. = FALSE)
  }
  invisible()
}

# ------ Ids ----------------------------------------------------------------------------
# Ids are numbers, and only numbers go into a filter.
extraction_filter <- function(id) {
  list(extraction_id = database_eq(extraction_model_number(id)))
}

extraction_model_number <- function(id) {
  id <- as.character(id %||% "")
  if (!grepl("^[0-9]+$", id)) stop("There's no draft with that number.", call. = FALSE)
  id
}

# A number as the database should see it: never in R's scientific notation (a 9-digit id with zeros at the
# end would otherwise be sent as "1.2e+08").
database_number <- function(x) {
  format(as.numeric(x), scientific = FALSE, trim = TRUE)
}

# ------ A wave's files -------------------------------------------------------------
# The files a wave was extracted from, kept with the wave when its extraction was pushed.
# Returns: A list: `pdf_sha256`, `pdf_name`, `supplement_name`, `supplement_sha256` and `supplement_text` (each
#   `NA` when it has none), or `NULL` when there is no such wave.
extraction_model_wave_files <- function(connection, wave_id) {
  columns <- c("wave_id", "pdf_sha256", "pdf_name", "supplement_name", "supplement_sha256", "supplement_text")
  rows <- database_model_get(connection, "wave", columns, order_by = "wave_id", filters = list(wave_id = database_eq(database_number(wave_id))))
  if (nrow(rows) > 0) as.list(rows[1, columns])
}

# ------ The PDFs --------------------------------------------------------------------
extraction_model_pdf_dir <- function() getOption("matr.pdf_dir", "uploads/pdfs")

extraction_model_pdf_path <- function(sha256) {
  file.path(extraction_model_pdf_dir(), paste0(sha256, ".pdf"))
}

# Keep an uploaded PDF: in Storage, a copy on this server, and its row (pages and size) in the database.
# Named by its hash, so the same file is only ever kept once: in Storage (`storage_pdf_bucket`), in
# `uploads/pdfs/` (the option `matr.pdf_dir` changes the folder) and as one PDF_FILE row.
# upload_path: Where the upload is now.
# sha256: Its SHA-256, from `extraction_model_sha256()`.
# pages: Its page count (`extraction_model_page_count()`), or `NULL` if unknown.
# Returns: Where the copy on this server is. An error with a sentence if Storage refused it.
extraction_model_save_pdf <- function(connection, upload_path, sha256, pages = NULL) {
  storage_model_upload(connection, storage_pdf_bucket, paste0(sha256, ".pdf"), upload_path)
  extraction_model_keep_pdf_file(connection, sha256, pages, file.size(upload_path))
  dir.create(extraction_model_pdf_dir(), recursive = TRUE, showWarnings = FALSE)
  path <- extraction_model_pdf_path(sha256)
  if (!file.exists(path)) file.copy(upload_path, path)
  path
}

# A PDF's row (PDF_FILE), added unless the file is there already.
extraction_model_keep_pdf_file <- function(connection, sha256, pages = NULL, bytes = NULL) {
  if (database_model_count(connection, "pdf_file", list(pdf_sha256 = database_eq(sha256))) == 0) {
    database_model_insert(connection, "pdf_file", list(pdf_sha256 = sha256, pdf_pages = pages, pdf_bytes = bytes), select = "pdf_sha256")
  }
  invisible()
}

# A PDF on this server, fetched from Storage first if this server hasn't got it.
# sha256: The PDF's SHA-256, as the record keeps it.
# Returns: Where it is on this server, or `NULL` if Storage hasn't got it either (it was uploaded
#   before PDFs were kept there, and the server that had it has lost it). An error with a sentence
#   if Storage couldn't be asked.
extraction_model_local_pdf <- function(connection, sha256) {
  if (length(sha256) != 1 || is.na(sha256) || !nzchar(sha256)) {
    return(NULL)
  }
  path <- extraction_model_pdf_path(sha256)
  if (file.exists(path)) {
    return(path)
  }
  dir.create(extraction_model_pdf_dir(), recursive = TRUE, showWarnings = FALSE)
  if (storage_model_download(connection, storage_pdf_bucket, paste0(sha256, ".pdf"), path)) path
}

# Remove a PDF nothing uses any more: its row, its copy in Storage and this server's.
# A PDF is in use while an extraction (as its questionnaire or its file of MATR IDs and codes) or a wave
# names it.
# sha256: The PDF's SHA-256, or `NULL`.
# Returns: `TRUE` if it was removed, `FALSE` if something still uses it (or there is none), invisibly.
extraction_model_forget_pdf <- function(connection, sha256) {
  if (length(sha256) != 1 || is.na(sha256) || !nzchar(sha256)) {
    return(invisible(FALSE))
  }
  used <- database_model_count(connection, extraction_table, list(pdf_sha256 = database_eq(sha256))) +
    database_model_count(connection, extraction_table, list(`supplement->>sha256` = database_eq(sha256))) +
    database_model_count(connection, "wave", list(pdf_sha256 = database_eq(sha256))) +
    database_model_count(connection, "wave", list(supplement_sha256 = database_eq(sha256)))
  if (used > 0) {
    return(invisible(FALSE))
  }
  database_model_delete(connection, "pdf_file", list(pdf_sha256 = database_eq(sha256)), select = "pdf_sha256")
  storage_model_delete(connection, storage_pdf_bucket, paste0(sha256, ".pdf"))
  unlink(extraction_model_pdf_path(sha256))
  invisible(TRUE)
}

# The SHA-256 of a file.
# It names the file's copy in the cache and says it's the same PDF.
# Returns: The hash, as a string.
extraction_model_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

# The text of an uploaded CSV.
# Returns: Its lines joined by newlines, read as UTF-8.
extraction_model_read_csv <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# ------ Reading a PDF's first bytes ----------------------------------------------
# Whether a file starts like a PDF.
# Returns: `TRUE` if it begins `%PDF-`.
extraction_model_is_pdf <- function(path) {
  identical(readBin(path, "raw", 5), charToRaw("%PDF-"))
}

# Count a PDF's pages.
# Counts the page objects the PDF lists in plain view. PDFs that pack them into
# compressed streams show none.
# path: The PDF.
# Returns: The count, or `NULL` (unknown, not 0) if none were found.
extraction_model_page_count <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  n_pages <- length(grepRaw("/Type[[:space:]]*/Page[^s]", bytes, all = TRUE))
  if (n_pages > 0) n_pages
}
