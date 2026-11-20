# ------ AI Monitor: the pushed AI-Powered extractions ---------------------------------------------
# How well the AI reads questionnaires is measured on the extractions that were pushed: the database keeps
# each as it was pushed (extraction_touch() in sql/4_functions.sql), with what the AI returned
# (extraction.ai_original) and how the reviewed copy differs from it (extraction.review_edits, worked out by
# the site each time staff saved). This reads the differences, not the questionnaires, which are big. A
# Manual extraction has neither and is left out.
#
# A reading is counted once, as of its latest push. A wave reopened from the Metadata page (Edit extraction,
# `public.reopen_wave()`) is a new extraction carrying the reading it was pushed from (its info's
# `reopened_from`, which may be a reopened one too), and staff may correct it further, or put a block back as the
# AI read it; pushed again, its differences from the reading are the ones that count. So each reading is listed
# under the extraction it was first pushed from, with that push's date, model and prompt (they say when and how
# it was read), and the corrections of its latest push. Its corrections to the questionnaire's name and
# abbreviation stay the first push's: a reopened draft takes those from the database, not from the reading. A
# change made on the Metadata page between the pushes shows as a correction too.
#
# A record's fields:
#   id              the extraction's id, as text
#   summary         what staff said it was
#   pdf_name        the PDF it was read from
#   applied         when it was pushed, as the database sends times
#   model, prompt_version   which model and which prompt read it
#   edits           its corrections: a list, one entry per difference (see the top of service_draft_record.R)
#   questionnaire   the abbreviation of the questionnaire it was pushed into ("KQ"); "" if that isn't known
#   latest_id       the extraction its corrections are from, when the reading was pushed again (else absent)

monitor_columns <- c("extraction_id", "summary", "pdf_name", "pushed_at", "prompt_version", "model", "review_edits", "info")

# Every reading pushed from an AI-Powered extraction, oldest pushed first, each once (see the top of this file).
# Returns: A list of records (see the top of this file), named by the id of the extraction each reading was
#   first pushed from; empty if there are none.
monitor_model_get_pushed <- function(connection) {
  rows <- database_model_select(
    connection, "extraction", monitor_columns,
    order_by = c("pushed_at.asc", "extraction_id.asc"),
    filters = list(status = database_eq("Pushed"), method = database_eq("AI-Powered"))
  )
  records <- monitor_model_name_questionnaires(connection, monitor_model_readings(rows))
  stats::setNames(records, vapply(records, function(record) record$id, character(1)))
}

# Gives each record its questionnaire's abbreviation (`questionnaire`), by the questionnaire it was pushed into
# (its info's `questionnaire_id`), read in one request.
# records: From `monitor_model_readings()`.
monitor_model_name_questionnaires <- function(connection, records) {
  ids <- unique(vapply(records, function(record) record$questionnaire_id, character(1)))
  ids <- ids[grepl("^[0-9]+$", ids)]
  found <- if (length(ids) > 0) {
    database_model_select(connection, "questionnaire", c("questionnaire_id", "abbreviation"), filters = list(questionnaire_id = database_in(ids)))
  }
  abbreviations <- stats::setNames(
    vapply(found, function(row) as.character(row$abbreviation), character(1)),
    vapply(found, function(row) as.character(row$questionnaire_id), character(1))
  )
  lapply(records, function(record) {
    record$questionnaire <- if (record$questionnaire_id %in% names(abbreviations)) abbreviations[[record$questionnaire_id]] else ""
    record$questionnaire_id <- NULL
    record
  })
}

# Each reading once: the extraction it was first pushed from, with the corrections of its latest push.
# rows: Pushed AI-Powered extractions, oldest pushed first, as the database gives them.
# Returns: A list of records, one for each extraction that isn't a reopened wave's, in the same order. A
#   reopened push whose first push isn't among `rows` is left out.
monitor_model_readings <- function(rows) {
  ids <- vapply(rows, function(row) as.character(row$extraction_id), character(1))
  reopened <- vapply(rows, function(row) !is.null(row$info$updates_wave_id), logical(1))
  from <- vapply(rows, function(row) as.character(row$info$reopened_from %||% ""), character(1))
  # the first push of each row's reading, by its place in `rows`: back through what each reopened push was
  # reopened from (NA if the trail breaks)
  first_push <- vapply(seq_along(rows), function(at) {
    seen <- integer()
    while (!is.na(at) && reopened[at] && !at %in% seen) {
      seen <- c(seen, at)
      at <- match(from[at], ids)
    }
    if (is.na(at) || reopened[at]) NA_integer_ else at
  }, integer(1))
  lapply(which(!reopened), function(at) {
    record <- monitor_model_record(rows[[at]])
    later <- which(first_push %in% at & seq_along(rows) != at)
    if (length(later) > 0) {
      latest <- monitor_model_record(rows[[max(later)]]) # pushed last: `rows` are in the order they were pushed
      of_questionnaire <- function(edit) identical(as.character(unlist(edit$item))[1], "questionnaire")
      record$edits <- c(Filter(of_questionnaire, record$edits), Filter(Negate(of_questionnaire), latest$edits))
      record$latest_id <- latest$id
    }
    record
  })
}

monitor_model_record <- function(row) {
  list(
    id = as.character(row$extraction_id),
    summary = row$summary %||% "",
    pdf_name = row$pdf_name %||% "",
    applied = row$pushed_at,
    model = row$model %||% NA_character_,
    prompt_version = row$prompt_version %||% NA_character_,
    edits = row$review_edits %||% list(),
    questionnaire_id = format(row$info$questionnaire_id %||% "", scientific = FALSE)
  )
}
