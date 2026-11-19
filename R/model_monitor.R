# ------ AI Monitor: the pushed AI-Powered extractions ---------------------------------------------
# How well the AI reads questionnaires is measured on the extractions that were pushed: the database keeps
# each as it was pushed (extraction_touch() in sql/4_functions.sql), with what the AI returned
# (extraction.ai_original) and how the reviewed copy differs from it (extraction.review_edits, worked out by
# the site each time staff saved). This reads the differences, not the questionnaires, which are big. A
# Manual extraction has neither and is left out.
#
# A record's fields:
#   id              the extraction's id, as text
#   summary         what staff said it was
#   pdf_name        the PDF it was read from
#   applied         when it was pushed, as the database sends times
#   model, prompt_version   which model and which prompt read it
#   edits           its corrections: a list, one entry per difference (see the top of service_draft_record.R)

monitor_columns <- c("extraction_id", "summary", "pdf_name", "pushed_at", "prompt_version", "model", "review_edits", "info")

# Every pushed AI-Powered extraction, oldest pushed first.
# Returns: A list of records (see the top of this file), named by id; empty if there are none.
monitor_model_get_pushed <- function(connection) {
  rows <- database_model_select(
    connection, "extraction", monitor_columns,
    order_by = c("pushed_at.asc", "extraction_id.asc"),
    filters = list(status = database_eq("Pushed"), method = database_eq("AI-Powered"))
  )
  # A wave reopened and pushed again (`public.reopen_wave()`) carries the reading it was first pushed from, which
  # is counted there: counted again it would be one reading measured twice.
  rows <- Filter(function(row) is.null(row$info$updates_wave_id), rows)
  records <- lapply(rows, monitor_model_record)
  stats::setNames(records, vapply(records, function(record) record$id, character(1)))
}

monitor_model_record <- function(row) {
  list(
    id = as.character(row$extraction_id),
    summary = row$summary %||% "",
    pdf_name = row$pdf_name %||% "",
    applied = row$pushed_at,
    model = row$model %||% NA_character_,
    prompt_version = row$prompt_version %||% NA_character_,
    edits = row$review_edits %||% list()
  )
}
