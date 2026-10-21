# ------ Extraction drafts: the questionnaire ---------------------------------------
# Three big jsonb columns of the draft's details (model_draft_row.R), kept apart from the rest
# of the record (model_extraction.R) because they are big:
#   questionnaire the reviewed copy staff save; empty until they first save (EXTRACTED_DRAFT)
#   ai_original   the questionnaire JSON the AI returned (after reading_service_tidy()), written once
#                 by the reading job and never changed afterwards (AI_EXTRACTED_DRAFT: AI-Powered
#                 extractions only)
#   review_edits  how the reviewed copy differs from what the AI returned, worked out again and
#                 replaced each time staff save (AI_EXTRACTED_DRAFT; see service_draft_record.R)

#' A draft's questionnaire.
#'
#' The questionnaire as staff last saved it or, if they haven't yet, as the reading
#' job returned it (or empty, for an extraction typed by hand).
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @return The questionnaire JSON as nested lists, or `NULL` if reading hasn't finished.
#'   An error if there is no such draft.
draft_model_get <- function(connection, id) {
  json <- draft_model_get_both(connection, id)
  json$questionnaire %||% json$original
}

#' A draft's reviewed copy and what the AI returned, read at once.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @return A list: `questionnaire` (the copy staff last saved, or `NULL`) and `original` (the
#'   questionnaire JSON the AI returned, or `NULL`). An error if there is no such draft.
draft_model_get_both <- function(connection, id) {
  rows <- draft_model_detail(connection, id, "questionnaire", "ai_original")
  list(questionnaire = rows$questionnaire, original = rows$ai$ai_original)
}

# `columns` of the draft's EXTRACTED_DRAFT row, and `ai_columns` of its AI_EXTRACTED_DRAFT row as
# `ai` (NULL for a Manual extraction).
draft_model_detail <- function(connection, id, columns, ai_columns) {
  embedded <- sprintf("ai:%s(%s)", ai_extracted_draft_table, paste(ai_columns, collapse = ","))
  rows <- draft_row_model_details(connection, extracted_draft_table, id, c(columns, embedded))[[as.character(id)]]
  if (is.null(rows)) stop("That draft is no longer in the database.", call. = FALSE)
  rows$ai <- draft_row_one(rows$ai)
  rows
}

#' Save the questionnaire as staff reviewed it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param draft_json The questionnaire JSON, as nested lists.
#' @param staff_id The staff id of the person saving.
#' @param review_edits How the copy differs from what the AI returned (an AI-Powered extraction), as a
#'   list that replaces what was kept, or `NULL` to leave it as it is.
#' @param expected_revision The revision the person was looking at; the save is refused if someone
#'   saved the draft since. `NULL` skips the check.
#' @return The draft's revision after the save, invisibly. An error if there is no such draft, or
#'   it has moved on from `expected_revision`.
draft_model_save_reviewed <- function(connection, id, draft_json, staff_id, review_edits = NULL, expected_revision = NULL) {
  detail <- list(questionnaire = draft_json)
  if (!is.null(review_edits)) detail$review_edits <- review_edits
  invisible(draft_row_model_save(connection, id, staff_id, detail = detail, expected_revision = expected_revision))
}

#' Save what the reading job returned.
#'
#' It is only kept if the draft is still being read for `reading_started`, so a job
#' that was overtaken by a new PDF or "Try reading again" saves nothing.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param draft_json The questionnaire JSON the job built, as nested lists.
#' @param reading_started The reading's start time, as the draft records it.
#' @return Whether it was kept.
draft_model_save_reading <- function(connection, id, draft_json, reading_started) {
  row <- draft_row_model_update_detail(
    connection, ai_extracted_draft_table, id, list(ai_original = draft_json),
    where = list(reading_started = database_eq(reading_started))
  )
  !is.null(row)
}
