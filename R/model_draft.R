# ------ Extraction drafts: the questionnaire ---------------------------------------
# Three big jsonb columns of the draft's EXTRACTED_DRAFT row (model_draft_row.R), kept apart
# from the rest of the record (model_extraction.R) because they are big:
#   ai_original   the questionnaire JSON the AI returned, written once by the reading job and
#                 never changed afterwards (AI-Powered extractions only)
#   questionnaire the reviewed copy staff save; empty until they first save
#   review_edits  what the reviewer changed in what the AI returned, a list that grows each
#                 time they save (AI-Powered extractions only; see service_draft_record.R)

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
  rows <- draft_model_detail(connection, id, c("questionnaire", "ai_original"))
  rows$questionnaire %||% rows$ai_original
}

#' What the AI returned, and what the reviewer has changed in it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @return A list: `original` (the questionnaire JSON the AI returned, or `NULL`) and `edits`
#'   (a list of the reviewer's changes, empty if none).
draft_model_get_record <- function(connection, id) {
  rows <- draft_model_detail(connection, id, c("ai_original", "review_edits"))
  list(original = rows$ai_original, edits = rows$review_edits %||% list())
}

draft_model_detail <- function(connection, id, columns) {
  rows <- draft_row_model_details(connection, extracted_draft_table, id, columns)[[as.character(id)]]
  if (is.null(rows)) stop("That draft is no longer in the database.", call. = FALSE)
  rows
}

#' Save the questionnaire as staff reviewed it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param draft_json The questionnaire JSON, as nested lists.
#' @param staff_id The staff id of the person saving.
#' @param review_edits The reviewer's changes so far (an AI-Powered extraction), as a list, or
#'   `NULL` to leave them as they are.
#' @return `draft_json`, invisibly. An error if there is no such draft.
draft_model_save_reviewed <- function(connection, id, draft_json, staff_id, review_edits = NULL) {
  detail <- list(questionnaire = draft_json)
  if (!is.null(review_edits)) detail$review_edits <- review_edits
  draft_row_model_save(connection, id, staff_id, detail = detail)
  invisible(draft_json)
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
    connection, extracted_draft_table, id, list(ai_original = draft_json),
    where = list(reading_started = database_eq(reading_started))
  )
  !is.null(row)
}
