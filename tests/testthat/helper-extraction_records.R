# ------ Extraction drafts for the tests ---------------------------------------------------------------------
# An extraction record as the service makes it before saving (no id yet), prepared by `staff`. Its PDF
# hash is different each time, so two drafts never share a file in the cache.
sample_extraction <- function(staff, ...) {
  record <- list(
    status = "Draft", method = extraction_ai, prepared_by = staff$name, prepared_by_id = staff$staff_id,
    started = "2026-10-02T10:00:00Z", saved = "2026-10-02T10:00:00Z",
    pdf = list(
      name = "AHP_wave3.pdf", path = NULL, pages = 2L, bytes = 51L,
      sha256 = paste0("zztest", test_marker, test_counter())
    ),
    extraction_status = "queued", reading_started = "2026-10-02T10:00:00Z",
    reading_claimed_at = NULL, reading_pid = NULL, error = NULL,
    info = list(questionnaire_id = "1", wave_id = "2"),
    matr_csv = NULL
  )
  utils::modifyList(record, list(...), keep.null = TRUE)
}

# An extraction draft saved in the database for the test, and removed when it ends with the other drafts of the
# test's staff (test_clear_drafts(), one request for them all). `detail` are more columns
# of its details to save with it (what the AI read, say), so it is made in one request.
local_extraction <- function(staff, ..., detail = list(), env = parent.frame()) {
  sample <- sample_extraction(staff, ...)
  withr::defer(test_delete_stored_pdf(sample$pdf$sha256), envir = env) # its row goes with the draft
  rows <- extraction_model_rows(sample, setdiff(names(sample), "requested_reviewer"))
  rows$parent$created_at <- sample$started
  if (!is.null(sample$requested_reviewer)) rows$parent$requested_reviewer <- sample$requested_reviewer$id
  id <- draft_row_model_create(test_connection, extraction_kind, sample$prepared_by_id, rows$parent, utils::modifyList(rows$detail, detail))
  extraction_model_get(test_connection, id)
}

# A draft that has been read: the hand-checked reading (`json`) is saved with it, so its questionnaire
# can be opened on the Edit Extraction page. It is the gold questionnaire's first two parts unless given:
# all of it is a large file to send and read back for a test that doesn't look inside it.
local_read_extraction <- function(staff, ..., json = short_gold_json(), env = parent.frame()) {
  local_extraction(staff, extraction_status = "ready", ..., detail = list(ai_original = json), env = env)
}

# A draft typed by hand, saved in the database for the test. `...` are its Basic Info picks.
local_manual_extraction <- function(staff, ..., env = parent.frame()) {
  typed <- utils::modifyList(list(questionnaire_id = "1", wave_id = "2"), list(...))
  record <- extraction_service_create_manual(test_connection, staff, extraction_service_info(list(info = typed)))
  record
}

# ------ A questionnaire that is recorded ------------------------------------------------------------
# A small questionnaire for the test: a section holding an instruction, a question with two choices and a
# write-in question. Every block's words carry the run's marker, so what a push adds can be told from real data.
# `matr` are the two variables' MATR IDs (marked too); NA leaves them blank.
test_questionnaire_draft <- function(matr = c("zz1", "zz2")) {
  draft <- draft_service_from_json(list(
    questionnaire = list(name = "Small", abbreviation = "SM"),
    components = list(list(
      component_type = "Section", display_order = 1, text = paste(test_marker, "Part A"),
      children = list(
        list(component_type = "Instruction", display_order = 1, text = paste(test_marker, "Please answer.")),
        list(
          component_type = "Question", display_order = 2, text = paste(test_marker, "Do you smoke?"), printed_number = "1",
          self_contained = TRUE,
          variables = list(list(
            variable_type = "closed_ended", display_order = 1,
            response_options = list(
              list(display_order = 1, answer_option = paste(test_marker, "Yes")),
              list(display_order = 2, answer_option = paste(test_marker, "No"))
            )
          ))
        ),
        list(
          component_type = "Question", display_order = 3, text = paste(test_marker, "How many?"), printed_number = "2",
          self_contained = TRUE,
          variables = list(list(variable_type = "open_ended", display_order = 1, answer_data_type = "NUMBER"))
        )
      )
    ))
  ))
  draft$variables$matr_id <- ifelse(is.na(matr), NA_character_, paste0(matr, "-", tolower(test_marker)))
  draft
}

# The Basic Info of an extraction going into the tree's questionnaire and wave (or the ones given).
picked_basic_info <- function(tree, questionnaire_id = tree$questionnaire$questionnaire_id, wave_id = tree$wave$wave_id) {
  list(
    registry_id = as.character(tree$registry$registry_id), study_id = as.character(tree$study$study_id),
    questionnaire_id = as.character(questionnaire_id), wave_id = as.character(wave_id)
  )
}

#' A questionnaire pushed to the database by an ADMIN, so it is recorded with its blocks and MATR IDs.
#'
#' What the push added is removed when the test ends, and the applied draft with its staff (see helper-00_realdb.R).
#'
#' @param admin An ADMIN, from `local_staff("ADMIN")`.
#' @param tree The registry and study it goes in, from `local_tree()`.
#' @param draft The questionnaire; `test_questionnaire_draft()` by default.
#' @param env The test's environment.
#' @return A list: `questionnaire_id` (as text), `draft`, `typed` (its Basic Info), `id` (the extraction draft's)
#'   and `history_id` (the applied draft's, as text).
local_pushed_questionnaire <- function(admin, tree, draft = test_questionnaire_draft(), env = parent.frame()) {
  typed <- picked_basic_info(tree)
  record <- extraction_service_create_manual(test_connection, admin, extraction_service_info(list(info = typed)))
  draft_service_save(test_connection, record$id, draft, admin$staff_id)
  record <- extraction_model_get(test_connection, record$id)
  outcome <- extraction_service_push(test_connection, record, admin, questionnaire_service_get_catalog(test_connection))
  history_id <- local_applied(change_request_service_add(test_connection, outcome$record, admin), env = env)
  list(questionnaire_id = typed$questionnaire_id, draft = draft, typed = typed, id = record$id, history_id = as.character(history_id))
}

# The differences from what the AI returned that a draft keeps (ai_extracted_draft.review_edits); empty for a
# Manual extraction, which keeps none.
review_edits_of <- function(connection, id) {
  draft_model_detail(connection, id, character(), "review_edits")$ai$review_edits %||% list()
}
