# ------ Extractions for the tests ---------------------------------------------------------------------------
# An extraction record as the service makes it before saving (no id yet), prepared by `staff`. Its PDF's
# hash is different each time, so two drafts never share a file in the cache.
sample_extraction <- function(staff, ...) {
  record <- list(
    status = "Draft", method = extraction_ai, prepared_by = staff$name, prepared_by_id = staff$staff_id,
    started = "2026-10-02T10:00:00Z", saved = "2026-10-02T10:00:00Z",
    pdf = list(name = "AHP_wave3.pdf", path = NULL, pages = 2L, bytes = 51L, sha256 = test_sha256()),
    extraction_status = "queued", reading_started = "2026-10-02T10:00:00Z",
    reading_claimed_at = NULL, reading_pid = NULL, error = NULL,
    info = list(questionnaire_id = "1", wave_id = "2"),
    matr_csv = NULL
  )
  utils::modifyList(record, list(...), keep.null = TRUE)
}

# A SHA-256 no real file has, different every time: a PDF's row (pdf_file) needs one in its shape.
test_sha256 <- function() digest::digest(paste(test_marker, test_counter()), algo = "sha256", serialize = FALSE)

# An extraction saved in the database for the test, and removed when it ends (its PDF's row too). Its PDF is
# never uploaded (its hash is made up), so there is nothing in Storage to remove. `detail` are more columns
# to write after it is made (what the AI read, say).
local_extraction <- function(staff, ..., detail = list(), env = parent.frame()) {
  sample <- sample_extraction(staff, ...)
  if (!is.null(sample$pdf$sha256)) {
    local_row("pdf_file", list(pdf_sha256 = sample$pdf$sha256, pdf_pages = sample$pdf$pages, pdf_bytes = sample$pdf$bytes), "pdf_sha256", env)
  }
  record <- extraction_model_create(test_connection, sample)
  withr::defer(test_delete_extraction(record$id), envir = env)
  if (length(detail) > 0) {
    database_model_update(test_connection, extraction_table, detail, filters = list(extraction_id = database_eq(record$id)), select = "extraction_id")
  }
  extraction_model_get(test_connection, record$id)
}

# A draft that has been read: the hand-checked reading (`json`) is saved with it, so its questionnaire
# can be opened on the Edit Extraction page. It is the gold questionnaire's first two parts unless given:
# all of it is a large file to send and read back for a test that doesn't look inside it.
local_read_extraction <- function(staff, ..., json = short_gold_json(), env = parent.frame()) {
  local_extraction(staff, extraction_status = "ready", ..., detail = list(ai_original = json), env = env)
}

# A draft typed by hand, saved in the database for the test and removed when it ends. `...` are its Basic
# Info picks.
local_manual_extraction <- function(staff, ..., env = parent.frame()) {
  typed <- utils::modifyList(list(questionnaire_id = "1", wave_id = "2"), list(...))
  record <- extraction_service_create_manual(test_connection, staff, extraction_service_info(list(info = typed)))
  withr::defer(test_delete_extraction(record$id), envir = env)
  record
}

# ------ A questionnaire that is recorded ------------------------------------------------------------
# A small questionnaire for the test: a section holding an instruction, a question with two choices and a
# write-in question whose number is in a unit. Every block's words, answer choice and tag carry the run's
# marker, so what a push adds can be told from real data and is removed when the run ends
# (test_forget_marked()). `matr` are the two variables' MATR IDs (marked too); NA leaves them blank.
test_questionnaire_draft <- function(matr = c("zz1", "zz2")) {
  draft <- draft_service_from_json(list(
    questionnaire = list(name = "Small", abbreviation = "SM"),
    components = list(list(
      component_type = "Section", display_order = 1, text = paste(test_marker, "Part A"),
      children = list(
        list(component_type = "Instruction", display_order = 1, text = paste(test_marker, "Please answer.")),
        list(
          component_type = "Question", display_order = 2, text = paste(test_marker, "Do you smoke?"), printed_number = "1",
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
          variables = list(list(
            variable_type = "open_ended", display_order = 1, answer_data_type = "NUMBER", unit = paste(test_marker, "cigarettes")
          ))
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

# A questionnaire pushed into the database, so it is recorded with its blocks and MATR IDs.
# What the push added into the tree goes with the tree when the test ends; the extraction with its staff.
# staff: Who pushes it, from `local_staff()`.
# tree: The registry and study it goes in, from `local_tree()`.
# draft: The questionnaire; `test_questionnaire_draft()` by default.
# Returns: A list: `questionnaire_id` (as text), `draft`, `typed` (its Basic Info), `id` (the extraction's),
#   `counts` (what the push did) and `record` (the extraction, Pushed).
local_pushed_questionnaire <- function(staff, tree, draft = test_questionnaire_draft(), env = parent.frame()) {
  typed <- picked_basic_info(tree)
  record <- do.call(local_manual_extraction, c(list(staff, env = env), typed))
  draft_service_save(test_connection, record$id, draft, staff$staff_id)
  record <- extraction_model_get(test_connection, record$id)
  outcome <- extraction_service_push(test_connection, record, staff, questionnaire_service_get_catalog(test_connection))
  list(
    questionnaire_id = typed$questionnaire_id, draft = draft, typed = typed, id = record$id,
    counts = outcome$counts, record = outcome$record
  )
}

# The differences from what the AI returned that an extraction keeps (extraction.review_edits); empty for a
# Manual extraction, which keeps none.
review_edits_of <- function(connection, id) {
  rows <- database_model_select(connection, extraction_table, "review_edits", filters = list(extraction_id = database_eq(id)))
  if (length(rows) > 0) rows[[1]]$review_edits %||% list() else list()
}

# A questionnaire pushed once for a test file and shared by its tests that only read it.
# It goes into a tree of its own (not shared_tree(), whose questionnaire other tests read as empty), by a staff
# member of its own: a pooled one's extractions are removed after each test. All of it is removed when the
# file ends. A test that changes what was pushed pushes its own.
# Returns: As `local_pushed_questionnaire()`, with the `tree` it went into.
shared_pushed_questionnaire <- function(env = parent.frame()) {
  if (!is_test_body(env)) {
    return(local_pushed_questionnaire(local_staff(env = env), local_tree(env = env), env = env))
  }
  file_shared(env, "pushed", function(file_env) {
    tree <- local_tree(env = file_env)
    pushed <- local_pushed_questionnaire(new_staff("STAFF", TRUE, NULL, env = file_env), tree, env = file_env)
    c(pushed, list(tree = tree))
  })
}

# An AI-Powered extraction, pushed: what the AI returned is `test_questionnaire_draft()`; the reviewer reworded
# question 1 and changed question 2's unit, and pushed it, so it is Pushed with those two corrections as its
# review_edits. What the push added goes with the tree when the test ends.
# staff: Who pushes it, from `local_staff()`.
# tree: The registry and study it goes in, from `local_tree()`.
# Returns: A list: `id` (the extraction's, as text) and `record` (as pushed).
local_pushed_ai_extraction <- function(staff, tree, env = parent.frame()) {
  json <- draft_service_to_json(test_questionnaire_draft())
  record <- local_extraction(
    staff, extraction_status = "ready", info = picked_basic_info(tree), detail = list(ai_original = json), env = env
  )
  opened <- extraction_service_open_draft(test_connection, record)
  reviewed <- opened$draft
  reviewed$components$text[reviewed$components$printed_number %in% "1"] <- paste(test_marker, "Do you smoke now?")
  reviewed$variables$unit[reviewed$variables$variable_type %in% "open_ended"] <- paste(test_marker, "cigars")
  extraction_service_save_draft(test_connection, record, reviewed, opened$original, staff)
  record <- extraction_model_get(test_connection, record$id)
  outcome <- extraction_service_push(test_connection, record, staff, questionnaire_service_get_catalog(test_connection))
  list(id = record$id, record = outcome$record)
}
