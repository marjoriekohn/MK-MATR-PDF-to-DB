# Where an extraction goes: a questionnaire of the Adult Health Study (study 1, in registry 1) and one
# of its waves; by default AHP and its Wave 2, which asks no questions yet.
picked_info <- function(questionnaire_id = "1", wave_id = "2") {
  info <- extraction_service_info()
  info[c("registry_id", "study_id", "questionnaire_id", "wave_id")] <- list("1", "1", questionnaire_id, wave_id)
  info
}

test_that("an upload becomes a draft that starts reading straight away", {
  local_pdf_cache()
  me <- local_staff()
  record <- extraction_service_create(test_connection, fake_upload(), me, picked_info())

  saved <- extraction_service_get_all(test_connection)[[record$id]]
  expect_equal(saved$status, "Draft")
  expect_equal(saved$method, extraction_ai)
  expect_equal(saved$prepared_by, me$name)
  expect_equal(saved$extraction_status, "queued")
  expect_equal(saved$pdf$pages, 2)
  expect_true(file.exists(saved$pdf$path))
  expect_equal(saved$info$wave_id, "2")
  expect_null(saved$error)
  expect_null(saved$reading_claimed_at)
})

test_that("a file that isn't a PDF is turned away", {
  local_pdf_cache()
  me <- local_staff()
  not_pdf <- fake_upload("notes.pdf", text = "just some words")
  expect_error(extraction_service_create(test_connection, not_pdf, me), "notes.pdf isn't a PDF")
  # no draft was added for them (only theirs are looked at: other runs of the tests add drafts meanwhile)
  expect_length(extraction_service_open_drafts(extraction_service_get_all(test_connection), prepared_by_id = me$staff_id), 0)
})

test_that("a draft typed by hand starts empty, with no reading, and can have a PDF or not", {
  local_pdf_cache()
  me <- local_staff()
  without <- extraction_service_create_manual(test_connection, me, picked_info())
  expect_equal(without$method, extraction_manual)
  expect_equal(without$status, "Draft")
  expect_equal(without$prepared_by_id, me$staff_id)
  expect_null(without$extraction_status)
  expect_null(without$pdf$name)
  expect_true(extraction_service_is_read(without)) # nothing to wait for
  expect_identical(draft_service_load(test_connection, without$id), draft_service_empty())

  with <- extraction_service_create_manual(test_connection, me, picked_info(), fake_upload("typed.pdf"))
  expect_equal(with$pdf$name, "typed.pdf")
  expect_equal(with$pdf$pages, 2)
  expect_true(file.exists(with$pdf$path))
  expect_error(extraction_service_create_manual(test_connection, me, picked_info(), fake_upload("notes.pdf", "words")), "isn't a PDF")
})

test_that("a draft is ready to review when it has been read, or was typed by hand", {
  expect_true(extraction_service_is_read(list(method = extraction_manual)))
  expect_true(extraction_service_is_read(list(method = extraction_ai, extraction_status = "ready")))
  expect_false(extraction_service_is_read(list(method = extraction_ai, extraction_status = "queued")))
  expect_false(extraction_service_is_read(list(method = extraction_ai, extraction_status = "failed")))
})

test_that("discarding deletes the draft, and its PDF once no other draft uses it", {
  local_pdf_cache()
  me <- local_staff()
  # a PDF of its own: the other tests' drafts share fake_upload()'s, and while one of them (in another process) has a
  # draft open with it, it is rightly kept
  upload <- fake_upload(text = paste0("%PDF-1.4\n% ", test_name("PDF"), "\n<< /Type /Page >>\n"))
  record <- extraction_service_create(test_connection, upload, me)
  same_pdf <- extraction_service_create(test_connection, upload, me)
  sha256 <- record$pdf$sha256
  pdf_rows <- function() database_model_count(test_connection, "pdf_file", list(pdf_sha256 = database_eq(sha256)))

  extraction_service_discard(test_connection, record$id, me)
  expect_error(extraction_model_get(test_connection, record$id), "no longer in the database")
  expect_false(record$id %in% names(extraction_service_get_all(test_connection)))
  expect_equal(pdf_rows(), 1) # the other draft still uses the file

  extraction_service_discard(test_connection, same_pdf$id, me)
  expect_equal(pdf_rows(), 0)
  expect_false(file.exists(extraction_model_pdf_path(sha256)))
})

test_that("Mine lists only the drafts I started", {
  local_pdf_cache()
  me <- local_staff()
  colleague <- local_staff()
  extraction_service_create(test_connection, fake_upload(), me, now = Sys.time() - 60)
  extraction_service_create(test_connection, fake_upload(), colleague)
  records <- extraction_service_get_all(test_connection)
  ours <- Filter(function(record) record$prepared_by_id %in% c(me$staff_id, colleague$staff_id), records)

  expect_length(extraction_service_open_drafts(ours), 2)
  mine <- extraction_service_open_drafts(ours, prepared_by_id = me$staff_id)
  expect_equal(unname(vapply(mine, function(record) record$prepared_by, character(1))), me$name)
})

test_that("news from a reading that has since started again is ignored", {
  local_pdf_cache()
  me <- local_staff()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  expect_false(extraction_service_reading_update(test_connection, record$id, "2026-01-01T00:00:00Z", list(extraction_status = "ready")))
  expect_equal(extraction_model_get(test_connection, record$id)$extraction_status, "queued")
})

test_that("saving a reviewed copy keeps how it differs from what the AI returned, for an AI draft only", {
  me <- local_staff()
  ai <- local_read_extraction(me)
  opened <- extraction_service_open_draft(test_connection, ai)
  expect_identical(opened$original, short_gold_draft())
  q1a <- block_numbered(opened$draft, "1a")$id
  edited <- draft_service_update(opened$draft, "components", q1a, "text", "Would you describe yourself as nervous?")
  extraction_service_save_draft(test_connection, ai, edited, opened$original, me)
  edits <- review_edits_of(test_connection, ai$id)
  expect_length(edits, 1)
  expect_equal(edits[[1]]$before, "would you call yourself a nervous person?")
  expect_equal(edits[[1]]$after, "would you describe yourself as nervous?")
  expect_equal(edits[[1]]$ref, q1a)

  # saving again, and opening the saved copy again, measure against what the AI returned: nothing piles up
  reopened <- extraction_service_open_draft(test_connection, ai)
  again <- draft_service_update(reopened$draft, "components", block_numbered(reopened$draft, "1b")$id, "text", "Do you worry?")
  extraction_service_save_draft(test_connection, ai, again, reopened$original, me)
  extraction_service_save_draft(test_connection, ai, again, reopened$original, me)
  expect_length(review_edits_of(test_connection, ai$id), 2)
  # a value put back is no difference
  restored <- draft_service_update(again, "components", block_numbered(again, "1a")$id, "text", "Would you call yourself a nervous person?")
  extraction_service_save_draft(test_connection, ai, restored, reopened$original, me)
  expect_equal(vapply(review_edits_of(test_connection, ai$id), function(edit) edit$after, character(1)), "do you worry?")
  expect_identical(draft_service_from_json(extraction_model_questionnaire(test_connection, ai$id)$original), short_gold_draft())

  manual <- local_manual_extraction(me)
  typed <- draft_service_update(short_gold_draft(), "components", block_numbered(short_gold_draft(), "1a")$id, "text", "Typed by hand")
  expect_null(extraction_service_open_draft(test_connection, manual)$original)
  extraction_service_save_draft(test_connection, manual, typed, NULL, me)
  expect_equal(review_edits_of(test_connection, manual$id), list())
  expect_identical(draft_service_load(test_connection, manual$id), typed)
})

test_that("the drafts are read again every few seconds only while one this person started is being read", {
  me <- list(staff_id = 7, name = "Maggie", role = "STAFF")
  draft <- function(prepared_by_id, extraction_status, status = "Draft") {
    list(prepared_by_id = prepared_by_id, extraction_status = extraction_status, status = status)
  }
  expect_true(extraction_service_is_reading(list(a = draft(7, "queued")), me))
  expect_true(extraction_service_is_reading(list(a = draft(7L, "received"), b = draft(8, "ready")), me))
  expect_false(extraction_service_is_reading(list(a = draft(7, "ready"), b = draft(7, "failed"), c = draft(7, NULL)), me))
  expect_false(extraction_service_is_reading(list(a = draft(8, "sent")), me)) # someone else's
  expect_false(extraction_service_is_reading(list(a = draft(7, "sent", status = "Pushed")), me)) # finished with
  expect_false(extraction_service_is_reading(list(), me))
})

test_that("reading steps follow the reading job's status", {
  states <- function(status, error = NULL) {
    extraction_service_reading_steps(list(extraction_status = status, error = error))$state
  }
  expect_equal(states("queued"), c("done", "in_progress", "not_started", "not_started", "not_started"))
  expect_equal(states("received"), c("done", "done", "done", "in_progress", "not_started"))
  expect_equal(states("ready"), rep("done", 5))

  failed <- extraction_service_reading_steps(list(extraction_status = "failed", error = "Stopped on page 6 of 18"))
  expect_equal(failed$state[3], "attention")
  expect_equal(failed$detail[3], "Stopped on page 6 of 18")
})

test_that("a file of Variable Names and Coding is a step of its own, after the questionnaire is read and checked", {
  coded <- function(status, error = NULL) {
    extraction_service_reading_steps(list(extraction_status = status, error = error, supplement = list(name = "KQ codebook.csv")))
  }
  steps <- coded("received")
  expect_equal(steps$label[4:6], c("Checking the result", "Applying Variable Names and Coding", "Saved as a draft"))
  expect_equal(steps$detail[4], "Makes sure every question has at least one variable") # checking is only checking
  expect_equal(steps$detail[5], "Applies the Variable Names and Coding in KQ codebook.csv")
  expect_equal(steps$state, c("done", "done", "done", "in_progress", "not_started", "not_started"))
  expect_equal(coded("assigning")$state, c(rep("done", 4), "in_progress", "not_started"))
  expect_equal(coded("ready")$state, rep("done", 6))
  not_applied <- coded("ready", error = "Gemini is busy")
  expect_equal(not_applied$state[5], "attention")
  expect_match(not_applied$detail[5], "Couldn't apply them \\(Gemini is busy\\)")
})

test_that("the progress on each card is how it was read and how much of it is checked, as the Edit Extraction page counts it", {
  record <- list(status = "Draft", extraction_status = "ready", info = picked_info(), matr_csv = NULL)
  draft <- with_checked(gold_draft(), gold_draft()$components$id[1:10])
  progress <- extraction_service_progress(record, draft)

  percent <- draft_service_percent_checked(draft_service_counts(draft))
  expect_equal(percent, as.integer(floor(100 * 10 / nrow(draft$components))))
  expect_equal(progress$label, c("Read by Gemini", paste0(percent, "% human-reviewed")))
  expect_equal(progress$state, c("done", "in_progress"))
  expect_false(any(grepl("MATR IDs|Submitted|Wave details", progress$label)))

  unread <- extraction_service_progress(list(status = "Draft", extraction_status = "failed", info = list(), matr_csv = NULL))
  expect_equal(unread$label, c("Read by Gemini · failed", "0% human-reviewed")) # nothing to check yet
  expect_equal(unread$state, c("attention", "not_started"))
  uncoded <- extraction_service_progress(utils::modifyList(record, list(error = "the file has no MATR IDs")), draft)
  expect_equal(uncoded$label[1], "Read by Gemini · codes not applied")
  expect_equal(uncoded$state[1], "attention")
  expect_equal(draft_service_percent_checked(list(blocks = 0, checked = 0)), 0L)
  # once read, how many pages were
  paged <- utils::modifyList(record, list(pdf = list(name = "AHP.pdf", pages = 8)))
  expect_equal(extraction_service_progress(paged, draft)$label[1], "8 pages read by Gemini")
  expect_equal(extraction_service_progress(utils::modifyList(paged, list(pdf = list(pages = 1))), draft)$label[1], "1 page read by Gemini")
  expect_equal(extraction_service_progress(utils::modifyList(paged, list(error = "no MATR IDs")), draft)$label[1], "8 pages read by Gemini · codes not applied")
})

test_that("every Basic Info pick left blank is named, in the form's order", {
  expect_equal(extraction_service_missing(extraction_service_info()), c("registry", "study", "questionnaire", "wave"))
  expect_equal(extraction_service_missing(picked_info()), character())
  expect_equal(extraction_service_missing(picked_info(wave_id = "")), "wave")
  expect_equal(extraction_service_missing_fields(picked_info(questionnaire_id = "", wave_id = "")), c("questionnaire_id", "wave_id"))
})

test_that("only the four picks are read from a record, so an old draft's new questionnaire or wave is asked for again", {
  old <- list(info = list(
    kind = "new_questionnaire", registry_id = "1", study_id = "1", name = "Twin Diet Survey", abbreviation = "TDS",
    wave_choice = "new", wave_name = "Wave 1"
  ))
  info <- extraction_service_info(old)
  expect_named(info, c("registry_id", "study_id", "questionnaire_id", "wave_id"))
  expect_equal(extraction_service_missing(info), c("questionnaire", "wave"))
  # "+ Add new …" in the form isn't a pick
  expect_equal(extraction_service_basic_info(utils::modifyList(picked_info(), list(wave_id = add_new_choice)))$wave_id, "")
})

test_that("a questionnaire's study and registry are filled in from it", {
  catalog <- test_catalog()
  blank <- picked_info("2", wave_id = "") # a draft saved before the form asked for the registry and study
  blank[c("registry_id", "study_id")] <- list("", "")
  placed <- extraction_service_place(blank, catalog)
  expect_equal(placed[c("registry_id", "study_id")], list(registry_id = "1", study_id = "2"))

  # What staff picked stays, and a questionnaire that isn't recorded fills nothing in
  expect_identical(extraction_service_place(picked_info(), catalog), picked_info())
  unknown <- picked_info("99")
  unknown[c("registry_id", "study_id")] <- list("", "")
  expect_equal(extraction_service_place(unknown, catalog)$study_id, "")
})

test_that("cards are named from where it goes, else what the AI read, else the PDF", {
  catalog <- test_catalog()
  record <- list(pdf = list(name = "scan.pdf"), info = picked_info())
  expect_equal(extraction_service_title(record, catalog), "Adult Health and Personality (AHP) · Wave 2")
  expect_equal(extraction_service_title(list(pdf = list(name = "scan.pdf"), info = list()), catalog), "scan.pdf")
  expect_equal(extraction_service_title(list(pdf = list(), info = list()), catalog), "Untitled questionnaire") # typed by hand, no PDF
  expect_equal(extraction_service_title(list(info = picked_info(wave_id = "")), catalog), "Adult Health and Personality (AHP)")
})

test_that("a recorded wave has to be the questionnaire's, ask no questions yet (else empty it first), and not be another draft's", {
  catalog <- test_catalog()
  info <- picked_info()
  expect_null(extraction_service_recorded_wave_problem(info, catalog))
  expect_equal(extraction_service_recorded_wave_problem(picked_info(wave_id = "3"), catalog), "Pick one of this questionnaire's waves.")
  expect_equal(
    extraction_service_recorded_wave_problem(info, catalog, question_counts = c("2" = 12L)),
    "Wave 2 already asks 12 questions. Empty the wave first (Metadata page), or pick a wave without questions."
  )
  other <- list(other = list(id = "other", status = "Draft", prepared_by = "D. Chen", prepared_by_id = 9L, info = info))
  pushed <- list(done = list(id = "done", status = "Pushed", prepared_by = "D. Chen", prepared_by_id = 9L, info = info))
  expect_null(extraction_service_recorded_wave_problem(info, catalog, records = pushed, id = "mine")) # finished with: not in the way
  expect_match(extraction_service_recorded_wave_problem(info, catalog, records = other, id = "mine"), "D. Chen already has a draft going into Wave 2")
  expect_match(
    extraction_service_recorded_wave_problem(info, catalog, records = other, id = "mine", me = list(staff_id = 9L, name = "D. Chen")),
    "^You already have a draft going into Wave 2"
  )
  expect_null(extraction_service_recorded_wave_problem(info, catalog, records = other, id = "other"))
  # no wave picked yet: nothing to check
  expect_null(extraction_service_recorded_wave_problem(picked_info(wave_id = ""), catalog))
  # every wave of the questionnaire is offered, saying whether it asks questions yet
  expect_equal(
    questionnaire_service_wave_choices(catalog, "1", c("1" = 3L)),
    c("Wave 1 · given 05/01/2019 · asks 3 questions: empty it first" = "1", "Wave 2 · given 04/15/2022 · no questions yet" = "2")
  )
  expect_equal(unname(questionnaire_service_wave_choices(catalog, "1")), c("1", "2"))
  expect_length(questionnaire_service_wave_choices(catalog, "99"), 0)
})

test_that("the Edit Extraction page is headed by where it goes, else the PDF's name, never what Gemini read", {
  catalog <- test_catalog()
  expect_equal(
    extraction_service_heading(list(info = picked_info()), catalog),
    list(title = "AHP · Wave 2", subtitle = "Adult Health and Personality")
  )
  expect_equal(extraction_service_heading(list(pdf = list(name = "AHP_Redacted.pdf"), info = list()), catalog), list(title = "AHP_Redacted.pdf", subtitle = ""))
  expect_equal(extraction_service_heading(list(info = list()), catalog)$title, "Unnamed questionnaire")
})

# ------ What a recorded questionnaire has already ----------------------------------------------------------
test_that("a new wave of a recorded questionnaire knows the blocks it prints and its questions' MATR IDs", {
  pushed <- shared_pushed_questionnaire()
  record <- list(info = picked_basic_info(pushed$tree, pushed$questionnaire_id))
  recorded <- extraction_service_recorded(test_connection, record)
  expect_named(recorded, c("texts", "matr_ids"))
  expect_setequal(recorded$texts$text, pushed$draft$components$text)
  expect_setequal(recorded$texts$component_type, c("Section", "Instruction", "Question"))
  expect_equal(anyDuplicated(recorded$texts), 0) # each wording once, however often it is printed
  expect_equal(nrow(recorded$matr_ids), 2)
  expect_setequal(recorded$matr_ids$matr_id, pushed$draft$variables$matr_id)
  smoke <- recorded$matr_ids[recorded$matr_ids$matr_id == pushed$draft$variables$matr_id[1], ]
  expect_equal(smoke$text, paste(test_marker, "Do you smoke?"))
  expect_equal(smoke$display_order, 1)
})

test_that("a pushed extraction is kept whole: what was read, what staff saved and where it went", {
  pushed <- shared_pushed_questionnaire()
  kept <- extraction_model_get(test_connection, pushed$id) # the record of what was read and reviewed, for the AI Monitor
  expect_equal(kept$status, "Pushed")
  expect_equal(kept$info$wave_id, pushed$typed$wave_id)
  expect_identical(draft_service_load(test_connection, pushed$id), pushed$draft)
  expect_equal(pushed$counts$questions, 2)
  expect_false(pushed$id %in% names(extraction_service_get_all(test_connection))) # not a draft any more
})

test_that("a draft with no questionnaire picked has nothing recorded", {
  tree <- local_tree()
  blank <- extraction_service_recorded(test_connection, list(info = list(questionnaire_id = "")))
  expect_equal(nrow(blank$matr_ids), 0)
  # a recorded questionnaire with no blocks yet has nothing to offer either
  empty <- extraction_service_recorded(test_connection, list(info = picked_basic_info(tree)))
  expect_equal(nrow(empty$texts), 0)
  expect_equal(nrow(empty$matr_ids), 0)
})

# ------ Re-extracting a wave ------------------------------------------------------------------------------
test_that("Re-extract on a wave empties it, then starts a new extraction from its files, into that wave", {
  me <- list(staff_id = 7, name = "Pat", role = "STAFF")
  pdf <- withr::local_tempfile(fileext = ".pdf")
  writeLines("%PDF-1.4\n<< /Type /Page >>\n", pdf)
  files <- list(
    `41` = list(wave_id = 41, pdf_sha256 = "abc", pdf_name = "AHP.pdf", supplement_name = "codes.csv", supplement_sha256 = NA, supplement_text = "AH01"),
    `42` = list(wave_id = 42, pdf_sha256 = NA, pdf_name = NA, supplement_name = NA, supplement_sha256 = NA, supplement_text = NA)
  )
  emptied <- list()
  local_stub("extraction_model_wave_files", function(connection, wave_id) files[[wave_id]])
  local_stub("extraction_model_local_pdf", function(connection, sha256) pdf)
  local_stub("extraction_model_create", function(connection, record) record) # what would be saved
  local_stub("database_model_call", function(connection, name, arguments) {
    emptied[[length(emptied) + 1]] <<- list(name = name, arguments = arguments)
    list(questions_removed = 12)
  })
  place <- list(registry_id = "1", study_id = "2", questionnaire_id = "3")

  record <- extraction_service_reextract_wave(NULL, "41", place, me)
  expect_equal(emptied, list(list(name = "empty_wave", arguments = list(p_wave_id = "41", p_staff_id = "7"))))
  expect_equal(record$method, extraction_ai)
  expect_equal(record$extraction_status, "queued") # read at once, with the prompt as it is now
  expect_equal(record$prepared_by_id, 7)
  expect_equal(record$pdf[c("name", "sha256")], list(name = "AHP.pdf", sha256 = "abc"))
  expect_equal(record$supplement, list(name = "codes.csv", text = "AH01"))
  # the same questionnaire and the same wave, which asks nothing now
  expect_equal(record$info, list(registry_id = "1", study_id = "2", questionnaire_id = "3", wave_id = "41"))

  # a wave with no PDF to read again: nothing is emptied
  expect_error(extraction_service_reextract_wave(NULL, "42", place, me), "wasn't extracted from a PDF")
  expect_error(extraction_service_reextract_wave(NULL, "43", place, me), "wasn't extracted from a PDF")
  expect_length(emptied, 1)
})

test_that("Re-extract on a real wave leaves it asking nothing, with a draft aimed at it being read", {
  local_pdf_cache()
  me <- local_staff()
  tree <- local_tree()
  upload <- fake_upload(text = paste0("%PDF-1.4\n% ", test_name("PDF"), "\n<< /Type /Page >>\n"))
  typed <- picked_basic_info(tree)
  record <- extraction_service_create_manual(test_connection, me, extraction_service_info(list(info = typed)), upload)
  withr::defer(test_delete_extraction(record$id))
  withr::defer(test_delete_pdf(record$pdf$sha256))
  draft_service_save(test_connection, record$id, test_questionnaire_draft(), me$staff_id)
  extraction_service_push(test_connection, extraction_model_get(test_connection, record$id), me, questionnaire_service_get_catalog(test_connection))
  asks <- function() database_model_count(test_connection, "wave_question", list(wave_id = database_eq(tree$wave$wave_id)))
  expect_equal(asks(), 2)

  place <- typed[c("registry_id", "study_id", "questionnaire_id")]
  again <- extraction_service_reextract_wave(test_connection, tree$wave$wave_id, place, me)
  withr::defer(test_delete_extraction(again$id))
  expect_equal(asks(), 0)
  expect_equal(again$status, "Draft")
  expect_equal(again$extraction_status, "queued")
  expect_equal(again$pdf$sha256, record$pdf$sha256) # the wave's PDF, kept when it was pushed
  expect_equal(again$info$wave_id, as.character(tree$wave$wave_id))
})

test_that("a wave extracted from a PDF has Re-extract on Metadata; one that wasn't has it off, saying why", {
  on <- as.character(wave_reextract_button(list(pdf_sha256 = "abc")))
  expect_match(on, 'data-action="reextract_wave"')
  expect_false(grepl("disabled", on))
  off <- as.character(wave_reextract_button(list(pdf_sha256 = NA)))
  expect_match(off, "disabled")
  expect_match(off, "wasn(&#39;|')t extracted from a PDF on this site")
  expect_match(as.character(reextract_wave_dialog(shiny::NS("x"), "AHP Wave 3")), "The wave is emptied first")
})
