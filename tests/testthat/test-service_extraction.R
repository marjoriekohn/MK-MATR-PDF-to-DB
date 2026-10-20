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
  before <- names(extraction_service_get_all(test_connection))
  expect_error(extraction_service_create(test_connection, not_pdf, me), "notes.pdf isn't a PDF")
  expect_equal(names(extraction_service_get_all(test_connection)), before)
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

test_that("giving a draft typed by hand a PDF keeps it with the draft and doesn't start a reading; an AI draft starts again", {
  local_pdf_cache()
  me <- local_staff()
  manual <- extraction_service_create_manual(test_connection, me, picked_info())
  added <- extraction_service_replace_pdf(test_connection, manual$id, fake_upload("later.pdf"), me)
  expect_equal(added$pdf$name, "later.pdf")
  expect_null(added$extraction_status)

  ai <- extraction_service_create(test_connection, fake_upload(), me, picked_info())
  extraction_service_reading_update(test_connection, ai$id, ai$reading_started, list(extraction_status = "failed", error = "Stopped"))
  replaced <- extraction_service_replace_pdf(test_connection, ai$id, fake_upload("again.pdf", "%PDF-1.4\n<< /Type /Page >>\n"), me)
  expect_equal(replaced$pdf$name, "again.pdf")
  expect_equal(replaced$extraction_status, "queued")
  expect_null(replaced$error)
  expect_false(identical(replaced$reading_started, ai$reading_started))
})

test_that("discarding keeps the draft but takes it off the open list", {
  local_pdf_cache()
  me <- local_staff()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  extraction_service_withdraw(test_connection, record$id, me)

  expect_equal(extraction_model_get(test_connection, record$id)$status, "Withdrawn")
  expect_false(record$id %in% names(extraction_service_get_all(test_connection))) # a list leaves it out
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

test_that("a MATR ID CSV is kept with the draft", {
  me <- local_staff()
  record <- local_manual_extraction(me)
  csv <- fake_csv_upload(c("q1a", "q1b"), "ids.csv")
  saved <- extraction_service_add_matr_csv(test_connection, record$id, csv, me)
  expect_equal(saved$matr_csv, list(name = "ids.csv", text = "q1a\nq1b"))
  expect_equal(extraction_model_get(test_connection, record$id)$matr_csv$name, "ids.csv")
})

test_that("saving a reviewed copy keeps a record of what the reviewer changed, for an AI draft only", {
  me <- local_staff()
  ai <- local_read_extraction(me, json = gold_json())
  base <- draft_service_load(test_connection, ai$id)
  edited <- draft_service_update(base, "components", block_numbered(base, "1a")$id, "text", "Would you describe yourself as nervous?")
  extraction_service_save_draft(test_connection, ai, edited, base, me, now = as.POSIXct("2026-10-05 09:00:00", tz = "UTC"))
  edits <- draft_model_get_record(test_connection, ai$id)$edits
  expect_length(edits, 1)
  expect_equal(edits[[1]]$before, "Would you call yourself a nervous person?")
  expect_equal(edits[[1]]$after, "Would you describe yourself as nervous?")
  expect_equal(edits[[1]]$by, me$staff_id)
  expect_equal(edits[[1]]$by_name, me$name)
  expect_equal(edits[[1]]$at, "2026-10-05T09:00:00Z")
  # the next save adds to the list; what the AI returned is still as it was
  again <- draft_service_update(edited, "components", block_numbered(base, "1b")$id, "text", "Do you worry?")
  extraction_service_save_draft(test_connection, ai, again, edited, me)
  expect_length(draft_model_get_record(test_connection, ai$id)$edits, 2)
  expect_identical(draft_service_from_json(draft_model_get_record(test_connection, ai$id)$original), gold_draft())

  manual <- local_manual_extraction(me)
  typed <- draft_service_update(gold_draft(), "components", block_numbered(gold_draft(), "1a")$id, "text", "Typed by hand")
  extraction_service_save_draft(test_connection, manual, typed, gold_draft(), me)
  expect_equal(draft_model_get_record(test_connection, manual$id)$edits, list())
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
  expect_false(extraction_service_is_reading(list(a = draft(7, "sent", status = "Discarded")), me)) # set aside
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

test_that("the progress on each card is how it was read and how much of it is checked, as the Edit Extraction page counts it", {
  record <- list(status = "Sent Back", extraction_status = "ready", info = picked_info(), matr_csv = NULL)
  draft <- with_checked(gold_draft(), gold_draft()$components$id[1:10])
  progress <- extraction_service_progress(record, draft)

  percent <- draft_service_percent_checked(draft_service_counts(draft))
  expect_equal(percent, as.integer(floor(100 * 10 / nrow(draft$components))))
  expect_equal(progress$label, c("Read by Gemini", paste0(percent, "% human-reviewed")))
  expect_equal(progress$state, c("done", "in_progress"))
  expect_false(any(grepl("MATR IDs|Submitted|Wave details", progress$label)))

  unread <- extraction_service_progress(list(status = "Draft", extraction_status = "failed", info = list(), matr_csv = NULL))
  expect_equal(unread$label, c("Read by Gemini · failed", "Structure"))
  expect_equal(unread$state, c("attention", "not_started"))
  expect_equal(draft_service_percent_checked(list(blocks = 0, checked = 0)), 0L)
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
  expect_equal(extraction_service_title(list(pdf = list(name = "scan.pdf"), info = list()), catalog, list(name = "Sleep Diary")), "Sleep Diary")
  expect_equal(extraction_service_title(list(pdf = list(), info = list()), catalog), "Untitled questionnaire") # typed by hand, no PDF
  expect_equal(extraction_service_title(list(info = picked_info(wave_id = "")), catalog), "Adult Health and Personality (AHP)")
})

test_that("a recorded wave has to be the questionnaire's, ask no questions yet, and not be another draft's", {
  catalog <- test_catalog()
  info <- picked_info()
  expect_null(extraction_service_recorded_wave_problem(info, catalog))
  expect_equal(extraction_service_recorded_wave_problem(picked_info(wave_id = "3"), catalog), "Pick one of this questionnaire's waves.")
  expect_equal(
    extraction_service_recorded_wave_problem(info, catalog, question_counts = c("2" = 12L)),
    "Wave 2 already asks 12 questions. Pick a wave without questions, or add a new wave."
  )
  other <- list(other = list(id = "other", status = "Awaiting Review", prepared_by = "D. Chen", prepared_by_id = 9L, info = info))
  expect_match(extraction_service_recorded_wave_problem(info, catalog, records = other, id = "mine"), "D. Chen already has a draft going into Wave 2")
  expect_match(
    extraction_service_recorded_wave_problem(info, catalog, records = other, id = "mine", me = list(staff_id = 9L, name = "D. Chen")),
    "^You already have a draft going into Wave 2"
  )
  expect_null(extraction_service_recorded_wave_problem(info, catalog, records = other, id = "other"))
  # no wave picked yet: nothing to check
  expect_null(extraction_service_recorded_wave_problem(picked_info(wave_id = ""), catalog))
  expect_equal(questionnaire_service_empty_wave_choices(catalog, "1", c("1" = 3L)), c("Wave 2 · given 04/15/2022 · no questions yet" = "2"))
})

test_that("the Edit Extraction page is headed by where it goes, else what Gemini read", {
  catalog <- test_catalog()
  read <- list(name = "Printed Title", abbreviation = "PT")
  expect_equal(
    extraction_service_heading(list(info = picked_info()), catalog, read),
    list(title = "AHP · Wave 2", subtitle = "Adult Health and Personality")
  )
  expect_equal(extraction_service_heading(list(info = list()), catalog, read), list(title = "PT", subtitle = "Printed Title"))
  expect_equal(extraction_service_heading(list(info = list()), catalog)$title, "Unnamed questionnaire")
})

test_that("MATR IDs are read from the CSV's ID column, with or without a heading", {
  expect_equal(extraction_service_matr_ids("q1a\nq1b\n q1c \n"), c("q1a", "q1b", "q1c"))
  expect_equal(extraction_service_matr_ids("\ufeffMATR ID\nAH1A\n\nAH1B"), c("AH1A", "AH1B"))
  expect_equal(extraction_service_matr_ids("question,matr_id\n1a,AH1A\n1b,AH1B"), c("AH1A", "AH1B"))
  expect_equal(extraction_service_matr_ids("NA\nAH2"), c("NA", "AH2"))
  expect_equal(extraction_service_matr_ids(""), character())

  gna <- extraction_model_read_csv(fixture_path("gna_matr_ids.csv")) # CRLF, no heading
  expect_length(extraction_service_matr_ids(gna), 251)
  expect_equal(head(extraction_service_matr_ids(gna), 1), "q1a")
})

# ------ What a recorded questionnaire has already ----------------------------------------------------------
test_that("a new wave of a recorded questionnaire knows the blocks it prints and its questions' MATR IDs", {
  admin <- local_staff("ADMIN")
  tree <- local_tree()
  pushed <- local_pushed_questionnaire(admin, tree)
  record <- list(info = picked_basic_info(tree, pushed$questionnaire_id))
  recorded <- extraction_service_recorded(test_connection, record)
  expect_named(recorded, c("texts", "matr_ids"))
  expect_setequal(recorded$texts$text, pushed$draft$components$text)
  expect_setequal(recorded$texts$component_type, c("Section", "Instruction", "Question"))
  expect_equal(nrow(recorded$matr_ids), 2)
  expect_setequal(recorded$matr_ids$matr_id, pushed$draft$variables$matr_id)
  smoke <- recorded$matr_ids[recorded$matr_ids$matr_id == pushed$draft$variables$matr_id[1], ]
  expect_equal(smoke$text, paste(test_marker, "Do you smoke?"))
  expect_equal(smoke$display_order, 1)
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
