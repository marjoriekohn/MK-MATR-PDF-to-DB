# The AI Monitor's model: the pushed AI-Powered extractions, with the corrections staff made (the review_edits
# the database keeps once an extraction is pushed). The rows are made here as the site and the push leave
# them (a test of the push is tests/database/), so what is tested is reading them back.

# A PDF's row, for the extractions that were read from one.
local_monitor_pdf <- function(env = parent.frame()) {
  sha256 <- digest::digest(test_name("pdf"), algo = "sha256") # 64 hex digits, different every time
  local_row("pdf_file", list(pdf_sha256 = sha256, pdf_pages = 2, pdf_bytes = 1000), "pdf_sha256", env)
}

# An extraction's row, Pushed unless said otherwise, AI-Powered with its PDF and reading unless Manual.
local_monitor_extraction <- function(staff, pdf = NULL, status = "Pushed", method = "AI-Powered", review_edits = list(), pushed_at = NULL, info = NULL, env = parent.frame()) {
  pushed <- if (status == "Pushed") pushed_at %||% format_time(Sys.time())
  row <- list(
    method = method, status = status, prepared_by = staff$staff_id, saved_by = staff$staff_id, summary = test_name("Extraction"),
    pdf_sha256 = pdf$pdf_sha256, pdf_name = if (!is.null(pdf)) "AHP_wave3.pdf", reading_status = if (method == "AI-Powered") "ready",
    model = "gemini-3.8-flash", prompt_version = "abc123", review_edits = review_edits, pushed_at = pushed
  )
  row$info <- info # (only when given: the column can't be null, and has its own default)
  local_row("extraction", row, "extraction_id", env)
}

test_that("a pushed AI-Powered extraction is listed with its corrections, its prompt and when it was pushed", {
  me <- local_staff()
  pdf <- local_monitor_pdf()
  edits <- list(
    list(change = "edited", item = "block", ref = "c1", where = "Question 1", field = "text", before = "how old", after = "how old are you?"),
    list(change = "edited", item = "variable", ref = "v1", where = "Question 2 > variable 1", field = "unit", before = NULL, after = "cigarettes")
  )
  pushed <- local_monitor_extraction(me, pdf, review_edits = edits)

  record <- monitor_model_get_pushed(test_connection)[[as.character(pushed$extraction_id)]]
  expect_equal(record$id, as.character(pushed$extraction_id))
  expect_equal(record$summary, pushed$summary)
  expect_equal(record$pdf_name, "AHP_wave3.pdf")
  expect_equal(record$model, "gemini-3.8-flash")
  expect_equal(record$prompt_version, "abc123")
  expect_lt(abs(as.numeric(difftime(Sys.time(), parse_time(record$applied), units = "mins"))), 5)
  kinds <- vapply(record$edits, function(edit) paste(edit$change, edit$item, edit$field), character(1))
  expect_setequal(kinds, c("edited block text", "edited variable unit"))
  unit <- record$edits[[which(kinds == "edited variable unit")]]
  expect_null(unit$before)
  expect_equal(unit$after, "cigarettes")
})

test_that("a pushed extraction says the abbreviation of the questionnaire it was pushed into", {
  me <- local_staff()
  pdf <- local_monitor_pdf()
  tree <- local_tree()
  pushed <- local_monitor_extraction(me, pdf, info = list(questionnaire_id = as.character(tree$questionnaire$questionnaire_id)))
  unknown <- local_monitor_extraction(me, pdf)

  records <- monitor_model_get_pushed(test_connection)
  expect_equal(records[[as.character(pushed$extraction_id)]]$questionnaire, tree$questionnaire$abbreviation)
  expect_equal(records[[as.character(unknown$extraction_id)]]$questionnaire, "") # where it went isn't known
})

test_that("an extraction still being prepared, and a Manual one that was pushed, aren't listed; the oldest pushed comes first", {
  me <- local_staff()
  pdf <- local_monitor_pdf()
  draft <- local_monitor_extraction(me, pdf, status = "Draft")
  manual <- local_monitor_extraction(me, method = "Manual")
  older <- local_monitor_extraction(me, pdf, pushed_at = "2026-01-02T10:00:00Z")
  newer <- local_monitor_extraction(me, pdf, pushed_at = "2026-01-03T10:00:00Z")

  records <- monitor_model_get_pushed(test_connection)
  expect_false(as.character(draft$extraction_id) %in% names(records))
  expect_false(as.character(manual$extraction_id) %in% names(records))
  expect_lt(match(as.character(older$extraction_id), names(records)), match(as.character(newer$extraction_id), names(records)))
  applied <- parse_time(vapply(records, function(record) record$applied, character(1)))
  expect_false(is.unsorted(applied))
  expect_equal(records[[as.character(older$extraction_id)]]$edits, list()) # pushed as the AI read it
})

test_that("a reading pushed again is counted once, under its first push, with the corrections of its latest push", {
  edit <- function(item, field, before, after) {
    list(change = "edited", item = item, ref = "c1", where = "Question 1", field = field, before = before, after = after)
  }
  row <- function(id, pushed_at, edits, info = list(), prompt = "abc123") {
    list(
      extraction_id = id, summary = paste("Extraction", id), pdf_name = "kq.pdf", pushed_at = pushed_at,
      prompt_version = prompt, model = "gemini-3.8-flash", review_edits = edits, info = info
    )
  }
  name <- edit("questionnaire", "name", NULL, "Kid Questionnaire")
  reworded <- edit("block", "text", "how old", "how old are you?")
  time_frame <- edit("block", "time_frame", "past year", "lifetime")
  first <- row(1, "2026-11-13T07:50:00Z", list(name, reworded, time_frame))
  other <- row(4, "2026-11-14T09:00:00Z", list(edit("block", "text", "a", "b")))
  # reopened and pushed again with the reworded question put back as the AI read it; its name is the database's
  second <- row(2, "2026-11-14T10:00:00Z", list(edit("questionnaire", "name", NULL, "kid questionnaire"), time_frame),
    info = list(updates_wave_id = "9", reopened_from = "1"), prompt = "zzz999"
  )

  readings <- monitor_model_readings(list(first, other, second))
  expect_equal(vapply(readings, function(record) record$id, character(1)), c("1", "4")) # each reading once
  expect_equal(readings[[1]]$edits, list(name, time_frame)) # put back, the reworded question no longer counts
  expect_equal(readings[[1]]$latest_id, "2")
  expect_equal(readings[[1]][c("applied", "prompt_version")], list(applied = "2026-11-13T07:50:00Z", prompt_version = "abc123")) # read then, by that prompt
  expect_null(readings[[2]]$latest_id)

  # reopened from the reopened one, with everything put back: as the AI read it
  third <- row(3, "2026-11-15T10:00:00Z", list(), info = list(updates_wave_id = "9", reopened_from = "2"))
  expect_equal(monitor_model_readings(list(first, other, second, third))[[1]]$edits, list(name))
  # one whose first push isn't among them, or whose trail goes round in a circle, is left out
  lost <- row(5, "2026-11-16T10:00:00Z", list(reworded), info = list(updates_wave_id = "8", reopened_from = "77"))
  circle <- list(
    row(6, "2026-11-16T11:00:00Z", list(reworded), info = list(updates_wave_id = "7", reopened_from = "7")),
    row(7, "2026-11-16T12:00:00Z", list(reworded), info = list(updates_wave_id = "7", reopened_from = "6"))
  )
  left <- monitor_model_readings(c(list(first, lost), circle))
  expect_equal(vapply(left, function(record) record$id, character(1)), "1")
  expect_equal(left[[1]]$edits, first$review_edits)
})
