# The AI-Powered mode of the Extraction page: choose a PDF, begin, and watch it being read, in the background;
# once it has been read it opens on the Edit Extraction page. The database is the real one; no reading job is
# started here (server.R does that).

ai_page <- function(me, extractions = test_extractions()) {
  did <- new.env()
  did$reviews <- list()
  did$modes <- character()
  list(
    did = did,
    args = list(
      extractions = extractions, connection = test_connection, me = me,
      open_edit_extraction = function(record) {
        did$reviews[[length(did$reviews) + 1]] <- record
        TRUE # it opened
      },
      switch_mode = function(mode) did$modes <- c(did$modes, mode)
    )
  )
}

# Removes the drafts this page made when the test ends.
local_page_drafts <- function(me, env = parent.frame()) {
  withr::defer(test_delete(extraction_table, list(prepared_by = database_eq(me$staff_id), status = database_eq("Draft"))), envir = env)
}

test_that("the files chosen can be swapped until Begin Extraction, which makes the draft with them; then they can't", {
  me <- local_staff()
  local_pdf_cache()
  local_page_drafts(me)
  page <- ai_page(me)
  shiny::testServer(extraction_ai_server, args = page$args, {
    errors <- collect_errors(session)
    expect_match(output$footer_button$html, "Begin Extraction")
    expect_match(output$footer_button$html, "disabled") # no PDF yet
    expect_equal(text_of(output$footer_note), "Choose the questionnaire PDF to begin.")
    session$setInputs(pdf = fake_upload("first.pdf"))
    expect_null(current_id()) # nothing is read yet
    expect_match(text_of(output$pdf_card), "first.pdf")
    session$setInputs(replace_pdf = fake_upload(), supplement = fake_csv_upload("AH01", "codes.csv"))
    expect_match(text_of(output$pdf_card), "AHP_wave3.pdf")
    expect_match(text_of(output$supplement_box), "codes.csv")
    expect_false(grepl("disabled", output$footer_button$html))

    session$setInputs(begin = 1)
    expect_false(is.null(current_id()))
    expect_equal(record()$pdf$name, "AHP_wave3.pdf")
    expect_equal(record()$supplement$name, "codes.csv")
    expect_equal(record()$extraction_status, "queued")
    expect_equal(record()$prepared_by_id, me$staff_id)
    expect_match(text_of(output$pdf_card), "AHP_wave3.pdf 2 pages")
    expect_false(grepl("Replace", output$pdf_card$html)) # the files are the draft's now
    expect_match(text_of(output$supplement_box), "^CSV codes.csv Variable Names and Coding")
    expect_match(text_of(output$reading), "Reading your questionnaire")
    expect_match(output$footer_button$html, "Open in Edit Extraction")
    expect_match(output$footer_button$html, "disabled")
    expect_length(errors(), 0)
  })
})

test_that("a file that isn't a PDF is turned away with a sentence, and no draft is made", {
  me <- local_staff()
  local_pdf_cache()
  seen <- local_browser()
  page <- ai_page(me)
  shiny::testServer(extraction_ai_server, args = page$args, {
    errors <- collect_errors(session)
    session$setInputs(pdf = fake_upload("notes.pdf", text = "just some words"), begin = 1)
    expect_null(current_id())
    expect_match(notice_texts(seen), "notes.pdf isn't a PDF")
    expect_length(errors(), 0)
  })
  expect_length(extraction_service_open_drafts(extraction_service_get_all(test_connection), prepared_by_id = me$staff_id), 0)
})

test_that("Open in Edit Extraction opens once reading has finished, and clears the page", {
  me <- local_staff()
  seen <- local_browser()
  first <- local_read_extraction(me)
  page <- ai_page(me)
  shiny::testServer(extraction_ai_server, args = page$args, {
    show_draft(first)
    expect_equal(current_id(), first$id)
    expect_match(output$footer_button$html, "Open in Edit Extraction")
    expect_false(grepl("disabled", output$footer_button$html))
    expect_equal(text_of(output$footer_note), "Reading has finished. Open the whole extraction to review it, and edit it if you need to.")
    expect_match(text_of(output$reading), "Finished reading")
    session$setInputs(open_extraction = 1)
    expect_equal(page$did$reviews[[1]]$id, first$id)
    expect_null(current_id())
  })
})

test_that("a reading that failed can be tried again, and the note under it points to Extraction Drafts", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  failed <- local_extraction(me, extraction_status = "failed", error = "Gemini stopped before the end.", info = picked_basic_info(tree))
  page <- ai_page(me)
  shiny::testServer(extraction_ai_server, args = page$args, {
    show_draft(failed)
    expect_match(text_of(output$reading), "Try reading again")
    expect_equal(text_of(output$footer_note), "Reading stopped. Try reading again, or cancel the extraction and begin again with another PDF.")
    session$setInputs(retry = 1)
    expect_equal(record()$extraction_status, "queued")
    session$setInputs(to_continue = 1)
    expect_equal(page$did$modes, "drafts")
  })
})

test_that("Cancel asks first, then deletes the draft and empties the page", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- local_extraction(me, info = picked_basic_info(tree))
  page <- ai_page(me)
  shiny::testServer(extraction_ai_server, args = page$args, {
    show_draft(first)
    session$setInputs(cancel = 1)
    expect_match(seen$modals[[1]], "Cancel this extraction\\?")
    session$setInputs(confirm_cancel = 1)
    expect_error(extraction_model_get(test_connection, first$id), "no longer in the database")
    expect_null(current_id())
    expect_match(notice_texts(seen), "Extraction cancelled.", all = FALSE)
    expect_equal(seen$closed, 1)
  })
})

test_that("the mode is drawn with a place for the PDF, the file of Variable Names and Coding, and its buttons", {
  html <- htmltools::renderTags(extraction_ai_ui("ai"))$html
  expect_match(html, 'id="ai-pdf_card"')
  # the box the file is shown in, not the file input inside it, which is ai-supplement (one id each)
  expect_match(html, 'id="ai-supplement_box"')
  expect_false(grepl('id="ai-supplement"', html, fixed = TRUE))
  expect_match(html, 'id="ai-footer_button"')
  expect_false(grepl("Reading happens in the background", html, fixed = TRUE))
  expect_false(grepl('id="ai-info-|About questionnaire|An AI reads it, and while it does', html, ignore.case = TRUE))
  manual <- htmltools::renderTags(extraction_manual_ui("manual"))$html
  expect_match(manual, 'id="manual-start"')
  expect_false(grepl("disabled", regmatches(manual, regexpr('<button[^>]*id="manual-start"[^>]*>', manual))))
  expect_false(grepl("Type the questionnaire in yourself, with no AI", manual))
})

test_that("how long reading has taken is drawn once, for the browser to count on from, and only while reading", {
  started <- format(Sys.time() - 100, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  html <- as.character(reading_elapsed(started))
  expect_match(html, 'class="reading-elapsed"')
  seconds <- as.numeric(sub('.*data-elapsed="([0-9]+)".*', "\\1", html))
  expect_gte(seconds, 100)
  expect_lt(seconds, 160)
  expect_match(html, ">1 min [0-9]+ s<")
  expect_null(reading_elapsed(NULL))
  expect_null(reading_elapsed(NA))

  reading <- list(extraction_status = "sent", reading_started = started, pdf = list(name = "q.pdf"))
  expect_match(htmltools::renderTags(reading_card(reading, shiny::NS("ai")))$html, "reading-elapsed")
  for (status in c("ready", "failed")) {
    done <- htmltools::renderTags(reading_card(utils::modifyList(reading, list(extraction_status = status)), shiny::NS("ai")))$html
    expect_false(grepl("reading-elapsed", done), info = status)
    # Cancel is there while it can still go wrong; a draft that has been read is discarded from Extraction Drafts
    expect_equal(grepl('id="ai-cancel"', done), status == "failed", info = status)
  }
  expect_match(htmltools::renderTags(reading_card(reading, shiny::NS("ai")))$html, 'id="ai-cancel"')
})

test_that("a file is chosen with its button: nothing says to drop it", {
  ns <- shiny::NS("ai")
  for (html in c(
    htmltools::renderTags(pdf_choice(ns))$html,
    htmltools::renderTags(supplement_card(NULL, begun = FALSE, ns))$html,
    htmltools::renderTags(manual_pdf_card(NULL, shiny::NS("manual")))$html
  )) {
    expect_match(html, 'class="file-box')
    expect_match(html, 'type="file"')
    expect_no_match(html, "[Dd]rop")
  }
})
