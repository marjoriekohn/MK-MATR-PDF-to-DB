# The AI-Powered mode of the Extraction page: upload a PDF and watch it being read, in the background; once
# it has been read it opens on the Edit Extraction page. The database is the real one; no reading job is
# started here (server.R does that).

# ai_page <- function(me, extractions = test_extractions()) {
#   did <- new.env()
#   did$reviews <- list()
#   did$modes <- character()
#   list(
#     did = did,
#     args = list(
#       extractions = extractions, connection = test_connection, me = me,
#       open_edit_extraction = function(record) did$reviews[[length(did$reviews) + 1]] <- record,
#       switch_mode = function(mode) did$modes <- c(did$modes, mode)
#     )
#   )
# }

# Removes the drafts this page made when the test ends.
# local_page_drafts <- function(me, env = parent.frame()) {
#   withr::defer(test_delete("draft", list(prepared_by = database_eq(me$staff_id), status = "neq.Applied")), envir = env)
# }

# test_that("an upload becomes a draft, and the page is then about it", {
#   me <- local_staff()
#   tree <- local_tree()
#   local_pdf_cache()
#   local_page_drafts(me)
#   page <- ai_page(me)
#   shiny::testServer(extraction_ai_server, args = page$args, {
#     errors <- collect_errors(session)
#     expect_null(current_id())
#     session$setInputs(pdf = fake_upload())
#     expect_false(is.null(current_id()))
#     expect_equal(record()$pdf$name, "AHP_wave3.pdf")
#     expect_equal(record()$extraction_status, "queued")
#     expect_equal(record()$prepared_by_id, me$staff_id)
#     expect_equal(record()$info$wave_name, "Wave 3")
#     expect_equal(record()$info$questionnaire_id, as.character(tree$questionnaire$questionnaire_id))
#     expect_equal(reading()$pdf$name, "AHP_wave3.pdf")
#     expect_match(text_of(output$pdf_card), "AHP_wave3.pdf 2 pages")
#     expect_match(text_of(output$reading), "Reading your questionnaire")
# })

# test_that("a file that isn't a PDF is turned away with a sentence, and no draft is made", {
#   me <- local_staff()
#   local_pdf_cache()
#   seen <- local_browser()
#   page <- ai_page(me)
#   shiny::testServer(extraction_ai_server, args = page$args, {
#     errors <- collect_errors(session)
#     session$setInputs(pdf = fake_upload("notes.pdf", text = "just some words"))
#     expect_null(current_id())
#     expect_match(notice_texts(seen), "notes.pdf isn't a PDF")
#     expect_length(errors(), 0)
#   })
#   expect_length(extraction_service_open_drafts(extraction_service_get_all(test_connection), prepared_by_id = me$staff_id), 0)
# })

# test_that("Open in Edit Extraction opens once reading has finished, and clears the page", {
#   me <- local_staff()
#   seen <- local_browser()
#   first <- local_read_extraction(me)
#   page <- ai_page(me)
#   shiny::testServer(extraction_ai_server, args = page$args, {
#     show_draft(first)
#     expect_equal(current_id(), first$id)
#     session$flushReact()
#     expect_false(last_update(seen, "open_extraction")$disabled)
#     expect_equal(text_of(output$footer_note), "Reading has finished. Open the whole extraction to review it, and edit it if you need to.")
#     expect_match(text_of(output$reading), "Finished reading")
#     session$setInputs(open_extraction = 1)
#     expect_equal(page$did$reviews[[1]]$id, first$id)
#     expect_null(current_id())
#   })
# })

# test_that("a reading that failed can be tried again, and the note under it points to Existing Draft", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   failed <- local_extraction(me, extraction_status = "failed", error = "Gemini stopped before the end.", info = typed_new_wave(tree))
#   page <- ai_page(me)
#   shiny::testServer(extraction_ai_server, args = page$args, {
#     show_draft(failed)
#     expect_match(text_of(output$reading), "Try reading again")
#     expect_equal(text_of(output$footer_note), "Reading stopped. Try reading again, or replace the PDF.")
#     session$setInputs(retry = 1)
#     expect_equal(record()$extraction_status, "queued")
#     session$setInputs(to_continue = 1)
#     expect_equal(page$did$modes, "drafts")
#   })
# })

# test_that("Cancel asks first, then sets the draft aside and empties the page", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- local_extraction(me, info = typed_new_wave(tree))
#   page <- ai_page(me)
#   shiny::testServer(extraction_ai_server, args = page$args, {
#     show_draft(first)
#     session$setInputs(cancel = 1)
#     expect_match(seen$modals[[1]], "Cancel this extraction\\?")
#     session$setInputs(confirm_cancel = 1)
#     expect_equal(extraction_model_get(test_connection, first$id)$status, "Withdrawn")
#     expect_null(current_id())
#     expect_match(notice_texts(seen), "Extraction cancelled.", all = FALSE)
#     expect_equal(seen$closed, 1)
#   })
# })

test_that("the mode is drawn with the drop zone, the note about reading in the background and the button that opens it", {
  html <- htmltools::renderTags(extraction_ai_ui("ai"))$html
  expect_match(html, 'id="ai-pdf_card"')
  expect_match(html, 'id="ai-open_extraction"')
  expect_match(html, "Reading happens in the background")
  expect_match(html, "Existing Draft")
  expect_false(grepl('id="ai-info-|About questionnaire|An AI reads it, and while it does', html, ignore.case = TRUE))
  manual <- htmltools::renderTags(extraction_manual_ui("manual"))$html
  expect_match(manual, 'id="manual-start"')
  expect_false(grepl("disabled", regmatches(manual, regexpr('<button[^>]*id="manual-start"[^>]*>', manual))))
  expect_false(grepl("Type the questionnaire in yourself, with no AI", manual))
})
