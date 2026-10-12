# The AI-Powered mode of the Extraction page: upload a PDF, watch it being read, and fill in what is known
# about the questionnaire. The form's own inputs are in a module called `info`, so they are named like
# `info-wave_name`. The database is the real one; no reading job is started here (server.R does that).

# ai_page <- function(me, extractions = test_extractions()) {
#   did <- new.env()
#   did$reviews <- list()
#   did$modes <- character()
#   list(
#     did = did,
#     args = list(
#       extractions = extractions, catalog = test_live_catalog(), connection = test_connection, me = me,
#       open_review = function(record) did$reviews[[length(did$reviews) + 1]] <- record,
#       switch_mode = function(mode) did$modes <- c(did$modes, mode)
#     )
#   )
# }

# Removes the drafts this page made when the test ends.
# local_page_drafts <- function(me, env = parent.frame()) {
#   withr::defer(test_delete("draft", list(prepared_by = database_eq(me$staff_id), status = "neq.Applied")), envir = env)
# }

# test_that("an upload becomes a draft, with what was typed before it, and the page is then about it", {
#   me <- local_staff()
#   tree <- local_tree()
#   local_pdf_cache()
#   local_page_drafts(me)
#   page <- ai_page(me)
#   shiny::testServer(extraction_ai_server, args = page$args, {
#     errors <- collect_errors(session)
#     expect_null(current_id())
#     session$setInputs(
#       `info-kind` = "new_wave", `info-registry_id` = as.character(tree$registry$registry_id),
#       `info-study_id` = as.character(tree$study$study_id),
#       `info-questionnaire_id` = as.character(tree$questionnaire$questionnaire_id), `info-wave_name` = "Wave 3"
#     )
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
#     expect_length(errors(), 0)
#   })
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

# test_that("typing after the upload is saved to the draft once it pauses, and the footer says what is still needed", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   local_pdf_cache()
#   local_page_drafts(me)
#   page <- ai_page(me)
#   shiny::testServer(extraction_ai_server, args = page$args, {
#     expect_equal(text_of(output$footer_note), "Upload the PDF to start the draft.")
#     session$setInputs(`info-kind` = "new_wave")
#     session$setInputs(pdf = fake_upload())
#     expect_match(text_of(output$footer_note), "^Fill in the registry, study, questionnaire and wave name to go to review\\.$")
#     expect_match(text_of(output$save_status), "Draft saved just now")
#
#     session$setInputs(
#       `info-registry_id` = as.character(tree$registry$registry_id), `info-study_id` = as.character(tree$study$study_id),
#       `info-questionnaire_id` = as.character(tree$questionnaire$questionnaire_id), `info-wave_name` = "Wave 3"
#     )
#     expect_match(text_of(output$save_status), "Saving")
#     session$elapse(info_save_delay_ms + 100)
#     expect_equal(extraction_model_get(test_connection, current_id())$info$wave_name, "Wave 3")
#     expect_equal(text_of(output$footer_note), "When reading finishes you can go straight to review.")
#     session$flushReact()
#     expect_true(last_update(seen, "go_to_review")$disabled) # not read yet
#   })
# })

# test_that("a wave name the questionnaire has is outlined, and the footer says to fix it", {
#   me <- local_staff()
#   tree <- local_tree()
#   local_pdf_cache()
#   local_page_drafts(me)
#   page <- ai_page(me)
#   shiny::testServer(extraction_ai_server, args = page$args, {
#     session$setInputs(pdf = fake_upload())
#     session$setInputs(
#       `info-kind` = "new_wave", `info-registry_id` = as.character(tree$registry$registry_id),
#       `info-study_id` = as.character(tree$study$study_id),
#       `info-questionnaire_id` = as.character(tree$questionnaire$questionnaire_id), `info-wave_name` = tree$wave$name
#     )
#     expect_match(form$problems()$wave_name, "already has a")
#     expect_equal(text_of(output$footer_note), "Fix the fields outlined in red to go to review.")
#   })
# })

# test_that("Go to review opens once reading has finished and the details are complete, and clears the page", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- local_read_extraction(me, info = typed_new_wave(tree))
#   page <- ai_page(me)
#   shiny::testServer(extraction_ai_server, args = page$args, {
#     show_draft(first)
#     expect_equal(current_id(), first$id)
#     expect_equal(last_update(seen, "wave_name")$value, "Wave 2")
#     session$setInputs(
#       `info-kind` = "new_wave", `info-registry_id` = as.character(tree$registry$registry_id),
#       `info-study_id` = as.character(tree$study$study_id),
#       `info-questionnaire_id` = as.character(tree$questionnaire$questionnaire_id), `info-wave_name` = "Wave 2"
#     )
#     session$flushReact()
#     expect_false(last_update(seen, "go_to_review")$disabled)
#     expect_equal(text_of(output$footer_note), "Reading has finished. The draft is ready to review.")
#     expect_match(text_of(output$reading), "Finished reading")
#     session$setInputs(go_to_review = 1)
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

test_that("the mode is drawn with the drop zone, the form and the Go to review button", {
  html <- htmltools::renderTags(extraction_ai_ui("ai"))$html
  expect_match(html, 'id="ai-pdf_card"')
  expect_match(html, 'id="ai-info-wave_name"')
  expect_match(html, 'id="ai-go_to_review"')
  expect_match(html, "About Questionnaire &amp; Wave")
})
