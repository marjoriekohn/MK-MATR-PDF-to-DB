# A questionnaire laid out to read, as the Change Requests dialog shows a reviewer an extraction.

test_that("the questionnaire is laid out section by section, each question numbered with its variables and choices", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  html <- htmltools::renderTags(questionnaire_preview_ui(draft))$html
  text <- text_of(questionnaire_preview_ui(draft))
  sections <- draft$components[draft$components$component_type == "Section", ]
  expect_equal(lengths(regmatches(html, gregexpr("preview-section-title", html))), nrow(sections))
  questions <- draft$components[draft$components$component_type == "Question", ]
  expect_equal(lengths(regmatches(html, gregexpr("preview-question-head", html))), nrow(questions))
  expect_match(text, paste0("Q", questions$question_number[1], " ", questions$text[1]), fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr('class="preview-variable"', html))), nrow(draft$variables))
  expect_match(text, "m001")
  choices <- draft_service_children(draft, "response_options", draft$variables$id[1])
  expect_match(text, paste0(choices$answer_option[1], " (", choices$code_value[1], ")"), fixed = TRUE)
  expect_false(grepl("data-field", html)) # nothing to edit
})

test_that("blanks are said in words, and an empty questionnaire says so", {
  draft <- gold_draft()
  text <- text_of(questionnaire_preview_ui(draft))
  expect_match(text, "no MATR ID")
  expect_match(text_of(questionnaire_preview_ui(draft_service_empty())), "no blocks")
})

test_that("the change request dialog lays an extraction's questionnaire out, with its PDF when the file is here", {
  me <- list(staff_id = 1L, name = "Reviewer", role = "STAFF")
  record <- list(
    id = "7", kind = extraction_kind, summary = "Push ZZQ Wave 3", note = "", prepared_by = "Alex", prepared_by_id = 2L,
    saved_by_id = 2L, saved = format_time(Sys.time()), status = "Awaiting Review", revision = 1L
  )
  draft <- draft_service_renumber_questions(with_matr_ids())
  detail <- list(summary = "Adds 1 wave.", unchecked = 2, draft = draft, pdf_url = "session/x/file.pdf")
  html <- htmltools::renderTags(change_request_dialog(record, me, shiny::NS("cr"), detail))$html
  expect_match(html, "modal-xl")
  expect_match(html, "questionnaire-preview")
  expect_match(html, 'href="session/x/file.pdf"')
  expect_match(html, "Open PDF")
  expect_match(html, "2 blocks haven't been marked as checked yet")

  no_file <- htmltools::renderTags(change_request_dialog(record, me, shiny::NS("cr"), detail[c("summary", "unchecked", "draft")]))$html
  expect_false(grepl("Open PDF", no_file))
  unread <- htmltools::renderTags(change_request_dialog(record, me, shiny::NS("cr"), list(problem = "x", unchecked = 0)))$html
  expect_match(unread, "couldn't be read")
})
