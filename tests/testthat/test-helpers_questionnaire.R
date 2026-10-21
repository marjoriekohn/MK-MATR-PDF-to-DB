# A questionnaire laid out to read, and an extraction's change request, as the Edit Extraction page's preview
# and the Change Requests dialog show them.

test_that("the questionnaire is a table: a row for each variable with its MATR ID, its question's words and its coding", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  html <- htmltools::renderTags(questionnaire_preview_ui(draft))$html
  text <- text_of(questionnaire_preview_ui(draft))
  expect_match(html, "<th scope=\"col\" class=\"qt-id\">Variable ID</th>", fixed = TRUE)
  expect_match(html, "Question Text")
  expect_match(html, "Variable Coding")
  groups <- draft$components[draft$components$component_type == "Question Group", ]
  expect_equal(lengths(regmatches(html, gregexpr('class="qt-section is-group"', html))), nrow(groups))
  expect_equal(lengths(regmatches(html, gregexpr('class="qt-question"', html))), nrow(draft$variables))
  # the question's words, without its number, then who the variable is about in square brackets (nothing
  # for the respondent)
  questions <- draft$components[draft$components$component_type == "Question", ]
  first <- draft$variables[draft$variables$component_id == questions$id[1], ][1, ]
  expect_true(is.na(first$subject))
  expect_match(text, paste0(first$matr_id, " ", questions$text[1], " 1 = Yes"), fixed = TRUE)
  children <- draft$variables[draft$variables$subject %in% "Respondent's Children", ][1, ]
  expect_match(text, paste0(children$matr_id, " "), fixed = TRUE)
  expect_match(text, "[Respondent's Children]", fixed = TRUE)
  expect_false(grepl(paste0("Q", questions$question_number[1], " "), text, fixed = TRUE))
  # each choice on its own line, code first
  choices <- draft_service_children(draft, "response_options", first$id)
  expect_match(text, paste0(choices$code_value[1], " = ", choices$answer_option[1]), fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr('class="qt-choice"', html))), nrow(draft$response_options))
  expect_false(grepl("data-field", html)) # nothing to edit
})

test_that("a variable's label and subject go in square brackets, and a number read with the words is left off", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  question <- draft$components$id[draft$components$component_type == "Question"][1]
  number <- draft_service_question_number(draft$components[draft$components$id == question, ]) # as printed: 1a
  words <- draft$components$text[draft$components$id == question]
  draft$components$text[draft$components$id == question] <- paste0(number, ". ", words)
  box <- draft$variables$id[draft$variables$component_id == question][1]
  draft$variables$label[draft$variables$id == box] <- "Lifetime"
  draft$variables$subject[draft$variables$id == box] <- "Respondent's Siblings"
  text <- text_of(questionnaire_preview_ui(draft))
  expect_match(text, paste0(" ", words, " [Lifetime] [Respondent's Siblings]"), fixed = TRUE)
  expect_false(grepl(paste0(number, ". ", words), text, fixed = TRUE))

  draft$variables$subject[draft$variables$id == box] <- NA
  draft$variables$label[draft$variables$id == box] <- NA
  expect_false(grepl(paste0(words, " ["), text_of(questionnaire_preview_ui(draft)), fixed = TRUE))
})

test_that("a write-in says its kind of answer under Variable Coding", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  box <- draft$variables$id[1]
  draft$variables$variable_type[1] <- "open_ended"
  draft$variables$answer_data_type[1] <- "Integer"
  expect_match(text_of(questionnaire_preview_ui(draft)), "Write-in: Integer")
})

test_that("blanks are said in words, and an empty questionnaire says so", {
  draft <- gold_draft()
  text <- text_of(questionnaire_preview_ui(draft))
  expect_match(text, "no MATR ID")
  expect_match(text_of(questionnaire_preview_ui(draft_service_empty())), "no blocks")
})

test_that("the change request dialog has the Basic Info form, what it adds and the questionnaire, with its PDF when the file is here", {
  me <- list(staff_id = 1L, name = "Reviewer", role = "STAFF")
  record <- list(
    id = "7", kind = extraction_kind, summary = "Push ZZQ Wave 3", prepared_by = "Alex", prepared_by_id = 2L,
    saved_by_id = 2L, saved = format_time(Sys.time()), status = "Awaiting Review", revision = 1L
  )
  draft <- draft_service_renumber_questions(with_matr_ids())
  preview <- list(
    what = list(list(kind = "Response options", title = "40 response options", subtitle = "", facts = "40 new", is_new = TRUE)),
    summary = "Adds the wave.", unchecked = 2
  )
  detail <- list(preview = preview, draft = draft, pdf_url = "session/x/file.pdf")
  html <- htmltools::renderTags(change_request_dialog(record, me, shiny::NS("cr"), detail))$html
  expect_match(html, "modal-xl")
  expect_match(html, "questionnaire-table")
  expect_match(html, 'href="session/x/file.pdf"')
  expect_match(html, "Open PDF")
  expect_match(html, "2 blocks haven&#39;t been marked as checked yet|2 blocks haven't been marked as checked yet")
  expect_match(html, "Basic Info")
  expect_match(html, 'id="cr-info-registry_id"')
  expect_match(html, 'id="cr-save_info"')
  expect_false(grepl("Where it goes", html))
  expect_match(html, "What it adds")
  expect_match(html, "fa-certificate")
  # its Basic Info isn't filled in, so Approve starts disabled
  approve <- regmatches(html, regexpr('<button[^>]*id="cr-approve"[^>]*>|<button[^>]*disabled[^>]*id="cr-approve"[^>]*>', html))
  expect_match(approve, "disabled")

  no_file <- htmltools::renderTags(change_request_dialog(record, me, shiny::NS("cr"), detail[c("preview", "draft")]))$html
  expect_false(grepl("Open PDF", no_file))
  unread <- htmltools::renderTags(change_request_dialog(record, me, shiny::NS("cr"), list(preview = list(problem = "x"))))$html
  expect_match(unread, "couldn't be read")
})

test_that("the Edit Extraction preview shows only the questionnaire and its note, without Basic Info", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  html <- htmltools::renderTags(extraction_request_ui(list(unchecked = 3), draft))$html
  expect_match(html, "questionnaire-table")
  expect_match(html, "3 blocks haven")
  expect_false(grepl("Where it goes|Basic Info|What it adds|Open PDF|registry_id", html))
  dialog <- htmltools::renderTags(preview_request_dialog(shiny::NS("edit")))$html
  expect_false(grepl("Where it goes|edit-info-", dialog))
})
