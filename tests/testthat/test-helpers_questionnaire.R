# A questionnaire laid out to read, and an extraction's change request, as the Edit Extraction page's preview
# and the Change Requests dialog show them.

test_that("the questionnaire is a table: a row for each variable with its MATR ID, its question's words and its coding", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  rows <- questionnaire_preview_rows(draft)
  questions <- draft$components[draft$components$component_type == "Question", ]
  expect_equal(sum(rows$kind == "Question"), nrow(draft$variables))
  # blocks that aren't questions have a row each, with their kind; sections and groups head the rows instead
  others <- draft$components[!draft$components$component_type %in% c("Question", "Section", "Question Group"), ]
  expect_equal(sum(rows$kind != "Question"), nrow(others))
  expect_false(any(rows$kind %in% c("Section", "Question Group")))
  # the question's words, without its number, then who the variable is about in square brackets (nothing
  # for the respondent)
  first <- draft$variables[draft$variables$component_id == questions$id[1], ][1, ]
  row <- rows[rows$matr_id %in% first$matr_id, ][1, ]
  expect_equal(row$text, questions$text[1])
  expect_equal(row$tags, "")
  choices <- draft_service_children(draft, "response_options", first$id)
  expect_equal(row$coding[[1]], paste(choices$code_value, "=", choices$answer_option)) # each choice on its own line
  children <- draft$variables[draft$variables$subject %in% "Respondent's Children", ][1, ]
  expect_match(rows$tags[rows$matr_id %in% children$matr_id][1], "[Respondent's Children]", fixed = TRUE)
  # each row names the sections and question groups it is in
  around <- draft$components[draft$components$id %in% draft_service_ancestors(draft, questions$id[1]), , drop = FALSE]
  expect_gt(nrow(around), 0)
  for (i in seq_len(nrow(around))) expect_match(row$section, preview_container_name(around[i, , drop = FALSE]), fixed = TRUE)
})

test_that("the table is a DataTables table, with no No. column, grouped by section, with Download CSV", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  html <- htmltools::renderTags(questionnaire_preview_ui(draft))$html
  expect_match(html, "questionnaire-table")
  expect_match(html, "datatables")
  expect_match(html, "Variable ID")
  expect_match(html, "Question Text")
  expect_match(html, "Variable Coding")
  expect_false(grepl("<th>No\\.|qt-number", html)) # no such column (the words may say "Yes or No.")
  expect_match(html, "Download CSV")
  expect_match(html, '"rowGroup"')
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
  matr <- draft$variables$matr_id[draft$variables$id == box]
  row <- questionnaire_preview_rows(draft)
  row <- row[row$matr_id %in% matr, ]
  expect_equal(row$text, words)
  expect_equal(row$tags, "[Lifetime] [Respondent's Siblings]")

  draft$variables$subject[draft$variables$id == box] <- NA
  draft$variables$label[draft$variables$id == box] <- NA
  row <- questionnaire_preview_rows(draft)
  expect_equal(row$tags[row$matr_id %in% matr], "")
})

test_that("a write-in says its kind of answer under Variable Coding", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  draft$variables$variable_type[1] <- "open_ended"
  draft$variables$answer_data_type[1] <- "Integer"
  rows <- questionnaire_preview_rows(draft)
  expect_equal(rows$coding[rows$matr_id %in% draft$variables$matr_id[1]][[1]], "Write-in: Integer")
})

test_that("blanks are said in words, and an empty questionnaire says so", {
  rows <- questionnaire_preview_rows(gold_draft())
  expect_true("no MATR ID" %in% rows$missing)
  expect_match(as.character(questionnaire_preview_ui(draft_service_empty())), "no blocks")
})

test_that("the change request dialog has the Basic Info form and what it adds, and leaves the questionnaire to Preview", {
  me <- list(staff_id = 2L, name = "Alex", role = "STAFF")
  record <- list(
    id = "7", kind = extraction_kind, summary = "Push ZZQ Wave 3", prepared_by = "Alex", prepared_by_id = 2L,
    saved_by_id = 2L, saved = format_time(Sys.time()), status = "Awaiting Review", revision = 1L
  )
  preview <- list(
    what = list(list(kind = "Response options", title = "40 response options", subtitle = "", facts = "40 new", is_new = TRUE)),
    summary = "Adds the wave.", unchecked = 2
  )
  html <- htmltools::renderTags(change_request_dialog(record, me, shiny::NS("cr"), list(preview = preview)))$html
  expect_match(html, "modal-xl")
  expect_match(html, "Basic Info")
  expect_match(html, 'id="cr-info-registry_id"')
  expect_match(html, 'id="cr-save_info"')
  expect_match(html, "What it adds")
  expect_match(html, "fa-certificate")
  # the questionnaire, its PDF, and Edit and Withdraw are the Preview's
  expect_false(grepl("questionnaire-table|Open PDF|marked as checked|The questionnaire", html))
  expect_false(grepl('id="cr-edit"|id="cr-withdraw"', html))

  # someone else's: Approve starts disabled, since its Basic Info isn't filled in
  reviewer <- list(staff_id = 1L, name = "Reviewer", role = "STAFF")
  html <- htmltools::renderTags(change_request_dialog(record, reviewer, shiny::NS("cr"), list(preview = preview)))$html
  approve <- regmatches(html, regexpr('<button[^>]*id="cr-approve"[^>]*>|<button[^>]*disabled[^>]*id="cr-approve"[^>]*>', html))
  expect_match(approve, "disabled")
  unread <- htmltools::renderTags(change_request_dialog(record, reviewer, shiny::NS("cr"), list(preview = list(problem = "x"))))$html
  expect_match(unread, "This can&#39;t be added as it is|This can't be added as it is")
})

test_that("an edit's request dialog keeps Edit and Withdraw, since it has no Preview", {
  me <- list(staff_id = 2L, name = "Alex", role = "STAFF")
  record <- list(
    id = "8", kind = "Edit", edit_kind = "Delete", summary = "Delete a type", prepared_by = "Alex", prepared_by_id = 2L,
    saved_by_id = 2L, saved = format_time(Sys.time()), status = "Awaiting Review", revision = 1L
  )
  html <- htmltools::renderTags(change_request_dialog(record, me, shiny::NS("cr"), list(counts = change_service_counts(list()), groups = list())))$html
  expect_match(html, 'id="cr-edit"')
  expect_match(html, 'id="cr-withdraw"')
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

test_that("an extraction's request card has Preview, which shows its questionnaire as Edit Extraction's Preview does", {
  me <- list(staff_id = 1L, name = "Reviewer", role = "STAFF")
  record <- list(
    id = "7", kind = extraction_kind, summary = "Push ZZQ Wave 3", prepared_by = "Alex", prepared_by_id = 2L,
    saved_by_id = 2L, saved = format_time(Sys.time()), status = "Awaiting Review", revision = 1L
  )
  card <- htmltools::renderTags(change_request_card_ui(record, me))$html
  expect_match(card, 'data-action="preview" data-id="7"', fixed = TRUE)
  expect_match(card, 'data-action="review" data-id="7"', fixed = TRUE)
  # an edit's request has no questionnaire to preview
  edit <- htmltools::renderTags(change_request_card_ui(utils::modifyList(record, list(kind = "Edit")), me))$html
  expect_false(grepl('data-action="preview"', edit, fixed = TRUE))

  draft <- draft_service_renumber_questions(with_matr_ids())
  dialog <- htmltools::renderTags(change_request_preview_dialog(record, me, shiny::NS("cr"), draft, "session/x/file.pdf"))$html
  expect_match(dialog, "modal-xl")
  expect_match(dialog, "Preview: Push ZZQ Wave 3")
  expect_match(dialog, "questionnaire-table")
  expect_match(dialog, "Open PDF")
  expect_false(grepl("Basic Info|cr-info-|What it adds", dialog))
  expect_match(dialog, "Review and approve")
  expect_match(dialog, 'id="cr-edit"') # Edit, with the questionnaire
  expect_false(grepl('id="cr-withdraw"', dialog)) # only the person who prepared it withdraws it
  mine <- htmltools::renderTags(change_request_preview_dialog(record, list(staff_id = 2L, name = "Alex", role = "STAFF"), shiny::NS("cr"), draft))$html
  expect_match(mine, 'id="cr-withdraw"')
  unread <- htmltools::renderTags(change_request_preview_dialog(record, me, shiny::NS("cr"), NULL))$html
  expect_match(unread, "couldn't be read")
})
