# A questionnaire laid out to read, as the Edit Extraction page's Preview shows it before the extraction is pushed.


test_that("the questionnaire is a table: a row for each variable with its MATR ID, its question's words and its coding", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  rows <- questionnaire_preview_rows(draft)
  questions <- draft$components[draft$components$component_type == "Question", ]
  expect_equal(sum(rows$kind == "Question"), nrow(draft$variables))
  # blocks that aren't questions have a row each, with their kind; sections, grids and groups head the rows instead
  others <- draft$components[!draft$components$component_type %in% c("Question", container_types), ]
  expect_equal(sum(rows$kind != "Question"), nrow(others))
  expect_false(any(rows$kind %in% container_types))
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
  # each row names the sections and groups it is in
  around <- draft$components[draft$components$id %in% draft_service_ancestors(draft, questions$id[1]), , drop = FALSE]
  expect_gt(nrow(around), 0)
  for (i in seq_len(nrow(around))) {
    name <- if (around$component_type[i] == "Group") "Group " else preview_container_name(around[i, , drop = FALSE])
    expect_match(row$section, name, fixed = TRUE)
  }
})

test_that("each group is named apart from the others, so where one ends shows as a section's end does", {
  question <- function(number) {
    list(
      component_type = "Question", text = paste("Question", number), printed_number = number,
      variables = list(list(variable_type = "closed_ended", response_options = list(list(answer_option = "Yes"))))
    )
  }
  draft <- draft_service_from_json(list(components = list(list(
    component_type = "Section", text = "Part A", children = list(
      list(component_type = "Group", children = list(question("1a"), question("1b"))),
      list(component_type = "Group", children = list(question("2a"), question("2b"))),
      question("3")
    )
  ))))
  rows <- questionnaire_preview_rows(draft)
  expect_equal(rows$section, c(rep("Part A \u203a Group 1", 2), rep("Part A \u203a Group 2", 2), "Part A"))
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
  # a unit and a time frame are shown too: label, unit, time frame, subject
  draft$variables$unit[draft$variables$id == box] <- "hours"
  draft$variables$time_frame[draft$variables$id == box] <- "in the past 30 days"
  row <- questionnaire_preview_rows(draft)
  expect_equal(row$tags[row$matr_id %in% matr], "[Lifetime] [hours] [in the past 30 days] [Respondent's Siblings]")

  draft$variables$subject[draft$variables$id == box] <- NA
  draft$variables$label[draft$variables$id == box] <- NA
  draft$variables$unit[draft$variables$id == box] <- NA
  draft$variables$time_frame[draft$variables$id == box] <- NA
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

test_that("the Edit Extraction preview shows the questionnaire and its note; the push dialog adds the Basic Info form", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  html <- htmltools::renderTags(extraction_request_ui(list(unchecked = 3), draft))$html
  expect_match(html, "questionnaire-table")
  expect_match(html, "3 blocks haven")
  expect_false(grepl("Where it goes|Basic Info|What it adds|Open PDF|registry_id", html))
  dialog <- htmltools::renderTags(push_dialog(shiny::NS("edit"), draft))$html
  expect_match(dialog, "Basic Info") # where it goes is picked here, before the push
  expect_match(dialog, "edit-info-")
})

test_that("what a push adds is cards with counts; the rows to open are listed only when the cards come with them", {
  cards <- list(
    list(kind = "Answer choices", title = "40 answer choices", subtitle = "", facts = "40 new", is_new = TRUE),
    list(kind = "Questions", title = "12 questions", subtitle = "", facts = list(), is_new = FALSE)
  )
  html <- htmltools::renderTags(extraction_request_ui(list(what = cards, summary = "Adds the wave."), with_matr_ids(), questionnaire = FALSE))$html
  expect_match(html, "What it adds")
  expect_match(html, "Adds the wave.")
  expect_match(html, "fa-certificate") # NEW on the card of a row the push adds
  expect_false(grepl("questionnaire-table|request-rows", html))
  expect_null(request_rows_ui(cards)) # the push's preview comes back as counts only
  with_rows <- cards
  with_rows[[1]]$rows <- data.frame(text = c("yes", "no"), is_new = c(TRUE, FALSE), status = c("", "used by 3"), stringsAsFactors = FALSE)
  with_rows[[1]]$columns <- c(text = "Choice")
  listed <- htmltools::renderTags(request_rows_ui(with_rows))$html
  expect_match(listed, "1 new row, 1 already there")
  expect_match(listed, "used by 3")
})
