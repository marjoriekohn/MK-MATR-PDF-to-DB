# A questionnaire laid out to read, as the Edit Extraction page's Preview shows it before the extraction is pushed.


test_that("the questionnaire is a table: a row for each variable with its Variable ID, its question's words and its coding", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  rows <- questionnaire_preview_rows(draft)
  questions <- draft$components[draft$components$component_type == "Question", ]
  expect_equal(sum(rows$kind == "Question"), nrow(draft$variables))
  # blocks that aren't questions have a row each, with their kind; sections, grids and groups head the rows instead
  others <- draft$components[!draft$components$component_type %in% c("Question", container_types), ]
  expect_equal(sum(rows$kind != "Question"), nrow(others))
  expect_false(any(rows$kind %in% container_types))
  # the question's words, without its number; who the variable is about is one of its tags (nothing for the
  # respondent)
  first <- draft$variables[draft$variables$component_id == questions$id[1], ][1, ]
  row <- rows[rows$matr_id %in% first$matr_id, ][1, ]
  expect_equal(row$text, questions$text[1])
  expect_equal(row$tags, "")
  choices <- draft_service_children(draft, "response_options", first$id)
  expect_equal(row$coding[[1]], paste(choices$code_value, "=", choices$answer_option)) # each choice on its own line
  # a tag of the question's own is said on each of its variables' rows
  about_children <- draft$components$id[draft$components$subject %in% "children"][1]
  children <- draft$variables[draft$variables$component_id %in% about_children, ][1, ]
  expect_match(rows$tags[rows$matr_id %in% children$matr_id][1], "[children]", fixed = TRUE)
  # and a variable's own tag comes before its question's of the same kind
  expect_equal(preview_variable_tags(list(label = NA, unit = NA, time_frame = "past 30 days", subject = NA), list(time_frame = "lifetime", subject = "twins")), "[past 30 days] [twins]")
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

test_that("the table is a DataTables table, with no No. column, grouped by section, with the site's own toolbar", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  html <- htmltools::renderTags(questionnaire_preview_ui(draft))$html
  expect_match(html, "questionnaire-table")
  expect_match(html, "datatables")
  expect_match(html, "Variable ID")
  expect_match(html, "Question Text")
  expect_match(html, "Tags")
  expect_match(html, "Variable Coding")
  expect_false(grepl("<th>No\\.|qt-number", html)) # no such column (the words may say "Yes or No.")
  expect_match(html, '"rowGroup"')
  expect_false(grepl("data-field", html)) # nothing to edit
  # Search and Download CSV are the site's own, above the table (www/js/preview.js works them)
  expect_match(html, 'class="qt-toolbar"', fixed = TRUE)
  expect_match(html, 'class="form-control form-control-sm qt-search"', fixed = TRUE)
  expect_match(html, "btn btn-sm btn-outline-dark qt-download", fixed = TRUE)
  expect_match(html, "Download CSV")
  expect_false(grepl("Search:", html, fixed = TRUE)) # not DataTables' own box
  expect_match(html, "caraPreviewTable", fixed = TRUE)
  # with no push tried there is nothing new to mark, so no marks and no choice of rows
  expect_false(grepl('class=\\"new-mark\\"', html, fixed = TRUE)) # (the CSV's formatter names the class too)
  expect_false(grepl("data-qt-filter", html, fixed = TRUE))
})

test_that("a variable's label goes in square brackets, its subject is a tag, and a number read with the words is left off", {
  draft <- draft_service_renumber_questions(with_matr_ids())
  question <- draft$components$id[draft$components$component_type == "Question"][1]
  number <- draft_service_question_number(draft$components[draft$components$id == question, ]) # as printed: 1a
  words <- draft$components$text[draft$components$id == question]
  draft$components$text[draft$components$id == question] <- paste0(number, ". ", words)
  box <- draft$variables$id[draft$variables$component_id == question][1]
  draft$variables$label[draft$variables$id == box] <- "Lifetime"
  draft$variables$subject[draft$variables$id == box] <- "siblings"
  matr <- draft$variables$matr_id[draft$variables$id == box]
  row <- questionnaire_preview_rows(draft)
  row <- row[row$matr_id %in% matr, ]
  expect_equal(row$text, words)
  expect_equal(row$label, "Lifetime")
  expect_equal(row$tags, "[siblings]")
  expect_equal(row$tag_names[[1]], "siblings")
  # a unit and a time frame are tags too: unit, time frame, subject
  draft$variables$unit[draft$variables$id == box] <- "hours"
  draft$variables$time_frame[draft$variables$id == box] <- "past 30 days"
  row <- questionnaire_preview_rows(draft)
  expect_equal(row$tags[row$matr_id %in% matr], "[hours] [past 30 days] [siblings]")
  html <- htmltools::renderTags(questionnaire_preview_ui(draft))$html
  # (the table's rows are written as JSON, which writes "</" as "<\\/")
  expect_match(html, '<span class=\\"qt-label\\">[Lifetime]<\\/span>', fixed = TRUE)
  expect_match(html, '<span class=\\"qt-tag\\">hours<\\/span><br><span class=\\"qt-tag\\">past 30 days<\\/span>', fixed = TRUE)

  draft$variables$subject[draft$variables$id == box] <- NA
  draft$variables$label[draft$variables$id == box] <- NA
  draft$variables$unit[draft$variables$id == box] <- NA
  draft$variables$time_frame[draft$variables$id == box] <- NA
  row <- questionnaire_preview_rows(draft)
  expect_equal(row$tags[row$matr_id %in% matr], "")
  expect_equal(row$label[row$matr_id %in% matr], "")
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
  expect_true("no Variable ID" %in% rows$missing)
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
  # a draft reopened from a wave goes back into it: no form, only which wave it updates
  updating <- htmltools::renderTags(push_dialog(shiny::NS("edit"), draft, updates = "Kid Questionnaire (KQ) · Wave 1"))$html
  expect_false(grepl("Basic Info|edit-info-", updating))
  expect_match(updating, "Updating")
  expect_match(updating, "Pushing replaces what\\s*<strong>Kid Questionnaire \\(KQ\\) · Wave 1</strong>\\s*asks with this draft\\.")
})

test_that("the dialog has no sentence of what a push adds: what is new is marked in the table", {
  html <- htmltools::renderTags(extraction_request_ui(list(problem = NULL, unchecked = 0), with_matr_ids(), questionnaire = FALSE))$html
  expect_false(grepl("What it adds|request-card|Adds ", html))
  refused <- htmltools::renderTags(extraction_request_ui(list(problem = "The wave asks questions already."), NULL, questionnaire = FALSE))$html
  expect_match(refused, "This can't be pushed as it is", fixed = TRUE)
  expect_match(refused, "The wave asks questions already.", fixed = TRUE)
})

test_that("once the push has been tried, NEW marks what it adds row by row, and the rows can be shown by it", {
  draft <- test_questionnaire_draft()
  questions <- draft$components[draft$components$component_type == "Question", ]
  smoke <- questions$text[grepl("smoke", questions$text, ignore.case = TRUE)]
  answers <- draft$response_options$answer_option
  yes <- answers[grepl("yes$", answers, ignore.case = TRUE)]
  no <- answers[grepl("no$", answers, ignore.case = TRUE)]
  unit <- draft$variables$unit[!is.na(draft$variables$unit)]
  # as the dry run names them: the wordings it found, and the choices, sets and tags it adds
  counts <- list(questions = 2L, names = list(
    question_reused = list(paste0("  ", smoke, " ")), # spaces don't matter
    answer_option_new = list(no),
    option_set_new = list(paste(no, yes, sep = " | ")), # nor the order of a set's choices
    tag_new = list(toupper(unit)) # nor a tag's capitals
  ))
  # and the catalog's instructions, definitions and notes, read apart: this instruction is there already
  instruction <- draft$components$text[draft$components$component_type == "Instruction"]
  blocks <- data.frame(kind = c("Instruction", "Definition"), text = c(paste0(" ", instruction), "a definition"))
  known <- extraction_service_known(counts, blocks)
  rows <- questionnaire_preview_rows(draft, known)
  smoking <- rows[rows$kind == "Question" & grepl("smoke", rows$text, ignore.case = TRUE), ]
  expect_false(smoking$text_new) # its wording is in the database
  expect_equal(smoking$coding_new[[1]], c(FALSE, TRUE)) # "no" is new, "yes" isn't
  expect_true(smoking$set_new)
  expect_equal(smoking$status, "new")
  how_many <- rows[grepl("how many", rows$text, ignore.case = TRUE), ]
  expect_true(how_many$text_new)
  expect_equal(how_many$tag_new[[1]], TRUE)
  expect_equal(how_many$status, "new")
  expect_equal(rows$status[rows$kind == "Instruction"], "found")
  expect_false(rows$text_new[rows$kind == "Instruction"])
  # one the catalog hasn't got is new; and when the catalog's couldn't be read, only All shows it
  other <- questionnaire_preview_rows(draft, extraction_service_known(counts, blocks[2, ]))
  expect_equal(other$status[other$kind == "Instruction"], "new")
  expect_true(other$text_new[other$kind == "Instruction"])
  unread <- questionnaire_preview_rows(draft, extraction_service_known(counts))
  expect_equal(unread$status[unread$kind == "Instruction"], "")
  # a question with nothing new is already in the database
  found <- questionnaire_preview_rows(draft, extraction_service_known(list(names = list(question_reused = as.list(questions$text)))))
  expect_equal(found$status[found$kind == "Question"], c("found", "found"))
  # with no dry run, or one refused, nothing is marked
  expect_null(extraction_service_known(NULL))
  expect_null(extraction_service_known(simpleError("The wave asks questions already.")))
  expect_null(extraction_service_known(list(questions = 2L)))
  plain <- questionnaire_preview_rows(draft)
  expect_true(all(plain$status == ""))
  expect_false(any(plain$text_new))

  html <- htmltools::renderTags(questionnaire_preview_ui(draft, known))$html
  expect_match(html, 'class=\\"new-mark\\"', fixed = TRUE)
  expect_match(html, "NEW option set", fixed = TRUE)
  expect_match(html, 'class=\\"qt-tag is-new\\"', fixed = TRUE)
  expect_match(html, "New (2)", fixed = TRUE)
  expect_match(html, "Already in the database (1)", fixed = TRUE) # the instruction: every row is one or the other
  expect_match(html, "All (3)", fixed = TRUE)
  other_html <- htmltools::renderTags(questionnaire_preview_ui(draft, extraction_service_known(counts, blocks[2, ])))$html
  expect_match(other_html, "This instruction isn&#39;t in the database yet", fixed = TRUE)
  expect_match(html, 'data-qt-filter=""', fixed = TRUE)
  expect_match(html, 'data-status-column="5"', fixed = TRUE)
  # the CSV leaves the marks and the status out
  expect_match(html, "new-mark[^", fixed = TRUE)
  expect_match(html, '"columns":[0,1,2,3,4]', fixed = TRUE)
})
