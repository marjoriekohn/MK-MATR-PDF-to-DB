# Skip instructions linked to where they send respondents, and the parents worked out from them.

# A small questionnaire: question 1 ("No" goes to question 3), 2a and 2b (skipped by "No"), then 3, and a
# section headed "Section B" holding question 1 again (printed numbers can repeat).
skip_json <- function(skip = "Go to Question 3", parent_of_2a = NULL, condition_of_2a = NULL) {
  question <- function(number, choices = list(list(answer_option = "Yes"), list(answer_option = "No")), parent = NULL, condition = NULL) {
    list(
      component_type = "Question", text = paste("Question", number), printed_number = number, self_contained = TRUE,
      parent_question = parent, condition = condition,
      variables = list(list(variable_type = "closed_ended", response_options = choices))
    )
  }
  list(components = list(
    question("1", list(list(answer_option = "Yes"), list(answer_option = "No", skip_instruction = skip))),
    list(component_type = "Group", children = list(question("2a", parent = parent_of_2a, condition = condition_of_2a), question("2b"))),
    question("3"),
    list(component_type = "Section", text = "Section B. Your health", children = list(question("1")))
  ))
}
id_of <- function(draft, number, which = 1) {
  questions <- draft$components[draft$components$component_type == "Question", ]
  questions$id[draft_service_question_number(questions) %in% number][which]
}
no_choice <- function(draft) draft$response_options$id[draft$response_options$answer_option %in% "no"][1]

test_that("the number a skip instruction names is found however it is printed", {
  expect_equal(
    skip_number(c("Go to Question 32", "Skip to Q. 5a.", "GO TO QUESTION #12", "go to 7", "Continue with 4#3", "End of survey", NA)),
    c("32", "5a", "12", "7", "4#3", NA, NA)
  )
  expect_equal(skip_section(c("Go to Section B", "Skip to Part 2", "Go to Question 3")), c("section b", "part 2", NA))
})

test_that("a reading's skip instructions are linked when it arrives: the nearest question after, then any, then the first part", {
  draft <- draft_service_from_json(skip_json())
  expect_equal(draft$response_options$skip_to_id[draft$response_options$id == no_choice(draft)], id_of(draft, "3"))
  # "1" is printed twice: from the last question, it is the one after it (in Section B)
  expect_equal(draft_service_skip_target(draft, no_choice(draft), "Go to Question 1"), id_of(draft, "1", 2))
  # "Go to Question 2" over 2a and 2b is the first of its parts
  expect_equal(draft_service_skip_target(draft, no_choice(draft), "Go to Question 2"), id_of(draft, "2a"))
  # a section by its heading
  section <- draft$components$id[draft$components$component_type == "Section"]
  expect_equal(draft_service_skip_target(draft, no_choice(draft), "Skip to Section B"), section)
  expect_equal(draft_service_skip_target(draft, no_choice(draft), "End of survey"), skip_to_end_id)
  expect_true(is.na(draft_service_skip_target(draft, no_choice(draft), "Go to Question 9")))
})

test_that("the questions a choice skips past follow its question, unless they have a parent already", {
  draft <- draft_service_from_json(skip_json())
  q1 <- id_of(draft, "1")
  for (number in c("2a", "2b")) {
    row <- draft$components$id == id_of(draft, number)
    expect_equal(draft$components$parent_question_id[row], q1)
    expect_true(draft$components$parent_from_skip[row])
  }
  expect_true(is.na(draft$components$parent_question_id[draft$components$id == id_of(draft, "3")])) # where it lands
  # a parent the AI read is kept
  read <- draft_service_from_json(skip_json(parent_of_2a = "3"))
  expect_false(read$components$parent_from_skip[read$components$id == id_of(read, "2a")])
})

test_that("a parent the AI read that a skip from the same question also gives is the skip's, without its own answers", {
  read <- draft_service_from_json(skip_json(parent_of_2a = "1", condition_of_2a = "If yes"))
  row <- read$components$id == id_of(read, "2a")
  expect_equal(read$components$parent_question_id[row], id_of(read, "1"))
  expect_true(read$components$parent_from_skip[row])
  expect_true(is.na(read$components$condition[row]))
  # so the details say it once, from the Skip to
  html <- as.character(skip_logic(read$components[row, , drop = FALSE], read, NS("outline")))
  expect_equal(lengths(regmatches(html, gregexpr("skip-sentence", html, fixed = TRUE))), 1L)
  expect_match(html, "from a Skip to in that question")
})

test_that("a parent worked out from a skip follows the skip, and a parent staff pick is never replaced", {
  draft <- draft_service_from_json(skip_json())
  no <- no_choice(draft)
  # the skip now lands on 2b: only 2a is skipped
  shorter <- draft_service_skip_parents(draft_service_update(draft, "response_options", no, "skip_to_id", id_of(draft, "2b")))
  expect_equal(shorter$components$parent_question_id[shorter$components$id == id_of(draft, "2a")], id_of(draft, "1"))
  expect_true(is.na(shorter$components$parent_question_id[shorter$components$id == id_of(draft, "2b")]))
  # staff pick 2b's parent: it stays when the skip covers it again
  picked <- draft_service_update(shorter, "components", id_of(draft, "2b"), "parent_question_id", id_of(draft, "2a"))
  again <- draft_service_skip_parents(draft_service_update(picked, "response_options", no, "skip_to_id", id_of(draft, "3")))
  expect_equal(again$components$parent_question_id[again$components$id == id_of(draft, "2b")], id_of(draft, "2a"))
  # no skip: the worked-out parents go
  gone <- draft_service_skip_parents(draft_service_update(draft, "response_options", no, "skip_to_id", ""))
  expect_true(is.na(gone$components$parent_question_id[gone$components$id == id_of(draft, "2a")]))
})

test_that("where a choice skips to is picked from the list, or typed as a number or section", {
  draft <- draft_service_from_json(skip_json())
  no <- no_choice(draft)
  targets <- skip_targets(draft)
  expect_equal(targets$label[targets$id == id_of(draft, "3")], "3 · Question 3")
  section <- draft$components$id[draft$components$component_type == "Section"]
  expect_equal(targets$label[targets$id == section], "Section · Section B. Your health")
  # a group by its first question, and the end of the questionnaire last
  group <- draft$components$id[draft$components$component_type == "Group"]
  expect_equal(targets$label[targets$id == group], "Group · 2a · Question 2a")
  expect_equal(utils::tail(targets$label, 1), "End of Questionnaire")
  at <- function(d) d$response_options$skip_to_id[d$response_options$id == no]
  expect_equal(at(draft_service_update(draft, "response_options", no, "skip_to_id", "2b · Question 2b")), id_of(draft, "2b"))
  expect_equal(at(draft_service_update(draft, "response_options", no, "skip_to_id", " 2b ")), id_of(draft, "2b"))
  expect_equal(at(draft_service_update(draft, "response_options", no, "skip_to_id", "Section · Section B. Your health")), section)
  expect_equal(at(draft_service_update(draft, "response_options", no, "skip_to_id", "Group · 2a · Question 2a")), group)
  expect_equal(at(draft_service_update(draft, "response_options", no, "skip_to_id", "End of Questionnaire")), skip_to_end_id)
  expect_true(is.na(at(draft_service_update(draft, "response_options", no, "skip_to_id", ""))))
  expect_error(draft_service_update(draft, "response_options", no, "skip_to_id", "99"), "Nothing in the questionnaire is 99")
  # printed words typed for a choice that isn't linked link it
  unlinked <- draft_service_update(draft, "response_options", no, "skip_to_id", "")
  expect_equal(at(draft_service_update(unlinked, "response_options", no, "skip_instruction", "Go to Question 2b")), id_of(draft, "2b"))
})

test_that("a skip that names a question it can't find, or points back, is a problem; deleting where it lands unlinks it", {
  lookups <- test_lookups()
  draft <- draft_service_from_json(skip_json(skip = "Go to Question 9"))
  draft$variables$matr_id <- paste0("m", seq_len(nrow(draft$variables)))
  problems <- draft_service_check(draft, lookups)
  expect_equal(problems$message, "Variable 1: pick where choice “no” skips to.")
  expect_equal(problems$field, "skip_to_id")
  back <- draft_service_update(draft, "response_options", no_choice(draft), "skip_to_id", id_of(draft, "1"))
  expect_match(draft_service_check(back, lookups)$message, "skips back to something printed before it")
  # "End of survey" is the end of the questionnaire
  ending <- draft_service_update(draft, "response_options", no_choice(draft), "skip_instruction", "End of survey")
  expect_equal(nrow(draft_service_check(ending, lookups)), 0)

  linked <- draft_service_from_json(skip_json())
  gone <- suppressMessages(draft_service_delete(linked, "components", id_of(linked, "3")))
  expect_true(is.na(gone$response_options$skip_to_id[gone$response_options$id == no_choice(linked)]))
})

test_that("a saved copy keeps where each choice skips to, and which parents came from a skip", {
  draft <- draft_service_from_json(skip_json())
  saved <- draft_service_to_json(draft)
  again <- draft_service_from_json(saved)
  expect_identical(again, draft)
  expect_true(any(vapply(saved$components, function(block) isTRUE(block$children[[1]]$parent_from_skip), logical(1))))
  # a block moved keeps the choices that skip to it
  q3 <- id_of(draft, "3")
  printed <- draft_service_printed_order(draft)
  parents <- draft$components$parent_id[match(printed, draft$components$id)]
  moved_order <- c(setdiff(printed, q3)[1:4], q3, setdiff(printed, q3)[-(1:4)])
  moved <- draft_service_apply_order(draft, moved_order, parents[match(moved_order, printed)])
  reloaded <- draft_service_from_json(draft_service_to_json(moved))
  target <- reloaded$response_options$skip_to_id[reloaded$response_options$ref == no_choice(draft)]
  expect_equal(reloaded$components$ref[reloaded$components$id == target], q3) # still question 3, wherever it is now
})

test_that("the accuracy record leaves out the links and parents the site works out from skips", {
  original <- draft_service_from_json(skip_json())
  relinked <- draft_service_update(original, "response_options", no_choice(original), "skip_to_id", id_of(original, "2b"))
  edits <- draft_service_review_edits(original, draft_service_skip_parents(relinked))
  expect_false(any(vapply(edits, function(edit) identical(edit$field, "skip_to_id"), logical(1))))
})

test_that("a skip to the end of the questionnaire covers every question after it, and a closing section with no question is the end", {
  draft <- draft_service_from_json(skip_json(skip = "End of questionnaire"))
  no <- no_choice(draft)
  expect_equal(draft$response_options$skip_to_id[draft$response_options$id == no], skip_to_end_id)
  after <- setdiff(draft$components$id[draft$components$component_type == "Question"], id_of(draft, "1"))
  expect_true(all(draft$components$parent_from_skip[draft$components$id %in% after]))
  # a section holding only its closing words: "Go to Section C" ends the questionnaire
  json <- skip_json(skip = "Go to Section C")
  json$components <- c(json$components, list(list(
    component_type = "Section", text = "Section C. Thank you",
    children = list(list(component_type = "Note", text = "You have finished."))
  )))
  closing <- draft_service_from_json(json)
  expect_equal(closing$response_options$skip_to_id[closing$response_options$id == no_choice(closing)], skip_to_end_id)
  # it is saved, and read back
  again <- draft_service_from_json(draft_service_to_json(draft))
  expect_equal(again$response_options$skip_to_id[again$response_options$id == no_choice(again)], skip_to_end_id)
})

test_that("a write-in skips for the answer it names, linked like a choice's, and is saved with it", {
  json <- skip_json()
  json$components[[1]]$variables <- list(list(
    variable_type = "open_ended", answer_data_type = "NUMBER", skip_condition = "None", skip_instruction = "Go to Question 3"
  ))
  draft <- draft_service_from_json(json)
  box <- draft$variables[draft$variables$component_id == id_of(draft, "1"), ]
  expect_equal(box$skip_condition, "None")
  expect_equal(box$skip_to_id, id_of(draft, "3"))
  expect_true(draft$components$parent_from_skip[draft$components$id == id_of(draft, "2a")])
  again <- draft_service_from_json(draft_service_to_json(draft))
  expect_identical(again$variables, draft$variables)
  # picked from the list, and checked like a choice's
  ended <- draft_service_update(draft, "variables", box$id, "skip_to_id", "End of Questionnaire")
  expect_equal(ended$variables$skip_to_id[ended$variables$id == box$id], skip_to_end_id)
  draft$variables$matr_id <- paste0("m", seq_len(nrow(draft$variables)))
  no_answer <- draft_service_update(draft, "variables", box$id, "skip_condition", "")
  expect_match(draft_service_check(no_answer, test_lookups())$message, "say which answer skips")
  back <- draft_service_update(draft, "variables", box$id, "skip_to_id", id_of(draft, "1", 1))
  expect_match(draft_service_check(back, test_lookups())$message, "skips back to something printed before it")
  # a variable that gets choices skips by its choices instead
  choices <- draft_service_update(draft, "variables", box$id, "variable_type", "closed_ended")
  expect_true(all(is.na(unlist(choices$variables[choices$variables$id == box$id, c("skip_condition", "skip_instruction", "skip_to_id")]))))
})

# ------ A group that skips, on the answers to the questions in it -----------------------------------------
# 22a and 22b in a group: "If you answered No to items 22a-b, go to Question 24", so 23 is skipped.
group_skip_json <- function(skip_when = "every") {
  question <- function(number) {
    list(
      component_type = "Question", text = paste("Question", number), printed_number = number, self_contained = TRUE,
      variables = list(list(variable_type = "closed_ended", response_options = list(list(answer_option = "Yes"), list(answer_option = "No"))))
    )
  }
  list(components = list(
    list(
      component_type = "Group", skip_condition = "No", skip_when = skip_when, skip_instruction = "Go to Question 24",
      children = list(question("22a"), question("22b"))
    ),
    question("23"),
    question("24")
  ))
}

test_that("a group's skip is linked when it is read, saved with it, and sends respondents past the questions after it", {
  draft <- draft_service_from_json(group_skip_json())
  group <- draft$components[draft$components$component_type == "Group", ]
  expect_equal(group$skip_to_id, id_of(draft, "24"))
  expect_equal(group$skip_condition, "No")
  expect_equal(draft_service_group_skipped(draft), data.frame(question = id_of(draft, "23"), group = group$id))
  # a group isn't a question, so the questions it skips have no parent
  expect_true(is.na(draft$components$parent_question_id[draft$components$id == id_of(draft, "23")]))
  # saved and read back the same
  again <- draft_service_from_json(draft_service_to_json(draft))
  expect_equal(again$components[, c("skip_condition", "skip_when", "skip_instruction", "skip_to_id")], draft$components[, c("skip_condition", "skip_when", "skip_instruction", "skip_to_id")])
  # nothing to fix
  expect_equal(nrow(group_skip_problems(draft)), 0)
})

test_that("a group's skip is checked: where it goes is picked, past the group, and it says which answer skips", {
  draft <- draft_service_from_json(group_skip_json())
  group <- draft$components$id[draft$components$component_type == "Group"]
  unlinked <- draft
  unlinked$components$skip_to_id[unlinked$components$id == group] <- NA
  expect_equal(group_skip_problems(unlinked)$field, "skip_to_id")
  back <- draft_service_update(draft, "components", group, "skip_to_id", id_of(draft, "22b"))
  expect_match(group_skip_problems(back)$message, "skips back")
  no_answer <- draft_service_update(draft, "components", group, "skip_condition", "")
  expect_equal(group_skip_problems(no_answer)$field, "skip_condition")
})

test_that("only a group skips: made another kind, its skip goes; ungrouped or deleted targets unlink it", {
  draft <- draft_service_from_json(group_skip_json())
  group <- draft$components$id[draft$components$component_type == "Group"]
  section <- draft_service_update(draft_service_update(draft, "components", group, "component_type", "Section"), "components", group, "text", "Part A")
  expect_true(all(is.na(unlist(section$components[section$components$id == group, c("skip_condition", "skip_when", "skip_instruction", "skip_to_id")]))))
  gone <- draft_service_delete(draft, "components", id_of(draft, "24"))
  expect_true(is.na(gone$components$skip_to_id[gone$components$id == group]))
})

test_that("a question a group skips says so, and links to the group", {
  draft <- draft_service_from_json(group_skip_json("any"))
  attr(draft, "outline_questions") <- outline_questions(draft)
  q23 <- draft$components[draft$components$id == id_of(draft, "23"), , drop = FALSE]
  html <- as.character(skip_logic(q23, draft, shiny::NS("x")))
  expect_match(text_of(html), "Respondents who answer “No” to any question in the group with questions 22a–22b skip this question", fixed = TRUE)
  expect_match(html, sprintf('data-id="%s"', draft$components$id[draft$components$component_type == "Group"]), fixed = TRUE)
  # 24, where it lands, is everyone's
  q24 <- draft$components[draft$components$id == id_of(draft, "24"), , drop = FALSE]
  expect_match(as.character(skip_logic(q24, draft, shiny::NS("x"))), "Every respondent sees this question.")
})

test_that("a group's details hold its skip: the answer, every or any, where it goes, and the words printed", {
  draft <- draft_service_from_json(group_skip_json())
  group <- draft$components[draft$components$component_type == "Group", , drop = FALSE]
  view <- list(problems = draft_service_check(draft, test_lookups()))
  html <- htmltools::renderTags(block_detail(group, draft, view, test_lookups(), shiny::NS("x")))$html
  expect_match(html, 'data-field="skip_condition"')
  expect_match(html, 'data-field="skip_when"')
  expect_match(html, 'data-field="skip_to_id"')
  expect_match(html, 'data-field="skip_instruction"')
  expect_match(html, 'value="24 · ', fixed = TRUE) # where it goes, as the list names it
})
