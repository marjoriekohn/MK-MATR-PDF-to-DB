test_that("the hand-checked extraction passes every check once its MATR IDs are filled in", {
  expect_equal(nrow(draft_service_check(with_matr_ids(), test_lookups())), 0)
})

test_that("every variable without a MATR ID is a problem to fix, named for its question", {
  problems <- draft_service_check(gold_draft(), test_lookups())
  questions <- gold_draft()$components$id[gold_draft()$components$component_type == "Question"]
  expect_setequal(unique(problems$id), questions)
  expect_true(all(grepl("^Variable [0-9]+: fill in its MATR ID\\.$", problems$message)))
  expect_equal(nrow(problems), nrow(gold_draft()$variables))
  expect_equal(nrow(draft_service_check(gold_draft(), test_lookups(), matr_ids = FALSE)), 0) # as the reading job checks
})

test_that("each broken rule is reported against the block to fix", {
  draft <- with_matr_ids()
  q1a <- block_numbered(draft, "1a")$id
  q1b <- block_numbered(draft, "1b")$id
  box <- draft_service_children(draft, "variables", q1a)$id
  choices <- draft_service_children(draft, "response_options", box)$id
  problem_for <- function(broken, message) {
    problems <- draft_service_check(broken, test_lookups())
    any(problems$id == q1a & grepl(message, problems$message, fixed = TRUE))
  }
  edit <- function(table, id, field, value) draft_service_update(draft, table, id, field, value)

  expect_true(problem_for(edit("components", q1a, "text", ""), "Add the printed words"))
  # a printed number can be missing, or repeat (each section can start again at 1)
  expect_false(any(draft_service_check(edit("components", q1a, "printed_number", ""), test_lookups())$id == q1a))
  expect_false(any(draft_service_check(edit("components", q1a, "printed_number", "1B"), test_lookups())$id == q1a))
  expect_true(problem_for(edit("components", q1a, "self_contained", "FALSE"), "Pick the question it follows"))
  expect_true(problem_for(edit("components", q1a, "parent_question_id", q1b), "printed before it"))
  expect_true(problem_for(suppressMessages(draft_service_delete(draft, "variables", box)), "Add at least one variable"))
  no_choices <- suppressMessages(draft_service_delete(draft_service_delete(draft, "response_options", choices[2]), "response_options", choices[1]))
  expect_true(problem_for(no_choices, "add its choices"))
  # a lone check box has one choice, and that is enough
  expect_false(problem_for(suppressMessages(draft_service_delete(draft, "response_options", choices[2])), "add its choices"))
  expect_true(problem_for(draft_service_add_choice(draft, box), "fill in the blank choice"))
  expect_true(problem_for(edit("response_options", choices[2], "answer_option", "Yes"), "list each choice once"))

  same_codes <- edit("response_options", choices[1], "code_value", "1")
  same_codes <- draft_service_update(same_codes, "response_options", choices[2], "code_value", "1")
  expect_true(problem_for(same_codes, "its own code"))

  # who it's about can be blank (the respondent) or a subject the database doesn't have yet
  expect_false(problem_for(edit("variables", box, "subject", "Respondents Family"), "Variable"))
  expect_false(problem_for(edit("variables", box, "subject", ""), "Variable"))
  expect_true(problem_for(edit("variables", box, "variable_type", "open_ended"), "kind of answer"))
  expect_true(problem_for(edit("variables", box, "matr_id", ""), "fill in its MATR ID"))

  # editing refuses a MATR ID another variable has, but a draft can come with one (from the AI, or saved before)
  q1b_box <- draft_service_children(draft, "variables", q1b)$id
  same_ids <- edit("variables", box, "matr_id", "q1a")
  same_ids$variables$matr_id[same_ids$variables$id == q1b_box] <- "q1a"
  expect_true(problem_for(same_ids, "MATR ID no other variable has"))
})

test_that("a problem with one field says which, so the page can outline it", {
  draft <- with_matr_ids()
  q1a <- block_numbered(draft, "1a")$id
  box <- draft_service_children(draft, "variables", q1a)$id
  choices <- draft_service_children(draft, "response_options", box)$id
  field_of <- function(broken, message) {
    problems <- draft_service_check(broken, test_lookups())
    problems[grepl(message, problems$message, fixed = TRUE), c("table", "row", "field")]
  }
  expect_equal(unlist(field_of(draft_service_update(draft, "variables", box, "matr_id", ""), "MATR ID")), c(table = "variables", row = box, field = "matr_id"))
  expect_equal(unlist(field_of(draft_service_update(draft, "response_options", choices[2], "answer_option", "Yes"), "each choice once")[2, ]), c(table = "response_options", row = choices[2], field = "answer_option"))
  expect_equal(unlist(field_of(draft_service_update(draft, "components", q1a, "text", ""), "printed words")), c(table = "components", row = q1a, field = "text"))
  no_choices <- suppressMessages(draft_service_delete(draft_service_delete(draft, "response_options", choices[2]), "response_options", choices[1]))
  whole_box <- field_of(no_choices, "add its choices")
  expect_equal(whole_box$row, box)
  expect_true(is.na(whole_box$field))
})

test_that("problems are listed in printed order, and the next one wraps around", {
  draft <- with_matr_ids()
  for (number in c("1c", "1a")) {
    draft <- draft_service_update(draft, "components", block_numbered(draft, number)$id, "text", "")
  }
  problems <- draft_service_check(draft, test_lookups())
  q1a <- block_numbered(draft, "1a")$id
  q1c <- block_numbered(draft, "1c")$id
  expect_equal(problems$id, c(q1a, q1c))

  expect_equal(draft_service_next_problem(draft, problems, after_id = NULL), q1a)
  expect_equal(draft_service_next_problem(draft, problems, after_id = q1a), q1c)
  expect_equal(draft_service_next_problem(draft, problems, after_id = q1c), q1a)
})

test_that("a section needs its printed heading, a group has none, and a section isn't inside a group", {
  draft <- with_matr_ids()
  group <- draft$components$parent_id[draft$components$id == block_numbered(draft, "1a")$id]
  problems_of <- function(broken) draft_service_check(broken, test_lookups())
  expect_equal(nrow(problems_of(draft)), 0) # the gold questionnaire's groups have no words

  worded <- draft
  worded$components$text[worded$components$id == group] <- "Part A"
  expect_true(any(grepl("group has no words of its own", problems_of(worded)$message)))

  as_section <- draft_service_update(draft, "components", group, "component_type", "Section")
  heading <- problems_of(as_section)
  expect_equal(heading$message[heading$id == group], "Add the section's printed heading. Blocks with no printed heading go in a Group instead.")
  expect_equal(heading$field[heading$id == group], "text")
  expect_equal(nrow(problems_of(draft_service_update(as_section, "components", group, "text", "Part A"))), 0)

  # a section the AI put inside a group is pointed out
  nested <- draft
  inner <- draft_service_next_id(nested, "components")
  nested <- draft_service_add_block(nested, "Group", inner, near_id = group)
  nested$components$component_type[nested$components$id == inner] <- "Section"
  nested$components$text[nested$components$id == inner] <- "Part B"
  expect_true(any(problems_of(nested)$id == inner & grepl("can't be inside a group", problems_of(nested)$message)))
})

test_that("a count of respondents can't be below 0, and a choice without one is fine", {
  draft <- gold_draft()
  expect_false(any(grepl("respondents", draft_service_check(draft, test_lookups())$message)))
  below <- draft_service_update(draft, "response_options", "o1", "respondent_count", "-3")
  problems <- draft_service_check(below, test_lookups())
  expect_true(any(grepl("can't be below 0", problems$message)))
  expect_false(any(grepl("can't be below 0", draft_service_check(draft_service_update(draft, "response_options", "o1", "respondent_count", "0"), test_lookups())$message)))
})

test_that("each variable of a question with several has a label of its own, so the rows of a grid can be told apart", {
  closed <- function(label = NULL, choices = c("often", "never"), subject = NULL) {
    options <- lapply(choices, function(words) list(answer_option = words))
    list(variable_type = "closed_ended", label = label, subject = subject, response_options = options)
  }
  asked <- function(...) {
    draft <- draft_service_from_json(list(questionnaire = list(), components = list(list(
      component_type = "Question", text = "How often did you do each of the following?", printed_number = "5", variables = list(...)
    ))))
    with_matr_ids(draft)
  }
  problems_of <- function(draft) draft_service_check(draft, test_lookups())
  # a single question needs no label; a grid's rows are each labelled with their words
  expect_equal(nrow(problems_of(asked(closed()))), 0)
  expect_equal(nrow(problems_of(asked(closed("Walked to work"), closed("Rode a bike")))), 0)
  # a lone check box says what it records itself ("these symptoms haven't stopped")
  expect_equal(nrow(problems_of(asked(closed("Walked to work"), closed(choices = "never did any of these")))), 0)
  # the same words for someone else are another row ("Age" of Twin A and of Twin B)
  expect_equal(nrow(problems_of(asked(closed("Age", subject = "Twin A"), closed("Age", subject = "Twin B")))), 0)

  unlabelled <- problems_of(asked(closed("Walked to work"), closed()))
  expect_equal(
    unlabelled$message, "Variable 2: label it with its row's words, so it can be told apart from the question's other variables."
  )
  expect_equal(unlabelled$field, "label")
  twice <- problems_of(asked(closed("Walked to work"), closed("walked to work ")))
  expect_equal(twice$message, sprintf("Variable %d: give it a label the question's other variables don't have.", 1:2))
})

test_that("a grid needs the lead-in its questions share, and holds nothing but them and what explains them", {
  question <- list(component_type = "Question", text = "have you felt calm?", printed_number = "1a", variables = list(list(
    variable_type = "open_ended", answer_data_type = "TEXT", matr_id = "m1"
  )))
  checked <- function(grid) {
    draft <- draft_service_from_json(list(questionnaire = list(), components = list(grid)))
    draft_service_check(draft, test_lookups_with_grid())
  }
  test_lookups_with_grid <- function() {
    lookups <- test_lookups()
    lookups$component_types <- union(lookups$component_types, "Grid")
    lookups
  }
  fine <- checked(list(component_type = "Grid", text = "In the past 4 weeks, how often", children = list(
    list(component_type = "Instruction", text = "Mark one."), question
  )))
  expect_equal(nrow(fine), 0)

  bare <- checked(list(component_type = "Grid", children = list(question)))
  expect_equal(bare$message, "Add the lead-in its questions share. Questions that are whole without one go in a Group instead.")
  expect_equal(bare$field, "text")

  holding <- checked(list(component_type = "Grid", text = "In the past 4 weeks,", children = list(
    list(component_type = "Group", children = list(question))
  )))
  expect_equal(holding$message, "A grid holds only its questions, with their instructions and definitions. Move this out of it.")
})
