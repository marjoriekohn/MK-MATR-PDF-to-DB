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

test_that("a section needs its printed heading, a question group has none, and a section isn't inside a group", {
  draft <- with_matr_ids()
  group <- draft$components$parent_id[draft$components$id == block_numbered(draft, "1a")$id]
  problems_of <- function(broken) draft_service_check(broken, test_lookups())
  expect_equal(nrow(problems_of(draft)), 0) # the gold questionnaire's groups have no words

  worded <- draft
  worded$components$text[worded$components$id == group] <- "Part A"
  expect_true(any(grepl("question group has no words of its own", problems_of(worded)$message)))

  as_section <- draft_service_update(draft, "components", group, "component_type", "Section")
  heading <- problems_of(as_section)
  expect_equal(heading$message[heading$id == group], "Add the section's printed heading. A group with no heading is a question group.")
  expect_equal(heading$field[heading$id == group], "text")
  expect_equal(nrow(problems_of(draft_service_update(as_section, "components", group, "text", "Part A"))), 0)

  # a section the AI put inside a group is pointed out
  nested <- draft
  inner <- draft_service_next_id(nested, "components")
  nested <- draft_service_add_block(nested, "Question Group", inner, near_id = group)
  nested$components$component_type[nested$components$id == inner] <- "Section"
  nested$components$text[nested$components$id == inner] <- "Part B"
  expect_true(any(problems_of(nested)$id == inner & grepl("can't be inside a question group", problems_of(nested)$message)))
})
