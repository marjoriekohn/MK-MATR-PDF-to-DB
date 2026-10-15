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
  expect_true(problem_for(edit("components", q1a, "question_number", ""), "Add the question number"))
  expect_true(problem_for(edit("components", q1a, "question_number", "1B"), "no other question has"))
  expect_true(problem_for(edit("components", q1a, "self_contained", "FALSE"), "Pick the question it follows"))
  expect_true(problem_for(edit("components", q1a, "parent_question_id", q1b), "printed before it"))
  expect_true(problem_for(suppressMessages(draft_service_delete(draft, "variables", box)), "Add at least one variable"))
  expect_true(problem_for(suppressMessages(draft_service_delete(draft, "response_options", choices[2])), "add at least two choices"))
  expect_true(problem_for(draft_service_add_choice(draft, box), "fill in the blank choice"))
  expect_true(problem_for(edit("response_options", choices[2], "answer_option", "Yes"), "list each choice once"))

  same_codes <- edit("response_options", choices[1], "code_value", "1")
  same_codes <- draft_service_update(same_codes, "response_options", choices[2], "code_value", "1")
  expect_true(problem_for(same_codes, "its own code"))

  expect_true(problem_for(edit("variables", box, "subject", "Respondents Family"), "who it's about from the list"))
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
  whole_box <- field_of(suppressMessages(draft_service_delete(draft, "response_options", choices[2])), "two choices")
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
