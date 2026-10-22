# Skip instructions linked to where they send respondents, and the parents worked out from them.

# A small questionnaire: question 1 ("No" goes to question 3), 2a and 2b (skipped by "No"), then 3, and a
# section headed "Section B" holding question 1 again (printed numbers can repeat).
skip_json <- function(skip = "Go to Question 3", parent_of_2a = NULL) {
  question <- function(number, choices = list(list(answer_option = "Yes"), list(answer_option = "No")), parent = NULL) {
    list(
      component_type = "Question", text = paste("Question", number), printed_number = number, self_contained = TRUE,
      parent_question = parent,
      variables = list(list(variable_type = "closed_ended", response_options = choices))
    )
  }
  list(components = list(
    question("1", list(list(answer_option = "Yes"), list(answer_option = "No", skip_instruction = skip))),
    list(component_type = "Question Group", children = list(question("2a", parent = parent_of_2a), question("2b"))),
    question("3"),
    list(component_type = "Section", text = "Section B. Your health", children = list(question("1")))
  ))
}
id_of <- function(draft, number, which = 1) {
  questions <- draft$components[draft$components$component_type == "Question", ]
  questions$id[draft_service_question_number(questions) %in% number][which]
}
no_choice <- function(draft) draft$response_options$id[draft$response_options$answer_option %in% "No"][1]

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
  expect_true(is.na(draft_service_skip_target(draft, no_choice(draft), "End of survey")))
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
  expect_equal(targets$label[nrow(targets) - 1], "Section · Section B. Your health")
  at <- function(d) d$response_options$skip_to_id[d$response_options$id == no]
  expect_equal(at(draft_service_update(draft, "response_options", no, "skip_to_id", "2b · Question 2b")), id_of(draft, "2b"))
  expect_equal(at(draft_service_update(draft, "response_options", no, "skip_to_id", " 2b ")), id_of(draft, "2b"))
  section <- draft$components$id[draft$components$component_type == "Section"]
  expect_equal(at(draft_service_update(draft, "response_options", no, "skip_to_id", "Section · Section B. Your health")), section)
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
  expect_equal(problems$message, "Variable 1: pick where choice “No” skips to.")
  expect_equal(problems$field, "skip_to_id")
  back <- draft_service_update(draft, "response_options", no_choice(draft), "skip_to_id", id_of(draft, "1"))
  expect_match(draft_service_check(back, lookups)$message, "skips back to something printed before it")
  # "End of survey" names nothing, so it needs no link
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
