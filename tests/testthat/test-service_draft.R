test_that("a draft turns back into the same JSON the pipeline returned, and a saved copy opens as it was", {
  json <- gold_json()
  saved <- draft_service_to_json(draft_service_from_json(json))
  # Review adds `schema_version`, `checked` and `question_number` (the question's place) to blocks, `ref`
  # to every row, `matr_id` to variables, `code_value` and `skip_to` (where it skips to) to choices, and names
  # a question's parent by its place rather than its printed number; nothing else changes.
  expect_equal(saved$schema_version, draft_schema_version)
  without_review_fields <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    review_fields <- c("schema_version", "checked", "question_number", "parent_question", "parent_from_skip", "ref", "matr_id", "code_value", "skip_to")
    if (!is.null(names(x))) x <- x[setdiff(names(x), review_fields)]
    lapply(x, without_review_fields)
  }
  as_text <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")
  expect_identical(as_text(without_review_fields(saved)), as_text(without_review_fields(json)))

  # the saved copy opens as the same draft, its rows still matched to the AI's (`ref`)
  expect_identical(draft_service_from_json(saved), gold_draft())
  draft <- gold_draft()
  expect_identical(draft$components$ref, draft$components$id)
  expect_identical(draft$variables$ref, draft$variables$id)
  expect_identical(draft$response_options$ref, draft$response_options$id)
})

test_that("a block keeps the PDF page it was read from, and a block added takes its neighbour's", {
  json <- short_gold_json()
  json$components[[1]]$page <- 2L
  draft <- draft_service_from_json(json)
  first <- draft$components$id[1]
  expect_equal(draft$components$page[1], 2L)
  expect_true(all(is.na(draft$components$page[-1])))
  new_id <- draft_service_next_id(draft, "components")
  added <- draft_service_add_block(draft, "Instruction", new_id, near_id = first)
  expect_equal(added$components$page[added$components$id == new_id], 2L)
  saved <- draft_service_to_json(added)
  expect_equal(saved$components[[1]]$page, 2L)
  expect_identical(draft_service_from_json(draft_service_to_json(draft))$components$page, draft$components$page)
})

test_that("a row keeps its ref however the draft changes, and a row added has none", {
  draft <- gold_draft()
  q1a <- block_numbered(draft, "1a")$id
  added <- draft_service_add_block(draft, "Question", draft_service_next_id(draft, "components"), near_id = q1a)
  added <- draft_service_add_box(added, q1a)
  moved_back <- draft_service_from_json(draft_service_to_json(draft_service_renumber_questions(added)))
  new_block <- moved_back$components[is.na(moved_back$components$ref), ]
  expect_equal(nrow(new_block), 1)
  expect_equal(new_block$component_type, "Question")
  expect_equal(sum(is.na(moved_back$variables$ref)), 1) # the copied variable
  expect_equal(sum(is.na(moved_back$response_options$ref)), 2) # and its two copied choices
  q1a_again <- moved_back$components[moved_back$components$ref %in% q1a, ]
  expect_equal(q1a_again$printed_number, "1a")
})

test_that("blocks are flattened in printed order, numbered within their section", {
  draft <- gold_draft()
  expect_equal(nrow(draft$components), 128)
  expect_equal(nrow(draft$variables), 251)
  expect_equal(nrow(draft$response_options), 330) # printed choices only

  first_three <- draft$components[1:3, ]
  expect_equal(first_three$component_type, c("Group", "Instruction", "Question"))
  expect_equal(first_three$parent_id, c(NA, "c1", "c1"))
  expect_equal(first_three$display_order, c(1, 1, 2))
})

test_that("a follow-up keeps the id of the question it follows, not its number", {
  draft <- gold_draft()
  expect_equal(block_numbered(draft, "8a")$parent_question_id, block_numbered(draft, "7")$id)

  # the reviewed copy names the parent by its place among the questions
  renumbered <- draft
  renumbered$components$question_number[renumbered$components$id == block_numbered(draft, "7")$id] <- "70"
  find_question <- function(blocks, number) {
    for (block in blocks) {
      if (identical(block$printed_number, number)) {
        return(block)
      }
      found <- find_question(block$children, number)
      if (!is.null(found)) {
        return(found)
      }
    }
  }
  q8a <- find_question(draft_service_to_json(renumbered)$components, "8a")
  expect_equal(q8a$parent_question, "70")
})

test_that("a draft opens as Gemini read it, then as staff last saved it", {
  me <- local_staff()
  record <- local_extraction(me)
  expect_error(draft_service_load(test_connection, record$id), "hasn't been read yet")

  expect_true(draft_model_save_reading(test_connection, record$id, short_gold_json(), record$reading_started))
  expect_identical(draft_service_load(test_connection, record$id), short_gold_draft())

  draft <- short_gold_draft()
  draft$components$checked[3] <- TRUE
  draft$response_options$code_value[1:2] <- c("1", "a") # codes are text: a letter too
  draft$variables$matr_id[1] <- "q1a"
  draft_service_save(test_connection, record$id, draft, me$staff_id)
  expect_identical(draft_service_load(test_connection, record$id), draft)
  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$saved_by_id, me$staff_id)
  expect_equal(saved$revision, 2)
  # what the AI returned is never touched by saving
  expect_identical(draft_service_from_json(draft_model_get_both(test_connection, record$id)$original), short_gold_draft())
})

test_that("a draft that isn't in the database is a sentence", {
  me <- local_staff()
  expect_error(draft_service_load(test_connection, "0"), "no longer in the database")
  expect_error(draft_service_save(test_connection, "0", short_gold_draft(), me$staff_id), "no longer in the database")
})

test_that("a draft typed by hand starts empty and can be saved and opened again", {
  me <- local_staff()
  record <- local_manual_extraction(me)
  empty <- draft_service_load(test_connection, record$id)
  expect_equal(nrow(empty$components), 0)
  expect_equal(nrow(empty$variables), 0)
  expect_equal(nrow(empty$response_options), 0)
  expect_identical(empty, draft_service_empty())

  typed <- gold_draft()
  draft_service_save(test_connection, record$id, typed, me$staff_id)
  expect_identical(draft_service_load(test_connection, record$id), typed)
  expect_equal(review_edits_of(test_connection, record$id), list()) # no record of an AI to compare with
})

test_that("choices are coded 1, 2, 3... in each variable", {
  choices <- gold_draft()$response_options
  expect_false(anyNA(choices$code_value))
  expect_identical(choices$code_value, as.character(choices$display_order))
})

test_that("variables are listed question by question in printed order", {
  draft <- gold_draft()
  boxes <- draft$variables[match(draft_service_boxes_in_order(draft), draft$variables$id), ]
  called <- function(draft, ids) draft_service_question_number(draft$components[match(ids, draft$components$id), ])
  numbers <- called(draft, boxes$component_id)
  expect_equal(head(numbers, 2), c("1a", "1b"))
  expect_equal(tail(numbers, 2), c("17", "112")) # the follow-up of 17 has no printed number: its place

  # Moving a question (a drag and drop in the outline) moves its boxes with it.
  q1a <- block_numbered(draft, "1a")$id
  q1b <- block_numbered(draft, "1b")$id
  printed <- draft_service_printed_order(draft)
  swapped <- printed
  swapped[match(c(q1a, q1b), printed)] <- c(q1b, q1a)
  moved <- draft_service_apply_order(draft, swapped, draft$components$parent_id[match(swapped, draft$components$id)])
  moved_boxes <- draft_service_boxes_in_order(moved)
  expect_equal(called(moved, moved$variables$component_id[match(moved_boxes[1:2], moved$variables$id)]), c("1b", "1a"))
})

test_that("printed order walks into each section before moving on", {
  draft <- gold_draft()
  printed <- draft_service_printed_order(draft)
  expect_equal(printed[1:3], c("c1", "c2", "c3"))
  expect_equal(length(printed), nrow(draft$components))
  expect_equal(draft_service_ancestors(draft, "c3"), "c1")
  expect_equal(draft_service_ancestors(draft, "c1"), character())
})

test_that("a draft counts how far its review has got", {
  draft <- gold_draft()
  counts <- draft_service_counts(draft)
  expect_equal(counts$blocks, nrow(draft$components))
  expect_equal(counts$boxes, nrow(draft$variables))
  expect_equal(counts$checked, sum(draft$components$checked))

  draft$components$checked[1:3] <- TRUE
  draft$variables$matr_id[1:2] <- c("q1a", "q1b")
  changed <- draft_service_counts(draft)
  expect_equal(changed$checked, counts$checked + 3 - sum(gold_draft()$components$checked[1:3]))
  expect_equal(changed$matr_ids, sum(!is.na(gold_draft()$variables$matr_id)) + 2 - sum(!is.na(gold_draft()$variables$matr_id[1:2])))
})

test_that("a save is refused if someone saved the draft after the person opened it", {
  me <- local_staff()
  record <- local_read_extraction(me)
  opened <- record$revision
  expect_equal(draft_service_save(test_connection, record$id, short_gold_draft(), me$staff_id, expected_revision = opened), opened + 1)
  expect_error(
    draft_service_save(test_connection, record$id, short_gold_draft(), me$staff_id, expected_revision = opened),
    "changed after you opened it"
  )
})


# ------ Grids --------------------------------------------------------------------------------------------------------
# Questions that share a printed lead-in which is part of each question: the lead-in is the Grid's words, each
# question's are the rest.
grid_draft <- function(lead_in = "In the past 4 weeks, how often", children = NULL) {
  question <- function(text, number) {
    list(component_type = "Question", text = text, printed_number = number, variables = list(list(
      variable_type = "closed_ended", response_options = list(list(answer_option = "Often"), list(answer_option = "Never"))
    )))
  }
  draft_service_from_json(list(questionnaire = list(), components = list(
    list(component_type = "Section", text = "Part A", children = list(
      list(component_type = "Grid", text = lead_in, children = children %||% list(
        list(component_type = "Instruction", text = "Mark one for each line."),
        question("have you felt calm and peaceful?", "20a"),
        question("have you had a lot of energy?", "20b")
      )),
      question("Overall, how is your health?", "21")
    ))
  )))
}

test_that("a grid keeps its words and its skip when saved and opened again", {
  draft <- grid_draft()
  grid <- draft$components$id[draft$components$component_type == "Grid"]
  draft <- draft_service_update(draft, "components", grid, "skip_condition", "No")
  draft <- draft_service_update(draft, "components", grid, "skip_instruction", "Go to Question 21")
  again <- draft_service_from_json(draft_service_to_json(draft))
  kept <- again$components[again$components$component_type == "Grid", ]
  expect_equal(kept$text, "In the past 4 weeks, how often")
  expect_equal(kept$skip_condition, "no")
  expect_equal(kept$skip_instruction, "go to question 21")
})

test_that("a grid holds only its questions with their instructions and definitions, and a section holds a grid", {
  draft <- grid_draft()
  grid <- draft$components$id[draft$components$component_type == "Grid"]
  part <- draft$components$id[draft$components$component_type == "Section"]
  instruction <- draft$components$id[draft$components$component_type == "Instruction"]
  expect_error(draft_service_update(draft, "components", instruction, "component_type", "Section"), "A grid holds only")
  expect_error(draft_service_update(draft, "components", instruction, "component_type", "Group"), "A grid holds only")
  # a grid with a section in it can't be made, nor one holding a group
  expect_error(draft_service_update(draft, "components", part, "component_type", "Grid"), "A grid holds only")
  # its skip stays when it changes kind to a group, and goes when it becomes a section
  draft <- draft_service_update(draft, "components", grid, "skip_condition", "No")
  expect_equal(draft_service_update(draft, "components", grid, "component_type", "Section")$components$skip_condition[draft$components$id == grid], NA_character_)
})

test_that("who a variable is about and the form's words are written with straight quotes, however they are typed", {
  expect_equal(blank_respondent(c("Respondent’s Siblings", " “Twin A” ", "Respondent")), c("Respondent's Siblings", '"Twin A"', NA))
  expect_equal(normal_text("the child’s “sex”"), "the child's \"sex\"")
  draft <- draft_service_update(gold_draft(), "variables", "v1", "subject", "Respondent’s Twin")
  expect_equal(draft$variables$subject[1], "Respondent's Twin")
})

test_that("a write-in's unit is written one way, kept with the draft, and changed like any field", {
  expect_equal(
    normal_unit(c("yrs.", "LBS", "oz", "Cigarettes per day", "Hrs", " ")),
    c("years", "pounds", "ounces", "cigarettes per day", "hours", NA)
  )
  draft <- gold_draft()
  box <- draft$variables$id[draft$variables$variable_type == "open_ended"][1]
  draft <- draft_service_update(draft, "variables", box, "unit", "Lbs.")
  expect_equal(draft$variables$unit[draft$variables$id == box], "pounds")
  again <- draft_service_from_json(draft_service_to_json(draft))
  expect_equal(again$variables$unit[again$variables$id == box], "pounds")
})

test_that("a section, grid or group can say who answers everything in it, and a question its own", {
  draft <- gold_draft()
  grid <- block_numbered(draft, "14a")$parent_id # "during the worst time:"
  draft <- draft_service_update(draft, "components", grid, "condition", "Female")
  expect_equal(component(draft, grid)$condition, "female")
  again <- draft_service_from_json(draft_service_to_json(draft))
  expect_equal(component(again, grid)$condition, "female")
  # a question inside it keeps a condition of its own
  item <- block_numbered(draft, "14a")$id
  draft <- draft_service_update(draft, "components", item, "condition", "If yes")
  expect_equal(component(draft, item)$condition, "if yes")
  # an instruction is for whoever its section or group is for: it has no condition of its own
  instruction <- draft$components$id[draft$components$component_type == "Instruction"][1]
  expect_error(draft_service_update(draft, "components", instruction, "condition", "Female"), "section, grid or group")
})
