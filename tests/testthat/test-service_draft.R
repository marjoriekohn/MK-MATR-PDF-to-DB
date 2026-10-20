test_that("a draft turns back into the same JSON the pipeline returned", {
  json <- gold_json()
  saved <- draft_service_to_json(draft_service_from_json(json))
  # Review adds `checked` to blocks, `printed_number` to questions (the number read off the PDF, kept
  # apart from its place), `matr_id` to variables and `code_value` to choices; nothing else changes.
  without_review_fields <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    if (!is.null(names(x))) x <- x[setdiff(names(x), c("checked", "printed_number", "matr_id", "code_value"))]
    lapply(x, without_review_fields)
  }
  as_text <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")
  expect_identical(as_text(without_review_fields(saved)), as_text(json))
})

test_that("blocks are flattened in printed order, numbered within their section", {
  draft <- gold_draft()
  expect_equal(nrow(draft$components), 130)
  expect_equal(nrow(draft$variables), 251)
  expect_equal(nrow(draft$response_options), 330) # printed choices only

  first_three <- draft$components[1:3, ]
  expect_equal(first_three$component_type, c("Section", "Instruction", "Question"))
  expect_equal(first_three$parent_id, c(NA, "c1", "c1"))
  expect_equal(first_three$display_order, c(1, 1, 2))
})

test_that("a follow-up keeps the id of the question it follows, not its number", {
  draft <- gold_draft()
  expect_equal(block_numbered(draft, "8a")$parent_question_id, block_numbered(draft, "7")$id)

  renumbered <- draft
  renumbered$components$question_number[renumbered$components$id == block_numbered(draft, "7")$id] <- "70"
  find_question <- function(blocks, number) {
    for (block in blocks) {
      if (identical(block$question_number, number)) {
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

  expect_true(draft_model_save_reading(test_connection, record$id, gold_json(), record$reading_started))
  expect_identical(draft_service_load(test_connection, record$id), gold_draft())

  draft <- gold_draft()
  draft$components$checked[3] <- TRUE
  draft$response_options$code_value[1:2] <- c(1L, 0L)
  draft$variables$matr_id[1] <- "q1a"
  draft_service_save(test_connection, record$id, draft, me$staff_id)
  expect_identical(draft_service_load(test_connection, record$id), draft)
  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$saved_by_id, me$staff_id)
  expect_equal(saved$revision, 2)
  # what the AI returned is never touched by saving
  expect_identical(draft_service_from_json(draft_model_get_record(test_connection, record$id)$original), gold_draft())
})

test_that("a draft that isn't in the database is a sentence", {
  me <- local_staff()
  expect_error(draft_service_load(test_connection, "0"), "no longer in the database")
  expect_error(draft_service_save(test_connection, "0", gold_draft(), me$staff_id), "no longer in the database")
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
  expect_equal(draft_model_get_record(test_connection, record$id)$edits, list()) # no record of an AI to compare with
})

test_that("choices are coded 1, 2, 3... in each variable", {
  choices <- gold_draft()$response_options
  expect_false(anyNA(choices$code_value))
  expect_identical(choices$code_value, as.integer(choices$display_order))
})

test_that("variables are listed question by question in printed order", {
  draft <- gold_draft()
  boxes <- draft$variables[match(draft_service_boxes_in_order(draft), draft$variables$id), ]
  numbers <- question_number_of(draft, boxes$component_id)
  expect_equal(head(numbers, 2), c("1a", "1b"))
  expect_equal(tail(numbers, 2), c("17a", "17b"))

  # Moving a question (a drag and drop in the outline) moves its boxes with it.
  q1a <- block_numbered(draft, "1a")$id
  q1b <- block_numbered(draft, "1b")$id
  printed <- draft_service_printed_order(draft)
  swapped <- printed
  swapped[match(c(q1a, q1b), printed)] <- c(q1b, q1a)
  moved <- draft_service_apply_order(draft, swapped, draft$components$parent_id[match(swapped, draft$components$id)])
  moved_boxes <- draft_service_boxes_in_order(moved)
  expect_equal(question_number_of(moved, moved$variables$component_id[match(moved_boxes[1:2], moved$variables$id)]), c("1b", "1a"))
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
