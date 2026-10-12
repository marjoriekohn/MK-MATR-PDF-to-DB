# What the reviewer changed in what the AI returned: one entry for each thing that differs between two versions of a
# draft, with who and when. Pure: both drafts are given.

me <- list(staff_id = 7L, name = "Ada Reviewer", role = "STAFF")
at <- as.POSIXct("2026-10-05 09:00:00", tz = "UTC")

edits_between <- function(before, after) draft_service_review_edits(before, after, me, at)

test_that("nothing changed is an empty list", {
  expect_equal(edits_between(gold_draft(), gold_draft()), list())
})

test_that("a changed field is one entry with the value before and after, where it is, who and when", {
  before <- gold_draft()
  q1a <- block_numbered(before, "1a")$id
  after <- draft_service_update(before, "components", q1a, "text", "Would you describe yourself as nervous?")
  edits <- edits_between(before, after)
  expect_length(edits, 1)
  expect_equal(edits[[1]], list(
    at = "2026-10-05T09:00:00Z", by = 7L, by_name = "Ada Reviewer", change = "edited", item = "block",
    where = "Question 1a", field = "text", before = "Would you call yourself a nervous person?",
    after = "Would you describe yourself as nervous?"
  ))
})

test_that("a field that was empty or is emptied has no before or no after", {
  before <- gold_draft()
  q1a <- block_numbered(before, "1a")$id
  with_condition <- draft_service_update(before, "components", q1a, "condition", "If you smoke")
  added <- edits_between(before, with_condition)
  expect_equal(added[[1]]$field, "condition")
  expect_null(added[[1]]$before)
  expect_equal(added[[1]]$after, "If you smoke")
  emptied <- edits_between(with_condition, before)
  expect_equal(emptied[[1]]$before, "If you smoke")
  expect_null(emptied[[1]]$after)
})

test_that("variables and choices are named by where they are", {
  before <- gold_draft()
  q1a <- block_numbered(before, "1a")$id
  variable <- before$variables$id[before$variables$component_id == q1a][1]
  choice <- before$response_options$id[before$response_options$variable_id == variable][1]
  after <- draft_service_update(before, "variables", variable, "label", "Nervous")
  after <- draft_service_update(after, "response_options", choice, "answer_option", "Definitely")
  edits <- edits_between(before, after)
  expect_equal(vapply(edits, function(edit) edit$item, character(1)), c("variable", "choice"))
  expect_equal(edits[[1]]$where, "Question 1a > variable 1")
  expect_equal(edits[[2]]$where, "Question 1a > variable 1 > choice “Definitely”")
  expect_equal(edits[[2]]$before, "Yes")
  expect_equal(edits[[2]]$after, "Definitely")
})

test_that("a block that was added or deleted is one entry that says what it was", {
  before <- gold_draft()
  q1b <- block_numbered(before, "1b")
  gone <- draft_service_delete(before, "components", q1b$id)
  deleted <- edits_between(before, gone)
  expect_true(all(vapply(deleted, function(edit) edit$change, character(1)) %in% "deleted"))
  expect_equal(deleted[[1]]$item, "block")
  expect_equal(deleted[[1]]$where, "Question 1b")
  expect_match(deleted[[1]]$before, "text: Are you a worrier\\?")
  expect_null(deleted[[1]]$after)
  expect_true("variable" %in% vapply(deleted, function(edit) edit$item, character(1))) # its variables went with it
  expect_true("choice" %in% vapply(deleted, function(edit) edit$item, character(1)))

  back <- edits_between(gone, before)
  expect_true(all(vapply(back, function(edit) edit$change, character(1)) == "added"))
  expect_null(back[[1]]$before)
  expect_match(back[[1]]$after, "component_type: Question")
})

test_that("a block moved to another place is an entry that says where it went; neighbours that only renumber don't count", {
  before <- gold_draft()
  q1a <- block_numbered(before, "1a")$id
  q1b <- block_numbered(before, "1b")$id
  printed <- draft_service_printed_order(before)
  swapped <- printed
  swapped[match(c(q1a, q1b), printed)] <- c(q1b, q1a)
  after <- draft_service_apply_order(before, swapped, before$components$parent_id[match(swapped, before$components$id)])
  moved <- Filter(function(edit) edit$change == "moved", edits_between(before, after))
  expect_setequal(vapply(moved, function(edit) edit$where, character(1)), c("Question 1a", "Question 1b")) # the two that swapped
  expect_equal(moved[[1]]$field, "position")
  expect_match(moved[[1]]$before, ", position [0-9]+$")
  expect_false(identical(moved[[1]]$before, moved[[1]]$after))

  # deleting a block renumbers its neighbours, which isn't a move
  gone <- draft_service_delete(before, "components", q1a)
  expect_false("moved" %in% vapply(edits_between(before, gone), function(edit) edit$change, character(1)))
})

test_that("the checked mark and MATR IDs are not recorded", {
  before <- gold_draft()
  after <- before
  after$components$checked[1:5] <- TRUE
  after$variables$matr_id[1:3] <- c("a", "b", "c")
  expect_equal(edits_between(before, after), list())
})

test_that("a follow-up's parent is shown as the question's number, not its id", {
  before <- gold_draft()
  q8a <- block_numbered(before, "8a")$id
  after <- draft_service_update(before, "components", q8a, "parent_question_id", block_numbered(before, "9")$id)
  edit <- edits_between(before, after)[[1]]
  expect_equal(c(edit$field, edit$before, edit$after), c("parent_question_id", "7", "9"))
})
