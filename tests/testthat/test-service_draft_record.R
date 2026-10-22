# How a reviewed copy differs from what the AI returned: one entry for each thing that differs, worked out
# from the two questionnaires alone. Pure: both drafts are given.

original <- function() gold_draft()
edits_of <- function(reviewed) draft_service_review_edits(original(), reviewed)
# The reviewed copy as it is after being saved and opened again (rows get new ids; refs stay).
reopened <- function(draft) draft_service_from_json(draft_service_to_json(draft_service_renumber_questions(draft)))
changes <- function(edits) vapply(edits, function(edit) edit$change, character(1))
q <- function(draft, number) block_numbered(draft, number)$id

test_that("a copy that is what the AI returned has no differences, however often it is saved and opened", {
  expect_equal(edits_of(original()), list())
  expect_equal(edits_of(reopened(reopened(original()))), list())
})

test_that("a changed field is one entry with what the AI returned, what the copy has, which row and where", {
  reviewed <- draft_service_update(original(), "components", q(original(), "1a"), "text", "Would you describe yourself as nervous?")
  edits <- edits_of(reviewed)
  expect_length(edits, 1)
  expect_equal(edits[[1]], list(
    change = "edited", item = "block", ref = q(original(), "1a"), where = "Question 1a", field = "text",
    before = "Would you call yourself a nervous person?", after = "Would you describe yourself as nervous?"
  ))
})

test_that("a value changed several times is one difference, and a value changed back is none", {
  q1a <- q(original(), "1a")
  once <- draft_service_update(original(), "components", q1a, "text", "First try")
  twice <- draft_service_update(reopened(once), "components", q(reopened(once), "1a"), "text", "Second try")
  edits <- edits_of(twice)
  expect_length(edits, 1)
  expect_equal(c(edits[[1]]$before, edits[[1]]$after), c("Would you call yourself a nervous person?", "Second try"))

  back <- draft_service_update(twice, "components", q(twice, "1a"), "text", "Would you call yourself a nervous person?")
  expect_equal(edits_of(back), list())
  expect_equal(edits_of(reopened(back)), list())
})

test_that("values are compared exactly: capitals, punctuation and spaces inside count; blank and missing don't differ", {
  q1a <- q(original(), "1a")
  expect_length(edits_of(draft_service_update(original(), "components", q1a, "text", "would you call yourself a nervous person?")), 1)
  expect_length(edits_of(draft_service_update(original(), "components", q1a, "text", "Would you call yourself a nervous person")), 1)
  expect_length(edits_of(draft_service_update(original(), "components", q1a, "text", "Would you call yourself a  nervous person?")), 1)
  # spaces at the ends are dropped as staff type, and an emptied field is the same as one never filled
  expect_equal(edits_of(draft_service_update(original(), "components", q1a, "text", " Would you call yourself a nervous person? ")), list())
  expect_equal(edits_of(draft_service_update(original(), "components", q1a, "condition", "")), list())
})

test_that("a field that was empty or is emptied has no before or no after", {
  q1a <- q(original(), "1a")
  with_condition <- draft_service_update(original(), "components", q1a, "condition", "If you smoke")
  added <- edits_of(with_condition)
  expect_equal(added[[1]]$field, "condition")
  expect_null(added[[1]]$before)
  expect_equal(added[[1]]$after, "If you smoke")

  # emptied again, it is as the AI returned it
  expect_equal(edits_of(draft_service_update(with_condition, "components", q1a, "condition", "")), list())
})

test_that("variables and choices are named by where they are, and by their row of the AI's answer", {
  q1a <- q(original(), "1a")
  variable <- original()$variables$id[original()$variables$component_id == q1a][1]
  choice <- original()$response_options$id[original()$response_options$variable_id == variable][1]
  reviewed <- draft_service_update(original(), "variables", variable, "label", "Nervous")
  reviewed <- draft_service_update(reviewed, "response_options", choice, "answer_option", "Definitely")
  edits <- edits_of(reviewed)
  expect_equal(vapply(edits, function(edit) edit$item, character(1)), c("variable", "choice"))
  expect_equal(vapply(edits, function(edit) edit$ref, character(1)), c(variable, choice))
  expect_equal(edits[[1]]$where, "Question 1a > variable 1")
  expect_equal(edits[[2]]$where, "Question 1a > variable 1 > choice “Definitely”")
  expect_equal(c(edits[[2]]$before, edits[[2]]$after), c("Yes", "Definitely"))
})

test_that("a block that was added or deleted is an entry that says what it was; one deleted and typed again is both", {
  q1b <- block_numbered(original(), "1b")
  gone <- draft_service_delete(original(), "components", q1b$id)
  deleted <- edits_of(gone)
  expect_true(all(changes(deleted) %in% "deleted"))
  expect_equal(deleted[[1]]$item, "block")
  expect_equal(deleted[[1]]$where, "Question 1b")
  expect_equal(deleted[[1]]$ref, q1b$id)
  expect_match(deleted[[1]]$before, "text: Are you a worrier\\?")
  expect_null(deleted[[1]]$after)
  expect_true(all(c("variable", "choice") %in% vapply(deleted, function(edit) edit$item, character(1)))) # its variables went with it

  # typed in again, it is a new block: the AI's was deleted and the reviewer added one
  retyped <- draft_service_add_block(gone, "Question", draft_service_next_id(gone, "components"), near_id = q(gone, "1a"))
  new_id <- setdiff(retyped$components$id, gone$components$id)
  retyped <- draft_service_update(retyped, "components", new_id, "text", "Are you a worrier?")
  edits <- edits_of(retyped)
  expect_true(all(c("deleted", "added") %in% changes(edits)))
  added <- Filter(function(edit) edit$change == "added", edits)[[1]]
  expect_null(added$ref)
  expect_null(added$before)
  expect_match(added$after, "component_type: Question")
})

test_that("moving one block is one move, wherever it goes; its neighbours that only renumber don't count", {
  q1a <- q(original(), "1a")
  q1b <- q(original(), "1b")
  printed <- draft_service_printed_order(original())
  swapped <- printed
  swapped[match(c(q1a, q1b), printed)] <- c(q1b, q1a)
  after <- draft_service_apply_order(original(), swapped, original()$components$parent_id[match(swapped, original()$components$id)])
  moved <- Filter(function(edit) edit$change == "moved", edits_of(after))
  expect_length(moved, 1) # two swapped neighbours are one block moved
  expect_equal(moved[[1]]$field, "position")
  expect_match(moved[[1]]$before, ", position [0-9]+$")
  expect_false(identical(moved[[1]]$before, moved[[1]]$after))
  expect_length(Filter(function(edit) edit$change == "moved", edits_of(reopened(after))), 1)

  # deleting a block renumbers its neighbours, which isn't a move
  expect_false("moved" %in% changes(edits_of(draft_service_delete(original(), "components", q1a))))
})

test_that("blocks left where a deleted question group was haven't moved: the deleted group is the difference", {
  group <- original()$components$parent_id[match(q(original(), "1a"), original()$components$id)]
  inside <- draft_service_children(original(), "components", group)$id
  # move what is inside to where the group is, then delete the group
  printed <- draft_service_printed_order(original())
  parents <- original()$components$parent_id[match(printed, original()$components$id)]
  parents[printed %in% inside] <- original()$components$parent_id[original()$components$id == group]
  flattened <- draft_service_apply_order(original(), printed, parents)
  flattened <- draft_service_delete(flattened, "components", group)
  edits <- edits_of(flattened)
  expect_equal(changes(edits), "deleted")
  expect_equal(edits[[1]]$ref, group)
})

test_that("what the site works out isn't a difference: codes, places, the checked mark and MATR IDs", {
  reviewed <- original()
  reviewed$components$checked[1:5] <- TRUE
  reviewed$variables$matr_id[1:3] <- c("a", "b", "c")
  reviewed$response_options$code_value[1:2] <- c(0L, 9L)
  expect_equal(edits_of(reviewed), list())
  expect_equal(edits_of(draft_service_copy_codes(reviewed, reviewed$variables$id[1])), list())
})

test_that("a variable about the respondent is the same, written as a blank, Respondent or Self", {
  box <- original()$variables$id[1]
  expect_true(is.na(original()$variables$subject[1]))
  expect_equal(edits_of(draft_service_update(original(), "variables", box, "subject", "Self")), list())
  expect_equal(edits_of(draft_service_update(original(), "variables", box, "subject", "respondent")), list())
  edit <- edits_of(draft_service_update(original(), "variables", box, "subject", "Respondent's Siblings"))[[1]]
  expect_equal(c(edit$field, edit$after), c("subject", "Respondent's Siblings"))
  expect_null(edit$before)
})

test_that("a follow-up's parent is compared by which question it is, and shown by its number", {
  q8a <- q(original(), "8a")
  after <- draft_service_update(original(), "components", q8a, "parent_question_id", q(original(), "9"))
  edit <- edits_of(after)[[1]]
  expect_equal(c(edit$field, edit$before, edit$after), c("parent_question_id", "7", "9"))

  # correcting the parent's printed number changes the parent's number, not which question the follow-up follows
  renumbered <- draft_service_update(original(), "components", q(original(), "7"), "printed_number", "7x")
  expect_equal(changes(edits_of(renumbered)), "edited")
  expect_equal(edits_of(renumbered)[[1]]$field, "printed_number")
})

test_that("the questionnaire's printed name and abbreviation are compared", {
  reviewed <- original()
  reviewed$questionnaire$abbreviation <- "GNA-2"
  edit <- edits_of(reviewed)[[1]]
  expect_equal(c(edit$item, edit$field, edit$before, edit$after), c("questionnaire", "abbreviation", "GNA", "GNA-2"))
})

test_that("the blocks changed from what the AI read: a field, a variable or choice added, changed or taken out, a block added", {
  original <- gold_draft()
  expect_length(draft_service_changed_blocks(original, original), 0)
  expect_null(draft_service_changed_blocks(NULL, original)) # typed by hand: nothing to compare with
  q1a <- block_numbered(original, "1a")$id
  q1b <- block_numbered(original, "1b")$id
  q1c <- block_numbered(original, "1c")$id
  q1d <- block_numbered(original, "1d")$id
  box_1b <- original$variables$id[original$variables$component_id == q1b][1]
  choice_1c <- original$response_options$id[original$response_options$variable_id == original$variables$id[original$variables$component_id == q1c][1]][1]
  box_1d <- original$variables$id[original$variables$component_id == q1d][1]
  reviewed <- draft_service_update(original, "components", q1a, "text", "Reworded")
  reviewed <- draft_service_update(reviewed, "variables", box_1b, "label", "A label")
  reviewed <- draft_service_update(reviewed, "response_options", choice_1c, "printed_label", "Y")
  reviewed <- suppressMessages(draft_service_delete(reviewed, "variables", box_1d))
  added <- draft_service_next_id(reviewed, "components")
  reviewed <- draft_service_add_block(reviewed, "Instruction", added, near_id = q1a)
  expect_setequal(draft_service_changed_blocks(original, reviewed), c(q1a, q1b, q1c, q1d, added))
  # the checked mark and a MATR ID aren't what the AI reads
  ticked <- draft_service_update(original, "components", q1a, "checked", TRUE)
  ticked <- draft_service_update(ticked, "variables", box_1b, "matr_id", "GNA_9")
  expect_length(draft_service_changed_blocks(original, ticked), 0)
})
