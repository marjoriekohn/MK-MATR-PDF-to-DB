# ------ Moving -------------------------------------------------------------------
test_that("drag and drop results are applied, and impossible ones refused", {
  draft <- gold_draft()
  ids <- draft_service_printed_order(draft)
  parent_ids <- draft$components$parent_id[match(ids, draft$components$id)]
  expect_identical(draft_service_apply_order(draft, ids, parent_ids), draft)

  q1a <- block_numbered(draft, "1a")$id
  to_the_end <- draft_service_apply_order(draft, c(setdiff(ids, q1a), q1a), c(parent_ids[ids != q1a], NA))
  expect_equal(tail(draft_service_children(to_the_end, "components", NA)$id, 1), q1a)

  into_a_question <- replace(parent_ids, ids == "c4", q1a)
  expect_error(draft_service_apply_order(draft, ids, into_a_question), "Only sections")
  into_itself <- replace(parent_ids, ids == "c1", "c1")
  expect_error(draft_service_apply_order(draft, ids, into_itself), "inside itself")
  expect_error(draft_service_apply_order(draft, ids[-1], parent_ids[-1]), "out of date")
})

test_that("variables are reordered within their question", {
  draft <- gold_draft()
  q <- block_numbered(draft, "3#3")$id
  box_ids <- draft_service_children(draft, "variables", q)$id
  reordered <- draft_service_reorder(draft, "variables", rev(box_ids))
  expect_equal(draft_service_children(reordered, "variables", q)$id, rev(box_ids))
})

# ------ Adding and deleting --------------------------------------------------------
test_that("a new block goes after the selected one, or inside a selected section", {
  draft <- gold_draft()
  id <- draft_service_next_id(draft, "components")
  expect_equal(id, paste0("c", nrow(draft$components) + 1)) # one past the last block

  after_1a <- draft_service_add_block(draft, "Note", id, near_id = "c3")
  expect_equal(draft_service_children(after_1a, "components", "c1")$id[3], id)

  inside_section <- draft_service_add_block(draft, "Note", id, near_id = "c1")
  expect_equal(tail(draft_service_children(inside_section, "components", "c1")$id, 1), id)

  section_after_section <- draft_service_add_block(draft, "Section", id, near_id = "c1")
  expect_equal(draft_service_children(section_after_section, "components", NA)$id[2], id)

  at_the_end <- draft_service_add_block(draft, "Question", id)
  expect_equal(tail(draft_service_printed_order(at_the_end), 1), id)
  expect_true(at_the_end$components$self_contained[at_the_end$components$id == id])
})

# The page compares drafts to tell staff whether they have unsaved changes.
test_that("adding something and deleting it again gives back the same draft", {
  draft <- gold_draft()
  block <- draft_service_next_id(draft, "components")
  with_block <- draft_service_add_block(draft, "Question", block, near_id = "c3")
  expect_identical(suppressMessages(draft_service_delete(with_block, "components", block)), draft)

  box <- draft_service_next_id(draft, "variables")
  with_box <- draft_service_add_box(draft, "c3")
  expect_identical(suppressMessages(draft_service_delete(with_box, "variables", box)), draft)
})

test_that("deleting a section takes its blocks, variables and choices with it", {
  draft <- gold_draft()
  gone <- c("c1", draft_service_printed_order(draft, "c1"))
  expect_message(deleted <- draft_service_delete(draft, "components", "c1"), "Undo")
  expect_false(any(deleted$components$id %in% gone))
  expect_false(any(deleted$variables$component_id %in% gone))
  expect_true(all(deleted$response_options$variable_id %in% deleted$variables$id))
  expect_equal(draft_service_children(deleted, "components", NA)$display_order, 1:15)
})

test_that("questions that followed a deleted question lose the link", {
  draft <- gold_draft()
  q7 <- block_numbered(draft, "7")$id
  deleted <- suppressMessages(draft_service_delete(draft, "components", q7))
  expect_false(any(deleted$components$parent_question_id %in% q7))
})

test_that("a new variable copies the question's last variable", {
  draft <- gold_draft()
  q <- block_numbered(draft, "3#3")$id
  before <- draft_service_children(draft, "variables", q)
  added <- draft_service_add_box(draft, q)
  after <- draft_service_children(added, "variables", q)
  new_box <- after[nrow(after), ]

  expect_equal(nrow(after), nrow(before) + 1)
  expect_equal(new_box$subject, before$subject[nrow(before)])
  expect_true(is.na(new_box$label))
  expect_equal(
    draft_service_children(added, "response_options", new_box$id)$answer_option,
    c("Male", "Female")
  )
})

# ------ Changing fields --------------------------------------------------------------
test_that("what staff type is stored as the column's type, blanks as NA", {
  draft <- gold_draft()
  expect_identical(
    draft_service_update(draft, "response_options", "o1", "code_value", "3")$response_options$code_value[1],
    3L
  )
  expect_false(draft_service_update(draft, "components", "c3", "self_contained", "FALSE")$components$self_contained[3])
  expect_true(is.na(draft_service_update(draft, "components", "c3", "condition", "  ")$components$condition[3]))
  expect_error(draft_service_update(draft, "components", "c3", "id", "c9"), "can't be changed")
})

test_that("a block's kind decides what it can hold", {
  draft <- gold_draft()
  expect_message(
    note <- draft_service_update(draft, "components", "c3", "component_type", "Note"),
    "Removed 1 variable"
  )
  expect_false("c3" %in% note$variables$component_id)
  expect_true(is.na(note$components$question_number[3]))
  expect_true(is.na(note$components$printed_number[3]))
  expect_error(
    draft_service_update(draft, "components", "c1", "component_type", "Note"),
    "Move the blocks out"
  )
})

test_that("a write-in box has no choices; switching back starts one blank one", {
  draft <- gold_draft()
  box <- draft$variables$id[draft$variables$variable_type == "closed_ended"][1]
  write_in <- draft_service_update(draft, "variables", box, "variable_type", "open_ended")
  expect_false(box %in% write_in$response_options$variable_id)

  choices_again <- draft_service_update(write_in, "variables", box, "variable_type", "closed_ended")
  expect_equal(nrow(draft_service_children(choices_again, "response_options", box)), 1)
})

test_that("every choice is printed: there is no field to say one isn't, and a lone check box has one choice", {
  draft <- gold_draft()
  expect_false("printed" %in% names(draft$response_options))
  expect_error(draft_service_update(draft, "response_options", "o1", "printed", "FALSE"), "can't be changed")
  draft <- draft_service_update(draft, "response_options", "o1", "printed_label", "Yes, often")
  draft <- draft_service_update(draft, "response_options", "o1", "skip_instruction", "Go to Q5")
  choice <- draft$response_options[draft$response_options$id == "o1", ]
  expect_equal(choice$printed_label, "Yes, often")
  expect_equal(choice$skip_instruction, "Go to Q5")

  # a draft saved with a choice that wasn't printed (a lone check box's "No") opens without it
  json <- list(components = list(list(
    component_type = "Question", text = "All of my life", question_number = "3", self_contained = TRUE,
    variables = list(list(variable_type = "closed_ended", response_options = list(
      list(answer_option = "Yes", printed = TRUE), list(answer_option = "No", printed = FALSE)
    )))
  )))
  opened <- draft_service_from_json(json)
  expect_equal(opened$response_options$answer_option, "Yes")
  expect_null(draft_service_to_json(opened)$components[[1]]$variables[[1]]$response_options[[1]][["printed"]])
})

test_that("a question keeps the number printed beside it as staff type it; its place follows the outline", {
  draft <- gold_draft()
  q <- block_numbered(draft, "1a")$id
  expect_equal(draft$components$printed_number[draft$components$id == q], "1a") # read off the PDF
  typed <- draft_service_update(draft, "components", q, "printed_number", " 1a4 ")
  expect_equal(typed$components$printed_number[typed$components$id == q], "1a4")
  renumbered <- draft_service_renumber_questions(typed)
  row <- renumbered$components[renumbered$components$id == q, ]
  expect_equal(row$question_number, "1") # its place
  expect_equal(row$printed_number, "1a4") # left as typed
  expect_equal(draft_service_question_number(row), "1a4")
  blank <- draft_service_update(renumbered, "components", q, "printed_number", "")
  expect_equal(draft_service_question_number(blank$components[blank$components$id == q, ]), "1") # asked as its place
  expect_equal(draft_service_question_name(blank, q), "question 1")
  # a saved copy keeps the typed number
  expect_equal(draft_service_from_json(draft_service_to_json(renumbered))$components$printed_number, renumbered$components$printed_number)
  # a block that isn't a question has none
  expect_true(all(is.na(draft$components$printed_number[draft$components$component_type != "Question"])))
})

test_that("codes typed for one box are used in every box with the same choices", {
  draft <- gold_draft()
  box <- draft$variables$id[1]
  yes_no <- draft_service_children(draft, "response_options", box)
  expect_equal(yes_no$answer_option, c("Yes", "No"))
  blank <- draft_service_update(draft, "response_options", yes_no$id[1], "code_value", "")
  expect_error(draft_service_copy_codes(blank, box), "code first")

  draft <- draft_service_update(draft, "response_options", yes_no$id[1], "code_value", "1")
  draft <- draft_service_update(draft, "response_options", yes_no$id[2], "code_value", "0")
  copied <- suppressMessages(draft_service_copy_codes(draft, box))

  choices <- copied$response_options
  is_yes_no <- tapply(choices$answer_option, choices$variable_id, paste, collapse = "/") == "Yes/No"
  in_yes_no_box <- choices$variable_id %in% names(which(is_yes_no))
  yes_no_choices <- choices[in_yes_no_box, ]
  expect_true(all(yes_no_choices$code_value == ifelse(yes_no_choices$answer_option == "Yes", 1L, 0L)))
  expect_identical(choices$code_value[!in_yes_no_box], draft$response_options$code_value[!in_yes_no_box])
})

test_that("new choices take the next code in their box, counting from 1", {
  draft <- gold_draft()
  box <- draft$variables$id[1]
  codes_in <- function(draft, box) draft_service_children(draft, "response_options", box)$code_value

  expect_equal(codes_in(draft_service_add_choice(draft, box), box), 1:3)

  yes_no <- draft_service_children(draft, "response_options", box)$id
  zero_one <- draft_service_update(draft, "response_options", yes_no[1], "code_value", "0")
  zero_one <- draft_service_update(zero_one, "response_options", yes_no[2], "code_value", "1")
  expect_equal(codes_in(draft_service_add_choice(zero_one, box), box), 0:2)

  write_in <- draft_service_update(draft, "variables", box, "variable_type", "open_ended")
  choices_again <- draft_service_update(write_in, "variables", box, "variable_type", "closed_ended")
  expect_equal(codes_in(choices_again, box), 1L)

  # Other boxes keep the codes they have, even blank ones.
  other <- draft$variables$id[2]
  other_choice <- draft_service_children(draft, "response_options", other)$id[1]
  blanked <- draft_service_update(draft, "response_options", other_choice, "code_value", "")
  expect_true(is.na(codes_in(draft_service_add_choice(blanked, box), other)[1]))
})

# ------ MATR IDs ---------------------------------------------------------------------
test_that("Auto Apply gives the variables the CSV's MATR IDs in printed order", {
  draft <- gold_draft()
  ids <- extraction_service_matr_ids(extraction_model_read_csv(fixture_path("gna_matr_ids.csv")))
  expect_length(ids, nrow(draft$variables))

  expect_message(filled <- draft_service_auto_apply_matr_ids(draft, ids), "Gave 251 variables a MATR ID")
  boxes <- filled$variables
  id_for <- function(number) boxes$matr_id[boxes$component_id == block_numbered(filled, number)$id]
  expect_equal(id_for("1a"), "q1a")
  expect_equal(id_for("15ao"), c("q15ao1", "q15ao2", "q15ao3", "q15ao4"))
  expect_equal(id_for("17b"), "q17b")
  expect_false(anyNA(boxes$matr_id))
  expect_equal(nrow(draft_service_check(filled, test_lookups())), 0)
})

test_that("a CSV with too few or too many MATR IDs fills what it can, loses nothing and says so", {
  draft <- gold_draft()
  n <- nrow(draft$variables)
  ids <- paste0("id", seq_len(n + 2))
  last_box <- function(draft) {
    draft$variables$matr_id[match(tail(draft_service_boxes_in_order(draft), 1), draft$variables$id)]
  }

  expect_message(short <- draft_service_auto_apply_matr_ids(draft, ids[1:(n - 1)]), "1 variable still has none")
  expect_true(is.na(last_box(short)))
  expect_message(long <- draft_service_auto_apply_matr_ids(draft, ids), "2 uploaded IDs are left over")
  expect_equal(last_box(long), ids[n])
  expect_equal(draft_service_available_matr_ids(long, ids), ids[n + 1:2]) # still offered
  expect_error(draft_service_auto_apply_matr_ids(draft, character()), "no MATR IDs")

  typed <- draft_service_update(draft, "variables", draft$variables$id[1], "matr_id", "  AH1  ")
  expect_equal(typed$variables$matr_id[1], "AH1")
})

test_that("Auto Apply keeps the MATR IDs already given, unless told to replace them", {
  draft <- gold_draft()
  in_order <- draft_service_boxes_in_order(draft)
  ids <- paste0("id", seq_along(in_order))
  picked <- draft_service_update(draft, "variables", in_order[2], "matr_id", "id5") # picked by hand

  kept <- suppressMessages(draft_service_auto_apply_matr_ids(picked, ids))
  of <- function(draft, box) draft$variables$matr_id[draft$variables$id == box]
  expect_equal(of(kept, in_order[2]), "id5")
  expect_equal(of(kept, in_order[1]), "id1")
  expect_equal(of(kept, in_order[3]), "id2") # the next ID no variable has
  expect_false(any(duplicated(stats::na.omit(kept$variables$matr_id))))

  replaced <- suppressMessages(draft_service_auto_apply_matr_ids(picked, ids, replace = TRUE))
  expect_equal(of(replaced, in_order[2]), "id2")
})

test_that("a MATR ID goes to one variable only, typed or picked, and the drop-downs offer the rest", {
  draft <- gold_draft()
  boxes <- draft_service_boxes_in_order(draft)
  draft <- draft_service_update(draft, "variables", boxes[1], "matr_id", "AH1")
  expect_error(draft_service_update(draft, "variables", boxes[2], "matr_id", " AH1 "), "already the MATR ID of variable")
  expect_equal(draft_service_update(draft, "variables", boxes[1], "matr_id", "AH1")$variables$matr_id[draft$variables$id == boxes[1]], "AH1")
  expect_equal(draft_service_available_matr_ids(draft, c("AH1", "AH2", "AH2", " ", "AH3")), c("AH2", "AH3"))
  expect_message(draft_service_auto_apply_matr_ids(gold_draft(), c("A", "B", "A")), "lists A more than once")
})

test_that("a new variable copying a write-in has no choices to copy", {
  draft <- gold_draft()
  q <- block_numbered(draft, "3#3")$id
  box <- draft_service_children(draft, "variables", q)$id[1]
  write_in <- draft_service_update(draft, "variables", box, "variable_type", "open_ended")
  write_in$variables <- write_in$variables[!(write_in$variables$component_id == q & write_in$variables$id != box), ]
  added <- draft_service_add_box(write_in, q)
  after <- draft_service_children(added, "variables", q)
  expect_equal(nrow(after), 2)
  expect_equal(after$variable_type, c("open_ended", "open_ended"))
  expect_equal(nrow(draft_service_children(added, "response_options", after$id[2])), 0)
  expect_length(draft_service_next_id(draft, "response_options", 0), 0)
})

# ------ The edges of the rules above ----------------------------------------------------------
test_that("a block can't be given no kind, and an instruction made a question can stand on its own", {
  draft <- gold_draft()
  expect_error(draft_service_update(draft, "components", "c3", "component_type", ""), "Pick a kind of block.")

  changed <- draft_service_update(draft, "components", "c2", "component_type", "Question")
  block <- component(changed, "c2")
  expect_equal(block$component_type, "Question")
  expect_true(block$self_contained) # a question is standalone until it follows another
})

test_that("a box is either choices or a write-in, and picking what it already is changes nothing", {
  draft <- gold_draft()
  expect_error(draft_service_update(draft, "variables", "v1", "variable_type", "something else"), "Pick choices or write-in.")
  expect_identical(draft_service_update(draft, "variables", "v1", "variable_type", draft$variables$variable_type[1]), draft)
})

test_that("a question that has no variable yet gets one with a blank choice", {
  draft <- draft_service_update(gold_draft(), "components", "c2", "component_type", "Question")
  expect_equal(nrow(draft_service_children(draft, "variables", "c2")), 0)
  added <- draft_service_add_box(draft, "c2")
  box <- draft_service_children(added, "variables", "c2")
  expect_equal(nrow(box), 1)
  expect_equal(nrow(draft_service_children(added, "response_options", box$id)), 1)
})

test_that("the sentence about a CSV with too few or too many IDs counts one and several, in the singular and plural", {
  expect_null(draft_service_matr_id_mismatch(5, 5))
  expect_equal(draft_service_matr_id_mismatch(4, 5), "The CSV has 4 MATR IDs for 5 variables, so the last variable gets none.")
  expect_equal(draft_service_matr_id_mismatch(2, 5), "The CSV has 2 MATR IDs for 5 variables, so the last 3 variables get none.")
  expect_equal(draft_service_matr_id_mismatch(6, 5), "The CSV has 6 MATR IDs for 5 variables, so its last ID is left over.")
  expect_equal(draft_service_matr_id_mismatch(9, 5), "The CSV has 9 MATR IDs for 5 variables, so its last 4 IDs are left over.")
  expect_equal(draft_service_matr_id_mismatch(1, 3), "The CSV has 1 MATR ID for 3 variables, so the last 2 variables get none.")
})

# ------ Checked blocks ------------------------------------------------------------------------
checked_gold <- function() with_checked(gold_draft(), c("c1", "c2", "c3", "c4", "c5"))

test_that("changing a checked block's words takes the check off that block, and only that block", {
  before <- checked_gold()
  after <- draft_service_uncheck_edited(before, draft_service_update(before, "components", "c3", "text", "Do you worry a lot?"))
  expect_equal(checked_blocks(after), c("c1", "c2", "c4", "c5"))
  expect_equal(component(after, "c3")$text, "Do you worry a lot?")
})

test_that("each field of a block, when changed, takes its check off", {
  before <- checked_gold()
  edits <- list(
    list("printed_number", "9z"), list("self_contained", "FALSE"), list("condition", "If yes to 1a"), list("parent_question_id", "c4")
  )
  for (edit in edits) {
    after <- draft_service_uncheck_edited(before, suppressMessages(draft_service_update(before, "components", "c5", edit[[1]], edit[[2]])))
    expect_false("c5" %in% checked_blocks(after), info = edit[[1]])
    expect_true("c4" %in% checked_blocks(after), info = edit[[1]])
  }
})

test_that("marking a block as checked or not checked is not an edit", {
  before <- checked_gold()
  off <- draft_service_uncheck_edited(before, draft_service_update(before, "components", "c3", "checked", FALSE))
  expect_equal(checked_blocks(off), c("c1", "c2", "c4", "c5"))
  on <- draft_service_uncheck_edited(off, draft_service_update(off, "components", "c3", "checked", TRUE))
  expect_equal(checked_blocks(on), c("c1", "c2", "c3", "c4", "c5"))
})

test_that("changing a value to what it already is changes nothing, and checks stay", {
  before <- checked_gold()
  same <- draft_service_update(before, "components", "c3", "text", component(before, "c3")$text)
  expect_identical(draft_service_uncheck_edited(before, same), before)
})

test_that("changing a variable or one of its choices takes the check off the block it belongs to", {
  before <- checked_gold()
  box <- draft_service_update(before, "variables", "v1", "label", "Nervous")
  expect_equal(checked_blocks(draft_service_uncheck_edited(before, box)), c("c1", "c2", "c4", "c5"))
  choice <- draft_service_update(before, "response_options", "o1", "answer_option", "Often")
  expect_equal(checked_blocks(draft_service_uncheck_edited(before, choice)), c("c1", "c2", "c4", "c5"))
  code <- draft_service_update(before, "response_options", "o3", "code_value", "7")
  expect_equal(checked_blocks(draft_service_uncheck_edited(before, code)), c("c1", "c2", "c3", "c5")) # o3 is in c4's box
})

test_that("adding, deleting or reordering variables and choices takes the check off", {
  before <- checked_gold()
  uncheck <- function(after) checked_blocks(draft_service_uncheck_edited(before, after))
  expect_equal(uncheck(draft_service_add_box(before, "c3")), c("c1", "c2", "c4", "c5"))
  expect_equal(uncheck(draft_service_add_choice(before, "v3")), c("c1", "c2", "c3", "c4"))
  expect_equal(uncheck(suppressMessages(draft_service_delete(before, "response_options", "o1"))), c("c1", "c2", "c4", "c5"))
  with_two_boxes <- draft_service_add_box(gold_draft(), "c3")
  two <- with_checked(with_two_boxes, "c3")
  boxes <- draft_service_children(two, "variables", "c3")$id
  reordered <- draft_service_reorder(two, "variables", rev(boxes))
  expect_equal(checked_blocks(draft_service_uncheck_edited(two, reordered)), character())
})

test_that("using one box's codes in the others takes the check off only the blocks whose codes changed", {
  before <- with_checked(gold_draft(), c("c3", "c4", "c5"))
  before <- draft_service_update(before, "response_options", "o1", "code_value", "1")
  before <- draft_service_update(before, "response_options", "o2", "code_value", "0")
  before <- with_checked(before, c("c3", "c4", "c5")) # editing the codes above took the check off c3
  expect_equal(checked_blocks(before), c("c3", "c4", "c5"))
  after <- draft_service_uncheck_edited(before, suppressMessages(draft_service_copy_codes(before, "v1")))
  # c4 and c5 have Yes/No boxes whose codes were 1 and 2; c3 is the box copied from and is unchanged
  expect_equal(checked_blocks(after), "c3")
})

test_that("a block moved into another section loses its check, but one moved among its neighbours does not", {
  before <- checked_gold()
  printed <- draft_service_printed_order(before)
  parents <- before$components$parent_id[match(printed, before$components$id)]
  within <- printed
  within[match(c("c3", "c4"), printed)] <- c("c4", "c3")
  same_section <- draft_service_apply_order(before, within, before$components$parent_id[match(within, before$components$id)])
  expect_equal(checked_blocks(draft_service_uncheck_edited(before, same_section)), c("c1", "c2", "c3", "c4", "c5"))

  parents[match("c3", printed)] <- NA # out of its section, to the top level
  elsewhere <- draft_service_apply_order(before, printed, parents)
  expect_equal(checked_blocks(draft_service_uncheck_edited(before, elsewhere)), c("c1", "c2", "c4", "c5"))
})

test_that("deleting a question takes the check off the follow-ups that lose it", {
  draft <- with_checked(gold_draft(), c("c3", "c4", "c5"))
  q1b <- block_numbered(draft, "1b")$id
  draft <- suppressMessages(draft_service_update(draft, "components", q1b, "self_contained", "FALSE"))
  draft <- suppressMessages(draft_service_update(draft, "components", q1b, "parent_question_id", "c3"))
  draft <- with_checked(draft, c("c3", "c4", "c5"))
  after <- draft_service_uncheck_edited(draft, suppressMessages(draft_service_delete(draft, "components", "c3")))
  expect_false("c3" %in% after$components$id)
  expect_false(q1b %in% checked_blocks(after))
  expect_true("c5" %in% checked_blocks(after))
})

test_that("a new block starts unchecked, and a block that wasn't checked stays as it is", {
  before <- checked_gold()
  added <- draft_service_add_block(before, "Instruction", draft_service_next_id(before, "components"), near_id = "c3")
  expect_identical(draft_service_uncheck_edited(before, added), added)
  expect_equal(checked_blocks(added), c("c1", "c2", "c3", "c4", "c5"))
  untouched <- gold_draft()
  edited <- draft_service_update(untouched, "components", "c3", "text", "Different")
  expect_identical(draft_service_uncheck_edited(untouched, edited), edited)
})

# ------ Question numbers follow the outline -----------------------------------------------------------
test_that("questions are numbered 1, 2, 3... in printed order, and renumbered when one moves or goes", {
  draft <- draft_service_renumber_questions(gold_draft())
  printed <- draft_service_printed_order(draft)
  questions <- printed[draft$components$component_type[match(printed, draft$components$id)] == "Question"]
  expect_equal(question_number_of(draft, questions), as.character(seq_along(questions)))
  expect_true(all(is.na(draft$components$question_number[draft$components$component_type != "Question"])))

  # the first question moved to the end becomes the last number, and the rest move up
  first <- questions[1]
  parents <- draft$components$parent_id[match(printed, draft$components$id)]
  moved <- draft_service_apply_order(draft, c(setdiff(printed, first), first), c(parents[printed != first], NA))
  moved <- draft_service_renumber_questions(moved)
  expect_equal(question_number_of(moved, first), as.character(length(questions)))
  expect_equal(question_number_of(moved, questions[2]), "1")

  without <- draft_service_renumber_questions(suppressMessages(draft_service_delete(draft, "components", questions[2])))
  expect_equal(question_number_of(without, questions[3]), "2")
  expect_identical(draft_service_renumber_questions(draft), draft) # numbering twice changes nothing
})

# ------ MATR IDs from the questionnaire's earlier waves -------------------------------------------------
test_that("a question worded like a recorded one gets its variables' MATR IDs, blank ones only, each once", {
  draft <- gold_draft()
  draft$variables$matr_id <- NA_character_
  q <- draft$components[draft$components$component_type == "Question", ][1, ]
  boxes <- draft_service_children(draft, "variables", q$id)
  draft <- draft_service_add_box(draft, q$id) # a second variable, to pair by position
  boxes <- draft_service_children(draft, "variables", q$id)
  recorded <- data.frame(
    text = c(toupper(paste0("  ", q$text, " ")), q$text, "Some other question"),
    matr_id = c("AH01", "AH02", "AH99"), display_order = c(1, 2, 1), stringsAsFactors = FALSE
  )
  expect_message(filled <- draft_service_fill_recorded_matr_ids(draft, recorded), "MATR IDs of 2 variables from the questionnaire's earlier waves")
  expect_equal(filled$variables$matr_id[match(boxes$id, filled$variables$id)], c("AH01", "AH02"))
  expect_equal(sum(!is.na(filled$variables$matr_id)), 2)

  # already filled, or typed: left alone, and nothing is said
  typed <- draft
  typed$variables$matr_id[typed$variables$id == boxes$id[1]] <- "TYPED"
  expect_no_message(kept <- draft_service_fill_recorded_matr_ids(typed, recorded[2, ]))
  expect_equal(kept$variables$matr_id[kept$variables$id == boxes$id[1]], "TYPED")
  expect_true(is.na(kept$variables$matr_id[kept$variables$id == boxes$id[2]])) # AH02 is the second variable's, but only AH02's row was given... as the first
  # an ID another variable of the draft already has is not used twice
  taken <- draft
  taken$variables$matr_id[which(!taken$variables$component_id %in% q$id)[1]] <- "AH01" # another question's variable has it
  expect_message(once <- draft_service_fill_recorded_matr_ids(taken, recorded), "of 1 variable from")
  expect_equal(sum(once$variables$matr_id %in% "AH01"), 1)
  # nothing recorded, nothing said
  expect_no_message(same <- draft_service_fill_recorded_matr_ids(draft, recorded[0, ]))
  expect_identical(same, draft)
  # from another questionnaire picked in the MATR IDs dialog, the message names it
  expect_message(draft_service_fill_recorded_matr_ids(draft, recorded, from = "AHP"), "MATR IDs of 2 variables from AHP, where")
})

test_that("a recorded questionnaire's MATR IDs are listed with their question's words, ready to fill a draft", {
  existing <- list(
    components = data.frame(component_id = c(100, 102), component_type_id = c(1, 2), text = c("Part A", "Do you smoke?")),
    variables = data.frame(variable_key = c(900, 901), component_id = c(102, 102), matr_id = c("SM1", "SM2"), display_order = c(2, 1))
  )
  printed <- extraction_service_printed(existing)
  expect_equal(printed$matr_ids, data.frame(text = c("Do you smoke?", "Do you smoke?"), matr_id = c("SM1", "SM2"), display_order = c(2, 1)))
  expect_equal(nrow(printed$texts), 0) # no kinds of block given, so no words to offer
  types <- data.frame(component_type_id = 1:2, name = c("Section", "Question"))
  expect_equal(extraction_service_printed(existing, types)$texts$component_type, c("Section", "Question"))
  expect_equal(nrow(extraction_service_printed(NULL)$matr_ids), 0)
  expect_error(extraction_service_questionnaire_matr_ids(list(url = "", key = ""), ""), "Pick a questionnaire first")
})

# ------ Words offered as a block's words are typed ------------------------------------------------------
test_that("the words of the draft's own blocks and the recorded questionnaire's are offered by kind, once each", {
  draft <- gold_draft()
  own <- draft_service_text_suggestions(draft)
  expect_true(all(names(own) %in% draft$components$component_type))
  for (kind in names(own)) {
    expect_setequal(own[[kind]], unique(trimws(stats::na.omit(draft$components$text[draft$components$component_type == kind]))))
    expect_equal(own[[kind]], sort(own[[kind]]))
  }
  recorded <- data.frame(component_type = c("Section", "Instruction", "Section"), text = c("Part Z", "Tick one box.", "Part Z"))
  both <- draft_service_text_suggestions(draft, recorded)
  expect_true("Part Z" %in% both$Section)
  expect_equal(sum(both$Section == "Part Z"), 1)
  expect_true("Tick one box." %in% both$Instruction)
  expect_equal(draft_service_text_suggestions(draft_service_empty()), list())
  blank <- draft_service_empty()
  blank <- draft_service_add_block(blank, "Section", "c1")
  expect_equal(draft_service_text_suggestions(blank), list()) # a block with no words offers none
})

# ------ Question groups --------------------------------------------------------
test_that("question groups hold blocks like sections, nest, and aren't counted among the questions", {
  draft <- gold_draft()
  group <- draft$components$parent_id[draft$components$id == block_numbered(draft, "1a")$id]
  expect_equal(draft$components$component_type[draft$components$id == group], "Question Group")

  # a group inside a group
  inner <- draft_service_next_id(draft, "components")
  nested <- draft_service_add_block(draft, "Question Group", inner, near_id = group)
  expect_equal(nested$components$parent_id[nested$components$id == inner], group)
  q1a <- block_numbered(nested, "1a")$id
  printed <- draft_service_printed_order(nested)
  parents <- nested$components$parent_id[match(printed, nested$components$id)]
  parents[printed == q1a] <- inner
  moved <- draft_service_renumber_questions(draft_service_apply_order(nested, printed, parents))
  expect_equal(moved$components$parent_id[moved$components$id == q1a], inner)
  # the questions are numbered 1, 2, 3... in printed order; groups aren't counted
  in_order <- moved$components[match(draft_service_printed_order(moved), moved$components$id), ]
  in_order <- in_order[in_order$component_type == "Question", ]
  expect_equal(in_order$question_number, as.character(seq_len(nrow(in_order))))
  expect_false(any(!is.na(moved$components$question_number[is_container(moved$components$component_type)])))

  # a group can't go inside itself
  into_itself <- parents
  into_itself[printed == group] <- inner
  expect_error(draft_service_apply_order(nested, printed, into_itself), "inside itself")
})

test_that("a section is never inside a question group", {
  draft <- gold_draft()
  q1a <- block_numbered(draft, "1a")$id
  group <- draft$components$parent_id[draft$components$id == q1a]
  section <- draft_service_next_id(draft, "components")
  # added next to a question inside a group, a new section goes after the group instead
  added <- draft_service_add_block(draft, "Section", section, near_id = q1a)
  expect_true(is.na(added$components$parent_id[added$components$id == section]))
  expect_equal(draft_service_printed_order(added)[match(group, draft_service_printed_order(added)) + length(draft_service_printed_order(added, group)) + 1], section)

  # dragged into a group, it is refused
  printed <- draft_service_printed_order(added)
  parents <- added$components$parent_id[match(printed, added$components$id)]
  parents[printed == section] <- group
  expect_error(draft_service_apply_order(added, printed, parents), "A section can't go inside a question group")

  # a block inside a group can't become a section, and a group holding a section can't stay one
  expect_error(draft_service_update(draft, "components", q1a, "component_type", "Section"), "Move the blocks out|can't be inside a question group")
  instruction <- draft_service_children(draft, "components", group)$id[1]
  expect_error(draft_service_update(draft, "components", instruction, "component_type", "Section"), "can't be inside a question group")
})

test_that("a question group has no words: one with words can't become a group, and a group can become a section", {
  draft <- gold_draft()
  group <- draft$components$parent_id[draft$components$id == block_numbered(draft, "1a")$id]
  as_section <- draft_service_update(draft, "components", group, "component_type", "Section")
  expect_equal(as_section$components$component_type[as_section$components$id == group], "Section")
  headed <- draft_service_update(as_section, "components", group, "text", "Part A")
  expect_error(draft_service_update(headed, "components", group, "component_type", "Question Group"), "has no words of its own")
  expect_equal(
    draft_service_update(as_section, "components", group, "component_type", "Question Group")$components$component_type[as_section$components$id == group],
    "Question Group"
  )
  # a group holding blocks can't become a kind that holds none
  expect_error(draft_service_update(draft, "components", group, "component_type", "Question"), "Move the blocks out")
})

test_that("the question a question follows is picked from its list or typed as a number: the nearest one before it", {
  draft <- gold_draft()
  q8a <- block_numbered(draft, "8a")$id
  q7 <- block_numbered(draft, "7")$id
  picked <- draft_service_update(draft, "components", q8a, "parent_question_id", "7 · Have you smoked cigarettes at any time in your life?")
  expect_equal(picked$components$parent_question_id[picked$components$id == q8a], q7)
  typed <- draft_service_update(draft, "components", q8a, "parent_question_id", " 1a ")
  expect_equal(typed$components$parent_question_id[typed$components$id == q8a], block_numbered(draft, "1a")$id)
  expect_true(is.na(draft_service_update(draft, "components", q8a, "parent_question_id", "")$components$parent_question_id[draft$components$id == q8a]))
  expect_error(draft_service_update(draft, "components", q8a, "parent_question_id", "99z"), "No question is numbered 99z")

  # with a printed number used twice, the nearest one before it
  q9 <- block_numbered(draft, "9")$id
  twice <- draft_service_update(draft, "components", q9, "printed_number", "7")
  q10 <- block_numbered(twice, "10")$id
  followed <- draft_service_update(twice, "components", q10, "parent_question_id", "7")
  expect_equal(followed$components$parent_question_id[followed$components$id == q10], q9)
})

test_that("who a variable is about is blank for the respondent, however it is typed", {
  draft <- gold_draft()
  box <- draft$variables$id[1]
  expect_true(is.na(draft_service_update(draft, "variables", box, "subject", "self")$variables$subject[1]))
  expect_true(is.na(draft_service_update(draft, "variables", box, "subject", " ")$variables$subject[1]))
  expect_equal(draft_service_update(draft, "variables", box, "subject", " Respondent's Twin ")$variables$subject[1], "Respondent's Twin")
})
