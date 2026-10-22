# The Edit Extraction page's outline: blocks as a list, with the picked block's details beside it. What a click, a
# keystroke or a drag does goes through `state$change()`, which edit_extraction_state() stands in for. The list is
# drawn again only when the layout changes; the selected row, the closed sections, the checked blocks and the
# problems are sent to the page script instead (outline_marks()). The details are drawn again for the block picked.

# Every test gives the outline the gold questionnaire's first two parts (short_gold_draft()): the outline draws
# every block it is given, again each time the layout changes, and all of the gold questionnaire's takes seconds.
short_page <- function(draft = short_gold_draft()) edit_extraction_state(draft)
outline_args <- function(page = short_page()) list(state = page$state, lookups = page$lookups)

sections_of <- function(draft) draft$components$id[is_container(draft$components$component_type)]
has_class <- function(html, class, id) grepl(sprintf('class="[^"]*%s[^"]*" [^>]*data-id="%s"', class, id), html)
tree_html <- function(output) paste(output$tree$html, collapse = "")
# The marks the page script is sent now. Called inside testServer(), where `collapsed` is the outline's.
marks_now <- function(page, collapsed, ns) {
  outline_marks(page$draft(), page$state$problems(), page$selected(), collapsed(), character(), ns)
}
# The details of the block picked, as drawn now. Called inside testServer().
detail_html <- function(output) paste(output$detail$html, collapse = "")

test_that("clicking a block selects it, and the selection is drawn the next time the tree is", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "select", id = "c5"))
    expect_equal(page$selected(), "c5")
    session$setInputs(action = list(action = "select", id = "c9"))
    expect_equal(page$selected(), "c9")
    expect_equal(marks_now(page, collapsed, ns)$selected, "c9")
    redraw(redraw() + 1L) # the tree is drawn again
    session$flushReact()
    expect_true(has_class(tree_html(output), "is-selected", "c9"))
    expect_false(has_class(tree_html(output), "is-selected", "c5"))
  })
})

test_that("sections open and close one at a time (a click on the row), or all at once, without the tree being drawn again", {
  page <- short_page()
  sections <- sections_of(short_gold_draft())
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    before <- tree_html(output)
    # a click on a section's or group's row opens or closes it; there is no arrow for it
    expect_match(before, 'class="outline-row[^"]*is-container[^"]*" tabindex="0" data-action="toggle" data-id="c1"')
    expect_false(grepl("btn-toggle|caret-down", before))
    expect_length(collapsed(), 0)
    session$setInputs(action = list(action = "toggle", id = "c1"))
    expect_equal(collapsed(), "c1")
    expect_equal(marks_now(page, collapsed, ns)$collapsed, list("c1"))
    expect_identical(tree_html(output), before) # the page script closes it
    redraw(redraw() + 1L)
    session$flushReact()
    expect_true(has_class(tree_html(output), "is-collapsed", "c1")) # and a tree drawn again keeps it closed
    session$setInputs(action = list(action = "toggle", id = "c1"))
    expect_length(collapsed(), 0)

    session$setInputs(action = list(action = "collapse_all"))
    expect_setequal(collapsed(), sections)
    session$setInputs(action = list(action = "expand_all"))
    expect_length(collapsed(), 0)
  })
})

test_that("selecting a block inside a collapsed section opens the sections around it, and only those", {
  page <- short_page()
  sections <- sections_of(short_gold_draft())
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "collapse_all"))
    session$setInputs(action = list(action = "select", id = "c3"))
    expect_setequal(collapsed(), setdiff(sections, "c1"))
    expect_false("c1" %in% unlist(marks_now(page, collapsed, ns)$collapsed))
  })
})

test_that("every block's words are in its row, a section's and a text block's typed there once edit is pressed, with the words of its kind offered", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    html <- tree_html(output)
    expect_match(html, 'class="form-control form-control-sm outline-text"')
    expect_match(html, 'data-suggest="[^"]*suggest-Instruction"')
    expect_match(html, 'class="outline-group-note"') # a question group has no words to type
    expect_match(html, 'data-table="components" data-id="c3" data-field="text"')
    expect_match(html, "Would you call yourself a nervous person?", fixed = TRUE)
    expect_match(html, 'id="[^"]*answer-options"')
    expect_match(html, 'id="[^"]*conditions"')
    expect_true(has_class(html, "is-editable", "c2")) # an instruction's words are typed in the list
    expect_false(has_class(html, "is-editable", "c3")) # a question's in its details
    # once its edit button is pressed (review.js): until then they can't be typed in
    expect_match(html, 'data-field="text"[^>]*readonly|readonly[^>]*data-field="text"')
    expect_match(html, "data-edit-words")
    # a row is the block's kind, words, counts and what there is to fix: no numbers the site works out, and its
    # other fields (the printed number, the parent question) are in the details; a question is verified there
    expect_false(grepl("Q1a", text_of(output$tree), fixed = TRUE))
    expect_false(grepl("outline-number\\b|outline-order", html))
    expect_false(grepl('data-field="printed_number"|data-field="parent_question_id"|data-action="check"', html))
    blocks <- short_gold_draft()$components
    expect_equal(lengths(regmatches(html, gregexpr("btn-open", html))), sum(!is_container(blocks$component_type)))
    # a section's, group's or text block's row has its buttons (shown when pointed at): change its kind, delete
    others <- blocks$id[blocks$component_type != "Question"]
    expect_equal(lengths(regmatches(html, gregexpr('data-action="delete" data-table="components"', html))), length(others))
    expect_match(html, 'data-action="edit_kind" data-id="c2" data-type="Note"')
    expect_false(grepl('data-action="edit_kind"[^>]*data-type="Question"', html))
    # a question says how many variables it has, the number alone (pointing at it lists their MATR IDs, from the
    # marks); a section or group doesn't count what it holds
    expect_match(html, '<span class="row-badge outline-count" tabindex="0" aria-label="1 variable">1</span>')
    expect_equal(lengths(regmatches(html, gregexpr("outline-count", html))), sum(blocks$component_type == "Question"))
  })
})

test_that("typing in a field changes the draft and says it was typed; a pick or a tick isn't typed", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    errors <- collect_errors(session)
    before <- tree_html(output)
    session$setInputs(edit = list(table = "components", id = "c3", field = "text", value = "Do you worry a lot?", typed = TRUE))
    expect_equal(component(page$draft(), "c3")$text, "Do you worry a lot?")
    expect_equal(tree_html(output), before) # typing never redraws the tree
    session$setInputs(edit = list(table = "components", id = "c3", field = "condition", value = "If yes"))
    expect_equal(component(page$draft(), "c3")$condition, "If yes")
    expect_equal(page$log$typed, c(TRUE, FALSE))
    expect_length(errors(), 0)
  })
})

test_that("a change the draft refuses is a sentence, and nothing changes", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(edit = list(table = "components", id = "gone", field = "text", value = "x"))
    expect_equal(page$log$errors, "That's no longer in the draft.")
    expect_identical(page$draft(), short_gold_draft())
  })
})

test_that("a question's details have its variables: one line each, with its choices in short until they are opened", {
  page <- edit_extraction_state(draft_service_update(short_gold_draft(), "variables", "v1", "subject", "")) # who it's about not picked yet
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "details", id = "c3")) # its arrow picks it
    expect_equal(page$selected(), "c3")
    html <- detail_html(output)
    expect_match(html, "question-panel")
    expect_match(text_of(html), "Variables")
    # what a field holds is said in it, and its label is kept for screen readers
    expect_match(html, 'placeholder="MATR ID"[^>]*aria-label="MATR ID"')
    # every choice is printed, so there is no check box to say one isn't
    expect_false(grepl('type="checkbox"|data-field="printed"', html))
    # one line per variable: MATR ID, and its choices in short, closed until clicked; who it's about (blank is
    # the respondent) and its label show only when they have something in them, or are added
    expect_match(html, "variable-head") # the column names
    expect_match(html, 'data-table="variables" data-id="v1" data-field="matr_id"')
    expect_false(grepl('data-id="v1" data-field="(subject|label)"', html))
    expect_match(html, 'data-action="show_field" data-id="v1" data-name="subject"')
    expect_match(html, 'data-action="show_field" data-id="v1" data-name="label"')
    expect_false(grepl('data-action="show_field" data-id="v1" data-name="skip"', html)) # a variable with choices skips by its choices
    expect_match(html, 'data-action="choices" data-id="v1"')
    expect_match(text_of(html), "1 Yes · 2 No", fixed = TRUE)
    expect_false(grepl('data-field="answer_option"', html))
  })
})

test_that("marking a block as checked moves on to the next one in printed order, and unchecking stays put", {
  page <- short_page(with_matr_ids(short_gold_draft())) # nothing to fix, so its blocks can be checked
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    before <- tree_html(output)
    page$selected("c3")
    session$setInputs(action = list(action = "check", id = "c3"))
    expect_true(component(page$draft(), "c3")$checked)
    expect_equal(page$selected(), "c4")
    expect_identical(tree_html(output), before) # the page script marks it
    expect_equal(marks_now(page, collapsed, ns)$checked, list("c3"))
    redraw(redraw() + 1L)
    session$flushReact()
    expect_true(has_class(tree_html(output), "is-verified", "c3")) # green in the list

    session$setInputs(action = list(action = "check", id = "c3"))
    expect_false(component(page$draft(), "c3")$checked)
    expect_equal(page$selected(), "c4")
  })
})

test_that("a block with something to fix can't be checked, and a section or group has no check of its own", {
  page <- short_page() # its questions have no MATR IDs yet
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c3")
    session$setInputs(action = list(action = "check", id = "c3"))
    expect_false(component(page$draft(), "c3")$checked)
    expect_equal(page$selected(), "c3")
    expect_match(seen$notifications[[1]]$text, "Fix what's marked first")
    session$setInputs(action = list(action = "check", id = "c1")) # the question group
    expect_false(component(page$draft(), "c1")$checked)
  })
})

test_that("checking the last block in printed order has nowhere to move on to", {
  page <- short_page(with_matr_ids(short_gold_draft()))
  last <- utils::tail(draft_service_printed_order(short_gold_draft()), 1)
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected(last)
    session$setInputs(action = list(action = "check", id = last))
    expect_true(component(page$draft(), last)$checked)
    expect_equal(page$selected(), last)
  })
})

test_that("a new block goes after the selected one and is selected, so its words can be typed", {
  page <- short_page()
  before <- nrow(short_gold_draft()$components)
  new_id <- draft_service_next_id(short_gold_draft(), "components")
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    tree <- tree_html(output)
    session$setInputs(action = list(action = "select", id = "c5"))
    session$setInputs(action = list(action = "add_block", type = "Instruction"))
    expect_equal(nrow(page$draft()$components), before + 1)
    expect_equal(page$selected(), new_id)
    printed <- draft_service_printed_order(page$draft())
    expect_equal(printed[match("c5", printed) + 1], new_id)
    expect_equal(page$draft()$components$component_type[page$draft()$components$id == new_id], "Instruction")
    # only the new block is drawn, selected, with its empty words ready for the cursor
    expect_identical(tree_html(output), tree)
    expect_equal(patched()$insert[[1]]$id, new_id)
    expect_match(patched()$insert[[1]]$html, sprintf('data-autofocus[^>]*data-id="%s"', new_id))
    expect_equal(patched()$selected, new_id)
    parent <- page$draft()$components$parent_id[page$draft()$components$id == new_id]
    section <- Filter(function(list) identical(list$parent, parent), patched()$lists)
    expect_equal(unlist(section[[1]]$ids), draft_service_children(page$draft(), "components", section[[1]]$parent)$id)
  })
})

test_that("with a section selected a new block goes inside it, unless the new block is a section too", {
  page <- short_page()
  new_id <- draft_service_next_id(short_gold_draft(), "components")
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c1") # picked as it is (a click on it opens its first question)
    session$setInputs(action = list(action = "add_block", type = "Instruction"))
    expect_equal(page$draft()$components$parent_id[page$draft()$components$id == new_id], "c1")

    page$selected("c1")
    section_id <- draft_service_next_id(page$draft(), "components")
    session$setInputs(action = list(action = "add_block", type = "Section"))
    added <- page$draft()$components[page$draft()$components$id == section_id, ]
    expect_equal(added$component_type, "Section")
    expect_true(is.na(added$parent_id)) # beside c1, not inside it
  })
})

test_that("a question gets another variable, and a variable another choice, drawn in its details", {
  page <- short_page()
  boxes <- function() nrow(draft_service_children(page$draft(), "variables", "c3"))
  choices <- function() nrow(draft_service_children(page$draft(), "response_options", "v1"))
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c3")
    expect_equal(boxes(), 1)
    session$setInputs(action = list(action = "add_box", id = "c3"))
    expect_equal(boxes(), 2)
    html <- detail_html(output)
    expect_equal(lengths(regmatches(html, gregexpr('class="variable-card( [^"]*)?"', html))), 2)

    expect_equal(choices(), 2)
    session$setInputs(action = list(action = "add_choice", id = "v1"))
    expect_equal(choices(), 3)
  })
})

test_that("Apply to All? lists the questions with the same choices coded otherwise, and Apply gives them these codes", {
  page <- short_page()
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(edit = list(table = "response_options", id = "o1", field = "code_value", value = "1"))
    session$setInputs(edit = list(table = "response_options", id = "o2", field = "code_value", value = "0"))
    session$setInputs(action = list(action = "pick_codes", id = "v1"))
    dialog <- seen$modals[[length(seen$modals)]]
    expect_match(dialog, "Apply these codes to all?")
    question <- page$draft()$variables$component_id[page$draft()$variables$id == "v2"]
    expect_match(dialog, outline_questions(page$draft())$questions$label[outline_questions(page$draft())$questions$id == question], fixed = TRUE)
    expect_match(dialog, "Cancel Apply", fixed = TRUE)
    suppressMessages(session$setInputs(apply_codes = 1))
    options <- page$draft()$response_options
    expect_equal(options$code_value[options$variable_id == "v2"], c(1L, 0L))
    expect_equal(seen$closed, 1L)

    # nothing left coded otherwise: it says so, without a dialog
    shown <- length(seen$modals)
    session$setInputs(action = list(action = "pick_codes", id = "v1"))
    expect_length(seen$modals, shown)
    expect_match(seen$notifications[[length(seen$notifications)]]$text, "No other question has these choices")

    # a choice without a code can't be given out
    session$setInputs(edit = list(table = "response_options", id = "o1", field = "code_value", value = ""))
    session$setInputs(action = list(action = "pick_codes", id = "v1"))
    session$setInputs(apply_codes = 2)
    expect_match(page$log$errors, "code first")
  })
})

test_that("deleting a block takes it out and clears its selection; deleting a variable keeps it", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c3")
    suppressMessages(session$setInputs(action = list(action = "delete", table = "variables", id = "v1")))
    expect_false("v1" %in% page$draft()$variables$id)
    expect_equal(page$selected(), "c3")

    suppressMessages(session$setInputs(action = list(action = "delete", table = "components", id = "c3")))
    expect_false("c3" %in% page$draft()$components$id)
    expect_null(page$selected())

    page$selected("c5")
    suppressMessages(session$setInputs(action = list(action = "delete", table = "components", id = "c4")))
    expect_equal(page$selected(), "c5") # another block's deletion leaves the selection
  })
})

test_that("dragging blocks into a new order is applied, with the top level as an empty section id", {
  page <- short_page()
  draft <- short_gold_draft()
  printed <- draft_service_printed_order(draft)
  parent_of <- function(ids) draft$components$parent_id[match(ids, draft$components$id)]
  swapped <- printed
  swapped[match(c("c3", "c4"), printed)] <- c("c4", "c3")
  as_sent <- function(ids, parents) list(ids = as.list(ids), parents = as.list(ifelse(is.na(parents), "", parents)))
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(order = as_sent(swapped, parent_of(swapped)))
    after <- draft_service_printed_order(page$draft())
    expect_lt(match("c4", after), match("c3", after))

    top <- as_sent(after, parent_of(after))
    top$parents[[match("c3", after)]] <- ""
    session$setInputs(order = top)
    expect_true(is.na(page$draft()$components$parent_id[page$draft()$components$id == "c3"]))

    session$setInputs(order = list(ids = as.list(printed[-1]), parents = as.list(rep("", length(printed) - 1))))
    expect_equal(utils::tail(page$log$errors, 1), "The outline was out of date. Try that again.")
  })
})

test_that("variables and choices dragged into a new order are reordered, and a stale list is refused", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "add_box", id = "c3"))
    boxes <- draft_service_children(page$draft(), "variables", "c3")$id
    session$setInputs(reorder = list(table = "variables", ids = as.list(rev(boxes))))
    expect_equal(draft_service_children(page$draft(), "variables", "c3")$id, rev(boxes))

    session$setInputs(reorder = list(table = "response_options", ids = list("o2", "o1")))
    expect_equal(draft_service_children(page$draft(), "response_options", "v1")$id, c("o2", "o1"))

    session$setInputs(reorder = list(table = "variables", ids = list("v1", "not-a-box")))
    expect_equal(utils::tail(page$log$errors, 1), "That list was out of date. Try that again.")
  })
})

test_that("+ Block offers every kind the database has, each in a box saying what it is for, and the toolbar counts what is to fix", {
  draft <- draft_service_update(with_matr_ids(short_gold_draft()), "components", "c4", "text", "")
  page <- edit_extraction_state(draft)
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "pick_block"))
    expect_match(seen$modals[[1]], paste("Instruction", block_kind_descriptions[["Instruction"]]), fixed = TRUE)
    menu <- htmltools::renderTags(add_block_dialog(ns, page$lookups$component_types))$html
    for (kind in page$lookups$component_types) expect_match(menu, sprintf('data-action="add_block" data-type="%s"', kind), fixed = TRUE)
    expect_match(menu, 'class="pick-boxes is-grid"')
    # Section first, then Question Group and Question, whatever order the database has them in
    expect_lt(regexpr('data-type="Section"', menu), regexpr('data-type="Question Group"', menu))
    expect_lt(regexpr('data-type="Question Group"', menu), regexpr('data-type="Question"', menu))
    expect_equal(text_of(output$to_fix), "1 block to fix")
    html <- tree_html(output)
    expect_true(grepl('outline-problems"[^>]*data-problems="Add the printed words', html)) # c4's badge, shown, listing them when pointed at
    expect_match(html, 'class="row-badge is-warning outline-problems" tabindex="0"')
    expect_true(grepl('outline-problems is-hidden"', html)) # the others', hidden until needed
    session$setInputs(edit = list(table = "components", id = "c4", field = "text", value = "Words again", typed = TRUE))
    expect_equal(text_of(output$to_fix), "Nothing to fix")
  })
})

test_that("a field the checks find at fault is outlined, says why, and the marks follow the checks", {
  draft <- draft_service_update(with_matr_ids(short_gold_draft()), "variables", "v1", "matr_id", "")
  page <- edit_extraction_state(draft)
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c3")
    html <- detail_html(output)
    expect_match(html, 'class="form-control form-control-sm has-problem"[^>]*aria-invalid="true" title="Variable 1: fill in its MATR ID."[^>]*data-id="v1" data-field="matr_id"')
    # the details don't list again what one field is outlined for
    expect_false(grepl("details-problems", html))
  })
})

test_that("each MATR ID field offers the uploaded IDs no variable has yet", {
  page <- edit_extraction_state(with_matr_ids(short_gold_draft()))
  page$state$matr_ids <- function() c("x1", "x2")
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    html <- tree_html(output)
    ids_list <- regmatches(html, regexpr('<datalist id="[^"]*matr-ids">.*?</datalist>', html))
    expect_length(ids_list, 1)
    expect_match(ids_list, '<option value="x1">')
    page$selected("c3")
    session$flushReact()
    expect_match(detail_html(output), 'data-field="matr_id"')
    expect_match(detail_html(output), 'list="[^"]*matr-ids"')
  })
})

test_that("the words offered for a kind of block are those the Edit Extraction page shares", {
  page <- short_page()
  page$state$suggestions <- function() list(Section = c("Part A", "Part B"), Instruction = "Tick one.")
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    html <- tree_html(output)
    section_list <- regmatches(html, regexpr('<datalist id="[^"]*suggest-Section">.*?</datalist>', html))
    expect_length(section_list, 1)
    expect_match(section_list, '<option value="Part A">')
    expect_match(section_list, '<option value="Part B">')
    expect_match(html, 'id="[^"]*suggest-Instruction"')
    expect_match(html, '<option value="Tick one.">', fixed = TRUE)
  })
})

test_that("editing a checked block takes its check off, and marking it as checked doesn't", {
  page <- edit_extraction_state(with_checked(with_matr_ids(short_gold_draft()), c("c3", "c4")))
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(edit = list(table = "components", id = "c3", field = "text", value = "Do you worry a lot?"))
    expect_equal(checked_blocks(page$draft()), "c4")
    session$setInputs(action = list(action = "check", id = "c3"))
    expect_equal(checked_blocks(page$draft()), c("c3", "c4"))
    session$setInputs(edit = list(table = "variables", id = "v2", field = "label", value = "Worrier"))
    expect_equal(checked_blocks(page$draft()), "c3")
  })
})

test_that("the marks say which blocks are checked, and a section only counts as all checked once everything in it is", {
  draft <- with_checked(short_gold_draft(), c("c1", "c3"))
  inside <- draft_service_printed_order(draft, "c1")
  marks <- outline_marks(draft, draft_service_check(draft, test_lookups()), "c3", "c1", character(), shiny::NS("x"))
  expect_setequal(unlist(marks$checked), c("c1", "c3"))
  expect_equal(unlist(marks$all_checked), "c3") # c1 has blocks inside that aren't checked
  expect_equal(marks$collapsed, list("c1"))
  expect_equal(marks$selected, "c3")
  expect_equal(marks$check_titles$checked, outline_check_titles[["checked"]])

  everything <- with_checked(short_gold_draft(), c("c1", inside))
  all_marks <- outline_marks(
    everything, draft_service_check(everything, test_lookups()), NULL, character(), character(), shiny::NS("x")
  )
  expect_true("c1" %in% unlist(all_marks$all_checked))
})

test_that("deleting, moving and picking put only what changed on the tree already drawn", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    tree <- tree_html(output)
    blocks <- function() page$draft()$components

    # a section is taken out with everything in it: only the section is named
    inside <- draft_service_printed_order(page$draft(), "c1")
    suppressMessages(session$setInputs(action = list(action = "delete", table = "components", id = "c1")))
    expect_equal(patched()$remove, list("c1"))
    expect_false(any(inside %in% blocks()$id))

    # a block dragged out of its section: the lists say where every block is, and the counts are drawn again
    order <- draft_service_printed_order(page$draft())
    parents <- blocks()$parent_id[match(order, blocks()$id)]
    section <- parents[!is.na(parents)][1]
    moved <- order[which(parents == section)[1]]
    parents[order == moved] <- ""
    parents[is.na(parents)] <- ""
    session$setInputs(order = list(ids = as.list(order), parents = as.list(parents)))
    expect_true(is.na(blocks()$parent_id[blocks()$id == moved]))
    top <- Filter(function(list) list$parent == "", patched()$lists)[[1]]
    expect_true(moved %in% unlist(top$ids))
    expect_true(section %in% vapply(patched()$rows, function(row) row$id, character(1))) # "N items" changed
    expect_length(patched()$insert, 0)

    # a pick from a list draws its row again; the numbers, orders and parent lists come with every patch
    questions <- blocks()$id[blocks()$component_type == "Question"]
    session$setInputs(edit = list(table = "components", id = questions[2], field = "parent_question_id", value = questions[1]))
    expect_equal(vapply(patched()$rows, function(row) row$id, character(1)), questions[2])
    listed <- outline_questions(page$draft())$questions
    expect_setequal(names(patched()$numbers), questions)
    expect_equal(unlist(patched()$questions), listed$label) # the list every parent-question field offers, in printed order

    expect_identical(tree_html(output), tree) # never drawn again
  })
})

test_that("a field the draft refuses is drawn again as the draft has it", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    tree_html(output)
    # a question group with blocks in it can't become another kind
    session$setInputs(edit = list(table = "components", id = "c1", field = "component_type", value = "Instruction"))
    expect_match(page$log$errors, "Move the blocks out of it first")
    expect_equal(vapply(patched()$rows, function(row) row$id, character(1)), "c1")
    expect_match(patched()$rows[[1]]$html, "type-chip is-section is-group")
  })
})

test_that("when much has changed, the outline is drawn again instead", {
  draft <- short_gold_draft()
  view <- list(selected = NULL, collapsed = character(), opened = character(), problems = draft_service_check(draft, test_lookups()))
  expect_null(outline_patch(NULL, draft, view, test_lookups(), shiny::NS("x")))
  reworded <- draft
  reworded$components$text <- paste(reworded$components$text, "again")
  expect_null(outline_patch(draft, reworded, view, test_lookups(), shiny::NS("x"), most = 5))
  same <- outline_patch(draft, draft, view, test_lookups(), shiny::NS("x"))
  expect_length(c(same$insert, same$remove, same$rows, same$panels), 0)
})

test_that("the outline grows with the questionnaire, not with its square: every parent field shares one list", {
  draft <- gold_draft()
  view <- list(
    selected = NULL, collapsed = character(), opened = character(), problems = draft_service_check(draft, test_lookups()),
    suggestions = list(), matr_ids = character(), shown = character()
  )
  html <- htmltools::renderTags(outline_tree(draft, view, test_lookups(), shiny::NS("x")))$html
  n_questions <- sum(draft$components$component_type == "Question")
  n_options <- lengths(regmatches(html, gregexpr("<option", html)))
  # one list of the questions, plus the short pick lists; a drop-down of earlier questions in every row was
  # n_questions^2 / 2 options (56,620 for 390 blocks)
  expect_lt(n_options, n_questions + 200)
  expect_lt(nchar(html), 600000)
})

test_that("a question group's row can ungroup it, keeping its blocks where it was", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    expect_match(tree_html(output), 'data-action="ungroup" data-id="c1"')
    expect_false(grepl('data-action="ungroup" data-id="c3"', tree_html(output))) # a question isn't a group
    inside <- draft_service_children(page$draft(), "components", "c1")$id
    page$selected("c1")
    suppressMessages(session$setInputs(action = list(action = "ungroup", id = "c1")))
    expect_false("c1" %in% page$draft()$components$id)
    expect_equal(head(draft_service_children(page$draft(), "components", NA)$id, length(inside)), inside)
    expect_equal(page$selected(), inside[1])
  })
})

test_that("a variable's choices open under its line, code first and both named, and close again", {
  page <- short_page()
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c3")
    session$setInputs(action = list(action = "choices", id = "v1"))
    expect_equal(choices_open(), "v1")
    html <- detail_html(output)
    expect_match(html, 'aria-expanded="true"')
    expect_match(html, 'placeholder="Response option"')
    expect_match(html, 'placeholder="Code"')
    expect_lt(regexpr('data-field="code_value"', html), regexpr('data-field="answer_option"', html))
    expect_match(text_of(html), "Code Response Option", fixed = TRUE)
    # + Choice is a wide gold button; Apply to All? waits until a code is changed (review.js shows it)
    expect_match(html, 'class="btn btn-primary btn-wide choice-add"')
    expect_match(html, 'data-action="pick_codes" data-id="v1" hidden')
    expect_false(grepl("Use these codes everywhere|copy_codes", html))
    # a choice's printed words and skip instruction show only on a choice that has them; "+" adds them
    expect_false(grepl('data-field="printed_label"|data-field="skip_instruction"', html))
    expect_match(html, 'data-action="show_field" data-id="o1" data-name="printed_label"')
    session$setInputs(action = list(action = "choices", id = "v1"))
    expect_length(choices_open(), 0)
    expect_false(grepl('data-field="answer_option"', detail_html(output)))
  })
})

test_that("a choice's printed words or skip are added to that choice only, with the cursor in them, and × takes them away", {
  page <- edit_extraction_state(draft_service_update(short_gold_draft(), "response_options", "o2", "skip_instruction", "Go to 1c"))
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c3")
    session$setInputs(action = list(action = "choices", id = "v1"))
    html <- detail_html(output)
    expect_equal(lengths(regmatches(html, gregexpr('data-field="skip_to_id"', html))), 1) # o2's, which has one
    session$setInputs(action = list(action = "show_field", id = "o1", name = "printed_label"))
    expect_equal(shown(), "o1:printed_label")
    html <- detail_html(output)
    expect_equal(lengths(regmatches(html, gregexpr('data-field="printed_label"', html))), 1)
    expect_match(html, 'data-id="o1" data-field="printed_label"[^>]*data-focus-now|data-focus-now[^>]*data-id="o1" data-field="printed_label"')
    expect_match(html, 'data-action="hide_field" data-id="o1" data-name="printed_label"')
    # the × of a field with words empties it, which Undo can take back
    session$setInputs(action = list(action = "hide_field", id = "o2", name = "skip_instruction"))
    expect_true(is.na(component_choice(page$draft(), "o2")$skip_instruction))
    session$setInputs(action = list(action = "hide_field", id = "o1", name = "printed_label"))
    expect_length(shown(), 0)
    expect_false(grepl('data-field="printed_label"|data-field="skip_instruction"|data-field="skip_to_id"', detail_html(output)))
    # a field that isn't optional is ignored
    session$setInputs(action = list(action = "show_field", id = "o1", name = "answer_option"))
    expect_length(shown(), 0)
  })
})

test_that("variables with the same choices show them once, edited for all of them, or each its own", {
  draft <- short_gold_draft()
  draft <- draft_service_add_box(draft, "c3") # a copy of v1, with the same choices
  added <- setdiff(draft$variables$id, short_gold_draft()$variables$id)
  page <- edit_extraction_state(draft)
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c3")
    html <- detail_html(output)
    expect_match(html, sprintf('data-shared="v1,%s"', added))
    expect_match(text_of(html), "Choices of all 2 variables")
    expect_equal(lengths(regmatches(html, gregexpr("choice-summary is-shared", html))), 2)
    # a change there is made to both variables' choices
    shared <- paste0("v1,", added)
    session$setInputs(edit = list(table = "response_options", id = "o1", field = "answer_option", value = "Yes, often", shared = shared))
    both <- page$draft()$response_options
    expect_equal(both$answer_option[both$display_order == 1 & both$variable_id %in% c("v1", added)], c("Yes, often", "Yes, often"))
    session$setInputs(action = list(action = "add_choice", id = "v1", shared = shared))
    expect_equal(nrow(draft_service_children(page$draft(), "response_options", added)), 3)
    suppressMessages(session$setInputs(action = list(action = "delete", table = "response_options", id = "o1", shared = shared)))
    expect_equal(nrow(draft_service_children(page$draft(), "response_options", "v1")), 2)
    expect_equal(nrow(draft_service_children(page$draft(), "response_options", added)), 2)
    # each its own
    session$setInputs(action = list(action = "split_choices", id = "c3"))
    expect_equal(split(), "c3")
    expect_false(grepl("data-shared=\"v1", detail_html(output)))
    expect_match(detail_html(output), 'data-action="join_choices"')
  })
})

test_that("a variable's About and label show when they have something in them, and can be added and taken away", {
  draft <- draft_service_update(short_gold_draft(), "variables", "v1", "subject", "Respondent's Family")
  page <- edit_extraction_state(draft)
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c3")
    html <- detail_html(output)
    expect_match(html, 'data-id="v1" data-field="subject"[^>]*value="Respondent&#39;s Family"|value="Respondent&#39;s Family"[^>]*data-id="v1" data-field="subject"')
    expect_match(html, 'data-action="hide_field" data-id="v1" data-name="subject"')
    expect_false(grepl('data-action="show_field" data-id="v1" data-name="subject"', html))
    # a label is added, empty, with the cursor in it; × on About makes it the respondent again
    session$setInputs(action = list(action = "show_field", id = "v1", name = "label"))
    expect_equal(shown(), "v1:label")
    expect_match(detail_html(output), 'data-id="v1" data-field="label"[^>]*data-focus-now|data-focus-now[^>]*data-id="v1" data-field="label"')
    session$setInputs(action = list(action = "hide_field", id = "v1", name = "subject"))
    expect_true(is.na(page$draft()$variables$subject[page$draft()$variables$id == "v1"]))
    expect_false(grepl('data-field="subject"', detail_html(output)))
  })
})

test_that("a write-in variable has the same + for its About and label, and one for where it skips to", {
  draft <- draft_service_update(short_gold_draft(), "variables", "v1", "variable_type", "open_ended")
  view <- list(problems = draft_service_check(draft, test_lookups()), shown = character())
  line_of <- function(draft, view) htmltools::renderTags(variable_line(draft$variables[draft$variables$id == "v1", ], draft, view, test_lookups(), shiny::NS("x")))$html
  html <- line_of(draft, view)
  expect_match(html, 'data-field="answer_data_type"')
  expect_match(html, 'data-action="show_field" data-id="v1" data-name="subject"')
  expect_match(html, 'data-action="show_field" data-id="v1" data-name="label"')
  expect_match(html, 'data-action="show_field" data-id="v1" data-name="skip"')
  expect_false(grepl("variable-skip", html))
  # added, it asks for the answer that skips and where it goes, the cursor in the answer
  html <- line_of(draft, utils::modifyList(view, list(shown = "v1:skip", focus = "v1:skip")))
  expect_match(text_of(html), "If the answer is skip to", fixed = TRUE)
  expect_match(html, 'data-table="variables" data-id="v1" data-field="skip_condition"[^>]*data-focus-now')
  expect_match(html, 'data-table="variables" data-id="v1" data-field="skip_to_id"')
  # a write-in with a skip shows it
  skips <- draft_service_update(draft_service_update(draft, "variables", "v1", "skip_condition", "None"), "variables", "v1", "skip_to_id", skip_to_end_id)
  html <- line_of(skips, utils::modifyList(view, list(targets = skip_targets(skips))))
  expect_match(html, 'value="None"')
  expect_match(html, 'value="End of Questionnaire"')
})

test_that("Skip to is a pick of where the choice sends respondents, its printed words kept but not shown, and × takes both away", {
  draft <- short_gold_draft()
  draft <- draft_service_update(draft, "response_options", "o2", "skip_instruction", "Go to Question 1c")
  page <- edit_extraction_state(draft)
  seen <- local_browser()
  target <- draft$components$id[draft_service_question_number(draft$components) %in% "1c"]
  expect_equal(draft$response_options$skip_to_id[draft$response_options$id == "o2"], target) # linked from its words
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    expect_match(tree_html(output), '<datalist id="[^"]*skip-targets">')
    page$selected("c3")
    session$setInputs(action = list(action = "choices", id = "v1"))
    html <- detail_html(output)
    expect_match(html, 'placeholder="Skips to"[^>]*data-pick[^>]*data-id="o2" data-field="skip_to_id"')
    expect_match(html, 'value="1c · [^"]*"[^>]*data-id="o2" data-field="skip_to_id"|data-id="o2" data-field="skip_to_id"[^>]*value="1c · ')
    expect_false(grepl('data-field="skip_instruction"|As printed', html))
    expect_match(html, 'title="Printed: Go to Question 1c"')
    session$setInputs(action = list(action = "hide_field", id = "o2", name = "skip_instruction"))
    choice <- component_choice(page$draft(), "o2")
    expect_true(is.na(choice$skip_instruction))
    expect_true(is.na(choice$skip_to_id))
  })
})

test_that("a question's row sums up its variables: one has its MATR ID there, several are counted", {
  draft <- short_gold_draft()
  n_boxes <- table(draft$variables$component_id)
  one <- names(n_boxes)[n_boxes == 1][1]
  several <- names(n_boxes)[n_boxes > 1][1]
  box <- draft$variables$id[draft$variables$component_id == one]
  draft <- draft_service_update(draft, "variables", box, "matr_id", "GNA_1")
  view <- list(problems = draft_service_check(draft, test_lookups()), selected = NULL, collapsed = character(), opened = character())
  row_of <- function(id) htmltools::renderTags(outline_row(draft$components[draft$components$id == id, ], draft, view, shiny::NS("x")))$html

  single <- row_of(one)
  expect_match(single, 'class="outline-row-summary"')
  expect_match(single, sprintf('data-field="matr_id" data-id="%s"[^>]*value="GNA_1"', box))
  expect_match(single, variable_summary_text(draft, box), fixed = TRUE)
  many <- row_of(several)
  expect_match(many, sprintf("<strong>%s</strong>", plural(n_boxes[[several]], "variable")))
  expect_false(grepl('data-field="matr_id"', many))
  # a write-in says its kind of answer, and who it is about when that isn't the respondent
  write_in <- draft_service_update(draft_service_update(draft, "variables", box, "variable_type", "open_ended"), "variables", box, "subject", "Respondent's Family")
  expect_match(variable_summary_text(write_in, box), "^Write-in: .* · About: Respondent's Family$")
})

test_that("giving out MATR IDs by hand opens every section, so each question's MATR ID can be typed under it", {
  page <- short_page()
  ticks <- shiny::reactiveVal(0L)
  page$state$show_variables <- ticks
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "collapse_all"))
    expect_setequal(collapsed(), sections_of(short_gold_draft()))
    ticks(1L) # Manual Assignment
    session$flushReact()
    expect_length(collapsed(), 0)
  })
  # the rows have each question's variables in short, the page script shows them then (is-assigning)
  draft <- short_gold_draft()
  view <- list(problems = draft_service_check(draft, test_lookups()), selected = NULL, collapsed = character())
  row <- htmltools::renderTags(outline_row(component(draft, "c3"), draft, view, shiny::NS("x")))$html
  expect_match(row, 'class="outline-row-summary"')
})

test_that("each block notes the PDF page it was read from, for Open PDF to open at", {
  draft <- short_gold_draft()
  draft$components$page <- seq_len(nrow(draft$components))
  view <- list(problems = draft_service_check(draft, test_lookups()), selected = NULL, collapsed = character(), opened = character())
  html <- htmltools::renderTags(outline_node(draft$components[2, ], draft, view, test_lookups(), shiny::NS("x")))$html
  expect_match(html, sprintf('data-id="%s"[^>]*data-page="2"', draft$components$id[2]))
})

test_that("a question group closes once everything in it is checked, and the first block after it that isn't a section or group is selected", {
  draft <- with_matr_ids(short_gold_draft())
  printed <- draft_service_printed_order(draft)
  group <- sections_of(draft)[1]
  inside <- draft_service_printed_order(draft, group)
  last <- utils::tail(inside, 1)
  after <- printed[seq_along(printed) > match(last, printed)]
  after <- after[!is_container(draft$components$component_type[match(after, draft$components$id)])]
  for (id in utils::head(inside, -1)) draft <- draft_service_update(draft, "components", id, "checked", TRUE)
  page <- short_page(draft)
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected(inside[2])
    session$setInputs(action = list(action = "check", id = inside[2])) # unchecked: nothing closes
    session$setInputs(action = list(action = "check", id = inside[2]))
    expect_false(group %in% collapsed())
    session$setInputs(action = list(action = "check", id = last))
    expect_true(group %in% collapsed())
    expect_equal(page$selected(), after[1])
    expect_false(page$selected() %in% inside)
  })
})

test_that("the marks say which blocks differ from what the AI read, for the Changed from AI filter", {
  ai_read <- short_gold_draft()
  page <- short_page()
  page$state$original <- function() ai_read
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    expect_length(outline_marks(page$draft(), page$state$problems(), NULL, character(), character(), ns)$changed, 0)
    page$state$change(draft_service_update(page$draft(), "components", "c4", "text", "Something else"))
    expect_identical(original(), ai_read) # the module's own, as the page gives it
    expect_equal(draft_service_changed_blocks(original(), page$draft()), "c4")
    changed <- outline_marks(
      page$draft(), page$state$problems(), NULL, character(), character(), ns,
      changed = draft_service_changed_blocks(original(), page$draft())
    )$changed
    expect_equal(changed, list("c4"))
  })
  expect_match(as.character(outline_filter()), 'data-outline-filter')
  expect_match(as.character(outline_filter()), '<option value="changed">Changed from AI</option>')
  expect_match(as.character(outline_filter()), '<option value="all">Add Filter</option>')
})

# ------ The list, with the picked block's details beside it -----------------------------------------------
detail_html <- function(output) paste(output$detail$html, collapse = "")

test_that("the picked block's details are drawn: its words, its skip logic, then its variables", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    expect_match(detail_html(output), "Pick a block in the list")
    page$selected("c3")
    session$flushReact()
    html <- detail_html(output)
    expect_match(html, 'class="block-detail" data-id="c3"')
    expect_match(html, 'data-field="text" data-id="c3"')
    expect_false(grepl('data-field="component_type"', html)) # its kind isn't changed here
    expect_match(html, 'class="variable-cards"')
    expect_lt(regexpr("Skip Conditions &amp; Logic", html), regexpr("<h3>Variables</h3>", html))
    expect_match(html, 'data-action="check" disabled data-id="c3"') # it has no MATR ID yet
    # a question's variables are in its details: its arrow picks it
    page$selected("c5")
    session$setInputs(action = list(action = "details", id = "c3"))
    expect_equal(page$selected(), "c3")
    session$setInputs(action = list(action = "add_box", id = "c3"))
    expect_match(detail_html(output), sprintf('data-id="%s"', utils::tail(page$draft()$variables$id, 1)))
  })
})

test_that("Add Skip Logic makes a question depend on an answer to an earlier one, said in a sentence, and its delete button takes it away", {
  page <- short_page()
  seen <- local_browser()
  of <- function(id) component(page$draft(), id)
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c4")
    session$setInputs(action = list(action = "add_skip_logic", id = "c4"))
    expect_match(seen$modals[[1]], "only shown to the respondent if they answered another question")
    expect_match(seen$modals[[1]], "meet a specific condition or characteristic")
    session$setInputs(logic_answer = 1)
    expect_match(seen$modals[[2]], "The earlier question")
    expect_match(text_of(output$logic_answers), "Pick the earlier question first")
    session$setInputs(logic_parent = "c3")
    expect_match(text_of(output$logic_answers), "Yes")
    session$setInputs(logic_answer_picks = "Yes", logic_answer_save = 1)
    expect_equal(of("c4")$parent_question_id, "c3")
    expect_equal(of("c4")$condition, "Yes")
    html <- detail_html(output)
    # the earlier question by its words, in full, and no number
    expect_match(text_of(html), "This question is only shown to respondents if they answered “Yes” to “Would you call yourself a nervous person?”", fixed = TRUE)
    expect_match(html, 'data-action="select" data-id="c3"') # the sentence goes to that question
    expect_match(regmatches(html, regexpr('<button[^>]*data-action="remove_logic".*?</button>', html)), "fa-trash") # deleted like everything else
    session$setInputs(action = list(action = "remove_logic", id = "c4", kind = "answer"))
    expect_true(is.na(of("c4")$parent_question_id))
    expect_true(is.na(of("c4")$condition))
    expect_match(text_of(detail_html(output)), "Every respondent sees this question.")
    # nothing picked says so, and changes nothing
    session$setInputs(logic_parent = "", logic_answer_save = 2)
    expect_match(seen$notifications[[length(seen$notifications)]]$text, "Pick the earlier question")
  })
})

test_that("Add Skip Logic can show a question only to some respondents, which takes away an earlier question picked", {
  page <- edit_extraction_state(draft_service_update(short_gold_draft(), "components", "c4", "parent_question_id", "c3"))
  seen <- local_browser()
  of <- function(id) component(page$draft(), id)
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    page$selected("c4")
    session$setInputs(action = list(action = "add_skip_logic", id = "c4"))
    session$setInputs(logic_condition = 1)
    expect_match(seen$modals[[2]], "This question is only shown to respondents who are:")
    session$setInputs(logic_condition_text = "Twins", logic_condition_save = 1)
    expect_equal(of("c4")$condition, "Twins")
    expect_true(is.na(of("c4")$parent_question_id))
    expect_match(text_of(detail_html(output)), "This question is only shown to respondents who are: Twins", fixed = TRUE)
    session$setInputs(action = list(action = "remove_logic", id = "c4", kind = "condition"))
    expect_true(is.na(of("c4")$condition))
  })
})

test_that("a Skip to on a choice or a write-in shows in the skip logic of the questions it skips past, with the answers that show them", {
  # the page works out the parents after every change (draft_service_skip_parents()); here it is done by hand
  draft <- draft_service_skip_parents(draft_service_update(short_gold_draft(), "response_options", "o2", "skip_instruction", "Go to Question 1c"))
  skipped <- component(draft, "c4") # 1b, between 1a and 1c
  expect_true(skipped$parent_from_skip)
  view <- list(problems = draft_service_check(draft, test_lookups()))
  detail_of <- function(draft, id) htmltools::renderTags(block_detail(component(draft, id), draft, view, test_lookups(), shiny::NS("x")))$html
  html <- detail_of(draft, "c4")
  expect_match(text_of(html), "only shown to respondents if they answered “Yes” to “Would you call yourself a nervous person?”", fixed = TRUE) # No skips past it
  expect_match(text_of(html), "from a Skip to in that question")
  expect_match(html, 'data-action="select" data-id="c3"') # the sentence goes to that question
  expect_false(grepl('data-action="remove_logic"', html)) # it goes when the Skip to does
  # the question with the Skip to doesn't list the questions it decides again: their own sentences say it
  expect_false(grepl('data-id="c4"', detail_of(draft, "c3")))

  # a write-in's: anything but the answer that skips
  write_in <- draft_service_update(short_gold_draft(), "variables", "v1", "variable_type", "open_ended")
  write_in <- draft_service_update(write_in, "variables", "v1", "skip_condition", "None")
  write_in <- draft_service_skip_parents(draft_service_update(write_in, "variables", "v1", "skip_to_id", "1c"))
  expect_true(component(write_in, "c4")$parent_from_skip)
  expect_match(text_of(detail_of(write_in, "c4")), "if they answered anything but “None” to “Would you call yourself a nervous person?”", fixed = TRUE)
  expect_false(grepl("“”", text_of(detail_of(draft, "c4")), fixed = TRUE)) # no write-in skip: no empty quotes
})

test_that("a section's or group's details have no variables, and Ungroup in their footer; nothing picked says so", {
  draft <- short_gold_draft()
  view <- list(problems = draft_service_check(draft, test_lookups()), selected = NULL, collapsed = character(), opened = character())
  container <- draft$components[draft$components$id == sections_of(draft)[1], , drop = FALSE]
  html <- htmltools::renderTags(block_detail(container, draft, view, test_lookups(), shiny::NS("x")))$html
  expect_match(html, 'data-action="ungroup"')
  expect_false(grepl("variable-cards|data-field=\"follower\"", html))
  expect_equal(grepl('data-field="text"', html), container$component_type == "Section") # a group has no words
  expect_match(as.character(block_detail(NULL, draft, view, test_lookups(), shiny::NS("x"))), "Pick a block in the list")
})

test_that("the toolbar is + Block, Add Filter, Search, Expand All, Collapse All and what is to fix, and the details sit beside the list", {
  html <- htmltools::renderTags(extraction_outline_ui("outline"))$html
  expect_match(html, 'class="outline-panel is-split"')
  # each button is its icon and its words (only the icon on a phone), the search and filter each with an icon
  expect_match(html, 'data-action="pick_block"')
  expect_match(html, '<span class="btn-text">Block</span>')
  expect_match(html, "fa-magnifying-glass")
  expect_match(html, "fa-filter")
  places <- vapply(c('data-action="pick_block"', "Add Filter", "data-outline-search", "Expand All", "Collapse All", "outline-to_fix"), function(x) {
    as.integer(regexpr(x, html, fixed = TRUE))
  }, integer(1))
  expect_true(all(places > 0))
  expect_equal(order(places), seq_along(places))
  expect_false(grepl("data-outline-layout|outline-layout", html)) # one layout: no Open or Close
  expect_match(html, 'id="outline-detail"')
})

test_that("a variable's line is its MATR ID and response, with About and Label columns once one has them, and no kind or number", {
  draft <- draft_service_add_box(short_gold_draft(), "c3")
  view <- list(problems = draft_service_check(draft, test_lookups()), shown = character(), split = "c3")
  cards <- function(draft) htmltools::renderTags(variable_cards(component(draft, "c3"), draft, view, test_lookups(), shiny::NS("x")))$html
  html <- cards(draft)
  expect_match(text_of(html), "MATR ID Response", fixed = TRUE)
  expect_false(grepl("About|Label|Kind", regmatches(html, regexpr('<div class="variable-line variable-head".*?</div>', html))))
  expect_false(grepl('data-field="variable_type"|variable-card-number', html))
  # + Variable is at the bottom of the details, not under the variables
  expect_false(grepl("Add Variable|add_box|wide-menu", html))
  # the line between two variables, not between the column names and the first (_edit_extraction.scss)
  expect_match(html, 'class="variable-line variable-head"')
  # one variable with an About gives the table an About column; the other's cell there is empty
  about <- draft_service_update(draft, "variables", "v1", "subject", "Respondent's Family")
  html <- cards(about)
  expect_match(html, 'class="variable-cards has-subject"')
  expect_match(regmatches(html, regexpr('<div class="variable-line variable-head".*?</div>', html)), "<span>About</span>")
  expect_equal(lengths(regmatches(html, gregexpr('class="variable-cell is-subject"', html))), 2)
})

test_that("+ Variable asks for choices or a write-in, each said in a box, and adds the kind picked", {
  page <- short_page()
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "pick_variable", id = "c3"))
    expect_match(seen$modals[[1]], "Add a variable Choices The respondent marks one of the printed choices")
    expect_match(seen$modals[[1]], "Write-in The respondent writes the answer")
    dialog <- htmltools::renderTags(add_variable_dialog(ns, "c3"))$html
    expect_match(dialog, sprintf('data-ns="%s"', ns(""))) # the boxes' clicks reach the outline's `action` (review.js)
    expect_match(dialog, 'data-action="add_box" data-id="c3" data-type="closed_ended"')
    expect_match(dialog, 'data-action="add_box" data-id="c3" data-type="open_ended"')
    session$setInputs(action = list(action = "add_box", id = "c3", type = "open_ended"))
    expect_equal(seen$closed, 1L)
    added <- utils::tail(page$draft()$variables, 1)
    expect_equal(added$variable_type, "open_ended")
    expect_equal(nrow(draft_service_children(page$draft(), "response_options", added$id)), 0)
  })
})

test_that("a link to a section or group (from the list of what is to fix) opens its first question; a draft opens with them all closed", {
  page <- short_page()
  loads <- shiny::reactiveVal(0L)
  page$state$loaded <- loads
  first <- draft_service_printed_order(short_gold_draft(), "c1")
  blocks <- short_gold_draft()$components
  first <- first[blocks$component_type[match(first, blocks$id)] == "Question"][1]
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$flushReact()
    expect_setequal(collapsed(), sections_of(short_gold_draft()))
    session$setInputs(action = list(action = "select", id = "c1"))
    expect_equal(page$selected(), first)
    expect_false("c1" %in% collapsed()) # opened around the question picked
  })
})

test_that("the editor's header is the block's words, typed there; its buttons are along the bottom: Verify, + Skip Logic, + Variable, Delete", {
  draft <- short_gold_draft()
  view <- list(problems = draft_service_check(draft, test_lookups()), selected = NULL, collapsed = character(), opened = character())
  html <- htmltools::renderTags(block_detail(component(draft, "c3"), draft, view, test_lookups(), shiny::NS("x")))$html
  head <- regmatches(html, regexpr('<div class="detail-head">.*?<div class="detail-content">', html))
  expect_match(head, 'class="[^"]*detail-words[^"]*" data-field="text" data-id="c3"')
  expect_false(grepl("data-action", gsub('data-action="select"', "", head))) # no buttons up there
  foot <- regmatches(html, regexpr('<div class="detail-actions">.*', html))
  expect_gt(regexpr('class="detail-actions"', html), regexpr('class="detail-content"', html)) # after the content
  places <- vapply(c('data-action="check"', 'data-action="add_skip_logic"', 'data-action="pick_variable"', 'data-action="delete"'), function(x) {
    as.integer(regexpr(x, foot, fixed = TRUE))
  }, integer(1))
  expect_true(all(places > 0))
  expect_equal(order(places), seq_along(places))
  expect_match(foot, 'data-action="check" disabled data-id="c3"') # something to fix: not yet
  # each its icon and short words ("+ Variable", not "Add Variable")
  expect_match(foot, '<span class="btn-text">Skip Logic</span>')
  expect_match(foot, '<span class="btn-text">Variable</span>')
  expect_match(foot, "fa-circle-plus")
  expect_match(foot, "fa-trash")
  expect_match(html, "Question Number")
  expect_match(html, "fa-hashtag")
  expect_false(grepl("Place among|Number as printed|component_type|Add Skip Logic|btn-wide", html))
  fixed <- with_matr_ids(draft)
  html <- htmltools::renderTags(block_detail(component(fixed, "c3"), fixed, list(problems = draft_service_check(fixed, test_lookups())), test_lookups(), shiny::NS("x")))$html
  expect_false(grepl('data-action="check" disabled', html))
})

test_that("a variable's choices have Printed as and Skip to columns once one of them has it, the others' cells left empty", {
  draft <- short_gold_draft()
  view <- list(problems = draft_service_check(draft, test_lookups()), shown = character())
  editor <- function(draft) htmltools::renderTags(choice_editor(draft$variables[draft$variables$id == "v1", ], draft, view, shiny::NS("x")))$html
  head_of <- function(html) regmatches(html, regexpr('<div class="choice-row choice-head".*?</div>', html))
  html <- editor(draft)
  expect_match(html, 'class="choice-table"')
  expect_match(text_of(head_of(html)), "Code Response Option", fixed = TRUE)
  expect_false(grepl("Printed as|Skip to", head_of(html)))
  printed <- draft_service_update(draft, "response_options", "o1", "printed_label", "YES")
  html <- editor(printed)
  expect_match(html, 'class="choice-table has-printed_label"')
  expect_match(head_of(html), "<span>Printed as</span>")
  expect_equal(lengths(regmatches(html, gregexpr('class="choice-cell is-printed_label"', html))), 2) # o2's is empty
  both <- draft_service_skip_parents(draft_service_update(printed, "response_options", "o2", "skip_instruction", "Go to Question 1c"))
  html <- editor(both)
  expect_match(html, 'class="choice-table has-printed_label has-skip_instruction"')
  expect_match(text_of(head_of(html)), "Code Response Option Printed as Skip to", fixed = TRUE)
})

test_that("a parent the AI read without saying which answers is said as depending on how they answered", {
  draft <- draft_service_update(short_gold_draft(), "components", "c4", "parent_question_id", "c3")
  html <- htmltools::renderTags(block_detail(component(draft, "c4"), draft, list(problems = draft_service_check(draft, test_lookups())), test_lookups(), shiny::NS("x")))$html
  expect_match(text_of(html), "This question is only shown to respondents depending on how they answered “Would you call yourself a nervous person?”", fixed = TRUE)
})

test_that("a parent the AI read, with a Skip to that passes the question, says the answers that reach it", {
  draft <- draft_service_update(short_gold_draft(), "components", "c4", "parent_question_id", "c3") # read by the AI
  draft <- draft_service_skip_parents(draft_service_update(draft, "response_options", "o2", "skip_instruction", "Go to Question 1c"))
  expect_false(component(draft, "c4")$parent_from_skip)
  html <- htmltools::renderTags(block_detail(component(draft, "c4"), draft, list(problems = draft_service_check(draft, test_lookups())), test_lookups(), shiny::NS("x")))$html
  expect_match(text_of(html), "if they answered “Yes” to “Would you call yourself a nervous person?”", fixed = TRUE)
})
