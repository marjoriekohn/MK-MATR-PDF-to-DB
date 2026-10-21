# The Edit Extraction page's outline: blocks as a tree, each edited where it is. What a click, a keystroke or a drag
# does goes through `state$change()`, which edit_extraction_state() stands in for. The tree is drawn again only when
# the layout changes; the selected row, the closed sections and open panels, the checked blocks and the problems
# are sent to the page script instead (outline_marks()), and a question's panel is added on its own.

# Every test gives the outline the gold questionnaire's first two parts (short_gold_draft()): the outline draws
# every block it is given, again each time the layout changes, and all of the gold questionnaire's takes seconds.
short_page <- function(draft = short_gold_draft()) edit_extraction_state(draft)
outline_args <- function(page = short_page()) list(state = page$state, lookups = page$lookups)

sections_of <- function(draft) draft$components$id[draft$components$component_type == "Section"]
has_class <- function(html, class, id) grepl(sprintf('class="[^"]*%s[^"]*" [^>]*data-id="%s"', class, id), html)
tree_html <- function(output) paste(output$tree$html, collapse = "")
# The marks the page script is sent now. Called inside testServer(), where `collapsed` and `opened` are the outline's.
marks_now <- function(page, collapsed, opened, ns) {
  outline_marks(page$draft(), page$state$problems(), page$selected(), collapsed(), opened(), character(), ns)
}
# The last question panel added to the tree.
panel_html <- function(seen) seen$inserted[[length(seen$inserted)]]$html

test_that("clicking a block selects it, and the selection is drawn the next time the tree is", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "select", id = "c5"))
    expect_equal(page$selected(), "c5")
    session$setInputs(action = list(action = "select", id = "c9"))
    expect_equal(page$selected(), "c9")
    expect_equal(marks_now(page, collapsed, opened, ns)$selected, "c9")
    redraw(redraw() + 1L) # the tree is drawn again
    session$flushReact()
    expect_true(has_class(tree_html(output), "is-selected", "c9"))
    expect_false(has_class(tree_html(output), "is-selected", "c5"))
  })
})

test_that("sections open and close one at a time, or all at once, without the tree being drawn again", {
  page <- short_page()
  sections <- sections_of(short_gold_draft())
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    before <- tree_html(output)
    expect_length(collapsed(), 0)
    session$setInputs(action = list(action = "toggle", id = "c1"))
    expect_equal(collapsed(), "c1")
    expect_equal(marks_now(page, collapsed, opened, ns)$collapsed, list("c1"))
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
    expect_false("c1" %in% unlist(marks_now(page, collapsed, opened, ns)$collapsed))
  })
})

test_that("every block's words are typed in its row, with the words of its kind offered", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    html <- tree_html(output)
    expect_match(html, 'class="form-control form-control-sm outline-text"')
    expect_match(html, 'data-suggest="[^"]*suggest-Section"')
    expect_match(html, 'data-table="components" data-id="c3" data-field="text"')
    expect_match(html, "Would you call yourself a nervous person?", fixed = TRUE)
    expect_match(html, 'id="[^"]*answer-options"')
    expect_match(html, 'id="[^"]*conditions"')
    # a question's row has its place among the questions, without a "Q", and the number printed beside
    # it, typed in its own field; every row has its check and delete buttons
    expect_false(grepl("Q1a", text_of(output$tree), fixed = TRUE))
    expect_match(html, '<span class="outline-number"[^>]*>1a</span>') # the draft's own number, until it is renumbered
    number_field <- regmatches(html, regexpr('<input[^>]*data-field="printed_number" data-id="c3"[^>]*>', html))
    expect_match(number_field, 'placeholder="No."', fixed = TRUE)
    expect_match(number_field, 'value="1a"', fixed = TRUE) # read off the PDF
    expect_equal(lengths(regmatches(html, gregexpr('data-field="printed_number"', html))), sum(short_gold_draft()$components$component_type == "Question"))
    expect_equal(lengths(regmatches(html, gregexpr('data-action="check"', html))), nrow(short_gold_draft()$components))
    expect_equal(lengths(regmatches(html, gregexpr('data-action="delete" data-table="components"', html))), nrow(short_gold_draft()$components))
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

test_that("a question's arrow opens its panel of variables; the question it follows is beside its words", {
  page <- edit_extraction_state(draft_service_update(short_gold_draft(), "variables", "v1", "subject", "")) # who it's about not picked yet
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    html <- tree_html(output)
    expect_false(grepl("question-panel", html))
    # the question it follows is picked in the question's row, with no tooltip
    expect_match(html, '<select class="form-select form-select-sm" aria-label="Parent question" data-table="components" data-id="c3" data-field="parent_question_id">')
    expect_match(html, "No Parent Question")
    expect_false(grepl("field-hint", html))
    session$setInputs(action = list(action = "details", id = "c3"))
    expect_equal(opened(), "c3")
    expect_equal(marks_now(page, collapsed, opened, ns)$opened, list("c3"))
    expect_identical(tree_html(output), html) # the panel is added to the tree on its own
    expect_equal(seen$inserted[[1]]$selector, '.outline-tree .outline-node[data-id="c3"]')
    expect_equal(seen$inserted[[1]]$where, "beforeEnd")
    html <- panel_html(seen)
    expect_match(html, "question-panel")
    panel <- text_of(html)
    expect_match(panel, "Variables")
    expect_false(grepl("Variables ·", panel))
    # what a field holds is said in it, and its label is kept for screen readers
    expect_match(html, 'placeholder="MATR ID"[^>]*aria-label="MATR ID"')
    expect_match(html, 'placeholder="Label \\(optional\\)"')
    # every choice is printed, so there is no check box to say one isn't
    expect_false(grepl('type="checkbox"|data-field="printed"', html))
    expect_match(html, 'data-field="printed_label"')
    expect_match(html, "Select who this question is about")
    expect_match(html, 'placeholder="Response Option"')
    expect_match(html, 'placeholder="Code"')
    expect_false(grepl("Pick or type|Leave blank if no difference|choice-heading", html))
    # the code comes before the response option it codes
    expect_lt(regexpr('data-field="code_value"', html), regexpr('data-field="answer_option"', html))
    expect_match(html, 'data-table="variables" data-id="v1" data-field="matr_id"')
    expect_match(html, "variable-card-row") # kind, MATR ID, subject and label on one row, with delete
    session$setInputs(action = list(action = "details", id = "c3"))
    expect_length(opened(), 0)
    expect_equal(seen$removed, '.outline-tree .outline-node[data-id="c3"] > .question-panel')
    redraw(redraw() + 1L)
    session$flushReact()
    expect_false(grepl("question-panel", tree_html(output))) # a tree drawn again keeps it closed
    expect_false(grepl("question-panel", tree_html(output)))
  })
})

test_that("marking a block as checked moves on to the next one in printed order, and unchecking stays put", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    before <- tree_html(output)
    page$selected("c3")
    session$setInputs(action = list(action = "check", id = "c3"))
    expect_true(component(page$draft(), "c3")$checked)
    expect_equal(page$selected(), "c4")
    expect_identical(tree_html(output), before) # the page script marks it
    expect_equal(marks_now(page, collapsed, opened, ns)$checked, list("c3"))
    redraw(redraw() + 1L)
    session$flushReact()
    expect_true(grepl('is-checked[^>]*data-action="check" data-id="c3"', tree_html(output)))

    session$setInputs(action = list(action = "check", id = "c3"))
    expect_false(component(page$draft(), "c3")$checked)
    expect_equal(page$selected(), "c4")
  })
})

test_that("checking the last block in printed order has nowhere to move on to", {
  page <- short_page()
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
    session$setInputs(action = list(action = "select", id = "c1"))
    session$setInputs(action = list(action = "add_block", type = "Instruction"))
    expect_equal(page$draft()$components$parent_id[page$draft()$components$id == new_id], "c1")

    session$setInputs(action = list(action = "select", id = "c1"))
    section_id <- draft_service_next_id(page$draft(), "components")
    session$setInputs(action = list(action = "add_block", type = "Section"))
    added <- page$draft()$components[page$draft()$components$id == section_id, ]
    expect_equal(added$component_type, "Section")
    expect_true(is.na(added$parent_id)) # beside c1, not inside it
  })
})

test_that("a question gets another variable (and its panel opens), and a variable another choice", {
  page <- short_page()
  boxes <- function() nrow(draft_service_children(page$draft(), "variables", "c3"))
  choices <- function() nrow(draft_service_children(page$draft(), "response_options", "v1"))
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    expect_equal(boxes(), 1)
    tree <- tree_html(output)
    session$setInputs(action = list(action = "add_box", id = "c3"))
    expect_equal(boxes(), 2)
    expect_equal(opened(), "c3")
    expect_identical(tree_html(output), tree) # only the question's panel is drawn
    expect_equal(patched()$panels[[1]]$id, "c3")
    expect_equal(lengths(regmatches(patched()$panels[[1]]$html, gregexpr("variable-card-row", patched()$panels[[1]]$html))), 2)

    expect_equal(choices(), 2)
    session$setInputs(action = list(action = "add_choice", id = "v1"))
    expect_equal(choices(), 3)
  })
})

test_that("Apply to all copies a variable's codes to every variable with the same choices", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(edit = list(table = "response_options", id = "o1", field = "code_value", value = "1"))
    session$setInputs(edit = list(table = "response_options", id = "o2", field = "code_value", value = "0"))
    suppressMessages(session$setInputs(action = list(action = "copy_codes", id = "v1")))
    options <- page$draft()$response_options
    expect_equal(options$code_value[options$variable_id == "v2"], c(1L, 0L))

    session$setInputs(edit = list(table = "response_options", id = "o1", field = "code_value", value = ""))
    session$setInputs(action = list(action = "copy_codes", id = "v1"))
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

test_that("the menu for adding a block offers every kind the database has, and the toolbar counts what is to fix", {
  draft <- draft_service_update(with_matr_ids(short_gold_draft()), "components", "c4", "text", "")
  page <- edit_extraction_state(draft)
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    menu <- text_of(output$add_block_menu)
    for (kind in page$lookups$component_types) expect_match(menu, kind, fixed = TRUE)
    expect_equal(text_of(output$to_fix), "1 block to fix")
    html <- tree_html(output)
    expect_true(grepl('outline-problems"[^>]*title="Add the printed words', html)) # c4's badge, shown
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
    session$setInputs(action = list(action = "details", id = "c3"))
    html <- panel_html(seen)
    expect_match(html, 'class="form-control form-control-sm has-problem"[^>]*aria-invalid="true" title="Variable 1: fill in its MATR ID."[^>]*data-id="v1" data-field="matr_id"')
    # the panel no longer lists what one field is outlined for
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
    session$setInputs(action = list(action = "details", id = "c3"))
    expect_match(panel_html(seen), 'data-field="matr_id"')
    expect_match(panel_html(seen), 'list="[^"]*matr-ids"')
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
  page <- edit_extraction_state(with_checked(short_gold_draft(), c("c3", "c4")))
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
  marks <- outline_marks(draft, draft_service_check(draft, test_lookups()), "c3", "c1", character(), character(), shiny::NS("x"))
  expect_setequal(unlist(marks$checked), c("c1", "c3"))
  expect_equal(unlist(marks$all_checked), "c3") # c1 has blocks inside that aren't checked
  expect_equal(marks$collapsed, list("c1"))
  expect_equal(marks$opened, list())
  expect_equal(marks$selected, "c3")
  expect_equal(marks$check_titles$checked, outline_check_titles[["checked"]])

  everything <- with_checked(short_gold_draft(), c("c1", inside))
  all_marks <- outline_marks(
    everything, draft_service_check(everything, test_lookups()), NULL, character(), character(), character(), shiny::NS("x")
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
    expect_match(patched()$rows[[1]]$html, sprintf('<option value="%s" selected>', questions[1]))
    expect_setequal(names(patched()$numbers), questions)
    in_order <- intersect(draft_service_printed_order(page$draft()), questions)
    expect_equal(vapply(patched()$questions, function(question) question$id, character(1)), in_order)

    expect_identical(tree_html(output), tree) # never drawn again
  })
})

test_that("a field the draft refuses is drawn again as the draft has it", {
  page <- short_page()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    tree_html(output)
    # a section with blocks in it can't become another kind
    session$setInputs(edit = list(table = "components", id = "c1", field = "component_type", value = "Instruction"))
    expect_match(page$log$errors, "Move the blocks out of this section first")
    expect_equal(vapply(patched()$rows, function(row) row$id, character(1)), "c1")
    expect_match(patched()$rows[[1]]$html, "Section")
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
