# The Edit Extraction page's outline: blocks as a tree, each edited where it is. What a click, a keystroke or a drag
# does goes through `state$change()`, which edit_extraction_state() stands in for. The tree is drawn again only when
# the layout changes; the selected row, the closed sections and open panels, the checked blocks and the problems
# are sent to the page script instead (outline_marks()), and a question's panel is added on its own.

# Every test gives the outline the gold questionnaire's first two parts (short_gold_draft()): the outline draws
# every block it is given, again each time the layout changes, and all of the gold questionnaire's takes seconds.
short_page <- function(draft = short_gold_draft()) edit_extraction_state(draft)
outline_args <- function(page = short_page()) list(state = page$state, lookups = page$lookups)

sections_of <- function(draft) draft$components$id[is_container(draft$components$component_type)]
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
    expect_match(html, 'data-suggest="[^"]*suggest-Instruction"')
    expect_match(html, 'class="outline-group-note"') # a question group has no words to type
    expect_match(html, 'data-table="components" data-id="c3" data-field="text"')
    expect_match(html, "Would you call yourself a nervous person?", fixed = TRUE)
    expect_match(html, 'id="[^"]*answer-options"')
    expect_match(html, 'id="[^"]*conditions"')
    # a question's row has its place among the questions, without a "Q", and the number printed beside
    # it, typed in its own field; every row has its check and delete buttons
    expect_false(grepl("Q1a", text_of(output$tree), fixed = TRUE))
    expect_match(html, '<span class="outline-number"[^>]*>1</span>') # its place among the questions
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
    # the question it follows is typed or picked in the question's row from one list every row shares, with no tooltip
    expect_match(html, 'placeholder="Parent question"[^>]*data-pick[^>]*data-table="components" data-id="c3" data-field="parent_question_id"')
    expect_match(html, '<datalist id="[^"]*parent-questions">')
    expect_false(grepl("<select[^>]*parent_question_id", html))
    expect_false(grepl("field-hint", html))
    # its condition shows beside it once it has a parent, or has words
    expect_false(grepl('data-id="c3" data-field="condition"', html))
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
    # every choice is printed, so there is no check box to say one isn't
    expect_false(grepl('type="checkbox"|data-field="printed"', html))
    # one line per variable: kind, MATR ID, and its choices in short, closed until clicked; who it's about
    # (blank is the respondent) and its label show only when they have something in them, or are added
    expect_match(html, "variable-head") # the column names
    expect_match(html, 'data-table="variables" data-id="v1" data-field="matr_id"')
    expect_false(grepl('data-id="v1" data-field="(subject|label)"', html))
    expect_match(html, 'data-action="show_field" data-id="v1" data-name="subject"')
    expect_match(html, 'data-action="show_field" data-id="v1" data-name="label"')
    expect_match(html, 'data-action="choices" data-id="v1"')
    expect_match(text_of(html), "1 Yes · 2 No", fixed = TRUE)
    expect_false(grepl('data-field="answer_option"', html))
    expect_false(grepl("Pick or type|Leave blank if no difference|choice-heading", html))
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
    expect_equal(lengths(regmatches(patched()$panels[[1]]$html, gregexpr('class="variable-card( [^"]*)?"', patched()$panels[[1]]$html))), 2)

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
    listed <- outline_questions(page$draft())$questions
    label <- listed$label[listed$id == questions[1]]
    expect_match(patched()$rows[[1]]$html, paste0('value="', htmltools::htmlEscape(label, attribute = TRUE), '"'), fixed = TRUE)
    expect_match(patched()$rows[[1]]$html, 'data-field="condition"') # it has a parent, so its condition can be typed
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
    session$setInputs(action = list(action = "select", id = "c1"))
    suppressMessages(session$setInputs(action = list(action = "ungroup", id = "c1")))
    expect_false("c1" %in% page$draft()$components$id)
    expect_equal(head(draft_service_children(page$draft(), "components", NA)$id, length(inside)), inside)
    expect_equal(page$selected(), inside[1])
  })
})

test_that("a variable's choices open under its line, code first, and close again", {
  page <- short_page()
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "details", id = "c3"))
    session$setInputs(action = list(action = "choices", id = "v1"))
    expect_equal(choices_open(), "v1")
    html <- patched()$panels[[1]]$html
    expect_match(html, 'aria-expanded="true"')
    expect_match(html, 'placeholder="Response option"')
    expect_match(html, 'placeholder="Code"')
    expect_lt(regexpr('data-field="code_value"', html), regexpr('data-field="answer_option"', html))
    # a choice's printed words and skip instruction show only on a choice that has them; "+" adds them
    expect_false(grepl('data-field="printed_label"|data-field="skip_instruction"', html))
    expect_match(html, 'data-action="show_field" data-id="o1" data-name="printed_label"')
    session$setInputs(action = list(action = "choices", id = "v1"))
    expect_length(choices_open(), 0)
    expect_false(grepl('data-field="answer_option"', patched()$panels[[1]]$html))
  })
})

test_that("a choice's printed words or skip are added to that choice only, with the cursor in them, and × takes them away", {
  page <- edit_extraction_state(draft_service_update(short_gold_draft(), "response_options", "o2", "skip_instruction", "Go to 1c"))
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "details", id = "c3"))
    session$setInputs(action = list(action = "choices", id = "v1"))
    html <- patched()$panels[[1]]$html
    expect_equal(lengths(regmatches(html, gregexpr('data-field="skip_instruction"', html))), 1) # o2's, which has one
    session$setInputs(action = list(action = "show_field", id = "o1", name = "printed_label"))
    expect_equal(shown(), "o1:printed_label")
    html <- patched()$panels[[1]]$html
    expect_equal(lengths(regmatches(html, gregexpr('data-field="printed_label"', html))), 1)
    expect_match(html, 'data-id="o1" data-field="printed_label"[^>]*data-focus-now|data-focus-now[^>]*data-id="o1" data-field="printed_label"')
    expect_match(html, 'data-action="hide_field" data-id="o1" data-name="printed_label"')
    # the × of a field with words empties it, which Undo can take back
    session$setInputs(action = list(action = "hide_field", id = "o2", name = "skip_instruction"))
    expect_true(is.na(component_choice(page$draft(), "o2")$skip_instruction))
    session$setInputs(action = list(action = "hide_field", id = "o1", name = "printed_label"))
    expect_length(shown(), 0)
    expect_false(grepl('data-field="printed_label"|data-field="skip_instruction"', patched()$panels[[1]]$html))
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
    session$setInputs(action = list(action = "details", id = "c3"))
    html <- panel_html(seen)
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
    expect_false(grepl("data-shared=\"v1", patched()$panels[[1]]$html))
    expect_match(patched()$panels[[1]]$html, 'data-action="join_choices"')
  })
})

test_that("a variable's About and label show when they have something in them, and can be added and taken away", {
  draft <- draft_service_update(short_gold_draft(), "variables", "v1", "subject", "Respondent's Family")
  page <- edit_extraction_state(draft)
  seen <- local_browser()
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "details", id = "c3"))
    html <- panel_html(seen)
    expect_match(html, 'data-id="v1" data-field="subject"[^>]*value="Respondent&#39;s Family"|value="Respondent&#39;s Family"[^>]*data-id="v1" data-field="subject"')
    expect_match(html, 'data-action="hide_field" data-id="v1" data-name="subject"')
    expect_false(grepl('data-action="show_field" data-id="v1" data-name="subject"', html))
    # a label is added, empty, with the cursor in it; × on About makes it the respondent again
    session$setInputs(action = list(action = "show_field", id = "v1", name = "label"))
    expect_equal(shown(), "v1:label")
    expect_match(patched()$panels[[1]]$html, 'data-id="v1" data-field="label"[^>]*data-focus-now|data-focus-now[^>]*data-id="v1" data-field="label"')
    session$setInputs(action = list(action = "hide_field", id = "v1", name = "subject"))
    expect_true(is.na(page$draft()$variables$subject[page$draft()$variables$id == "v1"]))
    expect_false(grepl('data-field="subject"', patched()$panels[[1]]$html))
  })
})

test_that("a write-in variable has the same + for its About and label", {
  draft <- draft_service_update(short_gold_draft(), "variables", "v1", "variable_type", "open_ended")
  view <- list(problems = draft_service_check(draft, test_lookups()), shown = character())
  html <- htmltools::renderTags(variable_line(draft$variables[draft$variables$id == "v1", ], draft, view, test_lookups(), shiny::NS("x")))$html
  expect_match(html, 'data-field="answer_data_type"')
  expect_match(html, 'data-action="show_field" data-id="v1" data-name="subject"')
  expect_match(html, 'data-action="show_field" data-id="v1" data-name="label"')
})

test_that("Skip to is a pick of where the choice sends respondents, beside its printed words, and × takes both away", {
  draft <- short_gold_draft()
  draft <- draft_service_update(draft, "response_options", "o2", "skip_instruction", "Go to Question 1c")
  page <- edit_extraction_state(draft)
  seen <- local_browser()
  target <- draft$components$id[draft_service_question_number(draft$components) %in% "1c"]
  expect_equal(draft$response_options$skip_to_id[draft$response_options$id == "o2"], target) # linked from its words
  shiny::testServer(extraction_outline_server, args = outline_args(page), {
    expect_match(tree_html(output), '<datalist id="[^"]*skip-targets">')
    session$setInputs(action = list(action = "details", id = "c3"))
    session$setInputs(action = list(action = "choices", id = "v1"))
    html <- patched()$panels[[1]]$html
    expect_match(html, 'placeholder="Skips to"[^>]*data-pick[^>]*data-id="o2" data-field="skip_to_id"')
    expect_match(html, 'value="1c · [^"]*"[^>]*data-id="o2" data-field="skip_to_id"|data-id="o2" data-field="skip_to_id"[^>]*value="1c · ')
    expect_match(html, 'data-id="o2" data-field="skip_instruction"')
    session$setInputs(action = list(action = "hide_field", id = "o2", name = "skip_instruction"))
    choice <- component_choice(page$draft(), "o2")
    expect_true(is.na(choice$skip_instruction))
    expect_true(is.na(choice$skip_to_id))
  })
})
