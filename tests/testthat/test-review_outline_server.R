# The review page's outline: blocks as a tree, each edited where it is. What a click, a keystroke or a drag
# does goes through `state$change()`, which review_state() stands in for. The tree is drawn again only when
# the layout changes; the selected row and the problems are sent to the page script instead.

outline_args <- function(page = review_state()) list(state = page$state, lookups = page$lookups)

sections_of <- function(draft) draft$components$id[draft$components$component_type == "Section"]
has_class <- function(html, class, id) grepl(sprintf('class="[^"]*%s[^"]*" [^>]*data-id="%s"', class, id), html)
tree_html <- function(output) paste(output$tree$html, collapse = "")

test_that("clicking a block selects it, and the selection is drawn the next time the tree is", {
  page <- review_state()
  shiny::testServer(review_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "select", id = "c5"))
    expect_equal(page$selected(), "c5")
    session$setInputs(action = list(action = "select", id = "c9"))
    expect_equal(page$selected(), "c9")
    session$setInputs(action = list(action = "collapse_all")) # the tree is drawn again
    expect_true(has_class(tree_html(output), "is-selected", "c9"))
    expect_false(has_class(tree_html(output), "is-selected", "c5"))
  })
})

test_that("sections open and close one at a time, or all at once", {
  page <- review_state()
  sections <- sections_of(gold_draft())
  shiny::testServer(review_outline_server, args = outline_args(page), {
    expect_length(collapsed(), 0)
    session$setInputs(action = list(action = "toggle", id = "c1"))
    expect_equal(collapsed(), "c1")
    expect_true(has_class(tree_html(output), "is-collapsed", "c1"))
    session$setInputs(action = list(action = "toggle", id = "c1"))
    expect_length(collapsed(), 0)

    session$setInputs(action = list(action = "collapse_all"))
    expect_setequal(collapsed(), sections)
    session$setInputs(action = list(action = "expand_all"))
    expect_length(collapsed(), 0)
  })
})

test_that("selecting a block inside a collapsed section opens the sections around it, and only those", {
  page <- review_state()
  sections <- sections_of(gold_draft())
  shiny::testServer(review_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "collapse_all"))
    session$setInputs(action = list(action = "select", id = "c3"))
    expect_setequal(collapsed(), setdiff(sections, "c1"))
    expect_false(has_class(tree_html(output), "is-collapsed", "c1"))
  })
})

test_that("every block's words are typed in its row, with the words of its kind offered", {
  page <- review_state()
  shiny::testServer(review_outline_server, args = outline_args(page), {
    html <- tree_html(output)
    expect_match(html, 'class="form-control form-control-sm outline-text"')
    expect_match(html, 'data-suggest="[^"]*suggest-Section"')
    expect_match(html, 'data-table="components" data-id="c3" data-field="text"')
    expect_match(html, "Would you call yourself a nervous person?", fixed = TRUE)
    expect_match(html, 'id="[^"]*answer-options"')
    expect_match(html, 'id="[^"]*conditions"')
    # questions are numbered as the draft has them, and every row has its check and delete buttons
    expect_match(text_of(output$tree), "Q1a")
    expect_equal(lengths(regmatches(html, gregexpr('data-action="check"', html))), nrow(gold_draft()$components))
    expect_equal(lengths(regmatches(html, gregexpr('data-action="delete" data-table="components"', html))), nrow(gold_draft()$components))
  })
})

test_that("typing in a field changes the draft and says it was typed; a pick or a tick isn't typed", {
  page <- review_state()
  shiny::testServer(review_outline_server, args = outline_args(page), {
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
  page <- review_state()
  shiny::testServer(review_outline_server, args = outline_args(page), {
    session$setInputs(edit = list(table = "components", id = "gone", field = "text", value = "x"))
    expect_equal(page$log$errors, "That's no longer in the draft.")
    expect_identical(page$draft(), gold_draft())
  })
})

test_that("a question's arrow opens its panel: the question it follows, its skip condition and its variables", {
  page <- review_state()
  shiny::testServer(review_outline_server, args = outline_args(page), {
    expect_false(grepl("question-panel", tree_html(output)))
    session$setInputs(action = list(action = "details", id = "c3"))
    expect_equal(opened(), "c3")
    html <- tree_html(output)
    expect_true(has_class(html, "is-open", "c3"))
    expect_match(html, "question-panel")
    panel <- text_of(output$tree)
    expect_match(panel, "Parent Question")
    expect_match(panel, "Variables")
    expect_false(grepl("Variables ·", panel))
    expect_match(panel, "MATR ID")
    expect_match(html, 'data-table="variables" data-id="v1" data-field="matr_id"')
    expect_match(html, "variable-card-row") # kind, MATR ID, About and Label on one row, with delete
    session$setInputs(action = list(action = "details", id = "c3"))
    expect_length(opened(), 0)
    expect_false(grepl("question-panel", tree_html(output)))
  })
})

test_that("marking a block as checked moves on to the next one in printed order, and unchecking stays put", {
  page <- review_state()
  shiny::testServer(review_outline_server, args = outline_args(page), {
    page$selected("c3")
    session$setInputs(action = list(action = "check", id = "c3"))
    expect_true(component(page$draft(), "c3")$checked)
    expect_equal(page$selected(), "c4")
    expect_true(grepl('is-checked[^>]*data-action="check" data-id="c3"', tree_html(output)))

    session$setInputs(action = list(action = "check", id = "c3"))
    expect_false(component(page$draft(), "c3")$checked)
    expect_equal(page$selected(), "c4")
  })
})

test_that("checking the last block in printed order has nowhere to move on to", {
  page <- review_state()
  last <- utils::tail(draft_service_printed_order(gold_draft()), 1)
  shiny::testServer(review_outline_server, args = outline_args(page), {
    page$selected(last)
    session$setInputs(action = list(action = "check", id = last))
    expect_true(component(page$draft(), last)$checked)
    expect_equal(page$selected(), last)
  })
})

test_that("a new block goes after the selected one and is selected, so its words can be typed", {
  page <- review_state()
  before <- nrow(gold_draft()$components)
  new_id <- draft_service_next_id(gold_draft(), "components")
  shiny::testServer(review_outline_server, args = outline_args(page), {
    session$setInputs(action = list(action = "select", id = "c5"))
    session$setInputs(action = list(action = "add_block", type = "Instruction"))
    expect_equal(nrow(page$draft()$components), before + 1)
    expect_equal(page$selected(), new_id)
    printed <- draft_service_printed_order(page$draft())
    expect_equal(printed[match("c5", printed) + 1], new_id)
    expect_equal(page$draft()$components$component_type[page$draft()$components$id == new_id], "Instruction")
    # drawn selected, with its empty words ready for the cursor
    expect_match(tree_html(output), sprintf('data-autofocus[^>]*data-id="%s"', new_id))
  })
})

test_that("with a section selected a new block goes inside it, unless the new block is a section too", {
  page <- review_state()
  new_id <- draft_service_next_id(gold_draft(), "components")
  shiny::testServer(review_outline_server, args = outline_args(page), {
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
  page <- review_state()
  boxes <- function() nrow(draft_service_children(page$draft(), "variables", "c3"))
  choices <- function() nrow(draft_service_children(page$draft(), "response_options", "v1"))
  shiny::testServer(review_outline_server, args = outline_args(page), {
    expect_equal(boxes(), 1)
    session$setInputs(action = list(action = "add_box", id = "c3"))
    expect_equal(boxes(), 2)
    expect_equal(opened(), "c3")
    expect_equal(lengths(regmatches(tree_html(output), gregexpr("variable-card-row", tree_html(output)))), 2)

    expect_equal(choices(), 2)
    session$setInputs(action = list(action = "add_choice", id = "v1"))
    expect_equal(choices(), 3)
  })
})

test_that("Apply to all copies a variable's codes to every variable with the same choices", {
  page <- review_state()
  shiny::testServer(review_outline_server, args = outline_args(page), {
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
  page <- review_state()
  shiny::testServer(review_outline_server, args = outline_args(page), {
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
  page <- review_state()
  draft <- gold_draft()
  printed <- draft_service_printed_order(draft)
  parent_of <- function(ids) draft$components$parent_id[match(ids, draft$components$id)]
  swapped <- printed
  swapped[match(c("c3", "c4"), printed)] <- c("c4", "c3")
  as_sent <- function(ids, parents) list(ids = as.list(ids), parents = as.list(ifelse(is.na(parents), "", parents)))
  shiny::testServer(review_outline_server, args = outline_args(page), {
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
  page <- review_state()
  shiny::testServer(review_outline_server, args = outline_args(page), {
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
  draft <- draft_service_update(with_matr_ids(), "components", "c4", "text", "")
  page <- review_state(draft)
  shiny::testServer(review_outline_server, args = outline_args(page), {
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

test_that("the words offered for a kind of block are those the review page shares", {
  page <- review_state()
  page$state$suggestions <- function() list(Section = c("Part A", "Part B"), Instruction = "Tick one.")
  shiny::testServer(review_outline_server, args = outline_args(page), {
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
  page <- review_state(with_checked(gold_draft(), c("c3", "c4")))
  shiny::testServer(review_outline_server, args = outline_args(page), {
    session$setInputs(edit = list(table = "components", id = "c3", field = "text", value = "Do you worry a lot?"))
    expect_equal(checked_blocks(page$draft()), "c4")
    session$setInputs(action = list(action = "check", id = "c3"))
    expect_equal(checked_blocks(page$draft()), c("c3", "c4"))
    session$setInputs(edit = list(table = "variables", id = "v2", field = "label", value = "Worrier"))
    expect_equal(checked_blocks(page$draft()), "c3")
  })
})
