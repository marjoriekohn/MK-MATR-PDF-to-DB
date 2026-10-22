#' The questionnaire outline of the Edit Extraction page.
#'
#' Blocks as a list, with the picked block's details beside it (`block_detail()`): type a section's or a text
#' block's words in the list, or any block's in its details, change a block's kind, open and close sections and
#' question groups (by clicking them), add skip logic to a question (the answer to an earlier question it
#' depends on, or who the respondent is), verify a block, add blocks (from a dialog of the kinds there are),
#' delete them, take a section or group away but keep what is inside, and drag any of them into a new order. A
#' question's variables and choices are the variable editor's (`variable_editor()`), as on Make an edit. Every
#' change goes through `state$change()`.
#'
#' Drawing the whole list takes seconds for a long questionnaire, for the server and again for the
#' browser, so it is drawn only when a draft opens or much of it changes. What a click or keystroke
#' changes without changing the layout (which block is selected, which sections are closed, which blocks
#' are checked, which blocks and fields have problems, and which uploaded MATR IDs are still free) is sent to
#' the page script, which marks the rows and fields (`outline_marks()`). When the layout changes (a block or
#' variable added, deleted, moved or picked from a list) only what is different is drawn and put in place
#' (`outline_patch()`). Words being typed are never drawn again, so the field being typed in is left alone.
#' The details are drawn again when the block picked or the layout changes, never for words typed.
#'
#' @param id The module's id.
#' @param state What the Edit Extraction page shares: `list(draft, change, selected, problems,
#'   layout_version, suggestions, matr_ids)`; `suggestions` (a reactive of
#'   `draft_service_text_suggestions()`), `matr_ids` (a reactive of the uploaded MATR IDs no variable
#'   has, `draft_service_available_matr_ids()`), `show_variables` (a reactive that ticks when the list should
#'   show each question's MATR IDs, to give them out by hand), `original` (a reactive of the draft as the AI
#'   read it, for the "Changed from AI" filter; `NULL` for a draft typed by hand) and `loaded` (a reactive that
#'   ticks when a draft is opened) may be left out.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @return Called for its effect.
extraction_outline_server <- function(
  id,
  state,
  lookups
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    collapsed <- reactiveVal(character()) # sections whose contents are hidden
    redraw <- reactiveVal(0L) # ticks when the whole tree must be drawn again
    drawn <- reactiveVal(NULL) # the draft as the tree on the page shows it
    patched <- reactiveVal(NULL) # the last change put on the tree already drawn (outline_patch())
    shown_version <- isolate(state$layout_version()) # the layout_version the tree on the page is up to date with
    suggestions <- state$suggestions %||% function() list()
    matr_ids <- state$matr_ids %||% function() character()
    original <- state$original %||% function() NULL
    logic_for <- reactiveVal(NULL) # the question Add Skip Logic is adding to
    detail_version <- reactiveVal(0L) # ticks when the details must be drawn again without the layout changing

    # Draws the details again for what changed in a question's variables without changing the draft, if it is
    # the one picked.
    redraw_question <- function(question) {
      if (any(question %in% state$selected())) detail_version(detail_version() + 1L)
    }

    # The picked question's variables and choices; how they are shown (which choices are open, the optional
    # fields added empty, the grids edited one by one) is the editor's.
    variables <- variable_editor(state$draft, state$change, redraw_question, ns)
    shown <- variables$shown
    choices_open <- variables$choices_open
    split <- variables$split

    flip <- function(ids, id) if (id %in% ids) setdiff(ids, id) else c(ids, id)

    observeEvent(input$action, {
      event <- input$action
      if (variables$action(event)) {
        return()
      }
      draft <- state$draft()
      blocks <- draft$components
      switch(event$action,
             select = select_block(draft, event$id),
             details = select_block(draft, event$id), # a question's variables are in its details
             toggle = collapsed(flip(collapsed(), event$id)),
             collapse_all = collapsed(blocks$id[is_container(blocks$component_type)]),
             expand_all = collapsed(character()),
             pick_block = showModal(add_block_dialog(ns, lookups$component_types)),
             add_block = {
               removeModal()
               add_block(event$type)
             },
             check = toggle_checked(draft, event$id),
             edit_kind = state$change(draft_service_update(draft, "components", event$id, "component_type", event$type)),
             add_skip_logic = {
               logic_for(event$id)
               showModal(skip_logic_choice_modal(ns))
             },
             remove_logic = remove_logic(draft, event$id, event$kind),
             ungroup = {
               inside <- draft_service_children(draft, "components", event$id)$id
               ungrouped <- state$change(draft_service_ungroup(draft, event$id))
               if (ungrouped && identical(state$selected(), event$id)) state$selected(inside[1])
             },
             delete = {
               deleted <- state$change(draft_service_delete(draft, "components", event$id))
               if (deleted && identical(state$selected(), event$id)) state$selected(NULL)
             }
      )
    })

    # Picking a section or question group opens its first question (it opens itself when it holds none).
    select_block <- function(draft, id) {
      blocks <- draft$components
      if (is_container(blocks$component_type[match(id, blocks$id)])) {
        inside <- draft_service_printed_order(draft, id)
        first <- inside[blocks$component_type[match(inside, blocks$id)] %in% "Question"][1]
        if (!is.na(first)) id <- first
      }
      state$selected(id)
    }

    # ------ Skip logic -----------------------------------------------------------------
    # Add Skip Logic asks which kind: shown only for some answers to an earlier question, or only to some
    # respondents (a condition, e.g. "Twins only"). Saving the first makes that question its parent, with the
    # answers as its condition; saving the second makes the words its condition, and an earlier question picked
    # as its parent is taken away (one worked out from a skip stays, as the skip says).
    observeEvent(input$logic_answer, {
      id <- req(logic_for())
      showModal(skip_logic_answer_modal(ns, state$draft(), id))
    })

    output$logic_answers <- renderUI({
      id <- req(logic_for())
      skip_logic_answer_choices(ns, state$draft(), id, input$logic_parent)
    })

    observeEvent(input$logic_answer_save, {
      id <- req(logic_for())
      parent <- input$logic_parent %||% ""
      # its choices ticked, or for a question without choices the answer typed (Shiny keeps the other's last value)
      has_choices <- nzchar(parent) && length(skip_logic_parent_answers(state$draft(), parent)) > 0
      answers <- trimws(if (has_choices) input$logic_answer_picks else input$logic_answer_text)
      answers <- answers[nzchar(answers)]
      if (!nzchar(parent) || length(answers) == 0) {
        showNotification("Pick the earlier question and the answer that shows this one.", type = "warning")
        return()
      }
      draft <- state$draft()
      changed <- state$change({
        linked <- draft_service_update(draft, "components", id, "parent_question_id", parent)
        draft_service_update(linked, "components", id, "condition", paste(answers, collapse = " or "))
      })
      if (changed) removeModal()
    })

    observeEvent(input$logic_condition, {
      id <- req(logic_for())
      block <- state$draft()$components[state$draft()$components$id == id, , drop = FALSE]
      current <- if (nrow(block) == 1 && (is.na(block$parent_question_id) || isTRUE(block$parent_from_skip))) block$condition else NA
      showModal(skip_logic_condition_modal(ns, current))
    })

    observeEvent(input$logic_condition_save, {
      id <- req(logic_for())
      words <- trimws(input$logic_condition_text %||% "")
      if (!nzchar(words)) {
        showNotification("Say who sees this question, e.g. Twins.", type = "warning")
        return()
      }
      draft <- state$draft()
      block <- draft$components[draft$components$id == id, , drop = FALSE]
      changed <- state$change({
        unlinked <- if (!is.na(block$parent_question_id) && !isTRUE(block$parent_from_skip)) {
          draft_service_update(draft, "components", id, "parent_question_id", "")
        } else {
          draft
        }
        draft_service_update(unlinked, "components", id, "condition", words)
      })
      if (changed) removeModal()
    })

    # The delete button of a sentence of skip logic: the earlier question picked (and its answers), or the
    # condition.
    remove_logic <- function(draft, id, kind) {
      state$change(
        if (identical(kind, "answer")) {
          draft_service_update(draft_service_update(draft, "components", id, "parent_question_id", ""), "components", id, "condition", "")
        } else {
          draft_service_update(draft, "components", id, "condition", "")
        }
      )
    }

    # A new block goes next to the selected one, and is selected so its words can be typed.
    add_block <- function(component_type) {
      draft <- state$draft()
      id <- draft_service_next_id(draft, "components")
      added <- state$change(draft_service_add_block(draft, component_type, id, near_id = state$selected()))
      if (added) state$selected(id)
    }

    # Marking a block as checked moves on to the next one, so a reviewer can
    # work down the outline.
    # A section or question group closes once everything in it is checked, and the next block selected is
    # the first after it (never a section or group itself).
    # A block is only verified once nothing is left to fix in it; a section or group is checked once
    # everything in it is, so it has no check of its own.
    toggle_checked <- function(draft, id) {
      block <- draft$components[draft$components$id == id, , drop = FALSE]
      if (nrow(block) != 1 || is_container(block$component_type)) {
        return()
      }
      checked <- !block$checked
      to_fix <- sum(state$problems()$id %in% id)
      if (checked && to_fix > 0) {
        showNotification(sprintf("Fix what's marked first (%s), then verify it.", plural(to_fix, "problem")), type = "warning")
        return()
      }
      updated <- draft_service_update(draft, "components", id, "checked", checked)
      # The page script marks it: nothing in the tree's layout changes.
      marked <- state$change(updated, redraw = FALSE)
      if (marked && checked) {
        done <- finished_containers(updated, id)
        if (length(done) > 0) collapsed(union(collapsed(), done))
        printed <- draft_service_printed_order(updated)
        inside_done <- unlist(lapply(done, function(container) draft_service_printed_order(updated, container)))
        after <- printed[seq_along(printed) > match(id, printed) & !printed %in% inside_done]
        after <- after[!is_container(updated$components$component_type[match(after, updated$components$id)])]
        if (length(after) > 0) state$selected(after[1])
      }
    }

    # The sections and question groups around a block everything in which is checked.
    finished_containers <- function(draft, id) {
      blocks <- draft$components
      around <- draft_service_ancestors(draft, id)
      around <- around[is_container(blocks$component_type[match(around, blocks$id)])]
      Filter(function(container) {
        inside <- draft_service_printed_order(draft, container)
        inside <- inside[!is_container(blocks$component_type[match(inside, blocks$id)])]
        length(inside) > 0 && all(blocks$checked[match(inside, blocks$id)] %in% TRUE)
      }, around)
    }

    # Typing, picking and ticking in the fields. A change the draft refuses puts the field back as the
    # draft has it.
    observeEvent(input$edit, {
      if (!variables$edit(input$edit)) put_back(input$edit$table, input$edit$id)
    })

    # Drag and drop: the page script sends every block id in printed order, with
    # the section each one is in ("" for the top level).
    observeEvent(input$order, {
      parent_ids <- unlist(input$order$parents)
      parent_ids[parent_ids == ""] <- NA
      state$change(draft_service_apply_order(state$draft(), unlist(input$order$ids), parent_ids))
    })

    # Variables or choices dragged into a new order; a new order the draft refuses is put back.
    observeEvent(input$reorder, {
      if (!variables$reorder(input$reorder)) put_back(input$reorder$table, unlist(input$reorder$ids)[1])
    })

    # Apply to All?'s Apply
    observeEvent(input$apply_codes, {
      if (variables$apply_codes()) removeModal()
    })

    # Draws again the row and the details that hold a field of `table`'s row `id`, as the draft has it.
    put_back <- function(table, id) {
      draft <- state$draft()
      if (table == "components") {
        patch <- outline_patch(drawn(), draft, current_view(), lookups, ns, rows = id)
        if (!is.null(patch)) send_patch(patch)
      }
      detail_version(detail_version() + 1L)
    }

    send_patch <- function(patch) {
      patched(patch)
      session$sendCustomMessage("edit-extraction-patch", patch)
    }

    # Giving out MATR IDs by hand: the list shows each question's variables in short under its row, a lone
    # variable's MATR ID typed there (the page script shows them: "edit-extraction-assign"); a question with
    # several has them in its details. Every section opens.
    if (!is.null(state$show_variables)) {
      observeEvent(state$show_variables(), {
        session$sendCustomMessage("edit-extraction-assign", TRUE)
        collapsed(character())
      }, ignoreInit = TRUE)
    }

    # A draft opens with every section and question group closed, and the MATR IDs in the list put away.
    if (!is.null(state$loaded)) {
      observeEvent(state$loaded(), {
        blocks <- state$draft()$components
        collapsed(blocks$id[is_container(blocks$component_type)])
        session$sendCustomMessage("edit-extraction-assign", FALSE)
      })
    }

    # Open the sections around the selected block, so it can be seen.
    observeEvent(state$selected(), {
      collapsed(setdiff(collapsed(), draft_service_ancestors(state$draft(), state$selected())))
    })

    # What changes without the layout changing, marked on the tree already drawn (outline_marks()).
    observe({
      draft <- state$draft()
      session$sendCustomMessage("edit-extraction-marks", outline_marks(
        draft, state$problems(), state$selected(), collapsed(), matr_ids(), ns,
        changed = draft_service_changed_blocks(original(), draft)
      ))
    })

    # What the tree is drawn with: what is selected, closed, open and at fault now.
    current_view <- function() {
      c(
        list(
          selected = state$selected(), collapsed = collapsed(),
          problems = state$problems(), suggestions = suggestions(), matr_ids = matr_ids(),
          component_types = lookups$component_types
        ),
        variables$view()
      )
    }

    # ------ OUTPUT ------------------------------------------------------------
    output$to_fix <- renderUI(to_fix_note(state$problems()))

    # A change to the layout (layout_version ticks) puts only what is different on the tree already drawn,
    # unless so much changed that drawing it all again is quicker. A change that didn't tick it (words
    # typed, a block marked as checked) is on the page already.
    observe({
      draft <- state$draft()
      version <- state$layout_version()
      isolate({
        if (identical(version, shown_version)) {
          drawn(draft)
          return()
        }
        shown_version <<- version
        patch <- outline_patch(drawn(), draft, current_view(), lookups, ns)
        if (is.null(patch)) {
          redraw(redraw() + 1L)
          return()
        }
        send_patch(patch)
        drawn(draft)
      })
    })

    # The picked block's details: drawn again when another block is picked, the layout changes (anything
    # but words typed and check marks, which the page script keeps up to date), or a question's variables
    # are shown differently.
    output$detail <- renderUI({
      id <- state$selected()
      state$layout_version()
      detail_version()
      isolate({
        draft <- state$draft()
        view <- current_view()
        view$focus <- variables$take_focus()
        block_detail(if (!is.null(id)) draft$components[draft$components$id == id, , drop = FALSE], draft, view, lookups, ns)
      })
    })

    output$tree <- renderUI({
      redraw()
      isolate({
        variables$reset() # fields added and left empty go when the whole outline is drawn again
        draft <- state$draft()
        drawn(draft)
        outline_tree(draft, current_view(), lookups, ns)
      })
    })
  })
}
