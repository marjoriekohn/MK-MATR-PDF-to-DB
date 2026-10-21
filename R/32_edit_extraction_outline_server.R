#' The questionnaire outline of the Edit Extraction page.
#'
#' Blocks as a tree, each edited where it is: type a block's words into its row, pick the question it
#' follows, open and close sections, open a question's panel for its skip condition and variables, mark
#' a block as checked, add and delete blocks, variables and choices, and drag any of them into a new
#' order. Every change goes through `state$change()`.
#'
#' The tree is drawn again only when the layout changes (a block or variable added, deleted, moved or
#' picked from a list), never as words are typed, so the field being typed in is left alone. Drawing it
#' takes seconds for a long questionnaire, so what a click or keystroke changes without changing the
#' layout (which block is selected, which sections are closed and question panels open, which blocks
#' are checked, which blocks and fields have problems, and which uploaded MATR IDs are still free) is
#' sent to the page script, which marks the rows and fields (`outline_marks()`); a question's panel is
#' added to the tree, or taken out, on its own.
#'
#' @param id The module's id.
#' @param state What the Edit Extraction page shares: `list(draft, change, selected, problems,
#'   layout_version, suggestions, matr_ids)`; `suggestions` (a reactive of
#'   `draft_service_text_suggestions()`), `matr_ids` (a reactive of the uploaded MATR IDs no variable
#'   has, `draft_service_available_matr_ids()`) and `show_variables` (a reactive that ticks when every
#'   question's variables should be shown, to give out MATR IDs by hand) may be left out.
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
    opened <- reactiveVal(character()) # questions whose panel is open
    redraw <- reactiveVal(0L) # ticks when the tree must be drawn again although the layout hasn't changed
    suggestions <- state$suggestions %||% function() list()
    matr_ids <- state$matr_ids %||% function() character()

    observeEvent(input$action, {
      event <- input$action
      draft <- state$draft()
      blocks <- draft$components
      switch(event$action,
             select = state$selected(event$id),
             toggle = collapsed(flip(collapsed(), event$id)),
             details = toggle_panel(draft, event$id),
             collapse_all = collapsed(blocks$id[blocks$component_type == "Section"]),
             expand_all = collapsed(character()),
             add_block = add_block(event$type),
             check = toggle_checked(draft, event$id),
             add_box = {
               opened(union(opened(), event$id))
               state$change(draft_service_add_box(draft, event$id))
             },
             add_choice = state$change(draft_service_add_choice(draft, event$id)),
             copy_codes = state$change(draft_service_copy_codes(draft, event$id)),
             delete = {
               deleted <- state$change(draft_service_delete(draft, event$table, event$id))
               if (deleted && event$table == "components" && identical(state$selected(), event$id)) state$selected(NULL)
             }
      )
    })

    flip <- function(ids, id) if (id %in% ids) setdiff(ids, id) else c(ids, id)

    # A question's panel is added to the tree already drawn, or taken out of it.
    toggle_panel <- function(draft, id) {
      node <- sprintf('.outline-tree .outline-node[data-id="%s"]', id)
      if (id %in% opened()) {
        opened(setdiff(opened(), id))
        removeUI(paste(node, "> .question-panel"))
        return()
      }
      block <- draft$components[draft$components$id == id, , drop = FALSE]
      if (nrow(block) != 1 || block$component_type != "Question") {
        return()
      }
      opened(c(opened(), id))
      insertUI(node, "beforeEnd", question_panel(block, draft, current_view(), lookups, ns))
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
    toggle_checked <- function(draft, id) {
      checked <- !draft$components$checked[draft$components$id == id]
      # The page script marks it: nothing in the tree's layout changes.
      marked <- state$change(draft_service_update(draft, "components", id, "checked", checked), redraw = FALSE)
      if (marked && checked) {
        printed <- draft_service_printed_order(draft)
        next_id <- printed[match(id, printed) + 1]
        if (!is.na(next_id)) state$selected(next_id)
      }
    }

    # Typing, picking and ticking in the fields.
    observeEvent(input$edit, {
      edit <- input$edit
      state$change(
        draft_service_update(state$draft(), edit$table, edit$id, edit$field, edit$value),
        typed = isTRUE(edit$typed)
      )
    })

    # Drag and drop: the page script sends every block id in printed order, with
    # the section each one is in ("" for the top level).
    observeEvent(input$order, {
      parent_ids <- unlist(input$order$parents)
      parent_ids[parent_ids == ""] <- NA
      state$change(draft_service_apply_order(state$draft(), unlist(input$order$ids), parent_ids))
    })

    # Variables or choices dragged into a new order.
    observeEvent(input$reorder, {
      state$change(draft_service_reorder(state$draft(), input$reorder$table, unlist(input$reorder$ids)))
    })

    # Giving out MATR IDs by hand: every question's panel opens, so each variable's MATR ID can be picked.
    if (!is.null(state$show_variables)) {
      observeEvent(state$show_variables(), {
        blocks <- state$draft()$components
        opened(blocks$id[blocks$component_type == "Question"])
        collapsed(character())
        redraw(redraw() + 1L) # every panel at once: drawn with the tree
      }, ignoreInit = TRUE)
    }

    # Open the sections around the selected block, so it can be seen.
    observeEvent(state$selected(), {
      collapsed(setdiff(collapsed(), draft_service_ancestors(state$draft(), state$selected())))
    })

    # What changes without the layout changing, marked on the tree already drawn (outline_marks()).
    observe({
      session$sendCustomMessage("edit-extraction-marks", outline_marks(
        state$draft(), state$problems(), state$selected(), collapsed(), opened(), matr_ids(), ns
      ))
    })

    # What the tree is drawn with: what is selected, closed, open and at fault now.
    current_view <- function() {
      list(
        selected = state$selected(), collapsed = collapsed(), opened = opened(),
        problems = state$problems(), suggestions = suggestions(), matr_ids = matr_ids()
      )
    }

    # ------ OUTPUT ------------------------------------------------------------
    output$add_block_menu <- renderUI(add_block_menu(lookups$component_types))

    output$to_fix <- renderUI(to_fix_note(state$problems()))

    output$tree <- renderUI({
      state$layout_version()
      redraw()
      isolate(outline_tree(state$draft(), current_view(), lookups, ns))
    })
  })
}
