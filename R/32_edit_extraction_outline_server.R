#' The questionnaire outline of the Edit Extraction page.
#'
#' Blocks as a tree, each edited where it is: type a block's words into its row, pick the question it
#' follows and type its condition, open and close sections and question groups, open a question's panel
#' for its variables, add a variable's optional fields, mark a block as checked, add and delete blocks,
#' variables and choices, and drag any of them into a new order. Every change goes through
#' `state$change()`.
#'
#' Drawing the whole tree takes seconds for a long questionnaire, for the server and again for the
#' browser, so it is drawn only when a draft opens or much of it changes. What a click or keystroke
#' changes without changing the layout (which block is selected, which sections are closed and question
#' panels open, which blocks are checked, which blocks and fields have problems, and which uploaded MATR
#' IDs are still free) is sent to the page script, which marks the rows and fields (`outline_marks()`).
#' When the layout changes (a block or variable added, deleted, moved or picked from a list) only what
#' is different is drawn and put in place (`outline_patch()`). Words being typed are never drawn again,
#' so the field being typed in is left alone; a question's panel is added to the tree, or taken out, on
#' its own.
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
    shown <- reactiveVal(character()) # optional fields added to variables, as "v12:label" (see variable_card())
    focus <- NULL # the optional field just added, which gets the cursor when it is drawn
    redraw <- reactiveVal(0L) # ticks when the whole tree must be drawn again
    drawn <- reactiveVal(NULL) # the draft as the tree on the page shows it
    patched <- reactiveVal(NULL) # the last change put on the tree already drawn (outline_patch())
    shown_version <- isolate(state$layout_version()) # the layout_version the tree on the page is up to date with
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
             collapse_all = collapsed(blocks$id[is_container(blocks$component_type)]),
             expand_all = collapsed(character()),
             add_block = add_block(event$type),
             check = toggle_checked(draft, event$id),
             add_box = {
               opened(union(opened(), event$id))
               state$change(draft_service_add_box(draft, event$id))
             },
             add_choice = state$change(draft_service_add_choice(draft, event$id)),
             copy_codes = state$change(draft_service_copy_codes(draft, event$id)),
             show_field = show_field(event$id, event$name),
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

    # An optional field added to a variable shows, empty, with the cursor in it; the variable's panel is
    # drawn again for it. It stays until the outline is drawn again from scratch, unless something is
    # typed into it.
    show_field <- function(id, name) {
      if (!name %in% names(optional_variable_fields)) {
        return()
      }
      shown(union(shown(), paste0(id, ":", name)))
      focus <<- paste0(id, ":", name)
      put_back("variables", id)
      focus <<- NULL
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

    # Typing, picking and ticking in the fields. A change the draft refuses puts the field back as the
    # draft has it.
    observeEvent(input$edit, {
      edit <- input$edit
      changed <- state$change(
        draft_service_update(state$draft(), edit$table, edit$id, edit$field, edit$value),
        typed = isTRUE(edit$typed)
      )
      if (!changed) put_back(edit$table, edit$id)
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
      ids <- unlist(input$reorder$ids)
      changed <- state$change(draft_service_reorder(state$draft(), input$reorder$table, ids))
      if (!changed) put_back(input$reorder$table, ids[1])
    })

    # Draws again the row or panel that holds a field of `table`'s row `id`, as the draft has it.
    put_back <- function(table, id) {
      draft <- state$draft()
      variable <- switch(table,
        variables = id,
        response_options = draft$response_options$variable_id[draft$response_options$id %in% id]
      )
      question <- draft$variables$component_id[draft$variables$id %in% variable]
      patch <- outline_patch(
        drawn(), draft, current_view(), lookups, ns,
        rows = if (table == "components") id, panels = intersect(question, opened())
      )
      if (!is.null(patch)) send_patch(patch)
    }

    send_patch <- function(patch) {
      patched(patch)
      session$sendCustomMessage("edit-extraction-patch", patch)
    }

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
        problems = state$problems(), suggestions = suggestions(), matr_ids = matr_ids(),
        shown = shown(), focus = focus
      )
    }

    # ------ OUTPUT ------------------------------------------------------------
    output$add_block_menu <- renderUI(add_block_menu(lookups$component_types))

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
        opened(intersect(opened(), draft$components$id))
        patch <- outline_patch(drawn(), draft, current_view(), lookups, ns)
        if (is.null(patch)) {
          redraw(redraw() + 1L)
          return()
        }
        send_patch(patch)
        drawn(draft)
      })
    })

    output$tree <- renderUI({
      redraw()
      isolate({
        shown(character()) # fields added and left empty go when the whole outline is drawn again
        draft <- state$draft()
        drawn(draft)
        outline_tree(draft, current_view(), lookups, ns)
      })
    })
  })
}
