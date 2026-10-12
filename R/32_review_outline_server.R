#' The questionnaire outline of the review page.
#'
#' Blocks as a tree, each edited where it is: type a block's words into its row, open and close
#' sections, open a question's panel for its variables and the question it follows, mark a block as
#' checked, add and delete blocks, variables and choices, and drag any of them into a new order. Every
#' change goes through `state$change()`.
#'
#' The tree is drawn again only when the layout changes (a block or variable added, deleted, moved or
#' picked from a list), never as words are typed, so the field being typed in is left alone. What changes
#' as words are typed (which block is selected, and which have problems) is sent to the page script,
#' which marks the rows.
#'
#' @param id The module's id.
#' @param state What the review page shares: `list(draft, change, selected, problems, layout_version,
#'   suggestions)`; `suggestions` (a reactive of `draft_service_text_suggestions()`) may be left out.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @return Called for its effect.
review_outline_server <- function(
  id,
  state,
  lookups
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    collapsed <- reactiveVal(character()) # sections whose contents are hidden
    opened <- reactiveVal(character()) # questions whose panel is open
    suggestions <- state$suggestions %||% function() list()

    observeEvent(input$action, {
      event <- input$action
      draft <- state$draft()
      blocks <- draft$components
      switch(event$action,
        select = state$selected(event$id),
        toggle = collapsed(flip(collapsed(), event$id)),
        details = opened(flip(opened(), event$id)),
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
      marked <- state$change(draft_service_update(draft, "components", id, "checked", checked))
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

    # Open the sections around the selected block, so it can be seen.
    observeEvent(state$selected(), {
      collapsed(setdiff(collapsed(), draft_service_ancestors(state$draft(), state$selected())))
    })

    # What changes as words are typed: the selected row, and each block's count of problems.
    observe({
      problems <- state$problems()
      counts <- as.list(table(problems$id))
      messages <- lapply(split(problems$message, problems$id), paste, collapse = "\n")
      session$sendCustomMessage("review-marks", list(
        selected = state$selected(),
        problems = lapply(names(counts), function(id) list(id = id, n = counts[[id]], title = messages[[id]]))
      ))
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$add_block_menu <- renderUI(add_block_menu(lookups$component_types))

    output$to_fix <- renderUI(to_fix_note(state$problems()))

    output$tree <- renderUI({
      state$layout_version()
      collapsed()
      opened()
      isolate({
        view <- list(
          selected = state$selected(), collapsed = collapsed(), opened = opened(),
          problems = state$problems(), suggestions = suggestions()
        )
        outline_tree(state$draft(), view, lookups, ns)
      })
    })
  })
}
