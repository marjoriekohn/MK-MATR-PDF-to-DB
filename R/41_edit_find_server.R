# The cards are narrowed once typing pauses for this long, not on every key.
query_delay_ms <- 300

#' The cards of the rows staff can change or delete.
#'
#' One card for each row of the kind staff chose, with its counts, to pick from. Cards are
#' found by typing any of the words on them. Long lists show a page at a time.
#'
#' @param id The module's id.
#' @param entity A reactive: the kind of thing; see `edit_entities`.
#' @param verb A reactive: `"update"` or `"delete"`.
#' @param snapshot A reactive: the tables the kind needs.
#' @param scope A reactive: the questionnaire's key, for a kind that belongs to one.
#' @param picked A reactiveVal for the key of the row picked: this sets it, and clears it
#'   when staff pick again.
#' @return Called for its effect.
edit_find_server <- function(
  id,
  entity,
  verb,
  snapshot,
  scope,
  picked
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    page_size <- 24

    # ------ REACTIVE ----------------------------------------------------------
    shown <- reactiveVal(page_size) # how many cards to show

    # Built once for the kind and questionnaire, and filtered as staff type.
    all_cards <- reactive({
      req(entity(), snapshot(), verb() %in% c("update", "delete"))
      edit_service_cards(entity(), snapshot(), scope())
    })
    query <- debounce(reactive(input$query %||% ""), query_delay_ms)
    cards <- reactive(edit_service_filter(all_cards(), query()))

    observeEvent(list(entity(), verb(), scope(), query()), shown(page_size), ignoreInit = TRUE)

    observeEvent(input$action, {
      event <- input$action
      switch(event$action,
        pick = picked(event$id),
        unpick = picked(NULL),
        more = shown(shown() + page_size)
      )
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$search <- renderUI({
      req(entity(), snapshot(), verb() %in% c("update", "delete"), is.null(picked()), nrow(all_cards()) > 0)
      div(
        class = "edit-find-header",
        div(
          h2(paste0("Which ", entity()$noun, " do you want to ", verb(), "?")),
          p(class = "form-text", "Pick one. Type any words on a card to narrow the list.")
        ),
        textInput(
          ns("query"), NULL,
          value = isolate(input$query) %||% "", width = "24rem",
          placeholder = "Search, e.g. a name or a questionnaire"
        )
      )
    })

    output$cards <- renderUI({
      req(entity(), snapshot(), verb() %in% c("update", "delete"))
      if (!is.null(picked())) {
        card <- all_cards()[all_cards()$key == picked(), , drop = FALSE]
        req(nrow(card) == 1)
        return(div(
          class = "edit-picked",
          edit_card_ui(card, verb(), selected = TRUE),
          div(class = "edit-picked-change", review_button("Pick a different one", "unpick", class = "btn btn-link"))
        ))
      }
      if (nrow(all_cards()) == 0) {
        return(div(class = "edit-empty", edit_find_nothing(entity(), scope())))
      }
      if (nrow(cards()) == 0) {
        return(div(class = "edit-empty", paste0("Nothing matches “", trimws(query()), "”. Try fewer or different words.")))
      }
      top <- utils::head(cards(), shown())
      tagList(
        p(
          class = "edit-find-count",
          if (nrow(cards()) > nrow(top)) {
            sprintf("Showing %d of %s", nrow(top), format(nrow(cards()), big.mark = ","))
          } else {
            edit_count(nrow(cards()), entity()$noun, entity()$plural)
          }
        ),
        div(class = "edit-cards", lapply(seq_len(nrow(top)), function(i) edit_card_ui(top[i, , drop = FALSE], verb()))),
        if (nrow(cards()) > nrow(top)) {
          div(class = "edit-more", review_button(
            sprintf("Show %d more", min(page_size, nrow(cards()) - nrow(top))), "more",
            class = "btn btn-outline-dark"
          ))
        }
      )
    })
  })
}

# What to say when there's nothing to pick from.
edit_find_nothing <- function(entity, scope) {
  if (!is.null(entity$scope) && !nzchar(scope %||% "")) {
    return(paste0("Choose a questionnaire above to see its ", entity$plural, "."))
  }
  if (!is.null(entity$scope)) {
    return(paste0("This questionnaire has no ", entity$plural, " yet."))
  }
  paste0("There are no ", entity$plural, " in the database yet. Choose “add” above to make the first.")
}
