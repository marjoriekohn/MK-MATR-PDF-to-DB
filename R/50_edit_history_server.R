# The Edit History page's server.
# The changes made to the MATR metadata database, newest first, filtered by kind and by words typed.
# "See what changed" opens what a change did: the fields it changed, old and new, or the counts of what a push
# or an emptying did.
# active: A reactive that says whether this page is the one on screen, or `NULL`. Opening
#   the page reads the history again, since a change goes in at once and the list was read before it.
edit_history_server <- function(
  id,
  connection,
  active = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    refreshed <- reactiveVal(0L) # ticks each time the page is opened
    showing <- reactiveVal(history_page_size) # how many changes are listed

    # If the database can't be reached the last changes read stay on screen, with a message.
    read_records <- last_good_read(function() history_service_get_all(connection))
    records <- reactive({
      refreshed()
      read_records()
    })

    matching <- reactive(history_service_filter(records(), input$kind %||% "all", input$search %||% ""))

    if (!is.null(active)) {
      observeEvent(active(), if (isTRUE(active())) refreshed(refreshed() + 1L), ignoreInit = TRUE)
    }
    observeEvent(list(input$kind, input$search), showing(history_page_size), ignoreInit = TRUE)
    observeEvent(input$action, {
      event <- input$action
      switch(event$action,
        more = showing(showing() + history_page_size),
        open = {
          record <- records()[[event$id %||% ""]]
          if (!is.null(record)) show_changes(record)
        }
      )
    })

    show_changes <- function(record) {
      changes <- try_showing(history_service_changes(connection, record$id), "Couldn't read what changed")
      if (is.null(changes)) {
        return()
      }
      showModal(modalDialog(
        title = record$summary,
        size = "l",
        easyClose = TRUE,
        p(class = "list-card-meta", paste(history_service_when(record), history_service_who(record), sep = " · ")),
        if (is.null(changes$groups)) {
          history_counts_ui(changes$lines)
        } else {
          tagList(p(class = "form-text", changes$text), change_items_ui(changes$groups))
        },
        footer = modalButton("Close")
      ))
    }

    # ------ OUTPUT ------------------------------------------------------------
    output$entries <- renderUI({
      found <- matching()
      if (length(found) == 0) {
        return(no_history(filtered = length(records()) > 0))
      }
      tagList(
        lapply(utils::head(found, showing()), history_entry_ui),
        if (length(found) > showing()) {
          div(
            class = "list-more",
            review_button(
              sprintf("Show more (%d more)", length(found) - showing()), "more",
              class = "btn btn-outline-dark"
            )
          )
        }
      )
    })
  })
}
