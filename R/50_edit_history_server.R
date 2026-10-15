#' The Edit history page's server.
#'
#' The edits applied to the MATR metadata database, newest first, filtered by kind and by words typed.
#' "See what changed" opens the fields an edit changed, old and new.
#'
#' @param id The module's id.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param active A reactive that says whether this page is the one on screen, or `NULL`. Opening
#'   the page reads the history again, since an add or an update goes in at once and the list
#'   was read before it.
#' @return Called for its effect.
edit_history_server <- function(
  id,
  connection,
  active = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    refreshed <- reactiveVal(0L) # ticks when staff ask for a fresh look
    showing <- reactiveVal(history_page_size) # how many edits are listed

    # If the database can't be reached the last edits read stay on screen, with a message.
    last_records <- list()
    records <- reactive({
      refreshed()
      tryCatch(
        {
          last_records <<- history_service_get_all(connection)
          last_records
        },
        error = function(problem) {
          showNotification(conditionMessage(problem), type = "error", id = "database_problem")
          last_records
        }
      )
    })

    matching <- reactive(history_service_filter(records(), input$kind %||% "all", input$search %||% ""))

    observeEvent(input$refresh, refreshed(refreshed() + 1L))
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
      changes <- tryCatch(
        history_service_changes(connection, record$id),
        error = function(problem) {
          show_problem(problem, "Couldn't read what changed")
          NULL
        }
      )
      if (is.null(changes)) {
        return()
      }
      showModal(modalDialog(
        title = record$summary,
        size = "l",
        easyClose = TRUE,
        p(class = "history-meta", paste(c(history_service_when(record), history_service_people(record)), collapse = " · ")),
        p(class = "form-text", change_service_counts_text(changes$counts)),
        change_items_ui(changes$groups),
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
            class = "history-more",
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
