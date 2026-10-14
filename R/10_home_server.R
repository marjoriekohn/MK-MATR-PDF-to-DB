#' The Home page's server.
#'
#' Reads the newest requests and changes when the page is shown, and each button opens the page
#' it names.
#'
#' @param id The module's id.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param show_page A function taking a page's name and showing it.
#' @param active A reactive that says whether this page is the one on screen, or `NULL`. Coming
#'   back to the page reads its lists again, since the other pages change them.
#' @return Called for its effect.
home_server <- function(
  id,
  connection,
  me,
  show_page,
  active = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    refreshed <- reactiveVal(0L) # ticks each time the page is shown again

    if (!is.null(active)) {
      observeEvent(active(), if (isTRUE(active())) refreshed(refreshed() + 1L), ignoreInit = TRUE)
    }

    # A read that fails leaves the box saying so, not the whole page.
    read <- function(what, fn) {
      tryCatch(fn(connection), error = function(problem) {
        showNotification(paste0("Couldn't read the ", what, ": ", conditionMessage(problem)), type = "error", id = "database_problem")
        NULL
      })
    }

    requests <- reactive({
      refreshed()
      read("change requests", home_service_recent_requests)
    })
    history <- reactive({
      refreshed()
      read("Edit history", home_service_recent_history)
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$requests <- renderUI({
      recent <- requests()
      if (is.null(recent)) {
        return(p(class = "home-empty", "The change requests couldn't be read."))
      }
      home_item_list(lapply(recent$shown, home_request_item, me = me), "Nothing is waiting for approval.")
    })

    output$history <- renderUI({
      recent <- history()
      if (is.null(recent)) {
        return(p(class = "home-empty", "The Edit history couldn't be read."))
      }
      home_item_list(lapply(recent$shown, home_history_item), "Nothing has been added to the MATR metadata database through this site yet.")
    })

    output$change_requests_more <- renderUI({
      recent <- requests()
      actionButton(
        ns("change_requests"), home_service_more_label(recent$total %||% 0L, length(recent$shown)),
        class = "btn-sm btn-outline-dark"
      )
    })

    output$edit_history_more <- renderUI({
      recent <- history()
      actionButton(
        ns("edit_history"), home_service_more_label(recent$total %||% 0L, length(recent$shown)),
        class = "btn-sm btn-outline-dark"
      )
    })

    # ------ UI ----------------------------------------------------------------
    # Each button opens the page it names.
    observeEvent(input$extraction, show_page("extraction"))
    observeEvent(input$make_edit, show_page("make_edit"))
    observeEvent(input$edit_history, show_page("edit_history"))
    observeEvent(input$change_requests, show_page("change_requests"))
  })
}
