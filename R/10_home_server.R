#' The Home page's server.
#'
#' Reads this person's saved edits, the requests waiting for them and the newest changes when the page is
#' shown; their extraction drafts are the ones the site shares, which `server.R` reads again as the page opens.
#' Continue opens a draft where it is worked on, Review opens a request in its dialog, and the buttons under
#' the lists open the pages that list them all.
#'
#' @param id The module's id.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param show_page A function taking a page's name and showing it.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list of records.
#' @param catalog The live tables, as `server.R` shares them: `get()` reads them. An extraction is named by its
#'   questionnaire and wave.
#' @param open_extraction A function taking an extraction record and opening it: on Edit Extraction, or on New
#'   Extraction while it is being read.
#' @param open_edit A function taking an edit record and opening it on the Metadata page.
#' @param review_request A function taking a change request's id and opening it on Change Requests.
#' @param active A reactive that says whether this page is the one on screen, or `NULL`. Coming
#'   back to the page reads its lists again, since the other pages change them.
#' @return Called for its effect.
home_server <- function(
  id,
  connection,
  me,
  show_page,
  extractions,
  catalog,
  open_extraction,
  open_edit,
  review_request,
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

    edits <- reactive({
      refreshed()
      read("saved edits", edit_service_get_all)
    })
    work <- reactive(home_service_my_work(extractions$all(), edits() %||% list(), me))
    requests <- reactive({
      refreshed()
      read("change requests", function(connection) home_service_waiting_for_me(connection, me))
    })
    history <- reactive({
      refreshed()
      read("Edit History", home_service_recent_history)
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$work <- renderUI({
      tables <- catalog$get()
      home_item_list(
        lapply(work(), home_work_item, catalog = tables),
        "Nothing in progress. Start with New Extraction, or change what's in the database on the Metadata page."
      )
    })

    output$requests <- renderUI({
      waiting <- requests()
      if (is.null(waiting)) {
        return(p(class = "home-empty", "The change requests couldn't be read."))
      }
      home_item_list(lapply(waiting$shown, home_request_item, me = me), "Nothing is waiting for you.")
    })

    output$history <- renderUI({
      recent <- history()
      if (is.null(recent)) {
        return(p(class = "home-empty", "The Edit History couldn't be read."))
      }
      home_item_list(lapply(recent$shown, home_history_item), "Nothing has been added to the MATR metadata database through this site yet.")
    })

    output$work_more <- renderUI({
      actionButton(ns("extraction_drafts"), "Everyone's extraction drafts", class = "btn-sm btn-outline-dark")
    })

    output$change_requests_more <- renderUI({
      waiting <- requests()
      actionButton(
        ns("change_requests"), home_service_more_label(waiting$total %||% 0L, length(waiting$shown)),
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
    # Continue and Review (the page script sends them), then the buttons that open a page.
    observeEvent(input$action, {
      event <- input$action
      id <- as.character(event$id %||% "")
      if (identical(event$action, "review")) {
        review_request(id)
        return()
      }
      if (!identical(event$action, "continue")) {
        return()
      }
      if (identical(event$kind, "extraction")) {
        record <- extractions$all()[[id]]
        if (!is.null(record)) open_extraction(record)
      } else {
        record <- (edits() %||% list())[[id]]
        if (!is.null(record)) open_edit(record)
      }
    })

    observeEvent(input$new_extraction, show_page("extraction"))
    observeEvent(input$extraction_drafts, show_page("drafts"))
    observeEvent(input$edit_history, show_page("edit_history"))
    observeEvent(input$change_requests, show_page("change_requests"))
  })
}
