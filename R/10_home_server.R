# The Home page's server.
# Counts, for the banner: this person's extraction drafts in progress and the changes added so far, read again
# each time the page is shown. The drafts are the ones the site shares, which `server.R` reads again as the page
# opens. The banner's buttons and the cards under "What you can do here" open the pages they name.
# extractions: The extraction drafts as `server.R` shares them: `all()` is a reactive list of records.
# active: A reactive that says whether this page is the one on screen, or `NULL`. Coming
#   back to the page reads its counts again, since the other pages change them.
home_server <- function(id, connection, me, show_page, extractions, active = NULL) {
  moduleServer(id, function(input, output, session) {
    # ------ REACTIVE ----------------------------------------------------------
    refreshed <- reactiveVal(0L) # ticks each time the page is shown again

    if (!is.null(active)) {
      observeEvent(active(), if (isTRUE(active())) refreshed(refreshed() + 1L), ignoreInit = TRUE)
    }

    work <- reactive(home_service_my_work(extractions$all(), me))
    # A read that fails leaves its count a dash, not the whole page.
    changes <- reactive({
      refreshed()
      tryCatch(home_service_changes_added(connection), error = function(problem) {
        showNotification(paste0("Couldn't read the Edit History: ", conditionMessage(problem)), type = "error", id = "database_problem")
        NULL
      })
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$stats <- renderUI(home_stats(work(), changes()))

    # ------ UI ----------------------------------------------------------------
    # The banner's buttons, then the cards under "What you can do here"
    observeEvent(input$new_extraction, show_page("extraction"))
    observeEvent(input$browse_metadata, show_page("make_edit"))
    observeEvent(input$go_extraction, show_page("extraction"))
    observeEvent(input$go_drafts, show_page("drafts"))
    observeEvent(input$go_metadata, show_page("make_edit"))
    observeEvent(input$go_history, show_page("edit_history"))
    observeEvent(input$go_monitor, show_page("ai_monitor"))
  })
}
