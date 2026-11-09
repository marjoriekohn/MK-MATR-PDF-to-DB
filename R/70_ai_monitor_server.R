# The AI Monitor page's server.
# The pushed AI-Powered extractions' corrections (service_monitor.R): the headline numbers, a chart of them over
# time, the prompts that read them, and the kinds of correction, the commonest first, narrowed to blocks,
# variables or choices.
# active: A reactive that says whether this page is the one on screen, or `NULL`. Opening the page reads the
#   extractions again, since one may have been pushed since.
ai_monitor_server <- function(
  id,
  connection,
  active = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    refreshed <- reactiveVal(0L) # ticks each time the page is opened

    # If the database can't be reached the last extractions read stay on screen, with a message.
    read_records <- last_good_read(function() monitor_service_get_all(connection))
    records <- reactive({
      refreshed()
      read_records()
    })
    corrections <- reactive(monitor_service_corrections(records()))

    # The kinds of correction to things of one sort (blocks, variables or choices), or all of them.
    common <- reactive({
      rows <- corrections()
      item <- input$item %||% "all"
      if (!identical(item, "all")) rows <- rows[rows$item == item, , drop = FALSE]
      monitor_service_common(rows, length(records()))
    })

    if (!is.null(active)) {
      observeEvent(active(), if (isTRUE(active())) refreshed(refreshed() + 1L), ignoreInit = TRUE)
    }

    # ------ OUTPUT ------------------------------------------------------------
    output$totals <- renderUI(monitor_totals_ui(monitor_service_totals(records())))
    output$chart <- renderUI(monitor_chart_ui(monitor_service_over_time(records())))
    output$by_prompt <- renderUI(monitor_by_prompt_ui(monitor_service_by_prompt(records())))
    output$common <- renderUI(monitor_common_ui(common()))
  })
}
