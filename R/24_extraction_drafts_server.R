#' The Existing Draft mode of the Extraction page: the server.
#'
#' Every extraction draft still being prepared, newest first. Continue opens a draft that
#' has been read (or typed by hand) on the Edit Extraction page, and one still being read in AI-Powered mode.
#'
#' @param id The module's id.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param catalog The live tables the pages pick from, as `server.R` shares them: `get()` reads them
#'   (`questionnaire_service_get_catalog()`) and `refresh()` reads them again.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_edit_extraction A function taking a record and opening it on the Edit Extraction page.
#' @param open_details A function taking a record and opening it in AI-Powered mode.
#' @param switch_mode A function taking a mode of the Extraction page and showing it.
#' @return Called for its effect.
extraction_drafts_server <- function(
  id,
  extractions,
  catalog,
  connection,
  me,
  open_edit_extraction,
  open_details,
  switch_mode
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    whose <- reactiveVal("everyone") # "mine" or "everyone"
    discarding <- reactiveVal(NULL) # the draft the Discard dialog is asking about

    open_drafts <- reactive(extraction_service_open_drafts(extractions$all()))
    shown <- reactive({
      if (whose() == "mine") extraction_service_open_drafts(open_drafts(), prepared_by_id = me$staff_id) else open_drafts()
    })

    observeEvent(input$whose, whose(input$whose))

    observeEvent(input$action, {
      event <- input$action
      record <- extractions$all()[[event$id %||% ""]]
      switch(event$action,
        new = switch_mode("ai"),
        continue = if (extraction_service_is_read(record)) open_edit_extraction(record) else open_details(record),
        retry = {
          extraction_service_retry(connection, record$id, me)
          extractions$changed()
        },
        discard = confirm_discard(record)
      )
    })

    confirm_discard <- function(record) {
      discarding(record$id)
      showModal(confirm_dialog(
        "Discard this draft?",
        p(strong(extraction_service_title(record, catalog$get()))),
        p("It's set aside and won't appear here again. Nothing has been added to the MATR metadata database."),
        confirm_id = ns("confirm_discard"), confirm_label = "Discard"
      ))
    }

    observeEvent(input$confirm_discard, {
      removeModal()
      extraction_service_withdraw(connection, discarding(), me)
      extractions$changed()
      showNotification("Draft discarded.", type = "message")
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$whose_toggle <- renderUI({
      n_mine <- length(extraction_service_open_drafts(open_drafts(), prepared_by_id = me$staff_id))
      whose_buttons(ns("whose"), whose(), n_mine, length(open_drafts()))
    })

    # Each card's progress needs its questionnaire, which is big. It's fetched
    # once per save and kept, so a refresh because another draft changed doesn't
    # download them all again.
    loaded_drafts <- new.env()
    load_draft <- function(record) {
      key <- paste(record$id, record$saved)
      draft <- loaded_drafts[[key]]
      if (is.null(draft)) {
        draft <- tryCatch(extraction_service_load_draft(connection, record), error = function(problem) NULL)
        if (!is.null(draft)) { # a failure isn't kept, so the next refresh tries again
          rm(list = grep(paste0("^", record$id, " "), ls(loaded_drafts), value = TRUE), envir = loaded_drafts)
          loaded_drafts[[key]] <- draft
        }
      }
      draft
    }

    output$drafts <- renderUI({
      if (length(shown()) == 0) {
        return(no_drafts(whose()))
      }
      lapply(shown(), function(record) {
        draft <- if (extraction_service_is_read(record)) load_draft(record)
        draft_card(record, draft, catalog$get(), me)
      })
    })
  })
}
