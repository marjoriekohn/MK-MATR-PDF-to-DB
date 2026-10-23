#' The Extraction Drafts page's server.
#'
#' Every extraction draft still being prepared, newest first. Continue opens a draft that
#' has been read (or typed by hand) on the Edit Extraction page, and one still being read on New Extraction, in
#' AI-Powered mode.
#' While someone else is working on a draft its card says so, and offers Take over instead (asking first).
#'
#' @param id The module's id.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param catalog The live tables the pages pick from, as `server.R` shares them: `get()` reads them
#'   (`questionnaire_service_get_catalog()`) and `refresh()` reads them again.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_edit_extraction A function taking a record (and `take_over`, whether to take it from
#'   whoever is working on it) and opening it on the Edit Extraction page.
#' @param open_details A function taking a record and opening it on New Extraction, in AI-Powered mode.
#' @param switch_mode A function taking a mode of New Extraction (`"manual"` or `"ai"`) and showing it.
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
    taking <- reactiveVal(NULL) # the draft the Take over dialog is asking about

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
        discard = confirm_discard(record),
        take_over = confirm_take_over(record)
      )
    })

    confirm_take_over <- function(record) {
      editor <- draft_row_service_editor(record, me)
      if (is.null(editor)) { # their claim lapsed since the card was drawn
        return(open_edit_extraction(record))
      }
      taking(record$id)
      showModal(confirm_dialog(
        sprintf("Take this draft over from %s?", editor$by),
        p(strong(extraction_service_title(record, catalog$get()))),
        p(sprintf(
          "%s was active on it %s. Only one person at a time can work on a draft, so nobody's changes are overwritten.",
          editor$by, time_ago(parse_time(editor$at))
        )),
        p(sprintf("Once you have it, %s can still see it but not change it, and anything they haven't saved can't be saved.", editor$by)),
        confirm_id = ns("confirm_take_over"), confirm_label = "Take over", keep_label = "Cancel"
      ))
    }

    observeEvent(input$confirm_take_over, {
      removeModal()
      record <- extractions$all()[[taking() %||% ""]]
      req(record)
      open_edit_extraction(record, take_over = TRUE)
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

    # A card says someone is working on a draft only until their claim lapses, so the cards are drawn
    # again then (what they read isn't: that is only when the drafts are read again).
    output$drafts <- renderUI({
      if (length(shown()) == 0) {
        return(no_drafts(whose()))
      }
      now <- Sys.time()
      lapses <- unlist(lapply(shown(), function(record) {
        editor <- draft_row_service_editor(record, me, now)
        if (!is.null(editor)) draft_claim_stale_seconds - as.numeric(difftime(now, parse_time(editor$at), units = "secs"))
      }))
      if (length(lapses) > 0) invalidateLater(ceiling(max(1, min(lapses))) * 1000)
      lapply(shown(), function(record) {
        draft <- if (extraction_service_is_read(record)) load_draft(record)
        draft_card(record, draft, catalog$get(), me, now)
      })
    })
  })
}
