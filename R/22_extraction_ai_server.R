# Typing is saved once it pauses for this long, so a draft isn't saved on every key.
info_save_delay_ms <- 1000
# How often "Draft saved 2 minutes ago" is brought up to date, and the reading timer.
saved_ago_refresh_ms <- 10000
reading_timer_refresh_ms <- 1000

#' The AI-Powered mode of the Extraction page: the server.
#'
#' Upload a questionnaire PDF, watch it being read, and meanwhile fill in what is known
#' about it. Typing is saved to the draft once it pauses; before a PDF is uploaded there is
#' no draft yet, so what is typed waits and is saved with the upload. Drafts are kept in
#' the database but nothing here adds to the MATR data itself.
#'
#' @param id The module's id.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param catalog The live tables the pages pick from, as `server.R` shares them: `get()` reads them
#'   (`questionnaire_service_get_catalog()`) and `refresh()` reads them again.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_review A function taking a record and opening it on the review page.
#' @param switch_mode A function taking a mode of the Extraction page (`"manual"`, `"ai"` or
#'   `"drafts"`) and showing it.
#' @return A list with `open`, a function that shows a draft opened from Existing Draft, or an
#'   empty form for `NULL`.
extraction_ai_server <- function(
  id,
  extractions,
  catalog,
  connection,
  me,
  open_review,
  switch_mode
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    current_id <- reactiveVal(NULL) # the draft on this page, once a PDF is uploaded

    record <- reactive({
      if (!is.null(current_id())) extractions$all()[[current_id()]]
    })

    # What the left column shows, kept apart so typing doesn't redraw it.
    reading <- reactiveVal(NULL)
    observe(reading(record()[c("pdf", "extraction_status", "error", "reading_started")]))

    # What staff know about the questionnaire and wave.
    form <- extraction_info_server("info", catalog, extractions, current_id, me, connection)
    form$show()

    info_paused <- debounce(form$info, info_save_delay_ms)

    # Only typing triggers a save, so a draft opened from Existing Draft is
    # never saved with what the form held before it.
    observeEvent(info_paused(), {
      req(record())
      if (!identical(info_paused(), extraction_service_info(record()))) save_info(info_paused())
    })

    save_info <- function(info) {
      extraction_service_save_info(connection, current_id(), info, me)
      extractions$changed()
    }

    # Shows a draft opened from Existing Draft, or with none, an empty form.
    show_draft <- function(opened = NULL) {
      current_id(opened$id)
      form$show(opened)
    }

    # Service functions explain themselves: their stop() texts are shown to staff.
    try_to <- function(expr) {
      tryCatch(
        {
          expr
          extractions$changed()
        },
        error = function(problem) show_problem(problem, type = "warning")
      )
    }

    # ------ The PDF -----------------------------------------------------------
    observeEvent(input$pdf, try_to({
      created <- extraction_service_create(connection, input$pdf, me, form$info())
      current_id(created$id)
    }))

    observeEvent(input$replace_pdf, try_to(
      extraction_service_replace_pdf(connection, current_id(), input$replace_pdf, me)
    ))

    observeEvent(input$retry, try_to(extraction_service_retry(connection, current_id(), me)))

    observeEvent(input$cancel, {
      showModal(confirm_dialog(
        "Cancel this extraction?",
        "The draft is set aside and won't appear under Existing Draft. Nothing has been added to the MATR data.",
        confirm_id = ns("confirm_cancel"), confirm_label = "Cancel extraction"
      ))
    })

    observeEvent(input$confirm_cancel, {
      removeModal()
      try_to(extraction_service_withdraw(connection, current_id(), me))
      show_draft(NULL)
      showNotification("Extraction cancelled.", type = "message")
    })

    observeEvent(input$to_continue, switch_mode("drafts"))

    observeEvent(input$go_to_review, {
      req(record())
      open_review(record())
      show_draft(NULL)
    })

    # Review opens once reading has finished, the required fields are filled in
    # and nothing needs fixing.
    details_done <- reactive(length(form$missing()) == 0 && length(form$problems()) == 0)

    observe({
      ready <- extraction_service_is_read(reading()) && details_done()
      updateActionButton(session, "go_to_review", disabled = !ready)
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$save_status <- renderUI({
      if (is.null(record())) {
        return(span(class = "save-status", "Saved once the PDF is uploaded"))
      }
      if (!identical(form$info(), extraction_service_info(record()))) {
        return(span(class = "save-status", "Saving…"))
      }
      invalidateLater(saved_ago_refresh_ms)
      span(
        class = "save-status", icon("check"),
        paste(" Draft saved", time_ago(parse_time(record()$saved)))
      )
    })

    output$pdf_card <- renderUI({
      if (is.null(reading())) pdf_drop_zone(ns) else pdf_file(reading(), ns)
    })

    output$reading <- renderUI({
      req(reading())
      reading_card(reading(), ns)
    })

    # How long reading has taken so far.
    output$elapsed <- renderText({
      req(reading(), !reading()$extraction_status %in% c("ready", "failed"))
      invalidateLater(reading_timer_refresh_ms)
      duration_text(difftime(Sys.time(), parse_time(reading()$reading_started), units = "secs"))
    })

    # What's still needed before review, e.g. "Fill in the study and wave name
    # to go to review."
    output$footer_note <- renderUI({
      status <- reading()$extraction_status
      if (is.null(status)) {
        return("Upload the PDF to start the draft.")
      }
      if (status == "failed") {
        return("Reading stopped. Try reading again, or replace the PDF.")
      }
      if (length(form$missing()) > 0) {
        return(paste0("Fill in the ", word_list(form$missing()), " to go to review."))
      }
      if (length(form$problems()) > 0) {
        return("Fix the fields outlined in red to go to review.")
      }
      if (status == "ready") {
        return("Reading has finished. The draft is ready to review.")
      }
      "When reading finishes you can go straight to review."
    })

    # ------ RETURN ------------------------------------------------------------
    list(open = show_draft)
  })
}
