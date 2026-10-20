#' The AI-Powered mode of the Extraction page: the server.
#'
#' Upload a questionnaire PDF and watch it being read, in the background. Once reading has finished
#' the draft opens on the Edit Extraction page. Where it goes in the database (its Basic Info) is filled in
#' later, on Change Requests, once its change request is submitted. Drafts are kept in the database but nothing here adds to the MATR
#' metadata database itself.
#'
#' @param id The module's id.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_edit_extraction A function taking a record and opening it on the Edit Extraction page.
#' @param switch_mode A function taking a mode of the Extraction page (`"manual"`, `"ai"` or
#'   `"drafts"`) and showing it.
#' @return A list with `open`, a function that shows a draft opened from Existing Draft, or an
#'   empty drop zone for `NULL`.
extraction_ai_server <- function(
  id,
  extractions,
  connection,
  me,
  open_edit_extraction,
  switch_mode
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    current_id <- reactiveVal(NULL) # the draft on this page, once a PDF is uploaded

    record <- reactive({
      if (!is.null(current_id())) extractions$all()[[current_id()]]
    })

    # What the page shows of the draft, kept apart so the draft's other changes don't redraw it.
    reading <- reactiveVal(NULL)
    observe(reading(record()[c("method", "pdf", "extraction_status", "error", "reading_started")]))

    # Shows a draft opened from Existing Draft, or with none, an empty drop zone.
    show_draft <- function(opened = NULL) {
      current_id(opened$id)
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
      created <- extraction_service_create(connection, input$pdf, me)
      current_id(created$id)
    }))

    observeEvent(input$replace_pdf, try_to(
      extraction_service_replace_pdf(connection, current_id(), input$replace_pdf, me)
    ))

    observeEvent(input$retry, try_to(extraction_service_retry(connection, current_id(), me)))

    observeEvent(input$cancel, {
      showModal(confirm_dialog(
        "Cancel this extraction?",
        "The draft is set aside and won't appear under Existing Draft. Nothing has been added to the MATR metadata database.",
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

    observeEvent(input$open_extraction, {
      req(record())
      open_edit_extraction(record())
      show_draft(NULL)
    })

    # The Edit Extraction page opens once reading has finished.
    observe({
      updateActionButton(session, "open_extraction", disabled = !extraction_service_is_read(reading()))
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$pdf_card <- renderUI({
      if (is.null(reading())) pdf_drop_zone(ns) else pdf_file(reading(), ns)
    })

    output$reading <- renderUI({
      req(reading())
      reading_card(reading(), ns)
    })

    # Where reading has got to, and what comes next.
    output$footer_note <- renderUI({
      status <- reading()$extraction_status
      if (is.null(status)) {
        return("Upload the PDF to start the draft.")
      }
      if (status == "failed") {
        return("Reading stopped. Try reading again, or replace the PDF.")
      }
      if (status == "ready") {
        return("Reading has finished. Open the whole extraction to review it, and edit it if you need to.")
      }
      "When reading finishes you can open the whole extraction to review and edit it."
    })

    # ------ RETURN ------------------------------------------------------------
    list(open = show_draft)
  })
}
