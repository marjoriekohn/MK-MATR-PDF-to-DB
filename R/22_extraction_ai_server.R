# The AI-Powered mode of the Extraction page: the server.
# Choose a questionnaire PDF (and the optional file of Variable Names and Coding), click Begin Extraction, and
# watch it being read, in the background. The draft starts then, with both files, which can't be changed
# after: to read other files, cancel the extraction and begin again. Once reading has finished
# the draft opens on the Edit Extraction page. Where it goes in the database (its Basic Info) is filled in
# there, when the extraction is pushed. Drafts are kept in the database but nothing here adds to the MATR
# metadata database itself.
# switch_mode: A function taking a mode of the Extraction page (`"manual"` or `"ai"`), or `"drafts"`
#   for the Extraction Drafts page, and showing it.
# Returns: A list with `open`, a function that shows a draft opened from Extraction Drafts, or an
#   empty place to choose the PDF for `NULL`.
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
    current_id <- reactiveVal(NULL) # the draft on this page, once the extraction has begun
    pdf_upload <- reactiveVal(NULL) # the PDF chosen, until the extraction begins
    supplement_upload <- reactiveVal(NULL) # the file of MATR IDs and codes chosen, until the extraction begins

    record <- reactive({
      if (!is.null(current_id())) extractions$all()[[current_id()]]
    })

    # What the page shows of the draft, kept apart so the draft's other changes don't redraw it.
    reading <- reactiveVal(NULL)
    observe(reading(record()[c("method", "pdf", "extraction_status", "error", "reading_started", "supplement")]))

    # Shows a draft opened from Extraction Drafts, or with none, an empty place to choose the PDF.
    show_draft <- function(opened = NULL) {
      current_id(opened$id)
      pdf_upload(NULL)
      supplement_upload(NULL)
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

    # ------ The files, then Begin Extraction -----------------------------------
    # Both are held here until the extraction begins; then the draft starts with them, and reading with it.
    observeEvent(input$pdf, pdf_upload(input$pdf))
    observeEvent(input$replace_pdf, pdf_upload(input$replace_pdf))
    observeEvent(input$supplement, supplement_upload(input$supplement))
    observeEvent(input$remove_supplement, supplement_upload(NULL))

    observeEvent(input$begin, {
      req(is.null(current_id()), pdf_upload())
      try_to({
        created <- extraction_service_create(connection, pdf_upload(), me, supplement = supplement_upload())
        show_draft(created)
      })
    })

    observeEvent(input$retry, try_to(extraction_service_retry(connection, current_id(), me)))

    observeEvent(input$cancel, {
      showModal(confirm_dialog(
        "Cancel this extraction?",
        "The draft is deleted, with its PDF. Nothing has been added to the MATR metadata database.",
        confirm_id = ns("confirm_cancel"), confirm_label = "Cancel extraction"
      ))
    })

    observeEvent(input$confirm_cancel, {
      removeModal()
      try_to(extraction_service_discard(connection, current_id(), me))
      show_draft(NULL)
      showNotification("Extraction cancelled.", type = "message")
    })

    observeEvent(input$to_continue, switch_mode("drafts"))

    # The reading's card stays until the draft is open there (not while someone else is working on it, say).
    observeEvent(input$open_extraction, {
      req(record())
      if (isTRUE(open_edit_extraction(record()))) show_draft(NULL)
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$pdf_card <- renderUI({
      if (!is.null(reading())) {
        return(pdf_file(reading()$pdf, ns))
      }
      chosen <- pdf_upload()
      if (is.null(chosen)) pdf_choice(ns) else pdf_file(list(name = chosen$name, bytes = chosen$size), ns, begun = FALSE)
    })

    output$supplement_box <- renderUI({
      begun <- !is.null(reading())
      name <- if (begun) reading()$supplement$name else supplement_upload()$name
      supplement_card(name, begun, ns)
    })

    # Begin Extraction until it has begun, once a PDF is chosen; then Open in Edit Extraction, once reading has finished.
    output$footer_button <- renderUI({
      if (is.null(reading())) {
        return(actionButton(ns("begin"), "Begin Extraction", class = "btn-primary", disabled = is.null(pdf_upload())))
      }
      actionButton(ns("open_extraction"), "Open in Edit Extraction",
        class = "btn-primary", disabled = !extraction_service_is_read(reading())
      )
    })

    output$reading <- renderUI({
      req(reading())
      reading_card(reading(), ns)
    })

    # Where reading has got to, and what comes next.
    output$footer_note <- renderUI({
      status <- reading()$extraction_status
      if (is.null(status)) {
        return(if (is.null(pdf_upload())) "Choose the questionnaire PDF to begin." else "The files can't be changed once the extraction begins.")
      }
      if (status == "failed") {
        return("Reading stopped. Try reading again, or cancel the extraction and begin again with another PDF.")
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
