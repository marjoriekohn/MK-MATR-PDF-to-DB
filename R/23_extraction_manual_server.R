# The Manual mode of the Extraction page: the server.
# Staff optionally add the PDF, and start: the draft is made with an empty questionnaire and opens on
# the Edit Extraction page, with every field there to fill in. Where it goes in the database (its Basic
# Info) is filled in there, when the extraction is pushed. Nothing here adds to the MATR metadata database itself.
extraction_manual_server <- function(
  id,
  extractions,
  connection,
  me,
  open_edit_extraction
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    upload <- reactiveVal(NULL) # the PDF chosen, if any

    observeEvent(input$pdf, upload(input$pdf))
    observeEvent(input$remove_pdf, upload(NULL))

    observeEvent(input$start, {
      record <- try_showing(extraction_service_create_manual(connection, me, upload = upload()), "Couldn't start it", type = "warning")
      if (is.null(record)) {
        return()
      }
      extractions$changed()
      upload(NULL)
      open_edit_extraction(record)
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$pdf_card <- renderUI(manual_pdf_card(upload(), ns))
  })
}
