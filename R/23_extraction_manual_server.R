#' The Manual mode of the Extraction page: the server.
#'
#' Staff optionally add the PDF, and start: the draft is made with an empty questionnaire and opens on
#' the Edit Extraction page, with every field there to fill in. Where it goes in the database is picked
#' later, when its change request is submitted. Nothing here adds to the MATR metadata database itself.
#'
#' @param id The module's id.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_edit_extraction A function taking a record and opening it on the Edit Extraction page.
#' @return Called for its effect.
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
      record <- tryCatch(
        extraction_service_create_manual(connection, me, upload = upload()),
        error = function(problem) {
          show_problem(problem, "Couldn't start it", type = "warning")
          NULL
        }
      )
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
