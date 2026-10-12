#' The Manual mode of the Extraction page: the server.
#'
#' Staff say which questionnaire and wave it is, optionally add the PDF, and start: the draft is
#' made with an empty questionnaire and opens on the review page. Nothing here adds to the MATR
#' data itself.
#'
#' @param id The module's id.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param catalog The live tables the pages pick from, as `server.R` shares them: `get()` reads them
#'   (`questionnaire_service_get_catalog()`) and `refresh()` reads them again.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_review A function taking a record and opening it on the review page.
#' @return Called for its effect.
extraction_manual_server <- function(
  id,
  extractions,
  catalog,
  connection,
  me,
  open_review
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    form <- extraction_info_server("info", catalog, extractions, reactive(NULL), me, connection)
    form$show()
    upload <- reactiveVal(NULL) # the PDF chosen, if any

    details_done <- reactive(length(form$missing()) == 0 && length(form$problems()) == 0)

    observeEvent(input$pdf, upload(input$pdf))
    observeEvent(input$remove_pdf, upload(NULL))

    observe(updateActionButton(session, "start", disabled = !details_done()))

    observeEvent(input$start, {
      req(details_done())
      record <- tryCatch(
        extraction_service_create_manual(connection, me, form$info(), upload()),
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
      form$show()
      open_review(record)
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$pdf_card <- renderUI(manual_pdf_card(upload(), ns))

    # What's still needed before it can start, e.g. "Fill in the study and wave name to start."
    output$footer_note <- renderUI({
      if (length(form$missing()) > 0) {
        return(paste0("Fill in the ", word_list(form$missing()), " to start."))
      }
      if (length(form$problems()) > 0) {
        return("Fix the fields outlined in red to start.")
      }
      "You'll type the questionnaire in on the review page."
    })
  })
}
