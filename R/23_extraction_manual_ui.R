# The Manual mode of the Extraction page: a PDF if staff have one, and a button that starts.
# No AI reads anything. Starting opens a blank Edit Extraction page, the same page an AI-Powered
# extraction is edited on, where every block, question and choice is typed in, with the PDF beside it
# if one was added.
# Returns: The mode's UI.
extraction_manual_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "extraction-mode extraction-manual",
    `data-ns` = ns(""),
    card(
      card_header(h2("Questionnaire PDF (optional)")),
      card_body(uiOutput(ns("pdf_card"))),
      card_footer(
        class = "extraction-footer",
        span("You'll type the questionnaire in on the Edit Extraction page."),
        actionButton(ns("start"), "Start Manual Extraction", class = "btn-primary")
      )
    )
  )
}

# Where a PDF for a Manual extraction is chosen, or the one that was.
# upload: One row of a `fileInput`'s value, or `NULL`.
manual_pdf_card <- function(upload, ns) {
  if (is.null(upload)) {
    return(file_box(
      fileInput(ns("pdf"), NULL, accept = c(".pdf", "application/pdf"), buttonLabel = "Choose PDF"),
      strong("Questionnaire PDF, if you have one"),
      div("It stays with the draft so you can look at it while you type.")
    ))
  }
  div(
    class = "pdf-file",
    span(class = "pdf-file-icon", "PDF"),
    div(class = "pdf-file-name", strong(upload$name), div(class = "form-text", file_size(upload$size))),
    actionButton(ns("remove_pdf"), "Remove", class = "btn-link")
  )
}
