#' The Manual mode of the Extraction page: a PDF if staff have one, and a button that starts.
#'
#' No AI reads anything. Starting opens a blank Edit Extraction page, the same page an AI-Powered
#' extraction is edited on, where every block, question and choice is typed in, with the PDF beside it
#' if one was added.
#'
#' @param id The module's id.
#' @return The mode's UI.
extraction_manual_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "extraction-mode extraction-manual",
    `data-ns` = ns(""),
    card(card_body(
      h2("Questionnaire PDF (optional)"),
      uiOutput(ns("pdf_card")),
      div(
        class = "extraction-footer",
        span("You'll type the questionnaire in on the Edit Extraction page."),
        actionButton(ns("start"), "Start Manual Extraction", class = "btn-primary")
      )
    ))
  )
}

#' Somewhere to drop a PDF for a Manual extraction, or the one that was chosen.
#'
#' @param upload One row of a `fileInput`'s value, or `NULL`.
#' @param ns The module's namespace function.
#' @return A UI element.
manual_pdf_card <- function(upload, ns) {
  if (is.null(upload)) {
    return(drop_zone(
      fileInput(ns("pdf"), NULL, accept = c(".pdf", "application/pdf"), buttonLabel = "Choose PDF"),
      strong("Drop the questionnaire PDF here, if you have one"),
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
