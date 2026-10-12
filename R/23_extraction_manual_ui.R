#' The Manual mode of the Extraction page: what staff know about the questionnaire, and a PDF if they have one.
#'
#' No AI reads anything. Staff type the questionnaire's blocks, questions and choices on the review
#' page, which is the same page an AI-Powered extraction is reviewed on.
#'
#' @param id The module's id.
#' @return The mode's UI.
extraction_manual_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "extraction-mode extraction-manual",
    `data-ns` = ns(""),
    mode_intro(
      paste0(
        "Type the questionnaire in yourself, with no AI. You build its blocks, questions and choices on the review page, ",
        "which shows the PDF beside them if you add one."
      )
    ),
    card(card_body(h2("Questionnaire PDF (optional)"), uiOutput(ns("pdf_card")))),
    card(card_body(
      div(class = "card-intro", h2("About Questionnaire & Wave")),
      extraction_info_ui(ns("info")),
      div(
        class = "extraction-footer",
        uiOutput(ns("footer_note"), inline = TRUE),
        actionButton(ns("start"), "Start typing it in", class = "btn-primary", disabled = TRUE)
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
