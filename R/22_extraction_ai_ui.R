#' The AI-Powered mode of the Extraction page: the PDF and how reading it is going, then what staff know about it.
#'
#' @param id The module's id.
#' @return The mode's UI.
extraction_ai_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "extraction-mode extraction-ai",
    `data-ns` = ns(""),
    card(card_body(h2("Questionnaire PDF"), uiOutput(ns("pdf_card")))),
    uiOutput(ns("reading")),
    card(card_body(
      div(
        class = "card-intro",
        h2("About Questionnaire & Wave"),
      ),
      extraction_info_ui(ns("info")),
      div(
        class = "extraction-footer",
        uiOutput(ns("footer_note"), inline = TRUE),
        actionButton(ns("go_to_review"), "Go to review", class = "btn-primary", disabled = TRUE)
      )
    ))
  )
}

#' A dashed box files can be dropped on.
#'
#' `www/js/extraction.js` passes dropped files to the file input inside.
#'
#' @param file_input The file input.
#' @param ... What to say about it.
#' @param icon The icon's name.
#' @return A UI element.
drop_zone <- function(file_input, ..., icon = "file-pdf") {
  div(
    class = "drop-zone",
    span(class = "drop-zone-icon", shiny::icon(icon)),
    div(class = "drop-zone-text", ...),
    file_input
  )
}

# ------ Step 1: the PDF -------------------------------------------------------------
#' Somewhere to drop the PDF, before one is uploaded.
#'
#' @param ns The module's namespace function.
#' @return A UI element.
pdf_drop_zone <- function(ns) {
  drop_zone(
    fileInput(ns("pdf"), NULL, accept = c(".pdf", "application/pdf"), buttonLabel = "Choose PDF"),
    strong("Drop the questionnaire PDF here"),
    div("Reading starts as soon as it's uploaded.")
  )
}

#' The uploaded file, and a way to swap it while it is being read.
#'
#' @param record An extraction record; only `pdf` is used.
#' @param ns The module's namespace function.
#' @return A UI element.
pdf_file <- function(record, ns) {
  pdf <- record$pdf
  facts <- c(if (!is.null(pdf$pages)) plural(pdf$pages, "page"), file_size(pdf$bytes))
  div(
    class = "pdf-file",
    span(class = "pdf-file-icon", "PDF"),
    div(class = "pdf-file-name", strong(pdf$name), div(class = "form-text", paste(facts, collapse = " · "))),
    if (!extraction_service_is_read(record)) {
      fileInput(ns("replace_pdf"), NULL, accept = c(".pdf", "application/pdf"), buttonLabel = "Replace") |>
        tagAppendAttributes(class = "file-link")
    }
  )
}

# ------ Reading progress --------------------------------------------------------------
#' How reading the PDF is going.
#'
#' @param record An extraction record.
#' @param ns The module's namespace function.
#' @return A UI element.
reading_card <- function(record, ns) {
  steps <- extraction_service_reading_steps(record)
  finished <- extraction_service_is_read(record)
  failed <- identical(record$extraction_status, "failed")
  card(card_body(
    div(
      class = "reading-header",
      h2(if (finished) "Finished reading" else "Reading your questionnaire"),
      textOutput(ns("elapsed"), inline = TRUE)
    ),
    tags$ol(class = "reading-steps", map_rows(steps, reading_step)),
    if (failed) actionButton(ns("retry"), "Try reading again", class = "btn-outline-dark"),
    if (!finished) {
      p(
        class = "reading-note",
        "You can leave this page. The draft will be waiting under ",
        actionLink(ns("to_continue"), "Existing Draft"), " until it's pushed."
      )
    },
    div(actionButton(ns("cancel"), "Cancel extraction", class = "btn-outline-dark"))
  ))
}

reading_step <- function(step) {
  tags$li(
    class = paste0("reading-step is-", step$state),
    span(class = "reading-step-mark", if (step$state == "done") icon("check") else if (step$state == "attention") icon("exclamation")),
    div(strong(step$label), div(class = "form-text", step$detail))
  )
}

#' The line at the top of a mode of the Extraction page: what it is for, and a control on the right.
#'
#' @param text What the mode is for.
#' @param ... Anything shown on the right (a save indicator, a toggle).
#' @return A UI element.
mode_intro <- function(text, ...) {
  div(class = "mode-intro", p(class = "subhead", text), div(class = "mode-intro-aside", ...))
}
