# The AI-Powered mode of the Extraction page: the PDF and how reading it is going.
# Staff choose the questionnaire PDF (and, if they have one, the file of Variable Names and Coding), and
# reading starts when they click Begin Extraction. From then on the files are the draft's and can't be
# changed. Reading happens in the background, so staff can leave and find the draft under Extraction Drafts; once it
# has finished, the whole extraction opens on the Edit Extraction page to review and edit before its
# change request is submitted.
# Returns: The mode's UI.
extraction_ai_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "extraction-mode extraction-ai",
    `data-ns` = ns(""),
    card(card_body(
      h2("Questionnaire PDF"),
      uiOutput(ns("pdf_card")),
      uiOutput(ns("supplement")),
      uiOutput(ns("reading")),
      div(class = "extraction-footer",
          uiOutput(ns("footer_note"), inline = TRUE),
          uiOutput(ns("footer_button"), inline = TRUE) # Begin Extraction, then Open in Edit Extraction
      )
    ))
  )
}

# A dashed box files can be dropped on.
# `www/js/extraction.js` passes dropped files to the file input inside.
# file_input: The file input.
# ...: What to say about it.
# icon: The icon, by its name in `app_icons`.
drop_zone <- function(file_input, ..., icon = "pdf") {
  div(
    class = "drop-zone",
    span(class = "drop-zone-icon", app_icon(icon)),
    div(class = "drop-zone-text", ...),
    file_input
  )
}

# ------ Step 1: the PDF -------------------------------------------------------------
# Somewhere to drop the PDF, before one is chosen.
pdf_drop_zone <- function(ns) {
  drop_zone(
    fileInput(ns("pdf"), NULL, accept = c(".pdf", "application/pdf"), buttonLabel = "Choose PDF"),
    strong("Drop the questionnaire PDF here"),
    div("Reading starts when you click Begin Extraction.")
  )
}

# The PDF: the one chosen, which can be swapped until the extraction begins, or the draft's.
# pdf: `list(name, bytes)`, and `pages` once the draft has it.
# begun: Whether the extraction has begun: the draft has the file, so it can't be swapped.
pdf_file <- function(pdf, ns, begun = TRUE) {
  facts <- c(if (!is.null(pdf$pages)) plural(pdf$pages, "page"), file_size(pdf$bytes))
  div(
    class = "pdf-file",
    span(class = "pdf-file-icon", "PDF"),
    div(class = "pdf-file-name", strong(pdf$name), div(class = "form-text", paste(facts, collapse = " · "))),
    if (!begun) {
      fileInput(ns("replace_pdf"), NULL, accept = c(".pdf", "application/pdf"), buttonLabel = "Replace") |>
        tagAppendAttributes(class = "file-button")
    }
  )
}

# ------ The file of Variable Names and Coding (optional) ---------------------------------
# Somewhere to give the file of Variable Names and Coding (MATR IDs and codes), or the file given.
# Optional. Gemini applies it once the questionnaire is read and checked, as a step of its own. It can be
# given, changed or taken away until the extraction begins; after that the page only says which file it is.
# name: The file's name, or `NULL` if none is given.
# begun: Whether the extraction has begun.
# Returns: A UI element, or `NULL` once the extraction has begun without one.
supplement_card <- function(name, begun, ns) {
  accept <- c(".pdf", ".csv", ".tsv", ".txt")
  if (begun) {
    return(if (!is.null(name)) {
      p(class = "form-text supplement-note", app_icon("matr_id"), " Variable Names and Coding from ", strong(name, .noWS = "after"), ".")
    })
  }
  if (is.null(name)) {
    return(drop_zone(
      fileInput(ns("supplement"), NULL, accept = accept, buttonLabel = "Choose file"),
      strong("Optional: Variable Names and Coding"),
      div("A codebook or list (PDF, CSV or text) giving each variable its name (MATR ID) and each answer its code.
          They're applied once the questionnaire is read; the questionnaire itself isn't changed."),
      icon = "matr_id"
    ) |> tagAppendAttributes(class = "is-supplement"))
  }
  div(
    class = "pdf-file is-supplement",
    span(class = "pdf-file-icon", app_icon("matr_id")),
    div(class = "pdf-file-name", strong(name), div(class = "form-text", "Variable Names and Coding, applied once the questionnaire is read")),
    fileInput(ns("supplement"), NULL, accept = accept, buttonLabel = "Replace") |>
      tagAppendAttributes(class = "file-button"),
    actionButton(ns("remove_supplement"), "Remove", class = "btn-outline-dark")
  )
}

# ------ Reading progress --------------------------------------------------------------
# How reading the PDF is going.
reading_card <- function(record, ns) {
  steps <- extraction_service_reading_steps(record)
  finished <- extraction_service_is_read(record)
  failed <- identical(record$extraction_status, "failed")
  card(card_body(
    div(
      class = "reading-header",
      h2(if (finished) "Finished reading" else "Reading your questionnaire"),
      if (!finished && !failed) reading_elapsed(record$reading_started)
    ),
    tags$ol(class = "reading-steps", map_rows(steps, reading_step)),
    if (failed) actionButton(ns("retry"), "Try reading again", class = "btn-outline-dark"),
    if (!finished) {
      tagList(
        p(
          class = "reading-note",
          "You can leave this page. The draft will be waiting under ",
          actionLink(ns("to_continue"), "Extraction Drafts"), " until its change request is submitted."
        ),
        # once it is read, the draft is discarded from Extraction Drafts instead
        div(actionButton(ns("cancel"), "Cancel extraction", class = "btn-outline-dark"))
      )
    }
  ))
}

# How long reading has taken so far.
# The browser counts on from here each second (`www/js/extraction.js`), so the server isn't asked,
# and the page doesn't show it is busy, every second.
# started: When reading started, as the database writes times, or `NULL`.
# Returns: A UI element, or `NULL` when reading hasn't started.
reading_elapsed <- function(started) {
  started <- if (!is.null(started) && length(started) == 1 && !is.na(started)) parse_time(started)
  if (is.null(started) || is.na(started)) {
    return(NULL)
  }
  seconds <- max(0, floor(as.numeric(difftime(Sys.time(), started, units = "secs"))))
  span(class = "reading-elapsed", `data-elapsed` = seconds, duration_text(seconds))
}

reading_step <- function(step) {
  tags$li(
    class = paste0("reading-step is-", step$state),
    span(class = "reading-step-mark", if (step$state == "done") app_icon("done") else if (step$state == "attention") app_icon("attention")),
    div(strong(step$label), div(class = "form-text", step$detail))
  )
}

# The line at the top of a mode of the Extraction page: what it is for, and a control on the right.
# text: What the mode is for.
# ...: Anything shown on the right (a save indicator, a toggle).
mode_intro <- function(text, ...) {
  div(class = "mode-intro", p(class = "subhead", text), div(class = "mode-intro-aside", ...))
}
