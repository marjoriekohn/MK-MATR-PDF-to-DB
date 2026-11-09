# The AI-Powered mode of the Extraction page: the PDF and how reading it is going.
# Staff choose the questionnaire PDF (and, if they have one, the file of Variable Names and Coding), and
# reading starts when they click Begin Extraction. From then on the files are the draft's and can't be
# changed. Reading happens in the background, so staff can leave and find the draft under Extraction Drafts; once it
# has finished, the whole extraction opens on the Edit Extraction page to review and edit before it is
# pushed.
# Returns: The mode's UI.
extraction_ai_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "extraction-mode extraction-ai",
    `data-ns` = ns(""),
    card(
      card_header(h2("Questionnaire PDF & Supplemental Files")),
      card_body(
        # the PDF and the file of Variable Names and Coding side by side on a wide window, one under the other on
        # a narrower one (bslib's grid); one alone, once reading has begun without a file, takes the whole row
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = 6), class = "upload-pair",
          uiOutput(ns("pdf_card")),
          uiOutput(ns("supplement_box"))
        ),
        uiOutput(ns("reading"))
      ),
      card_footer(
        class = "extraction-footer",
        uiOutput(ns("footer_note"), inline = TRUE),
        uiOutput(ns("footer_button"), inline = TRUE) # Begin Extraction, then Open in Edit Extraction
      )
    )
  )
}

# A box for a file not chosen yet: its icon, what it is for, and the button that chooses it.
# file_input: The file input.
# ...: What to say about it.
# icon: The icon, by its name in `app_icons`.
file_box <- function(file_input, ..., icon = "pdf") {
  div(
    class = "file-box",
    span(class = "file-box-icon", app_icon(icon)),
    div(class = "file-box-text", ...),
    file_input
  )
}

# ------ Step 1: the PDF -------------------------------------------------------------
# Where the PDF is chosen, before one is.
pdf_choice <- function(ns) {
  file_box(
    fileInput(ns("pdf"), NULL, accept = c(".pdf", "application/pdf"), buttonLabel = "Choose PDF"),
    strong("Questionnaire PDF"),
    div("Reading starts when you click Begin Extraction.")
  )
}

# A file given: its kind in a box (PDF, CSV), its name, a line of facts under it, and any buttons beside it.
# The PDF and the file of Variable Names and Coding are drawn the same way, chosen or begun.
# name: The file's name.
# facts: What to say under the name, joined with " · ".
# ...: Buttons (Replace, Remove), if the file can still be changed.
file_card <- function(name, facts, ...) {
  kind <- toupper(tools::file_ext(name))
  div(
    class = "pdf-file",
    span(class = "pdf-file-icon", if (nzchar(kind)) kind else "FILE"),
    div(class = "pdf-file-name", strong(name), div(class = "form-text", paste(facts, collapse = " · "))),
    ...
  )
}

# A file input drawn as the quiet Replace button beside a file.
replace_button <- function(ns, id, accept) {
  fileInput(ns(id), NULL, accept = accept, buttonLabel = "Replace") |> tagAppendAttributes(class = "file-button")
}

# The PDF: the one chosen, which can be swapped until the extraction begins, or the draft's.
# pdf: `list(name, bytes)`, and `pages` once the draft has it.
# begun: Whether the extraction has begun: the draft has the file, so it can't be swapped.
pdf_file <- function(pdf, ns, begun = TRUE) {
  facts <- c(if (!is.null(pdf$pages)) plural(pdf$pages, "page"), file_size(pdf$bytes))
  file_card(pdf$name, facts, if (!begun) replace_button(ns, "replace_pdf", c(".pdf", "application/pdf")))
}

# ------ The file of Variable Names and Coding (optional) ---------------------------------
# Somewhere to give the file of Variable Names and Coding (MATR IDs and codes), or the file given.
# Optional. Gemini applies it once the questionnaire is read and checked, as a step of its own. It can be
# given, changed or taken away until the extraction begins; after that the page only shows which file it is, as
# it shows the PDF.
# file: The file given, `list(name, bytes)` (`bytes` when it is known), or `NULL` if none is.
# begun: Whether the extraction has begun.
# Returns: A UI element, or `NULL` once the extraction has begun without one.
supplement_card <- function(file, begun, ns) {
  accept <- c(".pdf", ".csv", ".tsv", ".txt")
  if (is.null(file)) {
    return(if (!begun) {
      file_box(
        fileInput(ns("supplement"), NULL, accept = accept, buttonLabel = "Choose file"),
        strong("Optional: Variable Names and Coding"),
        div("A codebook or list (PDF, CSV or text) giving each variable its name (MATR ID) and each answer its code.
          They're applied once the questionnaire is read; the questionnaire itself isn't changed."),
        icon = "matr_id"
      ) |> tagAppendAttributes(class = "is-supplement")
    })
  }
  facts <- c("Variable Names and Coding", if (!is.null(file$bytes)) file_size(file$bytes))
  file_card(
    file$name, facts,
    if (!begun) replace_button(ns, "supplement", accept),
    if (!begun) actionButton(ns("remove_supplement"), "Remove", class = "btn-outline-dark")
  )
}

# ------ Reading progress --------------------------------------------------------------
# How reading the PDF is going: a panel in the Questionnaire PDF card, under the files.
reading_card <- function(record, ns) {
  steps <- extraction_service_reading_steps(record)
  finished <- extraction_service_is_read(record)
  failed <- identical(record$extraction_status, "failed")
  div(
    class = "reading-panel",
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
          actionLink(ns("to_continue"), "Extraction Drafts"), " until it is pushed."
        ),
        # once it is read, the draft is discarded from Extraction Drafts instead
        div(actionButton(ns("cancel"), "Cancel extraction", class = "btn-outline-dark"))
      )
    }
  )
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
