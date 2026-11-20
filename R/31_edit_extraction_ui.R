# The Edit Extraction page: a title on the left and a row of buttons on the right, with how far the checking
# has got across the page under both, over the extracted questionnaire.
# One panel: the whole questionnaire as it was extracted, as a list with the picked block's details
# beside it (`extraction_outline_ui()`). The header's buttons: Undo, Save, Open PDF (if the file is here: it
# opens in its own tab, at the page of the block picked), Re-extract (an AI-Powered draft read again with the latest prompt:
# `reextract_dialog()`) and Preview and push (where it goes, what pushing adds, and the push itself:
# `push_dialog()`).
# On a phone the buttons are only their icons, sharing a row of their own evenly under the progress bar.
# (It was called the review page.)
edit_extraction_ui <- function(id) {
  ns <- NS(id)
  as_fill_carrier(div(
    class = "edit-extraction-page",
    `data-ns` = ns(""),
    div(
      class = "review-header",
      div(
        class = "review-title",
        h1(textOutput(ns("title"), inline = TRUE)),
        p(class = "subhead", textOutput(ns("subtitle"), inline = TRUE), uiOutput(ns("badges"), inline = TRUE))
      ),
      div(
        class = "review-actions",
        uiOutput(ns("save_status"), inline = TRUE),
        div(
          class = "review-buttons",
          actionButton(ns("undo"), icon_label("undo", "Undo"), class = "btn-outline-dark", disabled = TRUE, title = "Undo"),
          actionButton(ns("save"), icon_label("save", "Save"), class = "btn-outline-dark", disabled = TRUE, title = "Save"),
          uiOutput(ns("pdf_link"), inline = TRUE),
          uiOutput(ns("reextract_button"), inline = TRUE),
          uiOutput(ns("request_button"), inline = TRUE)
        )
      ),
      uiOutput(ns("progress"), class = "review-progress") # how far the checking has got, across the page
    ),
    uiOutput(ns("banner")),
    card(
      class = "structure-card",
      card_body(class = "structure-body", padding = 0, extraction_outline_ui(ns("outline")))
    )
  ))
}

# A button that opens the questionnaire's PDF in a new window, if the file is on this server.
# On Edit Extraction it opens at the page the picked block was read from (`at_picked`, www/js/review.js),
# when that is known.
# url: The address the browser gets the PDF from, or `NULL`.
# at_picked: Whether it opens at the picked block's page.
# Returns: A UI element, or `NULL`.
pdf_open_button <- function(url, at_picked = FALSE) {
  if (is.null(url)) {
    return(NULL)
  }
  tags$a(
    href = url, target = "_blank", rel = "noopener", class = "btn btn-outline-dark",
    `data-pdf-at-picked` = if (at_picked) NA,
    title = if (at_picked) "Open the PDF in a new tab, at the page of the block picked" else "Open the PDF in a new tab",
    icon_label("pdf", "Open PDF")
  )
}

# How far the checking has got: a bar that fills as blocks are marked as checked.
# counts: From `draft_service_counts()`.
# Returns: A UI element saying "N% human-reviewed", with the counts in its tooltip. The bar is the browser's
#   own `<progress>`, drawn in _edit_extraction.scss, so the server sets how far, not how wide.
structure_progress_ui <- function(counts) {
  percent <- draft_service_percent_checked(counts)
  bslib::tooltip(
    div(
      class = "review-structure",
      div(
        class = "review-structure-bar",
        tags$progress(value = percent, max = 100, `aria-label` = "Blocks checked"),
        span(class = "review-structure-text", `aria-hidden` = "true", paste0(percent, "% human-reviewed"))
      )
    ),
    sprintf("%d of %d blocks marked as checked", counts$checked, counts$blocks)
  )
}

# What is said above the panel while this page doesn't have the draft to work on: who does, or that it
# was let go while staff were away. The page only shows the draft then (`.claim-lost`).
# claim: Why the page doesn't have it, as `edit_extraction_server()` keeps it: `NULL` while it does.
# unsaved: Whether there are changes on screen that aren't saved.
# Returns: A UI element or `NULL`.
edit_extraction_claim_banner <- function(ns, claim, unsaved = FALSE) {
  if (is.null(claim)) {
    return(NULL)
  }
  if (identical(claim$why, "taken")) {
    return(div(class = "claim-lost", edit_callout(
      "warning",
      title = sprintf("%s is working on this draft", claim$by),
      p(
        "You can look at it, but not change it, so neither of you overwrites the other.",
        if (unsaved) "Your changes that aren't saved can't be saved while they have it."
      ),
      actionButton(ns("take_over_banner"), "Take over", class = "btn-outline-dark")
    )))
  }
  div(class = "claim-lost", edit_callout(
    "info",
    title = "You were away, so others can open this draft",
    p("Nobody else has it yet. Keep working to change it again."),
    actionButton(ns("keep_working"), "Keep working", class = "btn-primary")
  ))
}

# Re-extract: the PDF read again from the start, with the extraction prompt as it is now, in place of the
# questionnaire on the page.
reextract_dialog <- function(ns) {
  confirm_dialog(
    "Re-extract this questionnaire?",
    p("Gemini reads the PDF again from the start, with the latest extraction instructions, and its Variable Names and Coding again if it has them."),
    p(strong("The new reading replaces the questionnaire here:"), " every change made on this page, saved or not, and every block marked as checked, is lost."),
    p("It is read in the background, as a new extraction is. You can watch it on New Extraction, or find it under Extraction Drafts."),
    confirm_id = ns("confirm_reextract"), confirm_label = "Re-extract", keep_label = "Cancel"
  )
}

# The Push to the database dialog, built from the pieces of `extraction_request_ui()`: what is in the way, the
# Basic Info form (where the extraction goes), why the database would refuse it if it would, and the
# questionnaire laid out as a table, with NEW beside what the push adds. The form is drawn once, here, so its
# picks stay while the parts around it are drawn again by the page (`push_blockers`, `push_what`,
# `preview_table`). Push adds it all to the MATR metadata database at once; Cancel closes the dialog with
# nothing added. A draft reopened from a wave (Edit extraction on the Metadata page) goes back into that wave
# and nowhere else (`extraction_service_reopened_info()`), so its dialog says which wave it updates instead.
# draft: The questionnaire on screen, for how many of its blocks haven't been marked as checked.
# updates: For a reopened draft, the wave it updates in words ("Kid Questionnaire (KQ) · Wave 1"); `NULL` for
#   any other, whose dialog has the form.
push_dialog <- function(ns, draft, updates = NULL) {
  modalDialog(
    title = "Push to the database",
    size = "xl",
    easyClose = FALSE,
    uiOutput(ns("push_blockers")),
    if (is.null(updates)) {
      extraction_request_ui(list(), NULL, basic_info = extraction_info_ui(ns("info")), questionnaire = FALSE)
    } else {
      div(
        class = "request-preview",
        div(
          class = "request-section",
          h3("Updating"),
          p(class = "form-text", "Pushing replaces what ", strong(updates), " asks with this draft.")
        )
      )
    },
    div(class = "edit-extraction-rewordings", `data-ns` = ns(""), uiOutput(ns("push_rewordings"))),
    uiOutput(ns("push_what")),
    extraction_request_ui(list(unchecked = extraction_service_unchecked(draft)), draft, table = uiOutput(ns("preview_table"))),
    footer = tagList(
      actionButton(ns("push_cancel"), "Cancel", class = "btn-outline-dark"),
      uiOutput(ns("push_blocker"), inline = TRUE, class = "quick-add-blocker"),
      actionButton(ns("push"), "Push", class = "btn-primary", disabled = TRUE)
    )
  )
}

# The wordings of a reopened extraction that other places print too, each with the choice of where its new
# words go: only here (this questionnaire gets a wording of its own) or everywhere (the wording itself changes,
# in every questionnaire that prints it). The choice made is the filled-in button.
# choices: From `extraction_service_rewordings()`.
# Returns: A section of the Push dialog, or `NULL` when there is nothing to choose. Its buttons are data-action
#   buttons of the page (`reword`).
rewordings_ui <- function(choices) {
  if (nrow(choices) == 0) {
    return(NULL)
  }
  choice <- function(row, everywhere, label, title) {
    review_button(
      label, "reword", `data-id` = row$id, `data-everywhere` = if (everywhere) "true" else "false",
      class = c("btn btn-sm", if (identical(row$everywhere, everywhere)) "btn-dark" else "btn-outline-dark"),
      `aria-pressed` = if (identical(row$everywhere, everywhere)) "true" else "false", title = title
    )
  }
  div(
    class = "request-section",
    h3("Similar Wording"),
    p(
      class = "form-text",
      "You changed these words but other places print them. Choose whether to update only this questionnaire's wording or update the wording in every questionnaire."
    ),
    tags$table(
      class = "table table-sm rewordings-table",
      tags$thead(tags$tr(tags$th("Was"), tags$th("Now"), tags$th("Also printed in"), tags$th("Change it"))),
      tags$tbody(lapply(seq_len(nrow(choices)), function(i) {
        row <- choices[i, ]
        tags$tr(
          tags$td(row$was), tags$td(row$now), tags$td(row$elsewhere),
          tags$td(div(
            class = "btn-group", role = "group", `aria-label` = "Where the new words go",
            choice(row, FALSE, "Only here", "This questionnaire gets a wording of its own; the other places keep theirs"),
            choice(row, TRUE, "Everywhere", "The wording itself changes, in every place that prints it")
          ))
        )
      }))
    )
  )
}

