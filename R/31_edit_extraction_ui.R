#' The Edit Extraction page: a title, how far the checking has got, and a row of buttons over the
#' extracted questionnaire.
#'
#' One panel: the outline of the whole questionnaire as it was extracted, where every block can be
#' edited in place (`extraction_outline_ui()`). The header's buttons are all the same size: Undo, Save,
#' Open PDF (if the file is here), MATR IDs and Preview Change Request.
#'
#' (It was called the review page. Change Requests has its own review, of requests waiting for approval.)
#'
#' @param id The module's id.
#' @return The page's UI.
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
      uiOutput(ns("progress"), class = "review-progress"),
      div(
        class = "review-actions",
        uiOutput(ns("save_status"), inline = TRUE),
        actionButton(ns("undo"), "Undo", icon = icon("rotate-left"), class = "btn-outline-dark", disabled = TRUE),
        actionButton(ns("save"), "Save", icon = icon("floppy-disk"), class = "btn-outline-dark", disabled = TRUE),
        uiOutput(ns("pdf_link"), inline = TRUE),
        review_button(
          tagList(icon("id-badge"), " MATR IDs"), "matr_ids",
          class = "btn btn-outline-dark", title = "Upload the variables' MATR IDs from a CSV, and give them out"
        ),
        uiOutput(ns("request_button"), inline = TRUE)
      )
    ),
    uiOutput(ns("banner")),
    card(
      class = "structure-card",
      card_body(class = "structure-body", padding = 0, extraction_outline_ui(ns("outline")))
    )
  ))
}

#' A button that opens the questionnaire's PDF in a new window, if the file is on this server.
#'
#' @param url The address the browser gets the PDF from, or `NULL`.
#' @return A UI element, or `NULL`.
pdf_open_button <- function(url) {
  if (is.null(url)) {
    return(NULL)
  }
  tags$a(
    href = url, target = "_blank", rel = "noopener", class = "btn btn-outline-dark",
    icon("file-pdf"), " Open PDF"
  )
}

#' How far the checking has got: a bar that fills as blocks are marked as checked.
#'
#' @param counts From `draft_service_counts()`.
#' @return A UI element saying "N% human-reviewed", with the counts in its tooltip.
structure_progress_ui <- function(counts) {
  percent <- draft_service_percent_checked(counts)
  bslib::tooltip(
    div(
      class = "review-structure",
      div(
        class = "progress review-structure-bar", role = "progressbar", `aria-label` = "Blocks checked",
        `aria-valuenow` = percent, `aria-valuemin` = 0, `aria-valuemax` = 100,
        div(class = "progress-bar", style = paste0("width: ", percent, "%")),
        span(class = "review-structure-text", paste0(percent, "% human-reviewed"))
      )
    ),
    sprintf("%d of %d blocks marked as checked", counts$checked, counts$blocks)
  )
}

#' What is said above the panel: a note from a reviewer who sent the draft back, or that it is waiting.
#'
#' @param record The extraction's record.
#' @return A UI element or `NULL`.
edit_extraction_banner <- function(record) {
  if (identical(record$status, "Sent Back") && !is.null(record$sent_back)) {
    return(edit_callout(
      "warning",
      title = paste0(record$sent_back$by, " sent this back"),
      p(record$sent_back$note), p("Change what they ask, save, and submit the change request again.")
    ))
  }
  if (identical(record$status, "Awaiting Review")) {
    edit_callout(
      "info",
      title = "This is waiting for approval on Change Requests",
      "Saving your changes makes a new version of the request, which nobody has approved yet."
    )
  }
}

#' The MATR IDs dialog: upload a CSV of them, then give them to the variables.
#'
#' Two ways: Auto Apply gives them out in the order the variables appear; Manual Assignment leaves each
#' variable's MATR ID to be picked, from the uploaded IDs no other variable has, or typed.
#'
#' @param ns The module's namespace function.
#' @param can_apply Whether there are uploaded IDs to give out.
#' @param recorded Whether the questionnaire's earlier waves have MATR IDs to fill in from.
#' @return A `modalDialog()`, for `showModal()`.
matr_ids_dialog <- function(ns, can_apply, recorded = FALSE) {
  modalDialog(
    title = "MATR IDs",
    size = "l",
    easyClose = TRUE,
    if (recorded) {
      tagList(
        p(
          "This questionnaire is recorded already, so a question worded the same as one it already asks ",
          "gets the MATR IDs that question has. That was done when the draft opened; do it again after changing ",
          "a question's words."
        ),
        p(actionButton(ns("fill_recorded"), "Fill from the earlier waves", class = "btn-outline-dark"))
      )
    },
    p("Upload a CSV that lists the MATR IDs in the order the variables appear on the questionnaire."),
    drop_zone(
      fileInput(ns("matr_csv"), NULL, accept = c(".csv", "text/csv"), buttonLabel = "Choose file") |>
        tagAppendAttributes(class = "quiet-upload"),
      uiOutput(ns("matr_csv_status")),
      icon = "file-lines"
    ),
    uiOutput(ns("matr_fill_note")),
    div(
      class = "matr-modes",
      div(
        class = "matr-mode",
        h3("Auto Apply"),
        p(
          class = "form-text",
          "Gives the uploaded IDs out in the order the variables appear: the first variable without a MATR ID ",
          "gets the first ID no variable has, and so on."
        ),
        checkboxInput(ns("matr_replace"), "Replace the MATR IDs the variables already have", value = FALSE),
        actionButton(ns("auto_apply"), "Auto Apply", class = "btn-primary", disabled = !can_apply)
      ),
      div(
        class = "matr-mode",
        h3("Manual Assignment"),
        p(
          class = "form-text",
          "Pick each variable's MATR ID from the uploaded IDs no other variable has, or type one. ",
          "Open a question's variables in the outline to do it."
        ),
        actionButton(ns("manual_assign"), "Assign by hand", class = "btn-outline-dark", disabled = !can_apply)
      )
    ),
    footer = modalButton("Close")
  )
}

#' The Preview Change Request dialog: the questionnaire as its change request shows it.
#'
#' Nothing is written while it is open. Submit Change Request goes on to sending it. Where it goes
#' (its Basic Info) isn't asked here: it is filled in on Change Requests.
#'
#' @param ns The module's namespace function.
#' @return A `modalDialog()`, for `showModal()`.
preview_request_dialog <- function(ns) {
  modalDialog(
    title = "Preview Change Request",
    size = "xl",
    easyClose = FALSE,
    uiOutput(ns("preview")),
    footer = tagList(
      actionButton(ns("preview_cancel"), "Cancel", class = "btn-outline-dark"),
      uiOutput(ns("preview_blocker"), inline = TRUE, class = "quick-add-blocker"),
      actionButton(ns("submit_request"), "Submit Change Request", class = "btn-primary", disabled = TRUE)
    )
  )
}

#' The dialog that submits an extraction's change request.
#'
#' Every request goes to Change Requests, where its Basic Info is filled in.
#'
#' @param ns The module's namespace function.
#' @param summary What the questionnaire holds, in words.
#' @param unchecked How many blocks haven't been marked as checked.
#' @param approval Whether it needs another staff member's approval (a STAFF member's request) rather
#'   than being added by whoever submits it (an ADMIN).
#' @param reviewers People who can be asked to review it: a named vector of staff ids, or empty.
#' @return A `modalDialog()`, for `showModal()`.
submit_request_dialog <- function(ns, summary, unchecked, approval, reviewers) {
  modalDialog(
    title = "Submit Change Request",
    size = "l",
    easyClose = FALSE,
    p(strong(summary)),
    if (unchecked > 0) {
      edit_callout(
        "warning",
        title = sprintf("%s haven't been marked as checked", plural(unchecked, "block")),
        "You can still go on. Reviewers may want them checked first."
      )
    },
    if (approval) {
      edit_callout(
        "info",
        title = "It goes to Change Requests",
        "Fill in its Basic Info there: the registry, study, questionnaire and wave it goes into. Then one other ",
        "staff member approves it. Nothing is added to the MATR metadata database until they do, and then all of it is added at once."
      )
    } else {
      edit_callout(
        "info",
        title = "It goes to Change Requests",
        "Fill in its Basic Info there: the registry, study, questionnaire and wave it goes into. As an ADMIN you can ",
        "then add it yourself, without approval. It is recorded in the Edit history."
      )
    },
    if (approval && length(reviewers) > 0) {
      tagList(
        reviewer_select(ns("request_reviewer"), reviewers),
        p(class = "form-text", "They are emailed a link once sign-in is set up. Until then, any other staff member can approve it.")
      )
    },
    footer = tagList(
      actionButton(ns("back_to_preview"), "Back", class = "btn-outline-dark"),
      actionButton(ns("confirm_request"), "Send to Change Requests", class = "btn-primary")
    )
  )
}
