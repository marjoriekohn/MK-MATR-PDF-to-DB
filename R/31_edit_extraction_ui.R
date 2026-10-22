#' The Edit Extraction page: a title on the left and a row of buttons on the right, with how far the checking
#' has got across the page under both, over the extracted questionnaire.
#'
#' One panel: the whole questionnaire as it was extracted, as a list with the picked block's details
#' beside it (`extraction_outline_ui()`). The header's buttons: Undo, Save, Open PDF (if the file is here:
#' it opens in its own tab, at the page of the block picked), MATR IDs and Preview (of the change request).
#' On a phone the buttons are only their icons, sharing a row of their own evenly under the progress bar.
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
      div(
        class = "review-actions",
        uiOutput(ns("save_status"), inline = TRUE),
        div(
          class = "review-buttons",
          actionButton(ns("undo"), icon_label("undo", "Undo"), class = "btn-outline-dark", disabled = TRUE, title = "Undo"),
          actionButton(ns("save"), icon_label("save", "Save"), class = "btn-outline-dark", disabled = TRUE, title = "Save"),
          uiOutput(ns("pdf_link"), inline = TRUE),
          review_button(
            icon_label("matr_id", "MATR IDs"), "matr_ids",
            class = "btn btn-outline-dark", title = "Give the variables their MATR IDs: from a CSV, or from a questionnaire that asks the same questions"
          ),
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

#' A button that opens the questionnaire's PDF in a new window, if the file is on this server.
#'
#' On Edit Extraction it opens at the page the picked block was read from (`at_picked`, www/js/review.js),
#' when that is known.
#'
#' @param url The address the browser gets the PDF from, or `NULL`.
#' @param at_picked Whether it opens at the picked block's page.
#' @return A UI element, or `NULL`.
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

#' What is said above the panel while this page doesn't have the draft to work on: who does, or that it
#' was let go while staff were away. The page only shows the draft then (`.claim-lost`).
#'
#' @param ns The module's namespace function.
#' @param claim Why the page doesn't have it, as `edit_extraction_server()` keeps it: `NULL` while it does.
#' @param unsaved Whether there are changes on screen that aren't saved.
#' @return A UI element or `NULL`.
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

#' The MATR IDs dialog: three ways to give the variables their MATR IDs.
#'
#' From a CSV, uploaded here: Auto Apply gives them out in the order the variables appear; Manual
#' Assignment leaves each variable's MATR ID to be picked, from the uploaded IDs no other variable has, or
#' typed. From a questionnaire: a question worded the same as one a recorded questionnaire asks gets the
#' MATR IDs that question's variables have.
#'
#' @param ns The module's namespace function.
#' @param can_apply Whether there are uploaded IDs to give out.
#' @param questionnaires The recorded questionnaires to pick from (`questionnaire_service_all_choices()`).
#' @param picked The questionnaire picked to start with: the one the extraction goes into, if it is known.
#' @return A `modalDialog()`, for `showModal()`.
matr_ids_dialog <- function(ns, can_apply, questionnaires = character(), picked = "") {
  modalDialog(
    title = "MATR IDs",
    size = "l",
    easyClose = TRUE,
    p("Upload a CSV that lists the MATR IDs in the order the variables appear on the questionnaire."),
    drop_zone(
      fileInput(ns("matr_csv"), NULL, accept = c(".csv", "text/csv"), buttonLabel = "Choose file") |>
        tagAppendAttributes(class = "quiet-upload"),
      uiOutput(ns("matr_csv_status")),
      icon = "csv"
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
      ),
      div(
        class = "matr-mode",
        h3("From a questionnaire"),
        p(
          class = "form-text",
          "A question worded the same as one the questionnaire asks gets the MATR IDs that question's ",
          "variables have, first to first. Only variables without a MATR ID are filled in."
        ),
        selectInput(
          ns("matr_questionnaire"), NULL,
          c(if (length(questionnaires) == 0) c("No questionnaires recorded yet" = "") else c("Select questionnaire" = ""), questionnaires),
          selected = picked, width = "100%", selectize = length(questionnaires) > 30
        ) |> tag_input_attributes("select", `aria-label` = "Questionnaire to take MATR IDs from"),
        actionButton(ns("import_matr_ids"), "Import MATR IDs", class = "btn-outline-dark", disabled = !has_text(picked))
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
