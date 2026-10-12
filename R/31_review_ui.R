#' The review page: a title, how far the checking has got, and a row of buttons over the
#' questionnaire's structure.
#'
#' One panel: the outline of what was extracted, where every block is edited in place
#' (`review_outline_ui()`). The header's buttons are all the same size: Undo, Save, Edit Details,
#' Open PDF (if the file is here), Upload MATR IDs and Push.
#'
#' @param id The module's id.
#' @return The page's UI.
review_ui <- function(id) {
  ns <- NS(id)
  as_fill_carrier(div(
    class = "review-page",
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
        # What staff typed on the Extraction page, opened in a dialog
        actionButton(
          ns("edit_info"), "Edit Details",
          icon = icon("pen"), class = "btn-outline-dark",
          title = "Change the questionnaire and wave details and notes for reviewers"
        ),
        uiOutput(ns("pdf_link"), inline = TRUE),
        review_button(
          tagList(icon("file-arrow-up"), " Upload MATR IDs"), "matr_ids",
          class = "btn btn-outline-dark", title = "Fill the variables' MATR IDs from a CSV"
        ),
        uiOutput(ns("push_button"), inline = TRUE)
      )
    ),
    uiOutput(ns("banner")),
    card(
      class = "structure-card",
      card_body(class = "structure-body", padding = 0, review_outline_ui(ns("outline")))
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
#' @param checked How many blocks are marked as checked.
#' @param blocks How many blocks there are.
#' @return A UI element saying "N% complete", with the counts in its tooltip.
structure_progress_ui <- function(checked, blocks) {
  percent <- if (blocks > 0) floor(100 * checked / blocks) else 0L
  bslib::tooltip(
    div(
      class = "review-structure",
      div(
        class = "progress review-structure-bar", role = "progressbar", `aria-label` = "Blocks checked",
        `aria-valuenow` = percent, `aria-valuemin` = 0, `aria-valuemax` = 100,
        div(class = "progress-bar", style = paste0("width: ", percent, "%")),
        span(class = "review-structure-text", paste0(percent, "% complete"))
      )
    ),
    sprintf("%d of %d blocks marked as checked", checked, blocks)
  )
}

#' What is said above the panels: a note from a reviewer who sent the draft back, or that it is waiting.
#'
#' @param record The extraction's record.
#' @return A UI element or `NULL`.
review_banner <- function(record) {
  if (identical(record$status, "Sent Back") && !is.null(record$sent_back)) {
    return(edit_callout(
      "warning",
      title = paste0(record$sent_back$by, " sent this back"),
      p(record$sent_back$note), p("Change what they ask, save, and push it again.")
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

#' The dialog that asks to push the extraction to the database.
#'
#' @param ns The module's namespace function.
#' @param blockers What stops it, as sentences; empty when it can go.
#' @param summary What it adds, in words.
#' @param unchecked How many blocks haven't been marked as checked.
#' @param approval Whether it goes to Change Requests (a STAFF member's push) rather than being added at once.
#' @param reviewers People who can be asked to review it: a named vector of staff ids, or empty.
#' @param note The note to start with.
#' @return A `modalDialog()`, for `showModal()`.
push_dialog <- function(ns, blockers, summary, unchecked, approval, reviewers, note = "") {
  modalDialog(
    title = if (approval) "Send this for approval" else "Add this to the database",
    size = "l",
    easyClose = TRUE,
    if (length(blockers) > 0) {
      edit_callout("danger", title = "This can't go in yet", tags$ul(lapply(blockers, tags$li)))
    } else {
      tagList(
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
            title = "One other staff member has to approve it",
            "It waits on Change Requests. Nothing is added to the MATR data until they do, and then all of it is added at once."
          )
        } else {
          edit_callout(
            "info",
            title = "This is added at once",
            "As an ADMIN your push needs no approval. It is recorded in the Edit history."
          )
        },
        textAreaInput(
          ns("push_note"), if (approval) "Anything else the reviewer should know?" else "Anything to record with it?",
          value = note, rows = 3, width = "100%",
          placeholder = "e.g. Page 6 is a scanned insert, so its choices were typed by hand."
        ),
        if (approval && length(reviewers) > 0) {
          tagList(
            selectInput(
              ns("push_reviewer"), "Ask someone to review it (optional)", c("Anyone" = "", reviewers),
              width = "100%", selectize = FALSE
            ),
            p(class = "form-text", "They are emailed a link once sign-in is set up. Until then, any other staff member can approve it.")
          )
        }
      )
    },
    footer = tagList(
      modalButton("Cancel"),
      if (length(blockers) == 0) {
        actionButton(ns("confirm_push"), if (approval) "Send for approval" else "Add it to the database", class = "btn-primary")
      }
    )
  )
}
