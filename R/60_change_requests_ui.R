#' The Change Requests page: items waiting for one other staff member to approve them.
#'
#' @param id The module's id.
#' @return The page's UI.
change_requests_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("main"),
    class = "page change-requests-page",
    `data-ns` = ns(""),
    page_header(
      "Change Requests",
      "Items waiting for approval before they go into the MATR metadata database.",
      uiOutput(ns("whose_toggle"))
    ),
    uiOutput(ns("cases"))
  )
}

#' "Waiting for me (1)", "My requests (2)" and "All waiting (3)": which requests are listed.
#'
#' @param input_id The input's id; its value is `"me"`, `"mine"` or `"everyone"`.
#' @param whose The one picked now.
#' @param n_me,n_mine,n_everyone The counts shown on the buttons.
#' @return A UI element.
change_request_whose_buttons <- function(input_id, whose, n_me, n_mine, n_everyone) {
  choices <- c("me", "mine", "everyone")
  names(choices) <- c(
    sprintf("Waiting for me (%d)", n_me), sprintf("My requests (%d)", n_mine), sprintf("All waiting (%d)", n_everyone)
  )
  toggle_input(input_id, choices, whose, label = "Which requests")
}

# ------ One card for each request ------------------------------------------------------------
#' One request waiting for approval.
#'
#' @param record A request, from `change_request_service_waiting()`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @return A UI element.
change_request_card_ui <- function(record, me) {
  state <- change_request_service_state(record, me)
  kind <- change_request_service_kind(record)
  div(
    class = "change-request-card",
    div(
      class = "change-request-body",
      div(
        class = "change-request-title-row",
        row_badge(kind, class = paste0("is-", tolower(kind))),
        h2(class = "change-request-title", if (nzchar(record$summary %||% "")) record$summary else "(no description)"),
        if (state$mine) row_badge("Yours"),
        if (state$asked_of_me) row_badge("Asked of you", class = "is-warning"),
        if (state$needs_info) row_badge("Needs Basic Info", class = "is-warning"),
        if (state$changed_by_me) row_badge("Changed by you")
      ),
      p(
        class = "change-request-meta",
        paste(c(
          paste("Prepared by", if (state$mine) "you" else record$prepared_by),
          paste("sent for review", time_ago(parse_time(record$submitted %||% record$saved))),
          if (!is.null(record$requested_reviewer)) paste("asked", record$requested_reviewer$name %||% "someone", "to review it"),
          if (!identical(record$saved_by_id, record$prepared_by_id)) {
            paste("last changed by", if (state$changed_by_me) "you" else record$saved_by)
          }
        ), collapse = " · ")
      ),
      if (state$mine) {
        p(class = "change-request-note-you", "You prepared this. One other staff member has to approve it.")
      } else if (state$changed_by_me) {
        p(class = "change-request-note-you", "You made the latest changes to this, so someone else has to approve it.")
      }
    ),
    div(
      class = "change-request-actions",
      review_button(
        change_request_review_label(state),
        "review",
        `data-id` = record$id,
        class = if (state$can_approve || state$can_add || (state$mine && state$needs_info)) "btn btn-primary" else "btn btn-outline-dark"
      )
    )
  )
}

# What the card's button says: what this person has to do about the request, if anything.
change_request_review_label <- function(state) {
  if (state$can_approve) {
    return("Review and approve")
  }
  if (state$mine && state$needs_info) {
    return("Fill in Basic Info")
  }
  if (state$can_add) {
    return("Review and add")
  }
  "See the request"
}

#' What a person reads before approving, sending back or withdrawing a request.
#'
#' For an extraction (`extraction_request_ui()`): its Basic Info, the form where the registry, study,
#' questionnaire and wave it goes into are filled in and saved; what it adds, as cards of the rows that
#' are there and the NEW ones; and the whole questionnaire laid out as a table, with a button to its PDF
#' when the file is here, so a reviewer needn't open it on the Edit Extraction page. It can't be
#' approved (or added, by an ADMIN who prepared it) until its Basic Info is saved; the page's server
#' keeps those buttons disabled until then.
#'
#' @param record A request.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param ns The module's namespace function.
#' @param detail What the request comes to: for an edit `change_request_service_changes()`; for an
#'   extraction `list(preview = <from extraction_service_request_preview()>, draft = <its questionnaire,
#'   or NULL if it couldn't be read>, pdf_url = <the PDF's address, or NULL>)`.
#' @return A `modalDialog()`, for `showModal()`.
change_request_dialog <- function(record, me, ns, detail) {
  state <- change_request_service_state(record, me)
  extraction <- identical(record$kind, extraction_kind)
  modalDialog(
    title = if (nzchar(record$summary %||% "")) record$summary else "(no description)",
    size = if (extraction) "xl" else "l",
    easyClose = TRUE,
    p(
      class = "change-request-meta",
      paste(c(
        paste("Prepared by", if (state$mine) "you" else record$prepared_by),
        paste("sent for review", time_ago(parse_time(record$submitted %||% record$saved))),
        if (!is.null(record$requested_reviewer)) paste("asked", record$requested_reviewer$name %||% "someone")
      ), collapse = " · ")
    ),
    if (extraction) {
      extraction_request_ui(detail$preview, detail$draft, detail$pdf_url, basic_info = basic_info_ui(ns, state))
    } else {
      tagList(
        p(class = "form-text", change_service_counts_text(detail$counts)),
        change_items_ui(detail$groups)
      )
    },
    if (state$can_approve) {
      edit_callout(
        "warning",
        title = "Approving adds this to the MATR metadata database now.",
        "All of it is done at once. If a row has changed since this was prepared, nothing is added and you are told which."
      )
    },
    footer = tagList(
      modalButton("Close"),
      actionButton(ns("edit"), "Edit", class = "btn-outline-dark"),
      if (state$can_withdraw) actionButton(ns("withdraw"), "Withdraw", class = "btn-outline-dark"),
      if (state$can_send_back) actionButton(ns("send_back"), "Send back", class = "btn-outline-dark"),
      if (state$can_add) actionButton(ns("add"), "Add it to the database", class = "btn-primary", disabled = state$needs_info),
      if (state$can_approve) actionButton(ns("approve"), "Approve", class = "btn-primary", disabled = state$needs_info)
    )
  )
}

#' An extraction request's Basic Info: the form, and the button that saves it.
#'
#' @param ns The module's namespace function.
#' @param state From `change_request_service_state()`.
#' @return A UI element.
basic_info_ui <- function(ns, state) {
  tagList(
    p(
      class = "form-text",
      "The registry, study, questionnaire and wave this goes into. It can't be approved until these are filled in and saved.",
      if (state$can_approve) " Saving them changes the request, so someone else then approves it."
    ),
    extraction_info_ui(ns("info")),
    div(
      class = "basic-info-actions",
      uiOutput(ns("info_status"), inline = TRUE),
      actionButton(ns("save_info"), "Save Basic Info", icon = icon("floppy-disk"), class = "btn-outline-dark", disabled = TRUE)
    )
  )
}

#' The dialog that asks what to say when sending a request back.
#'
#' @param record The request.
#' @param ns The module's namespace function.
#' @return A `modalDialog()`, for `showModal()`.
change_request_send_back_dialog <- function(record, ns) {
  modalDialog(
    title = "Send it back",
    p("It goes back to ", strong(record$prepared_by), ", with what you say here. They change it and send it again."),
    textAreaInput(
      ns("send_back_note"), NULL,
      rows = 3, width = "100%", placeholder = "What needs changing? e.g. This is the wrong wave: it should be Wave 2."
    ) |>
      tag_input_attributes("textarea", `aria-label` = "What needs changing?"),
    footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_send_back"), "Send it back", class = "btn-primary")),
    easyClose = TRUE
  )
}

#' What is shown when there is nothing to list.
#'
#' @param whose `"me"`, `"mine"` or `"everyone"`.
#' @return A UI element.
no_change_requests <- function(whose) {
  div(
    class = "change-request-card is-empty",
    p(switch(whose,
      me = "Nothing is waiting for you to approve.",
      mine = "You have no requests waiting.",
      "Nothing is waiting for approval."
    )),
    p(class = "form-text", "Requests appear here when a STAFF member sends a delete, or an extraction, for approval.")
  )
}
