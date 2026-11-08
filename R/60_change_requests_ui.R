# The Change Requests page: items waiting for one other staff member to approve them.
change_requests_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("main"),
    class = "page change-requests-page",
    `data-ns` = ns(""),
    page_header(
      "Change Requests",
      "Items waiting for approval before they go into the MATR metadata database.",
      uiOutput(ns("whose_toggle")),
      eyebrow = "Review", icon_name = "change_requests"
    ),
    uiOutput(ns("cases"))
  )
}

# "Waiting for me (1)", "My requests (2)" and "All waiting (3)": which requests are listed.
# input_id: The input's id; its value is `"me"`, `"mine"` or `"everyone"`.
# whose: The one picked now.
# n_me,n_mine,n_everyone: The counts shown on the buttons.
change_request_whose_buttons <- function(input_id, whose, n_me, n_mine, n_everyone) {
  choices <- c("me", "mine", "everyone")
  names(choices) <- c(
    sprintf("Waiting for me (%d)", n_me), sprintf("My requests (%d)", n_mine), sprintf("All waiting (%d)", n_everyone)
  )
  toggle_input(input_id, choices, whose, label = "Which requests")
}

# ------ One card for each request ------------------------------------------------------------
# One request waiting for approval.
# record: A request, from `change_request_service_waiting()`.
change_request_card_ui <- function(record, me) {
  state <- change_request_service_state(record, me)
  kind <- change_request_service_kind(record)
  div(
    class = "list-card",
    div(
      class = "list-card-body",
      div(
        class = "list-card-title-row",
        row_badge(kind, class = paste0("is-", tolower(kind))),
        h2(class = "list-card-title", if (nzchar(record$summary %||% "")) record$summary else "(no description)"),
        if (state$mine) row_badge("Yours"),
        if (state$asked_of_me) row_badge("Asked of you", class = "is-warning"),
        if (state$needs_info) row_badge("Needs Basic Info", class = "is-warning"),
        if (state$changed_by_me) row_badge("Changed by you")
      ),
      p(
        class = "list-card-meta",
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
        p(class = "list-card-note", "You prepared this. One other staff member has to approve it.")
      } else if (state$changed_by_me) {
        p(class = "list-card-note", "You made the latest changes to this, so someone else has to approve it.")
      }
    ),
    div(
      class = "list-card-actions",
      # an extraction's questionnaire, as Edit Extraction's Preview shows it
      if (identical(record$kind, extraction_kind)) {
        review_button(icon_label("preview", "Preview"), "preview", `data-id` = record$id, class = "btn btn-outline-dark")
      },
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

# What a person reads before approving, sending back or withdrawing a request.
# For an extraction (`extraction_request_ui()`): its Basic Info, the form where the registry, study,
# questionnaire and wave it goes into are filled in and saved, and what it adds, as cards of the rows that
# are there and the NEW ones. The questionnaire itself, with Edit and Withdraw, is the request's Preview
# (`change_request_preview_dialog()`). It can't be approved (or added, by an ADMIN who prepared it) until its
# Basic Info is saved; the page's server keeps those buttons disabled until then.
# detail: What the request comes to: for an edit `change_request_service_changes()`; for an
#   extraction `list(preview = <from extraction_service_request_preview()>)`.
# Returns: A `modalDialog()`, for `showModal()`. Saving an extraction's Basic Info changes the request, so what
#   depends on it is in parts of their own (`request_title`, `request_detail`, `request_footer`), which the page
#   draws again in place (`change_request_parts()`) while the form stays as it is.
change_request_dialog <- function(record, me, ns, detail) {
  state <- change_request_service_state(record, me)
  extraction <- identical(record$kind, extraction_kind)
  parts <- change_request_parts(record, me, ns, detail, ready = !state$needs_info)
  modalDialog(
    title = span(id = ns("request_title"), parts$title),
    size = if (extraction) "xl" else "l",
    easyClose = TRUE,
    p(
      class = "list-card-meta",
      paste(c(
        paste("Prepared by", if (state$mine) "you" else record$prepared_by),
        paste("sent for review", time_ago(parse_time(record$submitted %||% record$saved))),
        if (!is.null(record$requested_reviewer)) paste("asked", record$requested_reviewer$name %||% "someone")
      ), collapse = " · ")
    ),
    if (extraction) extraction_request_ui(list(), NULL, basic_info = basic_info_ui(ns, state), questionnaire = FALSE),
    div(id = ns("request_detail"), parts$detail),
    footer = div(id = ns("request_footer"), class = "change-request-footer", parts$footer)
  )
}

# The parts of a request's dialog that depend on the request as saved: its title, what it comes to (with the
# note that approving adds it now) and the buttons. Each is one element, so the page can put it in place of the
# one that is there.
# ready: Whether Approve (or an ADMIN's Add) can be pressed: its Basic Info is saved and fits.
# Returns: A list of UI elements: `title`, `detail` and `footer`.
change_request_parts <- function(record, me, ns, detail, ready) {
  state <- change_request_service_state(record, me)
  extraction <- identical(record$kind, extraction_kind)
  list(
    title = span(if (nzchar(record$summary %||% "")) record$summary else "(no description)"),
    detail = div(
      if (extraction) {
        extraction_request_ui(detail$preview, NULL, questionnaire = FALSE)
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
      }
    ),
    footer = div(
      class = "change-request-footer-buttons",
      modalButton("Close"),
      # an extraction's Edit and Withdraw are on its Preview, with the questionnaire
      if (!extraction) actionButton(ns("edit"), "Edit", class = "btn-outline-dark"),
      if (!extraction && state$can_withdraw) actionButton(ns("withdraw"), "Withdraw", class = "btn-outline-dark"),
      if (state$can_send_back) actionButton(ns("send_back"), "Send back", class = "btn-outline-dark"),
      if (state$can_add) actionButton(ns("add"), "Add it to the database", class = "btn-primary", disabled = !ready),
      if (state$can_approve) actionButton(ns("approve"), "Approve", class = "btn-primary", disabled = !ready)
    )
  )
}

# An extraction request's questionnaire, as the Edit Extraction page's Preview shows it: how many blocks
# haven't been marked as checked, its PDF and the questionnaire laid out as a table. Edit opens it on the Edit
# Extraction page, Withdraw sets it aside (its preparer only), and the last button goes on to the request's
# Basic Info dialog (`change_request_dialog()`).
# draft: Its questionnaire, or `NULL` if it couldn't be read.
# pdf_url: The address of its PDF, or `NULL`.
# table: Where the page draws the questionnaire's table once the dialog is open, or `NULL` to draw it here.
change_request_preview_dialog <- function(record, me, ns, draft, pdf_url = NULL, table = NULL) {
  state <- change_request_service_state(record, me)
  modalDialog(
    title = paste("Preview:", if (nzchar(record$summary %||% "")) record$summary else "(no description)"),
    size = "xl",
    easyClose = TRUE,
    extraction_request_ui(list(unchecked = if (!is.null(draft)) extraction_service_unchecked(draft)), draft, pdf_url, table = table),
    footer = tagList(
      modalButton("Close"),
      actionButton(ns("edit"), "Edit", class = "btn-outline-dark"),
      if (state$can_withdraw) actionButton(ns("withdraw"), "Withdraw", class = "btn-outline-dark"),
      actionButton(ns("preview_review"), change_request_review_label(state), class = "btn-primary")
    )
  )
}

# An extraction request's Basic Info: the form, and the button that saves it.
# state: From `change_request_service_state()`.
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
      actionButton(ns("save_info"), "Save Basic Info", icon = app_icon("save"), class = "btn-outline-dark", disabled = TRUE)
    )
  )
}

# The dialog that asks what to say when sending a request back.
# record: The request.
change_request_send_back_dialog <- function(record, ns) {
  confirm_dialog(
    "Send it back",
    p("It goes back to ", strong(record$prepared_by), ", with what you say here. They change it and send it again."),
    textAreaInput(
      ns("send_back_note"), NULL,
      rows = 3, width = "100%", placeholder = "What needs changing? e.g. This is the wrong wave: it should be Wave 2."
    ) |>
      tag_input_attributes("textarea", `aria-label` = "What needs changing?"),
    confirm_id = ns("confirm_send_back"), confirm_label = "Send it back", keep_label = "Cancel", easy_close = TRUE
  )
}

# What is shown when there is nothing to list.
# whose: `"me"`, `"mine"` or `"everyone"`.
no_change_requests <- function(whose) {
  div(
    class = "list-card is-empty",
    p(switch(whose,
      me = "Nothing is waiting for you to approve.",
      mine = "You have no requests waiting.",
      "Nothing is waiting for approval."
    )),
    p(class = "form-text", "Requests appear here when a STAFF member sends a delete, or an extraction, for approval.")
  )
}
