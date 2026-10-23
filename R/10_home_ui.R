#' The Home page: what this person has to do. Their work in progress (extraction drafts and saved edits, those
#' sent back first, each with Continue), the change requests waiting for them (each with Review), and the
#' newest changes added to the database. New Extraction starts one; the menu leads to every page.
#'
#' @param id The module's id.
#' @return The page's UI.
home_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page home-page",
    `data-ns` = ns(""),
    page_header(
      "Home", "What you've started, and what's waiting for you.",
      actionButton(ns("new_extraction"), icon_label("add", "New Extraction"), class = "btn-primary")
    ),
    div(
      class = "home-columns",
      home_box(
        ns, "work", "edit", "Your work in progress",
        "Drafts you started and haven't sent for approval, and any sent back to you.", uiOutput(ns("work"))
      ),
      div(
        class = "home-side",
        home_box(
          ns, "change_requests", "change_requests", "Waiting for you",
          "Change requests you can approve, and your own that need you.", uiOutput(ns("requests"))
        ),
        home_box(
          ns, "edit_history", "history", "Recent changes",
          "The newest changes added to the MATR metadata database.", uiOutput(ns("history"))
        )
      )
    )
  )
}

# A box that lists something, with a button under it to the page that lists them all (`<key>_more`).
home_box <- function(ns, key, icon_name, title, text, list_output) {
  card(
    class = "home-box",
    card_body(
      div(class = "home-box-head", app_icon(icon_name), h2(class = "home-box-title", title), p(class = "home-box-text", text)),
      list_output,
      div(class = "home-box-more", uiOutput(ns(paste0(key, "_more")), inline = TRUE))
    )
  )
}

#' One line of a list: badges saying what it is, its title, a line about it, a reviewer's note if it has one,
#' and its button.
#'
#' @param badges Badges, from `row_badge()`.
#' @param title What it is called.
#' @param meta A line about it: who, when.
#' @param note A reviewer's note, or `NULL`.
#' @param button Its button, or `NULL`.
#' @return A UI element.
home_item <- function(badges, title, meta, note = NULL, button = NULL) {
  div(
    class = "home-item",
    div(
      class = "home-item-body",
      div(class = "home-item-badges", badges),
      span(class = "home-item-title", if (has_text(title)) title else "(no description)"),
      span(class = "home-item-meta", meta),
      if (!is.null(note)) span(class = "home-item-note", strong(paste0(note$by %||% "A reviewer", ": ")), paste0("“", note$note, "”"))
    ),
    button
  )
}

#' One of this person's drafts: an extraction or an edit, with Continue.
#'
#' A draft still being read says how far it has got, and its button shows the reading.
#'
#' @param item From `home_service_my_work()`: `list(kind, record)`.
#' @param catalog The live tables, from `questionnaire_service_get_catalog()`: an extraction is named by its
#'   questionnaire and wave once they are picked.
#' @return A UI element.
home_work_item <- function(item, catalog) {
  record <- item$record
  sent_back <- if (identical(record$status, "Sent Back")) record$sent_back
  when <- if (!is.null(sent_back)) {
    paste("Sent back", day_phrase(parse_time(sent_back$date)))
  } else {
    paste("Saved", time_ago(parse_time(record$saved)))
  }
  if (identical(item$kind, "extraction")) {
    status <- record$extraction_status
    reading <- isTRUE(status %in% c("queued", "sent", "received"))
    return(home_item(
      tagList(
        row_badge("Extraction"),
        if (!is.null(sent_back)) row_badge("Sent back", class = "is-warning"),
        if (reading) row_badge("Being read"),
        if (identical(status, "failed")) row_badge("Reading stopped", class = "is-warning")
      ),
      extraction_service_title(record, catalog),
      paste(c(when, record$pdf$name), collapse = " · "),
      note = sent_back,
      button = home_button(if (reading) "See progress" else "Continue", "continue", record$id, "extraction")
    ))
  }
  kind <- change_request_service_kind(record)
  home_item(
    tagList(
      row_badge(kind, class = paste0("is-", tolower(kind))),
      if (!is.null(sent_back)) row_badge("Sent back", class = "is-warning")
    ),
    record$summary, when,
    note = sent_back,
    button = home_button("Continue", "continue", record$id, "edit")
  )
}

#' One change request waiting for this person, with Review.
#'
#' @param record A request, from `change_request_service_waiting()`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @return A UI element.
home_request_item <- function(record, me) {
  kind <- change_request_service_kind(record)
  state <- change_request_service_state(record, me)
  who <- if (state$mine) "you" else record$prepared_by
  home_item(
    tagList(
      row_badge(kind, class = paste0("is-", tolower(kind))),
      if (state$asked_of_me) row_badge("Asked of you", class = "is-mine")
    ),
    record$summary,
    paste("Sent by", who, time_ago(parse_time(record$submitted %||% record$saved))),
    button = home_button(if (state$mine) "Open" else "Review", "review", record$id)
  )
}

#' One applied change.
#'
#' @param record An applied change, from `history_service_get_all()`.
#' @return A UI element.
home_history_item <- function(record) {
  kind <- record$kind %||% "Edit"
  home_item(
    row_badge(kind, class = paste0("is-", tolower(kind))),
    record$summary,
    paste(c(history_service_when(record), history_service_people(record)), collapse = " · ")
  )
}

# An item's button; the page script sends what it says (`action`, `id`, `kind`).
home_button <- function(label, action, id, kind = NULL) {
  review_button(label, action, `data-id` = id, `data-kind` = kind, class = "btn btn-sm btn-outline-dark home-item-button")
}

#' A list of items, or a line saying there are none.
#'
#' @param items UI elements, one per item.
#' @param none What to say when there are none.
#' @return A UI element.
home_item_list <- function(items, none) {
  if (length(items) == 0) {
    return(p(class = "home-empty", none))
  }
  div(class = "home-items", items)
}
