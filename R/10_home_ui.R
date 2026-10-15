#' The Home page: a button to each working page, and the newest change requests and applied
#' changes.
#'
#' @param id The module's id.
#' @return The page's UI.
home_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page home-page",
    `data-ns` = ns(""),
    div(
      class = "home-links",
      home_link(
        ns("extraction"), "file-arrow-up", "Extraction",
        "Extract questionnaire metadata from a PDF."
      ),
      home_link(
        ns("make_edit"), "pencil", "Make an edit",
        "Add, update, or delete an item from the database."
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      home_box(
        ns, "change_requests", "clipboard-check", "Change Requests",
        "Waiting for a staff member to review and approve.", uiOutput(ns("requests"))
      ),
      home_box(
        ns, "edit_history", "clock-rotate-left", "Edit history",
        "The newest changes added to the MATR metadata database.", uiOutput(ns("history"))
      )
    )
  )
}

#' A button to one of the pages: its icon, its name and what it is for.
#'
#' @param id The button's input id.
#' @param icon_name The icon.
#' @param title The page's name.
#' @param text What the page is for.
#' @return A UI element.
home_link <- function(id, icon_name, title, text) {
  tags$button(
    id = id, type = "button", class = "action-button home-link",
    icon(icon_name),
    span(class = "home-link-title", title),
    span(class = "home-link-text", text)
  )
}

# A box that lists the newest few of something, with a button to the page that lists them all.
home_box <- function(ns, page, icon_name, title, text, list_output) {
  card(
    class = "home-box",
    card_body(
      div(class = "home-box-head", icon(icon_name), h2(class = "home-box-title", title), p(class = "home-box-text", text)),
      list_output,
      div(class = "home-box-more", uiOutput(ns(paste0(page, "_more")), inline = TRUE))
    )
  )
}

#' One change request, in a line.
#'
#' @param record A request, from `change_request_service_waiting()`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @return A UI element.
home_request_item <- function(record, me) {
  kind <- change_request_service_kind(record)
  who <- if (draft_row_service_is_mine(record, me)) "you" else record$prepared_by
  div(
    class = "home-item",
    row_badge(kind, class = paste0("is-", tolower(kind))),
    div(
      class = "home-item-body",
      span(class = "home-item-title", if (nzchar(record$summary %||% "")) record$summary else "(no description)"),
      span(
        class = "home-item-meta",
        paste("Sent by", who, time_ago(parse_time(record$submitted %||% record$saved)))
      )
    )
  )
}

#' One applied change, in a line.
#'
#' @param record An applied change, from `history_service_get_all()`.
#' @return A UI element.
home_history_item <- function(record) {
  div(
    class = "home-item",
    row_badge(record$kind %||% "Edit", class = paste0("is-", tolower(record$kind %||% "edit"))),
    div(
      class = "home-item-body",
      span(class = "home-item-title", if (nzchar(record$summary)) record$summary else "(no description)"),
      span(class = "home-item-meta", paste(c(history_service_when(record), history_service_people(record)), collapse = " · "))
    )
  )
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
