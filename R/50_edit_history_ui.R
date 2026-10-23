#' The Edit History page: every edit that has been applied to the MATR metadata database.
#'
#' @param id The module's id.
#' @return The page's UI.
edit_history_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("main"),
    class = "page history-page",
    `data-ns` = ns(""),
    page_header(
      "Edit History",
      "Every change that has been added to the MATR metadata database, newest first.",
      toggle_input(
        ns("kind"), c("All" = "all", "Added" = "Add", "Updated" = "Update", "Deleted" = "Delete"),
        selected = "all", label = "Kind of edit"
      )
    ),
    textInput(
      ns("search"), NULL,
      placeholder = "Search by what changed or who was behind it", width = "100%"
    ) |> tagAppendAttributes(class = "history-search"),
    uiOutput(ns("entries"))
  )
}

# ------ One card for each edit -----------------------------------------------------------
#' One edit in the history.
#'
#' @param record An edit, from `history_service_get_all()`.
#' @return A UI element.
history_entry_ui <- function(record) {
  div(
    class = "list-card",
    div(
      class = "list-card-body",
      div(
        class = "list-card-title-row",
        row_badge(record$kind %||% "Edit", class = paste0("is-", tolower(record$kind %||% "edit"))),
        h2(class = "list-card-title", if (nzchar(record$summary)) record$summary else "(no description)")
      ),
      p(class = "list-card-meta", paste(c(history_service_when(record), history_service_people(record)), collapse = " · "))
    ),
    div(
      class = "list-card-actions",
      review_button("See what changed", "open", `data-id` = record$id, class = "btn btn-outline-dark")
    )
  )
}

#' What is shown when there is nothing to list.
#'
#' @param filtered Whether a filter or search hid every edit, rather than there being none.
#' @return A UI element.
no_history <- function(filtered) {
  div(
    class = "list-card is-empty",
    p(if (filtered) "No edit matches that." else "Nothing has been added to the MATR metadata database through this site yet.")
  )
}
