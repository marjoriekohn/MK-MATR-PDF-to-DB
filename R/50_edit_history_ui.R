# The Edit History page: every change made to the MATR metadata database through this site.
edit_history_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("main"),
    class = "page history-page",
    `data-ns` = ns(""),
    page_header(
      "Edit History",
      "Every change made to the MATR metadata database through this site, newest first.",
      toggle_input(
        ns("kind"), c("All" = "all", stats::setNames(history_actions, history_actions)),
        selected = "all", label = "Kind of change"
      ),
      eyebrow = "Audit trail", icon_name = "history"
    ),
    textInput(
      ns("search"), NULL,
      placeholder = "Search by what changed or who made it", width = "100%"
    ) |> tagAppendAttributes(class = "history-search"),
    uiOutput(ns("entries"))
  )
}

# ------ One card for each change ---------------------------------------------------------
# One change in the history.
history_entry_ui <- function(record) {
  div(
    class = "list-card",
    div(
      class = "list-card-body",
      div(
        class = "list-card-title-row",
        row_badge(record$action, class = paste0("is-", tolower(record$action))),
        h2(class = "list-card-title", if (nzchar(record$summary)) record$summary else "(no description)")
      ),
      p(class = "list-card-meta", paste(history_service_when(record), history_service_who(record), sep = " · "))
    ),
    div(
      class = "list-card-actions",
      review_button("See what changed", "open", `data-id` = record$id, class = "btn btn-outline-dark")
    )
  )
}

# What a push or an emptying did: its counts, a line each.
# lines: From `history_service_count_lines()`.
history_counts_ui <- function(lines) {
  if (length(lines) == 0) {
    return(p(class = "form-text", "No counts were recorded for this change."))
  }
  tags$ul(class = "history-counts", lapply(lines, tags$li))
}

# What is shown when there is nothing to list.
# filtered: Whether a filter or search hid every change, rather than there being none.
no_history <- function(filtered) {
  div(
    class = "list-card is-empty",
    p(if (filtered) "No change matches that." else "Nothing has been changed in the MATR metadata database through this site yet.")
  )
}
