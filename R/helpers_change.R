# ------ A list of changed rows -------------------------------------------------------------
# What the Edit history and Change Requests pages draw for an edit's items: each row it adds,
# changes or removes, with its fields. The rows come grouped from `change_service_groups()`.

# The most rows drawn; a delete can reach thousands.
change_rows_shown <- 150

#' The rows an edit touches.
#'
#' A change shows the old value struck through and the new one highlighted, an add shows
#' what is put in and a removal what is taken out.
#'
#' @param groups The rows an edit touches, from `change_service_groups()`.
#' @param limit The most rows to draw.
#' @return A UI element.
change_items_ui <- function(groups, limit = change_rows_shown) {
  if (length(groups) == 0) {
    return(p(class = "form-text", "No fields are listed for this edit."))
  }
  div(
    class = "change-list",
    lapply(utils::head(groups, limit), change_row_ui),
    if (length(groups) > limit) {
      p(class = "form-text", paste0("and ", format(length(groups) - limit, big.mark = ","), " more rows, not shown here."))
    }
  )
}

change_row_ui <- function(group) {
  words <- c(Insert = "Add", Update = "Change", Delete = "Remove")
  div(
    class = c("change-row", paste0("is-", tolower(group$action))),
    div(
      class = "change-row-head",
      span(class = "change-action", if (group$action %in% names(words)) words[[group$action]] else group$action),
      span(class = "change-table", gsub("_", " ", group$table)),
      span(class = "change-key", group$row)
    ),
    if (nrow(group$fields) > 0) {
      tags$dl(class = "change-fields", lapply(seq_len(nrow(group$fields)), function(i) {
        field <- group$fields[i, ]
        tagList(tags$dt(field$column), tags$dd(change_value_ui(group$action, field$old, field$new)))
      }))
    }
  )
}

change_value_ui <- function(action, old, new) {
  shown <- function(value) if (is.na(value)) "(empty)" else value
  switch(action,
    Update = tagList(tags$del(shown(old)), span(class = "edit-arrow", " → "), tags$ins(shown(new))),
    Delete = tags$del(shown(old)),
    tags$ins(shown(new))
  )
}
