# ------ What a set of items changes ------------------------------------------------------
# An edit is a list of items, one for each field it changes (R/service_edit.R). The
# Change Requests page shows them before a change is applied and the Edit History page after.
# This turns them into what a person reads: the rows they touch, grouped by the row,
# with a count of each kind of change.

# A row an edit adds has no number yet and is named new:<table>:<n>.
change_new_row_pattern <- "^new:([a-z_]+):([0-9]+)$"

#' How a row is named in an item, in words.
#'
#' `{"component_id":88,"questionnaire_id":3}` is "component_id 88, questionnaire_id 3"; a row
#' the edit adds, `new:component:1`, is "a new row (1)".
#'
#' @param record_identifier The row's key as JSON, from `edit_identifier()`.
#' @return A string.
change_service_row_name <- function(record_identifier) {
  key <- tryCatch(jsonlite::fromJSON(record_identifier, simplifyVector = FALSE), error = function(problem) NULL)
  if (!is.list(key) || length(key) == 0) {
    return(as.character(record_identifier))
  }
  parts <- vapply(names(key), function(name) {
    value <- as.character(key[[name]])
    paste(name, if (grepl(change_new_row_pattern, value)) paste0("(new row ", sub(change_new_row_pattern, "\\2", value), ")") else value)
  }, character(1))
  paste(parts, collapse = ", ")
}

#' The rows an edit touches, each with the fields it changes.
#'
#' Items for the same row and action go together, in the order the rows first appear.
#'
#' @param items The items: a data frame with `entity_name`, `record_identifier`, `action`,
#'   `attribute_name`, `old_value` and `new_value`.
#' @return A list with one entry for each row: `table`, `row` (how it is named), `action`
#'   (`"Insert"`, `"Update"` or `"Delete"`) and `fields`, a data frame of `column`, `old`
#'   and `new` (`NA` for nothing). Empty if there are no items.
change_service_groups <- function(items) {
  if (is.null(items) || NROW(items) == 0) {
    return(list())
  }
  key <- paste(items$entity_name, items$record_identifier, items$action, sep = "\r")
  lapply(unique(key), function(one) {
    rows <- items[key == one, , drop = FALSE]
    has_field <- !is.na(rows$attribute_name)
    list(
      table = rows$entity_name[1],
      row = change_service_row_name(rows$record_identifier[1]),
      action = rows$action[1],
      fields = data.frame(
        column = rows$attribute_name[has_field],
        old = rows$old_value[has_field],
        new = rows$new_value[has_field],
        stringsAsFactors = FALSE
      )
    )
  })
}

#' How many rows an edit adds, changes and removes.
#'
#' @param groups The rows an edit touches, from `change_service_groups()`.
#' @return A list: `added`, `changed` and `removed` (counts of rows) and `tables` (how many
#'   tables they are in).
change_service_counts <- function(groups) {
  count <- function(action) sum(vapply(groups, function(group) identical(group$action, action), logical(1)))
  list(
    added = count("Insert"), changed = count("Update"), removed = count("Delete"),
    tables = length(unique(vapply(groups, function(group) group$table, character(1))))
  )
}

#' The counts as a sentence.
#'
#' For example "Adds 3 rows, changes 2 and removes 18, in 7 tables."
#'
#' @param counts The result of `change_service_counts()`.
#' @return A string; empty if the edit touches nothing.
change_service_counts_text <- function(counts) {
  parts <- c(
    if (counts$added > 0) paste("adds", plural(counts$added, "row")),
    if (counts$changed > 0) paste("changes", plural(counts$changed, "row")),
    if (counts$removed > 0) paste("removes", plural(counts$removed, "row"))
  )
  if (length(parts) == 0) {
    return("")
  }
  text <- if (length(parts) == 1) parts else paste(paste(parts[-length(parts)], collapse = ", "), "and", parts[length(parts)])
  paste0(sub("^(.)", "\\U\\1", text, perl = TRUE), ", in ", plural(counts$tables, "table"), ".")
}
