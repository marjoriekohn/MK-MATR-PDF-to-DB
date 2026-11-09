# ------ Edit History ---------------------------------------------------------------
# The history table: one row for each change staff made to the catalog, written by the database's own
# functions as they make it (push_extraction(), apply_change() and empty_*() in sql/4_functions.sql) and never
# edited (history_is_kept). The site only reads it.
#
# A record's fields:
#   id          the row's id, as text
#   action      Pushed (an extraction), Emptied (a wave, questionnaire, study or registry), Added, Updated or
#               Deleted (a change on Make an edit)
#   entity      the table the change was about; record_id its key, as text (NULL for none)
#   summary     the change in a sentence
#   staff       who made it (NA if they are no longer listed)
#   occurred    when, as the database sends times
#   details     only from history_model_get(): what was done. For Added, Updated and Deleted a list of items,
#               one per field, {item_number, entity_name, record_identifier, action, attribute_name, old_value,
#               new_value}; for Pushed and Emptied a named list of counts (questions, variable_new,
#               placements_removed...) with the ids of what was pushed or emptied.

history_table <- "history"
history_list_columns <- c("history_id", "occurred_at", "action", "entity", "record_id", "summary", "staff(name)")

# The columns of an item of what an edit changed, as the history's details list them (apply_change()).
history_item_columns <- c("item_number", "entity_name", "record_identifier", "action", "attribute_name", "old_value", "new_value")

# Every change, newest first, without its details.
# most: Only the newest this many (up to 1,000); all of them by default.
# Returns: A list of records, named by id; empty if nothing has been changed yet.
history_model_get_all <- function(connection, most = Inf) {
  rows <- database_model_select(
    connection, history_table, history_list_columns,
    order_by = c("occurred_at.desc", "history_id.desc"), most = most
  )
  records <- lapply(rows, history_model_record)
  stats::setNames(records, vapply(records, function(record) record$id, character(1)))
}

# One change, with its details.
# id: The row's id: digits only.
# Returns: The record. An error if there is none.
history_model_get <- function(connection, id) {
  if (!grepl("^[0-9]+$", id %||% "")) stop("There's no change with that number.", call. = FALSE)
  rows <- database_model_select(
    connection, history_table, c(history_list_columns, "details"),
    filters = list(history_id = database_eq(id))
  )
  if (length(rows) == 0) stop("That change is no longer in the history.", call. = FALSE)
  history_model_record(rows[[1]])
}

# The fields one change changed, as a table.
# id: The row's id: digits only.
# Returns: A data frame with `history_item_columns`, in the order they were applied; no rows for a push or an
#   emptying, whose details are counts.
history_model_get_items <- function(connection, id) {
  history_model_items(history_model_get(connection, id)$details)
}

# The number of changes.
history_model_count <- function(connection) {
  database_model_count(connection, history_table)
}

history_model_record <- function(row) {
  list(
    id = as.character(row$history_id),
    action = row$action,
    entity = row$entity,
    record_id = row$record_id,
    summary = row$summary %||% "",
    staff = as.character(row$staff$name %||% NA),
    occurred = row$occurred_at,
    details = row$details
  )
}

# A change's details as the items table Make an edit's pages read.
# details: A record's `details`: a list of items, or anything else (counts, nothing) for no items.
# Returns: A data frame with `history_item_columns`; a missing value is `NA`.
history_model_items <- function(details) {
  items <- if (is.list(details) && is.null(names(details))) details else list()
  columns <- lapply(history_item_columns, function(column) {
    vapply(items, function(item) {
      value <- item[[column]]
      if (is.null(value) || length(value) == 0) NA_character_ else as.character(value)
    }, character(1))
  })
  table <- as.data.frame(stats::setNames(columns, history_item_columns), stringsAsFactors = FALSE)
  table$item_number <- as.integer(table$item_number)
  table
}
