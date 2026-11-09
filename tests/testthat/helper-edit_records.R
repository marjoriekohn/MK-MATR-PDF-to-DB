# ------ Edits for the tests ---------------------------------------------------------------------------
# The tables an edit of a row the test just made is built from, as edit_service_load() reads them, but in one
# request: only the kind's own table is read. Nothing points at the test's own row yet, so the tables that would
# (for a respondent type, its questionnaires and everything they hold) are empty here instead of read, and the
# edit comes out the same. Fine for a kind whose form picks from no other table, as a respondent type's doesn't.
test_edit_snap <- function(entity) {
  tables <- edit_service_tables(entity)
  stats::setNames(lapply(tables, function(table) {
    if (identical(table, entity$table)) table_model_get(test_connection, table) else empty_table(table)
  }), tables)
}

# A table with no rows, its columns of the kinds table_model_get() gives them.
empty_table <- function(table) {
  kinds <- table_read_columns(database_tables[[table]])
  as.data.frame(lapply(kinds, function(kind) switch(kind, id = , number = numeric(), character())))
}

# Removes the rows an applied change added: each new row by the number the database gave it (`new_ids`, from
# `edit_service_apply()`), the tables that point at others first. The change's history row goes with the test's
# staff.
test_remove_added <- function(new_ids) {
  tables <- sub("^new:([a-z_]+):.*$", "\\1", names(new_ids))
  for (table in intersect(edit_delete_order, tables)) {
    for (id in unlist(new_ids[tables == table])) {
      test_delete(table, stats::setNames(list(database_eq(id)), edit_table_key(table)[1]))
    }
  }
  invisible()
}

# Removes them when the test ends.
local_added_rows <- function(new_ids, env = parent.frame()) {
  withr::defer(test_remove_added(new_ids), envir = env)
  invisible(new_ids)
}

# An edit applied by `staff`, the way Make an edit does it: the form for kind `key` in mode `verb`, with these
# values typed (by field id) and, for an update or a delete, the row `row_key` (one the test made: see
# test_edit_snap()). Returns what `edit_service_apply()` gives; what the edit added is removed when the test ends.
local_applied_edit <- function(staff, key, verb, row_key = NULL, ..., env = parent.frame()) {
  entity <- edit_entities[[key]]
  snap <- test_edit_snap(entity)
  row <- if (!is.null(row_key)) edit_service_row(entity, snap, as.character(row_key))
  built <- if (verb == "delete") {
    edit_service_build(entity, "delete", snap, row)
  } else {
    fields <- edit_service_fields(entity, snap, verb, row)
    values <- utils::modifyList(edit_service_initial(entity, fields, snap, verb, row), list(...))
    edit_service_build(entity, verb, snap, row, fields = fields, values = values)
  }
  applied <- edit_service_apply(test_connection, built, staff)
  local_added_rows(applied$new_ids, env = env)
  applied
}
