# ------ Adding from inside a drop-down ------------------------------------------------------
# The extraction pages' drop-downs can add what is missing without leaving the page: a respondent
# type, a study, and the investigator a study needs. It is the Make an edit form for one row, and
# like any add it is applied at once (for STAFF and ADMIN alike), written to the Edit history, and
# warned about if it looks like something that is already there (`edit_service_duplicates()`).

# The kinds that can be added this way.
edit_quick_kinds <- c("respondent_type", "investigator", "study")

#' The kind of thing a drop-down can add, as one row.
#'
#' @param key One of `edit_quick_kinds`.
#' @return A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_entities.R`).
edit_service_quick_entity <- function(key) {
  if (!key %in% edit_quick_kinds) stop("That can't be added from here.", call. = FALSE)
  entity <- edit_entities[[key]]
  if (key == "study") {
    entity$about <- paste0(
      "One research project run under a registry. It is added to the database now; the questionnaire you are ",
      "extracting is added to it when its change request is approved."
    )
  }
  entity
}

#' Add one row from inside a drop-down.
#'
#' Saved as an edit first, so if the database refuses it the person's work is still there on Make an
#' edit; then applied, which writes the Edit history.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param entity The kind, from `edit_service_quick_entity()`.
#' @param snap The tables the kind needs, as `edit_service_load()` reads them.
#' @param fields The form's fields, from `edit_service_fields()`.
#' @param values What is in the form: a named list of strings by field id.
#' @param me Who is adding it: `list(staff_id, name, role)`.
#' @param now The time to record.
#' @return A list: `record` (the saved edit), `history_id` and `key` (the new row's key, as text, for
#'   the drop-down to pick). An error with the database's own sentence if it refused.
edit_service_quick_add <- function(connection, entity, snap, fields, values, me, now = Sys.time()) {
  built <- edit_service_build(entity, "add", snap, fields = fields, values = values)
  form <- list(action = "add", entity = entity$key, scope = "", target = "", values = values)
  saved <- edit_service_save(connection, edit_service_record(me, form, built, "Draft", now = now), me)
  history_id <- edit_service_apply(connection, saved, me)
  list(
    record = saved,
    history_id = history_id,
    key = edit_service_quick_key(history_model_get_items(connection, as.character(history_id)), entity$table)
  )
}

#' The key of the row an add made, from the history's items.
#'
#' @param items The edit's items, from `history_model_get_items()`.
#' @param table The table that was added to.
#' @return The key as text, or `NA` if the edit added nothing to the table.
edit_service_quick_key <- function(items, table) {
  made <- items[items$entity_name == table & items$action == "Insert", , drop = FALSE]
  if (nrow(made) == 0) {
    return(NA_character_)
  }
  key <- edit_table_key(table)[1]
  key_text(jsonlite::fromJSON(made$record_identifier[1])[[key]])
}

#' The choices a drop-down offers after something was added.
#'
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param kind One of `edit_quick_kinds`.
#' @param key The new row's key, as text.
#' @return A one-row data frame of the row in the catalog, or `NULL` if it is not there.
edit_service_quick_row <- function(catalog, kind, key) {
  table <- switch(kind, respondent_type = "respondent_types", study = "studies", investigator = NULL)
  if (is.null(table)) {
    return(NULL)
  }
  rows <- catalog[[table]]
  id <- edit_table_key(edit_entities[[kind]]$table)[1]
  found <- rows[as.character(rows[[id]]) %in% key, , drop = FALSE]
  if (nrow(found) == 1) found
}
