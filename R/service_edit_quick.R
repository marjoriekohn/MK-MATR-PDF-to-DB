# ------ Adding from inside a drop-down ------------------------------------------------------
# The Basic Info drop-downs on Change Requests can add what is missing without leaving the page: a
# registry, a study, a questionnaire and a wave, and what those need (a study's investigator, a
# questionnaire's respondent type). It is the Make an edit form for one row, and
# like any add it is applied at once (for STAFF and ADMIN alike), written to the Edit History, and
# warned about if it looks like something that is already there (`edit_service_duplicates()`).

# The kinds that can be added this way.
edit_quick_kinds <- c("registry", "study", "questionnaire", "wave", "respondent_type", "investigator")

# What a kind's form needs that may not be there yet, and can be added from its pop-up: the field it
# fills, by the kind that adds it.
edit_quick_needs <- list(
  study = c(investigator = "study__principal_investigator_id"),
  questionnaire = c(respondent_type = "questionnaire__respondent_type_id")
)

# The kind of thing a drop-down can add, as one row.
# key: One of `edit_quick_kinds`.
# Returns: A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
edit_service_quick_entity <- function(key) {
  if (!key %in% edit_quick_kinds) stop("That can't be added from here.", call. = FALSE)
  entity <- edit_entities[[key]]
  later <- "It is added to the database now; the extraction goes into it when its change request is approved."
  entity$about <- switch(key,
    study = paste("One research project run under a registry.", later),
    questionnaire = paste("A survey, with its respondent type and the study it belongs to.", later),
    wave = paste("One time the questionnaire was given. It is added now with no questions;", "the extraction's questions go into it when its change request is approved."),
    entity$about
  )
  entity
}

# Add one row from inside a drop-down.
# Saved as an edit first, so if the database refuses it the person's work is still there on Make an
# edit; then applied, which writes the Edit History.
# entity: The kind, from `edit_service_quick_entity()`.
# me: Who is adding it: `list(staff_id, name, role)`.
# Returns: A list: `record` (the saved edit), `history_id` and `key` (the new row's key, as text, for
#   the drop-down to pick). An error with the database's own sentence if it refused: if it refused to apply it,
#   the sentence also says it is kept as a draft on the Metadata page.
edit_service_quick_add <- function(connection, entity, snap, fields, values, me, now = Sys.time()) {
  built <- edit_service_build(entity, "add", snap, fields = fields, values = values)
  form <- list(action = "add", entity = entity$key, scope = "", target = "", values = values)
  saved <- edit_service_save(connection, edit_service_record(me, form, built, "Draft", now = now), me)
  history_id <- tryCatch(edit_service_apply(connection, saved, me), error = function(problem) {
    stop(conditionMessage(problem), " It is kept as a draft on the Metadata page, to finish there.", call. = FALSE)
  })
  list(
    record = saved,
    history_id = history_id,
    key = edit_service_quick_key(history_model_get_items(connection, as.character(history_id)), entity$table)
  )
}

# The key of the row an add made, from the history's items.
# items: The edit's items, from `history_model_get_items()`.
# table: The table that was added to.
# Returns: The key as text, or `NA` if the edit added nothing to the table.
edit_service_quick_key <- function(items, table) {
  made <- items[items$entity_name == table & items$action == "Insert", , drop = FALSE]
  if (nrow(made) == 0) {
    return(NA_character_)
  }
  key <- edit_table_key(table)[1]
  key_text(jsonlite::fromJSON(made$record_identifier[1])[[key]])
}

# The choices a drop-down offers after something was added.
# kind: One of `edit_quick_kinds`.
# key: The new row's key, as text.
# Returns: A one-row data frame of the row in the catalog, or `NULL` if it is not there.
edit_service_quick_row <- function(catalog, kind, key) {
  table <- switch(kind,
    registry = "registries", study = "studies", questionnaire = "questionnaires", wave = "waves",
    respondent_type = "respondent_types", investigator = NULL
  )
  if (is.null(table)) {
    return(NULL)
  }
  rows <- catalog[[table]]
  id <- edit_table_key(edit_entities[[kind]]$table)[1]
  found <- rows[as.character(rows[[id]]) %in% key, , drop = FALSE]
  if (nrow(found) == 1) found
}

# What the Basic Info drop-downs pick once a row was added from one of them: the new row, what it belongs
# to above it (a study's registry, from the row itself), and nothing below it.
# catalog: The live tables the pages pick from, read again after the add.
# kind: `"registry"`, `"study"`, `"questionnaire"` or `"wave"`.
# key: The new row's key, as text (`edit_service_quick_add()`).
# before: What the drop-downs had picked before the add: `registry_id`, `study_id`, `questionnaire_id`
#   and `wave_id`.
# Returns: The picks, like `before`; `NULL` if the new row isn't in the catalog (it couldn't be read again), so
#   there is nothing to pick it from.
edit_service_quick_picks <- function(catalog, kind, key, before) {
  added <- if (length(key) == 1 && !is.na(key)) edit_service_quick_row(catalog, kind, key)
  if (is.null(added)) {
    return(NULL)
  }
  utils::modifyList(before, switch(kind,
    registry = list(registry_id = key, study_id = "", questionnaire_id = "", wave_id = ""),
    study = list(registry_id = key_text(added$registry_id), study_id = key, questionnaire_id = "", wave_id = ""),
    # a questionnaire's registry is the one its study is in; the drop-down finds it from the study
    questionnaire = list(registry_id = "", study_id = key_text(added$study_id), questionnaire_id = key, wave_id = ""),
    wave = list(questionnaire_id = key_text(added$questionnaire_id), wave_id = key)
  ))
}
