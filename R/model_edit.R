# ------ Applying a change ----------------------------------------------------------------
# A change made on Make an edit is a list of items, one per field of one row (service_edit.R), applied by the
# database's apply_change() in one transaction and written to HISTORY as one row (sql/4_functions.sql).
# Emptying a wave, questionnaire, study or registry is the database's empty_*() functions: the thing stays, what
# it holds goes, and the history gets a row.

edit_item_columns <- c(
  "item_number", "entity_name", "record_identifier", "action",
  "attribute_name", "old_value", "new_value"
)

# Apply a change now.
# items: A data frame of items, numbered, in the order to apply them (`edit_number_items()`). A row an edit adds
#   is named `new:<table>:<n>`; the database swaps in the number it gives it.
# summary: The change in a sentence, for the history.
# me: Who makes it: `list(staff_id, name, role)`.
# Returns: A list: `history_id` (the change's row in the history) and `new_ids` (the number the database gave
#   each new row, by its token). An error with the database's own sentence if it refused (a row changed since the
#   form was opened, a rule of the database).
edit_model_apply <- function(connection, items, summary, me) {
  result <- database_model_call(connection, "apply_change", list(
    p_staff_id = me$staff_id, p_items = items[edit_item_columns], p_summary = summary
  ))
  list(history_id = as.character(result$history_id), new_ids = lapply(result$new_ids %||% list(), as.numeric))
}

# The things that can be emptied, and the table each one's contents are counted from (`edit_model_contents()`).
edit_emptiable <- c("registry", "study", "questionnaire", "wave")

# Empty a wave, questionnaire, study or registry: it stays, its contents go.
# what: One of `edit_emptiable`.
# id: The row's id.
# Returns: What was removed, as counts: `questions_removed`, `placements_removed`, `variables_removed`.
edit_model_empty <- function(connection, what, id, me) {
  if (!what %in% edit_emptiable) stop("Only a registry, study, questionnaire or wave can be emptied.", call. = FALSE)
  arguments <- stats::setNames(list(as.numeric(id), me$staff_id), c(paste0("p_", what, "_id"), "p_staff_id"))
  lapply(database_model_call(connection, paste0("empty_", what), arguments), as.numeric)
}

# What emptying would remove, counted from the database: the questions a wave asks, or the blocks and variables
# of a questionnaire (or of every questionnaire of a study or registry).
# Returns: A list: `questions` (wave_question rows), `placements` and `variables`; a wave counts only its questions.
edit_model_contents <- function(connection, what, id) {
  if (what == "wave") {
    return(list(questions = database_model_count(connection, "wave_question", list(wave_id = database_eq(id)))))
  }
  questionnaires <- switch(what,
    questionnaire = as.numeric(id),
    study = database_model_get(connection, "questionnaire", "questionnaire_id", filters = list(study_id = database_eq(id)))$questionnaire_id,
    registry = {
      studies <- database_model_get(connection, "study", "study_id", filters = list(registry_id = database_eq(id)))$study_id
      if (length(studies) == 0) numeric() else database_model_get(connection, "questionnaire", "questionnaire_id", filters = list(study_id = database_in(studies)))$questionnaire_id
    }
  )
  if (length(questionnaires) == 0) {
    return(list(questions = 0L, placements = 0L, variables = 0L))
  }
  within <- list(questionnaire_id = database_in(questionnaires))
  list(
    questions = database_model_count(connection, "wave_question", within),
    placements = database_model_count(connection, "placement", within),
    variables = database_model_count(connection, "variable", within)
  )
}
