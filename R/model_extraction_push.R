# ------ What a push has to match against -----------------------------------------------
# Pushing an extraction adds rows to the MATR data. Some of what it needs may be there already:
# an answer choice is kept once for the whole registry (answer_option.standardized_label is
# unique), a set of choices is kept once (option_set), and the new wave of a questionnaire that
# is already recorded finds the questionnaire's blocks, questions and variables in place. These
# read what is there, so extraction_service_build_items() (service_extraction_push.R) can reuse it.
# `connection` is the MATR database (model_database.R).

# How many ids go in one request's filter, so the address stays short.
push_chunk_size <- 150

#' The answer choices and option sets in the database.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of data frames: `answer_options` (`answer_option_id`, `standardized_label`),
#'   `option_sets` (`option_set_id`, `name`) and `members` (`option_set_id`, `answer_option_id`).
push_model_options <- function(connection) {
  list(
    answer_options = database_model_get(
      connection, "answer_option", c("answer_option_id", "standardized_label"), order_by = "answer_option_id"
    ),
    option_sets = database_model_get(connection, "option_set", c("option_set_id", "name"), order_by = "option_set_id"),
    members = database_model_get(
      connection, "answer_option_set", c("option_set_id", "answer_option_id"),
      order_by = c("option_set_id", "answer_option_id")
    )
  )
}

#' What a recorded questionnaire has printed already.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param questionnaire_id The questionnaire's id.
#' @return A list of data frames: `placements` (`component_id`, `parent_component_id`,
#'   `display_order`), `components` (`component_id`, `component_type_id`, `text`), `questions`
#'   (`component_id`), `variables` (`variable_key`, `component_id`, `matr_id`, `display_order`) and
#'   `waves` (`name`).
push_model_existing <- function(connection, questionnaire_id) {
  here <- list(questionnaire_id = database_eq(questionnaire_id))
  placements <- database_model_get(
    connection, "questionnaire_component", c("component_id", "parent_component_id", "display_order"),
    order_by = c("parent_component_id", "display_order"), filters = here
  )
  components <- push_model_by_ids(
    connection, "component", c("component_id", "component_type_id", "text"), "component_id", placements$component_id
  )
  list(
    placements = placements,
    components = components,
    questions = database_model_get(connection, "questionnaire_question", "component_id", order_by = "component_id", filters = here),
    variables = database_model_get(
      connection, "variable", c("variable_key", "component_id", "matr_id", "display_order"),
      order_by = "variable_key", filters = here
    ),
    waves = database_model_get(connection, "wave", "name", order_by = "wave_id", filters = here)
  )
}

# The rows of a table whose `column` is one of `ids`, asked for a few ids at a time.
push_model_by_ids <- function(connection, table, columns, column, ids) {
  ids <- unique(ids[!is.na(ids)])
  if (length(ids) == 0) {
    return(no_rows(columns))
  }
  chunks <- split(ids, ceiling(seq_along(ids) / push_chunk_size))
  rows <- lapply(chunks, function(chunk) {
    database_model_get(
      connection, table, columns, order_by = column,
      filters = stats::setNames(list(database_in(format(chunk, scientific = FALSE, trim = TRUE))), column)
    )
  })
  do.call(rbind, unname(rows))
}
