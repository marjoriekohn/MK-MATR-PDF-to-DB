# ------ What an extraction has to match against -----------------------------------------------
# An extraction adds rows to the MATR metadata database. Some of what it needs may be there already:
# an answer choice is kept once for the whole registry (answer_option.standardized_label is
# unique), a set of choices is kept once (option_set), a component is kept once for each kind and words and
# placed wherever a questionnaire prints it, and a wave of a questionnaire that
# is already recorded finds the questionnaire's blocks, questions, variables and waves in place. These
# read what is there, so extraction_service_build_items() (service_extraction_push.R) can reuse it.
# `connection` is the MATR database (model_database.R).

# How many ids go in one request's filter, so the address stays short.
push_chunk_size <- 150

#' The answer choices and option sets in the database.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of data frames: `answer_options` (`answer_option_id`, `standardized_label`), `aliases`
#'   (`alias`, `answer_option_id`: wordings merged into a choice), `option_sets` (`option_set_id`, `name`),
#'   `members` (`option_set_id`, `answer_option_id`), `stems` (`question_stem_id`, `text`) and `linked`
#'   (`component_id`, `question_stem_id`: the questions that have a stem).
push_model_options <- function(connection) {
  list(
    answer_options = database_model_get(
      connection, "answer_option", c("answer_option_id", "standardized_label"), order_by = "answer_option_id"
    ),
    aliases = database_model_get(connection, "answer_option_alias", c("alias", "answer_option_id"), order_by = "alias"),
    stems = database_model_get(connection, "question_stem", c("question_stem_id", "text"), order_by = "question_stem_id"),
    linked = database_model_get(
      connection, "question", c("component_id", "question_stem_id"),
      order_by = "component_id", filters = list(question_stem_id = "not.is.null")
    ),
    option_sets = database_model_get(connection, "option_set", c("option_set_id", "name"), order_by = "option_set_id"),
    members = database_model_get(
      connection, "answer_option_set", c("option_set_id", "answer_option_id"),
      order_by = c("option_set_id", "answer_option_id")
    )
  )
}

#' The components there are: what a block can be, since there is one for each kind and words (and one Group,
#' which has none).
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A data frame: `component_id`, `component_type_id`, `text`.
push_model_components <- function(connection) {
  database_model_get(connection, "component", c("component_id", "component_type_id", "text"), order_by = "component_id")
}

#' What a recorded questionnaire has printed already.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param questionnaire_id The questionnaire's id.
#' @return A list of data frames: `placements` (`placement_id`, `component_id`, `parent_placement_id`,
#'   `display_order`), `components` (`component_id`, `component_type_id`, `text`), `questions`
#'   (`placement_id`), `variables` (`variable_key`, `placement_id`, `matr_id`, `display_order`),
#'   `waves` (`wave_id`, `name`) and `wave_questions` (`wave_id`, `placement_id`: what each wave asks).
push_model_existing <- function(connection, questionnaire_id) {
  here <- list(questionnaire_id = database_eq(questionnaire_id))
  placements <- database_model_get(
    connection, "questionnaire_component", c("placement_id", "component_id", "parent_placement_id", "display_order"),
    order_by = c("parent_placement_id", "display_order"), filters = here
  )
  components <- push_model_by_ids(
    connection, "component", c("component_id", "component_type_id", "text"), "component_id", placements$component_id
  )
  # A question is placed before it is a question, and a variable and a wave's question are a question's
  # (the foreign keys say so), so a questionnaire with nothing placed, a new one, has none to ask for.
  questions <- if (nrow(placements) > 0) {
    database_model_get(connection, "questionnaire_question", "placement_id", order_by = "placement_id", filters = here)
  } else {
    no_rows("placement_id")
  }
  asked <- nrow(questions) > 0
  list(
    placements = placements,
    components = components,
    questions = questions,
    variables = if (asked) {
      database_model_get(
        connection, "variable", c("variable_key", "placement_id", "matr_id", "display_order"),
        order_by = "variable_key", filters = here
      )
    } else {
      no_rows(c("variable_key", "placement_id", "matr_id", "display_order"))
    },
    waves = database_model_get(connection, "wave", c("wave_id", "name"), order_by = "wave_id", filters = here),
    wave_questions = if (asked) wave_question_model_get(connection, questionnaire_id) else no_rows(c("wave_id", "placement_id"))
  )
}

#' Which questionnaires print each component, and what each questionnaire is called: to say where a block another
#' questionnaire prints already is used.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of data frames: `placed` (`component_id`, `questionnaire_id`) and `questionnaires`
#'   (`questionnaire_id`, `name`, `abbreviation`).
push_model_placed <- function(connection) {
  list(
    placed = database_model_get(
      connection, "questionnaire_component", c("component_id", "questionnaire_id"), order_by = c("component_id", "questionnaire_id")
    ),
    questionnaires = database_model_get(connection, "questionnaire", c("questionnaire_id", "name", "abbreviation"), order_by = "questionnaire_id")
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
