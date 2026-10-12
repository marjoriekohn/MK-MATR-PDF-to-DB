#' The lists staff pick from on the review page, and the reading job tells Gemini to use.
#'
#' A list that can't be read comes back empty, and `problems` says why (e.g. a lookup
#' table the database doesn't have yet), so the page can say so instead of failing.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list: `component_types`, `subjects`, `answer_options` and
#'   `answer_data_types` (character vectors), and `problems` (sentences).
lookup_service_get_all <- function(connection) {
  problems <- character()
  names_in <- function(read, column, label) {
    tryCatch(
      as.character(read(connection)[[column]]),
      error = function(problem) {
        problems <<- c(problems, paste0("The list of ", label, " couldn't be read. ", conditionMessage(problem)))
        character()
      }
    )
  }
  list(
    component_types = names_in(lookup_model_get_component_types, "name", "component types"),
    subjects = names_in(lookup_model_get_subjects, "name", "subjects"),
    answer_options = names_in(lookup_model_get_answer_options, "standardized_label", "answer options"),
    # BR95: fixed by the schema, not a lookup table
    answer_data_types = c("NUMBER", "TEXT", "DATE", "TIME"),
    problems = problems
  )
}
