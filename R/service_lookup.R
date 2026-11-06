#' The lists staff pick from on the Edit Extraction page, and the reading job tells Gemini to use.
#'
#' A list that can't be read comes back empty, and `problems` says why (e.g. a lookup
#' table the database doesn't have yet), so the page can say so instead of failing.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list: `component_types`, `subjects` (who a variable can be about besides the respondent,
#'   who is a blank), `time_frames` (when a variable can be about), `answer_options` and `answer_data_types` (character vectors), `answer_option_aliases`
#'   (a data frame of `alias` and `label`: the wordings merged into a choice, and its words), and `problems`
#'   (sentences).
lookup_service_get_all <- function(connection) {
  problems <- character()
  rows_of <- function(read, label) {
    tryCatch(read(connection), error = function(problem) {
      problems <<- c(problems, paste0("The list of ", label, " couldn't be read. ", conditionMessage(problem)))
      NULL
    })
  }
  names_in <- function(read, column, label) as.character(rows_of(read, label)[[column]])
  component_types <- names_in(lookup_model_get_component_types, "name", "component types")
  subjects <- names_in(lookup_model_get_subjects, "name", "subjects")
  time_frames <- names_in(lookup_model_get_time_frames, "name", "time frames")
  choices <- rows_of(lookup_model_get_answer_options, "answer options")
  aliases <- rows_of(lookup_model_get_answer_option_aliases, "merged answer choices")
  list(
    component_types = component_types,
    # the respondent is a blank, not a pick
    subjects = subjects[!is.na(subjects) & !is.na(blank_respondent(subjects))],
    time_frames = time_frames,
    answer_options = as.character(choices$standardized_label),
    answer_option_aliases = data.frame(
      alias = as.character(aliases$alias),
      label = as.character(choices$standardized_label[match(aliases$answer_option_id, choices$answer_option_id)])
    ),
    # BR95: fixed by the schema, not a lookup table
    answer_data_types = c("NUMBER", "TEXT", "DATE", "TIME"),
    problems = problems
  )
}
