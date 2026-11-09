# The lists staff pick from on the Edit Extraction page, and the reading job tells Gemini to use.
# The kinds of block (edit_component_kinds, service_edit_entities_content.R) and the kinds of open-ended
# answer are fixed by the database's checks (component.kind, variable.answer_data_type), so they are fixed
# here too; the rest are read.
# A list that can't be read comes back empty, and `problems` says why, so the page can say so
# instead of failing.

# What an open-ended answer can be.
answer_data_types <- c("NUMBER", "TEXT", "DATE", "TIME")

# Returns: A list: `component_types`, `subjects` (who a variable can be about besides the respondent,
#   who is a blank), `time_frames` (when a variable can be about), `units` (what an open-ended number is
#   in), `answer_options` and `answer_data_types` (character vectors), and `problems` (sentences).
lookup_service_get_all <- function(connection) {
  problems <- character()
  rows_of <- function(read, label) {
    tryCatch(read(connection), error = function(problem) {
      problems <<- c(problems, paste0("The list of ", label, " couldn't be read. ", conditionMessage(problem)))
      NULL
    })
  }
  tags <- rows_of(lookup_model_get_tags, "subjects, time frames and units")
  of_kind <- function(kind) as.character(tags$name[tags$kind %in% kind])
  subjects <- of_kind("subject")
  choices <- rows_of(lookup_model_get_answer_options, "answer options")
  list(
    component_types = edit_component_kinds,
    # the respondent is a blank, not a pick
    subjects = subjects[!is.na(blank_respondent(subjects))],
    time_frames = of_kind("time frame"),
    units = of_kind("unit"),
    answer_options = as.character(choices$standardized_label),
    answer_data_types = answer_data_types,
    problems = problems
  )
}
