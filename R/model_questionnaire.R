# Questionnaires and the tables around them: the registries that hold everything
# and the studies in them, who leads each study, each questionnaire's respondent type,
# and its waves. `connection` is the CARA database (model_database.R).

# Every registry.
# The outermost container; every study rolls up to a registry.
# Returns: A data frame with `registry_id`, `name` and `abbreviation`, by name.
registry_model_get_all <- function(connection) {
  database_model_get(connection, "registry", c("registry_id", "name", "abbreviation"), order_by = "name")
}

# The questions one questionnaire's waves ask.
# Returns: A data frame with `wave_id` and `component_id`, one row per question a wave asks.
wave_question_model_get <- function(connection, questionnaire_id) {
  database_model_get(
    connection, "wave_question", c("wave_id", "placement_id"),
    order_by = c("wave_id", "placement_id"), filters = list(questionnaire_id = database_eq(questionnaire_id))
  )
}

# Every study, with its lead investigator's name, its questionnaires and their waves, in one request.
# Every questionnaire is in a study and every wave in a questionnaire, so this is all of each, without a
# request for each table.
# Returns: A list of data frames: `studies` (`study_id`, `registry_id`, `principal_investigator_id`, `name` and
#   `hidden`, by name), `investigators` (`principal_investigator_id` and `name`, only those who lead a study),
#   `questionnaires` (`questionnaire_id`, `study_id`, `respondent_type_id`, `name`, `abbreviation` and `hidden`)
#   and `waves` (`wave_id`, `questionnaire_id`, `name`, `administered_date`, `respondent_count` and `hidden`),
#   each by id. `hidden` is whether it is kept off the dashboard.
study_model_get_tree <- function(connection) {
  columns <- list(
    studies = c("study_id", "registry_id", "principal_investigator_id", "name", "hidden"),
    questionnaires = c("questionnaire_id", "study_id", "respondent_type_id", "name", "abbreviation", "hidden"),
    waves = c("wave_id", "questionnaire_id", "name", "administered_date", "respondent_count", "hidden")
  )
  select <- sprintf(
    # a study names two investigators (who leads it, and who to contact), so its foreign key says which
    # (sql/2_references.sql names it; the column alone is no hint, as the investigator's own is called the same)
    "%s,investigator:principal_investigator!study_principal_investigator_id_fkey(name),questionnaire(%s,wave(%s))",
    paste(columns$studies, collapse = ","), paste(columns$questionnaires, collapse = ","), paste(columns$waves, collapse = ",")
  )
  studies <- database_model_select(connection, "study", select, order_by = "name")
  questionnaires <- unlist(lapply(studies, function(study) study$questionnaire), recursive = FALSE)
  waves <- unlist(lapply(questionnaires, function(questionnaire) questionnaire$wave), recursive = FALSE)
  led <- Filter(function(study) !is.null(study$principal_investigator_id), studies)
  investigators <- unique(data_frame_of(
    lapply(led, function(study) list(principal_investigator_id = study$principal_investigator_id, name = study$investigator$name)),
    c("principal_investigator_id", "name")
  ))
  questionnaires <- data_frame_of(questionnaires, columns$questionnaires)
  waves <- data_frame_of(waves, columns$waves)
  in_order <- function(rows, by) {
    rows <- rows[order(rows[[by]]), , drop = FALSE]
    rownames(rows) <- NULL
    rows
  }
  list(
    studies = data_frame_of(studies, columns$studies),
    investigators = in_order(investigators, "principal_investigator_id"),
    questionnaires = in_order(questionnaires, "questionnaire_id"),
    waves = in_order(waves, "wave_id")
  )
}

# Rows read as lists (database_model_select()) as a data frame of `columns`, each as database_model_get() would
# give it: a missing value is NA, and a column of nothing but missing values is logical.
data_frame_of <- function(rows, columns) {
  if (length(rows) == 0) {
    return(no_rows(columns))
  }
  values <- lapply(stats::setNames(columns, columns), function(column) unlist(lapply(rows, function(row) row[[column]] %||% NA)))
  as.data.frame(values, stringsAsFactors = FALSE)
}

# Every respondent type.
# Returns: A data frame with `respondent_type_id`, `name`, `minimum_age` and `maximum_age`, by name.
respondent_type_model_get_all <- function(connection) {
  database_model_get(
    connection, "respondent_type",
    c("respondent_type_id", "name", "minimum_age", "maximum_age"),
    order_by = "name"
  )
}

# ------ The wordings that aren't questions ----------------------------------------------------
# Every instruction, definition, note and the like the catalog keeps (not questions, and not sections, grids or
# groups): a few dozen at most, read whole for the Push dialog to say which of a draft's are there already.
# Returns: A data frame with `kind` and `text`; no rows if there are none.
component_model_other_blocks <- function(connection) {
  not_these <- paste0("not.", database_in(c("Question", container_types)))
  rows <- database_model_get(connection, "component", c("kind", "text"), order_by = c("kind", "text"), filters = list(kind = not_these))
  if (nrow(rows) == 0) no_rows(c("kind", "text")) else rows
}

# ------ Where components are printed --------------------------------------------------------
# Some components as the catalog has them, and every place that prints one of them.
# component_ids: Component ids; anything that isn't a number is left out.
# Returns: A list: `components` (`component_id`, `kind`, `text`) and `places` (`component_id` and the
#   `questionnaire_id` of each placement, one row for each place), ids as text.
component_model_places <- function(connection, component_ids) {
  ids <- unique(as.character(component_ids))
  ids <- ids[!is.na(ids) & grepl("^[0-9]+$", ids)]
  read <- function(table, columns) {
    # a few at a time: the ids go in the request's address
    parts <- lapply(split(ids, ceiling(seq_along(ids) / 150)), function(some) {
      database_model_get(connection, table, columns, order_by = columns, filters = list(component_id = database_in(some)))
    })
    rows <- if (length(parts) > 0) do.call(rbind, parts) else no_rows(columns)
    rows$component_id <- as.character(rows$component_id)
    rows
  }
  places <- read("placement", c("component_id", "questionnaire_id"))
  places$questionnaire_id <- as.character(places$questionnaire_id)
  list(components = read("component", c("component_id", "kind", "text")), places = places)
}
