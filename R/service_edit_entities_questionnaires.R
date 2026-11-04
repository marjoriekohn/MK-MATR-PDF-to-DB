# Make an edit's kinds in the group Questionnaires: respondent types, questionnaires and their waves.
# The kinds are listed together in edit_entities (service_edit_kinds.R).

# ------ Questionnaires ------------------------------------------------------------------------
respondent_type_entity <- list(
  label = "Respondent Type", noun = "respondent type", plural = "respondent types", article = "a",
  group = "Questionnaires", table = "respondent_type",
  about = "Who a questionnaire is for, bounded by age: Adult, Child, Parent.",
  delete_about = paste0(
    "A respondent type can only be deleted once no questionnaire is for them. Move those ",
    "questionnaires to another respondent type first."
  ),
  cards = function(snap, scope) {
    rows <- snap$respondent_type
    n_q <- count_in(snap$questionnaire$respondent_type_id, rows$respondent_type_id)
    edit_cards(
      key_text(rows$respondent_type_id),
      paste0(rows$name, " (", edit_age_range(rows$minimum_age, rows$maximum_age), ")"),
      subtitle = ifelse(is.na(rows$description), "", rows$description),
      facts = lapply(n_q, function(n) edit_count(n, "questionnaire"))
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("name", "Name", required = TRUE, placeholder = "e.g. Adult"),
      edit_field("minimum_age", "Youngest age", "number",
        min = 0, placeholder = "e.g. 18", hint = "Leave both ages empty if it doesn't depend on age."
      ),
      edit_field("maximum_age", "Oldest age", "number",
        min = 0, placeholder = "e.g. 64",
        problem = function(value, values) {
          oldest <- edit_whole_number(value)
          youngest <- edit_whole_number(values$minimum_age)
          if (!is.na(oldest) && !is.na(youngest) && oldest < youngest) {
            "The oldest age can't be younger than the youngest age."
          }
        }
      ),
      edit_field("description", "Description", "textarea")
    )
  },
  duplicates = function(snap, values) {
    # only when an age is filled in: two respondent types with no ages are told apart by their names
    youngest <- edit_whole_number(values$minimum_age)
    oldest <- edit_whole_number(values$maximum_age)
    if (is.na(youngest) && is.na(oldest)) {
      return(list())
    }
    rows <- snap$respondent_type
    same_age <- function(age, typed) (is.na(age) & is.na(typed)) | (!is.na(age) & !is.na(typed) & age == typed)
    same <- rows[same_age(rows$minimum_age, youngest) & same_age(rows$maximum_age, oldest), , drop = FALSE]
    if (nrow(same) > 0) list(list(rows = same, why = "A respondent type with the same age range")) else list()
  }
)

questionnaire_entity <- list(
  label = "Questionnaire", noun = "questionnaire", plural = "questionnaires", article = "a",
  group = "Questionnaires", table = "questionnaire",
  about = paste0(
    "What stays true across every time a survey is given: its name, its respondent type and the study it ",
    "belongs to. Each time it's given is a wave."
  ),
  delete_about = paste0(
    "Deleting a questionnaire deletes its waves and everything printed in it: every placement of a ",
    "component, every question's settings and numbers, and every variable with its choices. The ",
    "components themselves stay, because other questionnaires may use them."
  ),
  cards = function(snap, scope) {
    rows <- snap$questionnaire
    n_waves <- count_in(snap$wave$questionnaire_id, rows$questionnaire_id)
    n_questions <- count_in(snap$questionnaire_question$questionnaire_id, rows$questionnaire_id)
    study <- edit_lookup(snap, "study", "study_id", rows$study_id, "name")
    who <- edit_lookup(snap, "respondent_type", "respondent_type_id", rows$respondent_type_id, "name")
    edit_cards(
      key_text(rows$questionnaire_id), edit_label(snap, "questionnaire", rows),
      subtitle = paste0(
        study, " · for ", tolower(who),
        ifelse(is.na(rows$participant_count), "", paste0(" · ", format(rows$participant_count, big.mark = ",", trim = TRUE), " participants"))
      ),
      facts = Map(function(w, q) c(edit_count(w, "wave"), edit_count(q, "question")), n_waves, n_questions)
    )
  },
  fields = function(snap, mode, row, scope) {
    study_taken <- function(column, what) {
      function(value, values) {
        if (!has_text(value) || !has_text(values$study_id)) {
          return(NULL)
        }
        same <- edit_taken(snap, "questionnaire", column, value, row, within = list(study_id = as.numeric(values$study_id)))
        if (nrow(same) > 0) {
          sprintf("This study already has a questionnaire with %s “%s”.", what, same[[column]][1])
        }
      }
    }
    list(
      edit_field("study_id", "Study", "select",
        required = TRUE, choices = edit_choices(snap, "study", blank = "Choose a study"),
        hint = if (mode == "update") "Changing this moves the questionnaire, with all its waves and answers, to another study."
      ),
      edit_field("respondent_type_id", "Respondent Type", "select",
        required = TRUE, short = "respondent type",
        choices = edit_choices(snap, "respondent_type", blank = "Choose a respondent type")
      ),
      edit_field(
        "name",
        "Name",
        required = TRUE,
        placeholder = "e.g. Adult Health and Personality",
        problem = study_taken("name", "the name")
      ),
      edit_field(
        "abbreviation",
        "Abbreviation",
        required = TRUE,
        placeholder = "e.g. AHP",
        problem = study_taken("abbreviation", "the abbreviation")
      ),
      edit_field("participant_count", "Total participants", "number",
        min = 0, placeholder = "e.g. 1,200",
        hint = "Across all its waves, for when each wave's count isn't known. The waves' counts can't add up to more.",
        problem = function(value, values) {
          total <- edit_whole_number(value)
          waves <- if (!is.null(row)) edit_wave_total(snap, row$questionnaire_id) else 0
          if (!is.na(total) && waves > total) {
            sprintf("Its waves have %s respondents between them, more than this.", format(waves, big.mark = ","))
          }
        }
      ),
      edit_field("description", "Description", "textarea")
    )
  },
  duplicates = function(snap, values) {
    study <- if (has_text(values$study_id)) list(study_id = as.numeric(values$study_id))
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "questionnaire", "name", values$name, "A questionnaire with this name in this study", within = study),
      edit_duplicate_rows(snap, "questionnaire", "abbreviation", values$abbreviation,
        "A questionnaire with this abbreviation in this study",
        within = study
      )
    ))
  }
)

wave_entity <- list(
  label = "Wave", noun = "wave", plural = "waves", article = "a",
  group = "Questionnaires", table = "wave",
  about = "One time a questionnaire was given. It records when, and how many people answered.",
  delete_about = paste0(
    "Deleting a wave deletes its record of which questions were asked and the number each had. A ",
    "question asked in no other wave can't stay in the questionnaire, so it goes too, with its ",
    "variables."
  ),
  cards = function(snap, scope) {
    rows <- snap$wave
    n_asked <- count_in(snap$wave_question$wave_id, rows$wave_id)
    edit_cards(
      key_text(rows$wave_id), edit_label(snap, "wave", rows),
      subtitle = paste0(
        ifelse(is.na(rows$administered_date), "date not recorded", paste("given", edit_date_text(rows$administered_date))),
        ifelse(is.na(rows$respondent_count), "", paste0(" · ", format(rows$respondent_count, big.mark = ",", trim = TRUE), " respondents"))
      ),
      facts = lapply(n_asked, function(n) edit_count(n, "question") |> paste("asked"))
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("questionnaire_id", "Questionnaire", "select",
        required = TRUE,
        choices = edit_choices(snap, "questionnaire", blank = "Choose a questionnaire"),
        locked = "A wave can't be moved to another questionnaire."
      ),
      edit_field("name", "Wave name",
        required = TRUE, placeholder = "e.g. Wave 3",
        problem = function(value, values) {
          if (!has_text(value) || !has_text(values$questionnaire_id)) {
            return(NULL)
          }
          same <- edit_taken(snap, "wave", "name", value, row, within = list(questionnaire_id = as.numeric(values$questionnaire_id)))
          if (nrow(same) > 0) {
            sprintf(
              "%s already has a %s. Pick another name.",
              edit_questionnaire_short(snap, as.numeric(values$questionnaire_id)),
              same$name[1]
            )
          }
        }
      ),
      edit_field("administered_date", "Date administered", "date", placeholder = "MM/DD/YYYY"),
      edit_field("respondent_count", "Number of respondents", "number",
        min = 0, placeholder = "e.g. 1,200",
        problem = function(value, values) {
          count <- edit_whole_number(value)
          q <- suppressWarnings(as.numeric(values$questionnaire_id))
          total <- snap$questionnaire$participant_count[match(q, snap$questionnaire$questionnaire_id)]
          if (is.na(count) || length(total) != 1 || is.na(total)) {
            return(NULL)
          }
          others <- edit_wave_total(snap, q, except = row$wave_id)
          if (others + count > total) {
            sprintf(
              "%s has %s participants in all, and its other waves have %s, so this wave can have at most %s.",
              edit_questionnaire_short(snap, q), format(total, big.mark = ","), format(others, big.mark = ","),
              format(max(total - others, 0), big.mark = ",")
            )
          }
        }
      )
    )
  }
)

# How many respondents a questionnaire's waves have between them, where known (leaving out the wave `except`).
edit_wave_total <- function(snap, questionnaire_id, except = NULL) {
  waves <- snap$wave[snap$wave$questionnaire_id %in% questionnaire_id & !snap$wave$wave_id %in% except, , drop = FALSE]
  sum(waves$respondent_count, na.rm = TRUE)
}
