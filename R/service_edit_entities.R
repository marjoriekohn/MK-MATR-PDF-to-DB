# ------ The kinds of thing staff can change ------------------------------------------------
# One entry per kind of thing in the "I want to ... a ..." sentence. The edit
# page is built from these: the drop-down, the cards staff pick from, the form
# and what each change reaches. An entry says
#   label, noun, article, plural, group   how the kind is named
#   about                                 what it is, shown under the sentence
#   table                                 the table whose rows it changes
#   scope                                 the questionnaire a kind that belongs to
#                                         just one must be asked about first
#   cards(snap, scope)                    one card per row, with counts
#   fields(snap, mode, row, scope)        the form, for "add" and "update"
#   parts                                 an add that needs more rows: see below (none does today)
#   delete_start(snap, row)               what a delete starts from, if not the row
#   delete_about, update_about            what staff should know before they do it
#   build(mode, ...)                      writes a change as items, if it's not
#                                         just the fields that differ
#   summary_add(snap, scope, fields, values)  an add's sentence, if not "Added the ... “words”"
#   can(snap, mode)                       a sentence if the kind can't be added,
#                                         updated or deleted here
#   duplicates(snap, values)              the rows an add looks like, if it looks like any: a
#                                         list of list(rows, why). A warning, not a block
# `snap` is a list of whole tables (edit_service_load()). Anything an entry
# doesn't say, edit_service_*() does the plain way.
#
# `parts`: an add that has to add other rows with it, in one edit. Each part is a kind,
# and `link` the column that holds the part before it. Adding a study or a
# questionnaire adds only that row: its questionnaires and waves are added
# afterwards, by themselves or with an extraction.

# ------ Building blocks ---------------------------------------------------------------------
#' A count of rows with its noun, thousands separated.
#'
#' For example "1 study", "3 studies", "1,200 waves".
#'
#' @param n The count.
#' @param singular The noun for one.
#' @param plural_form The noun for any other count; defaults to adding an "s".
#' @return A string; see `plural()`.
edit_count <- function(n, singular, plural_form = paste0(singular, "s")) {
  plural(n, singular, plural_form = plural_form, big_mark = ",")
}

# For each of `ids`, how many of `values` are it.
count_in <- function(values, ids) tabulate(match(values, ids), nbins = length(ids))

# How many characters of a component's words a card, or a line of a questionnaire's
# outline, shows before it is cut short.
edit_card_title_chars <- 140
edit_outline_title_chars <- 120

# A table of cards: what each says, and what staff can type to find it.
edit_cards <- function(key, title, subtitle = "", facts = NULL, tag = NA_character_, warn = NA_character_) {
  n <- length(key)
  if (n == 0) {
    cards <- data.frame(
      key = character(),
      title = character(),
      subtitle = character(),
      tag = character(),
      warn = character(),
      search = character()
    )
    cards$facts <- list()
    return(cards)
  }
  cards <- data.frame(
    key = key, title = title, subtitle = rep_len(subtitle, n), tag = rep_len(tag, n), warn = rep_len(warn, n),
    stringsAsFactors = FALSE
  )
  cards$facts <- facts %||% rep(list(character()), n)
  cards$search <- tolower(paste(
    cards$title, cards$subtitle, ifelse(is.na(cards$tag), "", cards$tag),
    vapply(cards$facts, paste, character(1), collapse = " ")
  ))
  cards
}

# "18+", "5-17", "up to 17" or "any age" (see age_range_text()).
edit_age_range <- function(minimum, maximum) age_range_text(minimum, maximum)

#' The choices of a drop-down of rows: shown by name, saved as the row's ID.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param table The table the rows are from.
#' @param rows The rows to offer; all of the table's by default.
#' @param blank The text of a leading choice with the value `""`, or `NULL` for none.
#' @return A named character vector: the names are shown, and the values, the ids, are saved.
edit_choices <- function(snap, table, rows = snap[[table]], blank = NULL) {
  # (with no rows, text pasted around nothing would still make one label)
  labels <- if (nrow(rows) == 0) {
    character()
  } else if (!is.null(edit_choice_labels[[table]])) {
    edit_choice_labels[[table]](snap, rows)
  } else {
    edit_label(snap, table, rows)
  }
  ids <- key_text(rows[[edit_table_key(table)[1]]])
  choices <- stats::setNames(ids, labels)
  if (!is.null(blank)) choices <- c(stats::setNames("", blank), choices)
  choices
}

edit_choice_labels <- list(
  registry = function(snap, rows) paste0(rows$name, " (", rows$abbreviation, ")"),
  respondent_type = function(snap, rows) paste0(rows$name, " (", edit_age_range(rows$minimum_age, rows$maximum_age), ")"),
  study = function(snap, rows) {
    paste0(rows$name, " · ", edit_lookup(snap, "registry", "registry_id", rows$registry_id, "abbreviation"))
  },
  questionnaire = function(snap, rows) {
    paste0(
      ifelse(is.na(rows$abbreviation) | !nzchar(rows$abbreviation), rows$name, paste0(rows$name, " (", rows$abbreviation, ")")),
      " · ", edit_lookup(snap, "study", "study_id", rows$study_id, "name")
    )
  }
)

# How a form's field is described. `type` is text, textarea, number, date,
# select (`choices`), choice (side-by-side buttons), or variables (a question's
# variables and their choices, as JSON, drawn by www/js/variables.js from what
# `options` lists). `locked` is the reason a field can't change once the row exists.
# `show_when` is list(field, value): the field only matters while another has
# that value. `problem` and `warn` are functions of (value, values), where
# `values` is everything typed in the same part of the form by field name,
# giving a sentence, or NULL: a problem stops the edit, a warning only says.
# `default` is what an added row starts with. `tooltip` is shown when the label is hovered or focused.
edit_field <- function(name, label, type = "text", required = FALSE, choices = NULL, hint = NULL,
                       placeholder = NULL, locked = NULL, min = NULL, rows = NULL, show_when = NULL,
                       problem = NULL, warn = NULL, suggest = NULL, default = NULL, short = NULL,
                       tooltip = NULL, options = NULL) {
  list(
    name = name, label = label, type = type, required = required, choices = choices, hint = hint,
    placeholder = placeholder, locked = locked, min = min, rows = rows, show_when = show_when,
    problem = problem, warn = warn, suggest = suggest, default = default, tooltip = tooltip, options = options,
    short = short %||% tolower_first(label) # what a sentence calls it: "changed the ..."
  )
}

# The words already used in a column, for a text field to offer as it's typed.
edit_suggestions <- function(values) {
  values <- unique(values[!is.na(values) & nzchar(values)])
  sort(values)
}

# Words as they are compared: capitals and extra spaces (at either end, or doubled inside) don't count.
edit_same_words <- function(text) tolower(gsub("[[:space:]]+", " ", trimws(text)))

# Another row of `table` already has `value` in `column` (ignoring case and
# extra spaces), apart from the row being changed.
edit_taken <- function(snap, table, column, value, row = NULL, within = NULL) {
  rows <- snap[[table]]
  if (!is.null(row)) rows <- rows[edit_key(rows, edit_table_key(table)) != edit_key(row, edit_table_key(table)), , drop = FALSE]
  if (!is.null(within)) rows <- rows[rows[[names(within)]] %in% within[[1]], , drop = FALSE]
  same <- edit_same_words(rows[[column]]) == edit_same_words(value)
  rows[which(same), , drop = FALSE]
}

# ------ Registries and studies ---------------------------------------------------------------
registry_entity <- list(
  label = "Registry", noun = "registry", plural = "registries", article = "a",
  group = "Registries and studies", table = "registry",
  about = "The outermost container: every study, questionnaire and variable belongs to one registry.",
  delete_about = paste0(
    "A registry holds studies, and they hold everything else. Deleting it deletes every study in ",
    "it, with all their questionnaires, waves and variables."
  ),
  cards = function(snap, scope) {
    rows <- snap$registry
    n_studies <- count_in(snap$study$registry_id, rows$registry_id)
    edit_cards(
      key_text(rows$registry_id), rows$name,
      subtitle = paste0(rows$abbreviation, " · ", format(rows$participant_count, big.mark = ",", trim = TRUE), " participants"),
      facts = lapply(n_studies, function(n) edit_count(n, "study", "studies"))
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("name", "Name", required = TRUE, placeholder = "e.g. Mid-Atlantic Twin Registry"),
      edit_field("abbreviation", "Abbreviation", required = TRUE, placeholder = "e.g. MATR"),
      edit_field("participant_count", "Participants", "number",
        required = TRUE, min = 0, placeholder = "e.g. 50,000",
        hint = "How many people are registered."
      ),
      edit_field("description", "Description", "textarea")
    )
  },
  duplicates = function(snap, values) {
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "registry", "name", values$name, "A registry with this name")
    ))
  }
)

investigator_entity <- list(
  label = "Investigator", noun = "investigator", plural = "investigators", article = "an",
  group = "Registries and studies", table = "principal_investigator",
  about = "A researcher who leads a study or is its contact.",
  delete_about = paste0(
    "An investigator can only be deleted once they no longer lead a study. Where they're a study's ",
    "contact, the contact is simply left empty."
  ),
  cards = function(snap, scope) {
    rows <- snap$principal_investigator
    leads <- count_in(snap$study$principal_investigator_id, rows$principal_investigator_id)
    contact <- count_in(snap$study$contact_principal_investigator_id, rows$principal_investigator_id)
    edit_cards(
      key_text(rows$principal_investigator_id), rows$name,
      subtitle = rows$email,
      facts = Map(function(l, c) {
        c(
          if (l > 0) paste("leads", edit_count(l, "study", "studies")),
          if (c > 0) paste("contact for", edit_count(c, "study", "studies")),
          if (l + c == 0) "not on any study"
        )
      }, leads, contact)
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("name", "Name", required = TRUE),
      edit_field("email", "Email",
        required = TRUE, placeholder = "name@vcu.edu",
        problem = function(value, values) {
          if (has_text(value) && !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", trimws(value))) {
            "Type a full email address, like name@vcu.edu."
          }
        }
      )
    )
  },
  duplicates = function(snap, values) {
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "principal_investigator", "name", values$name, "An investigator with this name"),
      edit_duplicate_rows(snap, "principal_investigator", "email", values$email, "An investigator with this email")
    ))
  }
)

# Study status is Active or Ended (study_status_check in the database).
study_statuses <- c("Active" = "Active", "Ended" = "Ended")
study_status_tooltip <- "Active: the study is ongoing. Ended: all data collections are finished."

study_entity <- list(
  label = "Study", noun = "study", plural = "studies", article = "a",
  group = "Registries and studies", table = "study",
  about = "One research project run under a registry.",
  delete_about = paste0(
    "Deleting a study deletes all its questionnaires, with their waves and variables. Registries ",
    "and investigators are not affected."
  ),
  cards = function(snap, scope) {
    rows <- snap$study
    n_q <- count_in(snap$questionnaire$study_id, rows$study_id)
    registry <- edit_lookup(snap, "registry", "registry_id", rows$registry_id, "abbreviation")
    lead <- edit_lookup(snap, "principal_investigator", "principal_investigator_id", rows$principal_investigator_id, "name")
    edit_cards(
      key_text(rows$study_id), rows$name,
      subtitle = paste0(registry, " · led by ", lead, ifelse(is.na(rows$status), "", paste0(" · ", rows$status))),
      facts = lapply(n_q, function(n) edit_count(n, "questionnaire"))
    )
  },
  fields = function(snap, mode, row, scope) {
    investigators <- edit_choices(snap, "principal_investigator", blank = "Choose an investigator")
    list(
      edit_field("registry_id", "Registry", "select",
        required = TRUE,
        choices = edit_choices(snap, "registry", blank = "Choose a registry")
      ),
      edit_field("principal_investigator_id", "Lead investigator", "select",
        required = TRUE, choices = investigators,
        locked = "A study's lead investigator never changes once it's recorded."
      ),
      edit_field("contact_principal_investigator_id", "Contact investigator", "select",
        required = TRUE, choices = investigators,
        hint = "Who to contact about this study."
      ),
      edit_field("name", "Name", required = TRUE, placeholder = "e.g. Adult Health Study"),
      edit_field("status", "Status", "select",
        required = TRUE, choices = c("Choose a status" = "", study_statuses), tooltip = study_status_tooltip
      ),
      edit_field("description", "Description", "textarea"),
      edit_field("methods", "Methods", "textarea"),
      edit_field("acknowledgements", "Acknowledgements", "textarea")
    )
  },
  duplicates = function(snap, values) {
    registry <- if (has_text(values$registry_id)) list(registry_id = as.numeric(values$registry_id))
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "study", "name", values$name, "A study with this name", within = registry)
    ))
  }
)

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
      subtitle = paste0(study, " · for ", tolower(who)),
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
          sprintf(
            "This study already has a questionnaire with %s “%s”. Two with the same %s are hard to tell apart.",
            what,
            trimws(value),
            what
          )
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
        warn = study_taken("name", "the name")
      ),
      edit_field(
        "abbreviation",
        "Abbreviation",
        required = TRUE,
        placeholder = "e.g. AHP",
        warn = study_taken("abbreviation", "the abbreviation")
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
      edit_field("respondent_count", "Number of respondents", "number", min = 0, placeholder = "e.g. 1,200")
    )
  }
)

# ------ Shared lists ---------------------------------------------------------------------------------
component_type_entity <- list(
  label = "Component type", noun = "component type", plural = "component types", article = "a",
  group = "Printed content", table = "component_type",
  about = "The kinds of block a questionnaire prints: Section, Question, Instruction and so on.",
  delete_about = "A component type can only be deleted once no component is of that kind.",
  extra_blockers = function(snap, row) {
    if (row$name %in% edit_core_types) {
      list(list(
        text = sprintf(
          paste0(
            "“%s” is built into this site. Questions and sections are handled differently from every other ",
            "kind of block, and the site finds them by this name."
          ),
          row$name
        ),
        fix = "Keep it.", labels = row$name, table = "component_type"
      ))
    }
  },
  cards = function(snap, scope) {
    rows <- snap$component_type
    n_components <- count_in(snap$component$component_type_id, rows$component_type_id)
    edit_cards(
      key_text(rows$component_type_id), rows$name,
      subtitle = ifelse(is.na(rows$description), "", rows$description),
      facts = lapply(n_components, function(n) edit_count(n, "component")),
      warn = ifelse(rows$name %in% edit_core_types, "Built in: the site relies on this name", NA_character_)
    )
  },
  fields = function(snap, mode, row, scope) {
    core <- mode == "update" && !is.null(row) && row$name %in% edit_core_types
    list(
      edit_field("name", "Name",
        required = TRUE, placeholder = "e.g. Instruction",
        locked = if (core) "This kind is built into the site, which finds it by its name.",
        problem = function(value, values) {
          same <- edit_taken(snap, "component_type", "name", value, row)
          if (has_text(value) && nrow(same) > 0) sprintf("There's already a component type called %s.", same$name[1])
        }
      ),
      edit_field("description", "Description", "textarea")
    )
  }
)

# Kinds of block the site treats specially (BR42, BR45, BR53).
edit_core_types <- c("Section", "Question", "Question Group")

category_entity <- list(
  label = "MeSH category", noun = "MeSH category", plural = "MeSH categories", article = "a",
  group = "Shared lists", table = "category",
  about = "A subject heading from the National Library of Medicine's MeSH list, used to tag questions by topic.",
  delete_about = "Deleting a MeSH category removes it from every question it tags. The questions stay.",
  cards = function(snap, scope) {
    rows <- snap$category
    n_tagged <- count_in(snap$question_category$mesh_id, rows$mesh_id)
    edit_cards(
      rows$mesh_id, rows$term,
      subtitle = paste0("MeSH ", rows$mesh_id),
      facts = lapply(n_tagged, function(n) paste("tags", edit_count(n, "question")))
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("mesh_id", "MeSH ID",
        required = TRUE, placeholder = "e.g. D012907",
        locked = "A MeSH ID is the heading's identity at the National Library of Medicine, so it never changes.",
        hint = "A letter followed by 6 to 9 digits.",
        problem = function(value, values) {
          value <- trimws(value)
          if (!nzchar(value)) {
            return(NULL)
          }
          if (!grepl("^[A-Z][0-9]{6,9}$", value)) {
            return("A MeSH ID is a capital letter and then 6 to 9 digits, like D012907.")
          }
          if (nrow(edit_taken(snap, "category", "mesh_id", value, row)) > 0) "That MeSH ID is already in the list."
        }
      ),
      edit_field("term", "Term", required = TRUE, placeholder = "e.g. Smoking")
    )
  }
)

answer_option_entity <- list(
  label = "Answer choice", noun = "answer choice", plural = "answer choices", article = "an",
  group = "Shared lists", table = "answer_option",
  about = paste0(
    "One answer a question can offer, like “Yes” or “Sometimes”, kept once for the whole registry. ",
    "Variables use it with their own code."
  ),
  delete_about = "An answer choice can only be deleted once no variable offers it and no option set includes it.",
  cards = function(snap, scope) {
    rows <- snap$answer_option
    on_boxes <- count_in(snap$response_option$answer_option_id, rows$answer_option_id)
    in_sets <- count_in(snap$answer_option_set$answer_option_id, rows$answer_option_id)
    edit_cards(
      key_text(rows$answer_option_id), rows$standardized_label,
      subtitle = "",
      facts = Map(function(b, s) {
        c(
          if (b > 0) paste("offered on", edit_count(b, "variable", "variables")) else "on no variables",
          if (s > 0) paste("in", edit_count(s, "option set"))
        )
      }, on_boxes, in_sets)
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("standardized_label", "Answer choice",
        required = TRUE, placeholder = "e.g. Sometimes",
        hint = if (mode == "update") {
          paste0(
            "Every variable that offers this choice shows the new words. Their codes and printed labels ",
            "stay as they are."
          )
        },
        problem = function(value, values) {
          same <- edit_taken(snap, "answer_option", "standardized_label", value, row)
          if (has_text(value) && nrow(same) > 0) sprintf("“%s” is already in the list.", same$standardized_label[1])
        }
      )
    )
  }
)

subject_entity <- list(
  label = "Subject", noun = "subject", plural = "subjects", article = "a",
  group = "Shared lists", table = "subject",
  about = paste0(
    "Who a variable can be about, like “Self” or “Respondent's Family”. The Edit Extraction page offers these, and the ",
    "reading instructions list them for Gemini."
  ),
  delete_about = paste0(
    "A subject can only be deleted once no variable is about it. A draft that still names it is ",
    "told to pick a subject from the list."
  ),
  cards = function(snap, scope) {
    rows <- snap$subject
    about <- count_in(snap$variable$subject_id, rows$subject_id)
    edit_cards(
      key_text(rows$subject_id), rows$name,
      facts = lapply(about, function(n) if (n > 0) paste("about", edit_count(n, "variable", "variables")) else "about no variables")
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("name", "Subject",
        required = TRUE, placeholder = "e.g. Respondent's Parents",
        hint = if (mode == "update") "A draft that still has the old name is told to pick from the list again.",
        problem = function(value, values) {
          same <- edit_taken(snap, "subject", "name", value, row)
          if (has_text(value) && nrow(same) > 0) sprintf("“%s” is already in the list.", same$name[1])
        }
      )
    )
  }
)

# ------ Small helpers the forms use ---------------------------------------------------------------------
#' A whole number typed as text.
#'
#' @param text For example `"1,200"`.
#' @return The number, or `NA` if `text` isn't a whole number.
edit_whole_number <- function(text) {
  text <- gsub(",", "", trimws(text %||% ""), fixed = TRUE)
  if (!grepl("^[0-9]+$", text)) {
    return(NA_real_)
  }
  as.numeric(text)
}

# "Apr 15, 2022" from "2022-04-15".
edit_date_text <- function(dates) {
  parsed <- as.Date(dates)
  ifelse(
    is.na(parsed), "",
    paste0(format(parsed, "%b "), as.integer(format(parsed, "%d")), ", ", format(parsed, "%Y"))
  )
}

# ------ Printed content: shared by every questionnaire that prints it ------------------------------
#' Whether components are questions.
#'
#' A component is kept once and any number of questionnaires place it. A question is a
#' component of the kind Question that also has a question row.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param component_ids Component ids.
#' @return A logical vector.
edit_is_question <- function(snap, component_ids) {
  component_ids %in% snap$question$component_id
}

#' The id of a kind of block.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param name The kind's name, e.g. `"Question"`.
#' @return The `component_type_id`, or `NA` if there isn't exactly one.
edit_type_id <- function(snap, name) {
  id <- snap$component_type$component_type_id[snap$component_type$name == name]
  if (length(id) == 1) id else NA_real_
}

# "seen in 2 questionnaires: AHP, MQ"; "not printed in any questionnaire".
edit_seen_in <- function(snap, component_id) {
  q_ids <- unique(snap$questionnaire_component$questionnaire_id[snap$questionnaire_component$component_id == component_id])
  if (length(q_ids) == 0) {
    return("not printed in any questionnaire")
  }
  names <- edit_questionnaire_short(snap, q_ids)
  paste0("seen in ", edit_count(length(q_ids), "questionnaire"), if (length(q_ids) <= 4) paste0(": ", paste(names, collapse = ", ")))
}

# The warning on a card, when changing it would reach more than one questionnaire.
edit_shared_warning <- function(snap, component_ids) {
  n <- edit_service_questionnaire_count(snap, component_ids)
  ifelse(n >= 2, paste("Shared: changes show in", n, "questionnaires"), NA_character_)
}

# A component's text field. Every kind has words but a question group, which has none of its own (BR42).
edit_text_field <- function(snap, mode, row, label = "Printed words", type_field = TRUE) {
  group <- edit_type_id(snap, "Question Group")
  edit_field(
    "text", label, "textarea",
    required = !type_field, rows = 3, short = if (type_field) "words" else "wording",
    problem = function(value, values) {
      is_group <- identical(as.numeric(values$component_type_id %||% NA), group)
      if (type_field && !has_text(value) && !is_group) {
        return("Add the printed words. Only a question group has none.")
      }
      if (type_field && has_text(value) && is_group) {
        "A question group has no words of its own. Put them in an Instruction inside it."
      }
    },
    warn = function(value, values) {
      if (!has_text(value)) {
        return(NULL)
      }
      same <- edit_taken(snap, "component", "text", value, row)
      if (nrow(same) > 0) {
        paste0(
          "Another component already has exactly these words (", edit_seen_in(snap, same$component_id[1]), "). ",
          "Using that one would keep the wording shared instead of making a copy."
        )
      }
    }
  )
}

component_entity <- list(
  label = "Component", noun = "component", plural = "components", article = "a",
  group = "Printed content", table = "component",
  about = paste0(
    "A heading, instruction, definition or any other block that isn't a question. Each is kept ",
    "once, and every questionnaire that prints it uses that one copy."
  ),
  update_about = "A component is shared. Changing its words changes them in every questionnaire that prints it.",
  delete_about = paste(
    "Deleting a component takes it out of every questionnaire that prints it, and everything inside it if it's a",
    "section or question group."
  ),
  cards = function(snap, scope) {
    rows <- snap$component[!edit_is_question(snap, snap$component$component_id), , drop = FALSE]
    type <- edit_lookup(snap, "component_type", "component_type_id", rows$component_type_id, "name")
    edit_cards(
      key_text(rows$component_id), edit_component_text(snap, rows$component_id, edit_card_title_chars),
      subtitle = "",
      facts = lapply(rows$component_id, function(id) edit_seen_in(snap, id)),
      tag = type, warn = edit_shared_warning(snap, rows$component_id)
    )
  },
  fields = function(snap, mode, row, scope) {
    types <- snap$component_type[snap$component_type$name != "Question", , drop = FALSE]
    list(
      edit_field(
        "component_type_id", "Kind of block", "select",
        required = TRUE,
        choices = if (mode == "add") {
          edit_choices(snap, "component_type", types, blank = "Choose a kind")
        } else {
          edit_choices(snap, "component_type")
        },
        locked = paste0(
          "A component's kind is fixed. To print a different kind of block in just one questionnaire, ",
          "change it there instead (Update, then Questionnaire component)."
        )
      ),
      edit_text_field(snap, mode, row)
    )
  },
  duplicates = function(snap, values) {
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "component", "text", values$text, "A block with exactly these words")
    ))
  }
)

question_entity <- list(
  label = "Question", noun = "question", plural = "questions", article = "a",
  group = "Printed content", table = "component",
  about = paste0(
    "A question's wording, kept once. Every questionnaire that asks it uses that one copy, with its ",
    "own variables and question number."
  ),
  update_about = "A question is shared. Changing its wording changes it in every questionnaire that asks it.",
  delete_about = paste0(
    "Deleting a question takes it out of every questionnaire that asks it. Its variables, their ",
    "choices, its MATR IDs, its question numbers in every wave, and its MeSH tags all go with it."
  ),
  cards = function(snap, scope) {
    rows <- snap$component[edit_is_question(snap, snap$component$component_id), , drop = FALSE]
    boxes <- count_in(snap$variable$component_id, rows$component_id)
    edit_cards(
      key_text(rows$component_id), edit_component_text(snap, rows$component_id, edit_card_title_chars),
      subtitle = "",
      facts = Map(
        function(id, n) c(edit_seen_in(snap, id), if (n > 0) edit_count(n, "variable", "variables")),
        rows$component_id,
        boxes
      ),
      tag = "Question", warn = edit_shared_warning(snap, rows$component_id)
    )
  },
  fields = function(snap, mode, row, scope) {
    list(edit_text_field(snap, mode, row, "Question wording", type_field = FALSE))
  },
  duplicates = function(snap, values) {
    questions <- snap$component[edit_is_question(snap, snap$component$component_id), , drop = FALSE]
    only_questions <- snap
    only_questions$component <- questions
    Filter(Negate(is.null), list(
      edit_duplicate_rows(only_questions, "component", "text", values$text, "A question with exactly these words")
    ))
  },
  can = function(snap, mode) {
    if (mode == "add" && !is.null(snap$component_type) && is.na(edit_type_id(snap, "Question"))) {
      "The database has no component type called Question, so a question can't be added yet."
    }
  },
  build = function(mode, snap, row, scope, fields, values) {
    if (mode == "add") {
      text_field <- Filter(function(f) f$name == "text", fields)[[1]]
      edit_new_question_items(snap, edit_service_normalize(text_field, values[["question__text"]]))
    } else {
      edit_update_items(edit_entities[["question"]], snap, row, fields, values)
    }
  }
)

# ------ Inside one questionnaire -------------------------------------------------------------------------
questionnaire_scope <- list(table = "questionnaire", label = "In which questionnaire?")

# A placement's card line: where it sits.
edit_where_text <- function(snap, placed, rows) {
  section <- edit_lookup(snap, "component", "component_id", rows$parent_component_id, "text")
  ifelse(
    is.na(rows$parent_component_id), paste0("top level · position ", rows$display_order),
    paste0("in ", shorten(ifelse(is.na(section), "an untitled section", section), 40), " · position ", rows$display_order)
  )
}

questionnaire_component_entity <- list(
  label = "Questionnaire component", noun = "questionnaire component", plural = "questionnaire components", article = "a",
  group = "In one questionnaire only", table = "questionnaire_component", scope = questionnaire_scope,
  about = paste0(
    "A block as it's printed in one questionnaire: where it sits in the outline and its place among ",
    "its neighbours. Changes here only affect that questionnaire."
  ),
  update_about = "You're changing this questionnaire only. Other questionnaires that print the same block are not affected.",
  delete_about = paste0(
    "This only removes the block from this questionnaire. The component stays in the database for ",
    "any other questionnaire that prints it."
  ),
  delete_start = function(snap, row) edit_placement_start(snap, row),
  reads = c("question", "component", "component_type", "wave", "wave_question", "variable", "question_category"),
  initial = function(snap, row, scope) edit_placement_initial(snap, row, scope),
  build = function(mode, snap, row, scope, fields, values) edit_placement_items(mode, snap, row, scope, fields, values),
  cards = function(snap, scope) {
    if (!nzchar(scope %||% "")) {
      return(edit_cards(character(), character()))
    }
    placed <- edit_service_outline(edit_service_placements(snap, as.numeric(scope)))
    elsewhere <- edit_service_questionnaire_count(snap, placed$component_id) - 1
    inside <- vapply(placed$component_id, function(id) sum(placed$parent_component_id %in% id), numeric(1))
    edit_cards(
      edit_key(placed, c("questionnaire_id", "component_id")),
      paste0(strrep("— ", placed$depth), edit_component_text(snap, placed$component_id, edit_outline_title_chars)),
      subtitle = edit_where_text(snap, placed, placed),
      facts = Map(function(n_inside, n_other, type) {
        c(
          if (type == "Section") edit_count(n_inside, "item") |> paste("inside"),
          if (n_other > 0) paste("also printed in", edit_count(n_other, "other questionnaire"))
        )
      }, inside, elsewhere, placed$type),
      tag = placed$type
    )
  },
  fields = function(snap, mode, row, scope) edit_placement_fields(snap, mode, row, scope)
)

questionnaire_question_entity <- list(
  label = "Questionnaire question", noun = "questionnaire question", plural = "questionnaire questions", article = "a",
  group = "In one questionnaire only", table = "questionnaire_question", scope = questionnaire_scope,
  about = paste0(
    "A question as it's printed in one questionnaire: whether it stands on its own, what it's ",
    "conditional on, and the number it was given in each wave. Changes here only affect that ",
    "questionnaire. A new question comes with its variables and their MATR IDs, typed here, so it is ",
    "added whole."
  ),
  update_about = "You're changing this questionnaire only. Other questionnaires that ask the same question are not affected.",
  delete_about = paste0(
    "This only removes the question from this questionnaire: its place in the outline, its numbers ",
    "in each wave, and its variables with their choices and MATR IDs. The question's wording ",
    "stays in the database for any other questionnaire that asks it."
  ),
  delete_start = function(snap, row) edit_placement_start(snap, row),
  reads = c(
    "question", "component", "component_type", "wave", "wave_question", "variable", "question_category",
    # adding a question (service_edit_question_add.R): who variables can be about, and the answer choices
    # and sets of choices there are
    "subject", "answer_option", "option_set", "answer_option_set"
  ),
  initial = function(snap, row, scope) edit_question_initial(snap, row, scope),
  build = function(mode, snap, row, scope, fields, values) {
    if (mode == "add") {
      edit_question_add_items_in(snap, scope, fields, values)
    } else {
      edit_question_items(mode, snap, row, scope, fields, values)
    }
  },
  summary_add = function(snap, scope, fields, values) edit_question_add_summary(snap, scope, fields, values),
  can = function(snap, mode) {
    if (mode == "add" && !is.null(snap$component_type) && is.na(edit_type_id(snap, "Question"))) {
      "The database has no component type called Question, so a question can't be added yet."
    }
  },
  cards = function(snap, scope) {
    if (!nzchar(scope %||% "")) {
      return(edit_cards(character(), character()))
    }
    q <- as.numeric(scope)
    placed <- edit_service_outline(edit_service_placements(snap, q))
    questions <- snap$questionnaire_question[snap$questionnaire_question$questionnaire_id == q, , drop = FALSE]
    placed <- placed[placed$component_id %in% questions$component_id, , drop = FALSE]
    questions <- questions[match(placed$component_id, questions$component_id), , drop = FALSE]
    asked <- snap$wave_question[snap$wave_question$questionnaire_id == q, , drop = FALSE]
    boxes <- snap$variable[snap$variable$questionnaire_id == q, , drop = FALSE]
    elsewhere <- edit_service_questionnaire_count(snap, placed$component_id) - 1
    edit_cards(
      edit_key(placed, c("questionnaire_id", "component_id")),
      edit_component_text(snap, placed$component_id, edit_card_title_chars),
      subtitle = vapply(placed$component_id, function(id) {
        mine <- asked[asked$component_id == id, , drop = FALSE]
        if (nrow(mine) == 0) {
          return("not asked in any wave")
        }
        paste0("asked as ", paste0(
          "question ", edit_called_number(mine), " in ", edit_lookup(snap, "wave", "wave_id", mine$wave_id, "name"),
          collapse = ", "
        ))
      }, character(1)),
      facts = Map(function(id, n_other, self_contained, parent) {
        c(
          edit_count(sum(boxes$component_id == id), "variable", "variables"),
          if (isFALSE(self_contained)) paste0("follow-up to “", edit_component_text(snap, parent, 40), "”"),
          if (n_other > 0) paste("also asked in", edit_count(n_other, "other questionnaire"))
        )
      }, placed$component_id, elsewhere, questions$self_contained, questions$parent_question_id),
      tag = "Question"
    )
  },
  fields = function(snap, mode, row, scope) {
    if (mode == "add") edit_question_add_fields(snap, scope) else edit_question_fields(snap, row, scope)
  }
)

# What deleting a block from one questionnaire starts from: its placement, and
# for a question its settings too (they are one block).
edit_placement_start <- function(snap, row) {
  key <- edit_key(row, c("questionnaire_id", "component_id"))
  start <- list(questionnaire_component = key)
  if (key %in% edit_key(snap$questionnaire_question, c("questionnaire_id", "component_id"))) {
    start$questionnaire_question <- key
  }
  start
}

# The form for a block in one questionnaire: its words and kind (which, if
# changed, make a new component for this questionnaire only), and where it sits.
edit_placement_fields <- function(snap, mode, row, scope) {
  q <- as.numeric(scope)
  placed <- edit_service_placements(snap, q)
  section <- edit_type_id(snap, "Section")
  non_question <- snap$component_type[snap$component_type$name != "Question", , drop = FALSE]
  updating <- mode == "update"
  is_question <- updating && edit_is_question(snap, row$component_id)
  has_inside <- updating && any(placed$parent_component_id %in% row$component_id)
  parent_choices <- edit_service_parent_choices(snap, placed, if (updating) row$component_id)

  where <- list(
    edit_field("parent_component_id", "Inside which section or question group?", "select",
      choices = parent_choices, short = "section or group it's in",
      hint = "Only sections and question groups can hold other blocks.",
      problem = function(value, values) {
        # a section is never inside a question group (BR53)
        type <- values$component_type_id %||%
          if (updating) edit_lookup(snap, "component", "component_id", row$component_id, "component_type_id") else NA
        into <- placed$type[match(as.numeric(value), placed$component_id)]
        if (identical(as.numeric(type), section) && has_text(value) && identical(into, "Question Group")) {
          "A section can't be inside a question group. Pick a section, or the top level."
        }
      }
    ),
    edit_field(
      "display_order", "Position", "number",
      required = updating, min = 1, short = "position",
      hint = if (updating) {
        "1 is first. The blocks next to it move over to make room."
      } else {
        "1 is first. The blocks next to it move over to make room. Leave empty to put it last."
      },
      warn = function(value, values) {
        position <- edit_whole_number(value)
        parent <- if (nzchar(values$parent_component_id %||% "")) as.numeric(values$parent_component_id) else NA_real_
        room <- edit_service_room(placed, parent) + if (updating && same_parent(row$parent_component_id, parent)) 0 else 1
        if (!is.na(position) && position > room) sprintf("There are only %d places there, so it will go last (position %d).", room, room)
      }
    )
  )
  if (updating) {
    return(c(list(
      edit_text_field(snap, mode, row),
      edit_field(
        "component_type_id", "Kind of block", "select",
        required = TRUE, choices = edit_choices(snap, "component_type"),
        locked = if (is_question) {
          "A question stays a question. Its settings are under Questionnaire question."
        } else if (has_inside) {
          "A section or question group with blocks inside can't change kind. Move them out first."
        },
        hint = if (!is_question && !has_inside) {
          "Changing the kind, like changing the words, gives this questionnaire its own new component."
        }
      )
    ), where))
  }
  candidates <- snap$component[
    !edit_is_question(snap, snap$component$component_id) &
      !snap$component$component_id %in% placed$component_id, ,
    drop = FALSE
  ]
  choices <- stats::setNames(
    key_text(candidates$component_id),
    paste0(
      "[", edit_lookup(snap, "component_type", "component_type_id", candidates$component_type_id, "name"), "] ",
      edit_component_text(snap, candidates$component_id, 80),
      recycle0 = TRUE
    )
  )
  c(list(
    edit_field(
      "source", "Which block?", "choice",
      required = TRUE,
      choices = c("One that's already in the database" = "existing", "A new one" = "new"),
      default = if (nrow(candidates) > 0) "existing" else "new",
      hint = "Placing an existing component keeps it shared with the questionnaires already printing it."
    ),
    edit_field("component_id", "The component", "select",
      required = TRUE, show_when = list(field = "source", value = "existing"),
      choices = c("Choose a component" = "", choices),
      hint = "Questions are added as a questionnaire question (Add, then Questionnaire question), so they aren't listed. Blocks already in this questionnaire aren't either."
    ),
    edit_field("component_type_id", "Kind of block", "select",
      required = TRUE, show_when = list(field = "source", value = "new"),
      choices = edit_choices(snap, "component_type", non_question, blank = "Choose a kind")
    ),
    edit_text_field(snap, mode, NULL) |> utils::modifyList(list(show_when = list(field = "source", value = "new")))
  ), where)
}

# The form for a question in one questionnaire.
edit_question_fields <- function(snap, row, scope) {
  q <- as.numeric(scope)
  asked <- edit_placed_rows(snap, "wave_question", q, row$component_id)
  asked <- asked[order(asked$wave_id), , drop = FALSE]
  numbers <- lapply(seq_len(nrow(asked)), function(i) {
    wave <- asked$wave_id[i]
    wave_name <- edit_lookup(snap, "wave", "wave_id", wave, "name")
    edit_field(
      paste0("qn_", key_text(wave)), paste("Number printed in", wave_name),
      placeholder = "None printed", short = paste("number printed in", wave_name),
      hint = if (i == 1) {
        paste(
          "As printed, so it can have letters, and repeat from section to section: 1, 2a, 14. Leave it empty if",
          "none is printed. Its place among the questions is worked out from the outline."
        )
      }
    )
  })
  c(list(
    edit_text_field(snap, "update", row, "Question wording", type_field = FALSE) |>
      utils::modifyList(list(hint = paste0(
        "Changing the wording gives this questionnaire its own new question with the new words. Other ",
        "questionnaires keep the old one."
      ))),
    edit_field(
      "self_contained", "Can a researcher understand it on its own?", "choice",
      choices = c("Yes" = "TRUE", "No, it follows up on another question" = "FALSE"),
      short = "whether it stands on its own"
    ),
    edit_field(
      "parent_question_id", "Which question does it follow up on?", "select",
      required = TRUE,
      short = "the question it follows up on",
      show_when = list(field = "self_contained", value = "FALSE"),
      choices = c("Choose a question" = "", edit_service_parent_question_choices(snap, q, row$component_id)),
      hint = "Only questions printed before this one are listed."
    ),
    edit_field("condition", "Printed condition",
      placeholder = "e.g. If you answered Yes to the previous question",
      hint = "Leave empty if the question is asked of everyone."
    )
  ), numbers)
}

# ------ All of them -------------------------------------------------------------------------------------------
edit_entities <- list(
  registry = registry_entity,
  investigator = investigator_entity,
  study = study_entity,
  respondent_type = respondent_type_entity,
  questionnaire = questionnaire_entity,
  wave = wave_entity,
  component_type = component_type_entity,
  component = component_entity,
  question = question_entity,
  questionnaire_component = questionnaire_component_entity,
  questionnaire_question = questionnaire_question_entity,
  category = category_entity,
  answer_option = answer_option_entity,
  subject = subject_entity
)
edit_entities <- Map(function(entity, key) c(list(key = key), entity), edit_entities, names(edit_entities))
