# ------ Adding one question to a recorded questionnaire ----------------------------------------------
# Make an edit can add a single question to a questionnaire that is recorded already, as a whole: its
# wording (new, or a question already in the database, to keep the wording shared), where it sits in the
# outline, whether it stands on its own and what it follows up on, its number in each wave that asks it,
# and its variables with their MATR IDs and choices. It is written as one edit, so it is added all at once
# or not at all, and nothing is left half-finished.
#
# The variables are typed in one field (type "variables"): the page script (www/js/variables.js) draws an
# editor for them and keeps the field's value as JSON, a list of variables in the shape the Edit
# Extraction page saves (service_draft.R): variable_type, matr_id, subject, label, answer_data_type and,
# for choices, response_options (code_value, answer_option, printed_label, skip_instruction). They are
# added the way an extraction adds them (push_variables(), service_extraction_push.R), so an answer choice
# or a set of choices that is there already is used again.

#' The form for adding a question to one questionnaire.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param scope The questionnaire's key.
#' @return A list of fields; see `edit_field()`.
edit_question_add_fields <- function(snap, scope) {
  q <- as.numeric(scope)
  placed <- edit_service_placements(snap, q)
  candidates <- snap$component[
    edit_is_question(snap, snap$component$component_id) & !snap$component$component_id %in% placed$component_id, ,
    drop = FALSE
  ]
  candidate_choices <- stats::setNames(
    key_text(candidates$component_id),
    paste0(
      edit_component_text(snap, candidates$component_id, 80), " · ",
      vapply(candidates$component_id, function(id) edit_seen_in(snap, id), character(1)),
      recycle0 = TRUE
    )
  )
  c(
    list(
      edit_field(
        "source", "Which question?", "choice",
        required = TRUE,
        choices = c("A new question" = "new", "One that's already in the database" = "existing"),
        default = "new",
        hint = "Using a question that's already in the database keeps its wording shared with the questionnaires that ask it."
      ),
      edit_text_field(snap, "add", NULL, "Question wording", type_field = FALSE) |>
        utils::modifyList(list(
          show_when = list(field = "source", value = "new"),
          hint = "The question's words only, as printed, without its number."
        )),
      edit_field("component_id", "The question", "select",
        required = TRUE, show_when = list(field = "source", value = "existing"),
        choices = c("Choose a question" = "", candidate_choices),
        hint = "Questions this questionnaire already asks aren't listed."
      ),
      edit_field("parent_component_id", "Inside which section?", "select",
        choices = edit_service_parent_choices(snap, placed), short = "section it's in",
        hint = "Only sections can hold other blocks."
      ),
      edit_field(
        "display_order", "Position", "number",
        min = 1, short = "position",
        hint = "1 is first. The blocks next to it move over to make room. Leave empty to put it last.",
        warn = function(value, values) {
          position <- edit_whole_number(value)
          parent <- if (has_text(values$parent_component_id)) as.numeric(values$parent_component_id) else NA_real_
          room <- edit_service_room(placed, parent) + 1
          if (!is.na(position) && position > room) sprintf("There are only %d places there, so it will go last (position %d).", room, room)
        }
      ),
      edit_field(
        "self_contained", "Can a researcher understand it on its own?", "choice",
        choices = c("Yes" = "TRUE", "No, it follows up on another question" = "FALSE"),
        default = "TRUE", short = "whether it stands on its own"
      ),
      edit_field(
        "parent_question_id", "Which question does it follow up on?", "select",
        required = TRUE, short = "the question it follows up on",
        show_when = list(field = "self_contained", value = "FALSE"),
        choices = c("Choose a question" = "", edit_service_parent_question_choices(snap, q)),
        hint = "Pick one printed before where this one goes."
      ),
      edit_field("condition", "Printed condition",
        placeholder = "e.g. If you answered Yes to the previous question",
        hint = "Leave empty if the question is asked of everyone."
      )
    ),
    edit_question_add_wave_fields(snap, q),
    list(edit_field(
      "variables", "Variables", "variables",
      required = TRUE, default = edit_variables_default(snap),
      options = list(
        subjects = sort(snap$subject$name),
        answer_data_types = c("NUMBER", "TEXT", "DATE", "TIME"),
        answer_options = sort(snap$answer_option$standardized_label)
      ),
      hint = paste(
        "One for each answer a respondent can record, in printed order, each with its MATR ID. A lone check",
        "box has one choice: only choices printed on the form are choices."
      ),
      problem = edit_variables_problem(snap, q)
    ))
  )
}

# One field for each of the questionnaire's waves: the question's number in it. A question is asked in at
# least one wave (BR69), so a questionnaire with none can't have a question added yet.
edit_question_add_wave_fields <- function(snap, q) {
  waves <- snap$wave[snap$wave$questionnaire_id == q, , drop = FALSE]
  waves <- waves[order(waves$wave_id), , drop = FALSE]
  if (nrow(waves) == 0) {
    return(list(edit_field(
      "qn_none", "Question number", required = TRUE,
      problem = function(value, values) {
        "This questionnaire has no waves yet. Add one first (Add, then Wave): a question is asked in a wave."
      }
    )))
  }
  names <- paste0("qn_", key_text(waves$wave_id))
  lapply(seq_len(nrow(waves)), function(i) {
    wave <- waves$wave_id[i]
    edit_field(
      names[i], paste("Question number in", waves$name[i]),
      placeholder = "e.g. 14a", short = paste("number in", waves$name[i]),
      hint = if (i == 1) "As printed, so it can have letters: 1, 2a, 1a4. Leave a wave empty if it didn't ask the question.",
      problem = function(value, values) {
        if (i == 1 && !any(vapply(names, function(name) has_text(values[[name]]), logical(1)))) {
          return("Give it its number in at least one wave: a question is asked in a wave.")
        }
        value <- trimws(value)
        asked <- snap$wave_question[snap$wave_question$wave_id == wave, , drop = FALSE]
        same <- asked[tolower(trimws(asked$question_number)) == tolower(value), , drop = FALSE]
        if (nzchar(value) && nrow(same) > 0) {
          sprintf(
            "Question %s in %s is already “%s”. Pick another number.",
            value, waves$name[i], edit_component_text(snap, same$component_id[1], 50)
          )
        }
      }
    )
  })
}

# ------ The variables, typed as JSON ----------------------------------------------------------------------
#' What the variables field starts with: one variable with one blank choice, about the respondent.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them.
#' @return A JSON string.
edit_variables_default <- function(snap) {
  subject <- if ("Self" %in% snap$subject$name) "Self" else ""
  as.character(jsonlite::toJSON(
    list(list(
      variable_type = "closed_ended", matr_id = "", subject = subject, label = "",
      response_options = list(list(code_value = "1", answer_option = "", printed_label = "", skip_instruction = ""))
    )),
    auto_unbox = TRUE
  ))
}

#' The variables typed, read from the field's JSON.
#'
#' @param value The field's value.
#' @return A list of variables, each a list; an error if it isn't a list of them.
edit_variables_parse <- function(value) {
  if (!has_text(value)) {
    return(list())
  }
  variables <- jsonlite::fromJSON(value, simplifyVector = FALSE)
  if (!is.list(variables) || (length(variables) > 0 && !all(vapply(variables, is.list, logical(1))))) {
    stop("The variables aren't a list.", call. = FALSE)
  }
  variables
}

# A value of a variable or choice as typed: trimmed text, "" for nothing.
edit_variable_text <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value[1])) "" else trimws(as.character(value[1]))
}

#' The check of the variables field.
#'
#' The rules the Edit Extraction page checks (service_draft_checks.R), for the variables of one question:
#' each has a MATR ID no other variable of the questionnaire has (BR93), who it's about, and either the
#' kind of answer it takes (a write-in) or its choices, each filled in, listed once and with a code of its
#' own (BR103).
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them.
#' @param q The questionnaire's id.
#' @return A function of `(value, values)` giving the first problem as a sentence, or `NULL`.
edit_variables_problem <- function(snap, q) {
  taken <- snap$variable$matr_id[snap$variable$questionnaire_id %in% q]
  function(value, values) {
    variables <- tryCatch(edit_variables_parse(value), error = function(problem) NULL)
    if (is.null(variables)) {
      return("The variables couldn't be read. Add them again.")
    }
    if (length(variables) == 0) {
      return("Add at least one variable.") # BR87
    }
    ids <- vapply(variables, function(box) edit_variable_text(box$matr_id), character(1))
    for (i in seq_along(variables)) {
      box <- variables[[i]]
      say <- function(text) sprintf("Variable %d: %s", i, text)
      if (!nzchar(ids[i])) {
        return(say("fill in its MATR ID."))
      }
      if (ids[i] %in% ids[seq_len(i - 1)]) {
        return(say("give it a MATR ID no other variable has."))
      }
      if (ids[i] %in% taken) {
        return(say(sprintf("%s is already a MATR ID in this questionnaire. Give it one of its own.", ids[i])))
      }
      if (!edit_variable_text(box$subject) %in% snap$subject$name) {
        return(say("pick who it's about."))
      }
      if (identical(box$variable_type, "open_ended")) {
        if (!edit_variable_text(box$answer_data_type) %in% c("NUMBER", "TEXT", "DATE", "TIME")) {
          return(say("pick the kind of answer it takes.")) # BR95
        }
        next
      }
      choices <- box$response_options %||% list()
      if (length(choices) == 0) {
        return(say("add its choices.")) # BR99: a lone check box has one
      }
      answers <- vapply(choices, function(choice) edit_variable_text(choice$answer_option), character(1))
      codes <- vapply(choices, function(choice) edit_variable_text(choice$code_value), character(1))
      if (!all(nzchar(answers))) {
        return(say("fill in the blank choice."))
      }
      if (anyDuplicated(answers) > 0) {
        return(say("list each choice once."))
      }
      if (!all(nzchar(codes))) {
        return(say("give each choice a code."))
      }
      if (anyDuplicated(codes) > 0) {
        return(say("give each choice its own code.")) # BR103
      }
    }
    NULL
  }
}

#' The variables typed, as the rows of a draft, so they can be added the way an extraction adds them.
#'
#' @param variables From `edit_variables_parse()`.
#' @return A draft holding one question, `c1`, with the variables and their choices.
edit_variables_draft <- function(variables) {
  boxes <- lapply(variables, function(box) {
    closed <- !identical(box$variable_type, "open_ended")
    text_or_null <- function(value) if (nzchar(edit_variable_text(value))) edit_variable_text(value)
    without_blanks(list(
      variable_type = if (closed) "closed_ended" else "open_ended",
      label = text_or_null(box$label),
      subject = text_or_null(box$subject),
      matr_id = text_or_null(box$matr_id),
      answer_data_type = if (!closed) text_or_null(box$answer_data_type),
      response_options = if (closed) {
        lapply(box$response_options %||% list(), function(choice) {
          without_blanks(list(
            answer_option = text_or_null(choice$answer_option),
            code_value = suppressWarnings(as.integer(edit_variable_text(choice$code_value))),
            printed_label = text_or_null(choice$printed_label),
            skip_instruction = text_or_null(choice$skip_instruction)
          ))
        })
      }
    ))
  })
  draft_service_from_json(list(components = list(list(component_type = "Question", variables = boxes))))
}

# ------ Writing it as items -----------------------------------------------------------------------------
#' The items for a question added to one questionnaire.
#'
#' In the order the database needs them: the question (if its wording is new), its place in the outline
#' (the blocks after it moving over to make room), its settings, its number in each wave that asks it, and
#' its variables with their choices.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them.
#' @param scope The questionnaire's key.
#' @param fields The form's fields, from `edit_service_fields()`.
#' @param values What is in the form, by field id.
#' @return A data frame of items; see `edit_item()`.
edit_question_add_items_in <- function(snap, scope, fields, values) {
  q <- as.numeric(scope)
  prefix <- "questionnaire_question"
  value_of <- function(name) values[[paste0(prefix, "__", name)]] %||% ""
  field <- function(name) Filter(function(f) f$name == name, fields)[[1]]
  items <- list()
  add <- function(rows) if (!is.null(rows)) items[[length(items) + 1]] <<- rows

  # the question: one that's there, or a new one
  if (identical(value_of("source"), "existing")) {
    component <- as.numeric(value_of("component_id"))
  } else {
    component <- "new:component:1"
    add(edit_new_question_items(snap, edit_service_normalize(field("text"), value_of("text"))))
  }

  # its place in the outline
  placed <- edit_service_placements(snap, q)
  parent <- if (has_text(value_of("parent_component_id"))) as.numeric(value_of("parent_component_id")) else NA_real_
  room <- edit_service_room(placed, parent)
  position <- edit_whole_number(value_of("display_order"))
  position <- if (is.na(position)) room + 1 else max(1, min(position, room + 1))
  later <- placed[same_parent(placed$parent_component_id, parent) & placed$display_order >= position, , drop = FALSE]
  for (i in seq_len(nrow(later))) {
    add(edit_item(
      "questionnaire_component", edit_identifier(list(questionnaire_id = q, component_id = later$component_id[i])),
      "Update", "display_order", later$display_order[i], later$display_order[i] + 1
    ))
  }
  here <- edit_identifier(list(questionnaire_id = q, component_id = component))
  if (!is.na(parent)) add(edit_item("questionnaire_component", here, "Insert", "parent_component_id", NA, parent))
  add(edit_item("questionnaire_component", here, "Insert", "display_order", NA, position))

  # what is true of it in this questionnaire
  self_contained <- edit_service_normalize(field("self_contained"), value_of("self_contained"))
  add(edit_item("questionnaire_question", here, "Insert", "self_contained", NA, self_contained))
  if (identical(self_contained, "false")) {
    add(edit_item("questionnaire_question", here, "Insert", "parent_question_id", NA, as.numeric(value_of("parent_question_id"))))
  }
  condition <- edit_service_normalize(field("condition"), value_of("condition"))
  if (!is.na(condition)) add(edit_item("questionnaire_question", here, "Insert", "condition", NA, condition))

  # its number in each wave that asks it
  for (name in grep("^qn_[0-9]+$", names(edit_part_values(fields, values, prefix)), value = TRUE)) {
    number <- trimws(value_of(name))
    if (!nzchar(number)) next
    wave <- as.numeric(sub("^qn_", "", name))
    add(edit_item(
      "wave_question", edit_identifier(list(questionnaire_id = q, wave_id = wave, component_id = component)),
      "Insert", "question_number", NA, number
    ))
  }

  # its variables, added the way an extraction adds them
  draft <- edit_variables_draft(edit_variables_parse(value_of("variables")))
  push <- push_start(
    ids = list(component_types = snap$component_type, subjects = snap$subject),
    existing = NULL,
    options = list(answer_options = snap$answer_option, option_sets = snap$option_set, members = snap$answer_option_set)
  )
  push_variables(push, draft, draft$components[1, ], component, q)
  add(do.call(rbind, push$items()))
  do.call(rbind, items)
}

#' The proposal's sentence for a question added to a questionnaire.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them.
#' @param scope The questionnaire's key.
#' @param fields The form's fields.
#' @param values What is in the form, by field id.
#' @return A string, e.g. `"Added the question “Do you smoke?” to AHP"`.
edit_question_add_summary <- function(snap, scope, fields, values) {
  value_of <- function(name) values[[paste0("questionnaire_question__", name)]] %||% ""
  words <- if (identical(value_of("source"), "existing")) {
    edit_component_text(snap, as.numeric(value_of("component_id")), 60)
  } else {
    shorten(trimws(value_of("text")), 60)
  }
  questionnaire <- snap$questionnaire[snap$questionnaire$questionnaire_id %in% as.numeric(scope), , drop = FALSE]
  paste0(
    "Added the question “", words, "”",
    if (nrow(questionnaire) == 1) paste0(" to ", questionnaire$abbreviation)
  )
}
