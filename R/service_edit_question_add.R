# ------ A question's variables, typed on Make an edit ----------------------------------------------
# A question added to a questionnaire on Make an edit comes whole: its wording, where it sits, the waves that ask
# it, and its variables with their MATR IDs, choices and tags (service_edit_placement.R). The variables are typed
# in one field (type "variables"), edited with the Edit Extraction page's variable editor
# (variable_editor_server()), which keeps the field's value as JSON: a list of variables in the shape an
# extraction saves (docs/database_design.md): variable_type, matr_id, label, subject, time_frame, unit, and for
# a write-in answer_data_type, or for choices response_options (code_value, answer_option). They are written the
# way a push writes them: an answer choice or a tag that is there already is used again, a new one is added.

# What the variables field starts with: one variable with one blank choice, about the respondent.
# Returns: A JSON string.
edit_variables_default <- function() {
  as.character(jsonlite::toJSON(
    list(list(
      variable_type = "closed_ended", matr_id = "", subject = "", label = "",
      response_options = list(list(code_value = "1", answer_option = ""))
    )),
    auto_unbox = TRUE
  ))
}

# What the variable editor offers in its drop-downs: the tags there are, by kind, and the answer choices.
edit_variables_options <- function(snap) {
  tags <- function(kind) sort(snap$tag$name[snap$tag$kind %in% kind])
  list(
    subjects = tags("subject")[!is.na(normal_subject(tags("subject")))],
    time_frames = tags("time frame"),
    units = tags("unit"),
    answer_data_types = c("NUMBER", "TEXT", "DATE", "TIME"),
    answer_options = sort(snap$answer_option$standardized_label)
  )
}

# The variables typed, read from the field's JSON.
# value: The field's value.
# Returns: A list of variables, each a list; an error if it isn't a list of them.
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

# The check of the variables field.
# The rules the database keeps, for the variables of one question: each has a MATR ID no other variable of the
# questionnaire has, and either the kind of answer it takes (a write-in) or its choices, each filled in, listed
# once and with a code of its own.
# Returns: A function of `(value, values)` giving the first problem as a sentence, or `NULL`.
edit_variables_problem <- function(snap, q) {
  taken <- snap$variable$matr_id[snap$variable$questionnaire_id %in% q]
  function(value, values) {
    variables <- tryCatch(edit_variables_parse(value), error = function(problem) NULL)
    if (is.null(variables)) {
      return("The variables couldn't be read. Add them again.")
    }
    if (length(variables) == 0) {
      return("Add at least one variable.")
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
      if (identical(box$variable_type, "open_ended")) {
        if (!edit_variable_text(box$answer_data_type) %in% c("NUMBER", "TEXT", "DATE", "TIME")) {
          return(say("pick the kind of answer it takes."))
        }
        next
      }
      choices <- box$response_options %||% list()
      if (length(choices) == 0) {
        return(say("add its choices."))
      }
      answers <- vapply(choices, function(choice) normal_answer(edit_variable_text(choice$answer_option)), character(1))
      answers[is.na(answers)] <- ""
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
        return(say("give each choice its own code."))
      }
    }
    NULL
  }
}

# ------ To and from the variable editor -----------------------------------------------------------------
# The variables typed, as the rows of a draft, so the variable editor can edit them as Edit Extraction does.
# variables: From `edit_variables_parse()`.
# Returns: A draft holding one question, `c1`, with the variables and their choices.
edit_variables_draft <- function(variables) {
  boxes <- lapply(variables, function(box) {
    closed <- !identical(box$variable_type, "open_ended")
    text_or_null <- function(value) if (nzchar(edit_variable_text(value))) edit_variable_text(value)
    without_blanks(list(
      variable_type = if (closed) "closed_ended" else "open_ended",
      label = normal_text(text_or_null(box$label)),
      subject = text_or_null(box$subject),
      time_frame = text_or_null(box$time_frame),
      unit = if (!closed) text_or_null(box$unit),
      matr_id = text_or_null(box$matr_id),
      answer_data_type = if (!closed) text_or_null(box$answer_data_type),
      response_options = if (closed) {
        lapply(box$response_options %||% list(), function(choice) {
          without_blanks(list(answer_option = text_or_null(choice$answer_option), code_value = text_or_null(choice$code_value)))
        })
      }
    ))
  })
  draft_service_from_json(list(components = list(list(component_type = "Question", variables = boxes))))
}

# The variables of a draft's question, as the field's JSON again: the other way from `edit_variables_draft()`.
# draft: A draft from `edit_variables_draft()`, as the editor has changed it.
# Returns: A JSON string; what isn't filled in is "".
edit_variables_json <- function(draft) {
  text <- function(value) if (length(value) == 0 || is.na(value)) "" else as.character(value)
  boxes <- draft_service_children(draft, "variables", draft$components$id[1])
  variables <- map_rows(boxes, function(box) {
    about <- list(
      variable_type = if (identical(box$variable_type, "open_ended")) "open_ended" else "closed_ended",
      matr_id = text(box$matr_id), subject = text(box$subject), label = text(box$label)
    )
    # a time frame only when it has one, so a form without them reads as it did
    if (nzchar(text(box$time_frame))) about$time_frame <- text(box$time_frame)
    if (about$variable_type == "open_ended") {
      return(c(about, list(answer_data_type = text(box$answer_data_type)), if (nzchar(text(box$unit))) list(unit = text(box$unit))))
    }
    choices <- draft_service_children(draft, "response_options", box$id)
    c(about, list(response_options = map_rows(choices, function(choice) {
      list(code_value = text(choice$code_value), answer_option = text(choice$answer_option))
    })))
  })
  as.character(jsonlite::toJSON(variables, auto_unbox = TRUE))
}

# ------ Writing the variables as items -----------------------------------------------------------------------
# The items that add a question's variables: each variable (`new:variable:<n>`), its choices (an answer choice
# that is in the list already is used, a new one added once, `new:answer_option:<n>`) and its tags (the same, by
# kind and name, `new:tag:<n>`).
# q: The questionnaire.
# placement: The question's placement: its id, or the token of one being added.
# variables: From `edit_variables_parse()`.
# Returns: A data frame of items, or `NULL` for no variables.
edit_variable_items <- function(snap, q, placement, variables) {
  items <- list()
  add <- function(...) items[[length(items) + 1]] <<- edit_item(...)
  made <- new.env() # the answer choices and tags this change adds, so each is added once
  made$answer_option <- character()
  made$tag <- character()

  # the row of a shared list with these words: the one the database has, or one added here
  find_or_add <- function(table, rows, column, value, extra = list()) {
    if (nrow(rows) > 0) {
      return(rows[[edit_table_key(table)]][1])
    }
    token <- made[[table]][value]
    if (!is.na(token)) {
      return(unname(token))
    }
    token <- sprintf("new:%s:%d", table, length(made[[table]]) + 1)
    made[[table]][value] <- token
    identifier <- edit_identifier(stats::setNames(list(token), edit_table_key(table)))
    for (name in names(extra)) add(table, identifier, "Insert", name, NA, extra[[name]])
    add(table, identifier, "Insert", column, NA, value)
    token
  }
  answer_option_for <- function(words) {
    normal <- normal_answer(words)
    rows <- snap$answer_option[snap$answer_option$standardized_label %in% normal, , drop = FALSE]
    find_or_add("answer_option", rows, "standardized_label", normal)
  }
  tag_for <- function(kind, name) {
    name <- squish(straight_quotes(name))
    rows <- snap$tag[snap$tag$kind %in% kind & edit_same_words(snap$tag$name) %in% edit_same_words(name), , drop = FALSE]
    find_or_add("tag", rows, "name", name, extra = list(kind = kind))
  }

  for (i in seq_along(variables)) {
    box <- variables[[i]]
    closed <- !identical(box$variable_type, "open_ended")
    token <- sprintf("new:variable:%d", i)
    here <- edit_identifier(list(variable_key = token))
    add("variable", here, "Insert", "questionnaire_id", NA, q)
    add("variable", here, "Insert", "placement_id", NA, placement)
    add("variable", here, "Insert", "matr_id", NA, edit_variable_text(box$matr_id))
    add("variable", here, "Insert", "display_order", NA, i)
    label <- normal_text(edit_variable_text(box$label))
    if (has_text(label)) add("variable", here, "Insert", "label", NA, label)
    add("variable", here, "Insert", "variable_type", NA, if (closed) "closed_ended" else "open_ended")
    if (!closed) add("variable", here, "Insert", "answer_data_type", NA, edit_variable_text(box$answer_data_type))
    if (closed) {
      choices <- box$response_options %||% list()
      for (j in seq_along(choices)) {
        choice <- edit_identifier(list(answer_option_id = answer_option_for(choices[[j]]$answer_option), variable_key = token))
        add("response_option", choice, "Insert", "display_order", NA, j)
        add("response_option", choice, "Insert", "code_value", NA, edit_variable_text(choices[[j]]$code_value))
      }
    }
    tags <- c(subject = "subject", time_frame = "time frame", unit = "unit")
    for (field in names(tags)) {
      name <- edit_variable_text(if (field == "subject") normal_subject(box[[field]]) else edit_tag_name(tags[[field]], box[[field]]))
      if (!nzchar(name)) next
      add("variable_tag", edit_identifier(list(tag_id = tag_for(tags[[field]], name), variable_key = token)), "Insert", NULL, NA, NA)
    }
  }
  if (length(items) == 0) NULL else do.call(rbind, items)
}
