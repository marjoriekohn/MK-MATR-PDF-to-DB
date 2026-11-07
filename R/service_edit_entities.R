# The pieces Make an edit's kinds are built from: cards, drop-downs, fields and the checks they share. The
# kinds themselves are in service_edit_entities_*.R, one file for each group, and listed in service_edit_kinds.R.

# ------ Building blocks ---------------------------------------------------------------------
# A count of rows with its noun, thousands separated.
# For example "1 study", "3 studies", "1,200 waves".
# singular: The noun for one.
# plural_form: The noun for any other count; defaults to adding an "s".
# Returns: A string; see `plural()`.
edit_count <- function(n, singular, plural_form = paste0(singular, "s")) {
  plural(n, singular, plural_form = plural_form, big_mark = ",")
}

# For each of `ids`, how many of `values` are it.
count_in <- function(values, ids) tabulate(match(values, ids), nbins = length(ids))

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

# The choices of a drop-down of rows: shown by name, saved as the row's ID.
# table: The table the rows are from.
# rows: The rows to offer; all of the table's by default.
# blank: The text of a leading choice with the value `""`, or `NULL` for none.
# Returns: A named character vector: the names are shown, and the values, the ids, are saved.
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
  registry = function(snap, rows) name_with_abbreviation(rows$name, rows$abbreviation),
  respondent_type = function(snap, rows) paste0(rows$name, " (", age_range_text(rows$minimum_age, rows$maximum_age), ")"),
  study = function(snap, rows) {
    paste0(rows$name, " · ", edit_lookup(snap, "registry", "registry_id", rows$registry_id, "abbreviation"))
  },
  questionnaire = function(snap, rows) {
    paste0(name_with_abbreviation(rows$name, rows$abbreviation), " · ", edit_lookup(snap, "study", "study_id", rows$study_id, "name"))
  }
)

edit_field_types <- c("text", "textarea", "number", "date", "select", "several", "choice", "variables")

# How a form's field is described. `type` is text, textarea, number, date,
# select (`choices`), several (any number of `choices`, saved as their values joined by commas),
# choice (side-by-side buttons), or variables (a question's
# variables and their choices, as JSON, edited with the variable editor,
# variable_editor_server(), which offers what `options` lists). `locked` is the reason a field can't change once the row exists.
# `show_when` is list(field, value): the field only matters while another has
# that value. `problem` and `warn` are functions of (value, values), where
# `values` is everything typed in the same part of the form by field name,
# giving a sentence, or NULL: a problem stops the edit, a warning only says.
# `default` is what an added row starts with. `tooltip` is added to the hint, after it.
edit_field <- function(name, label, type = "text", required = FALSE, choices = NULL, hint = NULL,
                       placeholder = NULL, locked = NULL, min = NULL, rows = NULL, show_when = NULL,
                       problem = NULL, warn = NULL, default = NULL, short = NULL,
                       tooltip = NULL, options = NULL, normalize = NULL) {
  # the form would draw any other as a text box, and check nothing about it
  if (!type %in% edit_field_types) stop("A form field can't be of type ", type, ".", call. = FALSE)
  list(
    name = name, label = label, type = type, required = required, choices = choices, hint = hint,
    placeholder = placeholder, locked = locked, min = min, rows = rows, show_when = show_when,
    problem = problem, warn = warn, default = default, tooltip = tooltip, options = options,
    normalize = normalize, # a function: what is typed, as it is saved
    short = short %||% tolower_first(label) # what a sentence calls it: "changed the ..."
  )
}

# Words as they are compared: capitals and extra spaces (at either end, or doubled inside) don't count.
edit_same_words <- function(text) tolower(squish(text))

# Another row of `table` already has `value` in `column` (ignoring case and
# extra spaces), apart from the row being changed.
edit_taken <- function(snap, table, column, value, row = NULL, within = NULL) {
  rows <- snap[[table]]
  if (!is.null(row)) rows <- rows[edit_key(rows, edit_table_key(table)) != edit_key(row, edit_table_key(table)), , drop = FALSE]
  if (!is.null(within)) rows <- rows[rows[[names(within)]] %in% within[[1]], , drop = FALSE]
  same <- edit_same_words(rows[[column]]) == edit_same_words(value)
  rows[which(same), , drop = FALSE]
}

# ------ Small helpers the forms use ---------------------------------------------------------------------
# A whole number typed as text.
# text: For example `"1,200"`.
# Returns: The number, or `NA` if `text` isn't a whole number.
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
