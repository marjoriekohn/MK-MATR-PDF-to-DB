# ------ Making an edit ----------------------------------------------------------------------
# What the Make an edit page does, apart from drawing it: reads what it needs
# from the live tables, lists the rows staff can pick, checks what they type,
# works out what a change reaches, and writes the change as a proposal.
#
# A proposal is a list of items, one per field changed (R/model_edit.R), in the
# shape of the database's WRITE_HISTORY_ITEM rows:
#   Update  one per field that differs, with its old and new value
#   Insert  one per field of each new row
#   Delete  one per field of the row and of every row that goes with it
# Items are numbered in the order they must be applied: what's new before
# what points at it, and what points at a row before the row.
#
# A row an edit adds has no ID yet. Its items name it `new:<table>:<n>`
# (new:study:1), and so does anything pointing at it; the apply step swaps in
# the IDs the database gives.

# ------ Items -----------------------------------------------------------------------------------
# One field of one row, as text: empty is NA, TRUE is "true".
item_text <- function(value) {
  if (length(value) == 0 || is.na(value) || identical(value, "")) {
    return(NA_character_)
  }
  if (is.logical(value)) {
    return(if (value) "true" else "false")
  }
  if (is.numeric(value)) {
    return(key_text(value))
  }
  as.character(value)
}

#' One item of a proposal: one change to one field of one row.
#'
#' @param entity_name The table.
#' @param record_identifier The row, from `edit_identifier()`.
#' @param action `"Insert"`, `"Update"` or `"Delete"`.
#' @param attribute_name The column, or `NULL` for a whole row.
#' @param old_value What it was (an Update or Delete).
#' @param new_value What it becomes (an Insert or Update).
#' @return A one-row data frame.
edit_item <- function(entity_name, record_identifier, action, attribute_name, old_value, new_value) {
  data.frame(
    entity_name = entity_name, record_identifier = record_identifier, action = action,
    attribute_name = attribute_name %||% NA_character_,
    old_value = item_text(old_value), new_value = item_text(new_value),
    stringsAsFactors = FALSE
  )
}

#' Name a row by its key columns.
#'
#' @param key A named list of the key columns to their values.
#' @return JSON with the columns in alphabetical order, e.g. `{"component_id":88,"questionnaire_id":3}`.
edit_identifier <- function(key) {
  key <- key[order(names(key))]
  key <- lapply(key, function(value) {
    if (is.numeric(value) && !is.na(value) && value == round(value) && abs(value) < 2^31) as.integer(value) else value
  })
  as.character(jsonlite::toJSON(key, auto_unbox = TRUE))
}

edit_row_identifier <- function(table, row) {
  edit_identifier(as.list(row[edit_table_key(table)]))
}

# Items, numbered from 1 in the order given.
edit_number_items <- function(items) {
  if (is.null(items) || nrow(items) == 0) {
    return(data.frame(
      item_number = integer(), entity_name = character(), record_identifier = character(), action = character(),
      attribute_name = character(), old_value = character(), new_value = character(), stringsAsFactors = FALSE
    ))
  }
  items <- cbind(item_number = seq_len(nrow(items)), items)
  rownames(items) <- NULL
  items
}

# ------ Reading --------------------------------------------------------------------------------------
# Every table the kind needs: its rows, what they point at and what points at
# them, so a card can count and a delete can say what goes.
edit_service_tables <- function(entity) {
  parts <- entity$parts %||% list(list(entity = entity$key))
  part_tables <- vapply(parts, function(part) edit_entities[[part$entity]]$table, character(1))
  parents <- unique(c(unlist(lapply(part_tables, function(table) {
    vapply(Filter(function(relation) relation$child == table, edit_relations), function(relation) relation$parent, character(1))
  })), entity$scope$table))
  unique(c(
    edit_tables_to_read(c(part_tables, entity$scope$table)), parents,
    unlist(edit_label_needs[parents]), unlist(edit_choice_needs[parents]), entity$reads,
    "questionnaire" # where a row appears is grouped by questionnaire
  ))
}

# A drop-down names a study with its registry, and a questionnaire with its study.
edit_choice_needs <- list(study = "registry", questionnaire = "study")

#' Read the tables a kind needs, afresh.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @return A list of data frames named after the tables.
edit_service_load <- function(connection, entity) {
  tables <- edit_service_tables(entity)
  stats::setNames(lapply(tables, function(table) table_model_get(connection, table)), tables)
}

#' Lower-case the first letter.
#'
#' @param text A string.
#' @return `text` with its first letter in lower case.
tolower_first <- function(text) paste0(tolower(substr(text, 1, 1)), substring(text, 2))

#' Lower-case a label for the middle of a sentence.
#'
#' A word that starts with a capital and goes on in lower case (Respondent Type) is lower-cased; an
#' acronym (MATR ID) is left as it is.
#'
#' @param text A string, e.g. a field's label.
#' @return `text`, e.g. `"respondent type"`.
tolower_words <- function(text) {
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  paste(ifelse(grepl("^[A-Z][a-z]", words), tolower_first(words), words), collapse = " ")
}

#' Why a kind can't be added, updated or deleted here.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param mode `"add"`, `"update"` or `"delete"`.
#' @return A sentence, or `NULL` if it can.
edit_service_can <- function(entity, snap, mode) {
  if (!is.null(entity$can)) entity$can(snap, mode)
}

# ------ Finding the row -----------------------------------------------------------------------------------
#' The cards staff pick from.
#'
#' Narrowed to those that have every word in `query`.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param scope The questionnaire's key, for a kind that belongs to one.
#' @param query Words typed to find a card.
#' @return A data frame of cards; see `edit_cards()`.
edit_service_cards <- function(entity, snap, scope = NULL, query = "") {
  edit_service_filter(entity$cards(snap, scope), query)
}

#' The cards with every one of the words typed.
#'
#' Building the cards counts things, so the page builds them once and filters as staff type.
#'
#' @param cards A data frame of cards.
#' @param query Words typed to find a card.
#' @return The matching cards.
edit_service_filter <- function(cards, query = "") {
  words <- strsplit(tolower(trimws(query %||% "")), "[[:space:]]+")[[1]]
  words <- words[nzchar(words)]
  for (word in words) cards <- cards[grepl(word, cards$search, fixed = TRUE), , drop = FALSE]
  cards
}

#' A kind's row by its key.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param key The row's key, as text (`edit_key()`).
#' @return A one-row data frame, or `NULL`.
edit_service_row <- function(entity, snap, key) {
  rows <- snap[[entity$table]]
  found <- rows[edit_key(rows, edit_table_key(entity$table)) %in% key, , drop = FALSE]
  if (nrow(found) == 1) found
}

# ------ The form ---------------------------------------------------------------------------------------------
#' The fields of an add or update form.
#'
#' Each has an `id` (kind__field) that is also its input's name. An add that needs more
#' rows (a study, with its first questionnaire and wave) has the fields of all of them,
#' `part` saying which.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param mode `"add"` or `"update"`.
#' @param row The row being updated.
#' @param scope The questionnaire's key, for a kind that belongs to one.
#' @return A list of fields, each a list with `id`, `name`, `label`, `type` and more.
edit_service_fields <- function(entity, snap, mode, row = NULL, scope = NULL) {
  parts <- if (mode == "add" && !is.null(entity$parts)) entity$parts else list(list(entity = entity$key))
  fields <- list()
  for (i in seq_along(parts)) {
    part <- parts[[i]]
    specs <- edit_entities[[part$entity]]$fields(snap, mode, if (i == 1) row, scope)
    if (!is.null(part$link)) specs <- Filter(function(spec) spec$name != part$link, specs)
    specs <- lapply(specs, function(spec) {
      spec$part <- part$entity
      spec$part_index <- i
      spec$part_title <- if (length(parts) > 1) part$title
      spec$id <- paste0(part$entity, "__", spec$name)
      if (mode != "update") spec$locked <- NULL
      spec
    })
    fields <- c(fields, specs)
  }
  fields
}

#' What each field starts with.
#'
#' A row's values for an update; for an add the field's default or nothing. Dates are
#' shown as MM/DD/YYYY.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param fields The form's fields, from `edit_service_fields()`.
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param mode `"add"` or `"update"`.
#' @param row The row being updated.
#' @param scope The questionnaire's key, for a kind that belongs to one.
#' @return A named list of strings by field id.
edit_service_initial <- function(entity, fields, snap, mode, row = NULL, scope = NULL) {
  special <- if (mode == "update" && !is.null(entity$initial)) entity$initial(snap, row, scope) else list()
  values <- lapply(fields, function(field) {
    if (mode == "add") {
      return(as.character(field$default %||% ""))
    }
    value <- if (field$name %in% names(special)) special[[field$name]] else row[[field$name]]
    edit_show_value(value, field)
  })
  stats::setNames(values, vapply(fields, function(field) field$id, character(1)))
}

edit_show_value <- function(value, field) {
  if (length(value) == 0 || is.na(value)) {
    return("")
  }
  switch(field$type,
    date = edit_us_date(value),
    number = key_text(value),
    select = key_text(value),
    as.character(value)
  )
}

#' What a field's value is saved as.
#'
#' Spaces trimmed, nothing as `NA`, numbers without commas, dates as YYYY-MM-DD,
#' "TRUE"/"FALSE" as true/false.
#'
#' @param field One of the form's fields.
#' @param value What was typed.
#' @return A string, or `NA_character_` for nothing.
edit_service_normalize <- function(field, value) {
  value <- trimws(value %||% "")
  if (!is.null(field$normalize)) value <- sub("^NA$", "", paste(field$normalize(value))) # an answer choice, as kept
  if (!nzchar(value)) {
    return(NA_character_)
  }
  switch(field$type,
    number = {
      number <- edit_whole_number(value)
      if (is.na(number)) value else key_text(number)
    },
    date = {
      parsed <- if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", value)) as.Date(value, "%m/%d/%Y") else as.Date(NA)
      if (is.na(parsed)) value else format(parsed)
    },
    choice = if (value %in% c("TRUE", "FALSE")) tolower(value) else value,
    several = edit_several_text(edit_several_ids(value)),
    value
  )
}

#' The values picked in a field of type several.
#'
#' @param value What the field holds: the values joined by commas, or (from the page) a vector of them.
#' @return The values, once each, as text, in order (as numbers if they all are).
edit_several_ids <- function(value) {
  ids <- trimws(unlist(strsplit(as.character(value %||% character()), ",", fixed = TRUE)))
  ids <- unique(ids[!is.na(ids) & nzchar(ids)])
  numbers <- suppressWarnings(as.numeric(ids))
  if (length(ids) > 0 && !anyNA(numbers)) ids <- key_text(sort(numbers)) else ids <- sort(ids)
  ids
}

#' Values of a field of type several, as it holds them: joined by commas, in order.
#'
#' @param ids The values.
#' @return A string; `""` for none.
edit_several_text <- function(ids) paste(edit_several_ids(paste(key_text(ids), collapse = ",")), collapse = ",")

# Whether a field matters, given the others: `show_when` hides it otherwise.
edit_service_visible <- function(field, values) {
  if (is.null(field$show_when)) {
    return(TRUE)
  }
  identical(values[[paste0(field$part, "__", field$show_when$field)]] %||% "", field$show_when$value)
}

# The values of one part of the form, by field name, for a field's own checks.
edit_part_values <- function(fields, values, part) {
  mine <- Filter(function(field) field$part == part, fields)
  stats::setNames(lapply(mine, function(field) values[[field$id]] %||% ""), vapply(mine, function(field) field$name, character(1)))
}

# Every field that's shown and can be changed: not hidden, not locked.
edit_service_active <- function(fields, values) {
  Filter(function(field) edit_service_visible(field, values) && is.null(field$locked), fields)
}

# ------ The details ------------------------------------------------------------------------------------------------
#' What a row's details list: its fields as text, each on a line of its own.
#'
#' A field that only matters while another has a value is left out when it doesn't, and so is the one field of a kind
#' that has only one, which is the details' title already (a subject's name). A kind with `details` lists what it
#' belongs to as well (a questionnaire's study), and picks and orders its own fields: `details(snap, row, own)` takes
#' these lines and gives the ones to show.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param row The row.
#' @param fields The form's fields, from `edit_service_fields()` (as an update).
#' @param values What each starts with, from `edit_service_initial()`.
#' @return A data frame of `name` (the field's, `NA` for a line about what the row belongs to), `label`, `value` (as
#'   staff read it; `"(empty)"` for nothing) and `wide`.
edit_service_details <- function(entity, snap, row, fields, values) {
  shown <- Filter(function(field) field$type != "variables" && edit_service_visible(field, values), fields)
  own <- edit_details_lines(
    vapply(shown, function(field) field$name, character(1)),
    vapply(shown, function(field) field$label, character(1)),
    vapply(shown, function(field) {
      value <- values[[field$id]]
      edit_display_value(field, if (is.null(value) || !nzchar(value)) NA else value)
    }, character(1)),
    wide = vapply(shown, function(field) field$type == "textarea", logical(1))
  )
  if (!is.null(entity$details)) {
    return(entity$details(snap, row, own))
  }
  if (nrow(own) == 1) own[0, ] else own
}

#' Lines of a row's details.
#'
#' @param name The fields' names; `NA` for a line about what the row belongs to.
#' @param label What each is.
#' @param value Each as staff read it; `NA` is shown as `"(empty)"`.
#' @param wide Whether each is long text.
#' @return A data frame, as `edit_service_details()` gives it.
edit_details_lines <- function(name, label, value, wide = FALSE) {
  value <- as.character(value)
  data.frame(
    name = as.character(name), label = as.character(label), value = ifelse(is.na(value) | !nzchar(value), "(empty)", value),
    wide = rep_len(as.logical(wide), length(label)), stringsAsFactors = FALSE
  )
}

# The lines named `names`, in that order (any the details don't have are left out).
edit_details_pick <- function(lines, names) {
  lines[stats::na.omit(match(names, lines$name)), , drop = FALSE]
}

# ------ Checking what's typed -----------------------------------------------------------------------------------
#' Check what staff typed.
#'
#' A problem stops the edit being sent; a warning only says.
#'
#' @param fields The form's fields, from `edit_service_fields()`.
#' @param values What is in the form: a named list of strings by field id.
#' @return A list: `missing` (required fields left empty, as their labels), `problems` and
#'   `warnings` (a sentence for each field, by id, that has one), and `missing_ids` (the ids of the
#'   required fields left empty, so the form can outline them in red).
edit_service_check <- function(fields, values) {
  missing <- list()
  problems <- list()
  warnings <- list()
  for (field in edit_service_active(fields, values)) {
    value <- values[[field$id]] %||% ""
    part_values <- edit_part_values(fields, values, field$part)
    blank <- !has_text(value)
    if (blank && field$required) missing[[length(missing) + 1]] <- field
    problem <- if (!blank) edit_type_problem(field, value)
    problem <- problem %||% if (!is.null(field$problem)) field$problem(value, part_values)
    if (!is.null(problem)) problems[[field$id]] <- problem
    warning <- if (!is.null(field$warn) && is.null(problem)) field$warn(value, part_values)
    if (!is.null(warning)) warnings[[field$id]] <- warning
  }
  list(
    missing = edit_missing_labels(missing), problems = problems, warnings = warnings,
    missing_ids = vapply(missing, function(field) field$id, character(1))
  )
}

# The labels of the fields left empty. Where two have the same label (a study's
# name and its questionnaire's), each says what it belongs to.
edit_missing_labels <- function(fields) {
  labels <- vapply(fields, function(field) tolower_words(field$label), character(1))
  twice <- labels %in% labels[duplicated(labels)]
  labels[twice] <- vapply(fields[twice], function(field) {
    paste(edit_entities[[field$part]]$noun, tolower_first(field$label))
  }, character(1))
  labels
}

edit_type_problem <- function(field, value) {
  switch(field$type,
    number = {
      number <- edit_whole_number(value)
      if (is.na(number)) {
        "Type a whole number, e.g. 1,200."
      } else if (!is.null(field$min) && number < field$min) {
        sprintf(
          "Type %s or more.",
          field$min
        )
      }
    },
    date = edit_date_problem(value),
    select = if (!value %in% unname(field$choices)) "Pick one from the list.",
    several = if (!all(edit_several_ids(value) %in% unname(field$choices))) "Pick from the list.",
    NULL
  )
}

#' A problem with a typed date, or `NULL`.
#'
#' @param text What was typed; blank is fine.
#' @return A sentence saying what to do, or `NULL`.
edit_date_problem <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) {
    return(NULL)
  }
  ok <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", text) && !is.na(as.Date(text, "%m/%d/%Y"))
  if (!ok) "Type the date as MM/DD/YYYY, e.g. 04/15/2022."
}

# ------ What changed ------------------------------------------------------------------------------------------------
#' The fields that differ from how the row started.
#'
#' Only for an update. A field that is no longer shown counts as emptied.
#'
#' @param fields The form's fields, from `edit_service_fields()`.
#' @param initial What the fields started with (`edit_service_initial()`).
#' @param values What is in the form: a named list of strings by field id.
#' @return A data frame with `id`, `name`, `label`, `short`, `type`, `old` and `new` (as
#'   shown) and `old_value` and `new_value` (as saved); no rows if nothing changed.
edit_service_changes <- function(fields, initial, values) {
  # A field that's no longer shown (a follow-up's parent, once the question
  # stands on its own) counts as emptied.
  unlocked <- Filter(function(field) is.null(field$locked), fields)
  rows <- lapply(unlocked, function(field) {
    was <- edit_service_normalize(field, initial[[field$id]])
    now <- if (edit_service_visible(field, values)) edit_service_normalize(field, values[[field$id]]) else NA_character_
    if (identical(was, now)) {
      return(NULL)
    }
    data.frame(
      id = field$id, name = field$name, label = field$label, short = field$short, type = field$type,
      old = edit_display_value(field, was), new = edit_display_value(field, now),
      old_value = was, new_value = now, stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(
      id = character(), name = character(), label = character(), short = character(), type = character(),
      old = character(), new = character(), old_value = character(), new_value = character()
    ))
  }
  do.call(rbind, rows)
}

#' A saved value as staff read it.
#'
#' The choice's name, or a date as MM/DD/YYYY.
#'
#' @param field One of the form's fields.
#' @param value The saved value.
#' @return A string; `"(empty)"` for `NA`.
edit_display_value <- function(field, value) {
  if (is.na(value)) {
    return("(empty)")
  }
  switch(field$type,
    select = names(field$choices)[match(value, unname(field$choices))] %||% value,
    several = {
      ids <- edit_several_ids(value)
      paste(ifelse(ids %in% unname(field$choices), names(field$choices)[match(ids, unname(field$choices))], ids), collapse = " / ")
    },
    choice = names(field$choices)[match(toupper(value), unname(field$choices))] %||% value,
    date = edit_us_date(value),
    value
  )
}

# A saved date (YYYY-MM-DD) as MM/DD/YYYY; anything else (a date still being typed) as it is.
edit_us_date <- function(value) {
  parsed <- as.Date(as.character(value), format = "%Y-%m-%d") # NA, not an error, for text that isn't a date
  if (is.na(parsed)) as.character(value) else format(parsed, "%m/%d/%Y")
}

#' Two texts as runs of words kept, taken out and put in.
#'
#' It reads as the new text with the changes marked.
#'
#' @param old The old text.
#' @param new The new text.
#' @return A data frame of `kind` (`"same"`, `"del"` or `"ins"`) and `text`.
edit_service_word_diff <- function(old, new) {
  tokens <- function(text) {
    text <- text %||% ""
    if (is.na(text) || !nzchar(text)) {
      return(character())
    }
    regmatches(text, gregexpr("[[:space:]]+|[^[:space:]]+", text))[[1]]
  }
  a <- tokens(old)
  b <- tokens(new)
  n <- length(a)
  m <- length(b)
  longest <- matrix(0L, n + 2, m + 2)
  for (i in rev(seq_len(n))) {
    for (j in rev(seq_len(m))) {
      longest[i, j] <- if (a[i] == b[j]) longest[i + 1, j + 1] + 1L else max(longest[i + 1, j], longest[i, j + 1])
    }
  }
  kinds <- character()
  texts <- character()
  i <- 1
  j <- 1
  while (i <= n && j <= m) {
    if (a[i] == b[j]) {
      kinds <- c(kinds, "same")
      texts <- c(texts, a[i])
      i <- i + 1
      j <- j + 1
    } else if (longest[i + 1, j] >= longest[i, j + 1]) {
      kinds <- c(kinds, "del")
      texts <- c(texts, a[i])
      i <- i + 1
    } else {
      kinds <- c(kinds, "ins")
      texts <- c(texts, b[j])
      j <- j + 1
    }
  }
  if (i <= n) {
    kinds <- c(kinds, rep("del", n - i + 1))
    texts <- c(texts, a[i:n])
  }
  if (j <= m) {
    kinds <- c(kinds, rep("ins", m - j + 1))
    texts <- c(texts, b[j:m])
  }
  if (length(kinds) == 0) {
    return(data.frame(kind = character(), text = character()))
  }
  runs <- cumsum(c(TRUE, kinds[-1] != kinds[-length(kinds)]))
  data.frame(kind = tapply(kinds, runs, `[`, 1), text = tapply(texts, runs, paste, collapse = ""), row.names = NULL)
}

# ------ Writing a change as items ----------------------------------------------------------------------------------------
#' What an add, update or delete comes to.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param mode `"add"`, `"update"` or `"delete"`.
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param row The row being updated or deleted.
#' @param scope The questionnaire's key, for a kind that belongs to one.
#' @param fields The form's fields, from `edit_service_fields()`. Not for a delete.
#' @param values What is in the form: a named list of strings by field id. Not for a delete.
#' @param plan For a delete, the plan if it is already worked out.
#' @return A list: `items` (numbered, in the order to apply them) and `summary` (one
#'   sentence for the proposal); a delete also has its `plan`. An error with a sentence if an add or an
#'   update would change something while its form still has a field to fill in or fix: the page only builds
#'   a checked form, but a caller that builds one itself (`edit_service_quick_add()`) is checked here too.
edit_service_build <- function(entity, mode, snap, row = NULL, scope = NULL, fields = NULL, values = NULL, plan = NULL) {
  if (mode == "delete") {
    plan <- plan %||% edit_service_delete_plan(entity, snap, row)
    return(list(
      items = edit_number_items(plan$items), plan = plan,
      summary = edit_summary_delete(entity, snap, row, plan)
    ))
  }
  items <- if (!is.null(entity$build)) {
    entity$build(mode, snap, row, scope, fields, values)
  } else if (mode == "add") {
    edit_add_items(entity, snap, fields, values)
  } else {
    edit_update_items(entity, snap, row, fields, values)
  }
  # an update that changes nothing writes nothing, whatever its form holds
  if (NROW(items) > 0) edit_refuse_unchecked(edit_service_check(fields, values))
  summary <- if (mode != "add" && !is.null(entity$summary_update)) {
    entity$summary_update(snap, row, fields, values)
  } else if (mode != "add") {
    edit_summary_update(entity, snap, row, fields, values)
  } else if (!is.null(entity$summary_add)) {
    entity$summary_add(snap, scope, fields, values)
  } else {
    edit_summary_add(entity, snap, fields, values)
  }
  list(items = edit_number_items(items), summary = summary)
}

# Stops with what is left to fill in or fix, if anything is.
edit_refuse_unchecked <- function(found) {
  left <- c(
    if (length(found$missing) > 0) paste0("Fill in the ", word_list(found$missing), "."),
    unlist(found$problems, use.names = FALSE)
  )
  if (length(left) > 0) stop(paste(left, collapse = " "), call. = FALSE)
}

# ------ * Add ------------------------------------------------------------------------------------------------------------
# One Insert item per field filled in, for each part in turn; a part's link
# holds the new row before it.
edit_add_items <- function(entity, snap, fields, values) {
  parts <- entity$parts %||% list(list(entity = entity$key))
  made <- character()
  items <- list()
  for (i in seq_along(parts)) {
    part <- parts[[i]]
    table <- edit_entities[[part$entity]]$table
    token <- sprintf("new:%s:%d", table, sum(vapply(parts[seq_len(i)], function(p) edit_entities[[p$entity]]$table == table, logical(1))))
    key <- edit_table_key(table)
    identifier <- edit_identifier(stats::setNames(list(token), key[1]))
    if (!is.null(part$link)) {
      items[[length(items) + 1]] <- edit_item(table, identifier, "Insert", part$link, NA, made[i - 1])
    }
    for (field in Filter(function(field) field$part == part$entity, edit_service_active(fields, values))) {
      value <- edit_service_normalize(field, values[[field$id]])
      if (!is.na(value)) items[[length(items) + 1]] <- edit_item(table, identifier, "Insert", field$name, NA, value)
    }
    made[i] <- token
  }
  do.call(rbind, items)
}

# ------ * Update ----------------------------------------------------------------------------------------------------------
#' One Update item per field that differs.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param row The row being updated.
#' @param fields The form's fields, from `edit_service_fields()`.
#' @param values What is in the form: a named list of strings by field id.
#' @return A data frame of items; see `edit_item()`.
edit_update_items <- function(entity, snap, row, fields, values) {
  initial <- edit_service_initial(entity, fields, snap, "update", row)
  changes <- edit_service_changes(fields, initial, values)
  identifier <- edit_row_identifier(entity$table, row)
  items <- lapply(seq_len(nrow(changes)), function(i) {
    edit_item(entity$table, identifier, "Update", changes$name[i], changes$old_value[i], changes$new_value[i])
  })
  if (length(items) == 0) NULL else do.call(rbind, items)
}

# ------ * Delete ------------------------------------------------------------------------------------------------------------
#' What a delete comes to.
#'
#' What it reaches, what stops it, and the items. The kind says where it starts (a
#' question's placement and its settings are one thing).
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param row The row being deleted.
#' @return A list: `cascade`, `reach`, `blocked` (empty when it can go ahead), `items`,
#'   `far_reaching` (whether anything beyond the row itself is affected) and `deleted` (the row picked:
#'   `table`, `row`, its key as JSON, and `name`, see `edit_deleted_name()`), which is all the Edit History keeps
#'   of a delete once it is applied.
edit_service_delete_plan <- function(entity, snap, row) {
  start <- if (!is.null(entity$delete_start)) {
    entity$delete_start(snap, row)
  } else {
    stats::setNames(list(edit_key(row, edit_table_key(entity$table))), entity$table)
  }
  cascade <- edit_service_cascade(snap, start)
  blocked <- c(if (!is.null(entity$extra_blockers)) entity$extra_blockers(snap, row), cascade$blocked)
  reach <- edit_service_reach(snap, cascade)
  list(
    cascade = cascade, reach = reach, blocked = blocked,
    items = edit_delete_items(snap, cascade),
    # whether anything beyond the row itself is affected
    far_reaching = reach$total > 0 || length(reach$changed) > 0,
    deleted = list(table = entity$table, row = edit_row_identifier(entity$table, row), name = edit_deleted_name(snap, entity$table, row))
  )
}

# What the Edit History calls a row once it is deleted ("Deleted Kid Questionnaire Wave 1"): a wave by its
# questionnaire's name and its own, anything else as Make an edit names it.
edit_deleted_name <- function(snap, table, row) {
  if (table == "wave") {
    questionnaire <- edit_lookup(snap, "questionnaire", "questionnaire_id", row$questionnaire_id, "name")
    if (!is.na(questionnaire)) return(paste(questionnaire, row$name))
  }
  edit_label(snap, table, row)
}

# Every field of every row that goes, children before parents, and first any
# reference to a row that's going (a study's contact) emptied. A wave that asks
# fewer questions numbers the rest again (edit_wave_numbers()).
edit_delete_items <- function(snap, cascade) {
  items <- list()
  for (clear in cascade$cleared) {
    for (row in split(edit_rows(snap, clear$table, clear$keys), seq_along(clear$keys))) {
      for (column in clear$columns) {
        items[[length(items) + 1]] <- edit_item(clear$table, edit_row_identifier(clear$table, row), "Update", column, row[[column]], NA)
      }
    }
  }
  for (table in intersect(edit_delete_order, names(cascade$removed))) {
    rows <- edit_children_first(edit_rows(snap, table, cascade$removed[[table]]), table)
    others <- setdiff(names(database_tables[[table]]$columns), database_tables[[table]]$key)
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, , drop = FALSE]
      identifier <- edit_row_identifier(table, row)
      if (length(others) == 0) {
        items[[length(items) + 1]] <- edit_item(table, identifier, "Delete", NA, NA, NA)
      }
      for (column in others) {
        items[[length(items) + 1]] <- edit_item(table, identifier, "Delete", column, row[[column]], NA)
      }
    }
  }
  items <- c(items, list(edit_renumber_after_delete(snap, cascade)))
  items <- Filter(Negate(is.null), items)
  if (length(items) == 0) NULL else do.call(rbind, items)
}

# The questions left in each wave that loses some, numbered 1, 2, 3... again in the order they had.
edit_renumber_after_delete <- function(snap, cascade) {
  gone <- cascade$removed$wave_question
  if (length(gone) == 0 || is.null(snap$wave_question)) {
    return(NULL)
  }
  asked <- snap$wave_question
  is_gone <- edit_key(asked, edit_table_key("wave_question")) %in% gone
  touched <- unique(asked[is_gone, c("questionnaire_id", "wave_id"), drop = FALSE])
  items <- lapply(seq_len(nrow(touched)), function(i) {
    q <- touched$questionnaire_id[i]
    wave <- touched$wave_id[i]
    left <- asked[!is_gone & asked$questionnaire_id == q & asked$wave_id == wave, , drop = FALSE]
    left <- left[order(suppressWarnings(as.numeric(left$question_number))), , drop = FALSE]
    edit_wave_numbers(snap, q, wave, key_text(left$component_id))$items
  })
  items <- Filter(Negate(is.null), items)
  if (length(items) > 0) do.call(rbind, items)
}

# Rows that hold others (a section's contents, a question's follow-ups) are
# deleted after what's inside them.
edit_children_first <- function(rows, table) {
  parent_column <- c(questionnaire_component = "parent_component_id", questionnaire_question = "parent_question_id")[table]
  if (is.na(parent_column) || nrow(rows) < 2) {
    return(rows)
  }
  depth <- edit_chain_depth(rows$component_id, rows[[parent_column]], rows$questionnaire_id)
  rows[order(-depth), , drop = FALSE]
}

# How many parents each row has above it, in its own questionnaire.
edit_chain_depth <- function(ids, parents, questionnaires) {
  depth <- numeric(length(ids))
  for (i in seq_along(ids)) {
    p <- parents[i]
    while (!is.na(p)) {
      depth[i] <- depth[i] + 1
      next_row <- which(ids == p & questionnaires == questionnaires[i])
      if (length(next_row) != 1) break
      p <- parents[next_row]
    }
  }
  depth
}

# ------ Sentences for the proposal ----------------------------------------------------------------------------------------------
# The first thing typed that names the row, e.g. its name or wording.
edit_new_title <- function(fields, values) {
  first_part <- Filter(function(field) field$part_index == 1 && field$type %in% c("text", "textarea"), edit_service_active(fields, values))
  for (field in first_part) {
    value <- trimws(values[[field$id]] %||% "")
    if (nzchar(value)) {
      return(shorten(value, 60))
    }
  }
  "(new)"
}

edit_summary_add <- function(entity, snap, fields, values) {
  paste0("Added the ", entity$noun, " “", edit_new_title(fields, values), "”")
}

edit_summary_update <- function(entity, snap, row, fields, values) {
  initial <- edit_service_initial(entity, fields, snap, "update", row)
  changes <- edit_service_changes(fields, initial, values)
  title <- shorten(edit_label(snap, entity$table, row), 80) # a summary is a line in a list
  paste0(
    "Changed the ", word_list(unique(changes$short)), " of the ", entity$noun, " “", title, "”"
  )
}

edit_summary_delete <- function(entity, snap, row, plan) {
  title <- shorten(edit_label(snap, entity$table, row), 80) # a summary is a line in a list
  total <- plan$reach$total
  paste0(
    "Deleted the ", entity$noun, " “", title, "”",
    if (total > 0) paste0(" and ", edit_count(total, "connected record"))
  )
}

# ------ Where a row is used ------------------------------------------------------------------------------------------------------
#' What changing a row reaches.
#'
#' For a component or question, every questionnaire that prints it; for anything else, the
#' rows that point at it.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param row The row being changed.
#' A kind with `usage_rows(snap, row, table, rows)` says how it lists the rows of `table` that point at it: a list of
#' `rows` (the ones to list, in order), `labels` and, if it likes, `text`; or `NULL` for the usual way, by name.
#'
#' @return A list: `uses`, `shared` (how many questionnaires print a component) and `pointing`, one list for each
#'   kind of row that points at it: `table`, `n`, `text` ("3 variables"), `labels` (every row's name, in order) and
#'   `groups`, the rows by the questionnaire they are in (see `edit_usage_groups()`), or `NULL` when they are all in
#'   the row's own questionnaire.
edit_service_usage <- function(entity, snap, row) {
  if (entity$table == "component") {
    uses <- edit_service_uses(snap, row$component_id)
    return(list(uses = uses, shared = length(uses), pointing = list()))
  }
  key <- edit_key(row, edit_table_key(entity$table))
  pointing <- lapply(Filter(function(relation) relation$parent == entity$table, edit_relations), function(relation) {
    hits <- edit_referencing(snap, relation, key)
    if (length(hits) == 0 || relation$child %in% edit_hidden_tables) {
      return(NULL)
    }
    rows <- edit_rows(snap, relation$child, hits)
    listed <- if (!is.null(entity$usage_rows)) entity$usage_rows(snap, row, relation$child, rows)
    if (is.null(listed)) {
      labels <- edit_label(snap, relation$child, rows)
      by_name <- order(tolower(labels), method = "radix")
      listed <- list(rows = rows[by_name, , drop = FALSE], labels = labels[by_name])
    }
    if (nrow(listed$rows) == 0) {
      return(NULL)
    }
    groups <- edit_usage_groups(snap, relation$child, listed$rows, listed$labels)
    own <- if (entity$table == "questionnaire") row$questionnaire_id else row$questionnaire_id %||% NA
    if (length(groups) == 1 && length(own) == 1 && !is.na(own) && isTRUE(groups[[1]]$id == own)) groups <- NULL
    list(
      table = relation$child, n = nrow(listed$rows), text = listed$text %||% plural_noun(nrow(listed$rows), relation$child),
      labels = listed$labels, groups = groups
    )
  })
  list(uses = list(), shared = 0, pointing = Filter(Negate(is.null), pointing))
}

#' Rows that point at something, by the questionnaire each is in.
#'
#' A row is in a questionnaire if it holds one (a variable, a wave) or its variable does (a variable's choice).
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param table The rows' table.
#' @param rows The rows.
#' @param labels Their names, from `edit_label()`.
#' @return A list with one list per questionnaire, the most rows first: `id`, `questionnaire` (its name, or "Not in
#'   a questionnaire") and `labels`, in the order given. `NULL` for rows that can't be in a questionnaire, or are
#'   questionnaires.
edit_usage_groups <- function(snap, table, rows, labels) {
  ids <- if (table == "questionnaire") {
    NULL
  } else if ("questionnaire_id" %in% names(rows)) {
    rows$questionnaire_id
  } else if ("variable_key" %in% names(rows) && !is.null(snap$variable)) {
    edit_lookup(snap, "variable", "variable_key", rows$variable_key, "questionnaire_id")
  }
  if (is.null(ids) || is.null(snap$questionnaire)) {
    return(NULL)
  }
  named <- snap$questionnaire[match(ids, snap$questionnaire$questionnaire_id), , drop = FALSE]
  names <- ifelse(is.na(named$questionnaire_id), "Not in a questionnaire", edit_label(snap, "questionnaire", named))
  by_name <- split(seq_along(labels), factor(names, unique(names)))
  by_name <- by_name[order(-lengths(by_name), names(by_name))]
  unname(Map(function(name, these) {
    list(id = named$questionnaire_id[these[1]], questionnaire = name, labels = labels[these])
  }, names(by_name), by_name))
}

#' Who each variable is about and what it records, as the questionnaire's preview shows them: "[Smokes now]
#' [Respondent]", each part when it has one.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param variables Rows of `variable`.
#' @return One string per variable; `""` for one with neither.
edit_variable_tags <- function(snap, variables) {
  about <- if (is.null(snap$subject)) rep(NA_character_, nrow(variables)) else edit_lookup(snap, "subject", "subject_id", variables$subject_id, "name")
  vapply(seq_len(nrow(variables)), function(i) preview_variable_tags(list(label = variables$label[i], subject = about[i])), character(1))
}

# ------ What's left to do ------------------------------------------------------------------------------------------------------------
#' What's left to do before a change can be sent.
#'
#' In the order to do it; nothing when it can go.
#'
#' @param mode `"add"`, `"update"` or `"delete"`.
#' @param found `edit_service_check()`'s `missing` and `problems`.
#' @param n_changes How many fields an update changes.
#' @param shared How many questionnaires use what an update changes.
#' @param plan The delete plan from `edit_service_delete_plan()`.
#' @param confirmed What staff ticked and typed: `list(read, word, shared)`.
#' @return A character vector of sentences; empty when it is ready.
edit_service_blockers <- function(mode, found = NULL, n_changes = 0, shared = 0, plan = NULL, confirmed = list()) {
  out <- character()
  if (mode == "delete") {
    if (length(plan$blocked) > 0) {
      return("This can't be deleted yet: see what's in the way above.")
    }
    if (!isTRUE(confirmed$read)) out <- c(out, "Tick the box to confirm you've read what will be deleted.")
    if (plan$far_reaching && !identical(toupper(trimws(confirmed$word %||% "")), "DELETE")) {
      out <- c(out, "Type DELETE to be sure.")
    }
    return(out)
  }
  if (length(found$missing) > 0) out <- c(out, paste0("Fill in the ", word_list(found$missing), "."))
  if (length(found$problems) > 0) out <- c(out, "Fix the fields outlined in red.")
  if (mode == "update" && n_changes == 0) out <- c(out, "Change something to continue.")
  if (mode == "update" && shared >= 2 && !isTRUE(confirmed$shared)) {
    out <- c(out, "Tick the box to confirm it changes everywhere it's used.")
  }
  out
}

#' What a form that still has things to fix is saved as.
#'
#' A draft as typed, with no items.
#'
#' @param verb `"add"`, `"update"` or `"delete"`.
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @return A list with `summary` and `items` (`NULL`), like `edit_service_build()`.
edit_service_unfinished <- function(verb, entity) {
  list(summary = paste0("Unfinished: ", verb, " ", entity$article, " ", entity$noun), items = NULL)
}

# ------ Proposals ------------------------------------------------------------------------------------------------------------------
# A proposal is open until it is applied, withdrawn or discarded. It is a draft of kind Edit
# (model_edit.R): Draft while being prepared, Awaiting Review once sent for approval, Sent Back if a
# reviewer returned it. An add or an update is applied at once; a STAFF member's delete is sent for
# approval (service_change_request.R).
edit_open_statuses <- c("Draft", "Sent Back", "Awaiting Review")

#' A proposal as it is saved.
#'
#' @param me Who is preparing it: `list(staff_id, name, role)`.
#' @param form How to open it again on the form: `action`, `entity`, `scope`, `target`, `values`.
#' @param built The result of `edit_service_build()`, or `edit_service_unfinished()`.
#' @param status `"Draft"`, or `"Awaiting Review"` to send it for approval. A proposal that was
#'   sent back, or is waiting for review, keeps that status when it is only saved again.
#' @param previous The record being saved again, if any; it keeps its id, preparer and start time.
#' @param reviewer The staff id of the person asked to review it, or `NULL` for no one in particular.
#' @param now The time to record.
#' @return A record; see `edit_model_get_all()`. `id` is `NULL` until it is first saved.
edit_service_record <- function(me, form, built, status, previous = NULL, reviewer = NULL, now = Sys.time()) {
  # A saved change keeps the status it has, unless it is being sent for review.
  if (status == "Draft" && !is.null(previous$status) && previous$status %in% c("Sent Back", "Awaiting Review")) {
    status <- previous$status
  }
  list(
    id = previous$id, # NULL until saved for the first time: the database numbers it
    kind = "Edit",
    edit_kind = edit_model_kind(form),
    status = status,
    prepared_by = previous$prepared_by %||% me$name,
    prepared_by_id = previous$prepared_by_id %||% me$staff_id,
    started = previous$started %||% format_time(now),
    saved = format_time(now),
    expected_revision = previous$revision,
    summary = built$summary,
    requested_reviewer = if (!is.null(reviewer) && nzchar(as.character(reviewer))) {
      list(id = as.numeric(reviewer))
    } else if (status == "Awaiting Review") {
      NULL
    } else {
      previous$requested_reviewer
    },
    form = form,
    reach = if (!is.null(built$plan)) {
      list(
        rows_removed = built$plan$reach$total,
        rows_emptied = sum(vapply(built$plan$reach$changed, function(change) change$n, numeric(1))),
        # the row picked: once the delete is applied, it is all applied_item keeps of it (apply_draft_items())
        deleted = built$plan$deleted
      )
    },
    items = built$items
  )
}

#' Other open proposals that change a row this one does.
#'
#' Staff should talk before both are applied.
#'
#' @param records The saved proposals.
#' @param items This proposal's items.
#' @param except The id of this proposal, so it doesn't clash with itself.
#' @return The records that overlap.
edit_service_conflicts <- function(records, items, except = NULL) {
  if (is.null(items) || nrow(items) == 0) {
    return(list())
  }
  # a row an edit adds is named new:<table>:1 in every edit, so it is no other edit's row
  rows_of <- function(items) {
    named <- unique(paste(items$entity_name, items$record_identifier))
    named[!grepl('"new:[a-z_]+:[0-9]+"', named)]
  }
  mine <- rows_of(items)
  Filter(function(record) {
    open <- record$status %in% edit_open_statuses && !identical(record$id, except)
    if (!open || NROW(record$items) == 0) {
      return(FALSE)
    }
    any(rows_of(record$items) %in% mine)
  }, records)
}

#' The saved proposals still open.
#'
#' @param records The saved proposals.
#' @param prepared_by_id Only those prepared by this staff member, if given.
#' @param status The statuses that count as open.
#' @return The matching records.
edit_service_open <- function(records, prepared_by_id = NULL, status = edit_open_statuses) {
  draft_row_service_open(records, status, prepared_by_id)
}

#' The saved proposals, newest first.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of records named by id; see `edit_model_get_all()`.
edit_service_get_all <- function(connection) {
  edit_model_get_all(connection)
}

#' One saved proposal.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The proposal's id.
#' @return The record, as in `edit_model_get_all()`. An error if there is none.
edit_service_get <- function(connection, id) {
  edit_model_get(connection, id)
}

#' Save a proposal, new or changed.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record A proposal record from `edit_service_record()`.
#' @param me Who saves it: `list(staff_id, name, role)`.
#' @return The record as saved; a new one now has its id. An error with a sentence if the
#'   database refused, e.g. because someone saved a newer version since this one was opened.
edit_service_save <- function(connection, record, me) {
  edit_model_save(connection, record, me$staff_id)
}

#' Send a proposal for approval.
#'
#' It waits in Change Requests until one other staff member approves it, which applies it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record A proposal record from `edit_service_record()`, with status `"Awaiting Review"`.
#' @param me Who sends it: `list(staff_id, name, role)`.
#' @return The record as saved.
edit_service_send <- function(connection, record, me) {
  edit_model_save(connection, record, me$staff_id)
}

#' Apply a saved proposal now.
#'
#' For a change that needs no approval (an add or an update, or anything an ADMIN does). Save it first
#' (`edit_service_save()`), so if the database refuses it (a row changed since the form was opened,
#' a rule of the database) the person's work is still there as a saved edit.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record A saved proposal record (it has an `id`).
#' @param me Who applies it: `list(staff_id, name, role)`; it has to be the person who prepared it.
#' @return The id of its row in the Edit History. An error with the database's own sentence if it
#'   refused.
edit_service_apply <- function(connection, record, me) {
  if (change_request_service_needs_approval(me, edit_draft_kind, record$edit_kind)) {
    stop("This needs approval from another staff member before it is applied.", call. = FALSE)
  }
  change_request_model_apply(connection, record$id, me$staff_id)
}

#' What staff chose to do, as the history writes it.
#'
#' @param verb `"add"`, `"update"` or `"delete"`.
#' @return `"Add"`, `"Update"` or `"Delete"`.
edit_service_kind_of <- function(verb) edit_model_kind(list(action = verb))

#' Discard a proposal.
#'
#' The record is kept, as Withdrawn, so it stays in the history.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The proposal's id.
#' @param me Who discards it: `list(staff_id, name, role)`.
#' @return Called for its effect.
edit_service_withdraw <- function(connection, id, me) {
  change_request_model_withdraw(connection, id, me$staff_id)
}

# ------ Something like it is there already ---------------------------------------------------------------
#' The rows an add looks like.
#'
#' When someone adds something that is already there, the form says so before it saves, and shows what
#' is there; it is a warning, not a block. Each kind says what counts as the same (`duplicates` in its
#' `service_edit_entities_*.R` file): its name for most, a respondent type's age range, and so on. Capitals and
#' extra spaces don't count.
#'
#' @param entity A kind of thing staff can edit: one entry of `edit_entities` (see `service_edit_kinds.R`).
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param fields The form's fields, from `edit_service_fields()`.
#' @param values What is in the form: a named list of strings by field id.
#' @return A list with one entry for each way it looks like something there: `entity` (the kind of what
#'   is there), `why` (a sentence) and `cards` (a data frame of the rows, as the cards show them; see
#'   `edit_cards()`). Empty if it looks like nothing.
edit_service_duplicates <- function(entity, snap, fields, values) {
  parts <- entity$parts %||% list(list(entity = entity$key))
  found <- list()
  for (part in parts) {
    kind <- edit_entities[[part$entity]]
    if (is.null(kind$duplicates)) next
    typed <- edit_part_values(fields, values, part$entity)
    for (match in kind$duplicates(snap, typed)) {
      rows <- match$rows
      cards <- kind$cards(snap, NULL)
      cards <- cards[cards$key %in% edit_key(rows, edit_table_key(kind$table)), , drop = FALSE]
      if (nrow(cards) > 0) found[[length(found) + 1]] <- list(entity = kind, why = match$why, cards = cards)
    }
  }
  found
}

# The rows of `table` whose `column` is what was typed, ignoring capitals and spaces at either end:
# one way a new row can look like one that is there. NULL when nothing was typed or nothing matches.
edit_duplicate_rows <- function(snap, table, column, value, why, within = NULL) {
  if (!has_text(value)) {
    return(NULL)
  }
  same <- edit_taken(snap, table, column, value, within = within)
  if (nrow(same) > 0) list(rows = same, why = why)
}
