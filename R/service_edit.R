# ------ Making an edit ----------------------------------------------------------------------
# What the Make an edit page does, apart from drawing it: reads what it needs from the catalog's tables, lists
# the rows staff can pick, checks what they type, works out what a change reaches, and applies the change.
#
# A change is a list of items, one per field changed, in the shape the database's apply_change() takes
# (sql/4_functions.sql):
#   Update  one per field that differs, with its old and new value
#   Insert  one per field of each new row
#   Delete  one per field of the row deleted (what goes with it, the database takes along itself)
# Items are numbered in the order they must be applied: what's new before what points at it.
#
# A row an edit adds has no ID yet. Its items name it `new:<table>:<n>` (new:study:1), and so does anything
# pointing at it; the database swaps in the IDs it gives and hands them back.

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

# One item of a change: one change to one field of one row.
# entity_name: The table.
# record_identifier: The row, from `edit_identifier()`.
# action: `"Insert"`, `"Update"` or `"Delete"`.
# attribute_name: The column, or `NULL` for a whole row.
# old_value: What it was (an Update or Delete).
# new_value: What it becomes (an Insert or Update).
# Returns: A one-row data frame.
edit_item <- function(entity_name, record_identifier, action, attribute_name, old_value, new_value) {
  data.frame(
    entity_name = entity_name, record_identifier = record_identifier, action = action,
    attribute_name = attribute_name %||% NA_character_,
    old_value = item_text(old_value), new_value = item_text(new_value),
    stringsAsFactors = FALSE
  )
}

# Name a row by its key columns.
# key: A named list of the key columns to their values.
# Returns: JSON with the columns in alphabetical order, e.g. `{"placement_id":88,"wave_id":3}`.
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
# Every table the kind needs: its rows, what they point at and what points at them, so a card can count and a
# delete can say what goes.
edit_service_tables <- function(entity) {
  parents <- unique(c(
    vapply(Filter(function(relation) relation$child == entity$table, edit_relations), function(relation) relation$parent, character(1)),
    entity$scope$table
  ))
  unique(c(
    edit_tables_to_read(c(entity$table, entity$scope$table)), parents,
    unlist(edit_label_needs[parents]), unlist(edit_choice_needs[parents]), entity$reads,
    "questionnaire" # where a row appears is grouped by questionnaire
  ))
}

# A drop-down names a study with its registry, and a questionnaire with its study.
edit_choice_needs <- list(study = "registry", questionnaire = "study")

# Read the tables a kind needs, afresh.
# Returns: A list of data frames named after the tables.
edit_service_load <- function(connection, entity) {
  tables <- edit_service_tables(entity)
  stats::setNames(lapply(tables, function(table) table_model_get(connection, table)), tables)
}

# Lower-case the first letter.
# text: A string.
# Returns: `text` with its first letter in lower case.
tolower_first <- function(text) paste0(tolower(substr(text, 1, 1)), substring(text, 2))

# Lower-case a label for the middle of a sentence.
# A word that starts with a capital and goes on in lower case (Respondent Type) is lower-cased; an
# acronym (MATR ID) is left as it is.
# text: A string, e.g. a field's label.
# Returns: `text`, e.g. `"respondent type"`.
tolower_words <- function(text) {
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  paste(ifelse(grepl("^[A-Z][a-z]", words), tolower_first(words), words), collapse = " ")
}

# Why a kind can't be added, updated or deleted here.
# Returns: A sentence, or `NULL` if it can.
edit_service_can <- function(entity, snap, mode) {
  if (!is.null(entity$can)) entity$can(snap, mode)
}

# ------ Finding the row -----------------------------------------------------------------------------------
# The cards staff pick from.
# Narrowed to those that have every word in `query`.
# query: Words typed to find a card.
# Returns: A data frame of cards; see `edit_cards()`.
edit_service_cards <- function(entity, snap, scope = NULL, query = "") {
  edit_service_filter(entity$cards(snap, scope), query)
}

# The cards with every one of the words typed.
# Building the cards counts things, so the page builds them once and filters as staff type.
# cards: A data frame of cards.
# query: Words typed to find a card.
# Returns: The matching cards.
edit_service_filter <- function(cards, query = "") {
  words <- strsplit(tolower(trimws(query %||% "")), "[[:space:]]+")[[1]]
  words <- words[nzchar(words)]
  for (word in words) cards <- cards[grepl(word, cards$search, fixed = TRUE), , drop = FALSE]
  cards
}

# A kind's row by its key.
# key: The row's key, as text (`edit_key()`).
# Returns: A one-row data frame, or `NULL`.
edit_service_row <- function(entity, snap, key) {
  rows <- snap[[entity$table]]
  found <- rows[edit_key(rows, edit_table_key(entity$table)) %in% key, , drop = FALSE]
  if (nrow(found) == 1) found
}

# ------ The form ---------------------------------------------------------------------------------------------
# The fields of an add or update form.
# Each has an `id` (kind__field) that is also its input's name.
# Returns: A list of fields, each a list with `id`, `name`, `label`, `type` and more.
edit_service_fields <- function(entity, snap, mode, row = NULL, scope = NULL) {
  specs <- entity$fields(snap, mode, row, scope)
  lapply(specs, function(spec) {
    spec$part <- entity$key
    spec$id <- paste0(entity$key, "__", spec$name)
    if (mode != "update") spec$locked <- NULL
    spec
  })
}

# What each field starts with.
# A row's values for an update; for an add the field's default or nothing. Dates are
# shown as MM/DD/YYYY.
# Returns: A named list of strings by field id.
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

# What a field's value is saved as.
# Spaces trimmed, nothing as `NA`, numbers without commas, dates as YYYY-MM-DD,
# "TRUE"/"FALSE" as true/false.
# field: One of the form's fields.
# value: What was typed.
# Returns: A string, or `NA_character_` for nothing.
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

# The values picked in a field of type several.
# value: What the field holds: the values joined by commas, or (from the page) a vector of them.
# Returns: The values, once each, as text, in order (as numbers if they all are).
edit_several_ids <- function(value) {
  ids <- trimws(unlist(strsplit(as.character(value %||% character()), ",", fixed = TRUE)))
  ids <- unique(ids[!is.na(ids) & nzchar(ids)])
  numbers <- suppressWarnings(as.numeric(ids))
  if (length(ids) > 0 && !anyNA(numbers)) ids <- key_text(sort(numbers)) else ids <- sort(ids)
  ids
}

# Values of a field of type several, as it holds them: joined by commas, in order.
# ids: The values.
# Returns: A string; `""` for none.
edit_several_text <- function(ids) paste(edit_several_ids(paste(key_text(ids), collapse = ",")), collapse = ",")

# Whether a field matters, given the others: `show_when` hides it otherwise.
edit_service_visible <- function(field, values) {
  if (is.null(field$show_when)) {
    return(TRUE)
  }
  identical(values[[paste0(field$part, "__", field$show_when$field)]] %||% "", field$show_when$value)
}

# The values of the form, by field name, for a field's own checks.
edit_part_values <- function(fields, values) {
  stats::setNames(lapply(fields, function(field) values[[field$id]] %||% ""), vapply(fields, function(field) field$name, character(1)))
}

# Every field that's shown and can be changed: not hidden, not locked.
edit_service_active <- function(fields, values) {
  Filter(function(field) edit_service_visible(field, values) && is.null(field$locked), fields)
}

# ------ The details ------------------------------------------------------------------------------------------------
# What a row's details list: its fields as text, each on a line of its own.
# A field that only matters while another has a value is left out when it doesn't, and so is the one field of a kind
# that has only one, which is the details' title already (an answer choice's words). A kind with `details` lists
# what it belongs to as well (a questionnaire's study), and picks and orders its own fields: `details(snap, row,
# own)` takes these lines and gives the ones to show.
# row: The row.
# fields: The form's fields, from `edit_service_fields()` (as an update).
# values: What each starts with, from `edit_service_initial()`.
# Returns: A data frame of `name` (the field's, `NA` for a line about what the row belongs to), `label`, `value` (as
#   staff read it; `"(empty)"` for nothing) and `wide`.
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

# Lines of a row's details.
# name: The fields' names; `NA` for a line about what the row belongs to.
# label: What each is.
# value: Each as staff read it; `NA` is shown as `"(empty)"`.
# wide: Whether each is long text.
# Returns: A data frame, as `edit_service_details()` gives it.
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
# Check what staff typed.
# A problem stops the edit being applied; a warning only says.
# Returns: A list: `missing` (required fields left empty, as their labels), `problems` and
#   `warnings` (a sentence for each field, by id, that has one), and `missing_ids` (the ids of the
#   required fields left empty, so the form can outline them in red).
edit_service_check <- function(fields, values) {
  missing <- list()
  problems <- list()
  warnings <- list()
  part_values <- edit_part_values(fields, values)
  for (field in edit_service_active(fields, values)) {
    value <- values[[field$id]] %||% ""
    blank <- !has_text(value)
    if (blank && field$required) missing[[length(missing) + 1]] <- field
    problem <- if (!blank) edit_type_problem(field, value)
    problem <- problem %||% if (!is.null(field$problem)) field$problem(value, part_values)
    if (!is.null(problem)) problems[[field$id]] <- problem
    warning <- if (!is.null(field$warn) && is.null(problem)) field$warn(value, part_values)
    if (!is.null(warning)) warnings[[field$id]] <- warning
  }
  list(
    missing = vapply(missing, function(field) tolower_words(field$label), character(1)), problems = problems, warnings = warnings,
    missing_ids = vapply(missing, function(field) field$id, character(1))
  )
}

edit_type_problem <- function(field, value) {
  switch(field$type,
    number = {
      number <- edit_whole_number(value)
      if (is.na(number)) {
        "Type a whole number, e.g. 1,200."
      } else if (!is.null(field$min) && number < field$min) {
        sprintf("Type %s or more.", field$min)
      }
    },
    date = edit_date_problem(value),
    select = if (!value %in% unname(field$choices)) "Pick one from the list.",
    several = if (!all(edit_several_ids(value) %in% unname(field$choices))) "Pick from the list.",
    NULL
  )
}

# A problem with a typed date, or `NULL`.
# text: What was typed; blank is fine.
# Returns: A sentence saying what to do, or `NULL`.
edit_date_problem <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) {
    return(NULL)
  }
  ok <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", text) && !is.na(as.Date(text, "%m/%d/%Y"))
  if (!ok) "Type the date as MM/DD/YYYY, e.g. 04/15/2022."
}

# ------ What changed ------------------------------------------------------------------------------------------------
# The fields that differ from how the row started.
# Only for an update. A field that is no longer shown counts as emptied.
# initial: What the fields started with (`edit_service_initial()`).
# Returns: A data frame with `id`, `name`, `label`, `short`, `type`, `old` and `new` (as
#   shown) and `old_value` and `new_value` (as saved); no rows if nothing changed.
edit_service_changes <- function(fields, initial, values) {
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

# A saved value as staff read it.
# The choice's name, or a date as MM/DD/YYYY.
# field: One of the form's fields.
# value: The saved value.
# Returns: A string; `"(empty)"` for `NA`.
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

# Two texts as runs of words kept, taken out and put in.
# It reads as the new text with the changes marked.
# old: The old text.
# new: The new text.
# Returns: A data frame of `kind` (`"same"`, `"del"` or `"ins"`) and `text`.
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
# What an add, update or delete comes to.
# row: The row being updated or deleted.
# fields: The form's fields, from `edit_service_fields()`. Not for a delete.
# values: What is in the form: a named list of strings by field id. Not for a delete.
# plan: For a delete, the plan if it is already worked out.
# Returns: A list: `items` (numbered, in the order to apply them) and `summary` (one sentence for the history); a
#   delete also has its `plan`. An error with a sentence if an add or an update would change something while its
#   form still has a field to fill in or fix: the page only builds a checked form, but a caller that builds one
#   itself (`edit_service_quick_add()`) is checked here too.
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
  summary <- if (mode != "add") {
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
# One Insert item per field filled in, on a row named `new:<table>:1`.
edit_add_items <- function(entity, snap, fields, values) {
  table <- entity$table
  identifier <- edit_identifier(stats::setNames(list(sprintf("new:%s:1", table)), edit_table_key(table)[1]))
  items <- lapply(edit_service_active(fields, values), function(field) {
    value <- edit_service_normalize(field, values[[field$id]])
    if (!is.na(value)) edit_item(table, identifier, "Insert", field$name, NA, value)
  })
  items <- Filter(Negate(is.null), items)
  if (length(items) == 0) NULL else do.call(rbind, items)
}

# ------ * Update ----------------------------------------------------------------------------------------------------------
# One Update item per field that differs.
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
# What a delete comes to.
# What it reaches, what stops it, and the items.
# row: The row being deleted.
# Returns: A list: `cascade`, `reach`, `blocked` (empty when it can go ahead), `items`, `far_reaching` (whether
#   anything beyond the row itself is affected) and `deleted` (the row picked: `table`, `row`, its key as JSON, and
#   `name`, see `edit_deleted_name()`).
edit_service_delete_plan <- function(entity, snap, row) {
  cascade <- edit_service_cascade(snap, stats::setNames(list(edit_key(row, edit_table_key(entity$table))), entity$table))
  reach <- edit_service_reach(snap, cascade)
  list(
    cascade = cascade, reach = reach, blocked = cascade$blocked,
    items = edit_delete_items(snap, cascade),
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

# The items of a delete: first any reference to the row that stays emptied (a study's contact), then the waves that
# lose questions numbering the rest again (edit_wave_numbers()), then every field of the row deleted. What points
# at the row and goes with it, the database deletes itself (sql/2_references.sql), so it isn't written.
edit_delete_items <- function(snap, cascade) {
  items <- list()
  for (clear in cascade$cleared) {
    for (row in split(edit_rows(snap, clear$table, clear$keys), seq_along(clear$keys))) {
      for (column in clear$columns) {
        items[[length(items) + 1]] <- edit_item(clear$table, edit_row_identifier(clear$table, row), "Update", column, row[[column]], NA)
      }
    }
  }
  items <- c(items, list(edit_renumber_after_delete(snap, cascade)))
  for (table in intersect(edit_delete_order, names(cascade$start))) {
    rows <- edit_rows(snap, table, cascade$start[[table]])
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
  # a wave that goes itself takes its numbers with it
  touched <- touched[!key_text(touched$wave_id) %in% cascade$removed$wave, , drop = FALSE]
  items <- lapply(seq_len(nrow(touched)), function(i) {
    q <- touched$questionnaire_id[i]
    wave <- touched$wave_id[i]
    left <- asked[!is_gone & asked$questionnaire_id == q & asked$wave_id == wave, , drop = FALSE]
    left <- left[order(suppressWarnings(as.numeric(left$question_number))), , drop = FALSE]
    edit_wave_numbers(snap, q, wave, key_text(left$placement_id))$items
  })
  items <- Filter(Negate(is.null), items)
  if (length(items) > 0) do.call(rbind, items)
}

# ------ Sentences for the history ----------------------------------------------------------------------------------------------
# The first thing typed that names the row, e.g. its name or wording.
edit_new_title <- function(fields, values) {
  for (field in Filter(function(field) field$type %in% c("text", "textarea"), edit_service_active(fields, values))) {
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
  paste0("Changed the ", word_list(unique(changes$short)), " of the ", entity$noun, " “", title, "”")
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
# What changing a row reaches.
# For a component, every place a questionnaire prints it; for anything else, the rows that point at it.
# row: The row being changed.
# A kind with `usage_rows(snap, row, table, rows)` says how it lists the rows of `table` that point at it: a list of
# `rows` (the ones to list, in order), `labels` and, if it likes, `text`; or `NULL` for the usual way, by name.
# Returns: A list: `uses`, `shared` (how many questionnaires print a component) and `pointing`, one list for each
#   kind of row that points at it: `table`, `n`, `text` ("3 variables"), `labels` (every row's name, in order) and
#   `groups`, the rows by the questionnaire they are in (see `edit_usage_groups()`), or `NULL` when they are all in
#   the row's own questionnaire.
edit_service_usage <- function(entity, snap, row) {
  if (entity$table == "component") {
    uses <- edit_service_uses(snap, row$component_id)
    shared <- length(unique(vapply(uses, function(use) as.numeric(use$questionnaire_id), numeric(1))))
    return(list(uses = uses, shared = shared, pointing = list()))
  }
  key <- edit_key(row, edit_table_key(entity$table))
  pointing <- lapply(Filter(function(relation) relation$parent == entity$table, edit_relations), function(relation) {
    hits <- edit_referencing(snap, relation, key)
    if (length(hits) == 0) {
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

# Rows that point at something, by the questionnaire each is in.
# A row is in a questionnaire if it holds one (a variable, a wave) or its variable does (a variable's choice).
# table: The rows' table.
# rows: The rows.
# labels: Their names, from `edit_label()`.
# Returns: A list with one list per questionnaire, the most rows first: `id`, `questionnaire` (its name, or "Not in
#   a questionnaire") and `labels`, in the order given. `NULL` for rows that can't be in a questionnaire, or are
#   questionnaires.
edit_usage_groups <- function(snap, table, rows, labels) {
  ids <- if (table == "questionnaire") {
    NULL
  } else if ("questionnaire_id" %in% names(rows)) {
    rows$questionnaire_id
  } else if ("variable_key" %in% names(rows) && !is.null(snap$variable)) {
    edit_lookup(snap, "variable", "variable_key", rows$variable_key, "questionnaire_id")
  } else if ("placement_id" %in% names(rows) && !is.null(snap$placement)) {
    edit_lookup(snap, "placement", "placement_id", rows$placement_id, "questionnaire_id")
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

# What each variable records, its unit, when and who it is about, as the questionnaire's preview shows them:
# "[Smokes now] [hours] [in the past 30 days] [Respondent]", each part when it has one. A variable's tags say
# the last three (variable_tag).
# variables: Rows of `variable`.
# Returns: One string per variable; `""` for one with neither.
edit_variable_tags <- function(snap, variables) {
  tag_of <- function(key, kind) {
    if (is.null(snap$variable_tag) || is.null(snap$tag)) {
      return(NA_character_)
    }
    tags <- snap$tag[match(snap$variable_tag$tag_id[snap$variable_tag$variable_key %in% key], snap$tag$tag_id), , drop = FALSE]
    names <- tags$name[tags$kind %in% kind]
    if (length(names) == 0) NA_character_ else paste(sort(names), collapse = ", ")
  }
  vapply(seq_len(nrow(variables)), function(i) {
    key <- variables$variable_key[i]
    preview_variable_tags(list(
      label = variables$label[i], unit = tag_of(key, "unit"), time_frame = tag_of(key, "time frame"), subject = tag_of(key, "subject")
    ))
  }, character(1))
}

# ------ What's left to do ------------------------------------------------------------------------------------------------------------
# What's left to do before a change can be applied.
# In the order to do it; nothing when it can go.
# found: `edit_service_check()`'s `missing` and `problems`.
# n_changes: How many fields an update changes.
# shared: How many questionnaires use what an update changes.
# plan: The delete plan from `edit_service_delete_plan()`.
# confirmed: What staff ticked and typed: `list(read, word, shared)`.
# Returns: A character vector of sentences; empty when it is ready.
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

# ------ Applying ------------------------------------------------------------------------------------------------------------------
# Apply a change now. Every change is applied at once, by whoever makes it, and written to the Edit History.
# built: The result of `edit_service_build()`.
# me: Who applies it: `list(staff_id, name, role)`.
# Returns: A list: `history_id` (its row in the Edit History) and `new_ids` (the number the database gave each new
#   row, by its `new:<table>:<n>` token). An error with the database's own sentence if it refused, e.g. because the
#   row changed since the form was opened.
edit_service_apply <- function(connection, built, me) {
  if (NROW(built$items) == 0) stop("There is nothing in this change to apply.", call. = FALSE)
  edit_model_apply(connection, built$items, built$summary, me)
}

# ------ Emptying ----------------------------------------------------------------------------------------------------------------------
# What emptying a registry, study, questionnaire or wave would remove, in a sentence for its confirm dialog.
# what: One of `edit_emptiable`.
# id: The row's id.
# Returns: A sentence, e.g. `"It asks 12 questions."` or `"Its questionnaires hold 40 blocks and 31 variables, asked as 60 questions in their waves."`.
edit_service_empty_preview <- function(connection, what, id) {
  counts <- edit_model_contents(connection, what, id)
  if (what == "wave") {
    return(if (counts$questions == 0) "It asks no questions, so there is nothing to remove." else paste0("It asks ", edit_count(counts$questions, "question"), "."))
  }
  if (counts$placements == 0 && counts$questions == 0) {
    return("Nothing is printed or asked in it, so there is nothing to remove.")
  }
  paste0(
    if (what == "questionnaire") "It holds " else "Its questionnaires hold ",
    edit_count(counts$placements, "block"), " and ", edit_count(counts$variables, "variable"),
    ", asked as ", edit_count(counts$questions, "question"), " in ", if (what == "questionnaire") "its" else "their", " waves."
  )
}

# Empty a registry, study, questionnaire or wave now: it stays, its contents go, and the history gets a row.
# Returns: A sentence saying what was removed, from the counts the database gives back.
edit_service_empty <- function(connection, what, id, me) {
  gone <- edit_model_empty(connection, what, id, me)
  paste0(
    "Removed ", edit_count(gone$questions_removed, "question"), ", ", edit_count(gone$placements_removed, "block"),
    " and ", edit_count(gone$variables_removed, "variable"), "."
  )
}

# ------ Something like it is there already ---------------------------------------------------------------
# The rows an add looks like.
# When someone adds something that is already there, the form says so before it saves, and shows what is there; it
# is a warning, not a block. Each kind says what counts as the same (`duplicates` in its `service_edit_entities_*.R`
# file): its name for most, a respondent type's age range, and so on. Capitals and extra spaces don't count.
# Returns: A list with one entry for each way it looks like something there: `entity` (the kind of what is there),
#   `why` (a sentence) and `cards` (a data frame of the rows, as the cards show them; see `edit_cards()`). Empty
#   if it looks like nothing.
edit_service_duplicates <- function(entity, snap, fields, values) {
  if (is.null(entity$duplicates)) {
    return(list())
  }
  typed <- edit_part_values(fields, values)
  found <- list()
  for (match in entity$duplicates(snap, typed)) {
    cards <- entity$cards(snap, NULL)
    cards <- cards[cards$key %in% edit_key(match$rows, edit_table_key(entity$table)), , drop = FALSE]
    if (nrow(cards) > 0) found[[length(found) + 1]] <- list(entity = entity, why = match$why, cards = cards)
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
