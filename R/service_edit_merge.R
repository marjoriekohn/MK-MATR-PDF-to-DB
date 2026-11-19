# ------ Merging one row into another --------------------------------------------------------------------
# Two rows that are the same thing kept twice: a question worded "…troubled about feelings of guilt?" in one
# questionnaire and "…troubled by feelings of guilt?" in another, two headings, two answer choices, two tags, two
# scales, two investigators. Merging one into the other makes everything that used the first use the second
# instead, and removes the first. Nothing else changes: each place a merged question or heading is printed keeps
# its own position, numbers, variables and what is inside it.
#
# A kind can be merged when it says what a row may be merged into (`alike(snap, row)` in its
# service_edit_entities_*.R file: the rows like it, e.g. the other components of its kind; not called merge-anything,
# since `entity$merge`, a tag's rename into another tag, would then find it by the start of its name). What moves is worked
# out from the same list of links a delete uses (edit_relations, service_edit_relations.R), so a table that points
# at a kind is followed without being named here. The change is items for the database's apply_change(), like
# every other one, and is written to the history as one change.

# Whether rows of a kind can be merged.
edit_can_merge <- function(entity) is.function(entity$alike)

# What a row is called in a sentence about it: its kind's noun, and for a printed wording the kind of block it is
# ("question", "section", "note"), since staff never call one a component.
edit_noun <- function(entity, row = NULL) {
  if (identical(entity$table, "component") && !is.null(row) && has_text(row$kind)) tolower(row$kind) else entity$noun
}

# The rows a row can be merged into.
# Returns: Rows of the kind's table, without `row` itself; none if nothing is like it; `NULL` for a kind that
#   can't be merged.
edit_merge_candidates <- function(entity, snap, row) {
  if (!edit_can_merge(entity) || is.null(row)) {
    return(NULL)
  }
  key <- edit_table_key(entity$table)
  rows <- entity$alike(snap, row)
  rows[!edit_key(rows, key) %in% edit_key(row, key), , drop = FALSE]
}

# The drop-down of what to merge a row into: each candidate by name (a kind's `alike_label(snap, rows)` says
# more, so near-duplicates can be told apart: a question with the questionnaires that print it), in alphabetical order.
# blank: The text of the leading choice with the value `""`.
# Returns: A named character vector, as `edit_choices()` gives.
edit_merge_choices <- function(entity, snap, row, blank = "Choose the one to keep") {
  rows <- edit_merge_candidates(entity, snap, row)
  if (NROW(rows) == 0) {
    return(stats::setNames("", blank))
  }
  labels <- if (is.function(entity$alike_label)) entity$alike_label(snap, rows) else edit_label(snap, entity$table, rows)
  by_name <- order(tolower(labels), method = "radix")
  c(stats::setNames("", blank), stats::setNames(edit_key(rows, edit_table_key(entity$table))[by_name], labels[by_name]))
}

# What merging a row into another comes to.
# row: The row that is merged, and goes.
# into_key: The key of the row it is merged into, which stays: one of `edit_merge_candidates()`.
# Returns: `NULL` if `into_key` isn't one of the candidates. Otherwise a list: `into` (that row) and `key` (its
#   key, to show it afterwards), `from` and `into_name` (the two names), `moves` (what follows, one list for each
#   table: `table`, `n` and `text`, "2 blocks in questionnaires"), `blocked` (what stands in the way, as a delete's
#   plan lists it: `text`, `fix`, `labels`), `items` (numbered, for `edit_service_apply()`; none while something
#   stands in the way), `sentence` (what merging does) and `summary` (for the history).
edit_service_merge_plan <- function(entity, snap, row, into_key) {
  candidates <- edit_merge_candidates(entity, snap, row)
  if (is.null(candidates) || !has_text(into_key)) {
    return(NULL)
  }
  table <- entity$table
  into <- candidates[edit_key(candidates, edit_table_key(table)) %in% into_key, , drop = FALSE]
  if (nrow(into) != 1) {
    return(NULL)
  }
  followed <- edit_merge_follow(snap, table, row, into)
  from <- edit_label(snap, table, row)
  into_name <- edit_label(snap, table, into)
  moved <- word_list(vapply(followed$moves, function(move) move$text, character(1)))
  total <- sum(vapply(followed$moves, function(move) move$n, numeric(1)))
  items <- if (length(followed$blocked) == 0) rbind(followed$items, edit_row_delete_items(table, row))
  list(
    into = into, key = edit_key(into, edit_table_key(table)), from = from, into_name = into_name,
    moves = followed$moves, blocked = followed$blocked, items = edit_number_items(items),
    sentence = if (total > 0) {
      sprintf("%s move%s to “%s”, and “%s” is removed.", moved, if (total == 1) "s" else "", into_name, from)
    } else {
      sprintf("Nothing uses “%s”, so it is only removed.", from)
    },
    summary = paste0(
      "Merged the ", edit_noun(entity, row), " “", shorten(from, 80), "” into “", shorten(into_name, 80), "”",
      if (total > 0) paste0(", moving ", moved)
    )
  )
}

# The rows that point at `row`, pointed at `into` instead.
# A row that only names it in a column (a place that prints a question, a study that names an investigator) has
# that column changed. A row it is part of the key of (a tag on a variable, a question's place in a scale, a
# choice a variable offers) can't change its key where it stands: it is taken out and put back under the other
# (`edit_rekey()`), unless the other has it already, when once is enough.
# Returns: A list: `items` (not numbered), `moves` (see `edit_service_merge_plan()`) and `blocked`.
edit_merge_follow <- function(snap, table, row, into) {
  key <- edit_table_key(table)
  items <- list()
  touched <- list() # by table, the keys of the rows that follow: a study that names an investigator twice is one study
  clashes <- list()
  for (relation in Filter(function(relation) relation$parent == table, edit_relations)) {
    child <- relation$child
    column <- names(relation$by)[relation$by == key]
    hits <- edit_rows(snap, child, edit_referencing(snap, relation, edit_key(row, key)))
    touched[[child]] <- union(touched[[child]], edit_key(hits, edit_table_key(child)))
    for (hit in split(hits, seq_len(nrow(hits)))) {
      if (!column %in% edit_table_key(child)) {
        items[[length(items) + 1]] <- edit_item(child, edit_row_identifier(child, hit), "Update", column, hit[[column]], into[[key]])
        next
      }
      moved <- edit_rekey(snap, child, hit, column, into[[key]])
      items[[length(items) + 1]] <- moved$items
      if (moved$clash) clashes[[child]] <- rbind(clashes[[child]], hit)
    }
  }
  touched <- touched[lengths(touched) > 0]
  list(
    items = do.call(rbind, Filter(Negate(is.null), items)),
    moves = unname(Map(function(child, keys) {
      list(table = child, n = length(keys), text = plural_noun(length(keys), child))
    }, names(touched), touched)),
    blocked = unname(Map(function(child, rows) edit_merge_clash(snap, child, rows), names(clashes), clashes))
  )
}

# A row given another value in one of its key columns: taken out and put back under the new key, with what it
# holds (a choice's code and order on its variable) and what points at it by that column (the tags on that
# choice; the database takes the old ones out with the old row). If a row with the new key is there already, a
# row that holds nothing of its own is only taken out; one that does (two choices of one variable, each with its
# own code) can't be made one.
# Returns: A list: `items`, and `clash` (whether it can't be done).
edit_rekey <- function(snap, table, row, column, value) {
  key <- edit_table_key(table)
  holds <- setdiff(names(database_tables[[table]]$columns), key)
  moved <- row
  moved[[column]] <- value
  there <- edit_key(moved, key) %in% edit_key(snap[[table]], key)
  if (there && length(holds) > 0) {
    return(list(items = NULL, clash = TRUE))
  }
  follow <- lapply(Filter(function(relation) relation$parent == table && column %in% relation$by, edit_relations), function(relation) {
    rows <- edit_rows(snap, relation$child, edit_referencing(snap, relation, edit_key(row, key)))
    rows[[names(relation$by)[relation$by == column]]] <- rep(value, nrow(rows))
    lapply(split(rows, seq_len(nrow(rows))), function(follower) edit_row_insert_items(relation$child, follower))
  })
  list(
    items = do.call(rbind, c(
      list(edit_row_delete_items(table, row)),
      if (!there) c(list(edit_row_insert_items(table, moved)), unlist(follow, recursive = FALSE))
    )),
    clash = FALSE
  )
}

# The items that take a row out: every field it holds, so the database checks each is still as it was (as a
# delete writes them, `edit_delete_items()`); a row that is only its key is one item.
edit_row_delete_items <- function(table, row) {
  identifier <- edit_row_identifier(table, row)
  holds <- setdiff(names(database_tables[[table]]$columns), edit_table_key(table))
  if (length(holds) == 0) {
    return(edit_item(table, identifier, "Delete", NA, NA, NA))
  }
  do.call(rbind, lapply(holds, function(column) edit_item(table, identifier, "Delete", column, row[[column]], NA)))
}

# The items that put a row in under the key it has: every field it holds that isn't empty; a row that is only its
# key is one item.
edit_row_insert_items <- function(table, row) {
  identifier <- edit_row_identifier(table, row)
  holds <- setdiff(names(database_tables[[table]]$columns), edit_table_key(table))
  holds <- holds[!vapply(holds, function(column) is.na(row[[column]]), logical(1))]
  if (length(holds) == 0) {
    return(edit_item(table, identifier, "Insert", NULL, NA, NA))
  }
  do.call(rbind, lapply(holds, function(column) edit_item(table, identifier, "Insert", column, NA, row[[column]])))
}

# What stands in the way of a merge: rows that would be there twice, each holding something of its own.
# rows: The rows of `child` that can't follow.
# Returns: A list of `text`, `fix`, `labels` and `table`, as a delete's plan lists what blocks it.
edit_merge_clash <- function(snap, child, rows) {
  if (child == "response_option") {
    variables <- snap$variable[match(unique(rows$variable_key), snap$variable$variable_key), , drop = FALSE]
    labels <- sort(edit_label(snap, "variable", variables), method = "radix")
    return(list(
      table = child, labels = labels,
      text = paste0(
        edit_count(length(labels), "variable"), if (length(labels) == 1) " offers" else " offer",
        " both of these answer choices, each with a code of its own, so they can't become one there."
      ),
      fix = "Take one of the two off those variables first (open the wave and press Edit extraction), then merge."
    ))
  }
  labels <- sort(edit_label(snap, child, rows), method = "radix")
  list(
    table = child, labels = labels,
    text = paste0(plural_noun(length(labels), child), " would be there twice."),
    fix = "Remove one of each first, then merge."
  )
}
