# ------ What a change reaches ---------------------------------------------------------
# The live tables point at each other: a wave at its questionnaire, a
# questionnaire's placement of a component at both. Before anyone deletes
# something, the edit pages work out what else would go with it, from the list
# of these links below and the rows in `snap` (a list of whole tables, named
# after them; table_model_get()).
#
# Each link says what happens to the rows that point at a row being deleted:
#   remove  they go with it (a questionnaire's waves)
#   clear   they stay, and what points at it is emptied (a study's contact)
#   block   they're in the way: the delete can't go ahead until they're dealt
#           with (a study's lead investigator, which never changes)
# A link can also say its parent can't be without these rows (`needs_child =
# "remove_parent"`): the parent is deleted too, as a question asked in no wave
# can't exist. A study can be left with no questionnaires, and a questionnaire
# with no waves, until more are added.
#
# A row's identity is its key columns joined with "|" (edit_key()).

#' Describe how one table points at another, and what deleting the parent does.
#'
#' @param child The table that holds the reference.
#' @param parent The table it points at.
#' @param by The child's columns, named, each holding the parent's column of that name's value.
#' @param on_delete What happens to the child's rows when the parent's go: `"remove"`, `"block"` or `"clear"`.
#' @param needs_child `"remove_parent"` if a parent left with none of these rows goes too; `NULL` otherwise.
#' @param why,fix For a block: why the child's rows are in the way, and what to do.
#' @param clears For a clear: what is emptied, e.g. `"contact investigator"`.
#' @return A list.
relation <- function(child, parent, by, on_delete, needs_child = NULL, why = NULL, fix = NULL, clears = NULL) {
  list(
    child = child, parent = parent,
    by = by, # the child's columns, named, each holding the parent's column of that name's value
    on_delete = on_delete, needs_child = needs_child,
    why = why, fix = fix, # a block: why the rows are in the way, and what to do
    clears = clears # a clear: what's emptied, e.g. "contact investigator"
  )
}

edit_relations <- list(
  relation("study", "registry", c(registry_id = "registry_id"), "remove"),
  relation(
    "study", "principal_investigator", c(principal_investigator_id = "principal_investigator_id"), "block",
    why = "A study's lead investigator can't be changed once it's recorded, so the studies they lead have to go first.",
    fix = "Delete those studies first, or keep this investigator."
  ),
  relation(
    "study", "principal_investigator", c(contact_principal_investigator_id = "principal_investigator_id"),
    "clear",
    clears = "contact investigator"
  ),
  relation("questionnaire", "study", c(study_id = "study_id"), "remove"),
  relation(
    "questionnaire", "respondent_type", c(respondent_type_id = "respondent_type_id"), "block",
    why = "Every questionnaire has a respondent type, so a respondent type can't go while questionnaires use it.",
    fix = "Change those questionnaires to another respondent type first (Update, then Questionnaire)."
  ),
  relation("wave", "questionnaire", c(questionnaire_id = "questionnaire_id"), "remove"),
  relation(
    "component", "component_type", c(component_type_id = "component_type_id"), "block",
    why = "Every component has a kind, so a component type can't go while components use it.",
    fix = "Delete those components first."
  ),
  relation("questionnaire_component", "questionnaire", c(questionnaire_id = "questionnaire_id"), "remove"),
  relation("questionnaire_component", "component", c(component_id = "component_id"), "remove"),
  # What's inside a section goes with it.
  relation(
    "questionnaire_component", "questionnaire_component",
    c(questionnaire_id = "questionnaire_id", parent_component_id = "component_id"), "remove"
  ),
  # A question's settings and its placement are one block: neither outlives the other.
  relation(
    "questionnaire_component", "questionnaire_question",
    c(questionnaire_id = "questionnaire_id", component_id = "component_id"), "remove"
  ),
  relation("question", "component", c(component_id = "component_id"), "remove"),
  relation("questionnaire_question", "question", c(component_id = "component_id"), "remove"),
  relation(
    "questionnaire_question", "questionnaire_component",
    c(questionnaire_id = "questionnaire_id", component_id = "component_id"), "remove"
  ),
  relation(
    "questionnaire_question", "questionnaire_question",
    c(questionnaire_id = "questionnaire_id", parent_question_id = "component_id"), "block",
    why = "These follow-up questions need the question above them as their parent.",
    fix = paste0(
      "Delete the follow-ups first, or make them stand on their own (Update, then Questionnaire ",
      "question, then answer Yes to \"Can a researcher understand it on its own?\")."
    )
  ),
  relation("wave_question", "wave", c(wave_id = "wave_id"), "remove"),
  relation(
    "wave_question", "questionnaire_question",
    c(questionnaire_id = "questionnaire_id", component_id = "component_id"), "remove",
    needs_child = "remove_parent"
  ),
  relation("question_category", "question", c(component_id = "component_id"), "remove"),
  relation("question_category", "category", c(mesh_id = "mesh_id"), "remove"),
  relation("question_scale", "question", c(component_id = "component_id"), "remove"),
  relation("question_scale", "measurement_scale", c(scale_id = "scale_id"), "remove"),
  relation(
    "variable", "questionnaire_question",
    c(questionnaire_id = "questionnaire_id", component_id = "component_id"), "remove"
  ),
  relation(
    "variable", "subject", c(subject_id = "subject_id"), "block",
    why = "Variables are about this subject.",
    fix = "Change those variables to another subject first. Until then the subject can be renamed instead."
  ),
  relation("open_ended", "variable", c(variable_key = "variable_key"), "remove"),
  relation("closed_ended", "variable", c(variable_key = "variable_key"), "remove"),
  relation("response_option", "closed_ended", c(variable_key = "variable_key"), "remove"),
  # a choice that skips to a component that goes no longer skips anywhere; its printed words stay
  relation("response_option", "component", c(skip_to_component_id = "component_id"), "clear", clears = "question it skips to"),
  relation(
    "response_option", "answer_option", c(answer_option_id = "answer_option_id"), "block",
    why = "Variables already offer this answer choice.",
    fix = "Take it off those variables first."
  ),
  relation(
    "answer_option_set", "answer_option", c(answer_option_id = "answer_option_id"), "block",
    why = "Option sets already include this answer choice.",
    fix = "Take it out of those option sets first."
  )
)

# Parents are deleted after what points at them, so the order a delete is
# written in is safe: the tables, children first.
edit_delete_order <- c(
  "response_option", "open_ended", "closed_ended", "variable", "wave_question",
  "question_scale", "question_category", "questionnaire_question",
  "questionnaire_component", "question", "component", "answer_option_set",
  "answer_option", "wave", "questionnaire", "study", "registry",
  "principal_investigator", "respondent_type", "component_type", "category",
  "measurement_scale", "subject"
)

# ------ Rows and their identity -------------------------------------------------------
#' Name rows by their key columns.
#'
#' @param rows A data frame.
#' @param columns The key columns.
#' @return One string per row; the columns' values joined with `|`. Empty for no rows.
edit_key <- function(rows, columns) {
  if (nrow(rows) == 0) {
    return(character())
  }
  parts <- lapply(columns, function(column) key_text(rows[[column]]))
  do.call(paste, c(parts, sep = "|"))
}

#' Key values as text.
#'
#' Numbers are written in full, never in scientific notation.
#'
#' @param values A vector.
#' @return A character vector.
key_text <- function(values) {
  if (is.numeric(values)) format(values, scientific = FALSE, trim = TRUE, digits = 15) else as.character(values)
}

#' A table's key columns.
#'
#' @param table A table in `database_tables`.
#' @return The column names.
edit_table_key <- function(table) database_tables[[table]]$key

#' The rows of a table with these keys.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param table The table.
#' @param keys Keys as text, from `edit_key()`.
#' @return The matching rows, in the table's order.
edit_rows <- function(snap, table, keys) {
  rows <- snap[[table]]
  rows[edit_key(rows, edit_table_key(table)) %in% keys, , drop = FALSE]
}

# The tables whose rows can be reached from rows of `table`: those that point
# at them, and the parents that need what's removed (a question asked in no
# wave goes with the wave), however far; `table` included.
edit_descendant_tables <- function(table) {
  found <- table
  repeat {
    children <- vapply(
      Filter(function(relation) relation$parent %in% found, edit_relations),
      function(relation) relation$child, character(1)
    )
    needing <- vapply(
      Filter(function(relation) relation$child %in% found && !is.null(relation$needs_child), edit_relations),
      function(relation) relation$parent, character(1)
    )
    reached <- unique(c(children, needing))
    if (all(reached %in% found)) break
    found <- union(found, reached)
  }
  found
}

#' The rows that point at some rows of their parent table.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param relation A `relation()`.
#' @param parent_keys The keys of rows of `relation$parent`.
#' @return The keys of the rows of `relation$child` that point at them.
edit_referencing <- function(snap, relation, parent_keys) {
  parents <- edit_rows(snap, relation$parent, parent_keys)
  children <- snap[[relation$child]]
  by <- relation$by
  points_at <- stats::complete.cases(children[names(by)]) &
    edit_key(children, names(by)) %in% edit_key(parents, unname(by))
  edit_key(children[points_at, , drop = FALSE], edit_table_key(relation$child))
}

# ------ The cascade ----------------------------------------------------------------------
#' What deleting some rows takes with it.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param start A list of keys named after their tables: the rows being deleted.
#' @return A list: `start` (as given), `removed` (the keys of every row that goes, by table,
#'   the starting rows first), `cleared` (rows that stay but have a column emptied: `table`, `columns`,
#'   `keys`, `what`) and `blocked` (what is in the way, if anything: each says `text` and
#'   `fix` and lists the `labels` of the rows involved).
edit_service_cascade <- function(snap, start) {
  removed <- list()
  queue <- list()
  cleared <- list()

  enqueue <- function(table, keys) {
    new <- setdiff(keys, removed[[table]])
    if (length(new) == 0) {
      return()
    }
    removed[[table]] <<- c(removed[[table]], new)
    queue[[length(queue) + 1]] <<- list(table = table, keys = new)
  }
  for (table in names(start)) enqueue(table, start[[table]])

  repeat {
    while (length(queue) > 0) {
      step <- queue[[1]]
      queue <- queue[-1]
      for (relation in Filter(function(r) r$parent == step$table, edit_relations)) {
        hits <- edit_referencing(snap, relation, step$keys)
        if (length(hits) == 0) next
        if (relation$on_delete == "remove") enqueue(relation$child, hits)
        if (relation$on_delete == "clear") {
          cleared <- c(cleared, list(list(
            table = relation$child, columns = names(relation$by), keys = hits, what = relation$clears
          )))
        }
      }
    }
    # A parent left with none of the children it needs goes too.
    orphans <- Filter(
      function(orphan) length(orphan$keys) > 0,
      lapply(
        Filter(function(r) identical(r$needs_child, "remove_parent"), edit_relations),
        function(relation) list(table = relation$parent, keys = edit_left_empty(snap, relation, removed))
      )
    )
    if (length(orphans) == 0) break
    for (orphan in orphans) enqueue(orphan$table, orphan$keys)
  }

  # A row that's going anyway needn't have anything emptied.
  cleared <- lapply(cleared, function(clear) {
    clear$keys <- setdiff(clear$keys, removed[[clear$table]])
    clear
  })
  cleared <- Filter(function(clear) length(clear$keys) > 0, cleared)

  list(start = start, removed = removed, cleared = cleared, blocked = edit_blockers(snap, removed))
}

# The keys of the parents (not themselves being removed) that had children
# removed and have none left.
edit_left_empty <- function(snap, relation, removed) {
  if (length(removed[[relation$child]]) == 0) {
    return(character())
  }
  gone <- edit_rows(snap, relation$child, removed[[relation$child]])
  if (nrow(gone) == 0) {
    return(character())
  }
  by <- relation$by
  children <- snap[[relation$child]]
  parents <- snap[[relation$parent]]
  parent_refs <- edit_key(parents, unname(by))
  lost <- unique(edit_key(gone[stats::complete.cases(gone[names(by)]), , drop = FALSE], names(by)))
  staying <- children[!edit_key(children, edit_table_key(relation$child)) %in% removed[[relation$child]], , drop = FALSE]
  kept <- edit_key(staying[stats::complete.cases(staying[names(by)]), , drop = FALSE], names(by))
  empty <- parent_refs %in% setdiff(lost, kept)
  edit_key(parents[empty, , drop = FALSE], edit_table_key(relation$parent)) |>
    setdiff(removed[[relation$parent]])
}

# Rows in the way: pointing at something that's going, but staying.
edit_blockers <- function(snap, removed) {
  blockers <- list()
  for (relation in edit_relations) {
    if (relation$on_delete == "block" && length(removed[[relation$parent]]) > 0) {
      hits <- setdiff(edit_referencing(snap, relation, removed[[relation$parent]]), removed[[relation$child]])
      if (length(hits) > 0) blockers <- c(blockers, list(edit_in_the_way(snap, relation, hits)))
    }
  }
  blockers
}

edit_in_the_way <- function(snap, relation, child_keys) {
  rows <- edit_rows(snap, relation$child, child_keys)
  labels <- edit_label(snap, relation$child, rows)
  list(
    text = paste0(
      plural_noun(nrow(rows), relation$child), " ", if (nrow(rows) == 1) "is" else "are", " in the way. ", relation$why
    ),
    fix = relation$fix, labels = labels, table = relation$child
  )
}

# ------ Describing rows ---------------------------------------------------------------------
# How each kind of row is named in lists, in staff's words.
edit_nouns <- list(
  registry = c("registry", "registries"),
  principal_investigator = c("investigator", "investigators"),
  study = c("study", "studies"),
  respondent_type = c("respondent type", "respondent types"),
  questionnaire = c("questionnaire", "questionnaires"),
  wave = c("wave", "waves"),
  component_type = c("component type", "component types"),
  component = c("component", "components"),
  question = c("question", "questions"),
  questionnaire_component = c("component in a questionnaire", "components in questionnaires"),
  questionnaire_question = c("question in a questionnaire", "questions in questionnaires"),
  wave_question = c("question in a wave", "questions in waves"),
  category = c("MeSH category", "MeSH categories"),
  question_category = c("MeSH tag on a question", "MeSH tags on questions"),
  answer_option = c("answer choice", "answer choices"),
  subject = c("subject", "subjects"),
  answer_option_set = c("option set entry", "option set entries"),
  measurement_scale = c("measurement scale", "measurement scales"),
  question_scale = c("measurement scale membership", "measurement scale memberships"),
  variable = c("variable", "variables"),
  open_ended = c("open-ended variable", "open-ended variables"),
  closed_ended = c("closed-ended variable", "closed-ended variables"),
  response_option = c("response options for a variable", "response options for variables")
)

#' A count of rows with their table's noun.
#'
#' For example "1 wave", "3 studies".
#'
#' @param n The count.
#' @param table The table.
#' @return A string; see `plural()`.
plural_noun <- function(n, table) {
  nouns <- edit_nouns[[table]]
  plural(n, nouns[1], plural_form = nouns[2])
}

# Parts of rows that are only detail of another row, so lists leave them out.
edit_hidden_tables <- c("question", "open_ended", "closed_ended")

# The order lists name kinds of rows in: the outermost first.
edit_display_order <- c(
  "registry", "study", "questionnaire", "wave", "component", "questionnaire_component",
  "questionnaire_question", "wave_question", "variable", "response_option",
  "question_category", "question_scale", "category", "measurement_scale", "answer_option",
  "answer_option_set", "principal_investigator", "respondent_type", "component_type", "subject"
)

# Naming a row can take other tables (a wave's name starts with its
# questionnaire's), so reading rows to name means reading these too.
edit_label_needs <- list(
  wave = "questionnaire",
  component = "component_type",
  question = c("component", "component_type"),
  questionnaire_component = c("questionnaire", "component", "component_type"),
  questionnaire_question = c("questionnaire", "component", "component_type"),
  wave_question = c("questionnaire", "wave", "component", "component_type"),
  question_category = c("category", "component", "component_type"),
  question_scale = c("measurement_scale", "component", "component_type"),
  open_ended = "variable",
  closed_ended = "variable",
  response_option = c("variable", "answer_option"),
  answer_option_set = "answer_option"
)

#' Every table to read to work out what deleting rows of some tables reaches.
#'
#' And to name what it finds.
#'
#' @param tables Table names.
#' @return Table names.
edit_tables_to_read <- function(tables) {
  reached <- unique(unlist(lapply(tables, edit_descendant_tables)))
  named_by <- unique(unlist(edit_label_needs[reached]))
  unique(c(reached, named_by, unlist(edit_label_needs[named_by])))
}

#' One name per row.
#'
#' For example "AHP · Do you now smoke cigarettes?".
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param table The table.
#' @param rows Rows of the table.
#' @return A character vector; empty for no rows.
edit_label <- function(snap, table, rows) {
  if (nrow(rows) == 0) {
    return(character())
  }
  edit_labels[[table]](snap, rows)
}

#' Look up a column of a table by id.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param table The table.
#' @param id_column The column holding the ids.
#' @param ids The ids to look up.
#' @param column The column to read.
#' @return The values of `column`, one for each id, `NA` where there is no such row.
edit_lookup <- function(snap, table, id_column, ids, column) {
  snap[[table]][[column]][match(ids, snap[[table]][[id_column]])]
}

#' The rows of a table for one component as printed in one questionnaire.
#'
#' What a block carries in a questionnaire: a question's settings (`questionnaire_question`), or the
#' number it was given in each wave (`wave_question`).
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param table `"questionnaire_question"` or `"wave_question"`.
#' @param questionnaire_id,component_id Which questionnaire, and which component in it.
#' @return The matching rows of `table`.
edit_placed_rows <- function(snap, table, questionnaire_id, component_id) {
  rows <- snap[[table]]
  rows[rows$questionnaire_id == questionnaire_id & rows$component_id == component_id, , drop = FALSE]
}

#' Questionnaires' short names.
#'
#' A questionnaire's abbreviation, or its name if it has none.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param ids Questionnaire ids.
#' @return A character vector.
edit_questionnaire_short <- function(snap, ids) {
  short <- edit_lookup(snap, "questionnaire", "questionnaire_id", ids, "abbreviation")
  name <- edit_lookup(snap, "questionnaire", "questionnaire_id", ids, "name")
  ifelse(is.na(short) | !nzchar(short), ifelse(is.na(name), "?", name), short)
}

#' A component's printed words, shortened.
#'
#' Sections can be untitled, and blocks can have no words; those get a placeholder.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param ids Component ids.
#' @param max_characters Where to cut the words short.
#' @return A character vector.
edit_component_text <- function(snap, ids, max_characters = 70) {
  text <- edit_lookup(snap, "component", "component_id", ids, "text")
  type <- edit_lookup(
    snap, "component_type", "component_type_id",
    edit_lookup(snap, "component", "component_id", ids, "component_type_id"), "name"
  )
  blank <- is.na(text) | !nzchar(trimws(text))
  text <- shorten(text, max_characters)
  text[blank] <- ifelse(is.na(type[blank]), "(no printed words)", paste0("(untitled ", tolower(type[blank]), ")"))
  text
}

edit_labels <- list(
  registry = function(snap, rows) rows$name,
  principal_investigator = function(snap, rows) rows$name,
  study = function(snap, rows) rows$name,
  respondent_type = function(snap, rows) rows$name,
  questionnaire = function(snap, rows) {
    ifelse(is.na(rows$abbreviation) | !nzchar(rows$abbreviation), rows$name, paste0(rows$name, " (", rows$abbreviation, ")"))
  },
  wave = function(snap, rows) paste(edit_questionnaire_short(snap, rows$questionnaire_id), rows$name),
  component_type = function(snap, rows) rows$name,
  component = function(snap, rows) edit_component_text(snap, rows$component_id),
  question = function(snap, rows) edit_component_text(snap, rows$component_id),
  questionnaire_component = function(snap, rows) {
    paste0(edit_questionnaire_short(snap, rows$questionnaire_id), " · ", edit_component_text(snap, rows$component_id, 50))
  },
  questionnaire_question = function(snap, rows) {
    paste0(edit_questionnaire_short(snap, rows$questionnaire_id), " · ", edit_component_text(snap, rows$component_id, 50))
  },
  wave_question = function(snap, rows) {
    waves <- edit_lookup(snap, "wave", "wave_id", rows$wave_id, "name")
    paste0(
      edit_questionnaire_short(snap, rows$questionnaire_id), " ", waves, " · question ", edit_called_number(rows),
      " · ", edit_component_text(snap, rows$component_id, 40)
    )
  },
  category = function(snap, rows) paste0(rows$term, " (", rows$mesh_id, ")"),
  question_category = function(snap, rows) {
    term <- edit_lookup(snap, "category", "mesh_id", rows$mesh_id, "term")
    paste0(ifelse(is.na(term), rows$mesh_id, term), " on “", edit_component_text(snap, rows$component_id, 40), "”")
  },
  answer_option = function(snap, rows) rows$standardized_label,
  subject = function(snap, rows) rows$name,
  answer_option_set = function(snap, rows) {
    paste0(
      "Option set ",
      rows$option_set_id,
      " includes ",
      edit_lookup(snap, "answer_option", "answer_option_id", rows$answer_option_id, "standardized_label")
    )
  },
  measurement_scale = function(snap, rows) rows$name,
  question_scale = function(snap, rows) {
    scale <- edit_lookup(snap, "measurement_scale", "scale_id", rows$scale_id, "name")
    paste0(ifelse(is.na(scale), "Scale", scale), " item ", rows$item_number, " · ", edit_component_text(snap, rows$component_id, 40))
  },
  variable = function(snap, rows) {
    paste0(rows$matr_id, ifelse(is.na(rows$label) | !nzchar(rows$label), "", paste0(" · ", rows$label)))
  },
  open_ended = function(snap, rows) edit_lookup(snap, "variable", "variable_key", rows$variable_key, "matr_id"),
  closed_ended = function(snap, rows) edit_lookup(snap, "variable", "variable_key", rows$variable_key, "matr_id"),
  response_option = function(snap, rows) {
    matr_id <- edit_lookup(snap, "variable", "variable_key", rows$variable_key, "matr_id")
    shown <- ifelse(
      is.na(rows$printed_label) | !nzchar(rows$printed_label),
      edit_lookup(snap, "answer_option", "answer_option_id", rows$answer_option_id, "standardized_label"),
      rows$printed_label
    )
    paste0(matr_id, " · ", shown, " (code ", rows$code_value, ")")
  }
)

# ------ What a delete reaches, for staff ---------------------------------------------------------
#' The cascade as lists staff can read.
#'
#' How many of each kind of row go, a few of each by name, how it falls on each
#' questionnaire, and what is changed without going.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param cascade The result of `edit_service_cascade()`.
#' @param examples How many rows to name for each kind.
#' @return A list: `lines`, `changed`, `by_questionnaire` and `total` (how many other rows are affected).
edit_service_reach <- function(snap, cascade, examples = 5) {
  start_keys <- cascade$start
  shown <- intersect(edit_display_order, names(cascade$removed))
  lines <- lapply(shown, function(table) {
    keys <- setdiff(cascade$removed[[table]], start_keys[[table]])
    if (length(keys) == 0) {
      return(NULL)
    }
    rows <- edit_rows(snap, table, keys)
    list(
      table = table, n = nrow(rows), text = plural_noun(nrow(rows), table),
      examples = utils::head(edit_label(snap, table, rows), examples),
      more = max(0, nrow(rows) - examples)
    )
  })
  lines <- Filter(Negate(is.null), lines)
  changed <- lapply(cascade$cleared, function(clear) {
    rows <- edit_rows(snap, clear$table, clear$keys)
    list(
      table = clear$table, n = nrow(rows), what = clear$what,
      text = paste0(plural_noun(nrow(rows), clear$table), " will lose their ", clear$what),
      examples = utils::head(edit_label(snap, clear$table, rows), examples)
    )
  })
  list(
    lines = lines, changed = changed, by_questionnaire = edit_by_questionnaire(snap, cascade),
    total = sum(vapply(lines, function(line) line$n, numeric(1)))
  )
}

# How a delete falls on each questionnaire: for every one that loses rows, what
# it loses.
edit_by_questionnaire <- function(snap, cascade) {
  tables <- c("questionnaire_component", "questionnaire_question", "wave_question", "variable", "wave")
  counts <- lapply(tables, function(table) {
    if (length(cascade$removed[[table]]) == 0) {
      return(NULL)
    }
    rows <- edit_rows(snap, table, cascade$removed[[table]])
    if (nrow(rows) == 0) {
      return(NULL)
    }
    data.frame(table = table, questionnaire_id = rows$questionnaire_id)
  })
  counts <- do.call(rbind, Filter(Negate(is.null), counts))
  if (is.null(counts)) {
    return(list())
  }
  # a questionnaire that's itself deleted needn't be broken down: it all goes
  counts <- counts[!key_text(counts$questionnaire_id) %in% cascade$removed$questionnaire, , drop = FALSE]
  lapply(unique(counts$questionnaire_id), function(id) {
    mine <- counts[counts$questionnaire_id == id, ]
    list(
      questionnaire = edit_questionnaire_short(snap, id),
      lines = vapply(
        intersect(tables, unique(mine$table)),
        function(table) plural_noun(sum(mine$table == table), table), character(1),
        USE.NAMES = FALSE
      )
    )
  })
}
