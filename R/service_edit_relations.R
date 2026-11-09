# ------ What a change reaches ---------------------------------------------------------
# The catalog's tables point at each other: a wave at its questionnaire, a placement at its component and its
# questionnaire. Before anyone deletes something, the Metadata page works out what else would go with it, from
# the list of these links below and the rows in `snap` (a list of whole tables, named after them;
# table_model_get()).
#
# Each link says what happens to the rows that point at a row being deleted, exactly as the database's foreign
# keys have it (sql/2_references.sql), so the preview shows what the database will do:
#   remove  they go with it (a questionnaire's waves and placements, a placement's variables)
#   clear   they stay, and what points at it is emptied (a study's contact, a question's dependency)
#   block   they're in the way: the database refuses the delete until they're dealt with (a component some
#           questionnaire prints, a tag or answer choice in use, a registry with studies)
# A component no placement names any more is forgotten by the database itself when a change ends
# (private.forget_components()), unless a measurement scale names it; the preview says so.
#
# A row's identity is its key columns joined with "|" (edit_key()).

# Describe how one table points at another, and what deleting the parent does.
# child: The table that holds the reference.
# parent: The table it points at.
# by: The child's columns, named, each holding the parent's column of that name's value.
# on_delete: What happens to the child's rows when the parent's go: `"remove"`, `"block"` or `"clear"`.
# why,fix: For a block: why the child's rows are in the way, and what to do.
# clears: For a clear: what is emptied, e.g. `"contact investigator"`.
# Returns: A list.
relation <- function(child, parent, by, on_delete, why = NULL, fix = NULL, clears = NULL) {
  list(child = child, parent = parent, by = by, on_delete = on_delete, why = why, fix = fix, clears = clears)
}

edit_relations <- list(
  relation(
    "study", "registry", c(registry_id = "registry_id"), "block",
    why = "A registry that still holds studies can't be deleted.",
    fix = "Empty the registry (its contents go, the registry and its studies stay), or delete its studies first."
  ),
  relation(
    "study", "principal_investigator", c(principal_investigator_id = "principal_investigator_id"), "block",
    why = "These studies name this investigator as their lead.",
    fix = "Change those studies to another lead investigator first (Update, then Study), or keep this investigator."
  ),
  relation(
    "study", "principal_investigator", c(contact_principal_investigator_id = "principal_investigator_id"),
    "clear",
    clears = "contact investigator"
  ),
  relation(
    "questionnaire", "study", c(study_id = "study_id"), "block",
    why = "A study that still holds questionnaires can't be deleted.",
    fix = "Empty the study (its contents go, the study and its questionnaires stay), or delete its questionnaires first."
  ),
  relation(
    "questionnaire", "respondent_type", c(respondent_type_id = "respondent_type_id"), "block",
    why = "Every questionnaire has a respondent type, so a respondent type can't go while questionnaires use it.",
    fix = "Change those questionnaires to another respondent type first (Update, then Questionnaire)."
  ),
  relation("wave", "questionnaire", c(questionnaire_id = "questionnaire_id"), "remove"),
  relation(
    "wave", "pdf_file", c(pdf_sha256 = "pdf_sha256"), "block",
    why = "These waves were extracted from it.", fix = "Keep it: it is the record of what they were read from."
  ),
  relation(
    "wave", "pdf_file", c(supplement_sha256 = "pdf_sha256"), "block",
    why = "These waves read their Variable Names and Coding from it.", fix = "Keep it: it is the record of what they were read from."
  ),
  relation("placement", "questionnaire", c(questionnaire_id = "questionnaire_id"), "remove"),
  relation(
    "placement", "component", c(component_id = "component_id"), "block",
    why = "Questionnaires still print this block there.",
    fix = "Delete it from those questionnaires first (Blocks, then the questionnaire). A block no questionnaire prints goes on its own."
  ),
  # What's inside a section, grid or group goes with it; a question whose dependency goes stands on its own.
  relation("placement", "placement", c(parent_placement_id = "placement_id"), "remove"),
  relation("placement", "placement", c(depends_on_placement_id = "placement_id"), "clear", clears = "question they depend on"),
  relation("wave_question", "wave", c(wave_id = "wave_id"), "remove"),
  relation("wave_question", "placement", c(placement_id = "placement_id"), "remove"),
  relation("variable", "placement", c(placement_id = "placement_id"), "remove"),
  relation("response_option", "variable", c(variable_key = "variable_key"), "remove"),
  relation(
    "response_option", "answer_option", c(answer_option_id = "answer_option_id"), "block",
    why = "Variables already offer this answer choice.",
    fix = "Take it off those variables first. Until then the choice can be reworded instead."
  ),
  relation("placement_tag", "placement", c(placement_id = "placement_id"), "remove"),
  relation("variable_tag", "variable", c(variable_key = "variable_key"), "remove"),
  relation("response_option_tag", "response_option", c(variable_key = "variable_key", answer_option_id = "answer_option_id"), "remove"),
  relation(
    "placement_tag", "tag", c(tag_id = "tag_id"), "block",
    why = "Questions carry this tag.", fix = "Take it off those questions first. Until then the tag can be renamed instead."
  ),
  relation(
    "variable_tag", "tag", c(tag_id = "tag_id"), "block",
    why = "Variables carry this tag.", fix = "Take it off those variables first. Until then the tag can be renamed instead."
  ),
  relation(
    "response_option_tag", "tag", c(tag_id = "tag_id"), "block",
    why = "Answer choices of variables carry this tag.", fix = "Take it off those choices first. Until then the tag can be renamed instead."
  ),
  relation(
    "question_scale", "measurement_scale", c(scale_id = "scale_id"), "block",
    why = "A measurement scale that still has items can't be deleted.",
    fix = "Delete its items first (Scale Items, then the scale)."
  ),
  relation(
    "question_scale", "component", c(component_id = "component_id"), "block",
    why = "Measurement scales list this question as an item.",
    fix = "Take it out of those scales first (Scale Items, then the scale)."
  )
)

# Parents are deleted after what points at them, so the order a delete is written in is safe: the tables,
# children first.
edit_delete_order <- c(
  "response_option_tag", "variable_tag", "placement_tag", "response_option", "variable", "wave_question",
  "question_scale", "placement", "component", "answer_option", "tag", "wave", "questionnaire", "study", "registry",
  "principal_investigator", "respondent_type", "measurement_scale", "pdf_file"
)

# ------ Rows and their identity -------------------------------------------------------
# Name rows by their key columns.
# rows: A data frame.
# columns: The key columns.
# Returns: One string per row; the columns' values joined with `|`. Empty for no rows.
edit_key <- function(rows, columns) {
  if (nrow(rows) == 0) {
    return(character())
  }
  parts <- lapply(columns, function(column) key_text(rows[[column]]))
  do.call(paste, c(parts, sep = "|"))
}

# Key values as text.
# Numbers are written in full, never in scientific notation.
# values: A vector.
key_text <- function(values) {
  if (is.numeric(values)) format(values, scientific = FALSE, trim = TRUE, digits = 15) else as.character(values)
}

# A table's key columns.
# table: A table in `database_tables`.
# Returns: The column names.
edit_table_key <- function(table) database_tables[[table]]$key

# The rows of a table with these keys.
# keys: Keys as text, from `edit_key()`.
# Returns: The matching rows, in the table's order.
edit_rows <- function(snap, table, keys) {
  rows <- snap[[table]]
  rows[edit_key(rows, edit_table_key(table)) %in% keys, , drop = FALSE]
}

# The tables whose rows can be reached from rows of `table`: those that point at them, however far; `table`
# included.
edit_descendant_tables <- function(table) {
  found <- table
  repeat {
    children <- vapply(
      Filter(function(relation) relation$parent %in% found, edit_relations),
      function(relation) relation$child, character(1)
    )
    if (all(children %in% found)) break
    found <- union(found, children)
  }
  found
}

# The rows that point at some rows of their parent table.
# relation: A `relation()`.
# parent_keys: The keys of rows of `relation$parent`.
# Returns: The keys of the rows of `relation$child` that point at them.
edit_referencing <- function(snap, relation, parent_keys) {
  parents <- edit_rows(snap, relation$parent, parent_keys)
  children <- snap[[relation$child]]
  by <- relation$by
  points_at <- stats::complete.cases(children[names(by)]) &
    edit_key(children, names(by)) %in% edit_key(parents, unname(by))
  edit_key(children[points_at, , drop = FALSE], edit_table_key(relation$child))
}

# ------ The cascade ----------------------------------------------------------------------
# What deleting some rows takes with it.
# start: A list of keys named after their tables: the rows being deleted.
# Returns: A list: `start` (as given), `removed` (the keys of every row that goes, by table, the starting rows
#   first), `cleared` (rows that stay but have a column emptied: `table`, `columns`, `keys`, `what`), `forgotten`
#   (the components no placement names once these go, which the database forgets) and `blocked` (what is in the
#   way, if anything: each says `text` and `fix` and lists the `labels` of the rows involved).
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

  while (length(queue) > 0) {
    step <- queue[[1]]
    queue <- queue[-1]
    for (relation in Filter(function(r) r$parent == step$table, edit_relations)) {
      hits <- edit_referencing(snap, relation, step$keys)
      if (length(hits) == 0) next
      if (relation$on_delete == "remove") enqueue(relation$child, hits)
      if (relation$on_delete == "clear") {
        cleared <- c(cleared, list(list(table = relation$child, columns = names(relation$by), keys = hits, what = relation$clears)))
      }
    }
  }

  # A row that's going anyway needn't have anything emptied.
  cleared <- lapply(cleared, function(clear) {
    clear$keys <- setdiff(clear$keys, removed[[clear$table]])
    clear
  })
  cleared <- Filter(function(clear) length(clear$keys) > 0, cleared)

  list(
    start = start, removed = removed, cleared = cleared,
    forgotten = edit_forgotten_components(snap, removed), blocked = edit_blockers(snap, removed)
  )
}

# The components left with no placement once `removed` goes, which the database forgets when the change ends
# (unless a measurement scale names them). Not the components being deleted themselves.
# Returns: Their keys.
edit_forgotten_components <- function(snap, removed) {
  placed <- snap$placement
  if (is.null(placed) || length(removed$placement) == 0 || is.null(snap$component)) {
    return(character())
  }
  going <- edit_key(placed, "placement_id") %in% removed$placement
  lost <- unique(placed$component_id[going])
  still <- unique(placed$component_id[!going])
  scaled <- if (!is.null(snap$question_scale)) snap$question_scale$component_id else numeric()
  orphans <- setdiff(lost, c(still, scaled))
  setdiff(key_text(orphans), removed$component)
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
  pdf_file = c("PDF", "PDFs"),
  component = c("component", "components"),
  placement = c("block in a questionnaire", "blocks in questionnaires"),
  wave_question = c("question in a wave", "questions in waves"),
  variable = c("variable", "variables"),
  answer_option = c("answer choice", "answer choices"),
  response_option = c("response option for a variable", "response options for variables"),
  tag = c("tag", "tags"),
  placement_tag = c("tag on a question", "tags on questions"),
  variable_tag = c("tag on a variable", "tags on variables"),
  response_option_tag = c("tag on an answer choice", "tags on answer choices"),
  measurement_scale = c("measurement scale", "measurement scales"),
  question_scale = c("scale item", "scale items")
)

# A count of rows with their table's noun.
# For example "1 wave", "3 studies".
# Returns: A string; see `plural()`.
plural_noun <- function(n, table) {
  nouns <- edit_nouns[[table]]
  plural(n, nouns[1], plural_form = nouns[2])
}

# The order lists name kinds of rows in: the outermost first.
edit_display_order <- c(
  "registry", "study", "questionnaire", "wave", "placement", "wave_question", "variable", "response_option",
  "placement_tag", "variable_tag", "response_option_tag", "question_scale", "component", "measurement_scale",
  "answer_option", "tag", "principal_investigator", "respondent_type", "pdf_file"
)

# Naming a row can take other tables (a wave's name starts with its questionnaire's), so reading rows to name
# means reading these too.
edit_label_needs <- list(
  wave = "questionnaire",
  placement = c("questionnaire", "component"),
  wave_question = c("questionnaire", "wave", "placement", "component"),
  question_scale = c("measurement_scale", "component"),
  response_option = c("variable", "answer_option"),
  placement_tag = c("tag", "placement", "component"),
  variable_tag = c("tag", "variable"),
  response_option_tag = c("tag", "variable", "answer_option"),
  pdf_file = "wave"
)

# Every table to read to work out what deleting rows of some tables reaches, and to name what it finds.
# tables: Table names.
# Returns: Table names.
edit_tables_to_read <- function(tables) {
  reached <- unique(unlist(lapply(tables, edit_descendant_tables)))
  # a placement that goes may leave a component nothing prints; whether a scale keeps it
  if ("placement" %in% reached) reached <- unique(c(reached, "component", "question_scale"))
  named_by <- unique(unlist(edit_label_needs[reached]))
  unique(c(reached, named_by, unlist(edit_label_needs[named_by])))
}

# One name per row.
# For example "AHP · Do you now smoke cigarettes?".
# rows: Rows of the table.
# Returns: A character vector; empty for no rows.
edit_label <- function(snap, table, rows) {
  if (nrow(rows) == 0) {
    return(character())
  }
  edit_labels[[table]](snap, rows)
}

# Look up a column of a table by id.
# id_column: The column holding the ids.
# ids: The ids to look up.
# column: The column to read.
# Returns: The values of `column`, one for each id, `NA` where there is no such row.
edit_lookup <- function(snap, table, id_column, ids, column) {
  snap[[table]][[column]][match(ids, snap[[table]][[id_column]])]
}

# The wave_question rows of one placement: the number it was given in each wave that asked it.
# placement_id: The placement.
edit_placed_rows <- function(snap, placement_id) {
  rows <- snap$wave_question
  rows[rows$placement_id %in% placement_id, , drop = FALSE]
}

# Questionnaires' short names.
# A questionnaire's abbreviation, or its name if it has none.
# ids: Questionnaire ids.
edit_questionnaire_short <- function(snap, ids) {
  short <- edit_lookup(snap, "questionnaire", "questionnaire_id", ids, "abbreviation")
  name <- edit_lookup(snap, "questionnaire", "questionnaire_id", ids, "name")
  ifelse(is.na(short) | !nzchar(short), ifelse(is.na(name), "?", name), short)
}

# A component's printed words, shortened.
# A group has no words, and a section can be untitled; those get a placeholder.
# ids: Component ids.
# max_characters: Where to cut the words short: only in a sentence (a summary, a warning). The page shows
#   them whole, since it has the room.
edit_component_text <- function(snap, ids, max_characters = Inf) {
  text <- edit_lookup(snap, "component", "component_id", ids, "text")
  kind <- edit_lookup(snap, "component", "component_id", ids, "kind")
  blank <- is.na(text) | !nzchar(trimws(text))
  text <- shorten(text, max_characters)
  text[blank] <- ifelse(is.na(kind[blank]), "(no printed words)", ifelse(kind[blank] == "Group", "(group)", paste0("(untitled ", tolower(kind[blank]), ")")))
  text
}

# A tag as a sentence names it: its name, and its kind when the sentence doesn't say.
edit_tag_text <- function(snap, tag_ids) {
  paste0(edit_lookup(snap, "tag", "tag_id", tag_ids, "name"), " (", edit_lookup(snap, "tag", "tag_id", tag_ids, "kind"), ")")
}

edit_labels <- list(
  registry = function(snap, rows) rows$name,
  principal_investigator = function(snap, rows) rows$name,
  study = function(snap, rows) rows$name,
  respondent_type = function(snap, rows) rows$name,
  questionnaire = function(snap, rows) name_with_abbreviation(rows$name, rows$abbreviation),
  wave = function(snap, rows) paste(edit_questionnaire_short(snap, rows$questionnaire_id), rows$name),
  pdf_file = function(snap, rows) edit_pdf_names(snap, rows$pdf_sha256),
  component = function(snap, rows) edit_component_text(snap, rows$component_id),
  placement = function(snap, rows) {
    paste0(edit_questionnaire_short(snap, rows$questionnaire_id), " · ", edit_component_text(snap, rows$component_id))
  },
  wave_question = function(snap, rows) {
    waves <- edit_lookup(snap, "wave", "wave_id", rows$wave_id, "name")
    paste0(
      edit_questionnaire_short(snap, rows$questionnaire_id), " ", waves, " · question ", edit_called_number(rows),
      " · ", edit_component_text(snap, edit_placement_component(snap, rows$placement_id))
    )
  },
  variable = function(snap, rows) {
    paste0(rows$matr_id, ifelse(is.na(rows$label) | !nzchar(rows$label), "", paste0(" · ", rows$label)))
  },
  answer_option = function(snap, rows) rows$standardized_label,
  response_option = function(snap, rows) {
    matr_id <- edit_lookup(snap, "variable", "variable_key", rows$variable_key, "matr_id")
    shown <- edit_lookup(snap, "answer_option", "answer_option_id", rows$answer_option_id, "standardized_label")
    paste0(matr_id, " · ", shown, " (code ", rows$code_value, ")")
  },
  tag = function(snap, rows) rows$name,
  placement_tag = function(snap, rows) {
    paste0(edit_tag_text(snap, rows$tag_id), " on “", edit_component_text(snap, edit_placement_component(snap, rows$placement_id)), "”")
  },
  variable_tag = function(snap, rows) {
    paste0(edit_tag_text(snap, rows$tag_id), " on ", edit_lookup(snap, "variable", "variable_key", rows$variable_key, "matr_id"))
  },
  response_option_tag = function(snap, rows) {
    paste0(
      edit_tag_text(snap, rows$tag_id), " on ", edit_lookup(snap, "variable", "variable_key", rows$variable_key, "matr_id"),
      " · ", edit_lookup(snap, "answer_option", "answer_option_id", rows$answer_option_id, "standardized_label")
    )
  },
  measurement_scale = function(snap, rows) rows$name,
  question_scale = function(snap, rows) {
    scale <- edit_lookup(snap, "measurement_scale", "scale_id", rows$scale_id, "name")
    paste0(ifelse(is.na(scale), "Scale", scale), " item ", rows$item_number, " · ", edit_component_text(snap, rows$component_id))
  }
)

# ------ What a delete reaches, for staff ---------------------------------------------------------
# The cascade as lists staff can read.
# How many of each kind of row go, a few of each by name, the components the database will forget, how it
# falls on each questionnaire, and what is changed without going.
# cascade: The result of `edit_service_cascade()`.
# examples: How many rows to name for each kind.
# Returns: A list: `lines`, `changed`, `by_questionnaire` and `total` (how many other rows are affected).
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
  if (length(cascade$forgotten) > 0) {
    rows <- edit_rows(snap, "component", cascade$forgotten)
    lines <- c(lines, list(list(
      table = "component", n = nrow(rows),
      text = paste(plural_noun(nrow(rows), "component"), "no questionnaire prints any more (forgotten by the database)"),
      examples = utils::head(edit_label(snap, "component", rows), examples), more = max(0, nrow(rows) - examples)
    )))
  }
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

# How a delete falls on each questionnaire: for every one that loses rows, what it loses.
edit_by_questionnaire <- function(snap, cascade) {
  tables <- c("placement", "wave_question", "variable", "wave")
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
