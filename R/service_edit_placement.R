# ------ Components inside one questionnaire ---------------------------------------------
# A questionnaire doesn't hold components, it holds placements of them: a row
# of questionnaire_component says which component, under which section, in
# what position. Questions have a second row (questionnaire_question) for what's
# true of them in this questionnaire, and a row per wave that asked them
# (wave_question) with the number they were printed with. Changing any of these
# changes that one questionnaire; the component itself, and the other
# questionnaires placing it, are untouched.
#
# The exception is the component's words or kind. Those belong to the
# component, so changing them for one questionnaire means a new component with
# the new words, which this questionnaire then uses instead (see
# edit_service_swap_items()).

#' Whether two parents are the same.
#'
#' "No parent" (the top level) is `NA`, and counts as the same as another `NA`.
#'
#' @param a,b Parent ids (vectors of equal length or length one).
#' @return A logical vector.
same_parent <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

#' One questionnaire's placements, with each one's component's kind and words.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param questionnaire_id The questionnaire's id.
#' @return The `questionnaire_component` rows of that questionnaire, with `type` and `text` added.
edit_service_placements <- function(snap, questionnaire_id) {
  placed <- snap$questionnaire_component
  placed <- placed[placed$questionnaire_id == questionnaire_id, , drop = FALSE]
  placed$type <- edit_lookup(
    snap, "component_type", "component_type_id",
    edit_lookup(snap, "component", "component_id", placed$component_id, "component_type_id"), "name"
  )
  placed$text <- edit_lookup(snap, "component", "component_id", placed$component_id, "text")
  placed
}

#' The placements in the order they print.
#'
#' Each section's contents straight after it.
#'
#' @param placed One questionnaire's placements, from `edit_service_placements()`.
#' @return `placed` in printed order, with `depth` added: how far in a placement sits (0 at the top level).
edit_service_outline <- function(placed) {
  ordered <- list()
  walk <- function(parent, depth) {
    siblings <- placed[same_parent(placed$parent_component_id, parent), , drop = FALSE]
    siblings <- siblings[order(siblings$display_order), , drop = FALSE]
    for (i in seq_len(nrow(siblings))) {
      row <- siblings[i, , drop = FALSE]
      row$depth <- depth
      ordered[[length(ordered) + 1]] <<- row
      walk(row$component_id, depth + 1)
    }
  }
  walk(NA_real_, 0)
  if (length(ordered) == 0) {
    return(cbind(placed[0, , drop = FALSE], depth = numeric()))
  }
  do.call(rbind, ordered)
}

# Everything under a component, however deep: the rows of `rows` whose `parent`
# column points at it, and the rows that point at those, and so on. `parent` is
# "parent_component_id" for the blocks in a questionnaire, and "parent_question_id"
# for the questions that follow up on a question.
edit_service_descendants <- function(rows, component_id, parent = "parent_component_id") {
  found <- numeric()
  frontier <- component_id
  while (length(frontier) > 0) {
    inside <- rows$component_id[rows[[parent]] %in% frontier]
    frontier <- setdiff(inside, found)
    found <- c(found, frontier)
  }
  found
}

# Where a placement goes: a new parent (NA for the top level) and a position
# among that parent's contents, counting from 1. Returns what changes: the
# placement, and every other one whose position moves up or down to make room
# or close the gap, as (component_id, old_parent, new_parent, old_order,
# new_order).
edit_service_move <- function(placed, component_id, parent, position) {
  moved <- placed[placed$component_id == component_id, , drop = FALSE]
  others <- placed[placed$component_id != component_id, , drop = FALSE]
  in_new_home <- others[same_parent(others$parent_component_id, parent), , drop = FALSE]
  in_new_home <- in_new_home[order(in_new_home$display_order), , drop = FALSE]
  position <- max(1, min(position, nrow(in_new_home) + 1))
  new_order <- append(in_new_home$component_id, component_id, after = position - 1)

  changes <- data.frame(component_id = new_order, new_parent = parent, new_order = seq_along(new_order))
  if (!same_parent(moved$parent_component_id, parent)) {
    # the old home closes up
    in_old_home <- others[same_parent(others$parent_component_id, moved$parent_component_id), , drop = FALSE]
    in_old_home <- in_old_home[order(in_old_home$display_order), , drop = FALSE]
    changes <- rbind(changes, data.frame(
      component_id = in_old_home$component_id, new_parent = in_old_home$parent_component_id,
      new_order = seq_len(nrow(in_old_home))
    ))
  }
  now <- placed[match(changes$component_id, placed$component_id), ]
  changes$old_parent <- now$parent_component_id
  changes$old_order <- now$display_order
  changes <- changes[!same_parent(changes$old_parent, changes$new_parent) | changes$old_order != changes$new_order, ]
  rownames(changes) <- NULL
  changes[c("component_id", "old_parent", "new_parent", "old_order", "new_order")]
}

#' How many placements are under a parent.
#'
#' Where a new placement can go without a clash: the contents of `parent` are `n`, so
#' it can take positions 1 to `n + 1`.
#'
#' @param placed One questionnaire's placements.
#' @param parent The parent's component id; `NA` for the top level.
#' @return `n`.
edit_service_room <- function(placed, parent) {
  sum(same_parent(placed$parent_component_id, parent))
}

# ------ Parents --------------------------------------------------------------------------
#' The sections a placement can go in.
#'
#' Any in the same questionnaire, except itself and what is inside it (a section can't be
#' inside itself).
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param placed One questionnaire's placements.
#' @param component_id The placement being moved, if any.
#' @return A named character vector for a drop-down; see `edit_choices()`.
edit_service_parent_choices <- function(snap, placed, component_id = NULL) {
  sections <- placed[placed$type == "Section", , drop = FALSE]
  barred <- if (!is.null(component_id)) c(component_id, edit_service_descendants(placed, component_id))
  sections <- sections[!sections$component_id %in% barred, , drop = FALSE]
  outline <- edit_service_outline(placed)
  sections <- sections[order(match(sections$component_id, outline$component_id)), , drop = FALSE]
  depth <- outline$depth[match(sections$component_id, outline$component_id)]
  words <- shorten(ifelse(is.na(sections$text) | !nzchar(sections$text), "(untitled section)", sections$text), 60)
  c("Top level (not inside a section)" = "", stats::setNames(
    key_text(sections$component_id), paste0(strrep("— ", depth), words)
  ))
}

#' The questions a question can follow up on.
#'
#' The ones printed before it in its questionnaire, except what is already a follow-up of it.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param questionnaire_id The questionnaire's id.
#' @param component_id The question being edited, if any.
#' @return A named character vector for a drop-down; see `edit_choices()`.
edit_service_parent_question_choices <- function(snap, questionnaire_id, component_id = NULL) {
  placed <- edit_service_placements(snap, questionnaire_id)
  outline <- edit_service_outline(placed)
  questions <- snap$questionnaire_question[snap$questionnaire_question$questionnaire_id == questionnaire_id, , drop = FALSE]
  asked <- outline[outline$component_id %in% questions$component_id, , drop = FALSE]
  if (!is.null(component_id) && component_id %in% asked$component_id) {
    asked <- asked[seq_len(match(component_id, asked$component_id) - 1), , drop = FALSE]
  }
  followers <- if (!is.null(component_id)) edit_service_descendants(questions, component_id, parent = "parent_question_id")
  asked <- asked[!asked$component_id %in% followers, , drop = FALSE]
  stats::setNames(key_text(asked$component_id), shorten(asked$text, 70))
}

# ------ Where a component is used ----------------------------------------------------------
#' Every questionnaire that places a component.
#'
#' With where, the waves that asked it (when it is a question) and its variables.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param component_id The component's id.
#' @return A list with one list per questionnaire; empty if none.
edit_service_uses <- function(snap, component_id) {
  placed <- snap$questionnaire_component
  placed <- placed[placed$component_id == component_id, , drop = FALSE]
  if (nrow(placed) == 0) {
    return(list())
  }
  lapply(seq_len(nrow(placed)), function(i) {
    row <- placed[i, ]
    q_id <- row$questionnaire_id
    section <- edit_lookup(snap, "component", "component_id", row$parent_component_id, "text")
    waves <- edit_placed_rows(snap, "wave_question", q_id, component_id)
    wave_names <- edit_lookup(snap, "wave", "wave_id", waves$wave_id, "name")
    boxes <- sum(snap$variable$questionnaire_id == q_id & snap$variable$component_id == component_id)
    list(
      questionnaire_id = q_id,
      questionnaire = edit_label(snap, "questionnaire", snap$questionnaire[snap$questionnaire$questionnaire_id == q_id, ]),
      section = if (is.na(row$parent_component_id)) NA_character_ else shorten(section, 50),
      position = row$display_order,
      numbers = stats::setNames(waves$question_number, wave_names),
      waves = wave_names,
      variables = boxes
    )
  })
}

#' How many questionnaires print each component.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param component_ids Component ids.
#' @return A numeric vector, one count for each id.
edit_service_questionnaire_count <- function(snap, component_ids) {
  placed <- snap$questionnaire_component
  vapply(component_ids, function(id) length(unique(placed$questionnaire_id[placed$component_id == id])), numeric(1))
}

# ------ Giving one questionnaire a different component -------------------------------------
# Rows are written as items (edit_item()), in the order they must be applied:
# the new component and what it needs exist before anything points at it, and
# the old placement goes last. The apply step runs them in one transaction with
# the display-order constraints deferred, since the new placement briefly
# shares its position with the old.
#
# `new` is list(component_type_id, text). The new component takes over this
# questionnaire's placement: where it is, `where` (list(parent, display_order);
# the old placement's if left out), what's inside it and, for a question, its
# settings, its wave numbers, its variables and its MeSH categories.
# `between` is items that change the new rows (named by the new component's
# `new:component:1`), written before the old placement goes. Measurement scale memberships aren't copied: each item number in
# a scale belongs to one question, so the scale keeps pointing at the old
# wording.
edit_service_swap_items <- function(snap, questionnaire_id, component_id, new, where = NULL, is_question = FALSE,
                                    between = NULL) {
  q <- questionnaire_id
  c_old <- component_id
  placement <- snap$questionnaire_component[
    snap$questionnaire_component$questionnaire_id == q & snap$questionnaire_component$component_id == c_old,
  ]
  token <- "new:component:1"
  items <- list()
  add <- function(...) items[[length(items) + 1]] <<- edit_item(...)

  add("component", edit_identifier(list(component_id = token)), "Insert", "component_type_id", NA, new$component_type_id)
  if (!is.na(new$text)) add("component", edit_identifier(list(component_id = token)), "Insert", "text", NA, new$text)
  if (is_question) add("question", edit_identifier(list(component_id = token)), "Insert", "component_id", NA, token)

  where <- where %||% list(parent = placement$parent_component_id, display_order = placement$display_order)
  new_key <- edit_identifier(list(component_id = token, questionnaire_id = q))
  if (!is.na(where$parent)) add("questionnaire_component", new_key, "Insert", "parent_component_id", NA, where$parent)
  add("questionnaire_component", new_key, "Insert", "display_order", NA, where$display_order)

  inside <- snap$questionnaire_component[
    snap$questionnaire_component$questionnaire_id == q & snap$questionnaire_component$parent_component_id %in% c_old,
  ]
  for (child in inside$component_id) {
    add(
      "questionnaire_component", edit_identifier(list(component_id = child, questionnaire_id = q)),
      "Update", "parent_component_id", c_old, token
    )
  }

  if (is_question) items <- c(items, edit_swap_question_items(snap, q, c_old, token))
  if (!is.null(between)) items <- c(items, list(between))

  old_key <- edit_identifier(list(component_id = c_old, questionnaire_id = q))
  if (is_question) {
    old_question <- edit_placed_rows(snap, "questionnaire_question", q, c_old)
    for (column in c("parent_question_id", "self_contained", "condition")) {
      add("questionnaire_question", old_key, "Delete", column, old_question[[column]], NA)
    }
  }
  add("questionnaire_component", old_key, "Delete", "parent_component_id", placement$parent_component_id, NA)
  add("questionnaire_component", old_key, "Delete", "display_order", placement$display_order, NA)
  do.call(rbind, items)
}

# The rows a question brings with it when its component changes.
edit_swap_question_items <- function(snap, q, c_old, token) {
  items <- list()
  add <- function(...) items[[length(items) + 1]] <<- edit_item(...)
  new_key <- edit_identifier(list(component_id = token, questionnaire_id = q))

  settings <- edit_placed_rows(snap, "questionnaire_question", q, c_old)
  for (column in c("parent_question_id", "self_contained", "condition")) {
    if (!is.na(settings[[column]])) add("questionnaire_question", new_key, "Insert", column, NA, settings[[column]])
  }

  followers <- snap$questionnaire_question[
    snap$questionnaire_question$questionnaire_id == q & snap$questionnaire_question$parent_question_id %in% c_old,
  ]
  for (follower in followers$component_id) {
    add(
      "questionnaire_question", edit_identifier(list(component_id = follower, questionnaire_id = q)),
      "Update", "parent_question_id", c_old, token
    )
  }
  asked <- edit_placed_rows(snap, "wave_question", q, c_old)
  for (i in seq_len(nrow(asked))) {
    add(
      "wave_question", edit_identifier(list(component_id = c_old, questionnaire_id = q, wave_id = asked$wave_id[i])),
      "Update", "component_id", c_old, token
    )
  }
  boxes <- snap$variable[snap$variable$questionnaire_id == q & snap$variable$component_id == c_old, ]
  for (key in boxes$variable_key) {
    add("variable", edit_identifier(list(variable_key = key)), "Update", "component_id", c_old, token)
  }
  tags <- snap$question_category[snap$question_category$component_id == c_old, ]
  for (mesh_id in tags$mesh_id) {
    add("question_category", edit_identifier(list(component_id = token, mesh_id = mesh_id)), "Insert", "mesh_id", NA, mesh_id)
  }
  if (length(items) == 0) NULL else items
}

# ------ Writing a change to a block in one questionnaire -------------------------------------------
#' What a form for a questionnaire component starts with beyond the row itself.
#'
#' The component's words and kind.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param row The `questionnaire_component` row.
#' @param scope The questionnaire's key.
#' @return A named list by field name.
edit_placement_initial <- function(snap, row, scope) {
  list(
    text = edit_lookup(snap, "component", "component_id", row$component_id, "text"),
    component_type_id = edit_lookup(snap, "component", "component_id", row$component_id, "component_type_id")
  )
}

#' What a form for a questionnaire question starts with beyond the row itself.
#'
#' The component's words, and each wave's question number (`qn_<wave id>`).
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param row The `questionnaire_question` row.
#' @param scope The questionnaire's key.
#' @return A named list by field name.
edit_question_initial <- function(snap, row, scope) {
  asked <- edit_placed_rows(snap, "wave_question", row$questionnaire_id, row$component_id)
  numbers <- stats::setNames(as.list(asked$question_number), paste0("qn_", key_text(asked$wave_id)))
  c(list(text = edit_lookup(snap, "component", "component_id", row$component_id, "text")), numbers)
}

# A new component's words and kind from the form, or NULL if they're as before.
# A question has no kind field: its kind is always Question.
edit_new_component <- function(changes, fields, values, type_field = "component_type_id", prefix) {
  if (!any(c("text", type_field) %in% changes$name)) {
    return(NULL)
  }
  field <- function(name) Filter(function(f) f$name == name, fields)[[1]]
  has_type <- any(vapply(fields, function(f) f$name == type_field, logical(1)))
  list(
    component_type_id = if (has_type) {
      as.numeric(edit_service_normalize(
        field(type_field),
        values[[paste0(prefix, "__", type_field)]]
      ))
    } else {
      NA_real_
    },
    text = edit_service_normalize(field("text"), values[[paste0(prefix, "__text")]])
  )
}

#' The items for a questionnaire component, added or changed.
#'
#' Changed words or kind make a new component for this questionnaire; changed nesting or
#' position move it and its neighbours.
#'
#' @param mode `"add"` or `"update"`.
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param row The row being updated.
#' @param scope The questionnaire's key.
#' @param fields The form's fields, from `edit_service_fields()`.
#' @param values What is in the form, by field id.
#' @return A data frame of items; see `edit_item()`.
edit_placement_items <- function(mode, snap, row, scope, fields, values) {
  q <- as.numeric(scope)
  placed <- edit_service_placements(snap, q)
  prefix <- "questionnaire_component"
  items <- list()
  add <- function(items_to_add) if (!is.null(items_to_add)) items[[length(items) + 1]] <<- items_to_add
  value_of <- function(name) values[[paste0(prefix, "__", name)]]
  parent <- if (nzchar(value_of("parent_component_id") %||% "")) as.numeric(value_of("parent_component_id")) else NA_real_

  if (mode == "add") {
    return(edit_placement_add_items(snap, q, placed, fields, values, parent))
  }

  initial <- edit_service_initial(edit_entities[[prefix]], fields, snap, "update", row, scope)
  changes <- edit_service_changes(fields, initial, values)
  c_old <- row$component_id
  position <- edit_whole_number(value_of("display_order"))
  moved <- any(c("parent_component_id", "display_order") %in% changes$name)
  moves <- if (moved) edit_service_move(placed, c_old, parent, position) else NULL
  new_component <- edit_new_component(changes, fields, values, prefix = prefix)

  here <- if (moved) moves[moves$component_id == c_old, ] else NULL
  where <- list(parent = parent, display_order = if (!is.null(here) && nrow(here) == 1) here$new_order else row$display_order)
  if (!moved) where <- NULL

  if (is.null(new_component)) {
    # the same component, somewhere else
    if (moved) {
      for (i in seq_len(nrow(moves))) add(edit_move_items(q, moves[i, ]))
    }
  } else {
    if (moved) for (i in seq_len(nrow(moves))) if (moves$component_id[i] != c_old) add(edit_move_items(q, moves[i, ]))
    add(edit_service_swap_items(snap, q, c_old, new_component, where, is_question = edit_is_question(snap, c_old)))
  }
  if (length(items) == 0) NULL else do.call(rbind, items)
}

# The Update items that put a placement in a new parent or position.
edit_move_items <- function(q, move) {
  identifier <- edit_identifier(list(questionnaire_id = q, component_id = move$component_id))
  rows <- list()
  if (!same_parent(move$old_parent, move$new_parent)) {
    rows[[length(rows) + 1]] <- edit_item(
      "questionnaire_component",
      identifier,
      "Update",
      "parent_component_id",
      move$old_parent,
      move$new_parent
    )
  }
  if (move$old_order != move$new_order) {
    rows[[length(rows) + 1]] <- edit_item("questionnaire_component", identifier, "Update", "display_order", move$old_order, move$new_order)
  }
  do.call(rbind, rows)
}

# Placing a component in a questionnaire: one that's there already, or a new
# one, at the position asked (the end if none).
edit_placement_add_items <- function(snap, q, placed, fields, values, parent) {
  prefix <- "questionnaire_component"
  value_of <- function(name) values[[paste0(prefix, "__", name)]]
  field <- function(name) Filter(function(f) f$name == name, fields)[[1]]
  items <- list()
  add <- function(rows) items[[length(items) + 1]] <<- rows

  if (identical(value_of("source"), "existing")) {
    component <- as.numeric(value_of("component_id"))
  } else {
    component <- "new:component:1"
    identifier <- edit_identifier(list(component_id = component))
    add(edit_item(
      "component",
      identifier,
      "Insert",
      "component_type_id",
      NA,
      edit_service_normalize(field("component_type_id"), value_of("component_type_id"))
    ))
    text <- edit_service_normalize(field("text"), value_of("text"))
    if (!is.na(text)) add(edit_item("component", identifier, "Insert", "text", NA, text))
  }

  room <- edit_service_room(placed, parent)
  position <- edit_whole_number(value_of("display_order"))
  position <- if (is.na(position)) room + 1 else max(1, min(position, room + 1))
  # room for it among its neighbours
  later <- placed[same_parent(placed$parent_component_id, parent) & placed$display_order >= position, , drop = FALSE]
  for (i in seq_len(nrow(later))) {
    add(edit_item(
      "questionnaire_component", edit_identifier(list(questionnaire_id = q, component_id = later$component_id[i])),
      "Update", "display_order", later$display_order[i], later$display_order[i] + 1
    ))
  }

  identifier <- edit_identifier(list(questionnaire_id = q, component_id = component))
  if (!is.na(parent)) add(edit_item("questionnaire_component", identifier, "Insert", "parent_component_id", NA, parent))
  add(edit_item("questionnaire_component", identifier, "Insert", "display_order", NA, position))
  do.call(rbind, items)
}

#' The items for a questionnaire question changed.
#'
#' Its words (a new question for this questionnaire), whether it stands alone and what it
#' follows up on, its condition, and its number in each wave.
#'
#' @inheritParams edit_placement_items
#' @return A data frame of items; see `edit_item()`.
edit_question_items <- function(mode, snap, row, scope, fields, values) {
  q <- as.numeric(scope)
  prefix <- "questionnaire_question"
  initial <- edit_service_initial(edit_entities[[prefix]], fields, snap, "update", row, scope)
  changes <- edit_service_changes(fields, initial, values)
  c_old <- row$component_id
  new_component <- edit_new_component(changes, fields, values, type_field = "none", prefix = prefix)
  # After a swap the question is the new component, so the same changes name that.
  target <- if (is.null(new_component)) c_old else "new:component:1"
  identifier <- edit_identifier(list(questionnaire_id = q, component_id = target))
  edits <- list()
  for (i in seq_len(nrow(changes))) {
    name <- changes$name[i]
    if (name %in% c("self_contained", "parent_question_id", "condition")) {
      edits[[length(edits) + 1]] <- edit_item(
        "questionnaire_question",
        identifier,
        "Update",
        name,
        changes$old_value[i],
        changes$new_value[i]
      )
    }
    if (startsWith(name, "qn_")) {
      wave <- as.numeric(sub("^qn_", "", name))
      edits[[length(edits) + 1]] <- edit_item(
        "wave_question", edit_identifier(list(questionnaire_id = q, wave_id = wave, component_id = target)),
        "Update", "question_number", changes$old_value[i], changes$new_value[i]
      )
    }
  }
  edits <- if (length(edits) > 0) do.call(rbind, edits)

  if (is.null(new_component)) {
    return(edits)
  }
  new_component$component_type_id <- edit_type_id(snap, "Question")
  edit_service_swap_items(snap, q, c_old, new_component, is_question = TRUE, between = edits)
}

#' The items for a new question's wording.
#'
#' A component of the kind Question (`new:component:1`), and its question row.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param text The question's words, as saved (`edit_service_normalize()`).
#' @return A data frame of items; see `edit_item()`.
edit_new_question_items <- function(snap, text) {
  token <- "new:component:1"
  identifier <- edit_identifier(list(component_id = token))
  rbind(
    edit_item("component", identifier, "Insert", "component_type_id", NA, edit_type_id(snap, "Question")),
    edit_item("component", identifier, "Insert", "text", NA, text),
    edit_item("question", identifier, "Insert", "component_id", NA, token)
  )
}
