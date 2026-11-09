# ------ Blocks inside one questionnaire ---------------------------------------------------
# A questionnaire doesn't hold components, it holds placements of them: a row of placement says which component,
# under which section, grid or group (another placement), in what position, and for a question which earlier
# question it depends on. A questionnaire can place a component more than once. A question has a row per wave
# that asked it (wave_question) with its place among the wave's questions and the number printed beside it, and
# its variables. Changing any of these changes that one placement; the component itself, and the other placements
# of it, are untouched.
#
# The exception is the component's words. Those belong to the component, kept once for each kind and words, so
# changing them at one placement points that placement at the component with the new words (the one there, or a
# new one): see edit_service_swap_items(). The old component is forgotten by the database when nothing places it.

# Whether two parents are the same.
# "No parent" (the top level) is `NA`, and counts as the same as another `NA`.
# a,b: Parent ids (vectors of equal length or length one).
same_parent <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

# One questionnaire's placements, with each one's component's kind and words.
# Returns: The `placement` rows of that questionnaire, with `kind` and `text` added.
edit_service_placements <- function(snap, questionnaire_id) {
  placed <- snap$placement
  placed <- placed[placed$questionnaire_id == questionnaire_id, , drop = FALSE]
  placed$kind <- edit_lookup(snap, "component", "component_id", placed$component_id, "kind")
  placed$text <- edit_lookup(snap, "component", "component_id", placed$component_id, "text")
  placed
}

# The placements in the order they print.
# Each section's contents straight after it.
# placed: One questionnaire's placements, from `edit_service_placements()`.
# Returns: `placed` in printed order, with `depth` added: how far in a placement sits (0 at the top level).
edit_service_outline <- function(placed) {
  ordered <- list()
  walk <- function(parent, depth) {
    siblings <- placed[same_parent(placed$parent_placement_id, parent), , drop = FALSE]
    siblings <- siblings[order(siblings$display_order), , drop = FALSE]
    for (i in seq_len(nrow(siblings))) {
      row <- siblings[i, , drop = FALSE]
      row$depth <- depth
      ordered[[length(ordered) + 1]] <<- row
      walk(row$placement_id, depth + 1)
    }
  }
  walk(NA_real_, 0)
  if (length(ordered) == 0) {
    return(cbind(placed[0, , drop = FALSE], depth = numeric()))
  }
  do.call(rbind, ordered)
}

# Everything under a placement, however deep: the rows of `rows` whose `parent` column points at it, and the rows
# that point at those, and so on. `parent` is "parent_placement_id" for the blocks inside one, and
# "depends_on_placement_id" for the questions that depend on a question.
edit_service_descendants <- function(rows, placement_id, parent = "parent_placement_id") {
  found <- numeric()
  frontier <- placement_id
  while (length(frontier) > 0) {
    inside <- rows$placement_id[rows[[parent]] %in% frontier]
    frontier <- setdiff(inside, found)
    found <- c(found, frontier)
  }
  found
}

# Where a placement goes: a new parent placement (NA for the top level) and a position among that parent's
# contents, counting from 1. Returns what changes: the placement, and every other one whose position moves up or
# down to make room or close the gap, as (placement_id, old_parent, new_parent, old_order, new_order).
edit_service_move <- function(placed, placement_id, parent, position) {
  moved <- placed[placed$placement_id == placement_id, , drop = FALSE]
  others <- placed[placed$placement_id != placement_id, , drop = FALSE]
  in_new_home <- others[same_parent(others$parent_placement_id, parent), , drop = FALSE]
  in_new_home <- in_new_home[order(in_new_home$display_order), , drop = FALSE]
  position <- max(1, min(position, nrow(in_new_home) + 1))
  new_order <- append(in_new_home$placement_id, placement_id, after = position - 1)

  changes <- data.frame(placement_id = new_order, new_parent = parent, new_order = seq_along(new_order))
  if (!same_parent(moved$parent_placement_id, parent)) {
    # the old home closes up
    in_old_home <- others[same_parent(others$parent_placement_id, moved$parent_placement_id), , drop = FALSE]
    in_old_home <- in_old_home[order(in_old_home$display_order), , drop = FALSE]
    changes <- rbind(changes, data.frame(
      placement_id = in_old_home$placement_id, new_parent = in_old_home$parent_placement_id,
      new_order = seq_len(nrow(in_old_home))
    ))
  }
  now <- placed[match(changes$placement_id, placed$placement_id), ]
  changes$old_parent <- now$parent_placement_id
  changes$old_order <- now$display_order
  changes <- changes[!same_parent(changes$old_parent, changes$new_parent) | changes$old_order != changes$new_order, ]
  rownames(changes) <- NULL
  changes[c("placement_id", "old_parent", "new_parent", "old_order", "new_order")]
}

# How many placements are under a parent.
# Where a new placement can go without a clash: the contents of `parent` are `n`, so it can take positions 1 to
# `n + 1`.
# parent: The parent's placement id; `NA` for the top level.
# Returns: `n`.
edit_service_room <- function(placed, parent) {
  sum(same_parent(placed$parent_placement_id, parent))
}

# ------ Parents --------------------------------------------------------------------------
# The sections, grids and groups a placement can go in.
# Any in the same questionnaire, except itself and what is inside it (a section can't be inside itself).
# placement_id: The placement being moved, if any.
# Returns: A named character vector for a drop-down, of placement ids; see `edit_choices()`.
edit_service_parent_choices <- function(snap, placed, placement_id = NULL) {
  sections <- placed[is_container(placed$kind), , drop = FALSE]
  barred <- if (!is.null(placement_id)) c(placement_id, edit_service_descendants(placed, placement_id))
  sections <- sections[!sections$placement_id %in% barred, , drop = FALSE]
  outline <- edit_service_outline(placed)
  sections <- sections[order(match(sections$placement_id, outline$placement_id)), , drop = FALSE]
  depth <- outline$depth[match(sections$placement_id, outline$placement_id)]
  # a group has no words: it is named by the words of the first block inside it
  first_inside <- vapply(sections$placement_id, function(id) {
    inside <- placed[placed$parent_placement_id %in% id, , drop = FALSE]
    if (nrow(inside) == 0) "empty" else inside$text[which.min(inside$display_order)]
  }, character(1))
  words <- ifelse(
    sections$kind == "Group", paste0("(group) ", first_inside),
    ifelse(
      sections$kind == "Grid", paste0("(grid) ", sections$text),
      ifelse(is.na(sections$text) | !nzchar(sections$text), "(untitled section)", sections$text)
    )
  )
  c("Top level (not inside a section, grid or group)" = "", stats::setNames(
    key_text(sections$placement_id), paste0(strrep("— ", depth), words)
  ))
}

# The questions a question can depend on.
# The ones printed before it in its questionnaire, except what already depends on it.
# placement_id: The placement of the question being edited, if any.
# Returns: A named character vector for a drop-down, of placement ids; see `edit_choices()`.
edit_service_depends_choices <- function(snap, questionnaire_id, placement_id = NULL) {
  placed <- edit_service_placements(snap, questionnaire_id)
  outline <- edit_service_outline(placed)
  asked <- outline[outline$kind %in% "Question", , drop = FALSE]
  # any other question, not just those printed before it: the same change may move it after them
  asked <- asked[!asked$placement_id %in% placement_id, , drop = FALSE]
  followers <- if (!is.null(placement_id)) edit_service_descendants(placed, placement_id, parent = "depends_on_placement_id")
  asked <- asked[!asked$placement_id %in% followers, , drop = FALSE]
  stats::setNames(key_text(asked$placement_id), asked$text)
}

# ------ Where a component is used ----------------------------------------------------------
# Every place a questionnaire prints a component (a questionnaire can print it more than once).
# With where, the waves that asked it there (when it is a question) and its variables there.
# component_id: The component's id.
# Returns: A list with one list per placement, by questionnaire name and position; empty if none. Each has
#   `questionnaire_id`, `placement_id`, `component_id`, `kind`, `questionnaire` (its name), `section`, `position`, `numbers`
#   (the number it was asked as, named by wave), `waves`, `variables` (how many) and `boxes` (each variable:
#   `matr_id`, `tags` and `coding`, see `edit_use_boxes()`).
edit_service_uses <- function(snap, component_id) {
  placed <- snap$placement
  placed <- placed[placed$component_id == component_id, , drop = FALSE]
  if (nrow(placed) == 0) {
    return(list())
  }
  uses <- lapply(seq_len(nrow(placed)), function(i) {
    row <- placed[i, ]
    q_id <- row$questionnaire_id
    section <- edit_lookup(snap, "component", "component_id", edit_placement_component(snap, row$parent_placement_id), "text")
    waves <- if (is.null(snap$wave_question)) snap$placement[0, c("placement_id"), drop = FALSE] else edit_placed_rows(snap, row$placement_id)
    waves <- waves[order(edit_lookup(snap, "wave", "wave_id", waves$wave_id, "administered_date"), waves$wave_id), , drop = FALSE]
    wave_names <- edit_lookup(snap, "wave", "wave_id", waves$wave_id, "name")
    boxes <- if (is.null(snap$variable)) NULL else snap$variable[snap$variable$placement_id %in% row$placement_id, , drop = FALSE]
    list(
      questionnaire_id = q_id, placement_id = row$placement_id, component_id = component_id,
      kind = edit_lookup(snap, "component", "component_id", component_id, "kind"),
      questionnaire = edit_label(snap, "questionnaire", snap$questionnaire[snap$questionnaire$questionnaire_id == q_id, ]),
      section = if (is.na(row$parent_placement_id)) NA_character_ else section,
      position = row$display_order,
      numbers = stats::setNames(edit_called_number(waves), wave_names),
      waves = wave_names,
      variables = NROW(boxes),
      boxes = if (NROW(boxes) > 0) edit_use_boxes(snap, boxes) else list()
    )
  })
  uses[order(
    tolower(vapply(uses, function(use) use$questionnaire, character(1))), vapply(uses, function(use) as.numeric(use$position), numeric(1)),
    method = "radix"
  )]
}

# A question's variables in one questionnaire, as its card there shows them.
# boxes: Rows of `variable`.
# Returns: A list with one list per variable, in order: `matr_id`, `tags` ("[Smokes now] [Respondent]", see
#   `edit_variable_tags()`) and `coding`, its choices ("1 = Yes", in order) or a write-in's kind of answer.
edit_use_boxes <- function(snap, boxes) {
  boxes <- boxes[order(boxes$display_order, boxes$matr_id), , drop = FALSE]
  tags <- edit_variable_tags(snap, boxes)
  lapply(seq_len(nrow(boxes)), function(i) {
    key <- boxes$variable_key[i]
    choices <- snap$response_option # (a kind that hasn't read them shows none)
    if (!is.null(choices)) choices <- choices[choices$variable_key %in% key, , drop = FALSE]
    if (NROW(choices) > 0) choices <- choices[order(choices$display_order), , drop = FALSE]
    coding <- if (NROW(choices) > 0) {
      shown <- edit_lookup(snap, "answer_option", "answer_option_id", choices$answer_option_id, "standardized_label")
      paste0(ifelse(is.na(choices$code_value), "?", choices$code_value), " = ", ifelse(is.na(shown), "(blank)", shown))
    } else if (identical(boxes$variable_type[i], "open_ended")) {
      paste("Write-in:", if (is.na(boxes$answer_data_type[i])) "answer type not picked" else boxes$answer_data_type[i])
    } else {
      character()
    }
    list(matr_id = boxes$matr_id[i], tags = tags[i], coding = coding)
  })
}

# The component each placement prints.
# placement_ids: Placement ids (NA for none).
# Returns: The component ids, NA where there is no placement.
edit_placement_component <- function(snap, placement_ids) {
  placed <- snap$placement
  placed$component_id[match(placement_ids, placed$placement_id)]
}

# How many questionnaires print each component.
# component_ids: Component ids.
# Returns: A numeric vector, one count for each id.
edit_service_questionnaire_count <- function(snap, component_ids) {
  placed <- snap$placement
  vapply(component_ids, function(id) length(unique(placed$questionnaire_id[placed$component_id == id])), numeric(1))
}

# ------ The component with these words -------------------------------------------------------------
# The component a block of `kind` with these words is: the one the database has, or a new one. The items that add
# a new one, if it is new.
# made: The token a new component would get.
# Returns: A list: `component` (its id, or the token) and `items` (the Insert items, or `NULL`).
edit_component_for <- function(snap, kind, text, made = "new:component:1") {
  text <- if (identical(kind, "Group")) NA_character_ else text
  same <- if (identical(kind, "Group")) {
    snap$component[snap$component$kind %in% "Group", , drop = FALSE]
  } else {
    edit_same_component(snap, kind, text)
  }
  if (nrow(same) > 0) {
    return(list(component = min(same$component_id), items = NULL))
  }
  identifier <- edit_identifier(list(component_id = made))
  items <- rbind(
    edit_item("component", identifier, "Insert", "kind", NA, kind),
    if (!is.na(text)) edit_item("component", identifier, "Insert", "text", NA, text)
  )
  list(component = made, items = items)
}

# ------ Giving one placement other words -------------------------------------------------------------
# New words at one placement point that placement at the component of the same kind with them: the one the
# database has, or a new one. What the placement holds (its numbers in each wave, its variables, its tags) is the
# placement's, so it stays as it is. Measurement scale memberships aren't copied: each item number in a scale
# belongs to one question, so the scale keeps pointing at the old wording. The old component is forgotten by the
# database when nothing places it any more.
# text: The new words, as saved (`edit_service_normalize()`).
edit_service_swap_items <- function(snap, placement_id, text) {
  placement <- snap$placement[snap$placement$placement_id == placement_id, ]
  kind <- edit_lookup(snap, "component", "component_id", placement$component_id, "kind")
  target <- edit_component_for(snap, kind, text)
  rbind(
    target$items,
    edit_item("placement", edit_identifier(list(placement_id = placement_id)), "Update", "component_id", placement$component_id, target$component)
  )
}

# ------ Writing a change to a block in one questionnaire -------------------------------------------
# What a form for a block in one questionnaire starts with beyond the row itself.
# The component's words and kind, and the number printed beside a question in each wave that asks it
# (`qn_<wave id>`).
# row: The `placement` row.
# Returns: A named list by field name.
edit_placement_initial <- function(snap, row, scope) {
  asked <- edit_placed_rows(snap, row$placement_id)
  numbers <- if (nrow(asked) == 0) list() else stats::setNames(as.list(asked$printed_number), paste0("qn_", key_text(asked$wave_id)))
  c(
    list(
      text = edit_lookup(snap, "component", "component_id", row$component_id, "text"),
      kind = edit_lookup(snap, "component", "component_id", row$component_id, "kind")
    ),
    numbers
  )
}

# The items for a block in one questionnaire, added or changed.
# Changed words make this place print the component with them; changed nesting or position move it and its
# neighbours, and the waves number their questions again; a question's dependency and printed numbers are its own.
edit_placement_items <- function(mode, snap, row, scope, fields, values) {
  q <- as.numeric(scope)
  if (mode == "add") {
    return(edit_placement_add_items(snap, q, fields, values))
  }
  placed <- edit_service_placements(snap, q)
  value_of <- function(name) values[[paste0("placement__", name)]]
  parent <- if (has_text(value_of("parent_placement_id"))) as.numeric(value_of("parent_placement_id")) else NA_real_
  initial <- edit_service_initial(edit_entities$placement, fields, snap, "update", row, scope)
  changes <- edit_service_changes(fields, initial, values)
  items <- list()
  add <- function(rows) if (!is.null(rows)) items[[length(items) + 1]] <<- rows

  moved <- any(c("parent_placement_id", "display_order") %in% changes$name)
  if (moved) {
    moves <- edit_service_move(placed, row$placement_id, parent, edit_whole_number(value_of("display_order")))
    for (i in seq_len(nrow(moves))) add(edit_move_items(moves[i, ]))
    # a move puts the questions in a new order, so each wave numbers them again
    add(edit_renumber_waves(snap, q, edit_moved_placements(placed, moves)))
  }
  if ("text" %in% changes$name) add(edit_service_swap_items(snap, row$placement_id, changes$new_value[changes$name == "text"]))
  identifier <- edit_identifier(list(placement_id = row$placement_id))
  for (i in seq_len(nrow(changes))) {
    name <- changes$name[i]
    if (name == "depends_on_placement_id") {
      add(edit_item("placement", identifier, "Update", name, changes$old_value[i], changes$new_value[i]))
    }
    if (startsWith(name, "qn_")) {
      wave <- as.numeric(sub("^qn_", "", name))
      add(edit_item(
        "wave_question", edit_identifier(list(placement_id = row$placement_id, wave_id = wave)),
        "Update", "printed_number", changes$old_value[i], changes$new_value[i]
      ))
    }
  }
  if (length(items) == 0) NULL else do.call(rbind, items)
}

# The Update items that put a placement in a new parent or position.
edit_move_items <- function(move) {
  identifier <- edit_identifier(list(placement_id = move$placement_id))
  rows <- list()
  if (!same_parent(move$old_parent, move$new_parent)) {
    rows[[length(rows) + 1]] <- edit_item("placement", identifier, "Update", "parent_placement_id", move$old_parent, move$new_parent)
  }
  if (move$old_order != move$new_order) {
    rows[[length(rows) + 1]] <- edit_item("placement", identifier, "Update", "display_order", move$old_order, move$new_order)
  }
  do.call(rbind, rows)
}

# ------ Adding a block to one questionnaire -----------------------------------------------------------------
# The items for a block added to one questionnaire, in the order the database needs them: its component (if the
# wording is new), its place in the outline (the blocks after it moving over to make room), and for a question its
# dependency, its place among the questions of each wave that asks it (the questions after it numbered again) and
# the number printed beside it there, and its variables with their choices and tags.
edit_placement_add_items <- function(snap, q, fields, values) {
  value_of <- function(name) values[[paste0("placement__", name)]] %||% ""
  field <- function(name) Filter(function(f) f$name == name, fields)[[1]]
  typed <- function(name) edit_service_normalize(field(name), value_of(name))
  items <- list()
  add <- function(rows) if (!is.null(rows)) items[[length(items) + 1]] <<- rows

  kind <- typed("kind")
  component <- edit_component_for(snap, kind, typed("text"))
  add(component$items)

  # its place in the outline
  placed <- edit_service_placements(snap, q)
  parent <- if (has_text(value_of("parent_placement_id"))) as.numeric(value_of("parent_placement_id")) else NA_real_
  room <- edit_service_room(placed, parent)
  position <- edit_whole_number(value_of("display_order"))
  position <- if (is.na(position)) room + 1 else max(1, min(position, room + 1))
  later <- placed[same_parent(placed$parent_placement_id, parent) & placed$display_order >= position, , drop = FALSE]
  for (i in seq_len(nrow(later))) {
    add(edit_item(
      "placement", edit_identifier(list(placement_id = later$placement_id[i])),
      "Update", "display_order", later$display_order[i], later$display_order[i] + 1
    ))
  }
  placement <- "new:placement:1"
  here <- edit_identifier(list(placement_id = placement))
  add(edit_item("placement", here, "Insert", "questionnaire_id", NA, q))
  add(edit_item("placement", here, "Insert", "component_id", NA, component$component))
  if (!is.na(parent)) add(edit_item("placement", here, "Insert", "parent_placement_id", NA, parent))
  add(edit_item("placement", here, "Insert", "display_order", NA, position))
  if (kind != "Question") {
    return(do.call(rbind, items))
  }
  depends_on <- typed("depends_on_placement_id")
  if (!is.na(depends_on)) add(edit_item("placement", here, "Insert", "depends_on_placement_id", NA, as.numeric(depends_on)))

  # each wave that asks it: its place among the wave's questions, once the outline has it, and the number printed
  # beside it
  keys <- function(ids) ifelse(is.na(ids), NA_character_, key_text(ids))
  outline <- placed[c("placement_id", "parent_placement_id", "display_order")]
  outline$display_order[match(later$placement_id, outline$placement_id)] <- later$display_order + 1
  outline$placement_id <- keys(outline$placement_id)
  outline$parent_placement_id <- keys(outline$parent_placement_id)
  outline <- rbind(outline, data.frame(placement_id = placement, parent_placement_id = keys(parent), display_order = position))
  for (wave in as.numeric(edit_several_ids(value_of("asked_in")))) {
    numbers <- edit_wave_numbers(snap, q, wave, edit_wave_order(snap, q, wave, outline, also = placement))
    add(numbers$items)
    asked <- edit_identifier(list(placement_id = placement, wave_id = wave))
    add(edit_item("wave_question", asked, "Insert", "questionnaire_id", NA, q))
    add(edit_item("wave_question", asked, "Insert", "question_number", NA, numbers$numbers[[placement]]))
    printed <- trimws(value_of(paste0("qn_", key_text(wave))))
    if (nzchar(printed)) add(edit_item("wave_question", asked, "Insert", "printed_number", NA, printed))
  }

  # its variables, with their choices and tags
  add(edit_variable_items(snap, q, placement, edit_variables_parse(value_of("variables"))))
  do.call(rbind, items)
}

# The change's sentence for a block added to a questionnaire.
# Returns: A string, e.g. `"Added the question “Do you smoke?” to AHP"`.
edit_placement_add_summary <- function(snap, scope, fields, values) {
  value_of <- function(name) values[[paste0("placement__", name)]] %||% ""
  kind <- value_of("kind")
  words <- if (identical(kind, "Group")) "(group)" else shorten(trimws(value_of("text")), 60)
  paste0(
    "Added the ", tolower(if (has_text(kind)) kind else "block"), " “", words, "” to ",
    edit_questionnaire_short(snap, as.numeric(scope))
  )
}

# ------ Question numbers in a wave ------------------------------------------------------------------------
# A wave numbers the questions it asks 1, 2, 3... in the order the questionnaire prints them
# (wave_question.question_number); sections and groups aren't counted, and staff never type it.
# The number printed beside a question is kept apart (wave_question.printed_number) and can repeat. So a
# change that adds, moves or removes a question numbers the wave's questions again.

# What a question is called in a wave: the number printed beside it, or its place where none is printed.
# rows: `wave_question` rows.
edit_called_number <- function(rows) {
  printed <- rows$printed_number %||% rep(NA_character_, nrow(rows))
  ifelse(!is.na(printed) & nzchar(printed), printed, rows$question_number)
}

# The questions a wave asks, in printed order, once the questionnaire's outline is `placed`.
# wave: The wave's id.
# placed: The questionnaire's placements after the change (`placement_id`, `parent_placement_id`,
#   `display_order`), a new one named by its token ("new:placement:1").
# also: A question placement the change adds to the wave (its id or token), if any.
# Returns: The question placements' keys (`key_text()`), in printed order.
edit_wave_order <- function(snap, q, wave, placed, also = NULL) {
  keys <- function(ids) ifelse(is.na(ids), NA_character_, key_text(ids))
  asked <- snap$wave_question[snap$wave_question$questionnaire_id == q & snap$wave_question$wave_id == wave, , drop = FALSE]
  outline <- placed[c("placement_id", "parent_placement_id", "display_order")]
  outline$placement_id <- keys(outline$placement_id)
  outline$parent_placement_id <- keys(outline$parent_placement_id)
  printed <- edit_service_outline(outline)$placement_id
  questions <- unique(c(keys(asked$placement_id), if (!is.null(also)) key_text(also)))
  questions[order(match(questions, printed))]
}

# The items that number a wave's questions 1, 2, 3... in the order given.
# wave: The wave's id.
# order: The keys of the questions the wave asks after the change, in printed order
#   (`edit_wave_order()`); a question the wave asks now that isn't in it is being removed.
# Returns: A list: `items` (an Update for each question the wave asks already whose number changes, or
#   `NULL`) and `numbers` (each question's number, named by its key).
edit_wave_numbers <- function(snap, q, wave, order) {
  numbers <- stats::setNames(as.character(seq_along(order)), order)
  asked <- snap$wave_question[snap$wave_question$questionnaire_id == q & snap$wave_question$wave_id == wave, , drop = FALSE]
  items <- list()
  for (i in seq_len(nrow(asked))) {
    key <- key_text(asked$placement_id[i])
    if (!key %in% order || identical(as.character(asked$question_number[i]), numbers[[key]])) next
    items[[length(items) + 1]] <- edit_item(
      "wave_question", edit_identifier(list(placement_id = asked$placement_id[i], wave_id = wave)),
      "Update", "question_number", asked$question_number[i], numbers[[key]]
    )
  }
  list(items = if (length(items) > 0) do.call(rbind, items), numbers = numbers)
}

# The items that number again the questions of every wave of a questionnaire whose outline changes.
# placed: The questionnaire's placements after the change.
# Returns: A data frame of Update items, or `NULL` when no number changes.
edit_renumber_waves <- function(snap, q, placed) {
  waves <- unique(snap$wave_question$wave_id[snap$wave_question$questionnaire_id == q])
  items <- lapply(waves, function(wave) edit_wave_numbers(snap, q, wave, edit_wave_order(snap, q, wave, placed))$items)
  items <- Filter(Negate(is.null), items)
  if (length(items) > 0) do.call(rbind, items)
}

# A questionnaire's placements after the moves `edit_service_move()` works out.
# moves: From `edit_service_move()`.
# Returns: `placed`, with the moved placements' parents and positions changed.
edit_moved_placements <- function(placed, moves) {
  rows <- match(moves$placement_id, placed$placement_id)
  placed$parent_placement_id[rows] <- moves$new_parent
  placed$display_order[rows] <- moves$new_order
  placed
}
