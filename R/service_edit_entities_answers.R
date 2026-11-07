# Make an edit's answer choices (group Shared lists), and merging one into another: an update to the words of
# another choice. The kinds are listed together in edit_entities (service_edit_kinds.R).

# ------ Answer choices --------------------------------------------------------------------------------
answer_option_entity <- list(
  label = "Answer choice", noun = "answer choice", plural = "answer choices", article = "an",
  group = "Shared lists", table = "answer_option",
  about = paste0(
    "One answer a question can offer, like “Yes” or “Sometimes”, kept once for the whole registry. ",
    "Variables use it with their own code. Give a choice the words of another to merge the two."
  ),
  update_about = paste0(
    "Typing the words of another answer choice merges this one into it: every variable and option set that has this ",
    "choice gets that one, with the same codes, and an extraction that reads these words uses it too."
  ),
  delete_about = "An answer choice can only be deleted once no variable offers it and no option set includes it.",
  reads = c("closed_ended", "option_set", "question_scale", "response_option_count", "answer_option_alias"),
  cards = function(snap, scope) {
    rows <- snap$answer_option
    on_boxes <- count_in(snap$response_option$answer_option_id, rows$answer_option_id)
    in_sets <- count_in(snap$answer_option_set$answer_option_id, rows$answer_option_id)
    merged <- snap$answer_option_alias
    edit_cards(
      key_text(rows$answer_option_id), rows$standardized_label,
      subtitle = vapply(rows$answer_option_id, function(id) {
        words <- merged$alias[merged$answer_option_id %in% id]
        if (length(words) == 0) "" else paste0("Also “", paste(words, collapse = "”, “"), "”")
      }, character(1)),
      facts = Map(function(b, s) {
        c(
          if (b > 0) paste("offered on", edit_count(b, "variable", "variables")) else "on no variables",
          if (s > 0) paste("in", edit_count(s, "option set"))
        )
      }, on_boxes, in_sets)
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("standardized_label", "Answer choice",
        required = TRUE, placeholder = "e.g. sometimes", normalize = normal_answer,
        hint = paste0(
          "Saved in lower case, without a question mark or a full stop or colon at the end. A range of numbers is ",
          "written “1-2”. [subject] stands for who the variable is about.",
          if (mode == "update") " Every variable that offers this choice shows the new words. Their codes stay as they are."
        ),
        problem = function(value, values) {
          if (!has_text(value)) {
            return(NULL)
          }
          words <- normal_answer(value)
          merged <- snap$answer_option_alias[snap$answer_option_alias$alias %in% words, , drop = FALSE]
          if (nrow(merged) > 0 && !identical(merged$answer_option_id[1], row$answer_option_id)) {
            into <- edit_lookup(snap, "answer_option", "answer_option_id", merged$answer_option_id[1], "standardized_label")
            return(sprintf("“%s” was merged into “%s”. Use that choice.", words, into))
          }
          same <- edit_taken(snap, "answer_option", "standardized_label", words, row)
          if (nrow(same) == 0) {
            return(NULL)
          }
          if (is.null(row)) {
            return(sprintf("“%s” is already in the list.", same$standardized_label[1]))
          }
          both <- edit_merge_clash(snap, row$answer_option_id, same$answer_option_id[1])
          if (length(both) > 0) {
            sprintf(
              "“%s” can't be merged into “%s”: %s %s both. Take one off first.", row$standardized_label,
              same$standardized_label[1], edit_count(length(both), "variable"), if (length(both) == 1) "offers" else "offer"
            )
          }
        },
        warn = function(value, values) {
          into <- edit_merge_target(snap, row, value)
          if (!is.null(into)) {
            sprintf(
              "“%s” is already in the list, so this merges “%s” into it. An extraction that reads “%s” will use “%s”.",
              into$standardized_label, row$standardized_label, row$standardized_label, into$standardized_label
            )
          }
        }
      )
    )
  },
  build = function(mode, snap, row, scope, fields, values) {
    into <- if (mode == "update") edit_merge_target(snap, row, values[[fields[[1]]$id]])
    if (!is.null(into)) {
      return(edit_merge_items(snap, row$answer_option_id, into$answer_option_id))
    }
    entity <- edit_entities[["answer_option"]]
    if (mode == "add") edit_add_items(entity, snap, fields, values) else edit_update_items(entity, snap, row, fields, values)
  },
  summary_update = function(snap, row, fields, values) {
    into <- edit_merge_target(snap, row, values[[fields[[1]]$id]])
    if (is.null(into)) {
      return(edit_summary_update(edit_entities[["answer_option"]], snap, row, fields, values))
    }
    paste0("Merged the answer choice “", shorten(row$standardized_label, 80), "” into “", shorten(into$standardized_label, 80), "”")
  }
)

# ------ Merging answer choices --------------------------------------------------------------------------
# The answer choice an update of `row` to these words merges it into: the other choice with those words, or NULL.
edit_merge_target <- function(snap, row, value) {
  if (is.null(row) || !has_text(value)) {
    return(NULL)
  }
  same <- edit_taken(snap, "answer_option", "standardized_label", normal_answer(value), row)
  if (nrow(same) == 0) NULL else same[1, , drop = FALSE]
}

# The variables that offer both choices, which can't be merged: a variable offers a choice once.
edit_merge_clash <- function(snap, from, to) {
  offered <- snap$response_option
  intersect(offered$variable_key[offered$answer_option_id %in% from], offered$variable_key[offered$answer_option_id %in% to])
}

# The items of merging one answer choice into another.
# Each variable's response option of `from` becomes one of `to`, with the same order, code, skip and counts of
# answers (a row's key can't change, so it goes and comes back). Each option set with `from` has `to` instead, and
# a set that then has the same choices as another is merged into it: the variables and scale items that used it
# use the other (the one more variables offer). `from`'s words, and the wordings merged into it before, become
# `to`'s, so an extraction that reads them uses `to` (answer_option_alias). Then `from` goes.
# from: The answer choice merged.
# to: The answer choice it is merged into.
edit_merge_items <- function(snap, from, to) {
  items <- list()
  add <- function(...) items[[length(items) + 1]] <<- edit_item(...)
  insert_row <- function(table, key, fields) {
    identifier <- edit_identifier(key)
    fields <- Filter(function(value) length(value) == 1 && !is.na(value), fields)
    if (length(fields) == 0) add(table, identifier, "Insert", names(key)[1], NA, key[[1]])
    for (column in names(fields)) add(table, identifier, "Insert", column, NA, fields[[column]])
  }
  # ---- each variable's response option, and its counts of answers
  offered <- snap$response_option[snap$response_option$answer_option_id %in% from, , drop = FALSE]
  counts <- snap$response_option_count
  for (i in seq_len(nrow(offered))) {
    option <- offered[i, , drop = FALSE]
    mine <- counts[counts$variable_key %in% option$variable_key & counts$answer_option_id %in% from, , drop = FALSE]
    for (j in seq_len(nrow(mine))) {
      add("response_option_count", edit_row_identifier("response_option_count", mine[j, ]), "Delete", NULL, NA, NA)
    }
    add("response_option", edit_row_identifier("response_option", option), "Delete", NULL, NA, NA)
    insert_row("response_option", list(answer_option_id = to, variable_key = option$variable_key), list(
      display_order = option$display_order, code_value = option$code_value, skip_instruction = option$skip_instruction,
      skip_to_placement_id = option$skip_to_placement_id, skip_to_end = option$skip_to_end
    ))
    for (j in seq_len(nrow(mine))) {
      insert_row(
        "response_option_count", list(answer_option_id = to, variable_key = option$variable_key, wave_id = mine$wave_id[j]),
        list(respondent_count = mine$respondent_count[j])
      )
    }
  }
  # ---- the option sets: what each has once `from` is `to`, and which go into another with the same choices
  members <- snap$answer_option_set
  touched <- unique(members$option_set_id[members$answer_option_id %in% from])
  after <- members
  after$answer_option_id[after$answer_option_id %in% from] <- to
  after <- unique(after)
  sets <- unique(after$option_set_id)
  keys <- vapply(sets, function(set) paste(sort(after$answer_option_id[after$option_set_id == set]), collapse = ","), character(1))
  offered_by <- count_in(snap$closed_ended$option_set_id, sets)
  member <- function(set, choice) edit_identifier(list(answer_option_id = choice, option_set_id = set))
  for (set in touched) {
    same <- which(keys == keys[sets == set])
    keep <- sets[same[order(-offered_by[same], sets[same])][1]]
    if (keep != set) {
      # the set goes: its variables and scale items use `keep`, which has the same choices
      for (box in snap$closed_ended$variable_key[snap$closed_ended$option_set_id %in% set]) {
        add("closed_ended", edit_identifier(list(variable_key = box)), "Update", "option_set_id", set, keep)
      }
      scale_items <- snap$question_scale[snap$question_scale$expected_option_set_id %in% set, , drop = FALSE]
      for (j in seq_len(nrow(scale_items))) {
        add("question_scale", edit_row_identifier("question_scale", scale_items[j, ]), "Update", "expected_option_set_id", set, keep)
      }
      for (choice in members$answer_option_id[members$option_set_id %in% set]) {
        add("answer_option_set", member(set, choice), "Delete", "display_order", edit_set_place(snap, set, choice), NA)
      }
      add("option_set", edit_identifier(list(option_set_id = set)), "Delete", NULL, NA, NA)
      next
    }
    place <- edit_set_place(snap, set, from)
    add("answer_option_set", member(set, from), "Delete", "display_order", place, NA)
    # `to` takes `from`'s place, so the set's name reads as before but for the merged choice
    if (!any(members$option_set_id %in% set & members$answer_option_id %in% to)) add("answer_option_set", member(set, to), "Insert", "display_order", NA, place)
  }
  # ---- the words: `from`'s, and those merged into it before, are `to`'s now
  merged <- snap$answer_option_alias
  for (alias in merged$alias[merged$answer_option_id %in% from]) {
    add("answer_option_alias", edit_identifier(list(alias = alias)), "Update", "answer_option_id", from, to)
  }
  insert_row("answer_option_alias", list(alias = edit_lookup(snap, "answer_option", "answer_option_id", from, "standardized_label")), list(answer_option_id = to))
  add("answer_option", edit_identifier(list(answer_option_id = from)), "Delete", "standardized_label",
      edit_lookup(snap, "answer_option", "answer_option_id", from, "standardized_label"), NA)
  do.call(rbind, items)
}
