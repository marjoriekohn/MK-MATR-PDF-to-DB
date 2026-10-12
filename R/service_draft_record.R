# ------ What the AI extracted, and what the reviewer changed ---------------------------
# For an AI-Powered extraction the database keeps the questionnaire exactly as the AI returned it
# (extracted_draft.ai_original, never changed afterwards) and a list of what the reviewer changed
# (extracted_draft.review_edits), added to each time they save. Later the list can be read to
# measure how often and where the AI needs correcting. A Manual extraction has neither.
#
# Each time the reviewer saves, the draft as it is then is compared with the draft as it was when
# the page opened it or last saved it, and one entry is added for each thing that differs:
#   at         when (the same format as the rest of the site)
#   by, by_name  who
#   change     "edited" a field, "added" a block, variable or choice, "deleted" one, or "moved" one
#   item       "block", "variable" or "choice"
#   where      which one, in words: Question 4 > Variable 2 > Choice "Yes"
#   field      for "edited": which field (and the position for "moved")
#   before     the value before (nothing for "added")
#   after      the value after (nothing for "deleted")
# Not recorded: the "checked" mark, which is the reviewer's own bookkeeping, and MATR IDs, which the
# AI doesn't read (they come from a CSV or are typed in).

# The fields that are compared, for each table of a draft.
review_edit_fields <- list(
  components = c("component_type", "text", "question_number", "self_contained", "condition", "parent_question_id"),
  variables = c("variable_type", "label", "subject", "answer_data_type"),
  response_options = c("answer_option", "code_value", "printed", "printed_label", "skip_instruction")
)
review_edit_items <- c(components = "block", variables = "variable", response_options = "choice")

#' What a reviewer changed between two versions of a draft.
#'
#' @param before,after Drafts with the same row ids (the page keeps them while it is open); see
#'   the top of `service_draft.R`.
#' @param me Who made the changes: `list(staff_id, name, role)`.
#' @param now The time to record.
#' @return A list with one entry for each thing that differs (see the top of this file); empty
#'   if nothing does.
draft_service_review_edits <- function(before, after, me, now = Sys.time()) {
  at <- format_time(now)
  entry <- function(change, table, where, field = NULL, was = NULL, now_value = NULL) {
    list(
      at = at, by = me$staff_id, by_name = me$name, change = change, item = review_edit_items[[table]],
      where = where, field = field, before = was, after = now_value
    )
  }
  edits <- list()
  for (table in names(review_edit_fields)) {
    old <- before[[table]]
    new <- after[[table]]
    kept <- intersect(old$id, new$id)
    for (id in setdiff(old$id, new$id)) {
      edits[[length(edits) + 1]] <- entry("deleted", table, review_edit_place(before, table, id),
        was = review_edit_summary(before, table, id)
      )
    }
    for (id in setdiff(new$id, old$id)) {
      edits[[length(edits) + 1]] <- entry("added", table, review_edit_place(after, table, id),
        now_value = review_edit_summary(after, table, id)
      )
    }
    for (id in kept) {
      for (field in review_edit_fields[[table]]) {
        was <- review_edit_value(before, table, id, field)
        now_value <- review_edit_value(after, table, id, field)
        if (!identical(was, now_value)) {
          edits[[length(edits) + 1]] <- entry("edited", table, review_edit_place(after, table, id), field, was, now_value)
        }
      }
    }
    for (id in review_edit_moved(old, new, table)) {
      edits[[length(edits) + 1]] <- entry(
        "moved", table, review_edit_place(after, table, id), "position",
        review_edit_position(before, table, id), review_edit_position(after, table, id)
      )
    }
  }
  edits
}

# A field's value as text, or NULL for none. A follow-up's parent is shown as the question's number.
review_edit_value <- function(draft, table, id, field) {
  value <- draft[[table]][[field]][match(id, draft[[table]]$id)]
  if (field == "parent_question_id") value <- question_number_of(draft, value)
  if (length(value) == 0 || is.na(value)) NULL else as.character(value)
}

# The rows that kept their id but changed parent, or changed their order among the rows that
# stayed (rows added or deleted around them don't count: they renumber the rest).
review_edit_moved <- function(old, new, table) {
  parent_column <- parent_columns[[table]]
  kept <- intersect(old$id, new$id)
  if (length(kept) == 0) {
    return(character())
  }
  old <- old[match(kept, old$id), , drop = FALSE]
  new <- new[match(kept, new$id), , drop = FALSE]
  parent_changed <- !mapply(identical, as.character(old[[parent_column]]), as.character(new[[parent_column]]))
  rank_among_kept <- function(rows) {
    group <- ifelse(is.na(rows[[parent_column]]), "", as.character(rows[[parent_column]]))
    stats::ave(rows$display_order, group, FUN = function(order) rank(order, ties.method = "first"))
  }
  # compare ranks only inside the rows whose parent is unchanged
  order_changed <- !parent_changed & rank_among_kept(old) != rank_among_kept(new)
  kept[parent_changed | order_changed]
}

# "Question 4", "Question 4 > variable 2", "Question 4 > variable 2 > choice “Yes”".
review_edit_place <- function(draft, table, id) {
  if (table == "components") {
    return(review_edit_block(draft, id))
  }
  if (table == "variables") {
    box <- draft$variables[match(id, draft$variables$id), ]
    return(paste0(review_edit_block(draft, box$component_id), " > variable ", box$display_order))
  }
  choice <- draft$response_options[match(id, draft$response_options$id), ]
  paste0(review_edit_place(draft, "variables", choice$variable_id), " > choice “", choice$answer_option, "”")
}

review_edit_block <- function(draft, id) {
  block <- draft$components[match(id, draft$components$id), , drop = FALSE]
  if (nrow(block) == 0) {
    return("(a block that is gone)")
  }
  block_name(block)
}

# What a whole row said, for an entry that adds or deletes it.
review_edit_summary <- function(draft, table, id) {
  row <- draft[[table]][match(id, draft[[table]]$id), , drop = FALSE]
  fields <- review_edit_fields[[table]]
  values <- vapply(fields, function(field) {
    value <- review_edit_value(draft, table, id, field)
    if (is.null(value)) "" else paste0(field, ": ", value)
  }, character(1))
  paste(values[nzchar(values)], collapse = "; ")
}

# "in section c3, position 2" in words that don't need the id: the section's name and the place.
review_edit_position <- function(draft, table, id) {
  row <- draft[[table]][match(id, draft[[table]]$id), , drop = FALSE]
  parent <- row[[parent_columns[[table]]]]
  inside <- if (is.na(parent)) "at the top level" else paste("inside", review_edit_parent_name(draft, table, parent))
  paste0(inside, ", position ", as.integer(row$display_order))
}

review_edit_parent_name <- function(draft, table, parent) {
  if (table == "components") {
    review_edit_block(draft, parent)
  } else {
    review_edit_place(draft, if (table == "variables") "components" else "variables", parent)
  }
}
