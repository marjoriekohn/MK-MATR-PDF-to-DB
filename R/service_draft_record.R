# ------ What the AI extracted, and how the reviewed copy differs from it ----------------
# For an AI-Powered extraction the database keeps the questionnaire as the AI returned it
# (ai_extracted_draft.ai_original, never changed afterwards) and how the reviewed copy differs from it
# (ai_extracted_draft.review_edits), worked out again each time the reviewer saves. Later the
# differences can be read to measure how often and where the AI needs correcting. A Manual extraction
# has neither.
#
# The differences depend only on the two questionnaires, not on how the reviewer got from one to the
# other: a value changed and changed back is no difference, a value changed three times is one, and
# saving again gives the same list. Rows are matched by their `ref`, the id each row had when the
# draft was loaded from what the AI returned (service_draft.R), so a row keeps its match however it is
# moved, and a row the reviewer added (no `ref`) is an addition. One entry for each thing that differs:
#   change     "edited" a field, "added" a block, variable or choice, "deleted" one, or "moved" one
#   item       "questionnaire", "block", "variable" or "choice"
#   ref        which row of the AI's answer ("c12", "v30", "o71"); none for a row the reviewer added
#   where      which one, in words: Question 4 > variable 2 > choice “Yes”
#   field      for "edited": which field (and "position" for "moved")
#   before     what the AI returned (nothing for "added")
#   after      what the reviewed copy has (nothing for "deleted")
# Who saved it, and when, are the draft's (draft.saved_by, draft.saved_at).
#
# Only what the AI reads is compared. Not compared: what the site works out (each question's place
# among the questions, display orders, the choices' codes), the reviewer's own bookkeeping (the
# "checked" mark) and MATR IDs, which the AI doesn't read (they come from a CSV or are typed in).
# Values are compared exactly, capitals, punctuation and spaces inside included; only a blank and a
# missing value are the same, and a variable about the respondent is blank however it was written.

# The fields that are compared, for each table of a draft.
review_edit_fields <- list(
  components = c("component_type", "text", "printed_number", "self_contained", "condition", "parent_question_id"),
  variables = c("variable_type", "label", "subject", "answer_data_type", "skip_condition", "skip_instruction"),
  response_options = c("answer_option", "printed_label", "skip_instruction")
)
review_edit_items <- c(components = "block", variables = "variable", response_options = "choice")

#' How a reviewed copy of a questionnaire differs from what the AI returned.
#'
#' @param original The draft loaded from what the AI returned (`draft_service_from_json(ai_original)`):
#'   its rows' refs are their ids.
#' @param reviewed The reviewed copy, as the Edit Extraction page has it; see the top of
#'   `service_draft.R`.
#' @return A list with one entry for each thing that differs (see the top of this file); empty if
#'   nothing does.
draft_service_review_edits <- function(original, reviewed) {
  entry <- function(change, item, ref, where, field = NULL, before = NULL, after = NULL) {
    list(change = change, item = item, ref = ref, where = where, field = field, before = before, after = after)
  }
  edits <- review_questionnaire_edits(original$questionnaire, reviewed$questionnaire, entry)
  for (table in names(review_edit_fields)) {
    old <- original[[table]]
    new <- reviewed[[table]]
    item <- review_edit_items[[table]]
    kept <- intersect(old$id, new$ref)
    for (ref in setdiff(old$id, kept)) {
      edits[[length(edits) + 1]] <- entry("deleted", item, ref, review_edit_place(original, table, ref),
        before = review_edit_summary(original, table, ref)
      )
    }
    for (id in new$id[is.na(new$ref) | !new$ref %in% old$id]) {
      edits[[length(edits) + 1]] <- entry("added", item, NULL, review_edit_place(reviewed, table, id),
        after = review_edit_summary(reviewed, table, id)
      )
    }
    # the fields of the rows in both, a whole column at a time; one entry per field that differs, row by row
    old_rows <- match(kept, old$id)
    new_rows <- match(kept, new$ref)
    fields <- review_edit_fields[[table]]
    differs <- vapply(fields, function(field) {
      a <- review_edit_keys(original, table, old_rows, field)
      b <- review_edit_keys(reviewed, table, new_rows, field)
      xor(is.na(a), is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
    }, logical(length(kept)))
    differs <- matrix(differs, nrow = length(kept), dimnames = list(NULL, fields))
    for (i in which(rowSums(differs) > 0)) {
      ref <- kept[i]
      id <- new$id[new_rows[i]]
      for (field in fields[differs[i, ]]) {
        before <- review_edit_value(original, table, ref, field)
        after <- review_edit_value(reviewed, table, id, field)
        edits[[length(edits) + 1]] <- entry("edited", item, ref, review_edit_place(reviewed, table, id), field, before, after)
      }
    }
    for (ref in review_edit_moved(original, reviewed, table)) {
      id <- new$id[match(ref, new$ref)]
      edits[[length(edits) + 1]] <- entry(
        "moved", item, ref, review_edit_place(reviewed, table, id), "position",
        review_edit_position(original, table, ref), review_edit_position(reviewed, table, id)
      )
    }
  }
  edits
}

#' The blocks of a reviewed copy that differ from what the AI returned, for the outline's "Changed from
#' AI" filter.
#'
#' A block is changed when it was added, when a field the accuracy record compares differs, or when one of
#' its variables or choices was added, taken out or has such a field that differs. Moves don't count: a
#' block moved is the same block, somewhere else.
#'
#' @param original The draft loaded from what the AI returned, or `NULL` (a draft typed by hand).
#' @param reviewed The draft as the Edit Extraction page has it.
#' @return The changed blocks' ids, or `NULL` when there is nothing to compare with.
draft_service_changed_blocks <- function(original, reviewed) {
  if (is.null(original)) {
    return(NULL)
  }
  # the question (or other block) each row belongs to
  block_of <- list(
    components = function(draft, ids) ids,
    variables = function(draft, ids) draft$variables$component_id[match(ids, draft$variables$id)],
    response_options = function(draft, ids) {
      boxes <- draft$response_options$variable_id[match(ids, draft$response_options$id)]
      draft$variables$component_id[match(boxes, draft$variables$id)]
    }
  )
  changed <- character()
  for (table in names(review_edit_fields)) {
    old <- original[[table]]
    new <- reviewed[[table]]
    added <- is.na(new$ref) | !new$ref %in% old$id
    kept <- which(!added)
    old_rows <- match(new$ref[kept], old$id)
    differs <- rep(FALSE, length(kept))
    for (field in review_edit_fields[[table]]) {
      a <- review_edit_keys(original, table, old_rows, field)
      b <- review_edit_keys(reviewed, table, kept, field)
      differs <- differs | xor(is.na(a), is.na(b)) | (!is.na(a) & !is.na(b) & a != b)
    }
    changed <- c(changed, block_of[[table]](reviewed, new$id[c(which(added), kept[differs])]))
    if (table != "components") {
      # a variable or choice taken out changes the block it was in (the AI's ids are the reviewed copy's refs)
      then <- block_of[[table]](original, setdiff(old$id, new$ref))
      changed <- c(changed, reviewed$components$id[match(then, reviewed$components$ref)])
    }
  }
  unique(changed[!is.na(changed)])
}

# The questionnaire's own name and abbreviation, as printed.
review_questionnaire_edits <- function(original, reviewed, entry) {
  edits <- list()
  for (field in c("name", "abbreviation")) {
    before <- review_blank(original[[field]])
    after <- review_blank(reviewed[[field]])
    if (!identical(before, after)) {
      edits[[length(edits) + 1]] <- entry("edited", "questionnaire", NULL, "The questionnaire", field, before, after)
    }
  }
  edits
}

# A value as text, or NULL for none (a blank is none).
review_blank <- function(value) {
  if (length(value) == 0 || is.na(value[1])) {
    return(NULL)
  }
  value <- trimws(as.character(value[1]))
  if (nzchar(value)) value
}

# A field's value as text, or NULL for none. A follow-up's parent is shown as the question's number.
review_edit_value <- function(draft, table, id, field) {
  value <- draft[[table]][[field]][match(id, draft[[table]]$id)]
  if (field == "parent_question_id") {
    value <- draft_service_question_number(draft$components[match(value, draft$components$id), , drop = FALSE])
  }
  if (field == "subject") value <- blank_respondent(value)
  review_blank(value)
}

# What a field's values (of rows `rows` of a table) are compared by: the values, trimmed, a blank as NA
# (and the respondent's subject as NA), except a follow-up's parent, which is which question of the AI's
# answer it is (its ref; "new" for one the reviewer added), so renumbering never counts.
review_edit_keys <- function(draft, table, rows, field) {
  value <- draft[[table]][[field]][rows]
  if (field == "parent_question_id") {
    refs <- draft$components$ref[match(value, draft$components$id)]
    return(ifelse(is.na(value), NA_character_, ifelse(is.na(refs), "new", refs)))
  }
  if (field == "subject") value <- blank_respondent(value)
  value <- trimws(as.character(value))
  value[!is.na(value) & !nzchar(value)] <- NA
  value
}

# `value`, or `otherwise` when it is missing.
if_missing <- function(value, otherwise) if (length(value) == 0 || is.na(value)) otherwise else value

# The rows of the AI's answer that the reviewed copy has somewhere else. A row has moved when it is in
# another section, question group, question or variable than its place in the AI's answer says, or
# when, among the rows that stayed together in one place, it is out of their order. A row whose section
# was deleted, and that now sits where the section did, hasn't moved: the deleted section is the
# difference. Of rows put in a new order, only the fewest that account for it count (those outside the
# longest run kept in order), so moving one block doesn't make all its neighbours "moved".
review_edit_moved <- function(original, reviewed, table) {
  old <- original[[table]]
  new <- reviewed[[table]]
  kept <- intersect(old$id, new$ref)
  if (length(kept) == 0) {
    return(character())
  }
  parent_column <- parent_columns[[table]]
  parent_table <- c(components = "components", variables = "components", response_options = "variables")[[table]]
  # where each row was in the AI's answer, skipping parents the reviewer deleted
  was_in <- vapply(kept, function(ref) {
    parent <- old[[parent_column]][match(ref, old$id)]
    while (!is.na(parent) && !parent %in% reviewed[[parent_table]]$ref) {
      parent <- if (parent_table == table) old[[parent_column]][match(parent, old$id)] else NA_character_
    }
    if_missing(parent, "")
  }, character(1))
  # where it is in the reviewed copy, as a row of the AI's answer ("+" for a parent the reviewer added)
  is_in <- vapply(kept, function(ref) {
    parent <- new[[parent_column]][match(ref, new$ref)]
    if (is.na(parent)) "" else if_missing(reviewed[[parent_table]]$ref[match(parent, reviewed[[parent_table]]$id)], paste0("+", parent))
  }, character(1))
  moved <- kept[was_in != is_in]
  # rows that stayed together can have come from different places (out of a deleted section), so blocks
  # are put in order by where they are printed, not by their place in their section
  old_rank <- if (table == "components") match(old$id, draft_service_printed_order(original)) else old$display_order
  new_rank <- if (table == "components") match(new$id, draft_service_printed_order(reviewed)) else new$display_order
  for (place in unique(was_in[was_in == is_in])) {
    together <- kept[was_in == place & is_in == place]
    in_old_order <- together[order(old_rank[match(together, old$id)])]
    in_new_order <- together[order(new_rank[match(together, new$ref)])]
    moved <- c(moved, setdiff(in_new_order, longest_common_run(in_old_order, in_new_order)))
  }
  kept[kept %in% moved]
}

# The longest subsequence the two orders of the same rows share.
longest_common_run <- function(a, b) {
  n <- length(a)
  if (n == 0) {
    return(character())
  }
  # the longest increasing run of b's rows' places in a (patience sorting), O(n log n)
  places <- match(b, a)
  tails <- integer()
  before <- integer(n)
  for (i in seq_len(n)) {
    k <- findInterval(places[i] - 0.5, places[tails]) + 1
    before[i] <- if (k > 1) tails[k - 1] else 0L
    tails[k] <- i
  }
  run <- integer()
  i <- tails[length(tails)]
  while (i > 0) {
    run <- c(b[i], run)
    i <- before[i]
  }
  run
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
  fields <- review_edit_fields[[table]]
  values <- vapply(fields, function(field) {
    value <- review_edit_value(draft, table, id, field)
    if (is.null(value)) "" else paste0(field, ": ", value)
  }, character(1))
  paste(values[nzchar(values)], collapse = "; ")
}

# "inside Section “Your health”, position 2": where the row sits, in words that don't need ids.
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
