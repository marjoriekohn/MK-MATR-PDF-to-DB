# ------ Editing a draft --------------------------------------------------------
# Each function takes a draft and returns the changed copy. When a change isn't
# allowed, stop() says what to do instead; message() notes what else changed.
# The review page shows both to staff.

# The fields staff can change by typing or picking. A value keeps its column's type.
editable_fields <- list(
  components = c(
    "component_type", "text", "question_number", "self_contained",
    "condition", "parent_question_id", "checked"
  ),
  variables = c("variable_type", "label", "subject", "answer_data_type", "matr_id"),
  response_options = c(
    "answer_option", "code_value", "printed", "printed_label", "skip_instruction"
  )
)

#' Change one field of one row.
#'
#' What staff type is stored as the column's type, blanks as `NA`. A block's kind decides
#' what else it can hold, so changing it may change other rows. Errors are sentences.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param table `"components"`, `"variables"` or `"response_options"`.
#' @param id The row's id.
#' @param field One of the table's editable fields.
#' @param value The new value, as typed.
#' @return The changed draft.
draft_service_update <- function(draft, table, id, field, value) {
  if (!field %in% editable_fields[[table]]) stop("That can't be changed here.")
  row <- which(draft[[table]]$id == id)
  if (length(row) != 1) stop("That's no longer in the draft.")
  value <- as_field_value(value, typeof(draft[[table]][[field]]))
  switch(field,
    component_type = set_component_type(draft, row, value),
    variable_type = set_variable_type(draft, row, value),
    printed = set_printed(draft, row, value),
    {
      draft[[table]][row, field] <- value
      draft
    }
  )
}

# Browsers send text (or TRUE/FALSE from a checkbox). Blanks become NA.
as_field_value <- function(value, type) {
  value <- trimws(as.character(value %||% ""))
  if (identical(value, "")) value <- NA
  suppressWarnings(as.vector(value, type))
}

# A block's kind decides what else it holds: only a Section holds blocks (BR53),
# and only a Question has a number and variables (BR45).
set_component_type <- function(draft, row, type) {
  block <- draft$components[row, ]
  if (is.na(type)) stop("Pick a kind of block.")
  if (type != "Section" && any(draft$components$parent_id %in% block$id)) {
    stop("Move the blocks out of this section first, then change its kind.")
  }
  draft$components$component_type[row] <- type
  if (type == "Question") {
    if (is.na(block$self_contained)) draft$components$self_contained[row] <- TRUE
    return(draft)
  }
  draft$components[row, c("question_number", "condition", "parent_question_id")] <- NA
  draft$components$self_contained[row] <- NA
  n_boxes <- sum(draft$variables$component_id == block$id)
  if (n_boxes > 0) {
    message(sprintf(
      "Removed %d variable%s, since only questions have them. Undo brings %s back.",
      n_boxes, if (n_boxes == 1) "" else "s", if (n_boxes == 1) "it" else "them"
    ))
  }
  draft$variables <- draft$variables[draft$variables$component_id != block$id, ]
  unlink_followers(drop_orphans(draft), block$id)
}

# Write-in boxes have an answer type; choice boxes have choices instead.
set_variable_type <- function(draft, row, type) {
  box <- draft$variables[row, ]
  if (!type %in% c("closed_ended", "open_ended")) stop("Pick choices or write-in.")
  if (identical(type, box$variable_type)) {
    return(draft)
  }
  draft$variables$variable_type[row] <- type
  if (type == "open_ended") {
    draft$response_options <- draft$response_options[draft$response_options$variable_id != box$id, ]
    return(draft)
  }
  draft$variables$answer_data_type[row] <- NA
  add_blank_choices(draft, box$id, 2)
}

# Printed as and Skip to only mean something for a choice printed on the form,
# so a choice that isn't printed loses them.
set_printed <- function(draft, row, printed) {
  draft$response_options$printed[row] <- printed
  if (!isFALSE(printed)) {
    return(draft)
  }
  printed_fields <- c("printed_label", "skip_instruction")
  if (!all(is.na(draft$response_options[row, printed_fields]))) {
    message("Cleared Printed as and Skip to, since the choice isn't printed. Undo brings them back.")
  }
  draft$response_options[row, printed_fields] <- NA
  draft
}

# ------ Checked blocks ---------------------------------------------------------
# A reviewer marks a block as checked once they have looked at it. If the block is
# edited afterwards the mark goes, so they have to look at it again.

# What a block holds, as one string: its own fields (but not whether it is checked, nor its place
# among its neighbours, which changes when a neighbour does), then each of its variables in
# order with their fields, then each variable's choices in order with theirs. Adding, deleting,
# reordering or changing any of it changes the string. Named by the block's id.
block_contents <- function(draft) {
  blocks <- draft$components
  row_text <- function(rows, leave_out) {
    columns <- lapply(setdiff(names(rows), leave_out), function(column) as.character(rows[[column]]))
    do.call(paste, c(columns, sep = "\r"))
  }
  join_by <- function(text, owner, owners) {
    vapply(split(text, factor(owner, levels = owners)), paste, character(1), collapse = "\n")
  }
  choices <- draft$response_options[order(draft$response_options$variable_id, draft$response_options$display_order), ]
  of_box <- join_by(row_text(choices, c("variable_id", "display_order")), choices$variable_id, draft$variables$id)
  boxes <- draft$variables[order(draft$variables$component_id, draft$variables$display_order), ]
  box_text <- paste(row_text(boxes, c("component_id", "display_order")), of_box[boxes$id], sep = "\r")
  of_block <- join_by(box_text, boxes$component_id, blocks$id)
  stats::setNames(paste(row_text(blocks, c("checked", "display_order")), of_block[blocks$id], sep = "\n"), blocks$id)
}

#' Take the check off the blocks a change has edited.
#'
#' A block that was marked as checked is no longer checked once it, its variables or their
#' choices have changed (their words, kind, codes, which there are, or the order they come in), or
#' once it has been moved into another section. Moving a block among its neighbours in the same
#' section doesn't count. New blocks and blocks that are only checked or unchecked are left alone.
#'
#' @param before,after The draft before and after a change; see the top of `service_draft.R`.
#' @return `after`, with the blocks that were checked and have been edited no longer checked.
draft_service_uncheck_edited <- function(before, after) {
  was <- block_contents(before)
  now <- block_contents(after)
  kept <- intersect(names(was), names(now))
  edited <- kept[was[kept] != now[kept]]
  after$components$checked[after$components$id %in% edited & after$components$checked %in% TRUE] <- FALSE
  after
}

# ------ Moving blocks ----------------------------------------------------------
#' Apply a drag and drop in the outline.
#'
#' The browser reports where every block now sits.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param ids All the block ids in printed order.
#' @param parent_ids The section each block is in; `NA` for the top level.
#' @return The changed draft. An error if the outline was out of date or a non-section
#'   would hold blocks.
draft_service_apply_order <- function(draft, ids, parent_ids) {
  blocks <- draft$components
  if (length(ids) != nrow(blocks) || !setequal(ids, blocks$id)) {
    stop("The outline was out of date. Try that again.")
  }
  parent_types <- blocks$component_type[match(parent_ids, blocks$id)]
  if (any(!is.na(parent_ids) & !parent_types %in% "Section")) {
    stop("Only sections can hold other blocks.")
  }
  rows <- match(ids, blocks$id)
  blocks$parent_id[rows] <- parent_ids
  blocks$display_order[rows] <- seq_along(ids)
  draft$components <- blocks
  draft <- renumber(draft, "components")
  if (length(draft_service_printed_order(draft)) != nrow(blocks)) {
    stop("A section can't go inside itself.")
  }
  draft
}

#' Apply a drag and drop within one question's variables, or one variable's choices.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param table `"variables"` or `"response_options"`.
#' @param ids The ids of the rows being reordered, in their new order.
#' @return The changed draft.
draft_service_reorder <- function(draft, table, ids) {
  rows <- match(ids, draft[[table]]$id)
  if (anyNA(rows)) stop("That list was out of date. Try that again.")
  draft[[table]]$display_order[rows] <- seq_along(ids)
  renumber(draft, table)
}

# ------ Adding and deleting ----------------------------------------------------
#' Add a block.
#'
#' It goes right after the selected one, or inside it if that is a section (unless the
#' new block is a section too), or at the very end.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param component_type The kind of block.
#' @param id The new block's id.
#' @param near_id The selected block's id, or `NULL`.
#' @return The changed draft.
draft_service_add_block <- function(draft, component_type, id, near_id = NULL) {
  near <- draft$components[draft$components$id %in% near_id, ]
  has_near <- nrow(near) == 1
  goes_inside <- has_near && near$component_type == "Section" && component_type != "Section"
  add_rows(draft, "components", data.frame(
    id = id,
    parent_id = if (goes_inside) near$id else if (has_near) near$parent_id else NA_character_,
    display_order = if (has_near && !goes_inside) near$display_order + 0.5 else Inf,
    component_type = component_type,
    self_contained = if (component_type == "Question") TRUE else NA
  ))
}

#' Add a variable to a question.
#'
#' It copies the question's last variable (its kind, who it is about and its choices), since
#' one question's variables are usually alike. A question's first variable starts with two blank choices.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param component_id The question's id.
#' @return The changed draft.
draft_service_add_box <- function(draft, component_id) {
  boxes <- draft_service_children(draft, "variables", component_id)
  id <- draft_service_next_id(draft, "variables")
  if (nrow(boxes) == 0) {
    draft <- add_rows(draft, "variables", data.frame(id = id, component_id = component_id))
    return(add_blank_choices(draft, id, 2))
  }
  template <- boxes[nrow(boxes), ]
  draft <- add_rows(draft, "variables", data.frame(
    id = id,
    component_id = component_id,
    variable_type = template$variable_type,
    subject = template$subject,
    answer_data_type = template$answer_data_type
  ))
  choices <- draft_service_children(draft, "response_options", template$id)
  choices$id <- draft_service_next_id(draft, "response_options", nrow(choices))
  choices$variable_id <- rep(id, nrow(choices))
  add_rows(draft, "response_options", choices)
}

#' Add a blank choice to a variable.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param variable_id The variable's id.
#' @return The changed draft.
draft_service_add_choice <- function(draft, variable_id) {
  add_blank_choices(draft, variable_id, 1)
}

add_blank_choices <- function(draft, variable_id, n) {
  draft <- add_rows(draft, "response_options", data.frame(
    id = draft_service_next_id(draft, "response_options", n),
    variable_id = variable_id
  ))
  default_codes(draft, variable_id)
}

#' Delete a block, variable or choice.
#'
#' A section takes everything inside it, a question its variables, and a variable its
#' choices. Questions that followed a deleted question lose the link. Says "Deleted. Undo
#' brings it back." with `message()`.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param table `"components"`, `"variables"` or `"response_options"`.
#' @param id The row's id.
#' @return The changed draft.
draft_service_delete <- function(draft, table, id) {
  if (table == "components") id <- c(id, draft_service_printed_order(draft, id))
  draft[[table]] <- draft[[table]][!draft[[table]]$id %in% id, ]
  message("Deleted. Undo brings it back.")
  unlink_followers(drop_orphans(renumber(draft, table)), id)
}

# ------ Codes ------------------------------------------------------------------
#' Give choices their default codes.
#'
#' Codes count 1, 2, 3... down a variable's choices until staff change them: a choice without a
#' code gets the next one after the highest in its variable.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param variable_ids Only these variables; `NULL` for all.
#' @return The changed draft.
default_codes <- function(draft, variable_ids = NULL) {
  choices <- draft$response_options
  rows <- which(is.null(variable_ids) | choices$variable_id %in% variable_ids)
  if (length(rows) == 0) {
    return(draft)
  }
  rows <- rows[order(choices$variable_id[rows], choices$display_order[rows])]
  choices$code_value[rows] <- ave(
    choices$code_value[rows], choices$variable_id[rows],
    FUN = function(codes) {
      blank <- is.na(codes)
      codes[blank] <- max(c(0L, codes), na.rm = TRUE) + seq_len(sum(blank))
      codes
    }
  )
  draft$response_options <- choices
  draft
}

#' Give every variable that offers the same choices the codes this variable uses.
#'
#' So a Yes/No coding is typed once rather than per variable.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param variable_id The variable whose codes to copy.
#' @return The changed draft. An error if a choice in the variable has no code.
draft_service_copy_codes <- function(draft, variable_id) {
  source <- draft_service_children(draft, "response_options", variable_id)
  if (nrow(source) == 0 || anyNA(source$code_value)) {
    stop("Give every choice in this variable a code first.")
  }
  choices <- draft$response_options
  choices <- choices[order(choices$variable_id, choices$display_order), ]
  choice_lists <- tapply(choices$answer_option, choices$variable_id, paste, collapse = "\n")
  same_boxes <- names(choice_lists)[choice_lists == choice_lists[[variable_id]]]
  rows <- draft$response_options$variable_id %in% same_boxes
  draft$response_options$code_value[rows] <-
    source$code_value[match(draft$response_options$answer_option[rows], source$answer_option)]
  message(sprintf(
    "Used these codes in %d other variables with the same choices.",
    length(same_boxes) - 1
  ))
  draft
}

# ------ MATR IDs ---------------------------------------------------------------
#' Fill the variables' MATR IDs from a CSV's list.
#'
#' The CSV lists one ID per variable in printed order. If the counts differ, variables past the end
#' of the list get none, and staff are told with `message()`.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param matr_ids The IDs, in order (`extraction_service_matr_ids()`).
#' @return The changed draft. An error if the list is empty.
draft_service_fill_matr_ids <- function(draft, matr_ids) {
  if (length(matr_ids) == 0) stop("The CSV has no MATR IDs in it. Check the file and add it again.")
  rows <- match(draft_service_boxes_in_order(draft), draft$variables$id)
  draft$variables$matr_id[rows] <- matr_ids[seq_along(rows)]
  message(paste(c(
    sprintf("Filled in the MATR IDs of %s.", plural(min(length(rows), length(matr_ids)), "variable")),
    draft_service_matr_id_mismatch(length(matr_ids), length(rows))
  ), collapse = " "))
  draft
}

#' Fill the variables' MATR IDs from the questionnaire's earlier waves.
#'
#' For a new wave of a recorded questionnaire: a question with the same words as one the questionnaire
#' already asks (ignoring capitals and extra spaces) gets its variables' MATR IDs, first variable to first
#' variable and so on. Only blank MATR IDs are filled, so nothing typed or filled from a CSV is lost. Says
#' how many were filled with `message()`; a MATR ID already used by another variable of the draft isn't
#' used again.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param recorded The recorded questions' MATR IDs: a data frame with `text`, `matr_id` and `display_order`
#'   (`extraction_service_recorded_matr_ids()`).
#' @return The changed draft; the same draft if nothing matched.
draft_service_fill_recorded_matr_ids <- function(draft, recorded) {
  same_words <- function(text) tolower(trimws(gsub("[[:space:]]+", " ", ifelse(is.na(text), "", text))))
  blocks <- draft$components
  questions <- blocks[blocks$component_type == "Question" & !is.na(blocks$text), , drop = FALSE]
  recorded <- recorded[order(recorded$display_order), , drop = FALSE]
  n_filled <- 0L
  for (i in seq_len(nrow(questions))) {
    ids <- recorded$matr_id[same_words(recorded$text) == same_words(questions$text[i])]
    ids <- ids[!is.na(ids) & nzchar(ids)]
    if (length(ids) == 0) next
    rows <- match(draft_service_children(draft, "variables", questions$id[i])$id, draft$variables$id)
    for (j in seq_len(min(length(rows), length(ids)))) {
      row <- rows[j]
      if (!is.na(draft$variables$matr_id[row]) || ids[j] %in% draft$variables$matr_id) next
      draft$variables$matr_id[row] <- ids[j]
      n_filled <- n_filled + 1L
    }
  }
  if (n_filled > 0) {
    message(sprintf(
      "Filled in the MATR IDs of %s from the questionnaire's earlier waves, where the question is worded the same.",
      plural(n_filled, "variable")
    ))
  }
  draft
}

#' The words to offer while a block's words are typed.
#'
#' For each kind of block: the words the draft's other blocks of that kind have, and the words the
#' recorded questionnaire prints for that kind (a new wave), each once, in alphabetical order.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param recorded_texts The recorded questionnaire's blocks: a data frame with `component_type` and
#'   `text` (`extraction_service_recorded()$texts`), or `NULL`.
#' @return A named list: for each kind of block that has any, a character vector of words.
draft_service_text_suggestions <- function(draft, recorded_texts = NULL) {
  own <- draft$components[c("component_type", "text")]
  all_texts <- rbind(own, if (!is.null(recorded_texts)) recorded_texts[c("component_type", "text")])
  all_texts <- all_texts[!is.na(all_texts$text) & nzchar(trimws(all_texts$text)), , drop = FALSE]
  if (nrow(all_texts) == 0) {
    return(list())
  }
  lapply(split(all_texts$text, all_texts$component_type), function(texts) sort(unique(trimws(texts))))
}

#' Number the questions 1, 2, 3... in the order they are printed.
#'
#' A question's number follows its place in the questionnaire, so moving, adding or deleting a question
#' renumbers the ones after it. Blocks that aren't questions have no number.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @return The changed draft.
draft_service_renumber_questions <- function(draft) {
  blocks <- draft$components
  printed <- draft_service_printed_order(draft)
  is_question <- blocks$component_type[match(printed, blocks$id)] == "Question"
  numbers <- rep(NA_character_, nrow(blocks))
  numbers[match(printed[is_question], blocks$id)] <- as.character(seq_len(sum(is_question)))
  draft$components$question_number <- numbers
  draft
}

#' What filling leaves undone when the CSV and the draft count variables differently.
#'
#' @param n_ids How many MATR IDs the CSV has.
#' @param n_boxes How many variables the draft has.
#' @return A sentence, or `NULL` when the counts match.
draft_service_matr_id_mismatch <- function(n_ids, n_boxes) {
  if (n_ids == n_boxes) {
    return(NULL)
  }
  extra <- abs(n_ids - n_boxes)
  sprintf(
    "The CSV has %s for %s, so %s.",
    plural(n_ids, "MATR ID"), plural(n_boxes, "variable"),
    if (n_ids < n_boxes) {
      if (extra == 1) "the last variable gets none" else sprintf("the last %d variables get none", extra)
    } else {
      if (extra == 1) "its last ID is left over" else sprintf("its last %d IDs are left over", extra)
    }
  )
}

# ------ Keeping the tables consistent ------------------------------------------
# Add rows to a table. `new_rows` holds some of its columns; the others take
# their defaults (new_row_defaults), so a new row lands last unless its
# display_order says otherwise.
add_rows <- function(draft, table, new_rows) {
  rows <- new_row_defaults[[table]][rep(1, nrow(new_rows)), ]
  rows[names(new_rows)] <- new_rows
  rownames(rows) <- NULL
  draft[[table]] <- rbind(draft[[table]], rows)
  renumber(draft, table)
}

#' Number each parent's rows 1, 2, 3... keeping their order.
#'
#' A move parks a row at an in-between `display_order` (2.5 lands between 2 and 3), then
#' calls this.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param table `"components"`, `"variables"` or `"response_options"`.
#' @return The changed draft.
renumber <- function(draft, table) {
  rows <- draft[[table]]
  parent <- rows[[parent_columns[[table]]]]
  parent[is.na(parent)] <- "" # ave() would skip the top level's NA parent
  rows$display_order <- ave(
    rows$display_order, parent,
    FUN = function(positions) rank(positions, ties.method = "first")
  )
  draft[[table]] <- rows
  draft
}

# Variables go with their question, and choices with their variable.
drop_orphans <- function(draft) {
  draft$variables <- draft$variables[draft$variables$component_id %in% draft$components$id, ]
  draft$response_options <- draft$response_options[
    draft$response_options$variable_id %in% draft$variables$id,
  ]
  draft
}

# Questions that followed a question that's gone (or is no longer a question)
# lose the link. The checks ask for a new one where it's needed.
unlink_followers <- function(draft, ids) {
  followers <- draft$components$parent_question_id %in% ids
  draft$components$parent_question_id[followers] <- NA
  draft
}
