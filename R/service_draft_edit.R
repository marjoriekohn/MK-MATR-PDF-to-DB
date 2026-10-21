# ------ Editing a draft --------------------------------------------------------
# Each function takes a draft and returns the changed copy. When a change isn't
# allowed, stop() says what to do instead; message() notes what else changed.
# The Edit Extraction page shows both to staff.

# The fields staff can change by typing or picking. A value keeps its column's type.
editable_fields <- list(
  components = c(
    "component_type", "text", "printed_number", "self_contained",
    "condition", "parent_question_id", "checked"
  ),
  variables = c("variable_type", "label", "subject", "answer_data_type", "matr_id"),
  response_options = c("answer_option", "code_value", "printed_label", "skip_instruction")
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
    matr_id = set_matr_id(draft, row, value),
    parent_question_id = set_parent_question(draft, row, value),
    subject = {
      draft$variables$subject[row] <- blank_respondent(value)
      draft
    },
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

# A block's kind decides what else it holds: only sections and question groups hold blocks (BR53), a
# section is never inside a question group, a question group has no words of its own, and only a
# Question has a number and variables (BR45).
set_component_type <- function(draft, row, type) {
  blocks <- draft$components
  block <- blocks[row, ]
  if (is.na(type)) stop("Pick a kind of block.")
  inside <- blocks$component_type[blocks$parent_id %in% block$id]
  if (!is_container(type) && length(inside) > 0) {
    stop("Move the blocks out of it first, then change its kind.")
  }
  if (type == "Question Group" && !is.na(block$text)) {
    stop("A question group has no words of its own. Move these words into an Instruction first, then make it a question group.")
  }
  if (type == "Question Group" && "Section" %in% inside) {
    stop("A section can't be inside a question group. Move the sections out of it first.")
  }
  if (type == "Section" && !is.na(block$parent_id) && !blocks$component_type[match(block$parent_id, blocks$id)] %in% "Section") {
    stop("A section can't be inside a question group. Move it out of the group first.")
  }
  draft$components$component_type[row] <- type
  if (type == "Question") {
    if (is.na(block$self_contained)) draft$components$self_contained[row] <- TRUE
    return(draft)
  }
  draft$components[row, c("question_number", "printed_number", "condition", "parent_question_id")] <- NA
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

# The question a question follows, as picked from its list ("7 · Have you smoked…") or typed: by its
# number (printed, or its place where none is printed), the nearest question before it with that number,
# or failing that the first one after it (which the checks point out). A block's id is taken as it is.
set_parent_question <- function(draft, row, value) {
  blocks <- draft$components
  if (is.na(value) || value %in% blocks$id) {
    draft$components$parent_question_id[row] <- value
    return(draft)
  }
  number <- trimws(sub(paste0(outline_number_separator, ".*$"), "", value))
  printed <- draft_service_printed_order(draft)
  questions <- printed[blocks$component_type[match(printed, blocks$id)] %in% "Question"]
  questions <- setdiff(questions, blocks$id[row])
  numbers <- draft_service_question_number(blocks[match(questions, blocks$id), , drop = FALSE])
  same <- questions[same_number(numbers, number)]
  if (length(same) == 0) {
    stop(sprintf("No question is numbered %s. Pick the question it follows from the list.", number))
  }
  earlier <- same[match(same, printed) < match(blocks$id[row], printed)]
  draft$components$parent_question_id[row] <- if (length(earlier) > 0) earlier[length(earlier)] else same[1]
  draft
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
  add_blank_choices(draft, box$id, 1)
}

# A MATR ID names one variable of the questionnaire (BR93), so one another variable has is refused,
# whether it was typed or picked from the uploaded IDs.
set_matr_id <- function(draft, row, matr_id) {
  boxes <- draft$variables
  taken_by <- which(boxes$matr_id %in% matr_id & seq_len(nrow(boxes)) != row)
  if (!is.na(matr_id) && length(taken_by) > 0) {
    other <- boxes[taken_by[1], ]
    stop(sprintf(
      "%s is already the MATR ID of variable %d of %s. Give each variable its own.",
      matr_id, as.integer(other$display_order), draft_service_question_name(draft, other$component_id)
    ))
  }
  draft$variables$matr_id[row] <- matr_id
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
#' @param parent_ids The section or question group each block is in; `NA` for the top level.
#' @return The changed draft. An error if the outline was out of date, a block that isn't a section or
#'   question group would hold blocks, or a section would be inside a question group.
draft_service_apply_order <- function(draft, ids, parent_ids) {
  blocks <- draft$components
  if (length(ids) != nrow(blocks) || !setequal(ids, blocks$id)) {
    stop("The outline was out of date. Try that again.")
  }
  parent_types <- blocks$component_type[match(parent_ids, blocks$id)]
  if (any(!is.na(parent_ids) & !is_container(parent_types))) {
    stop("Only sections and question groups can hold other blocks.")
  }
  moved_types <- blocks$component_type[match(ids, blocks$id)]
  if (any(moved_types == "Section" & !is.na(parent_ids) & !parent_types %in% "Section")) {
    stop("A section can't go inside a question group.")
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
#' It goes right after the selected one, or inside it if that is a section or question group (unless
#' the new block is a section), or at the very end. A new section never lands inside a question group:
#' it goes after the group.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param component_type The kind of block.
#' @param id The new block's id.
#' @param near_id The selected block's id, or `NULL`.
#' @return The changed draft.
draft_service_add_block <- function(draft, component_type, id, near_id = NULL) {
  blocks <- draft$components
  near <- blocks[blocks$id %in% near_id, ]
  has_near <- nrow(near) == 1
  if (has_near && component_type == "Section") {
    # climb out of question groups, so the section lands at the top level or in a section
    while (!is.na(near$parent_id) && !blocks$component_type[match(near$parent_id, blocks$id)] %in% "Section") {
      near <- blocks[blocks$id == near$parent_id, ]
    }
  }
  goes_inside <- has_near && is_container(near$component_type) && component_type != "Section"
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
#' one question's variables are usually alike; a write-in has no choices to copy. A question's first
#' variable starts with one blank choice: a lone check box has only the one.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param component_id The question's id.
#' @return The changed draft.
draft_service_add_box <- function(draft, component_id) {
  boxes <- draft_service_children(draft, "variables", component_id)
  id <- draft_service_next_id(draft, "variables")
  if (nrow(boxes) == 0) {
    draft <- add_rows(draft, "variables", data.frame(id = id, component_id = component_id))
    return(add_blank_choices(draft, id, 1))
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
  if (nrow(choices) == 0) {
    return(draft)
  }
  choices$id <- draft_service_next_id(draft, "response_options", nrow(choices))
  choices$variable_id <- rep(id, nrow(choices))
  choices$ref <- NA_character_ # copies, which the AI didn't return
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
# The uploaded CSV lists one MATR ID per variable, in the order the variables appear. They are given
# to the variables either all at once, in that order (Auto Apply), or one by one, each variable's MATR
# ID picked from the uploaded IDs no other variable has yet, or typed (Manual Assignment). Either way an
# ID goes to one variable only (set_matr_id()), and an ID the variables don't use is still offered.

#' The uploaded MATR IDs as they can be given: each once, blanks left out.
#'
#' @param matr_ids The IDs, in order (`extraction_service_matr_ids()`).
#' @return A character vector, in the CSV's order.
draft_service_uploaded_matr_ids <- function(matr_ids) {
  matr_ids <- trimws(as.character(matr_ids))
  unique(matr_ids[!is.na(matr_ids) & nzchar(matr_ids)])
}

#' The uploaded MATR IDs no variable has yet.
#'
#' What each variable's MATR ID drop-down offers.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param matr_ids The IDs, in order (`extraction_service_matr_ids()`).
#' @return A character vector, in the CSV's order.
draft_service_available_matr_ids <- function(draft, matr_ids) {
  setdiff(draft_service_uploaded_matr_ids(matr_ids), draft$variables$matr_id)
}

#' Give the variables the uploaded MATR IDs in the order they appear (Auto Apply).
#'
#' The first variable without a MATR ID gets the first uploaded ID no variable has, and so on, so what
#' was typed or picked already is kept. With `replace`, every variable is given the ID in its place in
#' the list instead. If the counts differ, nothing is lost: the variables left over keep no MATR ID and
#' the IDs left over are still offered in the drop-downs, and staff are told with `message()`.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param matr_ids The IDs, in order (`extraction_service_matr_ids()`).
#' @param replace Whether to replace the MATR IDs the variables have.
#' @return The changed draft. An error if the list is empty.
draft_service_auto_apply_matr_ids <- function(draft, matr_ids, replace = FALSE) {
  ids <- draft_service_uploaded_matr_ids(matr_ids)
  if (length(ids) == 0) stop("The CSV has no MATR IDs in it. Check the file and add it again.")
  in_order <- match(draft_service_boxes_in_order(draft), draft$variables$id)
  if (replace) {
    draft$variables$matr_id <- NA_character_
  }
  targets <- in_order[is.na(draft$variables$matr_id[in_order])]
  pool <- setdiff(ids, draft$variables$matr_id)
  n <- min(length(targets), length(pool))
  draft$variables$matr_id[targets[seq_len(n)]] <- pool[seq_len(n)]
  repeated <- unique(matr_ids[duplicated(trimws(matr_ids)) & nzchar(trimws(matr_ids))])
  n_unfilled <- length(targets) - n
  n_left <- length(pool) - n
  message(paste(c(
    sprintf("Gave %s a MATR ID, in the order they appear.", plural(n, "variable")),
    if (n_unfilled > 0) sprintf("%s still %s none.", plural(n_unfilled, "variable"), if (n_unfilled == 1) "has" else "have"),
    if (n_left > 0) {
      sprintf(
        "%s left over, still offered in each MATR ID drop-down.",
        plural(n_left, "uploaded ID is", plural_form = "uploaded IDs are")
      )
    },
    if (length(repeated) > 0) sprintf("The CSV lists %s more than once; each is given once.", word_list(repeated))
  ), collapse = " "))
  draft
}

#' Fill the variables' MATR IDs from a recorded questionnaire's questions.
#'
#' A question with the same words as one the questionnaire asks (ignoring capitals and extra spaces) gets
#' its variables' MATR IDs, first variable to first variable and so on. Used for the questionnaire the
#' extraction goes into (its earlier waves), and for any questionnaire picked in the MATR IDs dialog. Only
#' blank MATR IDs are filled, so nothing typed or filled from a CSV is lost. Says how many were filled with
#' `message()`; a MATR ID already used by another variable of the draft isn't used again.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param recorded The recorded questions' MATR IDs: a data frame with `text`, `matr_id` and `display_order`
#'   (`extraction_service_recorded()$matr_ids`, `extraction_service_questionnaire_matr_ids()`).
#' @param from Where they come from, for the message: e.g. `"the questionnaire's earlier waves"`.
#' @return The changed draft; the same draft if nothing matched.
draft_service_fill_recorded_matr_ids <- function(draft, recorded, from = "the questionnaire's earlier waves") {
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
      "Filled in the MATR IDs of %s from %s, where the question is worded the same.",
      plural(n_filled, "variable"), from
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
#' A question's place follows its place in the questionnaire, so moving, adding or deleting a question
#' renumbers the ones after it. Blocks that aren't questions (sections and question groups included) have
#' no number and aren't counted. The number printed beside a question (`printed_number`) is left as it is.
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
