# ------ Checking a draft -------------------------------------------------------
# Business rules the database will enforce once the draft is loaded, checked
# now so staff can fix problems while editing. Each problem names the block
# to fix (id) and says what to do next (message), in printed order, and, where
# one field is at fault, which (table, row, field), so the page can outline it.

#' Check a draft against the rules a questionnaire has to meet.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param matr_ids Whether a variable without a MATR ID is a problem. It is for staff reviewing a
#'   draft; not for the reading job, since Gemini never reads MATR IDs.
#' @return A data frame of `id` (the block to fix), `message` (what to do), and `table`, `row` and
#'   `field` (the field at fault: `NA` for a problem with the block as a whole, and `field` `NA` for
#'   one with a whole variable), in printed order; no rows if all is well.
draft_service_check <- function(draft, lookups, matr_ids = TRUE) {
  problems <- rbind(
    block_problems(draft, lookups),
    question_problems(draft),
    variable_problems(draft, matr_ids)
  )
  position <- match(problems$id, draft_service_printed_order(draft))
  problems[order(position), , drop = FALSE]
}

#' The next block with a problem.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param problems The result of `draft_service_check()`.
#' @param after_id The block to look after; `NULL` starts at the top.
#' @return The id of the first block with a problem after `after_id`, starting over at
#'   the top.
draft_service_next_problem <- function(draft, problems, after_id) {
  ids <- unique(problems$id)
  printed <- draft_service_printed_order(draft)
  current <- match(after_id %||% NA, printed, nomatch = 0)
  later <- ids[match(ids, printed) > current]
  if (length(later) > 0) later[1] else ids[1]
}

#' Problems with blocks themselves: their kind, the words a block must have (and a question group mustn't),
#' and where a section can be.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @return A data frame of `id` and `message`.
block_problems <- function(draft, lookups) {
  blocks <- draft$components
  is_group <- blocks$component_type == "Question Group"
  parent_type <- blocks$component_type[match(blocks$parent_id, blocks$id)]
  text_problem <- function(condition, message) {
    problems_where(blocks$id, condition, message, table = "components", rows = blocks$id, field = "text")
  }
  rbind(
    problems_where(
      blocks$id, !blocks$component_type %in% lookups$component_types,
      "Pick a kind of block from the list."
    ),
    text_problem(
      is.na(blocks$text) & blocks$component_type == "Section",
      "Add the section's printed heading. A group with no heading is a question group." # BR42
    ),
    text_problem(is.na(blocks$text) & !is_container(blocks$component_type), "Add the printed words."), # BR42
    text_problem(
      !is.na(blocks$text) & is_group,
      "A question group has no words of its own. Put them in an Instruction inside it."
    ),
    problems_where(
      blocks$id, blocks$component_type == "Section" & parent_type %in% "Question Group",
      "A section can't be inside a question group. Move it out, or make it a question group." # BR53
    )
  )
}

# A question's number is worked out from its place, and the number printed beside it can repeat (each
# section can start again at 1) or be missing, so neither is checked.
question_problems <- function(draft) {
  blocks <- draft$components
  questions <- blocks[blocks$component_type == "Question", ]
  printed <- draft_service_printed_order(draft)
  has_parent <- !is.na(questions$parent_question_id)
  parent_type <- blocks$component_type[match(questions$parent_question_id, blocks$id)]
  parent_is_earlier_question <- parent_type %in% "Question" &
    match(questions$parent_question_id, printed) < match(questions$id, printed)
  parent_field <- function(condition, message) {
    problems_where(questions$id, condition, message, table = "components", rows = questions$id, field = "parent_question_id")
  }
  rbind(
    problems_where(
      questions$id, !questions$id %in% draft$variables$component_id,
      "Add at least one variable." # BR87
    ),
    parent_field(
      questions$self_contained %in% FALSE & !has_parent,
      "Pick the question it follows, since it can't be understood on its own." # BR64
    ),
    parent_field(
      has_parent & !parent_is_earlier_question,
      "Pick the question it follows again. It has to be a question printed before it." # BR62, BR63
    )
  )
}

variable_problems <- function(draft, matr_ids = TRUE) {
  boxes <- draft$variables
  choices <- draft$response_options
  n_choices <- tabulate(match(choices$variable_id, boxes$id), nbins = nrow(boxes))
  is_choice_box <- boxes$variable_type == "closed_ended"
  # A problem with a variable's own field (or, with no field, the whole variable)...
  box_problem <- function(condition, message, field = NA_character_) {
    problems_where(
      boxes$component_id, condition,
      sprintf("Variable %d: %s", as.integer(boxes$display_order), message),
      table = "variables", rows = boxes$id, field = field
    )
  }
  # ...and with one field of each choice where `choice_condition` holds.
  choice_problem <- function(choice_condition, message, field) {
    box_row <- match(choices$variable_id, boxes$id)
    problems_where(
      boxes$component_id[box_row], choice_condition,
      sprintf("Variable %d: %s", as.integer(boxes$display_order[box_row]), message),
      table = "response_options", rows = choices$id, field = field
    )
  }
  # where each choice skips to: a skip instruction that names a question or section has to be linked to it,
  # and a link has to point forward
  printed <- draft_service_printed_order(draft)
  source_place <- match(boxes$component_id[match(choices$variable_id, boxes$id)], printed)
  skips <- choices$skip_instruction
  names_a_place <- !is.na(skips) & (!is.na(skip_number(skips)) | !is.na(skip_section(skips)))
  said <- sprintf("choice “%s”", ifelse(is.na(choices$answer_option), "(blank)", choices$answer_option))
  # a write-in's skip, the same way, and it says which answer skips
  box_place <- match(boxes$component_id, printed)
  box_names_a_place <- !is.na(boxes$skip_instruction) & (!is.na(skip_number(boxes$skip_instruction)) | !is.na(skip_section(boxes$skip_instruction)))
  rbind(
    box_problem(!is_choice_box & box_names_a_place & is.na(boxes$skip_to_id), "pick where it skips to.", "skip_to_id"),
    box_problem(
      !is_choice_box & match(boxes$skip_to_id, printed) <= box_place,
      "it skips back to something printed before it. Pick where it skips to.", "skip_to_id"
    ),
    box_problem(!is_choice_box & !is.na(boxes$skip_to_id) & is.na(boxes$skip_condition), "say which answer skips.", "skip_condition"),
    choice_problem(names_a_place & is.na(choices$skip_to_id), paste0("pick where ", said, " skips to."), "skip_to_id"),
    choice_problem(
      !is.na(choices$skip_to_id) & match(choices$skip_to_id, printed) <= source_place,
      paste0(said, " skips back to something printed before it. Pick where it skips to."), "skip_to_id"
    ),
    box_problem(is_choice_box & n_choices < 1, "add its choices."), # BR99: a lone check box has one
    choice_problem(is.na(choices$answer_option), "fill in the blank choice.", "answer_option"),
    choice_problem(duplicated_within(choices$answer_option, choices$variable_id), "list each choice once.", "answer_option"),
    choice_problem(
      duplicated_within(choices$code_value, choices$variable_id),
      "give each choice its own code.", "code_value" # BR103
    ),
    box_problem(!is_choice_box & is.na(boxes$answer_data_type), "pick the kind of answer it takes.", "answer_data_type"), # BR95
    # About isn't checked: blank is the respondent, and a new subject is added when the extraction is applied
    if (matr_ids) box_problem(is.na(boxes$matr_id), "fill in its MATR ID.", "matr_id"), # every variable needs one before it is sent
    box_problem(
      duplicated_within(boxes$matr_id, "questionnaire"),
      "give it a MATR ID no other variable has.", "matr_id" # BR93
    )
  )
}

# One problem for each id where `condition` holds. `table`, `rows` and `field` say which field is at
# fault, if one is: `rows` holds each id's row in `table`.
problems_where <- function(ids, condition, message, table = NA_character_, rows = NA_character_, field = NA_character_) {
  keep <- condition %in% TRUE
  n <- length(ids)
  data.frame(
    id = ids[keep],
    message = rep_len(message, n)[keep],
    table = rep_len(table, n)[keep],
    row = rep_len(as.character(rows), n)[keep],
    field = rep_len(field, n)[keep]
  )
}

# TRUE for each value that appears more than once in its group. NA never does.
duplicated_within <- function(values, groups) {
  key <- paste(groups, values, sep = "\r")
  !is.na(values) & (duplicated(key) | duplicated(key, fromLast = TRUE))
}
