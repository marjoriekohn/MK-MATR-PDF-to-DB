# ------ Checking a draft -------------------------------------------------------
# Business rules the database will enforce once the draft is loaded, checked
# now so staff can fix problems while reviewing. Each problem names the block
# to fix (id) and says what to do next (message), in printed order.

#' Check a draft against the rules a questionnaire has to meet.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param matr_ids Whether a variable without a MATR ID is a problem. It is for staff reviewing a
#'   draft; not for the reading job, since Gemini never reads MATR IDs.
#' @return A data frame of `id` (the block to fix) and `message` (what to do), in
#'   printed order; no rows if all is well.
draft_service_check <- function(draft, lookups, matr_ids = TRUE) {
  problems <- rbind(
    block_problems(draft, lookups),
    question_problems(draft),
    variable_problems(draft, lookups, matr_ids)
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

#' Problems with blocks themselves: their kind, and words a block must have.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @return A data frame of `id` and `message`.
block_problems <- function(draft, lookups) {
  blocks <- draft$components
  rbind(
    problems_where(
      blocks$id, !blocks$component_type %in% lookups$component_types,
      "Pick a kind of block from the list."
    ),
    problems_where(
      blocks$id, is.na(blocks$text) & blocks$component_type != "Section",
      "Add the printed words. Only a section can be blank." # BR42
    )
  )
}

question_problems <- function(draft) {
  blocks <- draft$components
  questions <- blocks[blocks$component_type == "Question", ]
  printed <- draft_service_printed_order(draft)
  has_parent <- !is.na(questions$parent_question_id)
  parent_type <- blocks$component_type[match(questions$parent_question_id, blocks$id)]
  parent_is_earlier_question <- parent_type %in% "Question" &
    match(questions$parent_question_id, printed) < match(questions$id, printed)
  rbind(
    problems_where(questions$id, is.na(questions$question_number), "Add the question number."),
    problems_where(
      questions$id, duplicated_within(tolower(questions$question_number), "wave"),
      "Give it a number no other question has." # BR71
    ),
    problems_where(
      questions$id, !questions$id %in% draft$variables$component_id,
      "Add at least one variable." # BR87
    ),
    problems_where(
      questions$id, questions$self_contained %in% FALSE & !has_parent,
      "Pick the question it follows, since it can't be understood on its own." # BR64
    ),
    problems_where(
      questions$id, has_parent & !parent_is_earlier_question,
      "Pick the question it follows again. It has to be a question printed before it." # BR62, BR63
    )
  )
}

variable_problems <- function(draft, lookups, matr_ids = TRUE) {
  boxes <- draft$variables
  choices <- draft$response_options
  n_choices <- tabulate(match(choices$variable_id, boxes$id), nbins = nrow(boxes))
  is_choice_box <- boxes$variable_type == "closed_ended"
  box_problem <- function(condition, message) {
    problems_where(
      boxes$component_id, condition,
      sprintf("Variable %d: %s", as.integer(boxes$display_order), message)
    )
  }
  boxes_where <- function(choice_condition) {
    boxes$id %in% choices$variable_id[choice_condition]
  }
  rbind(
    box_problem(is_choice_box & n_choices < 2, "add at least two choices."), # BR99
    box_problem(boxes_where(is.na(choices$answer_option)), "fill in the blank choice."),
    box_problem(
      boxes_where(duplicated_within(choices$answer_option, choices$variable_id)),
      "list each choice once."
    ),
    box_problem(
      boxes_where(duplicated_within(choices$code_value, choices$variable_id)),
      "give each choice its own code." # BR103
    ),
    box_problem(!is_choice_box & is.na(boxes$answer_data_type), "pick the kind of answer it takes."), # BR95
    box_problem(!boxes$subject %in% lookups$subjects, "pick who it's about from the list."),
    if (matr_ids) box_problem(is.na(boxes$matr_id), "fill in its MATR ID."), # every variable needs one before it can be pushed
    box_problem(
      duplicated_within(boxes$matr_id, "questionnaire"),
      "give it a MATR ID no other variable has." # BR93
    )
  )
}

# One problem for each id where `condition` holds.
problems_where <- function(ids, condition, message) {
  keep <- condition %in% TRUE
  data.frame(id = ids[keep], message = rep_len(message, length(ids))[keep])
}

# TRUE for each value that appears more than once in its group. NA never does.
duplicated_within <- function(values, groups) {
  key <- paste(groups, values, sep = "\r")
  !is.na(values) & (duplicated(key) | duplicated(key, fromLast = TRUE))
}
