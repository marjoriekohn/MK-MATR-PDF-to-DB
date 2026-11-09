# ------ Checking a draft -------------------------------------------------------
# Business rules the database will enforce once the draft is loaded, checked
# now so staff can fix problems while editing. Each problem names the block
# to fix (id) and says what to do next (message), in printed order, and, where
# one field is at fault, which (table, row, field), so the page can outline it.

# Check a draft against the rules a questionnaire has to meet.
# matr_ids: Whether a variable without a MATR ID is a problem. It is for staff reviewing a
#   draft; not for the reading job, since Gemini never reads MATR IDs.
# Returns: A data frame of `id` (the block to fix), `message` (what to do), and `table`, `row` and
#   `field` (the field at fault: `NA` for a problem with the block as a whole, and `field` `NA` for
#   one with a whole variable), in printed order; no rows if all is well.
draft_service_check <- function(draft, lookups, matr_ids = TRUE) {
  problems <- rbind(
    block_problems(draft, lookups),
    question_problems(draft),
    variable_problems(draft, matr_ids)
  )
  position <- match(problems$id, draft_service_printed_order(draft))
  problems[order(position), , drop = FALSE]
}

# The next block with a problem.
# problems: The result of `draft_service_check()`.
# after_id: The block to look after; `NULL` starts at the top.
# Returns: The id of the first block with a problem after `after_id`, starting over at
#   the top.
draft_service_next_problem <- function(draft, problems, after_id) {
  ids <- unique(problems$id)
  printed <- draft_service_printed_order(draft)
  current <- match(after_id %||% NA, printed, nomatch = 0)
  later <- ids[match(ids, printed) > current]
  if (length(later) > 0) later[1] else ids[1]
}

# Problems with blocks themselves: their kind, the words a block must have (and a group mustn't),
# and where a section can be.
# Returns: A data frame of `id` and `message`.
block_problems <- function(draft, lookups) {
  blocks <- draft$components
  is_group <- blocks$component_type == "Group"
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
      "Add the section's printed heading. Blocks with no printed heading go in a Group instead."
    ),
    text_problem(is.na(blocks$text) & !is_container(blocks$component_type), "Add the printed words."),
    text_problem(
      !is.na(blocks$text) & is_group,
      "A group has no words of its own. Put them in an Instruction inside it."
    ),
    text_problem(
      is.na(blocks$text) & blocks$component_type == "Grid",
      "Add the lead-in its questions share. Questions that are whole without one go in a Group instead."
    ),
    problems_where(
      blocks$id, !can_hold(parent_type, blocks$component_type),
      vapply(parent_type, cant_hold_message, character(1))
    )
  )
}

# A question's number is worked out from its place, and the number printed beside it can repeat (each
# section can start again at 1) or be missing, so neither is checked. The question it depends on, if any,
# has to be a question printed before it.
question_problems <- function(draft) {
  blocks <- draft$components
  questions <- blocks[blocks$component_type == "Question", ]
  printed <- draft_service_printed_order(draft)
  has_parent <- !is.na(questions$parent_question_id)
  parent_type <- blocks$component_type[match(questions$parent_question_id, blocks$id)]
  parent_is_earlier_question <- parent_type %in% "Question" &
    match(questions$parent_question_id, printed) < match(questions$id, printed)
  rbind(
    problems_where(
      questions$id, !questions$id %in% draft$variables$component_id,
      "Add at least one variable."
    ),
    problems_where(
      questions$id, has_parent & !parent_is_earlier_question,
      "Pick the question it depends on again. It has to be a question printed before it.",
      table = "components", rows = questions$id, field = "parent_question_id"
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
  # A question with several variables is a grid or battery: each row is labelled with its own words, so it can be
  # found and told apart, but for a lone check box, whose one choice says what it records, and a write-in whose unit
  # does (feet and inches). Two rows can share words only when they are about different people ("Age" of twin a
  # and of twin b), times (lifetime and the past 30 days) or units. A roster (a table of blank slots for the
  # respondent's children or siblings) is the exception: its rows are alike in every way and have no labels, since
  # their order and their MATR IDs tell them apart (a researcher asks for the whole roster, not the third child).
  label <- trimws(boxes$label)
  label[!nzchar(label)] <- NA
  unit <- if (is.null(boxes$unit)) rep(NA_character_, nrow(boxes)) else boxes$unit
  time_frame <- if (is.null(boxes$time_frame)) rep(NA_character_, nrow(boxes)) else boxes$time_frame
  named <- !is.na(label) | (!is_choice_box & !is.na(unit))
  in_grid <- ave(seq_along(boxes$id), boxes$component_id, FUN = length) > 1
  told_apart <- paste(tolower(label), tolower(unit), tolower(normal_subject(boxes$subject)), tolower(time_frame), sep = "\r")
  row_key <- ifelse(!named, NA, told_apart)
  roster <- roster_rows(boxes, choices, told_apart, is.na(label))
  rbind(
    box_problem(is_choice_box & n_choices < 1, "add its choices."), # a lone check box has one
    box_problem(
      in_grid & !named & !(is_choice_box & n_choices == 1) & !roster,
      "label it with its row's words, so it can be told apart from the question's other variables.", "label"
    ),
    box_problem(duplicated_within(row_key, boxes$component_id) & !roster, "give it a label the question's other variables don't have.", "label"),
    choice_problem(is.na(choices$answer_option), "fill in the blank choice.", "answer_option"),
    choice_problem(duplicated_within(choices$answer_option, choices$variable_id), "list each choice once.", "answer_option"),
    choice_problem(
      duplicated_within(choices$code_value, choices$variable_id),
      "give each choice its own code.", "code_value"
    ),
    box_problem(!is_choice_box & is.na(boxes$answer_data_type), "pick the kind of answer it takes.", "answer_data_type"),
    # The subject, time frame and unit aren't checked: blank is the respondent, and a new tag is added when the
    # extraction is pushed
    if (matr_ids) box_problem(is.na(boxes$matr_id), "fill in its MATR ID.", "matr_id"), # every variable needs one before it is pushed
    box_problem(
      duplicated_within(boxes$matr_id, "questionnaire"),
      "give it a MATR ID no other variable has.", "matr_id"
    )
  )
}

# Which variables are rows of a roster: a question whose variables are all unlabelled and each alike to another
# of them in everything else (kind of answer, choices and their codes, unit, subject and time frame), such as
# "What is the child's sex?" with a yes/no variable for each of five children, or a weight column with pounds and
# ounces for each child. They are told apart by their order and MATR IDs, so none needs a label.
# told_apart: Each variable's label, unit, subject and time frame as one key.
# unlabelled: Whether each variable has no label.
# Returns: A logical vector, one per variable.
roster_rows <- function(boxes, choices, told_apart, unlabelled) {
  if (nrow(boxes) == 0) {
    return(logical())
  }
  coded <- vapply(boxes$id, function(id) {
    own <- choices[choices$variable_id %in% id, , drop = FALSE]
    paste(sort(paste(own$code_value, own$answer_option)), collapse = "\n")
  }, character(1))
  alike <- paste(told_apart, boxes$variable_type, boxes$answer_data_type, coded, sep = "\r")
  alike_count <- ave(seq_along(alike), boxes$component_id, alike, FUN = length)
  as.logical(ave(as.integer(alike_count > 1 & unlabelled), boxes$component_id, FUN = function(fits) all(fits == 1)))
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
