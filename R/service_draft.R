# ------ Extraction drafts ------------------------------------------------------
# The extraction pipeline returns a questionnaire as nested JSON: sections hold
# blocks, questions hold variables, and variables hold choices
# (response options). The Edit Extraction page edits three flat tables instead, so
# moving a block only changes its parent_id and display_order, the same way
# QUESTIONNAIRE_COMPONENT stores it:
#
#   components        one row per block, pointing at its section (parent_id)
#   variables         one row per variable, pointing at its question
#   response_options  one row per choice, pointing at its variable
#
# Rows get ids when a draft is loaded (c1, v1, o1...). Ids aren't saved.
#
# A question has two numbers. `question_number` is its place among the questions (1, 2, 3...), kept
# up to date as blocks move (draft_service_renumber_questions()); `printed_number` is the number
# printed beside it on the questionnaire ("1a4"), typed by staff or read off the PDF. The printed one
# is the number the wave asks it as; the place stands in for a question that has none
# (draft_service_question_number()).

# The column that says which row each row sits under. display_order counts
# 1, 2, 3... among the rows that share it.
parent_columns <- c(
  components = "parent_id",
  variables = "component_id",
  response_options = "variable_id"
)

id_prefixes <- c(components = "c", variables = "v", response_options = "o")

# Every column of each table, with what a new row holds until it's filled in.
new_row_defaults <- list(
  components = data.frame(
    id = NA_character_, parent_id = NA_character_, display_order = Inf,
    component_type = NA_character_, text = NA_character_,
    question_number = NA_character_, printed_number = NA_character_, self_contained = NA,
    condition = NA_character_, parent_question_id = NA_character_, checked = FALSE
  ),
  variables = data.frame(
    id = NA_character_, component_id = NA_character_, display_order = Inf,
    variable_type = "closed_ended", label = NA_character_, subject = NA_character_,
    answer_data_type = NA_character_, matr_id = NA_character_
  ),
  response_options = data.frame(
    id = NA_character_, variable_id = NA_character_, display_order = Inf,
    answer_option = NA_character_, code_value = NA_integer_,
    printed_label = NA_character_, skip_instruction = NA_character_
  )
)

#' Open a draft's questionnaire for review.
#'
#' It opens where staff left it (their saved copy) or, if they haven't saved one,
#' fresh from the reading job.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The extraction draft's id.
#' @return A draft (see `draft_service_from_json()`). An error if reading hasn't finished.
draft_service_load <- function(connection, id) {
  json <- draft_model_get(connection, id)
  if (is.null(json)) stop("This draft hasn't been read yet.", call. = FALSE)
  draft_service_from_json(json)
}

#' Save staff's reviewed copy of a draft.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The extraction draft's id.
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param staff_id The staff id of the person saving.
#' @param review_edits What the reviewer has changed in what the AI returned, so far (see
#'   `draft_service_review_edits()`), or `NULL` to leave it as it is.
#' @param expected_revision The revision the person was looking at, or `NULL` not to check.
#' @return The draft's revision after the save, invisibly. An error with a sentence if someone saved
#'   it after `expected_revision`.
draft_service_save <- function(connection, id, draft, staff_id, review_edits = NULL, expected_revision = NULL) {
  draft_model_save_reviewed(connection, id, draft_service_to_json(draft), staff_id, review_edits, expected_revision)
}

#' A draft with nothing in it.
#'
#' What a Manual extraction starts as: no blocks, no variables, no choices.
#'
#' @return A draft: `list(questionnaire, components, variables, response_options)`.
draft_service_empty <- function() {
  draft_service_from_json(list(questionnaire = list(), components = list()))
}

#' How far a draft's review has got.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @return A list: `blocks` and how many are `checked`, `boxes` (variables) and how
#'   many have a MATR ID (`matr_ids`).
draft_service_counts <- function(draft) {
  list(
    blocks = nrow(draft$components), checked = sum(draft$components$checked),
    boxes = nrow(draft$variables), matr_ids = sum(!is.na(draft$variables$matr_id))
  )
}

#' How much of a draft has been checked, as a whole percentage.
#'
#' The Edit Extraction page's "N% human-reviewed" and the Existing Draft cards both show it.
#'
#' @param counts From `draft_service_counts()`.
#' @return A whole number from 0 to 100, rounded down; 0 for a draft with no blocks.
draft_service_percent_checked <- function(counts) {
  if (counts$blocks > 0) as.integer(floor(100 * counts$checked / counts$blocks)) else 0L
}

# ------ JSON to tables ---------------------------------------------------------
#' Turn the questionnaire JSON into the three tables the Edit Extraction page edits.
#'
#' Rows get ids when a draft is loaded (c1, v1, o1...); ids aren't saved. Positions are
#' numbered 1, 2, 3... within each parent, and choices without a code are coded. A choice marked as
#' not printed (`printed: false`, from drafts read before only printed choices were kept) is left out:
#' a respondent can't mark it, so it isn't a response option.
#'
#' @param json The questionnaire as nested lists: `questionnaire` and `components`.
#' @return A draft: `list(questionnaire, components, variables, response_options)`.
draft_service_from_json <- function(json) {
  blocks <- flatten_blocks(json$components)
  boxes <- flatten_children(blocks, "variables")
  choices <- Filter(function(choice) !isFALSE(choice[["printed"]]), flatten_children(boxes, "response_options"))
  question_numbers <- pluck(blocks, "question_number")
  block_ids <- ids_for("c", seq_along(blocks))

  draft <- list(
    questionnaire = json$questionnaire,
    components = data.frame(
      id = block_ids,
      parent_id = ids_for("c", pluck(blocks, ".parent", NA_integer_)),
      display_order = as.numeric(seq_along(blocks)),
      component_type = pluck(blocks, "component_type"),
      text = pluck(blocks, "text"),
      question_number = question_numbers,
      printed_number = printed_numbers(blocks),
      self_contained = pluck(blocks, "self_contained", NA),
      condition = pluck(blocks, "condition"),
      parent_question_id = find_parent_questions(
        question_numbers, pluck(blocks, "parent_question"), block_ids
      ),
      checked = pluck(blocks, "checked", FALSE)
    ),
    variables = data.frame(
      id = ids_for("v", seq_along(boxes)),
      component_id = ids_for("c", pluck(boxes, ".parent", NA_integer_)),
      display_order = as.numeric(seq_along(boxes)),
      variable_type = pluck(boxes, "variable_type"),
      label = pluck(boxes, "label"),
      subject = pluck(boxes, "subject"),
      answer_data_type = pluck(boxes, "answer_data_type"),
      matr_id = pluck(boxes, "matr_id")
    ),
    response_options = data.frame(
      id = ids_for("o", seq_along(choices)),
      variable_id = ids_for("v", pluck(choices, ".parent", NA_integer_)),
      display_order = as.numeric(seq_along(choices)),
      answer_option = pluck(choices, "answer_option"),
      code_value = pluck(choices, "code_value", NA_integer_),
      printed_label = pluck(choices, "printed_label"),
      skip_instruction = pluck(choices, "skip_instruction")
    )
  )
  # Rows were numbered in printed order; make that 1, 2, 3... within each parent.
  for (table in names(parent_columns)) draft <- renumber(draft, table)
  default_codes(draft)
}

#' Every block in the order a respondent meets it.
#'
#' @param blocks A list of blocks, each possibly holding `children`.
#' @param parent The position of the section the blocks sit in; `NA` for the top level.
#' @param flat The blocks found so far (used while it recurses).
#' @return A list of blocks, each noting the position of its section as `.parent`.
flatten_blocks <- function(blocks, parent = NA_integer_, flat = list()) {
  for (block in blocks) {
    flat[[length(flat) + 1]] <- c(block, list(.parent = parent))
    flat <- flatten_blocks(block$children, parent = length(flat), flat = flat)
  }
  flat
}

# The children listed in `field` of every row, each noting its row's position (.parent).
flatten_children <- function(rows, field) {
  children <- lapply(seq_along(rows), function(i) {
    lapply(rows[[i]][[field]], function(child) c(child, list(.parent = i)))
  })
  unlist(children, recursive = FALSE)
}

#' One field from every row.
#'
#' @param rows A list of rows (lists).
#' @param field The field's name.
#' @param missing What fills in for a row without the field; its type is the result's type.
#' @return A vector with one value for each row.
pluck <- function(rows, field, missing = NA_character_) {
  vapply(
    rows,
    function(row) as.vector(row[[field]] %||% missing, typeof(missing)),
    missing
  )
}

# The number printed beside each question. A block straight from the reading job (it has no `checked`,
# which a saved copy always has) was given the printed number as its `question_number`; a saved copy
# keeps it as `printed_number`.
printed_numbers <- function(blocks) {
  vapply(blocks, function(block) {
    if (!identical(block$component_type, "Question")) {
      return(NA_character_)
    }
    number <- block$printed_number %||% if (is.null(block$checked)) block$question_number
    as.character(number %||% NA_character_)
  }, NA_character_)
}

ids_for <- function(prefix, positions) {
  ids <- paste0(prefix, positions, recycle0 = TRUE) # no positions, no ids
  ids[is.na(positions)] <- NA
  ids
}

# The JSON names the question a follow-up hangs off by its printed number. The
# draft keeps that question's id instead, so renumbering never breaks the link.
# A number means the nearest question before it with that number, or failing
# that the first one after it (which the checks then point out).
find_parent_questions <- function(numbers, parent_numbers, ids) {
  vapply(seq_along(parent_numbers), function(i) {
    matches <- setdiff(which(same_number(numbers, parent_numbers[i])), i)
    earlier <- matches[matches < i]
    ids[if (length(earlier)) max(earlier) else matches[1]]
  }, "")
}

same_number <- function(numbers, number) {
  !is.na(numbers) & !is.na(number) &
    tolower(trimws(numbers)) == tolower(trimws(number))
}

# ------ Tables to JSON ---------------------------------------------------------
# The same shape the pipeline returns, plus what review adds: `checked` on each
# block, `matr_id` on each variable and `code_value` on each choice. Fields
# with nothing in them are left out, as in tests/testthat/fixtures/gna_gold.json.
draft_service_to_json <- function(draft) {
  list(
    questionnaire = draft$questionnaire,
    components = blocks_json(draft, parent_id = NA)
  )
}

blocks_json <- function(draft, parent_id) {
  map_rows(draft_service_children(draft, "components", parent_id), function(block) {
    json <- list(
      component_type = block$component_type,
      display_order = as.integer(block$display_order),
      text = block$text
    )
    if (block$component_type == "Section") {
      json$children <- blocks_json(draft, block$id)
    }
    if (block$component_type == "Question") {
      json$question_number <- block$question_number
      json$printed_number <- block$printed_number
      json$self_contained <- block$self_contained
      json$condition <- block$condition
      json$parent_question <- question_number_of(draft, block$parent_question_id)
      json$variables <- boxes_json(draft, block$id)
    }
    json$checked <- block$checked
    without_blanks(json)
  })
}

boxes_json <- function(draft, component_id) {
  map_rows(draft_service_children(draft, "variables", component_id), function(box) {
    json <- list(
      variable_type = box$variable_type,
      display_order = as.integer(box$display_order),
      label = box$label,
      subject = box$subject,
      matr_id = box$matr_id
    )
    if (box$variable_type == "closed_ended") {
      json$response_options <- choices_json(draft, box$id)
    } else {
      json$answer_data_type <- box$answer_data_type
    }
    without_blanks(json)
  })
}

choices_json <- function(draft, variable_id) {
  map_rows(draft_service_children(draft, "response_options", variable_id), function(choice) {
    without_blanks(list(
      display_order = as.integer(choice$display_order),
      answer_option = choice$answer_option,
      code_value = choice$code_value,
      printed_label = choice$printed_label,
      skip_instruction = choice$skip_instruction
    ))
  })
}

#' A JSON object without its empty fields.
#'
#' A missing field reads back as empty, so nothing is lost.
#'
#' @param fields A named list.
#' @return `fields` without the ones that are `NULL` or a single `NA`.
without_blanks <- function(fields) {
  blank <- vapply(fields, function(value) {
    is.null(value) || (is.atomic(value) && length(value) == 1 && is.na(value))
  }, logical(1))
  fields[!blank]
}

# ------ Finding things in a draft ----------------------------------------------
#' The rows of one table that sit under a parent, in order.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param table `"components"`, `"variables"` or `"response_options"`.
#' @param parent_id The parent's id; `NA` for the top level (components only).
#' @return The rows of `draft[[table]]` under `parent_id`, by `display_order`.
draft_service_children <- function(draft, table, parent_id) {
  rows <- draft[[table]]
  rows <- rows[rows[[parent_columns[[table]]]] %in% parent_id, , drop = FALSE]
  rows[order(rows$display_order), , drop = FALSE]
}

#' Block ids in the order a respondent meets them.
#'
#' Each block, then what is inside it.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param parent_id Where to start; `NA` for the whole questionnaire.
#' @return A character vector of component ids.
draft_service_printed_order <- function(draft, parent_id = NA) {
  ids <- draft_service_children(draft, "components", parent_id)$id
  as.character(unlist(lapply(ids, function(id) {
    c(id, draft_service_printed_order(draft, id))
  })))
}

#' Variable ids in the order a respondent meets them.
#'
#' Question by question in printed order, and each question's variables in their own order.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @return A character vector of variable ids.
draft_service_boxes_in_order <- function(draft) {
  boxes <- draft$variables
  question_position <- match(boxes$component_id, draft_service_printed_order(draft))
  boxes$id[order(question_position, boxes$display_order)]
}

#' The sections a block sits inside.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param id A component id.
#' @return The ids of its sections, innermost first; empty at the top level.
draft_service_ancestors <- function(draft, id) {
  parent_id <- draft$components$parent_id[match(id, draft$components$id)]
  if (length(parent_id) == 0 || is.na(parent_id)) {
    return(character())
  }
  c(parent_id, draft_service_ancestors(draft, parent_id))
}

#' The id the next new rows of a table get.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param table `"components"`, `"variables"` or `"response_options"`.
#' @param n How many ids; none gives none.
#' @return A character vector, e.g. `"c132"` after `"c131"`.
draft_service_next_id <- function(draft, table, n = 1) {
  used <- as.integer(substring(draft[[table]]$id, 2))
  # recycle0: without it, no numbers would still paste one id (the prefix alone)
  paste0(id_prefixes[[table]], max(c(0L, used)) + seq_len(n), recycle0 = TRUE)
}

#' A question's place among the questions (1, 2, 3...), which links a follow-up to its parent in the JSON.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param id Component ids.
#' @return Their question numbers, `NA` for a block with none.
question_number_of <- function(draft, id) {
  draft$components$question_number[match(id, draft$components$id)]
}

#' The number questions are asked as: the one printed beside them, or, where none was typed, their
#' place among the questions.
#'
#' What the wave records (`wave_question.question_number`), and what the preview and the messages call
#' a question.
#'
#' @param blocks Rows of a draft's `components`.
#' @return A character vector, one number for each row; `NA` for a block that isn't a question.
draft_service_question_number <- function(blocks) {
  printed <- trimws(blocks$printed_number %||% rep(NA_character_, nrow(blocks)))
  ifelse(!is.na(printed) & nzchar(printed), printed, blocks$question_number)
}

#' What a question is called in sentences: "question 1a4".
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param id A component id.
#' @return A string; "an unnumbered question" for a question without a number.
draft_service_question_name <- function(draft, id) {
  number <- draft_service_question_number(draft$components[match(id, draft$components$id), , drop = FALSE])
  if (length(number) == 0 || is.na(number)) "an unnumbered question" else paste("question", number)
}

#' Apply a function to each row of a data frame.
#'
#' @param rows A data frame.
#' @param fn A function taking a one-row data frame.
#' @param ... More arguments for `fn`.
#' @return A list with one result for each row.
map_rows <- function(rows, fn, ...) {
  lapply(seq_len(nrow(rows)), function(i) fn(rows[i, , drop = FALSE], ...))
}
