# ------ Extraction drafts ------------------------------------------------------
# The extraction pipeline returns a questionnaire as nested JSON: sections and question groups hold
# blocks, questions hold variables, and variables hold choices (response options). The Edit
# Extraction page edits three flat tables instead, so moving a block only changes its parent_id and
# display_order, the same way QUESTIONNAIRE_COMPONENT stores it:
#
#   components        one row per block, pointing at its section or question group (parent_id)
#   variables         one row per variable, pointing at its question
#   response_options  one row per choice, pointing at its variable
#
# Rows get ids when a draft is loaded (c1, v1, o1...), in printed order. Ids aren't saved, but each
# row's `ref` is: the id the row had when the draft was loaded from what the AI returned. It stays
# with the row however the draft is edited, so the reviewed copy can be compared with the AI's answer
# row by row (service_draft_record.R). A row staff added has none.
#
# Two kinds of JSON are loaded. What the reading job returns (the AI's answer, `ai_original`) has no
# `schema_version`: its questions have the number printed beside them (`printed_number`) and name
# their parent by its printed number. The reviewed copy the page saves has `schema_version`, and
# names a question's parent by its place among the questions (`question_number`), which, unlike a
# printed number, never repeats.
#
# A question has two numbers. `question_number` is its place among the questions (1, 2, 3...), worked
# out by the site and kept up to date as blocks move (draft_service_renumber_questions()); it is the
# number the wave records (`wave_question.question_number`). `printed_number` is the number printed
# beside it ("1a", "12"), read off the PDF and corrected by staff; it can repeat (each section can
# start again at 1) or be missing, and the wave keeps it too (`wave_question.printed_number`).
# Sections and question groups aren't counted.
#
# Each block also notes the PDF page it was read from (`page`: the first page of the range the reading
# job sent with it), so the page can open the PDF at it. Blocks staff add take the page
# of the block they're added next to.

# The version of the reviewed copy's JSON. A copy without one is what the reading job returned.
draft_schema_version <- 2L

# The kinds of block that hold others. A section has a printed heading; a question group is only a
# container (a battery, a definition with its question, a question with its follow-up) and has no words
# of its own. A section sits at the top level or inside another section, never inside a question group.
container_types <- c("Section", "Question Group")

#' Whether blocks of these kinds hold other blocks.
#'
#' @param component_types Kinds of block.
#' @return A logical vector.
is_container <- function(component_types) component_types %in% container_types

# Who a variable is about when it is about the respondent: the subject the database keeps for them, and the
# name it had before (docs/sql/5_respondent.sql renames it). Staff and the AI leave About blank for the
# respondent; the blank is saved to the database as this subject.
respondent_subject <- "Respondent"
respondent_names <- c("Respondent", "Self")

#' Who a variable is about, with the respondent as a blank.
#'
#' @param subjects Subjects as typed or read; "Respondent" or "Self" in any capitals, or a blank, is the
#'   respondent.
#' @return The subjects, trimmed, with `NA` for the respondent.
blank_respondent <- function(subjects) {
  subjects <- trimws(as.character(subjects))
  subjects[!is.na(subjects) & (!nzchar(subjects) | tolower(subjects) %in% tolower(respondent_names))] <- NA
  subjects
}

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
    condition = NA_character_, parent_question_id = NA_character_, checked = FALSE, ref = NA_character_,
    parent_from_skip = FALSE, page = NA_integer_
  ),
  variables = data.frame(
    id = NA_character_, component_id = NA_character_, display_order = Inf,
    variable_type = "closed_ended", label = NA_character_, subject = NA_character_,
    answer_data_type = NA_character_, matr_id = NA_character_, ref = NA_character_,
    skip_condition = NA_character_, skip_instruction = NA_character_, skip_to_id = NA_character_
  ),
  response_options = data.frame(
    id = NA_character_, variable_id = NA_character_, display_order = Inf,
    answer_option = NA_character_, code_value = NA_integer_,
    printed_label = NA_character_, skip_instruction = NA_character_, ref = NA_character_,
    skip_to_id = NA_character_
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
#' @param review_edits How this copy differs from what the AI returned (see
#'   `draft_service_review_edits()`), which replaces what was kept, or `NULL` to leave it as it is.
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
#' The Edit Extraction page's "N% human-reviewed" and the Extraction Drafts cards both show it.
#'
#' @param counts From `draft_service_counts()`.
#' @return A whole number from 0 to 100, rounded down; 0 for a draft with no blocks.
draft_service_percent_checked <- function(counts) {
  if (counts$blocks > 0) as.integer(floor(100 * counts$checked / counts$blocks)) else 0L
}

# ------ JSON to tables ---------------------------------------------------------
#' Turn the questionnaire JSON into the three tables the Edit Extraction page edits.
#'
#' Rows get ids when a draft is loaded (c1, v1, o1...); ids aren't saved, but `ref` is (see the top of
#' this file). Positions are numbered 1, 2, 3... within each parent, the questions by their place among
#' the questions, and choices without a code are coded. A variable about the respondent has a blank
#' subject, however the JSON says it. Skip instructions are linked to where they send respondents, and the
#' questions they skip past follow their question (service_draft_skips.R).
#'
#' @param json The questionnaire as nested lists: `questionnaire` and `components`, and `schema_version`
#'   for a reviewed copy (without it, it is what the reading job returned).
#' @return A draft: `list(questionnaire, components, variables, response_options)`.
draft_service_from_json <- function(json) {
  reviewed <- !is.null(json$schema_version)
  blocks <- flatten_blocks(json$components)
  boxes <- flatten_children(blocks, "variables")
  choices <- Filter(function(choice) !isFALSE(choice[["printed"]]), flatten_children(boxes, "response_options"))
  block_ids <- ids_for("c", seq_along(blocks))
  # The AI's answer names a parent by its printed number; the reviewed copy by its place.
  parent_by <- if (reviewed) pluck(blocks, "question_number") else pluck(blocks, "printed_number")
  # What the AI returned is the starting point that every later copy is compared with: its rows' refs
  # are their ids.
  refs <- function(rows, ids) if (reviewed) pluck(rows, "ref") else ids

  draft <- list(
    questionnaire = json$questionnaire,
    components = data.frame(
      id = block_ids,
      parent_id = ids_for("c", pluck(blocks, ".parent", NA_integer_)),
      display_order = as.numeric(seq_along(blocks)),
      component_type = pluck(blocks, "component_type"),
      text = pluck(blocks, "text"),
      question_number = rep(NA_character_, length(blocks)),
      printed_number = pluck(blocks, "printed_number"),
      self_contained = pluck(blocks, "self_contained", NA),
      condition = pluck(blocks, "condition"),
      parent_question_id = find_parent_questions(parent_by, pluck(blocks, "parent_question"), block_ids),
      checked = pluck(blocks, "checked", FALSE),
      ref = refs(blocks, block_ids),
      parent_from_skip = pluck(blocks, "parent_from_skip", FALSE),
      page = pluck(blocks, "page", NA_integer_)
    ),
    variables = data.frame(
      id = ids_for("v", seq_along(boxes)),
      component_id = ids_for("c", pluck(boxes, ".parent", NA_integer_)),
      display_order = as.numeric(seq_along(boxes)),
      variable_type = pluck(boxes, "variable_type"),
      label = pluck(boxes, "label"),
      subject = blank_respondent(pluck(boxes, "subject")),
      answer_data_type = pluck(boxes, "answer_data_type"),
      matr_id = pluck(boxes, "matr_id"),
      ref = refs(boxes, ids_for("v", seq_along(boxes))),
      # a write-in's skip: the answer that skips, the words printed for it, and where it sends respondents
      skip_condition = pluck(boxes, "skip_condition"),
      skip_instruction = pluck(boxes, "skip_instruction"),
      skip_to_id = skip_ids(boxes)
    ),
    response_options = data.frame(
      id = ids_for("o", seq_along(choices)),
      variable_id = ids_for("v", pluck(choices, ".parent", NA_integer_)),
      display_order = as.numeric(seq_along(choices)),
      answer_option = pluck(choices, "answer_option"),
      code_value = pluck(choices, "code_value", NA_integer_),
      printed_label = pluck(choices, "printed_label"),
      skip_instruction = pluck(choices, "skip_instruction"),
      ref = refs(choices, ids_for("o", seq_along(choices))),
      # a reviewed copy names where a choice skips to by the block's place in printed order, which is its id
      skip_to_id = skip_ids(choices)
    )
  )
  # Rows were numbered in printed order; make that 1, 2, 3... within each parent.
  for (table in names(parent_columns)) draft <- renumber(draft, table)
  draft <- draft_service_renumber_questions(default_codes(draft))
  # what the AI returned names where a choice skips to only in words, which are linked now
  if (!reviewed) draft <- draft_service_link_skips(draft)
  draft_service_skip_parents(draft)
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

# Where rows of a reviewed copy skip to: a block by its place in printed order (its id), or the end of the
# questionnaire (`skip_to_end`).
skip_ids <- function(rows) {
  ids <- ids_for("c", pluck(rows, "skip_to", NA_integer_))
  ids[pluck(rows, "skip_to_end", FALSE)] <- skip_to_end_id
  ids
}

ids_for <- function(prefix, positions) {
  ids <- paste0(prefix, positions, recycle0 = TRUE) # no positions, no ids
  ids[is.na(positions)] <- NA
  ids
}

# The JSON names the question a follow-up hangs off by its number (printed, in the AI's answer, which
# can repeat from section to section; its place, in a reviewed copy). The draft keeps that question's
# id instead, so renumbering never breaks the link. A number means the nearest question before it with
# that number, or failing that the first one after it (which the checks then point out).
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
# The reviewed copy: the same shape the pipeline returns, plus `schema_version`, and what review adds:
# `question_number` (its place) and `checked` on each block, `matr_id` on each variable, `code_value` on
# each choice, `ref` on each row the AI returned, where a choice or write-in skips to (`skip_to`: the
# block's place in printed order, or `skip_to_end`) and whether a question's parent was worked out from a skip. A question names its parent
# by the parent's place among the questions.
# Fields with nothing in them are left out, as in tests/testthat/fixtures/gna_gold.json.
draft_service_to_json <- function(draft) {
  attr(draft, "printed") <- draft_service_printed_order(draft)
  list(
    schema_version = draft_schema_version,
    questionnaire = draft$questionnaire,
    components = blocks_json(draft, parent_id = NA)
  )
}

blocks_json <- function(draft, parent_id) {
  map_rows(draft_service_children(draft, "components", parent_id), function(block) {
    json <- list(
      component_type = block$component_type,
      display_order = as.integer(block$display_order),
      text = block$text,
      page = block$page
    )
    if (is_container(block$component_type)) {
      json$children <- blocks_json(draft, block$id)
    }
    if (block$component_type == "Question") {
      json$question_number <- block$question_number
      json$printed_number <- block$printed_number
      json$self_contained <- block$self_contained
      json$condition <- block$condition
      json$parent_question <- question_number_of(draft, block$parent_question_id)
      if (isTRUE(block$parent_from_skip)) json$parent_from_skip <- TRUE
      json$variables <- boxes_json(draft, block$id)
    }
    json$checked <- block$checked
    json$ref <- block$ref
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
      json$skip_condition <- box$skip_condition
      json$skip_instruction <- box$skip_instruction
      json <- c(json, skip_json(box$skip_to_id, attr(draft, "printed")))
    }
    json$ref <- box$ref
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
      skip_instruction = choice$skip_instruction,
      skip_to = skip_json(choice$skip_to_id, attr(draft, "printed"))$skip_to,
      skip_to_end = skip_json(choice$skip_to_id, attr(draft, "printed"))$skip_to_end,
      ref = choice$ref
    ))
  })
}

# Where a row skips to, in the reviewed copy: `skip_to` (a block's place in printed order) or `skip_to_end`.
skip_json <- function(skip_to_id, printed) {
  if (identical(skip_to_id, skip_to_end_id)) list(skip_to = NA, skip_to_end = TRUE) else list(skip_to = match(skip_to_id, printed), skip_to_end = NA)
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

#' The number a question is called by: the one printed beside it, or, where none is printed, its
#' place among the questions.
#'
#' What the outline, the preview and the messages call a question. (The wave records both numbers:
#' see the top of this file.)
#'
#' @param blocks Rows of a draft's `components`.
#' @return A character vector, one number for each row; `NA` for a block that isn't a question.
draft_service_question_number <- function(blocks) {
  printed <- trimws(blocks$printed_number %||% rep(NA_character_, nrow(blocks)))
  ifelse(!is.na(printed) & nzchar(printed), printed, blocks$question_number)
}

# What goes between a question's number and its words where a list names it: "7 · Have you smoked…".
outline_number_separator <- " · "

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
