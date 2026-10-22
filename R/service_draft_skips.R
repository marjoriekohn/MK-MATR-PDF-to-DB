# ------ Skip instructions, linked to where they send respondents ---------------------------------------
# A choice can carry routing printed beside it ("Go to Question 32"). The words are kept as printed
# (`skip_instruction`), and the choice also points at the block it sends respondents to (`skip_to_id`: a
# question, or a section with a heading), so the link survives renumbering and is saved to the database
# (response_option.skip_to_component_id).
#
# When a reading arrives, each skip instruction is linked by the number it names: the nearest question after
# the choice's own question printed with that number, or failing that any question with it, or, when none
# has it, the first of its parts ("Go to Question 8" over 8a, 8b...); "Go to Section B" links to the section
# whose heading starts that way. What can't be linked, the checks point out.
#
# Every question a choice skips past (printed after the choice's question and before where it sends
# respondents) follows that question: its parent question is filled in, marked as worked out from the skip
# (`parent_from_skip`). A parent staff picked, or one the AI read, is never replaced; one worked out from a
# skip follows the skip when it changes, and goes when no skip covers the question any more.

#' The question number a skip instruction names.
#'
#' @param text Skip instructions, as printed ("Go to Question 32", "Skip to Q. 5a", "Go to #12").
#' @return The numbers, as text (`NA` where none is named).
skip_number <- function(text) {
  named <- regmatches(text, regexec("(?i)\\b(?:question|q)\\s*\\.?\\s*#?\\s*([0-9]+[a-z0-9.#]*)", text, perl = TRUE))
  bare <- regmatches(text, regexec(
    "(?i)\\b(?:go|skip|continue|proceed|turn)\\b.*?\\b(?:to|with)\\s+#?\\s*([0-9]+[a-z0-9.#]*)", text,
    perl = TRUE
  ))
  vapply(seq_along(text), function(i) {
    found <- c(named[[i]][2], bare[[i]][2])
    found <- found[!is.na(found)]
    if (length(found) == 0) NA_character_ else sub("[.]+$", "", found[1])
  }, character(1))
}

# The section a skip instruction names: "Go to Section B" -> "section b". NA where it names none.
skip_section <- function(text) {
  named <- regmatches(text, regexec("(?i)\\b(section|part)\\s+([a-z0-9]+)\\b", text, perl = TRUE))
  vapply(named, function(found) if (length(found) == 3) tolower(paste(found[2], found[3])) else NA_character_, character(1))
}

#' What a skip instruction links to: a question by its number, or a section by its heading.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param choice_id The choice.
#' @param text What the choice's skip instruction says (its own by default).
#' @return A block's id, or `NA` if nothing in the draft fits.
draft_service_skip_target <- function(draft, choice_id, text = NULL) {
  choices <- draft$response_options
  row <- match(choice_id, choices$id)
  text <- text %||% choices$skip_instruction[row]
  if (is.na(row) || is.na(text)) {
    return(NA_character_)
  }
  blocks <- draft$components
  printed <- draft_service_printed_order(draft)
  source <- draft$variables$component_id[match(choices$variable_id[row], draft$variables$id)]
  after <- printed[seq_along(printed) > match(source, printed)]
  number <- skip_number(text)
  if (!is.na(number)) {
    questions <- printed[blocks$component_type[match(printed, blocks$id)] %in% "Question"]
    numbers <- draft_service_question_number(blocks[match(questions, blocks$id), , drop = FALSE])
    same <- questions[same_number(numbers, number)]
    # "Go to Question 8" over 8a, 8b...: the first of its parts
    if (length(same) == 0) same <- questions[!is.na(numbers) & is_part_of(numbers, number)]
    later <- intersect(after, same)
    return(if (length(later) > 0) later[1] else if (length(same) > 0) same[1] else NA_character_)
  }
  section <- skip_section(text)
  if (!is.na(section)) {
    headings <- printed[blocks$component_type[match(printed, blocks$id)] %in% "Section"]
    words <- tolower(trimws(blocks$text[match(headings, blocks$id)]))
    starts <- vapply(c(" ", ".", ":"), function(after) startsWith(words, paste0(section, after)), logical(length(words)))
    same <- headings[!is.na(words) & (words == section | rowSums(matrix(starts, nrow = length(words))) > 0)]
    later <- intersect(after, same)
    return(if (length(later) > 0) later[1] else if (length(same) > 0) same[1] else NA_character_)
  }
  NA_character_
}

# Whether printed numbers are parts of `number`: "8a" and "8#2" are parts of "8"; "80" isn't.
is_part_of <- function(numbers, number) {
  numbers <- tolower(trimws(numbers))
  number <- tolower(trimws(number))
  startsWith(numbers, number) & nchar(numbers) > nchar(number) & !grepl("^[0-9]", substring(numbers, nchar(number) + 1))
}

#' Link every skip instruction that isn't linked yet to where it sends respondents.
#'
#' @param draft A draft.
#' @return The changed draft.
draft_service_link_skips <- function(draft) {
  choices <- draft$response_options
  todo <- which(!is.na(choices$skip_instruction) & is.na(choices$skip_to_id))
  for (row in todo) draft$response_options$skip_to_id[row] <- draft_service_skip_target(draft, choices$id[row])
  draft
}

#' Fill in the parent question of every question a choice skips past.
#'
#' See the top of this file: a parent worked out from a skip follows the skips; any other is left alone.
#'
#' @param draft A draft.
#' @return The changed draft.
draft_service_skip_parents <- function(draft) {
  blocks <- draft$components
  printed <- draft_service_printed_order(draft)
  place <- match(blocks$id, printed)
  choices <- draft$response_options[!is.na(draft$response_options$skip_to_id), , drop = FALSE]
  sources <- draft$variables$component_id[match(choices$variable_id, draft$variables$id)]
  from <- match(sources, printed)
  to <- match(choices$skip_to_id, printed)
  # each question skipped past, and the question it is skipped from (the nearest, if several skip it)
  skipped_from <- rep(NA_character_, nrow(blocks))
  for (i in which(!is.na(from) & !is.na(to) & to > from + 1)) {
    covered <- blocks$component_type == "Question" & place > from[i] & place < to[i]
    nearer <- covered & (is.na(skipped_from) | match(skipped_from, printed) < from[i])
    skipped_from[nearer] <- sources[i]
  }
  derived <- blocks$parent_from_skip %in% TRUE
  # a parent worked out from a skip follows the skips; an empty one is filled in
  follows <- derived | (is.na(blocks$parent_question_id) & !is.na(skipped_from))
  blocks$parent_question_id[follows] <- skipped_from[follows]
  blocks$parent_from_skip <- follows & !is.na(skipped_from)
  draft$components <- blocks
  draft
}

#' Make a choice's skip link point at a block.
#'
#' What staff pick from the list ("32 · Have you smoked…", "Section · Part B…") or type: by its number, the
#' nearest question after the choice's question with that number, or any question with it; a section by its
#' heading. A block's id is taken as it is.
#'
#' @param draft A draft.
#' @param row The choice's row in `draft$response_options`.
#' @param value What was picked or typed; `NA` for none.
#' @return The changed draft. An error with a sentence if nothing fits.
set_skip_to <- function(draft, row, value) {
  if (is.na(value) || value %in% draft$components$id) {
    draft$response_options$skip_to_id[row] <- value
    return(draft)
  }
  choice <- draft$response_options$id[row]
  target <- if (startsWith(value, skip_section_prefix)) {
    heading_target(draft, substring(value, nchar(skip_section_prefix) + 1))
  } else if (!is.na(skip_section(value))) {
    draft_service_skip_target(draft, choice, value)
  } else {
    draft_service_skip_target(draft, choice, paste("Question", trimws(sub(paste0(outline_number_separator, ".*$"), "", value))))
  }
  if (is.na(target)) stop(sprintf("Nothing in the questionnaire is %s. Pick where it skips to from the list.", value))
  draft$response_options$skip_to_id[row] <- target
  draft
}

# A section picked from the list by its heading.
heading_target <- function(draft, heading) {
  blocks <- draft$components
  same <- blocks$id[blocks$component_type == "Section" & tolower(shorten(blocks$text, 50)) == tolower(trimws(heading))]
  if (length(same) > 0) same[1] else NA_character_
}

# What a section is called in the list of places a choice can skip to: "Section · Part B. Your habits".
skip_section_prefix <- "Section · "

#' The places a choice can skip to, as the list on the page names them.
#'
#' The questions ("32 · Have you smoked…", as the parent-question list names them), and the sections with a
#' heading, in printed order.
#'
#' @param draft A draft.
#' @return A data frame of `id` and `label`.
skip_targets <- function(draft) {
  printed <- draft_service_printed_order(draft)
  blocks <- draft$components[match(printed, draft$components$id), , drop = FALSE]
  questions <- (attr(draft, "outline_questions") %||% outline_questions(draft))$questions
  keep <- blocks$component_type == "Question" | (blocks$component_type == "Section" & !is.na(blocks$text))
  blocks <- blocks[keep, , drop = FALSE]
  data.frame(
    id = blocks$id,
    label = ifelse(
      blocks$component_type == "Question", questions$label[match(blocks$id, questions$id)],
      paste0(skip_section_prefix, shorten(blocks$text, 50))
    ),
    stringsAsFactors = FALSE
  )
}

# Skip links to blocks that are gone are emptied.
unlink_skips <- function(draft, ids) {
  draft$response_options$skip_to_id[draft$response_options$skip_to_id %in% ids] <- NA
  draft
}
