# ------ A questionnaire, laid out to read -----------------------------------------------------
# A draft's questionnaire drawn as it would be read, with nothing to edit: its sections with what
# they hold, each question numbered with its words, and under a question its variables (MATR ID and
# label, and a write-in's kind of answer), each with its choices and their codes beside it. A change
# request shows this before it is submitted and while it waits for a reviewer to approve it.

#' A draft's questionnaire, laid out to read.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @return A UI element.
questionnaire_preview_ui <- function(draft) {
  if (nrow(draft$components) == 0) {
    return(p(class = "form-text", "This questionnaire has no blocks."))
  }
  div(class = "questionnaire-preview", preview_blocks(draft, NA))
}

# The blocks inside a section (or at the top level, for `parent_id = NA`), in printed order.
preview_blocks <- function(draft, parent_id) {
  map_rows(draft_service_children(draft, "components", parent_id), preview_block, draft)
}

preview_block <- function(block, draft) {
  switch(block$component_type,
    Section = div(
      class = "preview-section",
      h3(class = "preview-section-title", if (is.na(block$text)) "Untitled section" else block$text),
      div(class = "preview-section-body", preview_blocks(draft, block$id))
    ),
    Question = div(
      class = "preview-question",
      div(
        class = "preview-question-head",
        span(class = "preview-number", paste0("Q", if (is.na(block$question_number)) "?" else block$question_number)),
        span(class = "preview-text", preview_words(block)),
        preview_question_note(block, draft)
      ),
      preview_variables(block, draft)
    ),
    div(
      class = "preview-block",
      type_chip(block$component_type),
      span(class = "preview-text", preview_words(block))
    )
  )
}

preview_words <- function(block) {
  if (is.na(block$text)) tags$em("(no words)") else block$text
}

# "Follows Q3" and the skip condition, when a question has them.
preview_question_note <- function(block, draft) {
  follows <- question_number_of(draft, block$parent_question_id)
  notes <- c(
    if (!is.na(follows)) paste0("Follows Q", follows),
    if (!is.na(block$condition)) block$condition
  )
  if (length(notes) > 0) span(class = "preview-note", paste(notes, collapse = " · "))
}

preview_variables <- function(block, draft) {
  boxes <- draft_service_children(draft, "variables", block$id)
  if (nrow(boxes) == 0) {
    return(p(class = "preview-note", "No variables."))
  }
  tags$ul(class = "preview-variables", map_rows(boxes, preview_variable, draft))
}

preview_variable <- function(box, draft) {
  is_choice_box <- box$variable_type == "closed_ended"
  tags$li(
    class = "preview-variable",
    span(class = "preview-matr-id", if (is.na(box$matr_id)) tags$em("no MATR ID") else box$matr_id),
    span(
      class = "preview-variable-meta",
      paste(c(
        if (!is.na(box$label)) box$label,
        if (!is.na(box$subject)) paste("about", box$subject),
        if (is_choice_box) "choices" else paste("write-in", if (!is.na(box$answer_data_type)) tolower(box$answer_data_type))
      ), collapse = " · ")
    ),
    if (is_choice_box) preview_choices(box, draft)
  )
}

# "Yes (1), No (2)": each choice with its code; a choice not printed on the form is marked.
preview_choices <- function(box, draft) {
  choices <- draft_service_children(draft, "response_options", box$id)
  if (nrow(choices) == 0) {
    return(span(class = "preview-choices", tags$em("no choices")))
  }
  words <- paste0(
    ifelse(is.na(choices$answer_option), "(blank)", choices$answer_option),
    ifelse(is.na(choices$code_value), "", paste0(" (", choices$code_value, ")")),
    ifelse(choices$printed %in% FALSE, " [not printed]", "")
  )
  span(class = "preview-choices", paste(words, collapse = ", "))
}
