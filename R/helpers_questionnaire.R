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

# One line: the MATR ID and label, then the choices with their codes (or a write-in's kind of answer).
preview_variable <- function(box, draft) {
  is_choice_box <- box$variable_type == "closed_ended"
  tags$li(
    class = "preview-variable",
    span(class = "preview-matr-id", if (is.na(box$matr_id)) tags$em("no MATR ID") else box$matr_id),
    span(class = "preview-variable-meta", if (!is.na(box$label)) box$label),
    if (is_choice_box) {
      preview_choices(box, draft)
    } else {
      span(class = "preview-choices", paste("Answer:", if (is.na(box$answer_data_type)) "not picked" else tolower(box$answer_data_type)))
    }
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

# ------ A change request, laid out to read --------------------------------------------------------
# What an extraction's change request comes to: where it goes (the registry, study, questionnaire and
# wave, each as a card, recorded or NEW), what it adds (a card for each kind of row, saying how many
# rows are new, how many are already in the database and are used again, and which only link the two),
# and the questionnaire itself. The Edit Extraction page shows it before the request is submitted, and
# Change Requests while it waits for a reviewer.

#' What an extraction's change request comes to, laid out to read.
#'
#' @param preview From `extraction_service_request_preview()`: `where` and `what` (lists of cards),
#'   `summary` (a sentence), `problem` (why it can't be added, or `NULL`) and `unchecked` (how many
#'   blocks haven't been marked as checked).
#' @param draft The questionnaire, or `NULL` if it couldn't be read.
#' @param pdf_url The address of its PDF, or `NULL`.
#' @return A UI element.
extraction_request_ui <- function(preview, draft, pdf_url = NULL) {
  div(
    class = "request-preview",
    if (!is.null(preview$problem)) {
      edit_callout("danger", title = "This can't be added as it is", preview$problem)
    },
    if (length(preview$where) > 0) {
      div(class = "request-section", h3("Where it goes"), div(class = "request-cards", lapply(preview$where, request_card_ui)))
    },
    if (length(preview$what) > 0) {
      div(
        class = "request-section",
        h3("What it adds"),
        if (nzchar(preview$summary %||% "")) p(class = "form-text", preview$summary),
        div(class = "request-cards", lapply(preview$what, request_card_ui))
      )
    },
    if (isTRUE(preview$unchecked > 0)) {
      p(class = "form-text", sprintf("%s haven't been marked as checked yet.", plural(preview$unchecked, "block")))
    },
    div(
      class = "request-section",
      div(class = "change-request-questionnaire-head", pdf_open_button(pdf_url)),
      if (is.null(draft)) {
        p(class = "form-text", "The questionnaire couldn't be read. Edit opens it on the Edit Extraction page.")
      } else {
        div(class = "change-request-questionnaire", questionnaire_preview_ui(draft))
      }
    )
  )
}

#' One card of a change request: a row that is there, or a NEW one, the way Make an edit shows a row.
#'
#' @param card A list: `kind` (e.g. `"Study"`), `title`, `subtitle`, `facts` (short notes) and `is_new`.
#' @return A UI element.
request_card_ui <- function(card) {
  div(
    class = c("edit-card", "request-card", if (isTRUE(card$is_new)) "is-new", if (isTRUE(card$is_missing)) "is-missing"),
    if (isTRUE(card$is_new)) new_badge(),
    div(class = "edit-card-head", span(class = "request-card-kind", card$kind)),
    h3(class = "edit-card-title", card$title),
    if (nzchar(card$subtitle %||% "")) p(class = "edit-card-subtitle", card$subtitle),
    if (length(card$facts) > 0) div(class = "edit-card-facts", lapply(card$facts, function(fact) row_badge(fact)))
  )
}

#' The mark on a card for a row the request adds: a certificate with NEW on it.
#'
#' @return A UI element.
new_badge <- function() {
  span(
    class = "new-badge", role = "img", `aria-label` = "New: this request adds it",
    icon("certificate", class = "fa-solid"),
    span(class = "new-badge-text", `aria-hidden` = "true", "NEW")
  )
}
