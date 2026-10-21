# ------ A questionnaire, laid out to read -----------------------------------------------------
# A draft's questionnaire drawn as a table to read, with nothing to edit, the way a codebook lists it:
# one row for each variable, with the number its question is asked as (No.), its MATR ID (Variable ID),
# the words of its question with the variable's label and who it is about in square brackets (Question
# Text), and its codes and choices, or a write-in's kind of answer (Variable Coding). A section heads the
# rows printed under it, which are indented under it, and a gold line closes it where it ends; blocks
# that aren't questions are rows across the whole table, in printed order, so they start just under
# their section's heading rather than in the Question Text column. A change request shows this before
# it is submitted and while it waits for a reviewer to approve it.

#' A draft's questionnaire, laid out to read as a table.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @return A UI element.
questionnaire_preview_ui <- function(draft) {
  if (nrow(draft$components) == 0) {
    return(p(class = "form-text", "This questionnaire has no blocks."))
  }
  div(
    class = "questionnaire-preview",
    tags$table(
      class = "table table-sm questionnaire-table",
      tags$thead(tags$tr(
        tags$th(scope = "col", class = "qt-number", "No."),
        tags$th(scope = "col", class = "qt-id", "Variable ID"),
        tags$th(scope = "col", class = "qt-text", "Question Text"),
        tags$th(scope = "col", class = "qt-coding", "Variable Coding")
      )),
      tags$tbody(preview_rows(draft, NA, depth = 0L))
    )
  )
}

# How many columns the table has, for the rows that go across all of them.
preview_columns <- 4

# The rows for the blocks inside a section (or at the top level, for `parent_id = NA`), in printed order.
preview_rows <- function(draft, parent_id, depth) {
  rows <- map_rows(draft_service_children(draft, "components", parent_id), preview_block_rows, draft, depth)
  do.call(tagList, rows)
}

preview_block_rows <- function(block, draft, depth) {
  switch(block$component_type,
    Section = tagList(
      tags$tr(
        class = "qt-section", `data-depth` = depth,
        tags$th(
          colspan = preview_columns, scope = "rowgroup",
          type_chip(block$component_type), " ", if (is.na(block$text)) span(class = "preview-note", "(no heading)") else block$text
        )
      ),
      preview_rows(draft, block$id, depth + 1L),
      # where the section ends: a gold line under the last row inside it
      tags$tr(
        class = "qt-section-end", `data-depth` = depth, `aria-hidden` = "true",
        tags$td(colspan = preview_columns, span(class = "qt-section-end-label", paste("End of", preview_section_name(block))))
      )
    ),
    Question = preview_question_rows(block, draft, depth),
    tags$tr(
      class = "qt-block", `data-depth` = depth,
      tags$td(
        colspan = preview_columns, class = "qt-text",
        type_chip(block$component_type), " ", span(class = "preview-text", preview_words(block))
      )
    )
  )
}

# "PART B. YOUR HABITS", or "the section" for one without a heading.
preview_section_name <- function(block) {
  if (is.na(block$text)) "the section" else shorten(block$text, 60)
}

preview_words <- function(block) {
  if (is.na(block$text)) tags$em("(no words)") else block$text
}

# One row for each of a question's variables; a question with none still has its row. Its number is on
# its first row.
preview_question_rows <- function(block, draft, depth) {
  words <- preview_question_words(block)
  number <- draft_service_question_number(block)
  number_cell <- function(first) {
    tags$td(class = "qt-number", if (first) span(class = "preview-number", if (is.na(number)) "?" else number))
  }
  boxes <- draft_service_children(draft, "variables", block$id)
  if (nrow(boxes) == 0) {
    return(tags$tr(
      class = "qt-question", `data-depth` = depth,
      number_cell(TRUE),
      tags$td(class = "qt-id", tags$em("no variables")),
      tags$td(class = "qt-text", span(class = "preview-text", words)),
      tags$td(class = "qt-coding")
    ))
  }
  map_rows(boxes, function(box) {
    tags$tr(
      class = "qt-question", `data-depth` = depth,
      number_cell(box$display_order == 1),
      tags$td(class = "qt-id", if (is.na(box$matr_id)) tags$em("no MATR ID") else span(class = "preview-matr-id", box$matr_id)),
      tags$td(class = "qt-text", span(class = "preview-text", words), preview_variable_tags(box)),
      tags$td(class = "qt-coding", preview_coding(box, draft))
    )
  })
}

# The question's words, without its number at the start if they were read with it ("3. Have you ...").
preview_question_words <- function(block) {
  if (is.na(block$text)) {
    return(tags$em("(no words)"))
  }
  number <- draft_service_question_number(block)
  if (is.na(number) || !nzchar(number)) {
    return(block$text)
  }
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", number)
  trimmed <- sub(paste0("^\\s*(Q(uestion)?\\s*)?", escaped, "\\s*[.):-]\\s+"), "", block$text, ignore.case = TRUE)
  if (nzchar(trimmed)) trimmed else block$text
}

# " [Lifetime] [Self]": the variable's label and who it is about, each when it has one.
preview_variable_tags <- function(box) {
  tags <- c(box$label, box$subject)
  tags <- trimws(tags[!is.na(tags)])
  tags <- tags[nzchar(tags)]
  if (length(tags) > 0) span(class = "qt-tags", paste0(" ", paste0("[", tags, "]", collapse = " ")))
}

# Each choice on its own line with its code ("1 = Yes"), or a write-in's kind of answer.
preview_coding <- function(box, draft) {
  if (box$variable_type != "closed_ended") {
    return(span(
      class = "qt-write-in",
      paste("Write-in:", if (is.na(box$answer_data_type)) "answer type not picked" else box$answer_data_type)
    ))
  }
  choices <- draft_service_children(draft, "response_options", box$id)
  if (nrow(choices) == 0) {
    return(tags$em("no choices"))
  }
  lines <- lapply(seq_len(nrow(choices)), function(i) {
    choice <- choices[i, , drop = FALSE]
    div(
      class = "qt-choice",
      span(class = "qt-code", if (is.na(choice$code_value)) "?" else choice$code_value),
      " = ",
      if (is.na(choice$answer_option)) tags$em("(blank)") else choice$answer_option
    )
  })
  do.call(tagList, lines)
}

# ------ A change request, laid out to read --------------------------------------------------------
# What an extraction's change request comes to: on Change Requests, its Basic Info (the registry, study,
# questionnaire and wave it goes into, filled in there) and what it adds (a card for each kind of row,
# saying how many rows are new, how many are already in the database and are used again, and which only
# link the two); and, on both pages, the questionnaire itself. The Edit Extraction page shows the
# questionnaire before the request is submitted, and Change Requests the whole of it while it waits for
# a reviewer.

#' What an extraction's change request comes to, laid out to read.
#'
#' @param preview A list: `what` (a list of cards, from `extraction_service_request_preview()`),
#'   `summary` (a sentence), `problem` (why it can't be added, or `NULL`) and `unchecked` (how many
#'   blocks haven't been marked as checked). Any can be left out.
#' @param draft The questionnaire, or `NULL` if it couldn't be read.
#' @param pdf_url The address of its PDF, or `NULL`.
#' @param basic_info The Basic Info form, to show first, or `NULL`.
#' @return A UI element.
extraction_request_ui <- function(preview, draft, pdf_url = NULL, basic_info = NULL) {
  div(
    class = "request-preview",
    if (!is.null(basic_info)) {
      div(class = "request-section", h3("Basic Info"), basic_info)
    },
    if (!is.null(preview$problem)) {
      edit_callout("danger", title = "This can't be added as it is", preview$problem)
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
      if (!is.null(pdf_url)) div(class = "change-request-questionnaire-head", pdf_open_button(pdf_url)),
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
