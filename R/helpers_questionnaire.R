# ------ A questionnaire, laid out to read -----------------------------------------------------
# A draft's questionnaire drawn as a table to read, with nothing to edit, the way a codebook lists it: one
# row for each variable, with its MATR ID (Variable ID), the words of its question with the variable's label
# and who it is about in square brackets (Question Text), and its codes and choices, or a write-in's kind of
# answer (Variable Coding). Blocks that aren't questions (an instruction, a note) have a row each, in printed order.
# The rows are grouped under the section (and question group) they are printed in. It is a DataTables table
# (the DT package): it can be searched, and downloaded as a CSV. A change request shows it before it is
# submitted and while it waits for a reviewer to approve it.

#' A draft's questionnaire, laid out to read as a table.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @return A UI element.
questionnaire_preview_ui <- function(draft) {
  if (nrow(draft$components) == 0) {
    return(p(class = "form-text", "This questionnaire has no blocks."))
  }
  rows <- questionnaire_preview_rows(draft)
  shown <- data.frame(
    Section = htmltools::htmlEscape(rows$section),
    `Variable ID` = ifelse(is.na(rows$matr_id), preview_blank(rows$missing), htmltools::htmlEscape(rows$matr_id)),
    `Question Text` = paste0(
      htmltools::htmlEscape(rows$text),
      ifelse(nzchar(rows$tags), paste0(' <span class="qt-tags">', htmltools::htmlEscape(rows$tags), "</span>"), "")
    ),
    `Variable Coding` = vapply(rows$coding, function(lines) paste(htmltools::htmlEscape(lines), collapse = "<br>"), character(1)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  div(
    class = "questionnaire-preview",
    DT::datatable(
      shown,
      class = "compact questionnaire-table", rownames = FALSE, escape = FALSE,
      extensions = c("Buttons", "RowGroup"),
      options = list(
        dom = "Bfrtip", ordering = FALSE, pageLength = 50, autoWidth = FALSE,
        # the rows stay in printed order, grouped under their section
        rowGroup = list(dataSrc = 0, emptyDataGroup = "Not in a section"),
        columnDefs = list(
          list(targets = 0, visible = FALSE),
          list(targets = 1, width = "9rem"), list(targets = 3, width = "32%")
        ),
        buttons = list(list(
          extend = "csv", text = "Download CSV", filename = preview_file_name(draft),
          # every column, the section too, with a choice on each line and no markup
          exportOptions = list(columns = 0:3, format = list(body = DT::JS(
            "function (data) { return String(data).replace(/<br\\s*\\/?>/g, '\\n').replace(/<[^>]+>/g, '')",
            ".replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&quot;/g, '\"').replace(/&#39;/g, \"'\").replace(/&amp;/g, '&'); }"
          )))
        )),
        language = list(search = "Search:", info = "_START_ to _END_ of _TOTAL_ rows", emptyTable = "No rows")
      )
    )
  )
}

# "no MATR ID" or "no variables", in italics.
preview_blank <- function(missing) ifelse(nzchar(missing), paste0("<em>", missing, "</em>"), "")

# The CSV's name: the questionnaire's abbreviation or name, or "questionnaire".
preview_file_name <- function(draft) {
  name <- draft$questionnaire$abbreviation %||% NA
  if (!has_text(name)) name <- draft$questionnaire$name %||% NA
  if (!has_text(name)) name <- "questionnaire"
  gsub("[^A-Za-z0-9_-]+", "_", trimws(name))
}

#' The rows of the questionnaire's table, in printed order.
#'
#' A question has a row for each of its variables (or one, saying it has none); any other block that isn't a
#' section or question group has one row. Each row names the sections and question groups it is in.
#'
#' @param draft A draft.
#' @return A data frame: `section` (e.g. "PART B. YOUR HABITS › Question group", or "" at the top level),
#'   `kind`, `matr_id` (NA when there is none), `missing` ("no MATR ID", "no variables" or ""), `text`,
#'   `tags` ("[Lifetime] [Siblings]", or "") and `coding` (a list of lines: "1 = Yes", or the write-in's kind of
#'   answer).
questionnaire_preview_rows <- function(draft) {
  rows <- list()
  add <- function(section, kind, matr_id = NA_character_, missing = "", text = "", tags = "", coding = character()) {
    rows[[length(rows) + 1]] <<- list(
      section = section, kind = kind, matr_id = matr_id, missing = missing, text = text, tags = tags, coding = list(coding)
    )
  }
  walk <- function(parent_id, path) {
    blocks <- draft_service_children(draft, "components", parent_id)
    for (i in seq_len(nrow(blocks))) {
      block <- blocks[i, , drop = FALSE]
      if (is_container(block$component_type)) {
        walk(block$id, c(path, preview_container_name(block)))
        next
      }
      section <- paste(path, collapse = " \u203a ")
      if (block$component_type != "Question") {
        add(section, block$component_type, text = preview_words(block))
        next
      }
      words <- preview_question_words(block)
      boxes <- draft_service_children(draft, "variables", block$id)
      if (nrow(boxes) == 0) add(section, "Question", missing = "no variables", text = words)
      for (j in seq_len(nrow(boxes))) {
        box <- boxes[j, , drop = FALSE]
        add(
          section, "Question",
          matr_id = box$matr_id, missing = if (is.na(box$matr_id)) "no MATR ID" else "",
          text = words, tags = preview_variable_tags(box), coding = preview_coding(box, draft)
        )
      }
    }
  }
  walk(NA, character())
  if (length(rows) == 0) {
    return(data.frame(section = character(), kind = character(), matr_id = character(), missing = character(), text = character(), tags = character(), coding = I(list())))
  }
  data.frame(
    section = vapply(rows, `[[`, "", "section"),
    kind = vapply(rows, `[[`, "", "kind"),
    matr_id = vapply(rows, function(row) as.character(row$matr_id), ""),
    missing = vapply(rows, `[[`, "", "missing"),
    text = vapply(rows, `[[`, "", "text"),
    tags = vapply(rows, `[[`, "", "tags"),
    coding = I(lapply(rows, function(row) row$coding[[1]])),
    stringsAsFactors = FALSE
  )
}

# A section by its heading ("(no heading)" for one without), a question group as "Question group".
preview_container_name <- function(block) {
  if (block$component_type == "Question Group") {
    return("Question group")
  }
  if (is.na(block$text)) "(no heading)" else block$text
}

preview_words <- function(block) {
  if (is.na(block$text)) "(no words)" else block$text
}

# The question's words, without its number at the start if they were read with it ("3. Have you ...").
preview_question_words <- function(block) {
  if (is.na(block$text)) {
    return("(no words)")
  }
  number <- draft_service_question_number(block)
  if (is.na(number) || !nzchar(number)) {
    return(block$text)
  }
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", number)
  trimmed <- sub(paste0("^\\s*(Q(uestion)?\\s*)?", escaped, "\\s*[.):-]\\s+"), "", block$text, ignore.case = TRUE)
  if (nzchar(trimmed)) trimmed else block$text
}

# "[Lifetime] [Siblings]": the variable's label and who it is about, each when it has one (a variable about
# the respondent has no subject), or "".
preview_variable_tags <- function(box) {
  tags <- c(box$label, box$subject)
  tags <- trimws(tags[!is.na(tags)])
  tags <- tags[nzchar(tags)]
  if (length(tags) > 0) paste0("[", tags, "]", collapse = " ") else ""
}

# Each choice with its code ("1 = Yes"), one line each, or a write-in's kind of answer.
preview_coding <- function(box, draft) {
  if (box$variable_type != "closed_ended") {
    return(paste("Write-in:", if (is.na(box$answer_data_type)) "answer type not picked" else box$answer_data_type))
  }
  choices <- draft_service_children(draft, "response_options", box$id)
  if (nrow(choices) == 0) {
    return("no choices")
  }
  paste0(
    ifelse(is.na(choices$code_value), "?", choices$code_value), " = ",
    ifelse(is.na(choices$answer_option), "(blank)", choices$answer_option)
  )
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
#' @param table Where the questionnaire's table goes, in place of drawing it here: a `uiOutput()` the page draws
#'   it in once the dialog is open (a DataTables table put in a dialog as it opens stays empty). `NULL` draws it.
#' @param questionnaire Whether to show the questionnaire (its table, its PDF and how much of it is checked).
#'   Change Requests' Basic Info dialog leaves it to the request's Preview.
#' @return A UI element.
extraction_request_ui <- function(preview, draft, pdf_url = NULL, basic_info = NULL, table = NULL, questionnaire = TRUE) {
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
    if (questionnaire && isTRUE(preview$unchecked > 0)) {
      p(class = "form-text", sprintf("%s haven't been marked as checked yet.", plural(preview$unchecked, "block")))
    },
    if (questionnaire) {
      div(
        class = "request-section",
        if (!is.null(pdf_url)) div(class = "change-request-questionnaire-head", pdf_open_button(pdf_url)),
        if (is.null(draft)) {
          p(class = "form-text", "The questionnaire couldn't be read. Edit opens it on the Edit Extraction page.")
        } else {
          div(class = "change-request-questionnaire", table %||% questionnaire_preview_ui(draft))
        }
      )
    }
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
    app_icon("new", class = "fa-solid"),
    span(class = "new-badge-text", `aria-hidden` = "true", "NEW")
  )
}
