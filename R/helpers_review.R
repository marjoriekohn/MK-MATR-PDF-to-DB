# ------ Page building blocks ------------------------------------------------------
# Pieces the Edit Extraction page's outline is made of, and the buttons every page
# uses. What the page script (www/js/review.js) sends back to Shiny is written into
# the HTML:
#   data-ns      a module's outer element: its namespace, so events reach its inputs
#   data-action  a click sends the element's data-* values to the module's `action`
#   data-field   a change sends {table, id, field, value} to the module's `edit`

#' The colored label for a kind of block.
#'
#' Used in outline rows and the details. Blocks that aren't sections or questions all
#' look like Instruction, at its width, so the longer names are shortened.
#'
#' @param component_type The kind of block.
#' @return A UI element.
type_chip <- function(component_type) {
  kind <- switch(component_type,
    Section = "section",
    Question = "question",
    "instruction"
  )
  label <- switch(component_type,
    Introduction = "Intro",
    `Interviewer Script` = "Script",
    component_type
  )
  span(
    class = paste0("type-chip is-", kind),
    title = if (label != component_type) component_type,
    label
  )
}

#' A small note at the end of a row, e.g. "5 items".
#'
#' @param ... The note.
#' @param class Extra classes, e.g. `"is-warning"`.
#' @return A UI element.
row_badge <- function(..., class = NULL) {
  span(class = c("row-badge", class), ...)
}

#' A button the page script reports to the module's `action` input.
#'
#' It sends its `data-*` values too, e.g. `review_button("Move up", "move", `data-id` = id)`.
#'
#' @param label The button's label.
#' @param action What to report as `action`.
#' @param ... More attributes, e.g. `data-id`.
#' @param class The button's classes.
#' @param disabled Whether it is disabled.
#' @return A UI element.
review_button <- function(label, action, ..., class = "btn btn-sm btn-outline-dark",
                          disabled = FALSE) {
  tags$button(
    type = "button", class = class, `data-action` = action,
    disabled = if (disabled) NA, ..., label
  )
}

#' A short name for a block in sentences.
#'
#' For example "Drop here: inside section 3, after Q4".
#'
#' @param block One row of a draft's `components`.
#' @return A string.
block_label <- function(block) {
  switch(block$component_type,
    Section = if (is.na(block$text)) "the untitled section" else paste("section", shorten(block$text, 40)),
    Question = if (is.na(block$question_number)) "the unnumbered question" else paste0("Q", block$question_number),
    paste0(tolower(block$component_type), " “", shorten(block$text, 30), "”")
  )
}

#' Cut text short.
#'
#' @param text A character vector; `NA` becomes empty.
#' @param max_characters The most characters to keep, counting the `…` that marks a cut.
#' @return A character vector.
shorten <- function(text, max_characters) {
  text[is.na(text)] <- ""
  too_long <- nchar(text) > max_characters
  text[too_long] <- paste0(substr(text[too_long], 1, max_characters - 1), "…")
  text
}

# ------ Fields bound to the draft ----------------------------------------------
# Each input edits one field of one row: `table` and `row` say which, `field`
# names the column. What a field holds is said in it while it is empty
# (`placeholder`, or a drop-down's empty choice); `label` names it for screen readers.
field_attributes <- function(table, row, field) {
  list(`data-table` = table, `data-id` = row$id, `data-field` = field)
}

# ------ Fields at fault -------------------------------------------------------------
# A field the checks found at fault (`draft_service_check()`'s table, row and field) is outlined in red,
# and says why to screen readers and when pointed at. The page script puts the same marks on as the
# checks run again, since the outline isn't redrawn as words are typed (review.js). A `field` of NA
# means the whole row, e.g. a variable with too few choices.
field_problems <- function(problems, table, id, field) {
  if (is.null(problems) || nrow(problems) == 0 || is.null(problems$table)) {
    return(character())
  }
  at <- problems$table %in% table & problems$row %in% id &
    (if (is.na(field)) is.na(problems$field) else problems$field %in% field)
  problems$message[at]
}

field_problem_class <- function(problems, table, id, field) {
  if (length(field_problems(problems, table, id, field)) > 0) "has-problem"
}

field_problem_attributes <- function(problems, table, id, field) {
  messages <- field_problems(problems, table, id, field)
  if (length(messages) == 0) {
    return(list())
  }
  list(`aria-invalid` = "true", title = paste(messages, collapse = "\n"))
}

#' A one-line text field of the Edit Extraction page.
#'
#' @param table The draft table the row is in.
#' @param row One row of it, as a one-row data frame.
#' @param field The column.
#' @param placeholder Text shown while it is empty.
#' @param list The id of a `suggestions()` list.
#' @param type The input type.
#' @param label Its name for screen readers, or `NULL`.
#' @param problems The checks' problems, from `draft_service_check()`, to outline it if it is at fault.
#' @return A UI element; its changes are reported through `data-field`.
field_text <- function(table, row, field, placeholder = NULL, list = NULL,
                       type = "text", label = NULL, problems = NULL) {
  tags$input(
    type = type, class = c("form-control form-control-sm", field_problem_class(problems, table, row$id, field)),
    value = blank_if_na(row[[field]]), placeholder = placeholder, list = list, `aria-label` = label,
    !!!field_problem_attributes(problems, table, row$id, field),
    !!!field_attributes(table, row, field)
  )
}

#' A drop-down of the Edit Extraction page.
#'
#' A value that isn't one of the choices still shows, marked, so nothing changes silently.
#'
#' @param choices The choices; a named vector shows the names.
#' @param blank The empty choice's text, offered when the field is optional or has no value yet: it says
#'   what the field holds.
#' @param optional Whether the field may be left empty.
#' @inheritParams field_text
#' @return A UI element.
field_select <- function(table, row, field, choices, blank = "Pick one…",
                         optional = FALSE, label = NULL, problems = NULL) {
  value <- row[[field]]
  if (is.null(names(choices))) names(choices) <- choices
  if (!is.na(value) && !value %in% choices) {
    choices <- c(choices, setNames(value, paste(value, "(not in the list)")))
  }
  options <- Map(function(label, choice) {
    tags$option(value = choice, selected = if (identical(choice, value)) NA, label)
  }, names(choices), unname(choices))
  tags$select(
    class = c("form-select form-select-sm", field_problem_class(problems, table, row$id, field)),
    `aria-label` = label,
    !!!field_problem_attributes(problems, table, row$id, field),
    !!!field_attributes(table, row, field),
    if (optional || is.na(value)) tags$option(value = "", blank),
    unname(options)
  )
}

#' Side-by-side buttons that pick one value, e.g. Choices / Write-in.
#'
#' A `toggle_buttons()`, small to suit the panel.
#'
#' @param choices Maps each label to the value it stores.
#' @param label Names the group for screen readers.
#' @inheritParams field_text
#' @return A UI element.
field_toggle <- function(table, row, field, choices, label) {
  toggle_buttons(
    paste(row$id, field, sep = "-"), choices, as.character(row[[field]]), label,
    attributes = field_attributes(table, row, field), small = TRUE
  )
}

#' A check box of the Edit Extraction page.
#'
#' @param label Names it for screen readers.
#' @inheritParams field_text
#' @return A UI element.
field_checkbox <- function(table, row, field, label) {
  tags$input(
    type = "checkbox", class = "form-check-input", `aria-label` = label,
    checked = if (isTRUE(row[[field]])) NA,
    !!!field_attributes(table, row, field)
  )
}

#' Suggestions a text field offers as you type.
#'
#' The field still accepts new words.
#'
#' @param id The list's id, which the field names as its `list`.
#' @param values The suggestions.
#' @return A `datalist`.
suggestions <- function(id, values) {
  tags$datalist(id = id, lapply(values, function(value) tags$option(value = value)))
}

blank_if_na <- function(value) {
  if (is.na(value)) "" else as.character(value)
}
