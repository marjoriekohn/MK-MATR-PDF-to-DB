#' The outline: the questionnaire's blocks as a tree, each edited where it is.
#'
#' A block's words are typed straight into its row (with the words of other blocks of the same kind
#' offered as it is typed), and its row holds the buttons to mark it as checked and to delete it. A
#' question's row also opens a nested panel for what else a question has: the question it follows, its
#' skip condition, and its variables with their choices. Question numbers follow the questions' places in
#' the outline.
#'
#' @param id The module's id.
#' @return The page's UI.
review_outline_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "outline-panel",
    `data-ns` = ns(""),
    div(
      class = "outline-toolbar",
      uiOutput(ns("add_block_menu")),
      uiOutput(ns("to_fix"), inline = TRUE),
      review_button("Collapse all", "collapse_all"),
      review_button("Expand all", "expand_all")
    ),
    div(class = "outline-body", uiOutput(ns("tree")))
  )
}

#' The "+ Block" drop-down, one entry per kind of block.
#'
#' @param component_types The kinds of block.
#' @return A UI element.
add_block_menu <- function(component_types) {
  div(
    class = "dropdown",
    tags$button(
      type = "button", class = "btn btn-sm btn-outline-dark dropdown-toggle",
      `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      icon("plus"), " Block"
    ),
    tags$ul(
      class = "dropdown-menu",
      lapply(component_types, function(component_type) {
        tags$li(review_button(component_type, "add_block", `data-type` = component_type, class = "dropdown-item"))
      })
    )
  )
}

# ------ Outline tree -----------------------------------------------------------
# Each block is a row. A section's row sits above its own list, and dropping a
# block into that list puts the block inside the section. Only sections have a
# list, so only sections can hold blocks (BR53). `view` holds what's selected,
# which sections are collapsed, which questions have their panel open, the
# problems the checks found, and the words to suggest.

# SortableJS settings for every list in the outline: one shared group, so blocks
# move between sections, dragged by their dots. SortableJS draws the dragged
# block itself (forceFallback), since the browser's own drag image is
# see-through. The callbacks are in review.js.
outline_sortable_options <- sortable::sortable_options(
  group = "outline",
  handle = ".drag-handle",
  draggable = ".outline-node",
  forceFallback = TRUE,
  fallbackClass = "is-dragged",
  fallbackOnBody = TRUE,
  swapThreshold = 0.65,
  ghostClass = "is-drop-target",
  onMove = htmlwidgets::JS("reviewPage.showDropHint"),
  onEnd = htmlwidgets::JS("reviewPage.sendOutline")
)

# The id of the list of words offered for a kind of block, e.g. "review-outline-suggest-Section".
suggest_list_id <- function(ns, component_type) ns(paste0("suggest-", gsub("[^A-Za-z0-9]", "_", component_type)))

#' The outline as a tree.
#'
#' @param draft A draft.
#' @param view `list(selected, collapsed, opened, problems, suggestions)`: what is selected, which
#'   sections are closed, which questions have their panel open, the problems to mark, and the words to
#'   offer for each kind of block (`draft_service_text_suggestions()`).
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param ns The module's namespace function.
#' @return A UI element.
outline_tree <- function(draft, view, lookups, ns) {
  div(
    class = "outline-tree",
    outline_list(draft, section = NULL, view, lookups, ns),
    # The words offered as staff type, one list per kind of block, plus the answer choices and conditions
    lapply(names(view$suggestions %||% list()), function(component_type) {
      suggestions(suggest_list_id(ns, component_type), view$suggestions[[component_type]])
    }),
    suggestions(ns("answer-options"), lookups$answer_options),
    suggestions(ns("conditions"), unique(c("If yes", "If no", stats::na.omit(draft$components$condition))))
  )
}

outline_list <- function(draft, section, view, lookups, ns) {
  blocks <- draft_service_children(draft, "components", section$id %||% NA)
  list_id <- ns(paste0("list-", section$id %||% "top"))
  tagList(
    div(
      id = list_id, class = "outline-list",
      `data-label` = if (!is.null(section)) block_label(section),
      map_rows(blocks, outline_node, draft, view, lookups, ns)
    ),
    sortable::sortable_js(list_id, outline_sortable_options)
  )
}

outline_node <- function(block, draft, view, lookups, ns) {
  is_section <- block$component_type == "Section"
  is_question <- block$component_type == "Question"
  div(
    class = c(
      "outline-node", if (block$id %in% view$collapsed) "is-collapsed",
      if (is_question && block$id %in% view$opened) "is-open"
    ),
    `data-id` = block$id,
    `data-label` = block_label(block),
    outline_row(block, draft, view, ns),
    if (is_section) outline_list(draft, block, view, lookups, ns),
    if (is_question && block$id %in% view$opened) question_panel(block, draft, view, lookups, ns)
  )
}

outline_row <- function(block, draft, view, ns) {
  div(
    class = c("outline-row", if (identical(block$id, view$selected)) "is-selected"),
    tabindex = "0", `data-action` = "select", `data-id` = block$id,
    span(class = "drag-handle", title = "Drag to move", icon("grip-vertical")),
    outline_order(block),
    switch(block$component_type,
      Section = review_button(
        icon("caret-down"), "toggle",
        `data-id` = block$id, class = "btn-toggle", title = "Show or hide what's inside"
      ),
      Question = review_button(
        icon("caret-right"), "details",
        `data-id` = block$id, class = "btn-toggle is-details",
        title = "Show or hide the question's variables, the question it follows and its skip condition"
      ),
      span(class = "btn-toggle")
    ),
    type_chip(block$component_type),
    if (!is.na(block$question_number)) span(class = "outline-number", paste0("Q", block$question_number)),
    outline_text_field(block, ns),
    outline_badges(block, draft, view$problems),
    outline_actions(block, draft)
  )
}

# Its display order: 1, 2, 3... within its section (or the top level), kept up
# to date as blocks move, like the numbers on variables (BR55).
outline_order <- function(block) {
  where <- if (is.na(block$parent_id)) "at the top level" else "in its section"
  span(
    class = "outline-order", title = paste("Display order: block", block$display_order, where),
    as.integer(block$display_order)
  )
}

# The block's words, typed where they are. The words of the other blocks of the same kind, and of the
# recorded questionnaire's, are offered as they are typed (review.js reads the list `data-suggest` names).
outline_text_field <- function(block, ns) {
  placeholder <- switch(block$component_type,
    Section = "Section heading, as printed (e.g. ‘Section 1: About You’). A section can be left blank.",
    Question = "The question's words only. Introductions, instructions and definitions go in their own block.",
    paste("The", tolower(block$component_type), "as printed")
  )
  tags$textarea(
    class = "form-control form-control-sm outline-text",
    rows = 1, placeholder = placeholder, `aria-label` = paste(block$component_type, "text"),
    `data-autofocus` = if (is.na(block$text)) NA,
    `data-suggest` = suggest_list_id(ns, block$component_type),
    !!!field_attributes("components", block, "text"),
    blank_if_na(block$text)
  )
}

# What is said about the block at the end of its row. The problems badge is kept up to date by the page
# script as the checks run, so its count is drawn as the script expects (data-problems).
outline_badges <- function(block, draft, problems) {
  n_inside <- sum(draft$components$parent_id %in% block$id)
  n_boxes <- sum(draft$variables$component_id == block$id)
  follows <- question_number_of(draft, block$parent_question_id)
  block_problems <- problems$message[problems$id == block$id]
  span(
    class = "outline-badges",
    if (block$component_type == "Section") row_badge(paste(n_inside, if (n_inside == 1) "item" else "items")),
    if (!is.na(follows)) row_badge(paste0("Follows Q", follows)),
    if (block$component_type == "Question") row_badge(paste(n_boxes, if (n_boxes == 1) "variable" else "variables")),
    problem_badge(block_problems)
  )
}

#' The "N to fix" badge of a row, drawn even when there is nothing to fix (then hidden), so the page script
#' can show it without redrawing the row.
#'
#' @param messages The block's problems, as sentences.
#' @return A UI element.
problem_badge <- function(messages) {
  n <- length(messages)
  row_badge(
    icon("triangle-exclamation"), span(class = "problem-count", paste(n, "to fix")),
    class = c("is-warning", "outline-problems", if (n == 0) "is-hidden"),
    title = paste(messages, collapse = "\n")
  )
}

# Mark as checked, and delete. A section counts as checked for the eye only once everything inside it is.
outline_actions <- function(block, draft) {
  inside <- draft_service_printed_order(draft, block$id)
  all_checked <- block$checked && all(draft$components$checked[draft$components$id %in% inside])
  span(
    class = "outline-actions",
    review_button(
      icon("user-check"), "check",
      `data-id` = block$id,
      class = c("btn btn-sm btn-icon outline-check", if (block$checked) "is-checked", if (all_checked) "is-all-checked"),
      title = if (block$checked) {
        "Checked. Click to mark as not checked."
      } else {
        "Mark as checked and go to the next block. If you change it afterwards, it needs checking again."
      }
    ),
    review_button(
      icon("trash-can"), "delete",
      `data-table` = "components", `data-id` = block$id,
      class = "btn btn-sm btn-icon", title = paste("Delete this", tolower(block$component_type))
    )
  )
}

# ------ A question's panel ---------------------------------------------------------------
# Under a question's row, when its arrow is open: the question it follows and its skip condition, its
# variables and their choices, and what the checks found.
question_panel <- function(block, draft, view, lookups, ns) {
  messages <- view$problems$message[view$problems$id == block$id]
  div(
    class = "question-panel",
    if (length(messages) > 0) div(class = "callout details-problems", tags$ul(lapply(messages, tags$li))),
    question_fields(block, draft, ns),
    variable_cards(block, draft, lookups, ns)
  )
}

# The question it follows and the printed condition, in one row. The condition shows once there's a parent
# question, or if it already has words (e.g. its parent was deleted), so they're never hidden.
question_fields <- function(block, draft, ns) {
  has_condition <- !is.na(block$parent_question_id) || !is.na(block$condition)
  div(
    class = "details-row",
    labelled(
      "Parent Question",
      hint = paste(
        "When it's asked in the context of another question, or is only shown",
        "when a skip condition is met."
      ),
      field_select(
        "components", block, "parent_question_id", parent_question_choices(block, draft),
        blank = "None", optional = TRUE
      )
    ),
    if (has_condition) {
      labelled(
        "Skip Condition",
        hint = "Optional, e.g. “If yes”",
        field_text("components", block, "condition", list = ns("conditions"))
      )
    }
  )
}

# Questions printed before this one, as "Q7 · Have you smoked…". The current
# pick stays listed even if a move put it after this question.
parent_question_choices <- function(block, draft) {
  printed <- draft_service_printed_order(draft)
  questions <- draft$components[match(printed, draft$components$id), ]
  questions <- questions[questions$component_type == "Question" & questions$id != block$id, ]
  is_earlier <- match(questions$id, printed) < match(block$id, printed)
  keep <- is_earlier | questions$id %in% block$parent_question_id
  labels <- paste0(
    "Q", ifelse(is.na(questions$question_number), "?", questions$question_number),
    " · ", shorten(questions$text, 50),
    ifelse(is_earlier, "", " (printed after it)")
  )
  setNames(questions$id[keep], labels[keep])
}

# ------ Variables and choices -----------------------------------------------
# Variables reorder within their question, choices within their variable.
list_sortable_options <- function(handle) {
  sortable::sortable_options(handle = handle, onEnd = htmlwidgets::JS("reviewPage.sendList"))
}

#' A question's variables and their choices.
#'
#' Each variable is one row: its kind (choices or write-in), its MATR ID, who it's about, its label,
#' and, for a write-in, the kind of answer; its choices are listed under the row.
#'
#' @param block One row of a draft's `components`: a question.
#' @param draft A draft.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param ns The module's namespace function.
#' @return A UI element.
variable_cards <- function(block, draft, lookups, ns) {
  boxes <- draft_service_children(draft, "variables", block$id)
  list_id <- ns(paste0("boxes-", block$id))
  div(
    class = "variable-cards",
    div(
      class = "variable-cards-header",
      h3("Variables"),
      review_button(tagList(icon("plus"), " Variable"), "add_box", `data-id` = block$id)
    ),
    div(
      id = list_id, class = "box-list", `data-table` = "variables",
      map_rows(boxes, variable_card, draft, lookups, ns)
    ),
    sortable::sortable_js(list_id, list_sortable_options(".box-handle"))
  )
}

variable_card <- function(box, draft, lookups, ns) {
  is_choice_box <- box$variable_type == "closed_ended"
  div(
    class = "variable-card", `data-id` = box$id,
    div(
      class = "variable-card-row",
      span(class = "box-handle", title = "Drag to reorder", icon("grip-vertical")),
      span(class = "variable-card-number", as.integer(box$display_order)),
      field_toggle(
        "variables", box, "variable_type", c("Choices" = "closed_ended", "Write-in" = "open_ended"),
        label = "Kind"
      ),
      labelled(
        "MATR ID",
        hint = paste(
          "A new wave of a recorded questionnaire gets the MATR IDs its questions already have.",
          "Upload MATR IDs, at the top of the page, fills the rest from a CSV."
        ),
        field_text("variables", box, "matr_id")
      ),
      labelled("About", field_select("variables", box, "subject", lookups$subjects)),
      labelled("Label", field_text("variables", box, "label", placeholder = "Optional, e.g. weeks")),
      if (!is_choice_box) {
        labelled("Answer type", field_select("variables", box, "answer_data_type", lookups$answer_data_types))
      },
      review_button(
        icon("trash-can"), "delete",
        `data-table` = "variables", `data-id` = box$id,
        class = "btn btn-sm btn-icon", title = "Delete this variable"
      )
    ),
    if (is_choice_box) choice_table(box, draft, ns)
  )
}

# "Apply to all" sits under the Code column, since it copies the codes.
choice_table <- function(box, draft, ns) {
  choices <- draft_service_children(draft, "response_options", box$id)
  list_id <- ns(paste0("choices-", box$id))
  any_printed_fields <- any(shows_printed_fields(choices))
  div(
    class = "choice-table",
    div(
      class = "choice-row choice-heading",
      span(), span("Choice"), span("Code"), span("Printed?"),
      if (any_printed_fields) span("Printed as", hint_icon("Leave blank if no difference")) else span(),
      if (any_printed_fields) span("Skip to") else span(),
      span()
    ),
    div(
      id = list_id, class = "choice-list", `data-table` = "response_options",
      map_rows(choices, choice_row, ns)
    ),
    sortable::sortable_js(list_id, list_sortable_options(".choice-handle")),
    div(
      class = "choice-row choice-actions",
      review_button(tagList(icon("plus"), " Choice"), "add_choice", `data-id` = box$id, class = "btn btn-sm btn-outline-dark choice-add"),
      div(
        class = "choice-apply",
        bslib::tooltip(
          review_button("Apply to all", "copy_codes", `data-id` = box$id),
          "Use these codes wherever these same choices appear in the questionnaire."
        )
      )
    )
  )
}

# Printed as and Skip to only mean something for choices printed on the form.
# They still show if they have words, so nothing is hidden.
shows_printed_fields <- function(choices) {
  choices$printed %in% TRUE | !is.na(choices$printed_label) | !is.na(choices$skip_instruction)
}

choice_row <- function(choice, ns) {
  printed_fields <- shows_printed_fields(choice)
  div(
    class = "choice-row", `data-id` = choice$id,
    span(class = "choice-handle", title = "Drag to reorder", icon("grip-vertical")),
    field_text(
      "response_options", choice, "answer_option",
      placeholder = "Pick or type", list = ns("answer-options")
    ),
    field_text("response_options", choice, "code_value", type = "number"),
    field_checkbox("response_options", choice, "printed", "Printed on the form"),
    if (printed_fields) field_text("response_options", choice, "printed_label", placeholder = "—") else span(),
    if (printed_fields) field_text("response_options", choice, "skip_instruction", placeholder = "—") else span(),
    review_button(
      icon("trash-can"), "delete",
      `data-table` = "response_options", `data-id` = choice$id,
      class = "btn btn-sm btn-icon", title = "Delete this choice"
    )
  )
}

#' What the checks found, as a count for the toolbar.
#'
#' @param problems The result of `draft_service_check()`.
#' @return A UI element, or `NULL` when there is nothing to fix.
to_fix_note <- function(problems) {
  n <- length(unique(problems$id))
  if (n == 0) {
    return(span(class = "to-fix-note is-clear", icon("check"), " Nothing to fix"))
  }
  span(class = "to-fix-note", icon("triangle-exclamation"), paste0(" ", plural(n, "block"), " to fix"))
}
