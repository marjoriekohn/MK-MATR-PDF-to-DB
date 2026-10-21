#' The outline of the Edit Extraction page: the questionnaire's blocks as a tree, each edited where it is.
#'
#' A block's words are typed straight into its row (with the words of other blocks of the same kind
#' offered as it is typed), and its row holds the buttons to mark it as checked and to delete it. A
#' question's row also has its place among the questions (kept up to date as blocks move), the number
#' printed beside it, the question it follows, and opens a nested panel for what else a question has:
#' its skip condition, and its variables with their choices.
#'
#' @param id The module's id.
#' @return The page's UI.
extraction_outline_ui <- function(id) {
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
# problems the checks found, the words to suggest and the uploaded MATR IDs no
# variable has yet.

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
  onMove = htmlwidgets::JS("editExtractionPage.showDropHint"),
  onEnd = htmlwidgets::JS("editExtractionPage.sendOutline")
)

# The id of the list of words offered for a kind of block, e.g. "edit_extraction-outline-suggest-Section".
suggest_list_id <- function(ns, component_type) ns(paste0("suggest-", gsub("[^A-Za-z0-9]", "_", component_type)))

# The id of the list of uploaded MATR IDs no variable has yet, which every MATR ID field offers. The page
# script keeps it up to date as IDs are given (review.js).
matr_id_list_id <- function(ns) ns("matr-ids")

#' The outline as a tree.
#'
#' @param draft A draft.
#' @param view `list(selected, collapsed, opened, problems, suggestions, matr_ids)`: what is selected,
#'   which sections are closed, which questions have their panel open, the problems to mark, the words to
#'   offer for each kind of block (`draft_service_text_suggestions()`) and the uploaded MATR IDs to offer
#'   (`draft_service_available_matr_ids()`).
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param ns The module's namespace function.
#' @return A UI element.
outline_tree <- function(draft, view, lookups, ns) {
  # every question's row offers the questions before it; they are worked out once for the whole tree
  attr(draft, "outline_questions") <- outline_questions(draft)
  div(
    class = "outline-tree",
    outline_list(draft, section = NULL, view, lookups, ns),
    # The words offered as staff type, one list per kind of block, plus the answer choices, conditions and MATR IDs
    lapply(names(view$suggestions %||% list()), function(component_type) {
      suggestions(suggest_list_id(ns, component_type), view$suggestions[[component_type]])
    }),
    suggestions(ns("answer-options"), lookups$answer_options),
    suggestions(ns("conditions"), unique(c("If yes", "If no", stats::na.omit(draft$components$condition)))),
    suggestions(matr_id_list_id(ns), view$matr_ids %||% character())
  )
}

#' What the page script marks on the outline, so it isn't drawn again for it.
#'
#' Drawing the outline takes a second or more (every block's fields and drop-downs), and the browser
#' takes as long again to rebuild it. So what a click changes without changing the layout is marked on
#' the outline already there instead: which row is selected, which sections are closed, which questions
#' have their panel open, which blocks are checked (a section counts as checked for the eye only once
#' everything inside it is), each block's problems and the fields at fault, and the uploaded MATR IDs no
#' variable has yet.
#'
#' @param draft A draft.
#' @param problems From `draft_service_check()`.
#' @param selected The selected block's id, or `NULL`.
#' @param collapsed,opened The ids of the closed sections, and of the questions with their panel open.
#' @param matr_ids The uploaded MATR IDs no variable has yet.
#' @param ns The module's namespace function.
#' @return A list, sent as the "edit-extraction-marks" message (www/js/review.js).
outline_marks <- function(draft, problems, selected, collapsed, opened, matr_ids, ns) {
  counts <- as.list(table(problems$id))
  messages <- lapply(split(problems$message, problems$id), paste, collapse = "\n")
  at_fault <- problems[!is.na(problems$table), , drop = FALSE]
  blocks <- draft$components
  checked <- blocks$id[blocks$checked %in% TRUE]
  sections <- intersect(checked, blocks$id[blocks$component_type == "Section"])
  not_all <- sections[!vapply(sections, function(id) {
    all(blocks$checked[blocks$id %in% draft_service_printed_order(draft, id)] %in% TRUE)
  }, logical(1))]
  list(
    selected = selected,
    problems = lapply(names(counts), function(id) list(id = id, n = counts[[id]], title = messages[[id]])),
    fields = map_rows(at_fault, function(problem) {
      list(table = problem$table, id = problem$row, field = if (is.na(problem$field)) "" else problem$field, title = problem$message)
    }),
    matr_ids = as.list(matr_ids),
    matr_list = matr_id_list_id(ns),
    collapsed = as.list(collapsed),
    opened = as.list(opened),
    checked = as.list(checked),
    all_checked = as.list(setdiff(checked, not_all)),
    check_titles = as.list(outline_check_titles)
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
        title = "Show or hide the question's skip condition and variables"
      ),
      span(class = "btn-toggle")
    ),
    type_chip(block$component_type),
    if (block$component_type == "Question") question_number_fields(block, view$problems),
    outline_text_field(block, view$problems, ns),
    if (block$component_type == "Question") parent_question_field(block, draft, view$problems),
    outline_badges(block, draft, view$problems),
    outline_actions(block, draft)
  )
}

# A question's place among the questions (1, 2, 3..., which follows the outline), then the number printed
# beside it, typed (it can have letters: 1a4). The printed number is what the wave records; a question
# without one is asked as its place.
question_number_fields <- function(block, problems) {
  span(
    class = "outline-numbers",
    span(
      class = "outline-number", title = "Its place among the questions, which follows the outline",
      if (is.na(block$question_number)) "?" else block$question_number
    ),
    field_text(
      "components", block, "printed_number",
      placeholder = "No.", label = "Question number as printed, e.g. 1a4", problems = problems
    ) |> tagAppendAttributes(class = "outline-printed-number", title = "Question number as printed, e.g. 1a4")
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
outline_text_field <- function(block, problems, ns) {
  placeholder <- switch(block$component_type,
                        Section = "Section heading, as printed (e.g. ‘Section 1: About You’). A section can be left blank.",
                        Question = "The question's words only. Introductions, instructions and definitions go in their own block.",
                        paste("The", tolower(block$component_type), "as printed")
  )
  tags$textarea(
    class = c("form-control form-control-sm outline-text", field_problem_class(problems, "components", block$id, "text")),
    rows = 1, placeholder = placeholder, `aria-label` = paste(block$component_type, "text"),
    `data-autofocus` = if (is.na(block$text)) NA,
    `data-suggest` = suggest_list_id(ns, block$component_type),
    !!!field_problem_attributes(problems, "components", block$id, "text"),
    !!!field_attributes("components", block, "text"),
    blank_if_na(block$text)
  )
}

# The question it follows, beside its words: when it's asked in the context of another question, or is
# only shown when a skip condition is met.
parent_question_field <- function(block, draft, problems) {
  span(
    class = "outline-parent",
    field_select(
      "components", block, "parent_question_id", parent_question_choices(block, draft),
      blank = "No Parent Question", optional = TRUE, label = "Parent question", problems = problems
    )
  )
}

# What is said about the block at the end of its row. The problems badge is kept up to date by the page
# script as the checks run, so its count is drawn as the script expects (data-problems).
outline_badges <- function(block, draft, problems) {
  n_inside <- sum(draft$components$parent_id %in% block$id)
  n_boxes <- sum(draft$variables$component_id == block$id)
  block_problems <- problems$message[problems$id == block$id]
  span(
    class = "outline-badges",
    if (block$component_type == "Section") row_badge(paste(n_inside, if (n_inside == 1) "item" else "items")),
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

# What the check button says, checked or not. The page script says it too, when it marks a block
# as checked without the outline being drawn again (outline_marks()).
outline_check_titles <- c(
  checked = "Checked. Click to mark as not checked.",
  unchecked = "Mark as checked and go to the next block. If you change it afterwards, it needs checking again."
)

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
      title = outline_check_titles[[if (block$checked) "checked" else "unchecked"]]
    ),
    review_button(
      icon("trash-can"), "delete",
      `data-table` = "components", `data-id` = block$id,
      class = "btn btn-sm btn-icon", title = paste("Delete this", tolower(block$component_type))
    )
  )
}

# ------ A question's panel ---------------------------------------------------------------
# Under a question's row, when its arrow is open: its skip condition, its variables and their choices,
# and what the checks found that isn't one field's (a field at fault is outlined in red instead).
question_panel <- function(block, draft, view, lookups, ns) {
  of_block <- view$problems[view$problems$id == block$id, , drop = FALSE]
  messages <- of_block$message[is.na(of_block$table)]
  div(
    class = "question-panel",
    if (length(messages) > 0) div(class = "callout details-problems", tags$ul(lapply(messages, tags$li))),
    question_fields(block, ns),
    variable_cards(block, draft, view, lookups, ns)
  )
}

# The printed condition. It shows once there's a parent question, or if it already has words (e.g. its
# parent was deleted), so it's never hidden.
question_fields <- function(block, ns) {
  if (is.na(block$parent_question_id) && is.na(block$condition)) {
    return(NULL)
  }
  div(
    class = "details-row",
    span(
      class = "details-field",
      field_text("components", block, "condition", placeholder = "Skip condition", list = ns("conditions"), label = "Skip condition")
    )
  )
}

# Questions printed before this one, as "7 · Have you smoked…" (the number it is asked as). The current
# pick stays listed even if a move put it after this question.
parent_question_choices <- function(block, draft) {
  all <- attr(draft, "outline_questions") %||% outline_questions(draft)
  mine <- match(block$id, all$printed)
  questions <- all$questions[all$questions$id != block$id, , drop = FALSE]
  is_earlier <- questions$position < mine
  keep <- is_earlier | questions$id %in% block$parent_question_id
  labels <- paste0(questions$label, ifelse(is_earlier, "", " (printed after it)"))
  setNames(questions$id[keep], labels[keep])
}

# A draft's questions in printed order, each with its place and its label in a parent-question list
# ("1a · Do you smoke?"), and every block's id in printed order.
outline_questions <- function(draft) {
  printed <- draft_service_printed_order(draft)
  questions <- draft$components[match(printed, draft$components$id), , drop = FALSE]
  questions <- questions[questions$component_type == "Question", , drop = FALSE]
  numbers <- draft_service_question_number(questions)
  list(
    printed = printed,
    questions = data.frame(
      id = questions$id,
      position = match(questions$id, printed),
      label = paste0(ifelse(is.na(numbers), "?", numbers), " · ", shorten(questions$text, 50)),
      stringsAsFactors = FALSE
    )
  )
}

# ------ Variables and choices -----------------------------------------------
# Variables reorder within their question, choices within their variable.
list_sortable_options <- function(handle) {
  sortable::sortable_options(handle = handle, onEnd = htmlwidgets::JS("editExtractionPage.sendList"))
}

#' A question's variables and their choices.
#'
#' Each variable is one row: its kind (choices or write-in), its MATR ID, who it's about, its label,
#' and, for a write-in, the kind of answer; its choices are listed under the row. What each field holds
#' is said in it while it is empty, and its label is kept for screen readers.
#'
#' @param block One row of a draft's `components`: a question.
#' @param draft A draft.
#' @param view What the outline shows; see `outline_tree()`. `problems` outlines the fields at fault.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param ns The module's namespace function.
#' @return A UI element.
variable_cards <- function(block, draft, view, lookups, ns) {
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
      map_rows(boxes, variable_card, draft, view$problems, lookups, ns)
    ),
    sortable::sortable_js(list_id, list_sortable_options(".box-handle"))
  )
}

variable_card <- function(box, draft, problems, lookups, ns) {
  is_choice_box <- box$variable_type == "closed_ended"
  div(
    class = c("variable-card", field_problem_class(problems, "variables", box$id, NA)),
    `data-id` = box$id,
    div(
      class = "variable-card-row",
      span(class = "box-handle", title = "Drag to reorder", icon("grip-vertical")),
      span(class = "variable-card-number", as.integer(box$display_order)),
      field_toggle(
        "variables", box, "variable_type", c("Choices" = "closed_ended", "Write-in" = "open_ended"),
        label = "Kind"
      ),
      span(
        class = "details-field",
        field_text(
          "variables", box, "matr_id",
          placeholder = "MATR ID", list = matr_id_list_id(ns), label = "MATR ID", problems = problems
        )
      ),
      span(
        class = "details-field",
        field_select(
          "variables", box, "subject", lookups$subjects,
          blank = "Select who this question is about", label = "Question subject", problems = problems
        )
      ),
      span(class = "details-field", field_text("variables", box, "label", placeholder = "Label (optional)", label = "Optional label")),
      if (!is_choice_box) {
        span(
          class = "details-field",
          field_select(
            "variables", box, "answer_data_type", lookups$answer_data_types,
            blank = "Select an answer type", label = "Answer type", problems = problems
          )
        )
      },
      review_button(
        icon("trash-can"), "delete",
        `data-table` = "variables", `data-id` = box$id,
        class = "btn btn-sm btn-icon", title = "Delete this variable"
      )
    ),
    if (is_choice_box) choice_table(box, draft, problems, ns)
  )
}

# "Apply to all" sits under the Code column, since it copies the codes.
choice_table <- function(box, draft, problems, ns) {
  choices <- draft_service_children(draft, "response_options", box$id)
  list_id <- ns(paste0("choices-", box$id))
  div(
    class = "choice-table",
    div(
      id = list_id, class = "choice-list", `data-table` = "response_options",
      map_rows(choices, choice_row, problems, ns)
    ),
    sortable::sortable_js(list_id, list_sortable_options(".choice-handle")),
    div(
      class = "choice-row choice-actions",
      div(
        class = "choice-apply",
        bslib::tooltip(
          review_button("Apply to all", "copy_codes", `data-id` = box$id),
          "Use these codes wherever these same choices appear in the questionnaire."
        )
      ),
      review_button(tagList(icon("plus"), " Choice"), "add_choice", `data-id` = box$id, class = "btn btn-sm btn-outline-dark choice-add")
    )
  )
}

# Code first, then the response option it codes, how it is printed and where it skips to. Every choice is
# printed on the form: one that isn't can't be marked, so it isn't a choice (a lone check box has one).
choice_row <- function(choice, problems, ns) {
  div(
    class = "choice-row", `data-id` = choice$id,
    span(class = "choice-handle", title = "Drag to reorder", icon("grip-vertical")),
    field_text("response_options", choice, "code_value", placeholder = "Code", type = "number", label = "Code", problems = problems),
    field_text(
      "response_options", choice, "answer_option",
      placeholder = "Response Option", list = ns("answer-options"), label = "Response option", problems = problems
    ),
    field_text("response_options", choice, "printed_label", placeholder = "Printed as (optional)", label = "Printed as"),
    field_text("response_options", choice, "skip_instruction", placeholder = "Skip to (optional)", label = "Skip to"),
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
