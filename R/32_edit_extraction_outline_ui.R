#' The outline of the Edit Extraction page: the questionnaire's blocks as a tree, each edited where it is.
#'
#' A block's words are typed straight into its row (with the words of other blocks of the same kind
#' offered as it is typed), and its row holds the buttons to mark it as checked and to delete it. A
#' question's row also has its place among the questions (kept up to date as blocks move), the number
#' printed beside it, the question it follows and the condition printed for it, and opens a nested
#' panel for its variables with their choices. A question group's row has no words: it only holds
#' blocks.
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
# Each block is a row. A section's (or question group's) row sits above its own list, and dropping a
# block into that list puts the block inside it. Only sections and question groups have a list, so
# only they can hold blocks (BR53). `view` holds what's selected, which sections and groups are
# collapsed, which questions have their panel open, the problems the checks found, the words to suggest,
# the uploaded MATR IDs no variable has yet, and the optional fields staff have added to variables.

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

# The id of the list of questions every parent-question field offers ("7 · Have you smoked…"). The page
# script keeps it up to date as blocks move (review.js).
parent_list_id <- function(ns) ns("parent-questions")

# The id of the list of places a choice can skip to ("32 · Have you smoked…", "Section · Part B…"), which
# every Skip to field offers. The page script keeps it up to date as blocks move (review.js).
skip_list_id <- function(ns) ns("skip-targets")

#' The outline as a tree.
#'
#' @param draft A draft.
#' @param view `list(selected, collapsed, opened, problems, suggestions, matr_ids, shown)`: what is
#'   selected, which sections and groups are closed, which questions have their panel open, the problems
#'   to mark, the words to offer for each kind of block (`draft_service_text_suggestions()`), the uploaded
#'   MATR IDs to offer (`draft_service_available_matr_ids()`) and the optional fields added to variables
#'   that have nothing in them yet (`"v12:label"`; see `variable_card()`).
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
    suggestions(ns("subjects"), lookups$subjects),
    suggestions(ns("labels"), label_suggestions(draft)),
    suggestions(ns("conditions"), unique(c("If yes", "If no", stats::na.omit(draft$components$condition)))),
    suggestions(matr_id_list_id(ns), view$matr_ids %||% character()),
    suggestions(parent_list_id(ns), attr(draft, "outline_questions")$questions$label),
    suggestions(skip_list_id(ns), skip_targets(draft)$label)
  )
}

#' What the page script marks on the outline, so it isn't drawn again for it.
#'
#' Drawing the outline takes a second or more (every block's fields and drop-downs), and the browser
#' takes as long again to rebuild it. So what a click changes without changing the layout is marked on
#' the outline already there instead: which row is selected, which sections and groups are closed, which
#' questions have their panel open, which blocks are checked (a section or group counts as checked for the
#' eye only once everything inside it is), each block's problems and the fields at fault, and the uploaded
#' MATR IDs no variable has yet.
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
  sections <- intersect(checked, blocks$id[is_container(blocks$component_type)])
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
    labels = as.list(label_suggestions(draft)),
    label_list = ns("labels"),
    collapsed = as.list(collapsed),
    opened = as.list(opened),
    checked = as.list(checked),
    all_checked = as.list(setdiff(checked, not_all)),
    check_titles = as.list(outline_check_titles)
  )
}

#' What to change on the outline already drawn, instead of drawing it again.
#'
#' The outline was drawn from `drawn`, and changed since only as the page script changes it (words
#' typed, blocks dragged, marks). This compares it with `draft` and says what is different: the blocks
#' to take out and to add (only the outermost: a section comes with what is in it), the rows whose
#' fields or counts changed, the open question panels whose variables or choices changed, every
#' list's blocks in order (so a block that moved is moved, and a drag the draft refused is put back),
#' each block's display order and label, each question's number, and the questions in printed order
#' (which every parent-question field offers). When much has changed (another draft, say), drawing the
#' outline again is quicker, and this says so.
#'
#' @param drawn The draft as the outline on the page shows it, or `NULL` if it isn't drawn yet.
#' @param draft The draft as it is now.
#' @param view What the outline shows; see `outline_tree()`.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param ns The module's namespace function.
#' @param most How many blocks, rows and panels may be drawn before drawing it all is quicker.
#' @param rows,panels Rows and open question panels (by block id) to draw again even if their draft
#'   hasn't changed: a field there shows what the draft refused.
#' @return A list, sent as the "edit-extraction-patch" message (www/js/review.js), or `NULL` to draw
#'   the outline again.
outline_patch <- function(drawn, draft, view, lookups, ns, most = 30, rows = NULL, panels = NULL) {
  if (is.null(drawn)) {
    return(NULL)
  }
  questions <- outline_questions(draft)
  attr(draft, "outline_questions") <- questions
  old <- drawn$components
  new <- draft$components
  outermost <- function(ids, blocks) ids[!blocks$parent_id[match(ids, blocks$id)] %in% ids]
  removed <- outermost(setdiff(old$id, new$id), old)
  added <- outermost(setdiff(new$id, old$id), new)
  kept <- intersect(new$id, old$id)
  rows <- union(intersect(rows, kept), kept[outline_row_signature(drawn)[kept] != outline_row_signature(draft)[kept]])
  open <- intersect(view$opened, kept)
  panels <- union(intersect(panels, open), open[vapply(open, function(id) {
    !identical(outline_panel_signature(drawn, id), outline_panel_signature(draft, id))
  }, logical(1))])
  if (length(added) + length(rows) + length(panels) > most) {
    return(NULL)
  }
  block <- function(id) new[new$id == id, , drop = FALSE]
  html <- function(ui) htmltools::renderTags(ui)$html
  is_question <- new$component_type == "Question"
  sections <- new$id[is_container(new$component_type)]
  list(
    remove = as.list(removed),
    insert = lapply(added, function(id) {
      list(id = id, parent = blank_if_na(block(id)$parent_id), html = html(outline_node(block(id), draft, view, lookups, ns)))
    }),
    rows = lapply(rows, function(id) list(id = id, html = html(outline_row(block(id), draft, view, ns)))),
    panels = lapply(panels, function(id) list(id = id, html = html(question_panel(block(id), draft, view, lookups, ns)))),
    lists = lapply(c("", sections), function(parent) {
      list(parent = parent, ids = as.list(draft_service_children(draft, "components", if (nzchar(parent)) parent else NA)$id))
    }),
    orders = as.list(stats::setNames(as.integer(new$display_order), new$id)),
    numbers = as.list(stats::setNames(
      ifelse(is.na(new$question_number[is_question]), "?", new$question_number[is_question]), new$id[is_question]
    )),
    labels = stats::setNames(map_rows(new, block_label), new$id),
    questions = as.list(questions$questions$label),
    parent_list = parent_list_id(ns),
    skip_targets = as.list(skip_targets(draft)$label),
    skip_list = skip_list_id(ns),
    selected = view$selected
  )
}

# What a block's row is drawn from, as one string per block (named by id): its kind, words, printed
# number, parent question (as its list names it) and condition, and how many blocks or variables it has.
# What the page script keeps up to date on its own (selected, checked, problems, numbers, display order)
# is left out.
outline_row_signature <- function(draft) {
  blocks <- draft$components
  n_inside <- tabulate(match(blocks$parent_id, blocks$id), nbins = nrow(blocks))
  n_boxes <- tabulate(match(draft$variables$component_id, blocks$id), nbins = nrow(blocks))
  questions <- (attr(draft, "outline_questions") %||% outline_questions(draft))$questions
  parent <- questions$label[match(blocks$parent_question_id, questions$id)]
  stats::setNames(
    paste(blocks$component_type, blocks$text, blocks$printed_number, parent, blocks$condition, n_inside, n_boxes, sep = "\r"),
    blocks$id
  )
}

# What a question's panel is drawn from: its variables and their choices.
outline_panel_signature <- function(draft, id) {
  boxes <- draft$variables[draft$variables$component_id %in% id, , drop = FALSE]
  choices <- draft$response_options[draft$response_options$variable_id %in% boxes$id, , drop = FALSE]
  paste(jsonlite::toJSON(boxes), jsonlite::toJSON(choices))
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
  is_section <- is_container(block$component_type)
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
    if (is_container(block$component_type)) {
      review_button(
        icon("caret-down"), "toggle",
        `data-id` = block$id, class = "btn-toggle", title = "Show or hide what's inside", `aria-label` = "Show or hide what's inside"
      )
    } else if (block$component_type == "Question") {
      review_button(
        icon("caret-right"), "details",
        `data-id` = block$id, class = "btn-toggle is-details",
        title = "Show or hide the question's variables", `aria-label` = "Show or hide the question's variables"
      )
    } else {
      span(class = "btn-toggle")
    },
    type_chip(block$component_type),
    if (block$component_type == "Question") question_number_fields(block, view$problems),
    if (block$component_type == "Question Group") span(class = "outline-group-note") else outline_text_field(block, view$problems, ns),
    if (block$component_type == "Question") parent_question_field(block, draft, view$problems, ns),
    outline_badges(block, draft, view$problems),
    outline_actions(block, draft)
  )
}

# A question's place among the questions (1, 2, 3..., which follows the outline), then the number printed
# beside it, read off the PDF (it can have letters, and repeat from section to section: 1a4). The wave
# records both.
question_number_fields <- function(block, problems) {
  span(
    class = "outline-numbers",
    span(
      class = "outline-number", title = "Its place among the questions, worked out from the outline",
      if (is.na(block$question_number)) "?" else block$question_number
    ),
    field_text(
      "components", block, "printed_number",
      placeholder = "No.", label = "Question number as printed, e.g. 1a4 (blank if none is printed)", problems = problems
    ) |> tagAppendAttributes(class = "outline-printed-number", title = "Question number as printed, e.g. 1a4 (blank if none is printed)")
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
                        Section = paste(
                          "Section heading, as printed (e.g. ‘Section 1: About You’).",
                          "A group with no heading is a question group."
                        ),
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

# The question it follows, beside its words: when it's asked in the context of another question, or only
# some respondents reach it. Typed or picked from one list of the questions that every row shares (a
# drop-down in every row listed every question before it: tens of thousands of entries in a long
# questionnaire). Once it has one, the condition printed for it ("If yes") follows, also as printed.
parent_question_field <- function(block, draft, problems, ns) {
  questions <- (attr(draft, "outline_questions") %||% outline_questions(draft))$questions
  parent <- questions$label[match(block$parent_question_id, questions$id)]
  tagList(
    span(
      class = "outline-parent",
      tags$input(
        type = "text",
        class = c("form-control form-control-sm", field_problem_class(problems, "components", block$id, "parent_question_id")),
        value = blank_if_na(parent), placeholder = "Parent question", `aria-label` = "Parent question: its number, or pick it",
        `data-suggest` = parent_list_id(ns), `data-pick` = NA,
        !!!field_problem_attributes(problems, "components", block$id, "parent_question_id"),
        !!!field_attributes("components", block, "parent_question_id")
      )
    ),
    if (!is.na(block$parent_question_id) || !is.na(block$condition)) {
      span(
        class = "outline-condition",
        field_text(
          "components", block, "condition",
          placeholder = "Condition, as printed", list = ns("conditions"), label = "Condition, as printed (e.g. If yes)"
        )
      )
    }
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
    if (is_container(block$component_type)) row_badge(paste(n_inside, if (n_inside == 1) "item" else "items")),
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
    # listed as the badge is pointed at or focused (review.js)
    tabindex = "0", `data-problems` = paste(messages, collapse = "\n")
  )
}

# What the check button says, checked or not. The page script says it too, when it marks a block
# as checked without the outline being drawn again (outline_marks()).
outline_check_titles <- c(
  checked = "Checked. Click to mark as not checked.",
  unchecked = "Mark as checked and go to the next block. If you change it afterwards, it needs checking again."
)

# Mark as checked, take a section or group away but keep what's inside, and delete. A section or group counts
# as checked for the eye only once everything inside it is.
outline_actions <- function(block, draft) {
  inside <- draft_service_printed_order(draft, block$id)
  all_checked <- block$checked && all(draft$components$checked[draft$components$id %in% inside])
  ungroup <- if (block$component_type == "Section") "Remove this heading and keep what's inside" else "Ungroup: keep what's inside"
  span(
    class = "outline-actions",
    if (is_container(block$component_type)) {
      review_button(
        icon("object-ungroup"), "ungroup",
        `data-id` = block$id, class = "btn btn-sm btn-icon", title = ungroup, `aria-label` = ungroup
      )
    },
    review_button(
      icon("user-check"), "check",
      `data-id` = block$id,
      class = c("btn btn-sm btn-icon outline-check", if (block$checked) "is-checked", if (all_checked) "is-all-checked"),
      title = outline_check_titles[[if (block$checked) "checked" else "unchecked"]], `aria-label` = "Checked"
    ),
    review_button(
      icon("trash-can"), "delete",
      `data-table` = "components", `data-id` = block$id,
      class = "btn btn-sm btn-icon", title = paste("Delete this", tolower(block$component_type)),
      `aria-label` = paste("Delete this", tolower(block$component_type))
    )
  )
}

# ------ A question's panel ---------------------------------------------------------------
# Under a question's row, when its arrow is open: its variables and their choices, and what the checks
# found that isn't one field's (a field at fault is outlined in red instead).
question_panel <- function(block, draft, view, lookups, ns) {
  of_block <- view$problems[view$problems$id == block$id, , drop = FALSE]
  messages <- of_block$message[is.na(of_block$table)]
  div(
    class = "question-panel",
    if (length(messages) > 0) div(class = "callout details-problems", tags$ul(lapply(messages, tags$li))),
    variable_cards(block, draft, view, lookups, ns)
  )
}

# The labels the draft's variables have, each once, offered as a label is typed ("First child", "Second child").
label_suggestions <- function(draft) {
  labels <- trimws(draft$variables$label)
  sort(unique(labels[!is.na(labels) & nzchar(labels)]))
}

# A draft's questions in printed order, each with its place and its label in the parent-question list
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
      label = paste0(ifelse(is.na(numbers), "?", numbers), outline_number_separator, shorten(questions$text, 50)),
      stringsAsFactors = FALSE
    )
  )
}

# ------ Variables and choices -----------------------------------------------
# Variables reorder within their question, choices within their variable.
list_sortable_options <- function(handle) {
  sortable::sortable_options(handle = handle, onEnd = htmlwidgets::JS("editExtractionPage.sendList"))
}

#' A question's variables, as a table: one line for each, in printed order.
#'
#' A line has the variable's kind (choices or write-in), its MATR ID and, for a write-in, the kind of answer;
#' for a variable with choices, the choices in short ("1 Yes · 2 No"), which open their editor under the line
#' when clicked. Who it's about and its label show when they have something in them (About is blank for the
#' respondent), or once added from the line's "+". When the question's variables
#' all offer the same choices (a grid's columns), the choices are shown once under the table and edited once
#' for all of them, unless staff chose to edit each variable's own (`view$split`). What each field holds is
#' said in it while it is empty, and its label is kept for screen readers.
#'
#' @param block One row of a draft's `components`: a question.
#' @param draft A draft.
#' @param view What the outline shows; see `outline_tree()`. `problems` outlines the fields at fault, `opened`
#'   is used for the variables whose choices are open (`view$choices`), `split` for the questions whose
#'   variables' choices are edited one by one, and `shown` for the choices' optional fields added empty.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param ns The module's namespace function.
#' @return A UI element.
variable_cards <- function(block, draft, view, lookups, ns) {
  boxes <- draft_service_children(draft, "variables", block$id)
  shared <- if (block$id %in% view$split) character() else draft_service_shared_choices(draft, block$id)
  list_id <- ns(paste0("boxes-", block$id))
  div(
    class = "variable-cards",
    div(
      class = "variable-cards-header",
      h3("Variables"),
      if (block$id %in% view$split && length(draft_service_shared_choices(draft, block$id)) > 0) {
        review_button("Edit their choices together", "join_choices", `data-id` = block$id, class = "btn btn-sm btn-link")
      },
      review_button(tagList(icon("plus"), " Variable"), "add_box", `data-id` = block$id)
    ),
    if (nrow(boxes) > 0) {
      div(
        class = "variable-line variable-head", `aria-hidden` = "true",
        span(), span("#"), span("Kind"), span("MATR ID"), span("Choices or answer"), span("About and label"), span()
      )
    },
    div(
      id = list_id, class = "box-list", `data-table` = "variables",
      map_rows(boxes, variable_line, draft, view, lookups, ns, shared)
    ),
    sortable::sortable_js(list_id, list_sortable_options(".box-handle")),
    if (length(shared) > 0) choice_editor(boxes[boxes$id == shared[1], ], draft, view, ns, shared = shared, question = block$id)
  )
}

# One cell of a variable's line, named for a narrow screen, where the cells stack (data-label).
variable_cell <- function(label, ..., class = NULL) span(class = c("variable-cell", class), `data-label` = label, ...)

variable_line <- function(box, draft, view, lookups, ns, shared = character()) {
  problems <- view$problems
  is_choice_box <- box$variable_type == "closed_ended"
  choices <- draft_service_children(draft, "response_options", box$id)
  choices_at_fault <- any(problems$table %in% "response_options" & problems$row %in% choices$id)
  is_open <- is_choice_box && !box$id %in% shared && box$id %in% view$choices
  div(
    class = c("variable-card", field_problem_class(problems, "variables", box$id, NA), if (is_open) "is-open"),
    `data-id` = box$id,
    div(
      class = "variable-line",
      span(class = "box-handle", title = "Drag to reorder", icon("grip-vertical")),
      span(class = "variable-card-number", as.integer(box$display_order)),
      variable_cell("Kind", field_select(
        "variables", box, "variable_type", c("Choices" = "closed_ended", "Write-in" = "open_ended"), label = "Kind"
      ), class = "is-kind"),
      variable_cell("MATR ID", field_text(
        "variables", box, "matr_id",
        placeholder = "MATR ID", list = matr_id_list_id(ns), label = "MATR ID", problems = problems
      ), class = "is-id"),
      variable_cell(
        if (is_choice_box) "Choices" else "Answer type",
        if (!is_choice_box) {
          field_select(
            "variables", box, "answer_data_type", lookups$answer_data_types,
            blank = "Pick one", label = "Answer type", problems = problems
          )
        } else if (box$id %in% shared) {
          span(
            class = "choice-summary is-shared", title = "The same choices as the question's other variables, edited below",
            choice_summary(choices)
          )
        } else {
          review_button(
            tagList(icon(if (is_open) "caret-down" else "caret-right"), " ", choice_summary(choices)), "choices",
            `data-id` = box$id, `aria-expanded` = if (is_open) "true" else "false",
            class = c("btn btn-sm choice-summary", if (choices_at_fault) "has-problem"),
            title = if (is_open) "Close the choices" else "Edit the choices"
          )
        },
        class = "is-choices"
      ),
      variable_extras(box, view, ns),
      review_button(
        icon("trash-can"), "delete",
        `data-table` = "variables", `data-id` = box$id,
        class = "btn btn-sm btn-icon", title = "Delete this variable", `aria-label` = "Delete this variable"
      )
    ),
    if (is_open) choice_editor(box, draft, view, ns)
  )
}

# The optional fields of a variable, by what its "+" menu calls them: who it's about (blank is the
# respondent) and its label. A variable shows one when it has something in it, or once it is added.
optional_variable_fields <- c(subject = "About", label = "Label")

# A variable's About and label when it shows them, each with its ×, and the "+" that adds them.
variable_extras <- function(box, view, ns) {
  showing <- shown_optional(box, optional_variable_fields, view)
  field <- function(name) {
    input <- switch(name,
      subject = field_text(
        "variables", box, "subject",
        placeholder = "Who it's about", list = ns("subjects"), label = "About: who it's about (blank for the respondent)",
        problems = view$problems
      ),
      label = field_text("variables", box, "label", placeholder = "Label", list = ns("labels"), label = "Label")
    )
    if (identical(view$focus, paste0(box$id, ":", name))) input <- tagAppendAttributes(input, `data-focus-now` = NA)
    span(
      class = c("variable-optional", paste0("is-", name)), `data-label` = optional_variable_fields[[name]],
      input, remove_field_button(box, name, optional_variable_fields)
    )
  }
  addable <- setdiff(names(optional_variable_fields), showing)
  variable_cell(
    "About and label",
    lapply(showing, field),
    if (length(addable) > 0) add_field_menu(box, addable, optional_variable_fields, "Add who it's about, or a label"),
    class = "is-extras"
  )
}

# Which of a row's optional fields it shows: those with something in them, and those added empty.
shown_optional <- function(row, fields, view) {
  names(fields)[vapply(names(fields), function(field) {
    !is.na(row[[field]]) || paste0(row$id, ":", field) %in% view$shown
  }, logical(1))]
}

# A variable's choices in a few words: "1 Yes · 2 No", the first few only.
choice_summary <- function(choices) {
  if (nrow(choices) == 0) {
    return("No choices yet")
  }
  codes <- ifelse(is.na(choices$code_value), "", paste0(choices$code_value, " "))
  words <- paste0(codes, ifelse(is.na(choices$answer_option), "(blank)", choices$answer_option))
  shown <- head(words, 4)
  paste0(paste(shorten(shown, 24), collapse = " · "), if (length(words) > 4) paste0(" · +", length(words) - 4, " more"))
}

# The optional fields of a choice, by what its "+" menu calls them: how it is printed and where it skips to
# (a link to the question or section, and the words printed for it). A choice shows one when it has
# something in it, or once it is added; most choices have neither.
optional_choice_fields <- c(printed_label = "Printed as", skip_instruction = "Skip to")

#' The editor of a variable's choices: one line per choice, then + Choice.
#'
#' @param box One row of a draft's `variables`.
#' @param draft A draft.
#' @param view What the outline shows; see `outline_tree()`.
#' @param ns The module's namespace function.
#' @param shared The ids of the question's variables these choices are edited for (all of them get each
#'   change), or none for this variable's own.
#' @param question The question's id, for "Edit each variable's own" (with `shared`).
#' @return A UI element.
choice_editor <- function(box, draft, view, ns, shared = character(), question = NULL) {
  choices <- draft_service_children(draft, "response_options", box$id)
  if (any(!is.na(choices$skip_to_id))) view$targets <- skip_targets(draft) # to name where they skip to
  list_id <- ns(paste0("choices-", box$id))
  div(
    class = c("choice-table", if (length(shared) > 0) "is-shared"),
    `data-shared` = paste(shared, collapse = ","),
    if (length(shared) > 0) {
      div(
        class = "choice-table-title",
        strong(sprintf("Choices of all %d variables", length(shared))),
        review_button("Edit each variable's own", "split_choices", `data-id` = question, class = "btn btn-sm btn-link")
      )
    },
    div(
      id = list_id, class = "choice-list", `data-table` = "response_options",
      map_rows(choices, choice_row, view$problems, ns, view)
    ),
    sortable::sortable_js(list_id, list_sortable_options(".choice-handle")),
    div(
      class = "choice-actions",
      review_button(tagList(icon("plus"), " Choice"), "add_choice", `data-id` = box$id, class = "btn btn-sm btn-outline-dark choice-add"),
      bslib::tooltip(
        review_button("Use these codes everywhere", "copy_codes", `data-id` = box$id, class = "btn btn-sm btn-link choice-apply"),
        "Use these codes wherever these same choices appear in the questionnaire."
      )
    )
  )
}

# Code first, then the response option it codes, then how it is printed and where it skips to when it has
# them, each with the × that takes it away, and the "+" that adds them. Every choice is printed on the form:
# one that isn't can't be marked, so it isn't a choice (a lone check box has one).
choice_row <- function(choice, problems, ns, view = list()) {
  key <- function(field) paste0(choice$id, ":", field)
  showing <- union(shown_optional(choice, optional_choice_fields, view), if (!is.na(choice$skip_to_id)) "skip_instruction")
  showing <- intersect(names(optional_choice_fields), showing)
  optional <- function(field) {
    name <- optional_choice_fields[[field]]
    focus <- identical(view$focus, key(field))
    input <- if (field == "skip_instruction") {
      skip_fields(choice, problems, ns, view$targets, focus)
    } else {
      field_text("response_options", choice, field, placeholder = name, label = name)
    }
    if (focus && field != "skip_instruction") input <- tagAppendAttributes(input, `data-focus-now` = NA)
    span(class = c("choice-optional", paste0("is-", field)), input, remove_field_button(choice, field, optional_choice_fields))
  }
  addable <- setdiff(names(optional_choice_fields), showing)
  div(
    class = "choice-row", `data-id` = choice$id,
    span(class = "choice-handle", title = "Drag to reorder", icon("grip-vertical")),
    field_text("response_options", choice, "code_value", placeholder = "Code", type = "number", label = "Code", problems = problems) |>
      tagAppendAttributes(class = "choice-code"),
    field_text(
      "response_options", choice, "answer_option",
      placeholder = "Response option", list = ns("answer-options"), label = "Response option", problems = problems
    ) |> tagAppendAttributes(class = "choice-answer"),
    lapply(showing, optional),
    if (length(addable) > 0) add_field_menu(choice, addable, optional_choice_fields, "Add how it is printed, or where it skips to"),
    review_button(
      icon("trash-can"), "delete",
      `data-table` = "response_options", `data-id` = choice$id,
      class = "btn btn-sm btn-icon", title = "Delete this choice", `aria-label` = "Delete this choice"
    )
  )
}

# Where a choice skips to: the question or section, picked from the list every Skip to field shares (or its
# number typed), and the words printed for it ("Go to Question 32").
skip_fields <- function(choice, problems, ns, targets = NULL, focus = FALSE) {
  target <- if (!is.na(choice$skip_to_id) && !is.null(targets)) targets$label[match(choice$skip_to_id, targets$id)]
  tagList(
    tags$input(
      type = "text",
      class = c("form-control form-control-sm skip-target", field_problem_class(problems, "response_options", choice$id, "skip_to_id")),
      value = blank_if_na(target %||% NA), placeholder = "Skips to", `aria-label` = "Skips to: the question's number, or pick it",
      `data-suggest` = skip_list_id(ns), `data-pick` = NA, `data-focus-now` = if (focus) NA,
      !!!field_problem_attributes(problems, "response_options", choice$id, "skip_to_id"),
      !!!field_attributes("response_options", choice, "skip_to_id")
    ),
    field_text("response_options", choice, "skip_instruction", placeholder = "As printed", label = "Skip instruction, as printed")
  )
}

# The "+" of a variable or choice: the optional fields it doesn't show yet (`names` says what each is called).
add_field_menu <- function(row, fields, names, title) {
  div(
    class = "dropdown add-field",
    tags$button(
      type = "button", class = "btn btn-sm btn-icon dropdown-toggle", `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      title = title, `aria-label` = title,
      icon("plus")
    ),
    tags$ul(
      class = "dropdown-menu dropdown-menu-end",
      lapply(fields, function(field) {
        tags$li(review_button(names[[field]], "show_field", `data-id` = row$id, `data-name` = field, class = "dropdown-item"))
      })
    )
  )
}

# The × that empties an optional field of a variable or choice and hides it again; Undo brings it back.
remove_field_button <- function(row, field, names) {
  what <- paste("Take away", tolower(names[[field]]))
  review_button(
    icon("xmark"), "hide_field",
    `data-id` = row$id, `data-name` = field, class = "btn btn-sm btn-icon field-remove", title = what, `aria-label` = what
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
  # pointing at it, or focusing it, lists the problems, each a link to its block (review.js)
  span(class = "to-fix-note", tabindex = "0", icon("triangle-exclamation"), paste0(" ", plural(n, "block"), " to fix"))
}
