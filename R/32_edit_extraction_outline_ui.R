# The outline of the Edit Extraction page: the questionnaire's blocks as a list, and the picked block's
# details beside it.
# The list is for finding and ordering blocks: each row is its kind, its words, how many variables a question
# has (pointing at the number lists their MATR IDs), what there is to fix, and an arrow that opens it.
# Clicking a section or group opens it, or closes it when it is open. A section's or text block's
# words are typed in the list once its edit button is pressed, and pointing at its row offers that, to change
# its kind, and to delete it. A block verified is green, and a section or group once everything in it is. The
# block picked is edited in the panel beside it (`block_detail()`). On a phone the list and the details take
# turns, with Back. A group's row has no words: it only holds blocks.
# The toolbar, in order: + Block (a dialog of the kinds of block), Filter (which blocks the list shows),
# Search, Expand All, Collapse All, and how many blocks have something to fix. Its buttons are only their
# icons on a phone.
extraction_outline_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "outline-panel is-split",
    `data-ns` = ns(""),
    div(
      class = "outline-toolbar",
      review_button(icon_label("add", "Block"), "pick_block", class = "btn btn-outline-dark", title = "Add a block"),
      outline_filter(),
      outline_search(),
      review_button(icon_label("expand", "Expand All"), "expand_all", class = "btn btn-outline-dark", title = "Open every section"),
      review_button(icon_label("collapse", "Collapse All"), "collapse_all", class = "btn btn-outline-dark", title = "Close every section"),
      uiOutput(ns("to_fix"), inline = TRUE)
    ),
    div(
      class = "outline-split",
      div(
        class = "outline-body",
        uiOutput(ns("tree")),
        p(class = "outline-filter-empty", "No blocks to show.")
      ),
      outline_divider(),
      div(
        class = "detail-panel", role = "region", `aria-label` = "The block picked",
        uiOutput(ns("detail"), class = "detail-body")
      )
    )
  )
}

# The divider between the list and the picked row's details: dragging it (or the arrow keys, while it has focus)
# makes the list wider or narrower, and a double click puts it back (www/js/split.js). Make an edit has one too.
outline_divider <- function() {
  div(
    class = "outline-divider", role = "separator", tabindex = "0", `aria-orientation` = "vertical",
    `aria-label` = "Resize the list", title = "Drag to resize the list; double-click to put it back"
  )
}

# The toolbar's search: the list shows only the blocks whose words or variables have what is typed (with the
# sections and groups they are in). The page script does it; it isn't a Shiny input. Make an edit's list has one
# too.
# label: What it searches, for screen readers.
outline_search <- function(label = "Search the blocks' words and MATR IDs") {
  span(
    class = "toolbar-field",
    app_icon("search"),
    tags$input(
      type = "search", class = "form-control outline-search", `data-outline-search` = NA,
      placeholder = "Search", `aria-label` = label
    )
  )
}

# Filter: which blocks the list shows: all of them, those not checked yet, those with something to fix,
# or those staff changed from what the AI read. The page script hides the others, keeping the sections and
# groups they are in (www/js/review.js); it isn't a Shiny input.
outline_filter <- function() {
  span(
    class = "toolbar-field",
    app_icon("filter"),
    tags$select(
      class = "form-select outline-filter", `data-outline-filter` = NA, `aria-label` = "Filter",
      title = "Which blocks the list shows",
      tags$option(value = "all", "Filter"),
      tags$option(value = "unchecked", "Not checked"),
      tags$option(value = "problems", "To fix"),
      tags$option(value = "changed", "Changed from AI")
    )
  )
}

# ------ Picking what to add, in a dialog of boxes ------------------------------------------------------
# + Block and + Variable each open a dialog with a box for each kind, saying what it is for. The boxes are
# data-action buttons like the page's others: the dialog's body carries the module's namespace (data-ns), since
# a dialog is drawn outside the module.

# What each kind of block is for, as Add Block says it (and the prompt, prompts/extraction.md). A kind the
# database has that isn't here is "Any other printed text".
block_kind_descriptions <- c(
  Section = "A part of the questionnaire under a printed heading, holding the blocks printed under it.",
  Grid = "Questions that share a printed lead-in which is part of each one (“In the past 4 weeks, how often” + “have you felt calm?”). Its words are the lead-in; each question's are the rest.",
  `Group` = "Sets apart blocks that belong together with no words of their own: questions with an instruction or definition only they share, or that skip together. Each question is whole on its own.",
  Question = "Asks for one or more answers, each recorded by a variable.",
  Introduction = "Opening words that say what the questionnaire or study is about.",
  Instruction = "Tells respondents how to answer, or says what the questions after it are about.",
  Definition = "Says what a term means, e.g. “A drink is one beer, one glass of wine or one shot of liquor”.",
  `Interviewer Script` = "Words an interviewer reads aloud, or directions for the interviewer.",
  Note = "Any other printed text that belongs to the questionnaire."
)

# A dialog with a box for each choice, each saying what it is for.
# title: The dialog's title.
# boxes: A list of `list(icon, title, text, action, data)`: `data` is a named list of the box's
#   `data-*` values (e.g. `list(type = "Section")`).
# wide: Whether the boxes are squares in a grid (Add Block), rather than one under another.
pick_dialog <- function(title, ns, boxes, wide = FALSE) {
  modalDialog(
    title = title, easyClose = TRUE, footer = modalButton("Cancel"), size = if (wide) "l" else "m",
    div(
      class = c("pick-boxes", if (wide) "is-grid"), `data-ns` = ns(""),
      lapply(boxes, function(box) {
        attributes <- stats::setNames(box$data %||% list(), paste0("data-", names(box$data %||% list())))
        do.call(review_button, c(
          list(
            tagList(app_icon(box$icon), span(class = "pick-box-words", strong(box$title), span(box$text))),
            box$action, class = "btn pick-box"
          ),
          attributes
        ))
      })
    )
  )
}

# + Block: a box for each kind of block, in the order `block_kind_descriptions` has them (kinds it doesn't know
# last).
# component_types: The kinds of block.
add_block_dialog <- function(ns, component_types) {
  kinds <- c(intersect(names(block_kind_descriptions), component_types), setdiff(component_types, names(block_kind_descriptions)))
  pick_dialog("Add a block", ns, lapply(kinds, function(kind) {
    icon_name <- gsub("[^a-z]+", "_", tolower(kind))
    list(
      icon = if (icon_name %in% names(app_icons)) icon_name else "block", title = kind,
      text = if (kind %in% names(block_kind_descriptions)) block_kind_descriptions[[kind]] else block_kind_descriptions[["Note"]],
      action = "add_block", data = list(type = kind)
    )
  }), wide = TRUE)
}

# ------ Outline tree -----------------------------------------------------------
# Each block is a row. A section's (or group's) row sits above its own list, and dropping a
# block into that list puts the block inside it. Only sections and groups have a list, so
# only they can hold blocks (BR53). `view` holds what's selected, which sections and groups are
# collapsed, the problems the checks found, the words to suggest, the optional fields staff have added to
# variables, and the kinds of block there are.

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

# The id of the list of questions every parent-question field offers ("7 · Have you smoked…"). The page
# script keeps it up to date as blocks move (review.js).
parent_list_id <- function(ns) ns("parent-questions")

# The id of the list of places a choice can skip to ("32 · Have you smoked…", "Section · Part B…"), which
# every Skip to field offers. The page script keeps it up to date as blocks move (review.js).
skip_list_id <- function(ns) ns("skip-targets")

# The outline as a tree.
# view: `list(selected, collapsed, opened, problems, suggestions, shown)`: what is selected, which
#   sections and groups are closed, which questions have their panel open, the problems to mark, the words to
#   offer for each kind of block (`draft_service_text_suggestions()`) and the optional fields added to
#   variables that have nothing in them yet (`"v12:label"`; see `variable_card()`).
outline_tree <- function(draft, view, lookups, ns) {
  # every question's row offers the questions before it; they are worked out once for the whole tree
  attr(draft, "outline_questions") <- outline_questions(draft)
  div(
    class = "outline-tree",
    outline_list(draft, section = NULL, view, lookups, ns),
    # The words offered as staff type, one list per kind of block, plus the variables' (answer choices, who
    # they're about and labels), conditions, and the questions and places to skip to
    lapply(names(view$suggestions %||% list()), function(component_type) {
      suggestions(suggest_list_id(ns, component_type), view$suggestions[[component_type]])
    }),
    variable_editor_lists(ns, lookups, draft),
    suggestions(ns("conditions"), unique(c("If yes", "If no", stats::na.omit(draft$components$condition)))),
    suggestions(parent_list_id(ns), attr(draft, "outline_questions")$questions$label),
    suggestions(skip_list_id(ns), skip_targets(draft)$label)
  )
}

# What the page script marks on the outline, so it isn't drawn again for it.
# Drawing the outline takes a second or more (every block's fields and drop-downs), and the browser
# takes as long again to rebuild it. So what a click changes without changing the layout is marked on
# the outline already there instead: which row is selected, which sections and groups are closed, which
# blocks are checked (a section or group is once everything inside it is), each block's problems and the
# fields at fault, and each question's MATR IDs (listed when its count of variables is pointed at).
# problems: From `draft_service_check()`.
# selected: The selected block's id, or `NULL`.
# collapsed: The ids of the closed sections and groups.
# changed: The blocks changed from what the AI read (`draft_service_changed_blocks()`), or `NULL`.
# Returns: A list, sent as the "edit-extraction-marks" message (www/js/review.js).
outline_marks <- function(draft, problems, selected, collapsed, ns, changed = NULL) {
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
    labels = as.list(label_suggestions(draft)),
    label_list = ns("labels"),
    collapsed = as.list(collapsed),
    checked = as.list(checked),
    all_checked = as.list(setdiff(checked, not_all)),
    check_titles = as.list(outline_check_titles),
    changed = if (!is.null(changed)) as.list(changed),
    matr = question_matr_ids(draft)
  )
}

# Each question's variables' MATR IDs, one line each in their order ("No MATR ID yet" for one without), named
# by the question's id.
question_matr_ids <- function(draft) {
  boxes <- draft$variables[order(draft$variables$component_id, draft$variables$display_order), , drop = FALSE]
  if (nrow(boxes) == 0) {
    return(stats::setNames(list(), character()))
  }
  ids <- ifelse(is.na(boxes$matr_id) | !nzchar(trimws(boxes$matr_id)), "No MATR ID yet", boxes$matr_id)
  as.list(vapply(split(ids, boxes$component_id), paste, character(1), collapse = "\n"))
}

# What to change on the outline already drawn, instead of drawing it again.
# The outline was drawn from `drawn`, and changed since only as the page script changes it (words
# typed, blocks dragged, marks). This compares it with `draft` and says what is different: the blocks
# to take out and to add (only the outermost: a section comes with what is in it), the rows whose
# words or counts changed, every list's blocks in order (so a block that moved is moved, and a drag the draft refused is put back),
# each block's display order and label, each question's number, and the questions in printed order
# (which every parent-question field offers). When much has changed (another draft, say), drawing the
# outline again is quicker, and this says so.
# drawn: The draft as the outline on the page shows it, or `NULL` if it isn't drawn yet.
# draft: The draft as it is now.
# most: How many blocks and rows may be drawn before drawing it all is quicker.
# rows: Rows (by block id) to draw again even if their draft hasn't changed: a field there shows what
#   the draft refused.
# Returns: A list, sent as the "edit-extraction-patch" message (www/js/review.js), or `NULL` to draw
#   the outline again.
outline_patch <- function(drawn, draft, view, lookups, ns, most = 30, rows = NULL) {
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
  if (length(added) + length(rows) > most) {
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
# number, parent question (as its list names it) and condition, how many blocks it holds, and how many
# variables it has (its count badge).
# What the page script keeps up to date on its own (selected, checked, problems, numbers, display order)
# is left out.
outline_row_signature <- function(draft) {
  blocks <- draft$components
  n_inside <- tabulate(match(blocks$parent_id, blocks$id), nbins = nrow(blocks))
  questions <- (attr(draft, "outline_questions") %||% outline_questions(draft))$questions
  parent <- questions$label[match(blocks$parent_question_id, questions$id)]
  n_boxes <- tabulate(match(draft$variables$component_id, blocks$id), nbins = nrow(blocks))
  stats::setNames(
    paste(blocks$component_type, blocks$text, blocks$printed_number, parent, blocks$condition, n_inside, n_boxes, sep = "\r"),
    blocks$id
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
  div(
    # a group, or a grid, is drawn as a gold box around the blocks it holds (_outline.scss)
    class = c(
      "outline-node", if (block$component_type == "Group") "is-group", if (block$component_type == "Grid") "is-grid",
      if (block$id %in% view$collapsed) "is-collapsed"
    ),
    `data-id` = block$id,
    `data-label` = block_label(block),
    `data-page` = if (isTRUE(block$page > 0)) block$page, # Open PDF opens at it (review.js)
    outline_row(block, draft, view, ns),
    if (is_container(block$component_type)) outline_list(draft, block, view, lookups, ns)
  )
}

# A row: its kind, its words, how many variables a question has and what there is to fix, and the arrow that
# opens it in the details (a question's or text block's, and a group's, whose details hold its skip). Clicking a
# section's or group's row opens or closes it. A section's and a
# text block's words are typed in the row once its edit button is pressed (a question's in its details), and
# pointing at the row puts its buttons at its end: edit, change its kind, ungroup, delete. The numbers the
# site works out (display order, place among the questions) order the blocks but aren't shown.
outline_row <- function(block, draft, view, ns) {
  type <- block$component_type
  is_section <- is_container(type)
  div(
    class = c(
      "outline-row", if (identical(block$id, view$selected)) "is-selected", if (block$checked) "is-verified",
      if (is_section) "is-container", if (type != "Question" && type != "Group") "is-editable"
    ),
    tabindex = "0", `data-action` = if (is_section) "toggle" else "select", `data-id` = block$id,
    `aria-expanded` = if (is_section) if (block$id %in% view$collapsed) "false" else "true",
    span(class = "drag-handle", title = "Drag to move", app_icon("drag")),
    type_chip(type),
    # typed in once the row's edit button is pressed (review.js)
    if (type == "Group") span(class = "outline-group-note") else outline_text_field(block, view$problems, ns) |> tagAppendAttributes(readonly = NA),
    outline_badges(block, draft, view$problems),
    if (type != "Question") row_actions(block, view$component_types),
    if (!is_section || type %in% skipping_types) {
      review_button(
        app_icon("open"), "select",
        `data-id` = block$id, class = "btn btn-sm btn-icon btn-open", title = "Open in the editor", `aria-label` = "Open in the editor"
      )
    }
  )
}

# The block's words, typed where they are. The words of the other blocks of the same kind, and of the
# recorded questionnaire's, are offered as they are typed (review.js reads the list `data-suggest` names).
outline_text_field <- function(block, problems, ns) {
  placeholder <- switch(block$component_type,
                        Section = paste(
                          "Section heading, as printed (e.g. ‘Section 1: About You’).",
                          "Blocks with no printed heading go in a Group instead."
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

# What is said about the block at the end of its row: how many variables a question has, and what there is to
# fix. Pointing at the number lists the variables' MATR IDs, which the page script keeps up to date as they are
# typed (outline_marks()). The problems badge is kept up to date by the page script as the checks run, so its
# count is drawn as the script expects (data-problems).
outline_badges <- function(block, draft, problems) {
  block_problems <- problems$message[problems$id == block$id]
  n <- if (block$component_type == "Question") sum(draft$variables$component_id %in% block$id)
  span(
    class = "outline-badges",
    if (!is.null(n)) row_badge(n, class = "outline-count", tabindex = "0", `aria-label` = plural(n, "variable")),
    problem_badge(block_problems)
  )
}


# The badge of a row that says how much there is to fix: a warning sign and the number. Pointing at it lists
# the problems (review.js). It is drawn even when there is nothing to fix (then hidden), so the page script
# can show it without redrawing the row.
# messages: The block's problems, as sentences.
problem_badge <- function(messages) {
  n <- length(messages)
  row_badge(
    app_icon("problem"), span(class = "problem-count", n),
    class = c("is-warning", "outline-problems", if (n == 0) "is-hidden"),
    # listed as the badge is pointed at or focused (review.js)
    tabindex = "0", `aria-label` = paste(n, "to fix"), `data-problems` = paste(messages, collapse = "\n")
  )
}

# What the check button says, checked or not. The page script says it too, when it marks a block
# as checked without the outline being drawn again (outline_marks()).
outline_check_titles <- c(
  checked = "Checked. Click to mark as not checked.",
  unchecked = "Mark as checked and go to the next block. If you change it afterwards, it needs checking again."
)

# The buttons a section's, group's or text block's row shows when it is pointed at, at its end: edit its words
# in the list (not a group's, which has none: the page script does it), change its kind (to any kind
# but a question), ungroup a section or group keeping what's inside, and delete. A block is verified in its
# details; a section or group is checked once everything in it is (draft_service_check_containers()).
row_actions <- function(block, component_types = NULL) {
  what <- tolower(block$component_type)
  ungroup <- if (block$component_type == "Section") "Remove this heading and keep what's inside" else "Ungroup: keep what's inside"
  span(
    class = "outline-actions",
    if (block$component_type != "Group") {
      tags$button(
        type = "button", class = "btn btn-sm btn-icon", `data-edit-words` = NA,
        title = paste("Edit the", what, "words"), `aria-label` = paste("Edit the", what, "words"),
        app_icon("edit")
      )
    },
    kind_menu(block, component_types),
    if (is_container(block$component_type)) {
      review_button(
        app_icon("ungroup"), "ungroup",
        `data-id` = block$id, class = "btn btn-sm btn-icon", title = ungroup, `aria-label` = ungroup
      )
    },
    review_button(
      app_icon("delete"), "delete",
      `data-table` = "components", `data-id` = block$id,
      class = "btn btn-sm btn-icon", title = paste("Delete this", what), `aria-label` = paste("Delete this", what)
    )
  )
}

# A menu that changes a block to another kind: any kind there is but a question, and its own.
kind_menu <- function(block, component_types) {
  kinds <- setdiff(component_types, c("Question", block$component_type))
  if (length(kinds) == 0) {
    return(NULL)
  }
  div(
    class = "dropdown kind-menu",
    tags$button(
      type = "button", class = "btn btn-sm btn-icon dropdown-toggle", `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      title = "Change it to another kind of block", `aria-label` = "Change it to another kind of block",
      app_icon("change_kind")
    ),
    tags$ul(
      class = "dropdown-menu dropdown-menu-end",
      tags$li(tags$h6(class = "dropdown-header", "Change to")),
      lapply(kinds, function(kind) {
        tags$li(review_button(kind, "edit_kind", `data-id` = block$id, `data-type` = kind, class = "dropdown-item"))
      })
    )
  )
}

# ------ A question's variables ---------------------------------------------------------------
# In its details: its variables and their choices, and what the checks found that isn't one field's (a field
# at fault is outlined in red instead).
question_panel <- function(block, draft, view, lookups, ns) {
  of_block <- view$problems[view$problems$id == block$id, , drop = FALSE]
  messages <- of_block$message[is.na(of_block$table)]
  div(
    class = "question-panel",
    if (length(messages) > 0) div(class = "callout details-problems", tags$ul(lapply(messages, tags$li))),
    variable_cards(block, draft, view, lookups, ns)
  )
}

# ------ The picked block's details, beside the list ------------------------------------------------

# The details of the block picked in the list, edited beside it.
# The header is one row: the block's kind, its words (typed there; a group has none) and what there
# is to fix. Then, for a question, the number printed beside it, its skip logic (`skip_logic()`), its variables
# and their choices (`question_panel()`), and its categories and measurement scales (coming); for a section or group, how many
# blocks it holds, and for a group its skip (`group_skip()`). Its buttons are along the bottom (`detail_actions()`). Fields have a small label above
# them. They are the list's own (the same `data-field`s), so the page script keeps the row and the details in
# step as words are typed.
# block: One row of a draft's `components`, or `NULL` when nothing is picked.
block_detail <- function(block, draft, view, lookups, ns) {
  if (is.null(block) || nrow(block) != 1) {
    return(div(class = "detail-empty", "Pick a block in the list to see and edit it here."))
  }
  attr(draft, "outline_questions") <- outline_questions(draft)
  type <- block$component_type
  problems <- view$problems
  block_problems <- problems$message[problems$id == block$id]
  div(
    class = "block-detail", `data-id` = block$id,
    detail_head(
      type_chip(type),
      if (type == "Group") {
        h2(class = "detail-group-title", "Group")
      } else {
        outline_text_field(block, problems, ns) |> tagAppendAttributes(class = "detail-words")
      },
      problem_badge(block_problems) |> tagAppendAttributes(`data-id` = block$id)
    ),
    div(
      class = "detail-content",
      if (type == "Question") {
        tagList(
          detail_section(
            NULL,
            detail_field(tagList(app_icon("question_number"), " Question Number"), field_text(
              "components", block, "printed_number",
              placeholder = "None", label = "Question number as printed, e.g. 1a4 (blank if none is printed)", problems = problems
            ) |> tagAppendAttributes(class = "detail-number"))
          ),
          detail_section("Skip Logic", skip_logic(block, draft, ns)),
          detail_section("Variables", question_panel(block, draft, view, lookups, ns)),
          # the question's MeSH categories and the measurement scales it is an item of, to come
          detail_section("Categories & Measurement Scales", p(class = "detail-note", "Coming soon!"))
        )
      } else if (is_container(type)) {
        tagList(
          detail_section(NULL, detail_field("Holds", span(class = "detail-value", plural(sum(draft$components$parent_id %in% block$id), "block")))),
          detail_section("Who answers it", detail_field("Only for", field_text(
            "components", block, "condition",
            placeholder = "Everyone", list = ns("conditions"),
            label = "Who answers everything in it, e.g. female (blank for everyone)", problems = problems
          ))),
          if (type %in% skipping_types) detail_section("Skip", group_skip(block, draft, problems, ns))
        )
      }
    ),
    detail_actions(block, block_problems)
  )
}

# A question's skip conditions and logic: who sees it, in sentences.
# - Shown only for some answers to an earlier question (its parent): "This question is only shown to
#  respondents if they answered **“Yes”** to **“Do you smoke?”**", a link to that question. Picked with Add
#  Skip Logic, the answers are the ones picked (its condition); otherwise they are worked out from the
#  parent's Skip tos (on a choice, or a write-in): the ones that don't skip past it. With neither, it reads
#  "depending on how they answered".
# - Shown only to some respondents: "This question is only shown to respondents who are:
#  **Twins**" (its condition, when it has no parent picked).
# - Skipped by a group's skip: "Respondents who answer **“No”** to every question in **the group with
#  questions 22a–22d** skip this question", a link to the group (`draft_service_group_skipped()`).
# The question is named by its words, in full, and they and the answers are in quotes. A sentence staff added
# has a delete button that takes it away; one worked out from a Skip to goes when the Skip to does. More is
# added with + Skip Logic, at the bottom of the details.
# block: One row of a draft's `components`: a question.
# draft: A draft, with its `outline_questions` attribute.
skip_logic <- function(block, draft, ns) {
  blocks <- draft$components
  derived <- isTRUE(block$parent_from_skip)
  has_parent <- !is.na(block$parent_question_id)
  sentences <- list(
    if (has_parent) {
      # the answers picked, or else those the parent's skips leave to reach it
      answers <- if (derived || is.na(block$condition)) skip_answers(draft, block$id, block$parent_question_id) else condition_answers(block$condition)
      parent <- blocks[blocks$id == block$parent_question_id, , drop = FALSE]
      words <- quoted(blank_if_na(parent$text))
      skip_sentence(
        review_button(
          # which answers isn't known for a parent the AI read without them
          if (is.null(answers)) {
            tagList("This question is only shown to respondents depending on how they answered ", strong(words))
          } else {
            tagList("This question is only shown to respondents if they answered ", strong(answers), " to ", strong(words))
          },
          "select", `data-id` = parent$id, class = "btn btn-link skip-link", title = "Go to that question"
        ),
        if (derived) span(class = "detail-note", "from a Skip to in that question") else remove_logic_button(block, "answer")
      )
    },
    if (!is.na(block$condition) && (!has_parent || derived)) {
      skip_sentence(
        span(class = "skip-text", "This question is only shown to respondents who are: ", strong(block$condition)),
        remove_logic_button(block, "condition")
      )
    }
  )
  # for whoever the sections, grids and groups it is in are for
  for (holder in rev(draft_service_ancestors(draft, block$id))) {
    held_by <- blocks[blocks$id == holder, , drop = FALSE]
    if (is.na(held_by$condition)) next
    sentences[[length(sentences) + 1]] <- skip_sentence(review_button(
      tagList("Everything in ", strong(container_name(draft, held_by)), " is only shown to respondents who are: ", strong(held_by$condition)),
      "select", `data-id` = holder, class = "btn btn-link skip-link", title = "Go to it"
    ))
  }
  # skipped by a group's skip, on the answers to the questions in it
  by <- draft_service_group_skipped(draft)
  group <- blocks[blocks$id %in% by$group[by$question == block$id], , drop = FALSE]
  if (nrow(group) == 1) {
    sentences[[length(sentences) + 1]] <- skip_sentence(
      review_button(
        tagList(
          "Respondents who answer ", strong(if (is.na(group$skip_condition)) "as it says" else quoted(group$skip_condition)),
          " to ", if (identical(group$skip_when, "any")) "any" else "every", " question in ", strong(group_name(draft, group$id)),
          " skip this question"
        ),
        "select", `data-id` = group$id, class = "btn btn-link skip-link", title = "Go to that group"
      ),
      span(class = "detail-note", "from the Skip in that group")
    )
  }
  sentences <- Filter(Negate(is.null), sentences)
  div(
    class = "skip-logic is-wide",
    if (length(sentences) == 0) p(class = "detail-note", "Every respondent sees this question.") else sentences
  )
}

skip_sentence <- function(...) div(class = "skip-sentence", ...)

# A group's skip, on the answers to the questions in it, in its details: "If they answer [No] to [every
# question] in this group, skip to [28 · …]", then the routing as printed. Left empty, the group doesn't skip.
# Where it skips to is picked like a choice's (`skip_fields()`); "every question" is what an empty
# `skip_when` means.
# block: One row of a draft's `components`: a group.
# draft: A draft, with its `outline_questions` attribute.
# problems: The checks' problems, to outline a field at fault.
group_skip <- function(block, draft, problems, ns) {
  tagList(
    detail_field(
      "Skips when", wide = TRUE,
      div(
        class = "variable-skip group-skip",
        span(class = "variable-skip-words", "If they answer"),
        field_text(
          "components", block, "skip_condition",
          placeholder = "e.g. No", list = ns("answer-options"), label = "The answer that skips, e.g. No", problems = problems
        ),
        span(class = "variable-skip-words", "to"),
        field_select(
          "components", block, "skip_when", c(`any question` = "any", `every question` = "every"),
          blank = "every question", label = "Every question in the group, or any", problems = problems
        ),
        span(class = "variable-skip-words", "in this group, skip to"),
        skip_fields(block, problems, ns, skip_targets(draft), table = "components")
      )
    ),
    detail_field(
      "As printed", wide = TRUE,
      field_text(
        "components", block, "skip_instruction",
        placeholder = "e.g. If you answered No to items 22a-d, go to Question 28", label = "The skip as printed", problems = problems
      )
    )
  )
}

# A section, grid or group in a sentence: the section “part a: your health”, the grid “during the worst time:”, or
# a group by the questions in it (group_name()).
container_name <- function(draft, block) {
  if (block$component_type == "Group" || is.na(block$text)) {
    return(group_name(draft, block$id))
  }
  paste0("the ", tolower(block$component_type), " ", quoted(shorten(block$text, 60)))
}

# A group by the questions in it: "the group with questions 22a–22d", "the group with question 5", or "the group".
group_name <- function(draft, id) {
  inside <- draft_service_printed_order(draft, id)
  questions <- draft$components[match(inside, draft$components$id), , drop = FALSE]
  numbers <- draft_service_question_number(questions[questions$component_type %in% "Question", , drop = FALSE])
  numbers <- numbers[!is.na(numbers) & nzchar(numbers)]
  if (length(numbers) == 0) {
    return("the group")
  }
  if (length(numbers) == 1) paste("the group with question", numbers) else paste0("the group with questions ", numbers[1], "\u2013", numbers[length(numbers)])
}

# What a group's skip says, in a sentence: "If they answer “no” to every question in this group, they skip to
# 28 · Have you…". `targets` names where it goes (`skip_targets()`).
group_skip_text <- function(block, targets) {
  answer <- if (is.na(block$skip_condition)) "the answer that skips" else quoted(block$skip_condition)
  when <- if (identical(block$skip_when, "any")) "any" else "every"
  to <- targets$label[match(block$skip_to_id, targets$id)]
  paste0(
    "If they answer ", answer, " to ", when, " question in this group, they skip to ",
    if (is.na(to)) "(where isn't picked yet)" else to
  )
}

# Words in curly quotes: “Yes”. None gives none (recycle0), not an empty pair of quotes.
quoted <- function(words) paste0("“", words, "”", recycle0 = TRUE)

remove_logic_button <- function(block, kind) {
  review_button(
    app_icon("delete"), "remove_logic", `data-id` = block$id, `data-kind` = kind,
    class = "btn btn-sm btn-icon field-remove", title = "Take this skip logic away", `aria-label` = "Take this skip logic away"
  )
}

# The answers a question's condition names when staff picked them ("Yes or Sometimes"), each in quotes; a
# printed "If yes" reads “yes”. NULL when it names none.
condition_answers <- function(condition) {
  if (is.na(condition)) NULL else paste(quoted(condition_answer_list(condition)), collapse = " or ")
}

# The answers a condition names, without quotes: "Yes or Sometimes" is "Yes" and "Sometimes".
condition_answer_list <- function(condition) {
  if (is.na(condition)) character() else strsplit(sub("(?i)^if\\s+", "", trimws(condition), perl = TRUE), " or ", fixed = TRUE)[[1]]
}

# The answers to `parent` that show `id`, worked out from the skips on `parent`: of each variable whose choices
# skip past `id`, the choices that don't; of each write-in that does, anything but the answer that skips.
# id: The question shown only for some answers.
# parent: The question with the skips.
# Returns: Text such as "“Yes”" or "“Yes” or “Sometimes”" or "anything but “None”", or
#   `NULL` when none can be said.
skip_answers <- function(draft, id, parent) {
  printed <- draft_service_printed_order(draft)
  place <- match(id, printed)
  past <- function(to) ifelse(to == skip_to_end_id, TRUE, match(to, printed) > place) %in% TRUE
  skips <- draft_service_skips(draft)
  skips <- skips[skips$source %in% parent & past(skips$to), , drop = FALSE]
  answers <- character()
  for (box in unique(skips$variable_id[skips$table == "response_options"])) {
    choices <- draft_service_children(draft, "response_options", box)
    stays <- is.na(choices$skip_to_id) | !past(choices$skip_to_id)
    answers <- c(answers, quoted(choices$answer_option[stays & !is.na(choices$answer_option)]))
  }
  writes <- skips[skips$table == "variables" & !is.na(skips$skip_condition), , drop = FALSE]
  answers <- unique(c(answers, paste0("anything but ", quoted(writes$skip_condition), recycle0 = TRUE)))
  if (length(answers) == 0) NULL else paste(answers, collapse = " or ")
}

# The details' buttons, along the bottom, the same size and evenly spaced: for a question Verify, + Skip
# Logic, + Variable and Delete; for another block Verify and Delete; for a section or group Ungroup and
# Delete. Verify marks it as checked and goes to the next block (the page script keeps it up to date as the
# checks run); it waits until nothing is left to fix (it says so when pointed at). A section or group has no
# Verify: it is checked once everything in it is. On a phone they are only their icons.
# block: One row of a draft's `components`.
# block_problems: Its problems, as sentences.
detail_actions <- function(block, block_problems) {
  type <- tolower(block$component_type)
  blocked <- !block$checked && length(block_problems) > 0
  button <- function(icon_name, text, action, ..., class = "btn-outline-dark") {
    review_button(icon_label(icon_name, text), action, `data-id` = block$id, ..., class = c("btn", class))
  }
  div(
    class = "detail-actions",
    if (!is_container(block$component_type)) {
      span(
        class = "verify-wrap", title = if (blocked) "Fix what's marked before verifying",
        review_button(
          tagList(app_icon("verify"), span(class = "btn-text verify-label", if (block$checked) "Verified" else "Verify")), "check",
          `data-id` = block$id, disabled = blocked,
          class = c("btn btn-outline-dark detail-verify", if (block$checked) "is-checked"),
          title = outline_check_titles[[if (block$checked) "checked" else "unchecked"]]
        )
      )
    },
    if (block$component_type == "Question") {
      tagList(
        button("add", "Skip Logic", "add_skip_logic", title = "Add skip logic: who sees this question"),
        button("add", "Variable", "pick_variable", title = "Add a variable")
      )
    },
    if (is_container(block$component_type)) {
      button(
        "ungroup", if (block$component_type == "Section") "Remove heading, keep contents" else "Ungroup", "ungroup",
        title = "Take it away but keep what's inside"
      )
    },
    review_button(
      icon_label("delete", "Delete"), "delete",
      `data-table` = "components", `data-id` = block$id,
      class = "btn btn-delete detail-delete", title = paste("Delete this", type)
    )
  )
}

skip_logic_choice_modal <- function(ns) {
  modalDialog(
    title = "Add Skip Logic", easyClose = TRUE, footer = modalButton("Cancel"),
    div(
      class = "logic-choices",
      actionButton(
        ns("logic_answer"), class = "logic-choice",
        label = tagList(
          app_icon("skip_answer"),
          span("This question is only shown to the respondent if they answered another question with a specific answer")
        )
      ),
      actionButton(
        ns("logic_condition"), class = "logic-choice",
        label = tagList(
          app_icon("skip_respondent"),
          span("This question is only shown to the respondent if they meet a specific condition or characteristic (e.g., “Female”, “Twins”)")
        )
      )
    )
  )
}

# Shown only for some answers to an earlier question: the question, picked from those before it, then its answers.
skip_logic_answer_modal <- function(ns, draft, id) {
  questions <- outline_questions(draft)$questions
  place <- questions$position[questions$id == id]
  earlier <- questions[questions$position < place, , drop = FALSE]
  block <- draft$components[draft$components$id == id, , drop = FALSE]
  current <- if (!is.na(block$parent_question_id) && !isTRUE(block$parent_from_skip)) block$parent_question_id else ""
  confirm_dialog(
    "Shown only for some answers",
    selectizeInput(
      ns("logic_parent"), "The earlier question",
      choices = c("Pick a question" = "", stats::setNames(earlier$id, earlier$label)), selected = current, width = "100%"
    ),
    uiOutput(ns("logic_answers")),
    confirm_id = ns("logic_answer_save"), confirm_label = "Save", keep_label = "Cancel", easy_close = TRUE
  )
}

# The answers of the earlier question picked: its choices to tick, or, for a question without choices, the
# answer typed. Those staff picked before are ticked again.
skip_logic_answer_choices <- function(ns, draft, id, parent) {
  if (is.null(parent) || !nzchar(parent)) {
    return(p(class = "detail-note", "Pick the earlier question first."))
  }
  answers <- skip_logic_parent_answers(draft, parent)
  block <- draft$components[draft$components$id == id, , drop = FALSE]
  before <- if (identical(block$parent_question_id, parent)) condition_answer_list(block$condition) else character()
  if (length(answers) > 0) {
    checkboxGroupInput(
      ns("logic_answer_picks"), "The answers that show this question",
      choices = answers, selected = answers[tolower(answers) %in% tolower(before)]
    )
  } else {
    textInput(ns("logic_answer_text"), "The answer that shows this question", value = paste(before, collapse = " or "), placeholder = "e.g. More than 5")
  }
}

# The choices an earlier question offers, each once, in printed order.
skip_logic_parent_answers <- function(draft, parent) {
  boxes <- draft_service_children(draft, "variables", parent)
  choices <- draft$response_options[draft$response_options$variable_id %in% boxes$id, , drop = FALSE]
  choices <- choices[order(match(choices$variable_id, boxes$id), choices$display_order), , drop = FALSE]
  unique(choices$answer_option[!is.na(choices$answer_option)])
}

# Shown only to some respondents: who they are, in a word or two ("Twins", "Female", "Parents").
skip_logic_condition_modal <- function(ns, current = NA) {
  confirm_dialog(
    "Shown only to some respondents",
    textInput(
      ns("logic_condition_text"), "This question is only shown to respondents who are:",
      value = blank_if_na(current), placeholder = "e.g. Twins, Female, Parents", width = "100%"
    ),
    p(class = "detail-note", "A question picked as the one this question follows is taken away: a question has one condition."),
    confirm_id = ns("logic_condition_save"), confirm_label = "Save", keep_label = "Cancel", easy_close = TRUE
  )
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
      # recycle0: a draft with no questions yet has no labels, not one made of the separator
      label = paste0(ifelse(is.na(numbers), "?", numbers), outline_number_separator, shorten(questions$text, 50), recycle0 = TRUE),
      stringsAsFactors = FALSE
    )
  )
}

# What the checks found, as a count for the toolbar.
# problems: The result of `draft_service_check()`.
# Returns: A UI element, or `NULL` when there is nothing to fix.
to_fix_note <- function(problems) {
  n <- length(unique(problems$id))
  if (n == 0) {
    return(span(class = "to-fix-note is-clear", app_icon("done"), " Nothing to fix"))
  }
  # pointing at it, or focusing it, lists the problems, each a link to its block (review.js)
  span(class = "to-fix-note", tabindex = "0", app_icon("problem"), paste0(" ", plural(n, "block"), " to fix"))
}
