#' The outline of the Edit Extraction page: the questionnaire's blocks as a list, and the picked block's
#' details beside it.
#'
#' The list is for finding and ordering blocks: each row is its kind, its words, how many variables a question
#' has (pointing at the number lists their MATR IDs), what there is to fix, and an arrow that opens it.
#' Clicking a section or question group opens it, or closes it when it is open. A section's or text block's
#' words are typed in the list once its edit button is pressed, and pointing at its row offers that, to change
#' its kind, and to delete it. A block verified is green, and a section or group once everything in it is. The
#' block picked is edited in the panel beside it (`block_detail()`). On a phone the list and the details take
#' turns, with Back. A question group's row has no words: it only holds blocks.
#'
#' The toolbar, in order: + Block (a dialog of the kinds of block), Add Filter (which blocks the list shows),
#' Search, Expand All, Collapse All, and how many blocks have something to fix. Its buttons are only their
#' icons on a phone.
#'
#' @param id The module's id.
#' @return The page's UI.
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
      div(
        class = "detail-panel", role = "region", `aria-label` = "The block picked",
        uiOutput(ns("detail"), class = "detail-body")
      )
    )
  )
}

#' The toolbar's search: the list shows only the blocks whose words or variables have what is typed (with the
#' sections and groups they are in). The page script does it; it isn't a Shiny input.
#'
#' @return A UI element.
outline_search <- function() {
  span(
    class = "toolbar-field",
    app_icon("search"),
    tags$input(
      type = "search", class = "form-control outline-search", `data-outline-search` = NA,
      placeholder = "Search", `aria-label` = "Search the blocks' words and MATR IDs"
    )
  )
}

#' Add Filter: which blocks the list shows: all of them, those not checked yet, those with something to fix,
#' or those staff changed from what the AI read. The page script hides the others, keeping the sections and
#' question groups they are in (www/js/review.js); it isn't a Shiny input.
#'
#' @return A UI element.
outline_filter <- function() {
  span(
    class = "toolbar-field",
    app_icon("filter"),
    tags$select(
      class = "form-select outline-filter", `data-outline-filter` = NA, `aria-label` = "Add a filter",
      title = "Which blocks the list shows",
      tags$option(value = "all", "Add Filter"),
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
  `Question Group` = "Sets apart blocks that belong together and have no heading: questions under a lead-in only they share, or a question with its follow-up.",
  Question = "Asks for one or more answers, each recorded by a variable.",
  Introduction = "Opening words that say what the questionnaire or study is about.",
  Instruction = "Tells respondents how to answer, or is the lead-in the items after it share.",
  Definition = "Says what a term means, e.g. “A drink is one beer, one glass of wine or one shot of liquor”.",
  `Interviewer Script` = "Words an interviewer reads aloud, or directions for the interviewer.",
  Note = "Any other printed text that belongs to the questionnaire."
)

#' A dialog with a box for each choice, each saying what it is for.
#'
#' @param title The dialog's title.
#' @param ns The module's namespace function.
#' @param boxes A list of `list(icon, title, text, action, data)`: `data` is a named list of the box's
#'   `data-*` values (e.g. `list(type = "Section")`).
#' @param wide Whether the boxes are squares in a grid (Add Block), rather than one under another.
#' @return A `modalDialog()`.
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

#' + Block: a box for each kind of block, in the order `block_kind_descriptions` has them (kinds it doesn't know
#' last).
#'
#' @param ns The module's namespace function.
#' @param component_types The kinds of block.
#' @return A `modalDialog()`.
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

#' + Variable: choices or a write-in, for the question `id`.
#'
#' @param ns The module's namespace function.
#' @param id The question's id.
#' @return A `modalDialog()`.
add_variable_dialog <- function(ns, id) {
  pick_dialog("Add a variable", ns, list(
    list(
      icon = "choices", title = "Choices", action = "add_box", data = list(id = id, type = "closed_ended"),
      text = "The respondent marks one of the printed choices: boxes, bubbles or numbers to circle, each with its code."
    ),
    list(
      icon = "write_in", title = "Write-in", action = "add_box", data = list(id = id, type = "open_ended"),
      text = "The respondent writes the answer: a number, words, a date or a time on a line or in boxes."
    )
  ))
}

# ------ Outline tree -----------------------------------------------------------
# Each block is a row. A section's (or question group's) row sits above its own list, and dropping a
# block into that list puts the block inside it. Only sections and question groups have a list, so
# only they can hold blocks (BR53). `view` holds what's selected, which sections and groups are
# collapsed, the problems the checks found, the words to suggest, the uploaded MATR IDs no variable has
# yet, the optional fields staff have added to variables, and the kinds of block there are.

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
#' blocks are checked (a section or group is once everything inside it is), each block's problems and the
#' fields at fault, each question's MATR IDs (listed when its count of variables is pointed at), and the
#' uploaded MATR IDs no variable has yet.
#'
#' @param draft A draft.
#' @param problems From `draft_service_check()`.
#' @param selected The selected block's id, or `NULL`.
#' @param collapsed The ids of the closed sections and groups.
#' @param matr_ids The uploaded MATR IDs no variable has yet.
#' @param ns The module's namespace function.
#' @return A list, sent as the "edit-extraction-marks" message (www/js/review.js).
#' @param changed The blocks changed from what the AI read (`draft_service_changed_blocks()`), or `NULL`.
outline_marks <- function(draft, problems, selected, collapsed, matr_ids, ns, changed = NULL) {
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

#' What to change on the outline already drawn, instead of drawing it again.
#'
#' The outline was drawn from `drawn`, and changed since only as the page script changes it (words
#' typed, blocks dragged, marks). This compares it with `draft` and says what is different: the blocks
#' to take out and to add (only the outermost: a section comes with what is in it), the rows whose
#' words or counts changed, every list's blocks in order (so a block that moved is moved, and a drag the draft refused is put back),
#' each block's display order and label, each question's number, and the questions in printed order
#' (which every parent-question field offers). When much has changed (another draft, say), drawing the
#' outline again is quicker, and this says so.
#'
#' @param drawn The draft as the outline on the page shows it, or `NULL` if it isn't drawn yet.
#' @param draft The draft as it is now.
#' @param view What the outline shows; see `outline_tree()`.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param ns The module's namespace function.
#' @param most How many blocks and rows may be drawn before drawing it all is quicker.
#' @param rows Rows (by block id) to draw again even if their draft hasn't changed: a field there shows what
#'   the draft refused.
#' @return A list, sent as the "edit-extraction-patch" message (www/js/review.js), or `NULL` to draw
#'   the outline again.
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
# number, parent question (as its list names it) and condition, how many blocks it holds, and its
# variables' summary line.
# What the page script keeps up to date on its own (selected, checked, problems, numbers, display order)
# is left out.
outline_row_signature <- function(draft) {
  blocks <- draft$components
  n_inside <- tabulate(match(blocks$parent_id, blocks$id), nbins = nrow(blocks))
  questions <- (attr(draft, "outline_questions") %||% outline_questions(draft))$questions
  parent <- questions$label[match(blocks$parent_question_id, questions$id)]
  # the question's variables, as its summary line shows them
  boxes <- draft$variables[order(draft$variables$component_id, draft$variables$display_order), , drop = FALSE]
  box_text <- paste(boxes$matr_id, variable_summary_text(draft, boxes$id))
  summary <- vapply(split(box_text, factor(boxes$component_id, levels = blocks$id)), paste, character(1), collapse = "\n")
  stats::setNames(
    paste(blocks$component_type, blocks$text, blocks$printed_number, parent, blocks$condition, n_inside, summary, sep = "\r"),
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
    class = c("outline-node", if (block$id %in% view$collapsed) "is-collapsed"),
    `data-id` = block$id,
    `data-label` = block_label(block),
    `data-page` = if (isTRUE(block$page > 0)) block$page, # Open PDF opens at it (review.js)
    outline_row(block, draft, view, ns),
    if (is_container(block$component_type)) outline_list(draft, block, view, lookups, ns)
  )
}

# A row: its kind, its words, how many variables a question has and what there is to fix, and the arrow that
# opens it in the details. Clicking a section's or question group's row opens or closes it. A section's and a
# text block's words are typed in the row once its edit button is pressed (a question's in its details), and
# pointing at the row puts its buttons at its end: edit, change its kind, ungroup, delete. A question's
# variables in short are under its row only while MATR IDs are given out by hand (review.js). The numbers the
# site works out (display order, place among the questions) order the blocks but aren't shown.
outline_row <- function(block, draft, view, ns) {
  type <- block$component_type
  is_section <- is_container(type)
  div(
    class = c(
      "outline-row", if (identical(block$id, view$selected)) "is-selected", if (block$checked) "is-verified",
      if (is_section) "is-container", if (type != "Question" && type != "Question Group") "is-editable"
    ),
    tabindex = "0", `data-action` = if (is_section) "toggle" else "select", `data-id` = block$id,
    `aria-expanded` = if (is_section) if (block$id %in% view$collapsed) "false" else "true",
    span(class = "drag-handle", title = "Drag to move", app_icon("drag")),
    type_chip(type),
    # typed in once the row's edit button is pressed (review.js)
    if (type == "Question Group") span(class = "outline-group-note") else outline_text_field(block, view$problems, ns) |> tagAppendAttributes(readonly = NA),
    outline_badges(block, draft, view$problems),
    if (type != "Question") row_actions(block, view$component_types),
    if (!is_section) {
      review_button(
        app_icon("open"), "select",
        `data-id` = block$id, class = "btn btn-sm btn-icon btn-open", title = "Open in the editor", `aria-label` = "Open in the editor"
      )
    },
    if (type == "Question") variable_summary(block, draft, view, ns)
  )
}

#' A question's variables in one line under its row, while its panel is closed.
#'
#' Most questions have one variable: its MATR ID can be typed here, beside its choices in short ("1 Yes · 2
#' No") or its kind of answer, and who it's about and its label when it has them. A question with several
#' variables lists the first few. Clicking the line opens the panel, where everything else is edited.
#'
#' @param block One row of a draft's `components`: a question.
#' @param draft A draft.
#' @param view What the outline shows; see `outline_tree()`.
#' @param ns The module's namespace function.
#' @return A UI element.
variable_summary <- function(block, draft, view, ns) {
  boxes <- draft_service_children(draft, "variables", block$id)
  open <- function(...) {
    review_button(..., "details", `data-id` = block$id, class = "btn btn-sm summary-open", title = "Show the question's variables")
  }
  div(
    class = "outline-row-summary",
    if (nrow(boxes) == 0) {
      open(tags$em("No variables yet"))
    } else if (nrow(boxes) == 1) {
      tagList(
        field_text(
          "variables", boxes, "matr_id",
          placeholder = "MATR ID", list = matr_id_list_id(ns), label = "MATR ID", problems = view$problems
        ) |> tagAppendAttributes(class = "summary-matr-id"),
        open(variable_summary_text(draft, boxes$id))
      )
    } else {
      shown <- head(boxes, 3)
      open(
        strong(plural(nrow(boxes), "variable")), ": ",
        paste(
          paste0(ifelse(is.na(shown$matr_id), "no MATR ID", shown$matr_id), " (", variable_summary_text(draft, shown$id), ")"),
          collapse = "; "
        ),
        if (nrow(boxes) > 3) paste0("; +", nrow(boxes) - 3, " more")
      )
    }
  )
}

# Each variable in a few words: its choices in short or its kind of answer, then who it's about and its label
# when it has them. Named by the variables' ids.
variable_summary_text <- function(draft, ids) {
  boxes <- draft$variables[match(ids, draft$variables$id), , drop = FALSE]
  choices <- draft$response_options[draft$response_options$variable_id %in% ids, , drop = FALSE]
  choices <- choices[order(choices$variable_id, choices$display_order), , drop = FALSE]
  answer <- vapply(seq_len(nrow(boxes)), function(i) {
    if (boxes$variable_type[i] != "closed_ended") {
      return(paste("Write-in:", if (is.na(boxes$answer_data_type[i])) "answer type not picked" else boxes$answer_data_type[i]))
    }
    choice_summary(choices[choices$variable_id == boxes$id[i], , drop = FALSE])
  }, character(1))
  about <- ifelse(is.na(boxes$subject), "", paste0(" · About: ", boxes$subject))
  label <- ifelse(is.na(boxes$label), "", paste0(" · “", shorten(boxes$label, 30), "”"))
  stats::setNames(paste0(answer, about, label), boxes$id)
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


#' The badge of a row that says how much there is to fix: a warning sign and the number. Pointing at it lists
#' the problems (review.js). It is drawn even when there is nothing to fix (then hidden), so the page script
#' can show it without redrawing the row.
#'
#' @param messages The block's problems, as sentences.
#' @return A UI element.
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
# in the list (not a question group's, which has none: the page script does it), change its kind (to any kind
# but a question), ungroup a section or group keeping what's inside, and delete. A block is verified in its
# details; a section or group is checked once everything in it is (draft_service_check_containers()).
row_actions <- function(block, component_types = NULL) {
  what <- tolower(block$component_type)
  ungroup <- if (block$component_type == "Section") "Remove this heading and keep what's inside" else "Ungroup: keep what's inside"
  span(
    class = "outline-actions",
    if (block$component_type != "Question Group") {
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

#' The details of the block picked in the list, edited beside it.
#'
#' The header is one row: the block's kind, its words (typed there; a question group has none) and what there
#' is to fix. Then, for a question, the number printed beside it, its skip conditions and logic
#' (`skip_logic()`), and its variables and their choices (`question_panel()`); for a section or group, how many
#' blocks it holds. Its buttons are along the bottom (`detail_actions()`). Fields have a small label above
#' them. They are the list's own (the same `data-field`s), so the page script keeps the row and the details in
#' step as words are typed.
#'
#' @param block One row of a draft's `components`, or `NULL` when nothing is picked.
#' @param draft A draft.
#' @param view What the outline shows; see `outline_tree()`.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param ns The module's namespace function.
#' @return A UI element.
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
    div(
      class = "detail-head",
      tags$button(type = "button", class = "btn btn-sm btn-link detail-back", `data-detail-back` = NA, app_icon("back"), " List"),
      type_chip(type),
      if (type == "Question Group") {
        h2(class = "detail-group-title", "Question group")
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
          detail_section("Skip Conditions & Logic", skip_logic(block, draft, ns)),
          detail_section("Variables", question_panel(block, draft, view, lookups, ns))
        )
      } else if (is_container(type)) {
        detail_section(NULL, detail_field("Holds", span(class = "detail-value", plural(sum(draft$components$parent_id %in% block$id), "block"))))
      }
    ),
    detail_actions(block, block_problems)
  )
}

# A heading over some of the details' fields, laid out in a grid.
detail_section <- function(heading, ...) {
  div(class = "detail-section", if (!is.null(heading)) h3(heading), div(class = "detail-grid", ...))
}

# One field of the details, with its label above it; `wide` takes the grid's whole width.
detail_field <- function(label, ..., wide = FALSE) {
  div(class = c("detail-field", if (wide) "is-wide"), span(class = "detail-label", label), ...)
}

#' A question's skip conditions and logic: who sees it, in sentences.
#'
#' - Shown only for some answers to an earlier question (its parent): "This question is only shown to
#'   respondents if they answered **“Yes”** to **“Do you smoke?”**", a link to that question. Picked with Add
#'   Skip Logic, the answers are the ones picked (its condition); otherwise they are worked out from the
#'   parent's Skip tos (on a choice, or a write-in): the ones that don't skip past it. With neither, it reads
#'   "depending on how they answered".
#' - Shown only to some respondents: "This question is only shown to respondents who are:
#'   **Twins**" (its condition, when it has no parent picked).
#'
#' The question is named by its words, in full, and they and the answers are in quotes. A sentence staff added
#' has a delete button that takes it away; one worked out from a Skip to goes when the Skip to does. More is
#' added with + Skip Logic, at the bottom of the details.
#'
#' @param block One row of a draft's `components`: a question.
#' @param draft A draft, with its `outline_questions` attribute.
#' @param ns The module's namespace function.
#' @return A UI element.
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
  sentences <- Filter(Negate(is.null), sentences)
  div(
    class = "skip-logic is-wide",
    if (length(sentences) == 0) p(class = "detail-note", "Every respondent sees this question.") else sentences
  )
}

skip_sentence <- function(...) div(class = "skip-sentence", ...)

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

#' The answers to `parent` that show `id`, worked out from the skips on `parent`: of each variable whose choices
#' skip past `id`, the choices that don't; of each write-in that does, anything but the answer that skips.
#'
#' @param draft A draft.
#' @param id The question shown only for some answers.
#' @param parent The question with the skips.
#' @return Text such as "“Yes”" or "“Yes” or “Sometimes”" or "anything but “None”", or
#'   `NULL` when none can be said.
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

#' The details' buttons, along the bottom, the same size and evenly spaced: for a question Verify, + Skip
#' Logic, + Variable and Delete; for another block Verify and Delete; for a section or group Ungroup and
#' Delete. Verify marks it as checked and goes to the next block (the page script keeps it up to date as the
#' checks run); it waits until nothing is left to fix (it says so when pointed at). A section or group has no
#' Verify: it is checked once everything in it is. On a phone they are only their icons.
#'
#' @param block One row of a draft's `components`.
#' @param block_problems Its problems, as sentences.
#' @return A UI element.
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

#' Apply to All?: the questions whose variables offer the same choices as this one but code them
#' differently, which Apply gives these codes.
#'
#' @param ns The module's namespace function.
#' @param draft A draft.
#' @param boxes The ids of the variables that would change (`draft_service_codes_differ()`).
#' @param codes This variable's choices with their codes ("1 = Yes · 2 = No").
#' @return A `modalDialog()`.
apply_codes_dialog <- function(ns, draft, boxes, codes) {
  questions <- outline_questions(draft)$questions
  affected <- unique(draft$variables$component_id[match(boxes, draft$variables$id)])
  labels <- questions$label[match(affected, questions$id)]
  modalDialog(
    title = "Apply these codes to all?", easyClose = TRUE,
    p("Codes: ", strong(codes)),
    p(sprintf("%s the same choices with other codes. Apply gives them these codes:", if (length(affected) == 1) "This question has" else paste(length(affected), "questions have"))),
    tags$ul(class = "apply-codes-list", lapply(labels[!is.na(labels)], tags$li)),
    footer = tagList(modalButton("Cancel"), actionButton(ns("apply_codes"), "Apply", class = "btn-primary"))
  )
}

# ------ Add Skip Logic ---------------------------------------------------------------------
# A dialog that asks which kind of skip logic, then one for each kind (the outline's server saves them).

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
  modalDialog(
    title = "Shown only for some answers", easyClose = TRUE,
    selectizeInput(
      ns("logic_parent"), "The earlier question",
      choices = c("Pick a question" = "", stats::setNames(earlier$id, earlier$label)), selected = current, width = "100%"
    ),
    uiOutput(ns("logic_answers")),
    footer = tagList(modalButton("Cancel"), actionButton(ns("logic_answer_save"), "Save", class = "btn-primary"))
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
  modalDialog(
    title = "Shown only to some respondents", easyClose = TRUE,
    textInput(
      ns("logic_condition_text"), "This question is only shown to respondents who are:",
      value = blank_if_na(current), placeholder = "e.g. Twins, Female, Parents", width = "100%"
    ),
    p(class = "detail-note", "A question picked as the one this question follows is taken away: a question has one condition."),
    footer = tagList(modalButton("Cancel"), actionButton(ns("logic_condition_save"), "Save", class = "btn-primary"))
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
      # recycle0: a draft with no questions yet has no labels, not one made of the separator
      label = paste0(ifelse(is.na(numbers), "?", numbers), outline_number_separator, shorten(questions$text, 50), recycle0 = TRUE),
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
#' A line has the variable's MATR ID and its response: for a write-in the kind of answer, for a variable with
#' choices the choices in short ("1 Yes · 2 No"), which open their editor under the line when clicked. Who it's
#' about and its label have their own columns, there once a variable of the question has one (About is blank
#' for the respondent); a line's "+" adds them. The order the variables are in is kept (they are dragged into
#' a new one) but not shown. A write-in can also skip, for an answer it names ("If the answer is None, skip to
#' Question 57"), on a line of its own under it, added from its "+". When the question's variables all offer the
#' same choices (a grid's columns), the
#' choices are shown once under the table and edited once for all of them, unless staff chose to edit each
#' variable's own (`view$split`). + Variable, at the bottom of the details, asks whether it has choices or is a
#' write-in. What each field holds is said in it while it is empty, and its label is kept for screen readers.
#'
#' @param block One row of a draft's `components`: a question.
#' @param draft A draft.
#' @param view What the outline shows; see `outline_tree()`. `problems` outlines the fields at fault, `choices`
#'   lists the variables whose choices are open, `split` the questions whose variables' choices are edited one
#'   by one, and `shown` the optional fields added empty.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param ns The module's namespace function.
#' @return A UI element.
variable_cards <- function(block, draft, view, lookups, ns) {
  boxes <- draft_service_children(draft, "variables", block$id)
  shared <- if (block$id %in% view$split) character() else draft_service_shared_choices(draft, block$id)
  list_id <- ns(paste0("boxes-", block$id))
  # the About and Label columns, once one of the variables shows it
  columns <- intersect(c("subject", "label"), unlist(map_rows(boxes, shown_optional, optional_variable_fields, view)))
  if (any(!is.na(boxes$skip_to_id))) view$targets <- skip_targets(draft) # to name where write-ins skip to
  div(
    class = c("variable-cards", paste0("has-", columns, recycle0 = TRUE)),
    if (nrow(boxes) > 0) {
      div(
        class = "variable-line variable-head", `aria-hidden` = "true",
        span(), span("MATR ID"), span("Response"), lapply(optional_variable_fields[columns], span), span(), span()
      )
    },
    div(
      id = list_id, class = "box-list", `data-table` = "variables",
      map_rows(boxes, variable_line, draft, view, lookups, ns, shared, columns)
    ),
    sortable::sortable_js(list_id, list_sortable_options(".box-handle")),
    if (length(shared) > 0) choice_editor(boxes[boxes$id == shared[1], ], draft, view, ns, shared = shared, question = block$id),
    if (block$id %in% view$split && length(draft_service_shared_choices(draft, block$id)) > 0) {
      review_button("Edit their choices together", "join_choices", `data-id` = block$id, class = "btn btn-sm btn-link")
    },
    if (nrow(boxes) == 0) p(class = "detail-note", "No variables yet: add one with + Variable.")
  )
}

# One cell of a variable's line, named for a narrow screen, where the cells stack (data-label).
variable_cell <- function(label, ..., class = NULL) span(class = c("variable-cell", class), `data-label` = label, ...)

variable_line <- function(box, draft, view, lookups, ns, shared = character(), columns = character()) {
  problems <- view$problems
  is_choice_box <- box$variable_type == "closed_ended"
  choices <- draft_service_children(draft, "response_options", box$id)
  choices_at_fault <- any(problems$table %in% "response_options" & problems$row %in% choices$id)
  is_open <- is_choice_box && !box$id %in% shared && box$id %in% view$choices
  showing <- shown_optional(box, optional_variable_fields, view)
  # a variable with choices skips by its choices
  addable <- setdiff(names(optional_variable_fields), c(showing, if (is_choice_box) "skip"))
  div(
    class = c("variable-card", field_problem_class(problems, "variables", box$id, NA), if (is_open) "is-open"),
    `data-id` = box$id,
    div(
      class = "variable-line",
      span(class = "box-handle", title = "Drag to reorder", app_icon("drag")),
      variable_cell("MATR ID", field_text(
        "variables", box, "matr_id",
        placeholder = "MATR ID", list = matr_id_list_id(ns), label = "MATR ID", problems = problems
      ), class = "is-id"),
      variable_cell(
        "Response",
        if (!is_choice_box) {
          field_select(
            "variables", box, "answer_data_type", lookups$answer_data_types,
            blank = "Write-in: pick the answer type", label = "Answer type", problems = problems
          )
        } else if (box$id %in% shared) {
          span(
            class = "choice-summary is-shared", title = "The same choices as the question's other variables, edited below",
            choice_summary(choices)
          )
        } else {
          review_button(
            tagList(app_icon(if (is_open) "choices_open" else "choices_closed"), " ", choice_summary(choices)), "choices",
            `data-id` = box$id, `aria-expanded` = if (is_open) "true" else "false",
            class = c("btn btn-sm choice-summary", if (choices_at_fault) "has-problem"),
            title = if (is_open) "Close the choices" else "Edit the choices"
          )
        },
        class = "is-response"
      ),
      lapply(columns, function(name) {
        variable_cell(
          optional_variable_fields[[name]],
          if (name %in% showing) variable_optional(box, name, view, ns),
          class = paste0("is-", name)
        )
      }),
      span(
        class = "variable-add",
        if (length(addable) > 0) {
          add_field_menu(box, addable, optional_variable_fields, if (is_choice_box) "Add who it's about, or a label" else "Add who it's about, a label, or where it skips to")
        }
      ),
      review_button(
        app_icon("delete"), "delete",
        `data-table` = "variables", `data-id` = box$id,
        class = "btn btn-sm btn-icon variable-delete", title = "Delete this variable", `aria-label` = "Delete this variable"
      )
    ),
    if (!is_choice_box && "skip" %in% showing) write_in_skip(box, view, ns),
    if (is_open) choice_editor(box, draft, view, ns)
  )
}

# The optional fields of a variable, by what its "+" menu calls them: who it's about (blank is the
# respondent), its label, and for a write-in where it skips to. A variable shows one when it has something in
# it, or once it is added.
optional_variable_fields <- c(subject = "About", label = "Label", skip = "Skip to")

# A write-in's skip, under its line: the answer that skips, and where it sends respondents (a question, section,
# question group or the end, picked like a choice's), with the delete button that takes it away.
write_in_skip <- function(box, view, ns) {
  condition <- field_text(
    "variables", box, "skip_condition",
    placeholder = "e.g. None", label = "The answer that skips, e.g. None", problems = view$problems
  )
  if (identical(view$focus, paste0(box$id, ":skip"))) condition <- tagAppendAttributes(condition, `data-focus-now` = NA)
  div(
    class = "variable-skip",
    span(class = "variable-skip-words", "If the answer is"),
    condition,
    span(class = "variable-skip-words", "skip to"),
    skip_fields(box, view$problems, ns, view$targets, table = "variables"),
    remove_field_button(box, "skip", optional_variable_fields)
  )
}

# A variable's About or label, with its delete button that takes it away.
variable_optional <- function(box, name, view, ns) {
  input <- switch(name,
    subject = field_text(
      "variables", box, "subject",
      placeholder = "Who it's about", list = ns("subjects"), label = "About: who it's about (blank for the respondent)",
      problems = view$problems
    ),
    label = field_text("variables", box, "label", placeholder = "Label", list = ns("labels"), label = "Label")
  )
  if (identical(view$focus, paste0(box$id, ":", name))) input <- tagAppendAttributes(input, `data-focus-now` = NA)
  span(class = c("variable-optional", paste0("is-", name)), input, remove_field_button(box, name, optional_variable_fields))
}

# Which of a row's optional fields it shows: those with something in them, and those added empty. A write-in's
# skip has something in it when any of its three fields does.
shown_optional <- function(row, fields, view) {
  has <- function(field) {
    if (field == "skip") any(!is.na(unlist(row[c("skip_condition", "skip_instruction", "skip_to_id")]))) else !is.na(row[[field]])
  }
  names(fields)[vapply(names(fields), function(field) has(field) || paste0(row$id, ":", field) %in% view$shown, logical(1))]
}

# A variable's choices in a few words: "1 Yes · 2 No", every one (the box wraps when they don't fit).
choice_summary <- function(choices) {
  if (nrow(choices) == 0) {
    return("No choices yet")
  }
  codes <- ifelse(is.na(choices$code_value), "", paste0(choices$code_value, " "))
  paste(paste0(codes, ifelse(is.na(choices$answer_option), "(blank)", choices$answer_option)), collapse = " · ")
}

# The optional fields of a choice, by what its "+" menu calls them: how it is printed and where it skips to
# (a link to the question or section, and the words printed for it). A choice shows one when it has
# something in it, or once it is added; most choices have neither.
optional_choice_fields <- c(printed_label = "Printed as", skip_instruction = "Skip to")

#' The editor of a variable's choices: one line per choice, then + Choice (a wide gold button). Once a code is
#' changed, Apply to All? shows over it (the page script shows it): it lists the other questions with the same
#' choices whose codes would change, to give them these codes too.
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
  # the Printed as and Skip to columns, once one of the choices shows it
  columns <- intersect(names(optional_choice_fields), unlist(map_rows(choices, choice_showing, view)))
  div(
    class = c("choice-table", if (length(shared) > 0) "is-shared", paste0("has-", columns, recycle0 = TRUE)),
    `data-shared` = paste(shared, collapse = ","),
    if (length(shared) > 0) {
      div(
        class = "choice-table-title",
        strong(sprintf("Choices of all %d variables", length(shared))),
        review_button("Edit each variable's own", "split_choices", `data-id` = question, class = "btn btn-sm btn-link")
      )
    },
    if (nrow(choices) > 0) {
      div(
        class = "choice-row choice-head", `aria-hidden` = "true",
        span(), span("Code"), span("Response Option"), lapply(optional_choice_fields[columns], span), span(), span()
      )
    },
    div(
      id = list_id, class = "choice-list", `data-table` = "response_options",
      map_rows(choices, choice_row, view$problems, ns, view, columns)
    ),
    sortable::sortable_js(list_id, list_sortable_options(".choice-handle")),
    div(
      class = "choice-actions",
      review_button(
        "Apply to All?", "pick_codes", `data-id` = box$id, hidden = NA,
        class = "btn btn-outline-dark choice-apply", title = "Give these codes to the other questions with the same choices"
      ),
      review_button(icon_label("add", "Choice"), "add_choice", `data-id` = box$id, class = "btn btn-primary btn-wide choice-add", title = "Add a choice")
    )
  )
}

# Which of a choice's optional fields it shows: those with something in them (a link to where it skips counts),
# and those added empty.
choice_showing <- function(choice, view = list()) {
  showing <- union(shown_optional(choice, optional_choice_fields, view), if (!is.na(choice$skip_to_id)) "skip_instruction")
  intersect(names(optional_choice_fields), showing)
}

# Code first, then the response option it codes, then how it is printed and where it skips to, in columns that
# are there once any of the variable's choices has them (`columns`; a choice without one has an empty cell, so
# the columns line up), each with the delete button that takes it away, and the "+" that adds them. Every choice is printed
# on the form: one that isn't can't be marked, so it isn't a choice (a lone check box has one).
choice_row <- function(choice, problems, ns, view = list(), columns = character()) {
  key <- function(field) paste0(choice$id, ":", field)
  showing <- choice_showing(choice, view)
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
    span(class = "choice-handle", title = "Drag to reorder", app_icon("drag")),
    field_text("response_options", choice, "code_value", placeholder = "Code", type = "number", label = "Code", problems = problems) |>
      tagAppendAttributes(class = "choice-code"),
    field_text(
      "response_options", choice, "answer_option",
      placeholder = "Response option", list = ns("answer-options"), label = "Response option", problems = problems
    ) |> tagAppendAttributes(class = "choice-answer"),
    lapply(columns, function(field) {
      span(class = c("choice-cell", paste0("is-", field)), `data-label` = optional_choice_fields[[field]], if (field %in% showing) optional(field))
    }),
    span(
      class = "choice-add-field",
      if (length(addable) > 0) add_field_menu(choice, addable, optional_choice_fields, "Add how it is printed, or where it skips to")
    ),
    review_button(
      app_icon("delete"), "delete",
      `data-table` = "response_options", `data-id` = choice$id,
      class = "btn btn-sm btn-icon choice-delete", title = "Delete this choice", `aria-label` = "Delete this choice"
    )
  )
}

# Where a choice (or a write-in, `table = "variables"`) skips to: the question, section, question group or the end
# of the questionnaire, picked from the list every Skip to field shares (or its number typed). The words printed
# for it ("Go to Question 32") are kept with it but not shown, except when pointed at.
skip_fields <- function(choice, problems, ns, targets = NULL, focus = FALSE, table = "response_options") {
  target <- if (!is.na(choice$skip_to_id) && !is.null(targets)) targets$label[match(choice$skip_to_id, targets$id)]
  tags$input(
    type = "text",
    class = c("form-control form-control-sm skip-target", field_problem_class(problems, table, choice$id, "skip_to_id")),
    value = blank_if_na(target %||% NA), placeholder = "Skips to", `aria-label` = "Skips to: the question's number, or pick it",
    title = if (!is.na(choice$skip_instruction)) paste("Printed:", choice$skip_instruction),
    `data-suggest` = skip_list_id(ns), `data-pick` = NA, `data-focus-now` = if (focus) NA,
    !!!field_problem_attributes(problems, table, choice$id, "skip_to_id"),
    !!!field_attributes(table, choice, "skip_to_id")
  )
}

# The "+" of a variable or choice: the optional fields it doesn't show yet (`names` says what each is called).
add_field_menu <- function(row, fields, names, title) {
  div(
    class = "dropdown add-field",
    tags$button(
      type = "button", class = "btn btn-sm btn-icon dropdown-toggle", `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      title = title, `aria-label` = title,
      app_icon("add")
    ),
    tags$ul(
      class = "dropdown-menu dropdown-menu-end",
      lapply(fields, function(field) {
        tags$li(review_button(names[[field]], "show_field", `data-id` = row$id, `data-name` = field, class = "dropdown-item"))
      })
    )
  )
}

# The delete button that empties an optional field of a variable or choice and hides it again; Undo brings it
# back.
remove_field_button <- function(row, field, names) {
  what <- paste("Take away", tolower(names[[field]]))
  review_button(
    app_icon("delete"), "hide_field",
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
    return(span(class = "to-fix-note is-clear", app_icon("done"), " Nothing to fix"))
  }
  # pointing at it, or focusing it, lists the problems, each a link to its block (review.js)
  span(class = "to-fix-note", tabindex = "0", app_icon("problem"), paste0(" ", plural(n, "block"), " to fix"))
}
