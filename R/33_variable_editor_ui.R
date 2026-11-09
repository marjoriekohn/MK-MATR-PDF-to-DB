# ------ The variable editor ----------------------------------------------------------------------------
# A question's variables and their choices, edited: on Edit Extraction in the picked question's details, and on
# Make an edit for the question being added (variable_editor_server()). One editor, so every change to it reaches
# both pages. Its fields and buttons say what they change (data-field, data-action) and the page script sends
# that to the module they sit in (www/js/review.js); what they do is variable_editor() (33_variable_editor_server.R).

# The variable editor of Make an edit's form: its variables and their choices, drawn by its server
# (`variable_editor_server()`, output `cards`) once the form gives it the variables.
# id: The editor module's id.
# label: Names it for screen readers, e.g. "Variables".
variable_editor_ui <- function(id, label) {
  ns <- NS(id)
  div(
    class = "variable-editor", role = "group", `aria-label` = label, `data-ns` = ns(""),
    div(class = "variable-editor-title", label),
    uiOutput(ns("cards"))
  )
}

# What a variable editor draws for a question on its own (Make an edit): its variables, the lists their fields
# offer, and + Variable under them.
variable_editor_body <- function(block, draft, view, lookups, ns) {
  tagList(
    variable_editor_lists(ns, lookups, draft),
    variable_cards(block, draft, view, lookups, ns),
    review_button(
      icon_label("add", "Variable"), "pick_variable",
      `data-id` = block$id, class = "btn btn-outline-dark btn-wide variable-editor-add"
    )
  )
}

# The words a variable's fields offer as they are typed: the answer choices, who and when a variable can be about,
# the units a write-in can be in, and the labels the draft's variables have.
# lookups: The pick lists: `answer_options`, `subjects`, `time_frames` and `units` (the database's tags).
# Returns: A tag list of `datalist`s.
variable_editor_lists <- function(ns, lookups, draft) {
  tagList(
    suggestions(ns("answer-options"), lookups$answer_options),
    suggestions(ns("subjects"), lookups$subjects),
    suggestions(ns("time-frames"), lookups$time_frames),
    suggestions(ns("units"), lookups$units),
    suggestions(ns("labels"), label_suggestions(draft))
  )
}

# ------ Variables and choices -----------------------------------------------
# Variables reorder within their question, choices within their variable.
list_sortable_options <- function(handle) {
  sortable::sortable_options(handle = handle, onEnd = htmlwidgets::JS("editExtractionPage.sendList"))
}

# A question's variables, as a table: one line for each, in printed order.
# A line has the variable's MATR ID and its response: for a write-in the kind of answer, for a variable with
# choices the choices in short ("1 Yes · 2 No"), which are open under the line (their editor) until the line is clicked to close them. Who it's
# about, when it's about, its label and (a write-in's) unit have their own columns, there once a variable of the
# question has one (Subject is blank for the respondent); a line's "+" adds them. The order the variables are
# in is kept (they are dragged into a new one) but not shown. When the question's variables all offer the
# same choices (a grid's columns), the
# choices are shown once under the table and edited once for all of them, unless staff chose to edit each
# variable's own (`view$split`). + Variable, at the bottom of the details, asks whether it has choices or is a
# write-in. What each field holds is said in it while it is empty, and its label is kept for screen readers.
# block: One row of a draft's `components`: a question.
# view: How the editor shows the draft (`variable_editor()`'s `view()`, with the page's own): `problems`
#   outlines the fields at fault, `closed` lists the variables whose choices staff closed (the others are open), `split` the questions
#   whose variables' choices are edited one by one, `shown` the optional fields added empty and `focus` the one
#   to put the cursor in. A page can leave out what it can't offer: `apply_codes = FALSE` leaves out Apply to
#   All? (for a draft that has one question).
variable_cards <- function(block, draft, view, lookups, ns) {
  boxes <- draft_service_children(draft, "variables", block$id)
  shared <- if (block$id %in% view$split) character() else draft_service_shared_choices(draft, block$id)
  list_id <- ns(paste0("boxes-", block$id))
  # the Subject, Time frame, Label and Unit columns, once one of the variables shows it
  columns <- intersect(names(optional_variable_fields), unlist(map_rows(boxes, shown_optional, optional_variable_fields, view)))
  div(
    class = c("variable-cards", paste0("has-", columns, recycle0 = TRUE), paste0("optional-", length(columns))),
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
  is_open <- is_choice_box && !box$id %in% shared && !box$id %in% view$closed
  showing <- shown_optional(box, optional_variable_fields, view)
  # a variable with choices has no unit
  addable <- setdiff(names(optional_variable_fields), c(showing, if (is_choice_box) "unit"))
  div(
    class = c("variable-card", field_problem_class(problems, "variables", box$id, NA), if (is_open) "is-open"),
    `data-id` = box$id,
    div(
      class = "variable-line",
      span(class = "box-handle", title = "Drag to reorder", app_icon("drag")),
      variable_cell("MATR ID", field_text(
        "variables", box, "matr_id",
        placeholder = "MATR ID", label = "MATR ID", problems = problems
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
          add_field_menu(
            box, addable, optional_variable_fields,
            if (is_choice_box) "Add who or when it's about, or a label" else "Add who or when it's about, a label, or its unit"
          )
        }
      ),
      review_button(
        app_icon("delete"), "delete",
        `data-table` = "variables", `data-id` = box$id,
        class = "btn btn-sm btn-icon variable-delete", title = "Delete this variable", `aria-label` = "Delete this variable"
      )
    ),
    if (is_open) choice_editor(box, draft, view, ns)
  )
}

# The optional fields of a variable, by what its "+" menu calls them: who it's about (blank is the
# respondent), when it's about ("in the past 30 days"), its label, and for a write-in the unit its number is in
# ("years"). A variable shows one when it has something in it, or once it is added.
optional_variable_fields <- c(subject = "Subject", time_frame = "Time frame", label = "Label", unit = "Unit")

# A variable's subject, time frame, label or unit, with its delete button that takes it away.
variable_optional <- function(box, name, view, ns) {
  input <- switch(name,
    subject = field_text(
      "variables", box, "subject",
      placeholder = "Who it's about", list = ns("subjects"), label = "Subject: who it's about (blank for the respondent)",
      problems = view$problems
    ),
    time_frame = field_text(
      "variables", box, "time_frame",
      placeholder = "e.g. in the past 30 days", list = ns("time-frames"), label = "Time frame: when its answer is about",
      problems = view$problems
    ),
    label = field_text("variables", box, "label", placeholder = "Label", list = ns("labels"), label = "Label"),
    unit = field_text("variables", box, "unit", placeholder = "e.g. years", list = ns("units"), label = "Unit: what its number is in")
  )
  if (identical(view$focus, paste0(box$id, ":", name))) input <- tagAppendAttributes(input, `data-focus-now` = NA)
  span(class = c("variable-optional", paste0("is-", name)), input, remove_field_button(box, name, optional_variable_fields))
}

# Which of a variable's optional fields it shows: those with something in them, and those added empty.
shown_optional <- function(row, fields, view) {
  names(fields)[vapply(names(fields), function(field) !is.na(row[[field]]) || paste0(row$id, ":", field) %in% view$shown, logical(1))]
}

# A variable's choices in a few words, each code = its answer: "1=yes; 0=no", every one (the box wraps when they
# don't fit). A choice with no code yet is its answer alone.
choice_summary <- function(choices) {
  if (nrow(choices) == 0) {
    return("No choices yet")
  }
  codes <- ifelse(is.na(choices$code_value), "", paste0(choices$code_value, "="))
  paste(paste0(codes, ifelse(is.na(choices$answer_option), "(blank)", choices$answer_option)), collapse = "; ")
}

# The editor of a variable's choices: one line per choice, then + Choice (a wide gold button). Once a code is
# changed, Apply to All? shows over it (the page script shows it): it lists the other questions with the same
# choices whose codes would change, to give them these codes too.
# box: One row of a draft's `variables`.
# shared: The ids of the question's variables these choices are edited for (all of them get each
#   change), or none for this variable's own.
# question: The question's id, for "Edit each variable's own" (with `shared`).
choice_editor <- function(box, draft, view, ns, shared = character(), question = NULL) {
  choices <- draft_service_children(draft, "response_options", box$id)
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
    if (nrow(choices) > 0) {
      div(
        class = "choice-row choice-head", `aria-hidden` = "true",
        span(), span("Code"), span("Response Option"), span()
      )
    },
    div(
      id = list_id, class = "choice-list", `data-table` = "response_options",
      map_rows(choices, choice_row, view$problems, ns)
    ),
    sortable::sortable_js(list_id, list_sortable_options(".choice-handle")),
    div(
      class = "choice-actions",
      if (!isFALSE(view$apply_codes)) {
        review_button(
          "Apply to All?", "pick_codes", `data-id` = box$id, hidden = NA,
          class = "btn btn-outline-dark choice-apply", title = "Give these codes to the other questions with the same choices"
        )
      },
      review_button(icon_label("add", "Choice"), "add_choice", `data-id` = box$id, class = "btn btn-primary btn-wide choice-add", title = "Add a choice")
    )
  )
}

# Code first, then the response option it codes, then the delete button. Every choice is printed on the form:
# one that isn't can't be marked, so it isn't a choice (a lone check box has one).
choice_row <- function(choice, problems, ns) {
  div(
    class = "choice-row", `data-id` = choice$id,
    span(class = "choice-handle", title = "Drag to reorder", app_icon("drag")),
    # (the class goes in with the field: adding it afterwards would sort the attributes, and the page script and
    # the tests read them in the order every field has them)
    field_text("response_options", choice, "code_value", placeholder = "Code", label = "Code", problems = problems, class = "choice-code"),
    field_text(
      "response_options", choice, "answer_option",
      placeholder = "Response option", list = ns("answer-options"), label = "Response option", problems = problems, class = "choice-answer"
    ),
    review_button(
      app_icon("delete"), "delete",
      `data-table` = "response_options", `data-id` = choice$id,
      class = "btn btn-sm btn-icon choice-delete", title = "Delete this choice", `aria-label` = "Delete this choice"
    )
  )
}

# The "+" of a variable: the optional fields it doesn't show yet (`names` says what each is called).
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

# The delete button that empties an optional field of a variable and hides it again; Undo brings it back.
remove_field_button <- function(row, field, names) {
  what <- paste("Take away", tolower(names[[field]]))
  review_button(
    app_icon("delete"), "hide_field",
    `data-id` = row$id, `data-name` = field, class = "btn btn-sm btn-icon field-remove", title = what, `aria-label` = what
  )
}

# + Variable: choices or a write-in, for the question `id`.
# id: The question's id.
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

# Apply to All?: the questions whose variables offer the same choices as this one but code them
# differently, which Apply gives these codes.
# boxes: The ids of the variables that would change (`draft_service_codes_differ()`).
# codes: This variable's choices with their codes ("1 = Yes · 2 = No").
apply_codes_dialog <- function(ns, draft, boxes, codes) {
  questions <- outline_questions(draft)$questions
  affected <- unique(draft$variables$component_id[match(boxes, draft$variables$id)])
  labels <- questions$label[match(affected, questions$id)]
  confirm_dialog(
    "Apply these codes to all?",
    p("Codes: ", strong(codes)),
    p(sprintf("%s the same choices with other codes. Apply gives them these codes:", if (length(affected) == 1) "This question has" else paste(length(affected), "questions have"))),
    tags$ul(class = "apply-codes-list", lapply(labels[!is.na(labels)], tags$li)),
    confirm_id = ns("apply_codes"), confirm_label = "Apply", keep_label = "Cancel", easy_close = TRUE
  )
}

# The labels the draft's variables have, each once, offered as a label is typed ("child a", "child b").
label_suggestions <- function(draft) {
  labels <- trimws(draft$variables$label)
  sort(unique(labels[!is.na(labels) & nzchar(labels)]))
}
