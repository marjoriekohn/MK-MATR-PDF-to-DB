#' What a variable editor's buttons and fields do (33_variable_editor_ui.R).
#'
#' One for every page that edits a question's variables: Edit Extraction's outline (in the picked question's
#' details) and Make an edit (`variable_editor_server()`). The page keeps the draft and says how a change is
#' made; this keeps how the editor shows it: which variables' choices are open (`choices_open`), which optional
#' fields were added empty (`shown`, as "v3:subject" or "o12:printed_label"), the questions whose shared
#' choices are edited one by one (`split`), and the field just added, which gets the cursor when it is drawn.
#'
#' Its buttons add variables (from a dialog: Choices or Write-in) and choices, give a variable's codes to the
#' other questions with the same choices (Apply to All?), open and close a variable's choices, edit a grid's
#' shared choices together or one by one, add and take away optional fields, and delete variables and choices.
#'
#' @param draft A reactive of the draft.
#' @param change The page's way of changing it: `change(new_draft, typed = FALSE)`, TRUE if it took the change.
#'   A change it refuses puts the fields back as the draft has them.
#' @param redraw Called with a question's id when its variables are to be drawn again without the draft
#'   changing (choices opened, a field added).
#' @param ns The page module's namespace function, for its dialogs.
#' @return A list: `action(event)` (TRUE if it was one of the editor's), `edit(event)` and `reorder(event)`
#'   (TRUE if the draft took them), `apply_codes()` (Apply in the Apply to All? dialog), `view()` (how the editor
#'   shows the draft, for `variable_cards()`), `take_focus()` (the field to put the cursor in, once), `reset()`
#'   (forget the fields added empty, when everything is drawn again), and the reactive values above.
variable_editor <- function(draft, change, redraw, ns) {
  shown <- reactiveVal(character())
  choices_open <- reactiveVal(character())
  split <- reactiveVal(character())
  codes_for <- reactiveVal(NULL) # the variable Apply to All? gives the codes of
  focus <- NULL

  # The variables a change in the shared choices editor goes to (the page script sends them as `shared`).
  shared_of <- function(event) {
    ids <- strsplit(event$shared %||% "", ",", fixed = TRUE)[[1]]
    ids[nzchar(ids)]
  }

  flip <- function(ids, id) if (id %in% ids) setdiff(ids, id) else c(ids, id)

  # The question a variable or choice belongs to.
  question_of <- function(table, id) {
    now <- draft()
    variable <- if (table == "variables") id else now$response_options$variable_id[now$response_options$id %in% id]
    now$variables$component_id[now$variables$id %in% variable]
  }

  # Which table an optional field is in: a variable's About, label and (a write-in's) skip, or a choice's
  # printed words and skip.
  optional_table <- function(name) {
    if (name %in% names(optional_variable_fields)) "variables" else if (name %in% names(optional_choice_fields)) "response_options"
  }

  action <- function(event) {
    now <- draft()
    shared <- shared_of(event)
    switch(event$action,
      pick_variable = showModal(add_variable_dialog(ns, event$id)),
      add_box = {
        removeModal()
        change(draft_service_add_box(now, event$id, event$type)) # Choices or Write-in, as picked
      },
      add_choice = change(
        if (length(shared) > 0) draft_service_add_choice_shared(now, shared) else draft_service_add_choice(now, event$id)
      ),
      pick_codes = pick_codes(now, event$id),
      choices = {
        choices_open(flip(choices_open(), event$id))
        redraw(question_of("variables", event$id))
      },
      split_choices = {
        split(union(split(), event$id))
        redraw(event$id)
      },
      join_choices = {
        split(setdiff(split(), event$id))
        redraw(event$id)
      },
      show_field = show_field(event$id, event$name),
      hide_field = hide_field(now, event$id, event$name, shared),
      delete = {
        if (!event$table %in% c("variables", "response_options")) {
          return(FALSE) # a block: the page's own
        }
        change(
          if (event$table == "response_options" && length(shared) > 0) {
            draft_service_delete_shared(now, shared, event$id)
          } else {
            draft_service_delete(now, event$table, event$id)
          }
        )
      },
      return(FALSE)
    )
    TRUE
  }

  # Typing, picking and ticking in the fields (a block's too); in the shared choices, in every variable's.
  edit <- function(event) {
    now <- draft()
    shared <- shared_of(event)
    change(
      if (event$table == "response_options" && length(shared) > 0) {
        draft_service_update_shared(now, shared, event$id, event$field, event$value)
      } else {
        draft_service_update(now, event$table, event$id, event$field, event$value)
      },
      typed = isTRUE(event$typed)
    )
  }

  # Variables or choices dragged into a new order.
  reorder <- function(event) {
    ids <- unlist(event$ids)
    shared <- shared_of(event)
    change(
      if (event$table == "response_options" && length(shared) > 0) {
        draft_service_reorder_shared(draft(), shared, ids)
      } else {
        draft_service_reorder(draft(), event$table, ids)
      }
    )
  }

  # Once a choice's code is changed, Apply to All? lists the questions with the same choices coded otherwise;
  # Apply gives them these codes. With none, it says so.
  pick_codes <- function(now, id) {
    boxes <- draft_service_codes_differ(now, id)
    if (length(boxes) == 0) {
      showNotification("No other question has these choices with other codes.", type = "message")
      return()
    }
    codes_for(id)
    showModal(apply_codes_dialog(ns, now, boxes, choice_summary(draft_service_children(now, "response_options", id))))
  }

  apply_codes <- function() {
    id <- req(codes_for())
    change(draft_service_copy_codes(draft(), id))
  }

  # An optional field added to a variable or choice shows, empty, with the cursor in it; its question is drawn
  # again for it. It stays until everything is drawn again from scratch, unless something is typed into it.
  show_field <- function(id, name) {
    table <- optional_table(name)
    if (is.null(table)) {
      return()
    }
    shown(union(shown(), paste0(id, ":", name)))
    focus <<- paste0(id, ":", name)
    redraw(question_of(table, id))
  }

  # The delete button of an optional field: it is emptied (a change Undo takes back) and hidden again; in the
  # shared choices, in every variable's choice.
  hide_field <- function(now, id, name, shared = character()) {
    table <- optional_table(name)
    if (is.null(table)) {
      return()
    }
    shown(setdiff(shown(), paste0(id, ":", name)))
    # taking away where a choice or write-in skips to takes its link with its printed words (and the answer)
    fields <- switch(name,
      skip_instruction = c("skip_to_id", "skip_instruction"),
      skip = c("skip_condition", "skip_instruction", "skip_to_id"),
      name
    )
    row <- match(id, now[[table]]$id)
    if (any(!is.na(unlist(now[[table]][row, fields])))) {
      changed <- now
      for (field in fields) {
        changed <- if (table == "response_options" && length(shared) > 0) {
          draft_service_update_shared(changed, shared, id, field, "")
        } else {
          draft_service_update(changed, table, id, field, "")
        }
      }
      change(changed)
    }
    redraw(question_of(table, id))
  }

  list(
    action = action, edit = edit, reorder = reorder, apply_codes = apply_codes,
    view = function() list(shown = shown(), choices = choices_open(), split = split()),
    take_focus = function() {
      field <- focus
      focus <<- NULL
      field
    },
    reset = function() shown(character()),
    shown = shown, choices_open = choices_open, split = split
  )
}

#' Make an edit's variable editor: the variables of the question being added, edited as Edit Extraction edits a
#' question's (`variable_editor()`), with Skip to as the words printed for it (there is no outline here to link
#' it to) and without Apply to All? (there are no other questions).
#'
#' The form holds the variables as JSON (`edit_variables_parse()`): `load()` gives them to the editor when the
#' form is drawn, and `value()` is them again, as they are now. Changes aren't kept for Undo: each field and
#' button is its own small change, and the form isn't sent until it is checked.
#'
#' @param id The module's id.
#' @return A list: `load(json, options)`, where `options` is the field's (`subjects`, `answer_data_types`,
#'   `answer_options`), and `value()`, a reactive of the variables as JSON (`NULL` until loaded).
variable_editor_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    draft <- reactiveVal(NULL) # the question being added, alone, with its variables
    lookups <- list() # what its fields offer
    version <- reactiveVal(0L) # ticks when the variables are drawn again

    redraw <- function(...) version(version() + 1L)

    # A change refused puts the fields back as the draft has them; words typed aren't drawn again, so the cursor
    # stays where it is. The draft's notes speak of Undo, which this page hasn't got, so they aren't shown.
    change <- function(new_draft, typed = FALSE) {
      new_draft <- try_change(new_draft, notes = FALSE)
      if (is.null(new_draft)) {
        redraw()
        return(FALSE)
      }
      draft(new_draft)
      if (!typed) redraw()
      TRUE
    }

    editor <- variable_editor(draft, change, redraw, ns)

    observeEvent(input$action, editor$action(input$action))
    observeEvent(input$edit, editor$edit(input$edit))
    observeEvent(input$reorder, editor$reorder(input$reorder))

    output$cards <- renderUI({
      version()
      isolate({
        now <- req(draft())
        view <- c(editor$view(), list(focus = editor$take_focus(), skip_words = TRUE, apply_codes = FALSE))
        variable_editor_body(now$components[1, , drop = FALSE], now, view, lookups, ns)
      })
    })

    list(
      # Called while the form is drawn, so it reads nothing reactively: otherwise the form would depend on what this
      # changes, and draw itself again, for ever.
      load = function(json, options) {
        isolate({
          variables <- tryCatch(edit_variables_parse(json), error = function(problem) list())
          lookups <<- list(
            subjects = options$subjects, answer_data_types = options$answer_data_types, answer_options = options$answer_options
          )
          editor$reset()
          draft(edit_variables_draft(variables))
          redraw()
        })
      },
      value = reactive(if (!is.null(draft())) edit_variables_json(draft()))
    )
  })
}
