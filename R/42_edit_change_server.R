#' A row's details, the form for adding or changing one, or the review before deleting one: what Make an
#' edit's details panel shows.
#'
#' The details (`verb` "view") are the row's fields as text, with where it is used. Nothing is saved here: the
#' page saves proposals. The form is drawn once for each kind and row. What it says about what you have typed
#' (the notes under fields, what changed, what it reaches) is drawn apart from it, so typing never redraws the
#' fields. A form is marked
#' with a number (`form_token`) the browser sends back once it has drawn it, and what was
#' typed counts only once the number matches, so the fields of the form before are never
#' read as the new form's.
#'
#' @param id The module's id.
#' @param entity,verb,snapshot,row,scope,resume Reactives from the page: the kind, the verb ("view",
#'   "add", "update" or "delete"), the tables, the row picked, the questionnaire, and a saved draft being
#'   opened again.
#' @return A list of reactives: `active`, `stuck` (a delete something stands in the way
#'   of), `ready` (it can be sent), `blockers` (what is left to do), `built` (the items and
#'   summary), `form` (as it would be opened again), `jump` (a request to go to another
#'   kind), `token` (which form is on screen), and `fields` and `values` (the form's fields and
#'   what is typed in them).
edit_change_server <- function(
  id,
  entity,
  verb,
  snapshot,
  row,
  scope,
  resume
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    note_slots <- 60 # more than any form has fields

    # ------ REACTIVE ----------------------------------------------------------
    generation <- reactiveVal(0L) # which form is on screen
    jump <- reactiveVal(NULL) # a request to go to another kind, e.g. to add what a drop-down lacks
    # A question's variables (adding a question), edited as on Edit Extraction; a form has at most one such field.
    variables <- variable_editor_server("variables")

    # What's being done to what, once it's all known: otherwise nothing is drawn.
    context <- reactive({
      kind <- req(entity())
      snap <- req(snapshot())
      mode <- req(verb())
      req(mode %in% c("view", "add", "update", "delete"))
      if (!is.null(kind$scope)) req(nzchar(scope() %||% ""))
      target <- if (mode == "add") NULL else req(row())
      if (mode != "view") req(is.null(edit_service_can(kind, snap, mode)))
      list(entity = kind, snap = snap, mode = mode, row = target, scope = if (nzchar(scope() %||% "")) scope())
    })
    active <- reactive(tryCatch(
      {
        context()
        TRUE
      },
      shiny.silent.error = function(problem) FALSE
    ))

    # The details are the fields as a change would start from them.
    form_mode <- reactive(if (context()$mode == "view") "update" else context()$mode)
    fields <- reactive({
      ctx <- context()
      req(ctx$mode != "delete")
      edit_service_fields(ctx$entity, ctx$snap, form_mode(), ctx$row, ctx$scope)
    })
    initial <- reactive({
      ctx <- context()
      edit_service_initial(ctx$entity, fields(), ctx$snap, form_mode(), ctx$row, ctx$scope)
    })

    # What's in the form now, by field id.
    values <- reactive({
      all_fields <- fields()
      start <- initial()
      req(identical(as.character(input$form_token), as.character(generation())))
      now <- lapply(all_fields, function(field) {
        if (!is.null(field$locked)) {
          start[[field$id]]
        } else if (field$type == "variables") {
          variables$value() %||% start[[field$id]]
        } else {
          input[[field$id]] %||% start[[field$id]]
        }
      })
      stats::setNames(now, vapply(all_fields, function(field) field$id, character(1)))
    })

    check <- reactive(edit_service_check(fields(), values()))
    changes <- reactive({
      req(context()$mode == "update")
      edit_service_changes(fields(), initial(), values())
    })
    usage <- reactive({
      ctx <- context()
      req(ctx$mode %in% c("view", "update"))
      edit_service_usage(ctx$entity, ctx$snap, ctx$row)
    })
    plan <- reactive({
      ctx <- context()
      req(ctx$mode == "delete")
      edit_service_delete_plan(ctx$entity, ctx$snap, ctx$row)
    })

    # Items and a summary; not for a form that still has things to fix.
    built <- reactive({
      ctx <- context()
      req(ctx$mode != "view")
      if (ctx$mode == "delete") {
        return(edit_service_build(ctx$entity, "delete", ctx$snap, ctx$row, ctx$scope, plan = plan()))
      }
      req(length(check()$missing) == 0, length(check()$problems) == 0)
      edit_service_build(ctx$entity, ctx$mode, ctx$snap, ctx$row, ctx$scope, fields(), values())
    })

    # What's left to do before it can be sent, in the order to do it.
    blockers <- reactive({
      # (while a form is being redrawn, what's typed can't be read yet)
      tryCatch(what_is_left(), shiny.silent.error = function(problem) "One moment…")
    })

    what_is_left <- function() {
      if (!active()) {
        return("Choose what to change first.")
      }
      ctx <- context()
      if (ctx$mode == "view") {
        return("Press Edit to change it.")
      }
      edit_service_blockers(
        ctx$mode,
        found = if (ctx$mode != "delete") check(),
        n_changes = if (ctx$mode == "update") nrow(changes()) else 0,
        shared = if (ctx$mode == "update") usage()$shared else 0,
        plan = if (ctx$mode == "delete") plan(),
        confirmed = list(read = input$confirm_read, word = input$confirm_word, shared = input$confirm_shared)
      )
    }
    # A delete that something stands in the way of can't be sent at all, so there's nothing to say about why.
    stuck <- reactive(tryCatch(
      active() && context()$mode == "delete" && length(plan()$blocked) > 0,
      shiny.silent.error = function(problem) FALSE
    ))

    ready <- reactive(tryCatch(active() && length(blockers()) == 0, shiny.silent.error = function(problem) FALSE))

    # The form as it would be opened again, for a saved draft.
    form <- reactive({
      ctx <- context()
      list(
        action = ctx$mode, entity = ctx$entity$key, scope = ctx$scope %||% "",
        target = if (is.null(ctx$row)) "" else edit_key(ctx$row, edit_table_key(ctx$entity$table)),
        values = if (ctx$mode == "delete") list() else values()
      )
    })

    observeEvent(input$action, {
      event <- input$action
      if (identical(event$action, "jump")) jump(list(kind = event$kind, verb = event$verb %||% "add", at = Sys.time()))
    })

    # ------ UI ----------------------------------------------------------------
    output$main <- renderUI({
      ctx <- context()
      switch(ctx$mode,
        view = view_ui(ctx),
        delete = delete_ui(ctx),
        form_ui(ctx)
      )
    })

    # The row's fields as text (a field that only matters while another has a value is left out when it doesn't),
    # and where the row is used.
    view_ui <- function(ctx) {
      start <- initial()
      shown <- Filter(function(field) field$type != "variables" && edit_service_visible(field, start), fields())
      tagList(
        detail_section(NULL, lapply(shown, function(field) {
          value <- start[[field$id]]
          detail_field(
            field$label,
            span(class = "detail-value", edit_display_value(field, if (is.null(value) || !nzchar(value)) NA else value)),
            wide = field$type == "textarea"
          )
        })),
        div(class = "edit-usage", edit_usage_ui(ctx$entity, edit_label(ctx$snap, ctx$entity$table, ctx$row), usage()))
      )
    }

    delete_ui <- function(ctx) {
      title <- edit_label(ctx$snap, ctx$entity$table, ctx$row)
      tagList(
        edit_delete_ui(ctx$entity, title, plan()),
        if (length(plan()$blocked) == 0) edit_delete_confirm_ui(ns, plan())
      )
    }

    form_ui <- function(ctx) {
      all_fields <- fields()
      start <- initial()
      start <- resume_values(start, ctx)
      generation(isolate(generation()) + 1L)
      for (field in Filter(function(field) field$type == "variables", all_fields)) variables$load(start[[field$id]], field$options)

      rows <- Map(function(field, i) {
        heading <- if (!is.null(field$part_title) && (i == 1 || all_fields[[i - 1]]$part_index != field$part_index)) {
          edit_part_heading(field$part_title, edit_part_why(ctx$entity, field$part_index))
        }
        tagList(heading, edit_field_ui(ns, field, start[[field$id]], ns(paste0("note_", i))))
      }, all_fields, seq_along(all_fields))

      tagList(
        edit_empty_choices_ui(all_fields),
        div(class = "edit-token", textInput(ns("form_token"), NULL, value = isolate(generation()))),
        rows,
        if (ctx$mode == "update") uiOutput(ns("changes")),
        if (ctx$mode == "update" && isolate(usage()$shared) >= 2) {
          checkboxInput(ns("confirm_shared"), sprintf(
            "I understand this changes it in all %d questionnaires that use it.", isolate(usage()$shared)
          ), width = "100%")
        },
        # an add reaches nothing yet; what it is is said in the info block at the top
        if (ctx$mode != "add") uiOutput(ns("impact"), class = "edit-usage")
      )
    }

    # A draft being opened again puts back what was typed in it, once, if it's
    # the same form.
    resume_values <- function(start, ctx) {
      saved <- isolate(resume())
      if (is.null(saved)) {
        return(start)
      }
      same <- identical(saved$form$entity, ctx$entity$key) && identical(saved$form$action, ctx$mode) &&
        identical(as.character(saved$form$scope %||% ""), as.character(ctx$scope %||% "")) &&
        identical(
          as.character(saved$form$target %||% ""),
          if (is.null(ctx$row)) "" else edit_key(ctx$row, edit_table_key(ctx$entity$table))
        )
      if (!same) {
        return(start)
      }
      kept <- intersect(names(saved$form$values), names(start))
      start[kept] <- lapply(saved$form$values[kept], as.character)
      isolate(resume(NULL))
      start
    }

    edit_part_why <- function(kind, index) {
      kind$parts[[index]]$why
    }

    output$changes <- renderUI(edit_changes_ui(changes()))

    output$impact <- renderUI({
      ctx <- context()
      req(ctx$mode == "update")
      update_aside(ctx)
    })

    update_aside <- function(ctx) {
      title <- edit_label(ctx$snap, ctx$entity$table, ctx$row)
      use <- usage()
      local <- !is.null(ctx$entity$scope)
      words_changed <- local && any(c("text", "component_type_id") %in% changes()$name)
      tagList(
        if (use$shared >= 2) {
          edit_callout(
            "warning",
            title = paste("Shared by", use$shared, "questionnaires"),
            p("If only one questionnaire should have the new words, change it there instead. That edits just that questionnaire's copy."),
            review_button(
              paste("Change it in one questionnaire only"), "jump",
              `data-kind` = if (ctx$entity$key == "question") "questionnaire_question" else "questionnaire_component",
              `data-verb` = "update", class = "btn btn-sm btn-outline-dark"
            )
          )
        },
        if (words_changed) {
          elsewhere <- length(edit_service_uses(ctx$snap, ctx$row$component_id)) - 1
          edit_callout(
            "warning",
            title = "New words mean a new component",
            p(paste0(
              "The words and the kind of block belong to the component, which other questionnaires share. So ",
              "this questionnaire gets its own new component with your words, and stops using the old one."
            )),
            p(if (elsewhere > 0) {
              paste0("The old one stays, for the ", edit_count(elsewhere, "other questionnaire"), " that still print it.")
            } else {
              "Nothing else prints the old one, so it's left unused in the database."
            })
          )
        },
        edit_usage_ui(ctx$entity, title, use)
      )
    }

    lapply(seq_len(note_slots), function(i) {
      output[[paste0("note_", i)]] <- renderUI({
        all_fields <- fields()
        if (i > length(all_fields)) {
          return(NULL)
        }
        id <- all_fields[[i]]$id
        found <- check()
        if (!is.null(found$problems[[id]])) {
          return(span(class = "field-problem", found$problems[[id]]))
        }
        if (!is.null(found$warnings[[id]])) {
          return(span(class = "field-warning", found$warnings[[id]]))
        }
        # a required field left empty: outlined in red, and said to screen readers
        if (id %in% found$missing_ids) span(class = "field-missing visually-hidden", "Required")
      })
    })

    # ------ RETURN ------------------------------------------------------------
    list(
      active = active, stuck = stuck, ready = ready, blockers = blockers, built = built, form = form,
      jump = jump, token = generation, fields = fields, values = values
    )
  })
}
