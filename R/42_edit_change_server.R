# A row's details, the form for adding or changing one, or the review before deleting one: what Make an
# edit's details panel shows.
# The details (`verb` "view") are the row's fields as text, with where it is used. Nothing is saved here: the
# page applies the change. The form is drawn once for each kind and row. What it says about what you have typed
# (the notes under fields, what changed, what it reaches) is drawn apart from it, so typing never redraws the
# fields. A form is marked with a number (`form_token`) the browser sends back once it has drawn it, and what was
# typed counts only once the number matches, so the fields of the form before are never read as the new form's.
# entity,verb,snapshot,row,scope: Reactives from the page: the kind, the verb ("view", "add", "update" or
#   "delete"), the tables, the row picked and the questionnaire.
# prefill: A reactive of what to start the form with, by field id (what a pop-up had typed before it opened another),
#   or `NULL`; it is used once, when the form is drawn.
# Returns: A list of reactives: `active`, `stuck` (a delete something stands in the way of), `ready` (it can be
#   applied), `blockers` (what is left to do), `built` (the items and summary), `jump` (a request to go to another
#   kind), `token` (which form is on screen), and `fields` and `values` (the form's fields and what is typed in them).
edit_change_server <- function(id, entity, verb, snapshot, row, scope, prefill = reactive(NULL)) {
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
        } else if (field$type == "several") {
          edit_several_text(input[[field$id]]) # nothing picked is NULL, not "the field isn't drawn yet"
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

    # What's left to do before it can be applied, in the order to do it.
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
    # A delete that something stands in the way of can't be applied at all, so there's nothing to say about why.
    stuck <- reactive(tryCatch(
      active() && context()$mode == "delete" && length(plan()$blocked) > 0,
      shiny.silent.error = function(problem) FALSE
    ))

    ready <- reactive(tryCatch(active() && length(blockers()) == 0, shiny.silent.error = function(problem) FALSE))

    observeEvent(input$action, {
      event <- input$action
      if (identical(event$action, "jump")) {
        # an item to open (a block as it is in one questionnaire) says which: its questionnaire and key
        jump(list(kind = event$kind, verb = event$verb %||% "add", scope = event$scope, key = event$key, at = Sys.time()))
      }
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

    # The row's details as text, each on a line of its own and whole (see edit_service_details()), and where the row
    # is used.
    view_ui <- function(ctx) {
      lines <- edit_service_details(ctx$entity, ctx$snap, ctx$row, fields(), initial())
      tagList(
        if (nrow(lines) > 0) {
          div(class = "edit-view", detail_section(NULL, lapply(seq_len(nrow(lines)), function(i) {
            detail_field(lines$label[i], span(class = "detail-value", lines$value[i]), wide = lines$wide[i])
          })))
        },
        div(class = "edit-usage", edit_usage_ui(ctx$entity, usage()))
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
      # what a pop-up had typed before it opened another is put back, once
      typed <- isolate(prefill())
      if (!is.null(typed)) {
        kept <- intersect(names(typed), names(start))
        start[kept] <- lapply(typed[kept], as.character)
      }
      generation(isolate(generation()) + 1L)
      for (field in Filter(function(field) field$type == "variables", all_fields)) variables$load(start[[field$id]], field$options)

      tagList(
        edit_empty_choices_ui(all_fields),
        div(class = "edit-token", textInput(ns("form_token"), NULL, value = isolate(generation()))),
        Map(function(field, i) edit_field_ui(ns, field, start[[field$id]], ns(paste0("note_", i))), all_fields, seq_along(all_fields)),
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

    output$changes <- renderUI(edit_changes_ui(changes()))

    output$impact <- renderUI({
      ctx <- context()
      req(ctx$mode == "update")
      update_aside(ctx)
    })

    update_aside <- function(ctx) {
      use <- usage()
      words_changed <- ctx$entity$table == "placement" && "text" %in% changes()$name
      tagList(
        if (use$shared >= 2) {
          edit_callout(
            "warning",
            title = paste("Shared by", use$shared, "questionnaires"),
            p(paste(
              "If only one place should have the new words, change it there instead: press Open in this",
              "questionnaire on its card below. That changes just that place."
            ))
          )
        },
        if (words_changed) {
          elsewhere <- length(edit_service_uses(ctx$snap, ctx$row$component_id)) - 1
          edit_callout(
            "warning",
            title = "New words change this place only",
            p(paste0(
              "The words belong to the component, which every place that prints them shares. So this place prints ",
              "the component with your words (the one the database has, or a new one), and stops printing the old one."
            )),
            p(if (elsewhere > 0) {
              paste0("The old one stays, for the ", edit_count(elsewhere, "other place"), " that still print it.")
            } else {
              "Nothing else prints the old one, so the database forgets it."
            })
          )
        },
        edit_usage_ui(ctx$entity, use)
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
      active = active, stuck = stuck, ready = ready, blockers = blockers, built = built,
      jump = jump, token = generation, fields = fields, values = values
    )
  })
}
