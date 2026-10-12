#' The Make an edit page's server.
#'
#' Staff finish "I want to [add/update/delete] a [kind]", then pick the row (an update or
#' delete) and fill in or review the change; the find and change modules do those. This page
#' loads the tables the kind needs, keeps drafts, and makes the change: Save as draft, then
#' either apply it at once (an add or an update, or anything an ADMIN does) or send it to
#' Change Requests for one other staff member to approve (a STAFF member's delete); see
#' `service_change_request.R`. A draft is saved in the database (`model_edit.R`). A request
#' that was sent back, or is waiting for approval, can be opened here again: saving it makes a
#' new revision.
#'
#' @param id The module's id.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param show_page A function taking a page's name and showing it.
#' @return A list with `open`, a function taking a saved edit's record that opens it on the form.
make_edit_server <- function(
  id,
  connection,
  me,
  show_page
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    verb <- reactive(input$verb %||% "")
    kind <- reactive(input$kind %||% "")
    entity <- reactive(if (kind() %in% names(edit_entities)) edit_entities[[kind()]])

    loaded <- reactiveVal(NULL) # list(kind, tables): the tables the kind needs
    load_problem <- reactiveVal(NULL) # why they couldn't be read
    scope <- reactiveVal("") # the questionnaire, for a kind that belongs to one
    picked <- reactiveVal(NULL) # the key of the row picked from the cards
    resume <- reactiveVal(NULL) # a saved draft being opened again
    draft <- reactiveVal(NULL) # the saved draft this form is, to save over
    done <- reactiveVal(NULL) # what was just done: list(applied, record, history_id)
    saved_count <- reactiveVal(0L) # ticks when a proposal is saved
    duplicates <- reactiveVal(NULL) # the existing rows an add looks like, while staff decide

    # Whether what is on the form needs another staff member's approval before it is applied.
    needs_approval <- reactive({
      req(nzchar(verb()))
      change_request_service_needs_approval(me, edit_draft_kind, edit_service_kind_of(verb()))
    })
    reviewers <- reactive(tryCatch(staff_service_reviewers(connection, me), error = function(problem) character()))
    # A request being edited again is saved as a new version of itself, not sent anew.
    editing_request <- reactive(identical(draft()$status, "Awaiting Review"))

    # Only the tables of the kind on screen: a pick of another kind has none yet.
    snapshot <- reactive({
      tables <- loaded()
      if (!is.null(tables) && identical(tables$kind, kind())) tables$tables
    })

    row <- reactive({
      req(entity(), snapshot(), picked())
      edit_service_row(entity(), snapshot(), picked())
    })

    # Read again when the database says a proposal changed (a reviewer can send one back) and when this
    # session saved or discarded one. If the database can't be reached the last proposals read stay on
    # screen, with a message, and the next change tries again (as server.R does for the extraction drafts).
    stored <- reactivePoll(
      change_requests_poll_ms, session,
      checkFunc = function() edit_service_signature(connection),
      valueFunc = function() edit_service_signature(connection)
    )
    last_proposals <- list()
    proposals <- reactive({
      saved_count()
      stored()
      tryCatch(
        {
          last_proposals <<- edit_service_get_all(connection)
          last_proposals
        },
        error = function(problem) {
          showNotification(conditionMessage(problem), type = "error", id = "database_problem")
          last_proposals
        }
      )
    })

    can_message <- reactive({
      req(entity(), nzchar(verb()))
      edit_service_can(entity(), snapshot() %||% list(), verb())
    })

    # ------ MODULES -----------------------------------------------------------
    edit_find_server("find", entity, verb, snapshot, scope, picked)
    change <- edit_change_server("change", entity, verb, snapshot, row, scope, resume)

    # ------ The sentence ------------------------------------------------------
    # A new kind means new tables, so the choices made for the last are dropped.
    observeEvent(kind(),
      {
        resuming <- !is.null(resume()) && identical(resume()$form$entity, kind())
        if (!resuming) {
          picked(NULL)
          scope("")
          draft(NULL)
        }
        loaded(NULL)
        load_problem(NULL)
        if (is.null(entity())) {
          return()
        }
        load_tables()
        if (resuming) restore_choices(resume())
      },
      ignoreNULL = FALSE
    )

    # Choosing another thing to do starts the pick again, unless a draft is
    # being opened.
    observeEvent(verb(), {
      if (is.null(resume())) picked(NULL)
    })

    observeEvent(list(verb(), kind()),
      {
        if (nzchar(verb()) || nzchar(kind())) done(NULL)
      },
      ignoreInit = TRUE
    )

    load_tables <- function() {
      kind_now <- kind()
      result <- withProgress(message = "Reading the database…", value = 0.5, {
        tryCatch(edit_service_load(connection, entity()), error = function(problem) {
          load_problem(conditionMessage(problem))
          NULL
        })
      })
      if (!is.null(result)) loaded(list(kind = kind_now, tables = result))
    }

    output$article <- renderUI(if (!is.null(entity())) entity()$article else "a")

    output$explain <- renderUI({
      if (!is.null(load_problem())) {
        return(edit_callout(
          "danger",
          title = "Couldn't read the database",
          p(load_problem()),
          actionButton(ns("retry"), "Try again", class = "btn-sm btn-outline-dark")
        ))
      }
      blocked <- can_message()
      tagList(
        p(class = "edit-hint", entity()$about),
        if (!is.null(blocked)) {
          edit_callout(
            "warning",
            title = paste("You can't", verb(), paste0(entity()$article, " ", entity()$noun), "here"), blocked,
            if (!is.null(entity()$redirect)) {
              actionButton(ns("redirect"), entity()$redirect$label, class = "btn-sm btn-primary")
            }
          )
        } else {
          switch(verb(),
            add = if (!is.null(entity()$parts)) NULL,
            update = edit_callout(
              "info",
              "Pick the one to change, then change it. Before you send anything you'll see everywhere it's used."
            ),
            delete = edit_callout(
              "danger",
              title = "Deleting needs care",
              paste0(
                "You'll see everything that would be deleted along with it, and you'll have to confirm, before ",
                "anything is sent. ",
                if (needs_approval()) {
                  "Nothing is deleted until one other staff member approves it on Change Requests."
                } else {
                  "As an ADMIN your delete is applied at once, with nothing to approve."
                }
              )
            )
          )
        }
      )
    }) |> bindEvent(entity(), verb(), load_problem(), snapshot())

    observeEvent(input$retry, {
      load_problem(NULL)
      if (!is.null(entity())) load_tables()
    })

    observeEvent(input$redirect, {
      req(entity()$redirect)
      show_page(entity()$redirect$page)
    })

    # A kind that belongs to one questionnaire asks which first.
    output$where <- renderUI({
      req(entity(), snapshot(), nzchar(verb()), !is.null(entity()$scope), is.null(can_message()))
      card(card_body(
        h2(entity()$scope$label),
        selectInput(
          ns("scope"), NULL, edit_choices(snapshot(), "questionnaire", blank = "Choose a questionnaire"),
          selected = isolate(scope()), width = "32rem", selectize = FALSE
        )
      ))
    })

    observeEvent(input$scope, {
      chosen <- input$scope %||% ""
      if (!identical(chosen, scope())) {
        scope(chosen)
        picked(NULL)
      }
    })

    observeEvent(picked(),
      {
        if (!is.null(picked())) session$sendCustomMessage("edit-scroll-to", ns("change-main"))
      },
      ignoreNULL = TRUE
    )

    # A form can ask to go to another kind, e.g. to add the registry it needs.
    observeEvent(change$jump(), {
      jump <- change$jump()
      updateSelectInput(session, "verb", selected = jump$verb)
      updateSelectInput(session, "kind", selected = jump$kind)
    })

    # ------ Steps -------------------------------------------------------------
    output$steps <- renderUI({
      steps <- edit_service_steps(
        verb(),
        has_sentence = !is.null(entity()) && nzchar(verb()), picked = !is.null(picked()),
        ready = change$ready(), sent = !is.null(done()),
        approval = if (nzchar(verb())) needs_approval() else TRUE
      )
      edit_steps_ui(steps$labels, steps$current)
    })

    # ------ Saved drafts ------------------------------------------------------
    mine <- reactive(edit_service_open(proposals(), prepared_by_id = me$staff_id, status = c("Draft", "Sent Back")))

    output$drafts <- renderUI({
      records <- mine()
      if (length(records) == 0 || !is.null(done())) {
        return(NULL)
      }
      tags$details(
        class = "edit-drafts",
        open = if (any(vapply(records, function(record) identical(record$status, "Sent Back"), logical(1)))) NA,
        tags$summary(sprintf("Your saved edits (%d)", length(records))),
        lapply(records, function(record) {
          div(
            class = "edit-draft",
            div(
              p(strong(record$summary), if (identical(record$status, "Sent Back")) row_badge("Sent back", class = "is-warning")),
              if (!is.null(record$sent_back)) {
                p(class = "edit-draft-meta", strong(paste0(record$sent_back$by, ": ")), paste0("“", record$sent_back$note, "”"))
              },
              p(class = "edit-draft-meta", paste("Saved", time_ago(parse_time(record$saved))))
            ),
            div(
              review_button("Discard", "discard", `data-id` = record$id, class = "btn btn-link"),
              review_button("Continue", "continue", `data-id` = record$id, class = "btn btn-sm btn-primary")
            )
          )
        })
      )
    })

    observeEvent(input$action, {
      event <- input$action
      record <- proposals()[[event$id %||% ""]]
      switch(event$action,
        continue = if (!is.null(record)) open_draft(record),
        discard = if (!is.null(record)) confirm_discard(record)
      )
    })

    # Puts the sentence back as it was; once the tables are read, the questionnaire
    # and the row.
    open_draft <- function(record) {
      resume(record)
      draft(record)
      done(NULL)
      updateSelectInput(session, "verb", selected = record$form$action)
      if (identical(kind(), record$form$entity)) {
        load_tables()
        restore_choices(record)
      } else {
        updateSelectInput(session, "kind", selected = record$form$entity)
      }
    }

    restore_choices <- function(record) {
      scope(as.character(record$form$scope %||% ""))
      target <- as.character(record$form$target %||% "")
      if (!nzchar(target)) {
        return()
      }
      if (is.null(snapshot()) || is.null(edit_service_row(entity(), snapshot(), target))) {
        showNotification("What this draft changes has been changed or removed since you saved it, so it can't be opened.", type = "warning")
        resume(NULL)
        return()
      }
      picked(target)
    }

    confirm_discard <- function(record) {
      showModal(confirm_dialog(
        "Discard this saved edit?",
        p(strong(record$summary)),
        p("It's set aside and won't be listed here again. Nothing has been added to the MATR data."),
        confirm_id = ns("confirm_discard"), confirm_label = "Discard", easy_close = TRUE
      ))
      discarding(record$id)
    }
    discarding <- reactiveVal(NULL)

    observeEvent(input$confirm_discard, {
      removeModal()
      tryCatch(
        {
          edit_service_withdraw(connection, discarding(), me)
          showNotification("Saved edit discarded.", type = "message")
        },
        error = function(problem) show_problem(problem, "Couldn't discard it")
      )
      saved_count(saved_count() + 1L)
    })

    # ------ The bar at the bottom ---------------------------------------------
    # The main button follows the rule (service_change_request.R): an add or an update, or anything an
    # ADMIN does, is applied at once; a STAFF member's delete is sent for approval. A request being
    # edited again is saved as a new version, which nobody has approved yet.
    primary_label <- reactive({
      if (editing_request()) {
        return("Save the new version")
      }
      if (needs_approval()) {
        return("Send for approval")
      }
      switch(verb(), add = "Add it", update = "Save the change", delete = "Delete it", "Apply")
    })

    output$footer <- renderUI({
      req(change$active(), is.null(done()))
      stuck <- change$stuck()
      sent_back <- draft()$sent_back
      tagList(
        if (!stuck) {
          div(
            class = "edit-why",
            uiOutput(ns("conflicts")),
            if (identical(draft()$status, "Sent Back") && !is.null(sent_back)) {
              edit_callout(
                "warning",
                title = paste0(sent_back$by, " sent this back"),
                p(sent_back$note), p("Change what they ask, then send it again.")
              )
            },
            card(card_body(
              textAreaInput(
                ns("note"), "Why are you making this change?",
                value = isolate(draft()$note) %||% "", rows = 2, width = "100%",
                placeholder = "Reviewers read this. Say where the correct information came from."
              ),
              if (needs_approval() && !editing_request() && length(reviewers()) > 0) {
                selectInput(
                  ns("reviewer"), "Ask someone to review it (optional)",
                  c("Anyone" = "", reviewers()), selected = isolate(draft()$requested_reviewer$id) %||% "",
                  width = "100%", selectize = FALSE
                )
              },
              if (needs_approval() && !editing_request() && length(reviewers()) > 0) {
                p(class = "form-text", "They are emailed a link once sign-in is set up. Until then, any staff member can approve it.")
              }
            ))
          )
        },
        div(
          class = "edit-footer",
          div(
            class = "edit-footer-bar",
            actionLink(ns("cancel"), if (stuck) "Start over" else "Cancel"),
            uiOutput(ns("footer_note"), class = "edit-footer-note"),
            if (!stuck) {
              tagList(
                if (!editing_request()) actionButton(ns("save_draft"), "Save as draft", class = "btn-outline-dark"),
                actionButton(ns("submit"), primary_label(), class = "btn-primary", disabled = TRUE)
              )
            }
          )
        )
      )
    })

    has_note <- reactive(has_text(input$note))

    observe({
      updateActionButton(session, "submit", disabled = !(change$ready() && has_note()))
    })

    output$footer_note <- renderUI({
      if (!change$ready()) {
        return(change$blockers()[1])
      }
      if (!has_note()) {
        return("Say why you're making this change, so reviewers know.")
      }
      if (editing_request()) {
        return("Saving makes a new version of this request. Nobody has approved it yet.")
      }
      if (needs_approval()) {
        return("Sending puts this on Change Requests. Nothing changes in the MATR data until another staff member approves it.")
      }
      "This changes the MATR data right away. It is recorded in the Edit history."
    })

    # Other open edits that change the same record: staff should talk first.
    output$conflicts <- renderUI({
      items <- tryCatch(change$built()$items, shiny.silent.error = function(problem) NULL)
      clashes <- edit_service_conflicts(proposals(), items, except = draft()$id)
      if (length(clashes) == 0) {
        return(NULL)
      }
      edit_callout(
        "warning",
        title = "Another open edit already changes this",
        lapply(clashes, function(record) {
          who <- if (identical(record$prepared_by_id, me$staff_id)) "You have" else paste(record$prepared_by, "has")
          p(strong(who), paste0(" an open edit that changes the same thing: “", record$summary, "”."))
        }),
        p("Check it first, and talk to whoever made it, so the two don't undo each other.")
      )
    })

    # ------ Saving ------------------------------------------------------------
    # A form with things still to fix can be saved as a draft: it's saved as typed, without its items.
    build_record <- function(status) {
      built <- tryCatch(change$built(), shiny.silent.error = function(problem) NULL) %||%
        edit_service_unfinished(verb(), entity())
      edit_service_record(me, change$form(), built, input$note, status, previous = draft(), reviewer = input$reviewer)
    }

    observeEvent(input$save_draft, {
      saved <- tryCatch(
        {
          saved <- edit_service_save(connection, build_record("Draft"), me)
          saved_count(saved_count() + 1L)
          saved
        },
        error = function(problem) {
          show_problem(problem, "Couldn't save")
          NULL
        }
      )
      if (is.null(saved)) {
        return()
      }
      draft(saved)
      showNotification("Saved as a draft. You can pick it up again from the top of this page.", type = "message")
    })

    # The main button. An add is checked for something like it first.
    observeEvent(input$submit, {
      req(change$ready(), has_note())
      if (identical(verb(), "add") && !editing_request()) {
        found <- tryCatch(
          edit_service_duplicates(entity(), snapshot(), change$fields(), change$values()),
          shiny.silent.error = function(problem) list()
        )
        if (length(found) > 0) {
          duplicates(found)
          showModal(edit_duplicates_dialog(found, ns("confirm_duplicate")))
          return()
        }
      }
      proceed()
    })

    observeEvent(input$confirm_duplicate, {
      removeModal()
      duplicates(NULL)
      proceed()
    })

    # Saves, then does what the rule says: applies it, sends it for approval, or saves a new version.
    proceed <- function() {
      outcome <- tryCatch(
        {
          if (editing_request()) {
            saved <- edit_service_save(connection, build_record("Awaiting Review"), me)
            list(applied = FALSE, record = saved, resaved = TRUE)
          } else if (needs_approval()) {
            saved <- edit_service_send(connection, build_record("Awaiting Review"), me)
            list(applied = FALSE, record = saved)
          } else {
            saved <- edit_service_save(connection, build_record("Draft"), me)
            draft(saved) # if the database refuses it, what was typed is still a saved edit
            saved_count(saved_count() + 1L)
            list(applied = TRUE, record = saved, history_id = edit_service_apply(connection, saved, me))
          }
        },
        error = function(problem) {
          show_problem(problem, if (needs_approval()) "Couldn't send it" else "Couldn't apply it")
          NULL
        }
      )
      if (is.null(outcome)) {
        return()
      }
      saved_count(saved_count() + 1L)
      done(outcome)
      clear_page()
    }

    # ------ Starting over ------------------------------------------------------
    clear_page <- function() {
      picked(NULL)
      scope("")
      draft(NULL)
      resume(NULL)
      updateSelectInput(session, "verb", selected = "")
      updateSelectInput(session, "kind", selected = "")
    }

    observeEvent(input$cancel, {
      showModal(modalDialog(
        title = "Cancel this edit?",
        "What you've typed isn't saved. Save it as a draft first if you want to come back to it.",
        footer = tagList(modalButton("Keep editing"), actionButton(ns("confirm_cancel"), "Cancel the edit", class = "btn-primary")),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_cancel, {
      removeModal()
      clear_page()
    })

    output$sent <- renderUI({
      outcome <- done()
      req(outcome)
      record <- outcome$record
      card(class = "edit-sent", card_body(
        h2(
          span(class = "edit-sent-mark", icon("check")),
          if (isTRUE(outcome$applied)) "Done" else if (isTRUE(outcome$resaved)) "Saved" else "Sent for approval"
        ),
        p(strong(record$summary)),
        p(
          if (isTRUE(outcome$applied)) {
            "It is in the database now, and recorded in the Edit history."
          } else if (isTRUE(outcome$resaved)) {
            "This is a new version of the request. It is waiting on Change Requests, where one other staff member has to approve it."
          } else {
            "It is waiting on Change Requests, where one other staff member has to approve it. Nothing has changed in the MATR data yet."
          }
        ),
        div(
          actionButton(ns("another"), "Make another edit", class = "btn-primary"),
          if (isTRUE(outcome$applied)) {
            actionButton(ns("to_history"), "See the Edit history", class = "btn-outline-dark")
          } else {
            actionButton(ns("to_change_requests"), "Go to Change Requests", class = "btn-outline-dark")
          }
        )
      ))
    })

    observeEvent(input$another, done(NULL))
    observeEvent(input$to_history, show_page("edit_history"))
    observeEvent(input$to_change_requests, show_page("change_requests"))

    # ------ RETURN ------------------------------------------------------------
    list(open = open_draft)
  })
}
