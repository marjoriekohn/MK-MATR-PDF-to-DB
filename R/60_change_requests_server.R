# The Change Requests page's server.
# The requests waiting for approval (a STAFF member's delete, or an extraction they submitted),
# longest waiting first. "Review" shows what a request changes; from there a person other than
# who prepared or last changed it approves it, which adds it to the MATR metadata database at once, or sends
# it back with a note; whoever prepared it can withdraw it; anyone can open it to edit it, which
# makes a new revision. The database does the approving and the applying and checks the rules
# again (`model_change_request.R`); this page offers the buttons that make sense.
# An extraction's Basic Info (the registry, study, questionnaire and wave it goes into) is filled in
# and saved here, in its dialog (`extraction_info_server()`). Saving it is a change like any other: it
# makes a new revision, so whoever saves it can't approve it. Approve (and Add, for an ADMIN's own
# request) stay disabled until the Basic Info is saved, complete and as the form shows it.
# open_edit: A function taking an edit request's record and opening it on Make an edit.
# open_edit_extraction: A function taking an extraction request's record and opening it on the Edit Extraction page.
# catalog: The live tables the Basic Info form picks from, as `server.R` shares them: `get()`
#   reads them and `refresh()` reads them again.
# active: A reactive that says whether this page is the one on screen, or `NULL`. Opening the
#   page reads the requests again, since other staff send and approve them too.
change_requests_server <- function(
  id,
  connection,
  me,
  open_edit,
  open_edit_extraction,
  catalog,
  extractions,
  active = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    whose <- reactiveVal("me") # "me", "mine" or "everyone"
    refreshed <- reactiveVal(0L) # ticks when the requests should be read again
    reviewing <- reactiveVal(NULL) # the request the dialog is about, as it was read
    reviewing_detail <- reactiveVal(NULL) # what the dialog shows of it, from request_detail()

    # Read when the page is opened (other staff send requests and approve them too) and straight after this session changed one; the database isn't asked over and over. If it
    # can't be reached the last requests read stay on screen, with a message, and the next read tries again.
    if (!is.null(active)) {
      observeEvent(active(), if (isTRUE(active())) refreshed(refreshed() + 1L), ignoreInit = TRUE)
    }
    read_requests <- last_good_read(function() change_request_service_waiting(connection))
    requests <- reactive({
      refreshed()
      read_requests()
    })
    shown <- reactive(change_request_service_filter(requests(), me, whose()))

    observeEvent(input$whose, whose(input$whose))

    observeEvent(input$action, {
      event <- input$action
      record <- requests()[[event$id %||% ""]]
      if (is.null(record)) {
        return()
      }
      if (identical(event$action, "review")) show_request(record)
      if (identical(event$action, "preview")) show_preview(record)
    })

    # An extraction's questionnaire, read afresh, as Edit Extraction's Preview shows it, with Edit and Withdraw
    # (which act on `reviewing()`). Its last button goes on to the request's Basic Info dialog, in its place.
    previewing <- reactiveVal(NULL)
    show_preview <- function(record) {
      previewing(record)
      reviewing(record)
      draft <- tryCatch(draft_service_load(connection, record$id), error = function(problem) NULL)
      shown_draft(draft)
      showModal(change_request_preview_dialog(record, me, ns, draft, request_pdf_url(record), table = request_table()))
    }

    # The Preview's table, drawn once the dialog is open: a DataTables table put in a dialog as it opens stays
    # empty. Each Preview gets an output of its own, so the table is drawn anew; the one before goes (an output
    # set to NULL stops drawing), so a long session doesn't keep one for every Preview it opened.
    shown_draft <- reactiveVal(NULL)
    table_count <- 0L
    request_table <- function() {
      if (table_count > 0) output[[paste0("request_table_", table_count)]] <- NULL
      table_count <<- table_count + 1L
      name <- paste0("request_table_", table_count)
      output[[name]] <- renderUI(questionnaire_preview_ui(req(shown_draft())))
      uiOutput(ns(name))
    }
    observeEvent(input$preview_review, {
      record <- req(previewing())
      previewing(NULL)
      show_request(record)
    })

    # The dialog for a request, read afresh; an extraction's Basic Info form is filled from it.
    show_request <- function(record) {
      extraction <- identical(record$kind, extraction_kind)
      if (extraction) catalog$refresh()
      detail <- request_detail(record)
      reviewing(record)
      reviewing_detail(detail)
      showModal(change_request_dialog(record, me, ns, detail))
      if (extraction) info_form$show(record)
    }

    # What a request comes to, for its dialog. An extraction's rows are worked out from its questionnaire and the
    # database as it is now, so the reviewer is told at once if it can't be added as it stands (the questionnaire
    # itself is the Preview's).
    request_detail <- function(record) {
      if (!identical(record$kind, extraction_kind)) {
        return(change_request_service_changes(record))
      }
      draft <- tryCatch(draft_service_load(connection, record$id), error = function(problem) NULL)
      info <- extraction_service_info(record)
      preview <- tryCatch(
        if (is.null(draft)) {
          list(what = list(), problem = "The questionnaire couldn't be read.")
        } else {
          extraction_service_request_preview(
            draft, info, catalog$get(), extraction_service_push_context(connection),
            extraction_service_push_existing(connection, info)
          )
        },
        error = function(problem) list(problem = conditionMessage(problem))
      )
      list(preview = preview)
    }

    # The address the browser gets the request's PDF from, for this session only (see `pdf_address()`);
    # NULL if there is none to be had (`extraction_service_pdf()`).
    request_pdf_url <- function(record) {
      pdf <- extraction_service_pdf(connection, record)
      if (is.null(pdf)) {
        return(NULL)
      }
      pdf_address(session, ns(paste0("file-", record$id)), list(path = pdf$path, name = pdf$name, sha256 = pdf$sha256))
    }

    # ------ An extraction's Basic Info ------------------------------------------------------------
    # The form holds what is typed; the request holds what was saved. Approving uses what was saved.
    # The pop-up that adds a study or a Respondent Type takes the dialog's place, so the dialog is put
    # back as it was when it closes, and the form filled again.
    info_form <- extraction_info_server(
      "info", catalog, extractions, reactive(reviewing()$id), me, connection,
      reopen = function() {
        opened <- isolate(reviewing())
        if (!is.null(opened)) showModal(change_request_dialog(opened, me, ns, isolate(reviewing_detail())))
      }
    )

    form_info <- reactive(info_form$info())
    saved_info <- reactive(extraction_service_place(extraction_service_basic_info(extraction_service_info(req(reviewing()))), catalog$get()))
    info_unsaved <- reactive({
      req(identical(reviewing()$kind, extraction_kind))
      !identical(form_info(), saved_info())
    })

    # What stops the request being approved, from the Basic Info as saved.
    info_blockers <- reactive({
      opened <- req(reviewing())
      req(identical(opened$kind, extraction_kind))
      info <- extraction_service_basic_info(extraction_service_info(opened))
      counts <- tryCatch(
        questionnaire_service_wave_question_counts(connection, info$questionnaire_id),
        error = function(problem) integer()
      )
      extraction_service_info_blockers(opened, info, catalog$get(), extractions$all(), counts)
    })

    observe({
      req(identical(reviewing()$kind, extraction_kind))
      ready <- !info_unsaved() && length(info_blockers()) == 0
      updateActionButton(session, "save_info", disabled = !info_unsaved())
      updateActionButton(session, "approve", disabled = !ready)
      updateActionButton(session, "add", disabled = !ready)
    })

    output$info_status <- renderUI({
      if (info_unsaved()) {
        return(span(class = "save-status is-dirty", "Unsaved changes"))
      }
      blockers <- info_blockers()
      if (length(blockers) > 0) {
        return(span(class = "form-text", blockers[1]))
      }
      span(class = "save-status", "Basic Info saved")
    })

    observeEvent(input$save_info, {
      opened <- req(reviewing())
      saved <- try_showing(extraction_service_save_info(connection, opened, form_info(), me, catalog$get()), "Couldn't save the Basic Info")
      if (is.null(saved)) {
        return()
      }
      extractions$changed()
      refreshed(refreshed() + 1L)
      showNotification("Basic Info saved.", type = "message")
      # what it adds is worked out again for where it goes now, and drawn in place: the form stays as it is
      reviewing(saved)
      detail <- request_detail(saved)
      reviewing_detail(detail)
      parts <- change_request_parts(saved, me, ns, detail, ready = length(info_blockers()) == 0)
      for (part in names(parts)) {
        where <- paste0("#", ns(paste0("request_", part)))
        removeUI(paste(where, "> *"), multiple = TRUE)
        insertUI(where, "afterBegin", parts[[part]])
      }
    })

    # Approving, or adding an ADMIN's own request, can't go ahead with Basic Info missing or unsaved.
    ready_to_add <- function() {
      if (!identical(reviewing()$kind, extraction_kind)) {
        return(TRUE)
      }
      if (info_unsaved()) {
        showNotification("Save the Basic Info first.", type = "warning")
        return(FALSE)
      }
      blockers <- info_blockers()
      if (length(blockers) > 0) {
        showNotification(blockers[1], type = "warning")
        return(FALSE)
      }
      TRUE
    }

    # Approving, adding or withdrawing the request open: the dialog closes, `act` runs (saying `done` if it worked,
    # or what went wrong after `doing`), and the list is read again without it.
    finish_request <- function(act, done, doing) {
      removeModal()
      try_showing({
        act
        showNotification(done, type = "message")
      }, doing)
      reviewing(NULL)
      refreshed(refreshed() + 1L)
    }

    # What was approved is what the person was shown: the revision they opened.
    observeEvent(input$approve, {
      opened <- reviewing()
      req(opened, ready_to_add())
      finish_request(
        change_request_service_approve(connection, opened, me),
        "Approved. It is in the MATR metadata database now, and in the Edit History.", "Couldn't approve it"
      )
      extractions$changed()
    })

    observeEvent(input$add, {
      opened <- reviewing()
      req(opened, ready_to_add())
      finish_request(
        change_request_service_add(connection, opened, me),
        "Added to the MATR metadata database. It is in the Edit History.", "Couldn't add it"
      )
      extractions$changed()
    })

    observeEvent(input$send_back, {
      opened <- reviewing()
      req(opened)
      showModal(change_request_send_back_dialog(opened, ns))
    })

    observeEvent(input$confirm_send_back, {
      opened <- reviewing()
      req(opened)
      sent <- tryCatch(
        {
          change_request_service_send_back(connection, opened, me, input$send_back_note)
          TRUE
        },
        error = function(problem) {
          show_problem(problem, "Couldn't send it back", type = "warning")
          FALSE
        }
      )
      if (!sent) {
        return()
      }
      removeModal()
      showNotification(paste0("Sent back to ", opened$prepared_by, "."), type = "message")
      reviewing(NULL)
      refreshed(refreshed() + 1L)
    })

    observeEvent(input$withdraw, {
      opened <- reviewing()
      req(opened)
      showModal(confirm_dialog(
        "Withdraw this request?",
        p(strong(opened$summary)),
        p(if (identical(opened$kind, extraction_kind)) {
          "The extraction is deleted, with its PDF, and can't be got back. Nothing has been added to the MATR metadata database."
        } else {
          "It is set aside and won't be asked of anyone. Nothing has been added to the MATR metadata database."
        }),
        confirm_id = ns("confirm_withdraw"), confirm_label = "Withdraw it", easy_close = TRUE
      ))
    })

    observeEvent(input$confirm_withdraw, {
      opened <- reviewing()
      req(opened)
      finish_request(change_request_service_withdraw(connection, opened, me), "Withdrawn.", "Couldn't withdraw it")
    })

    # Editing opens the request where it was made, with everything filled in. Saving there makes a new
    # revision, which nobody has approved yet.
    observeEvent(input$edit, {
      opened <- reviewing()
      req(opened)
      removeModal()
      if (identical(opened$kind, extraction_kind)) open_edit_extraction(opened) else open_edit(opened)
      reviewing(NULL)
    })

    # ------ OUTPUT ------------------------------------------------------------
    output$whose_toggle <- renderUI({
      change_request_whose_buttons(
        ns("whose"), whose(),
        length(change_request_service_filter(requests(), me, "me")),
        length(change_request_service_filter(requests(), me, "mine")),
        length(requests())
      )
    })

    output$cases <- renderUI({
      if (length(shown()) == 0) {
        return(no_change_requests(whose()))
      }
      lapply(unname(shown()), change_request_card_ui, me = me)
    })
  })
}
