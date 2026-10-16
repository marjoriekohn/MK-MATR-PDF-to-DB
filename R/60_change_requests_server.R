#' The Change Requests page's server.
#'
#' The requests waiting for approval (a STAFF member's delete, or an extraction they submitted),
#' longest waiting first. "Review" shows what a request changes; from there a person other than
#' who prepared or last changed it approves it, which adds it to the MATR metadata database at once, or sends
#' it back with a note; whoever prepared it can withdraw it; anyone can open it to edit it, which
#' makes a new revision. The database does the approving and the applying and checks the rules
#' again (`model_change_request.R`); this page offers the buttons that make sense.
#'
#' An extraction's Basic Info (the registry, study, questionnaire and wave it goes into) is filled in
#' and saved here, in its dialog (`extraction_info_server()`). Saving it is a change like any other: it
#' makes a new revision, so whoever saves it can't approve it. Approve (and Add, for an ADMIN's own
#' request) stay disabled until the Basic Info is saved, complete and as the form shows it.
#'
#' @param id The module's id.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_edit A function taking an edit request's record and opening it on Make an edit.
#' @param open_edit_extraction A function taking an extraction request's record and opening it on the Edit Extraction page.
#' @param catalog The live tables the Basic Info form picks from, as `server.R` shares them: `get()`
#'   reads them and `refresh()` reads them again.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list of
#'   records, and `changed()` tells it this session changed one.
#' @return Called for its effect.
change_requests_server <- function(
  id,
  connection,
  me,
  open_edit,
  open_edit_extraction,
  catalog,
  extractions
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    whose <- reactiveVal("me") # "me", "mine" or "everyone"
    refreshed <- reactiveVal(0L) # ticks when this session approved, sent back or withdrew something
    reviewing <- reactiveVal(NULL) # the request the dialog is about, as it was read
    reviewing_detail <- reactiveVal(NULL) # what the dialog shows of it, from request_detail()

    # Read again when the database says something changed (other staff send requests and approve them
    # too) and straight after this session did. If the database can't be reached the last requests
    # read stay on screen, with a message, and the next change tries again.
    stored <- reactivePoll(
      change_requests_poll_ms, session,
      checkFunc = function() change_request_service_signature(connection),
      valueFunc = function() change_request_service_signature(connection)
    )
    last_requests <- list()
    requests <- reactive({
      refreshed()
      stored()
      tryCatch(
        {
          last_requests <<- change_request_service_waiting(connection)
          last_requests
        },
        error = function(problem) {
          showNotification(conditionMessage(problem), type = "error", id = "database_problem")
          last_requests
        }
      )
    })
    shown <- reactive(change_request_service_filter(requests(), me, whose()))

    observeEvent(input$whose, whose(input$whose))

    observeEvent(input$action, {
      event <- input$action
      record <- requests()[[event$id %||% ""]]
      if (identical(event$action, "review") && !is.null(record)) show_request(record)
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

    # What a request comes to, laid out as its preparer saw it before submitting it. An extraction's
    # rows are worked out from the database as it is now, so the reviewer is told at once if it can't be
    # added as it stands; its questionnaire is read so the dialog can lay it out, and its PDF is served
    # to this session if the file is here.
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
      list(preview = preview, draft = draft, pdf_url = request_pdf_url(record))
    }

    # The address the browser gets the request's PDF from, for this session only; NULL if the file
    # isn't on this server (see `pdf_address()`).
    request_pdf_url <- function(record) {
      pdf <- record$pdf
      if (length(pdf$path) == 0 || !file.exists(pdf$path)) {
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
      saved <- tryCatch(
        extraction_service_save_info(connection, opened, form_info(), me, catalog$get()),
        error = function(problem) {
          show_problem(problem, "Couldn't save the Basic Info")
          NULL
        }
      )
      if (is.null(saved)) {
        return()
      }
      extractions$changed()
      refreshed(refreshed() + 1L)
      showNotification("Basic Info saved.", type = "message")
      show_request(saved) # what it adds is worked out again for where it goes now
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

    # What was approved is what the person was shown: the revision they opened.
    observeEvent(input$approve, {
      opened <- reviewing()
      req(opened, ready_to_add())
      removeModal()
      tryCatch(
        {
          change_request_service_approve(connection, opened, me)
          showNotification("Approved. It is in the MATR metadata database now, and in the Edit history.", type = "message")
        },
        error = function(problem) show_problem(problem, "Couldn't approve it")
      )
      reviewing(NULL)
      refreshed(refreshed() + 1L)
      extractions$changed()
    })

    observeEvent(input$add, {
      opened <- reviewing()
      req(opened, ready_to_add())
      removeModal()
      tryCatch(
        {
          change_request_service_add(connection, opened, me)
          showNotification("Added to the MATR metadata database. It is in the Edit history.", type = "message")
        },
        error = function(problem) show_problem(problem, "Couldn't add it")
      )
      reviewing(NULL)
      refreshed(refreshed() + 1L)
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
        p("It is set aside and won't be asked of anyone. Nothing has been added to the MATR metadata database."),
        confirm_id = ns("confirm_withdraw"), confirm_label = "Withdraw it", easy_close = TRUE
      ))
    })

    observeEvent(input$confirm_withdraw, {
      opened <- reviewing()
      req(opened)
      removeModal()
      tryCatch(
        {
          change_request_service_withdraw(connection, opened, me)
          showNotification("Withdrawn.", type = "message")
        },
        error = function(problem) show_problem(problem, "Couldn't withdraw it")
      )
      reviewing(NULL)
      refreshed(refreshed() + 1L)
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
