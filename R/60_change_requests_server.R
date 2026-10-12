#' The Change Requests page's server.
#'
#' The requests waiting for approval (a STAFF member's delete, or an extraction they pushed),
#' longest waiting first. "Review" shows what a request changes; from there a person other than
#' who prepared or last changed it approves it, which adds it to the MATR data at once, or sends
#' it back with a note; whoever prepared it can withdraw it; anyone can open it to edit it, which
#' makes a new revision. The database does the approving and the applying and checks the rules
#' again (`model_change_request.R`); this page offers the buttons that make sense.
#'
#' @param id The module's id.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_edit A function taking an edit request's record and opening it on Make an edit.
#' @param open_review A function taking an extraction request's record and opening it on the review page.
#' @return Called for its effect.
change_requests_server <- function(
  id,
  connection,
  me,
  open_edit,
  open_review
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    whose <- reactiveVal("me") # "me", "mine" or "everyone"
    refreshed <- reactiveVal(0L) # ticks when this session approved, sent back or withdrew something
    reviewing <- reactiveVal(NULL) # the request the dialog is about, as it was read

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
      if (identical(event$action, "review") && !is.null(record)) {
        reviewing(record)
        showModal(change_request_dialog(record, me, ns, request_detail(record)))
      }
    })

    # What a request comes to. An extraction's rows are worked out from the MATR data as it is now, so
    # the reviewer is told at once if it can't be added as it stands; its questionnaire is read so the
    # dialog can lay it out, and its PDF is served to this session if the file is here.
    request_detail <- function(record) {
      if (!identical(record$kind, extraction_kind)) {
        return(change_request_service_changes(record))
      }
      draft <- tryCatch(draft_service_load(connection, record$id), error = function(problem) NULL)
      detail <- tryCatch(
        {
          built <- extraction_service_push_build(connection, record)
          list(
            summary = extraction_service_push_summary(built$summary),
            unchecked = if (is.null(draft)) 0 else extraction_service_unchecked(draft)
          )
        },
        error = function(problem) list(problem = conditionMessage(problem), unchecked = 0)
      )
      c(detail, list(draft = draft, pdf_url = request_pdf_url(record)))
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

    # What was approved is what the person was shown: the revision they opened.
    observeEvent(input$approve, {
      opened <- reviewing()
      req(opened)
      removeModal()
      tryCatch(
        {
          change_request_service_approve(connection, opened, me)
          showNotification("Approved. It is in the MATR data now, and in the Edit history.", type = "message")
        },
        error = function(problem) show_problem(problem, "Couldn't approve it")
      )
      reviewing(NULL)
      refreshed(refreshed() + 1L)
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
        p("It is set aside and won't be asked of anyone. Nothing has been added to the MATR data."),
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
      if (identical(opened$kind, extraction_kind)) open_review(opened) else open_edit(opened)
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
