# How many changes Undo can take back.
undo_depth <- 50

#' The review page's server, for one extraction draft.
#'
#' The outline of what was extracted, every block edited where it is (`review_outline_server()`).
#' Other pages open a draft here with the `open` function this returns. Changes stay in this session
#' until Save writes them to the draft's reviewed copy in the database. Push sends the saved copy to
#' the database: a STAFF member's push goes to Change Requests for one other staff member to approve,
#' an ADMIN's is added at once (`service_extraction_push.R`). Questions are numbered by their place in
#' the outline, and a new wave of a recorded questionnaire gets the MATR IDs its questions already have.
#'
#' @param id The module's id.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param catalog The live tables the pages pick from, as `server.R` shares them: `get()` reads them
#'   (`questionnaire_service_get_catalog()`) and `refresh()` reads them again.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param show_page A function taking a page's name and showing it.
#' @return A list with `open`, a function taking a record that opens it, unless the draft on
#'   screen has unsaved changes and staff choose to stay; it returns whether it opened.
review_server <- function(
  id,
  extractions,
  catalog,
  connection,
  me,
  show_page
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ INITIALIZE --------------------------------------------------------
    lookups <- lookup_service_get_all(connection)
    # A pick list that couldn't be read is empty; say why instead of leaving it blank.
    for (problem in lookups$problems) showNotification(problem, type = "warning", duration = 20)

    # ------ REACTIVE ----------------------------------------------------------
    record_id <- reactiveVal(NULL) # the extraction draft open on this page
    record <- reactive(extractions$all()[[req(record_id())]]) # as last saved, by anyone
    draft <- reactiveVal(NULL) # its questionnaire as it is on screen
    saved <- reactiveVal(NULL) # its questionnaire as it was last saved
    undo_stack <- reactiveVal(list()) # earlier drafts, newest first
    selected <- reactiveVal(NULL) # id of the selected block
    layout_version <- reactiveVal(0L) # ticks when the outline must rebuild its inputs
    waiting <- reactiveVal(NULL) # a draft to open once unsaved changes are dealt with
    recorded <- reactiveVal(NULL) # what the recorded questionnaire prints already (a new wave); see extraction_service_recorded()
    problems <- reactive(draft_service_check(req(draft()), lookups))
    matr_ids <- reactive(extraction_service_matr_ids(record()$matr_csv$text %||% "")) # from the CSV
    has_unsaved_changes <- reactive(!identical(draft(), saved()))
    heading <- reactive(extraction_service_heading(record(), catalog$get(), req(draft())$questionnaire))
    suggestions <- reactive(draft_service_text_suggestions(req(draft()), recorded()$texts))

    # Every change to the draft comes through here so it can be undone. Service
    # functions explain themselves: their stop() and message() texts are shown
    # to staff. `typed` is for words typed into a field, which the field already
    # shows; rebuilding it would move the cursor. A block that was marked as checked and has
    # been edited is no longer checked, unless `keep_checked`. The questions are numbered again by
    # their place in the outline. Returns TRUE if it worked.
    change_draft <- function(new_draft, typed = FALSE, keep_checked = FALSE) {
      new_draft <- tryCatch(
        withCallingHandlers(new_draft, message = function(note) {
          showNotification(conditionMessage(note), type = "message")
          invokeRestart("muffleMessage")
        }),
        error = function(problem) {
          show_problem(problem, type = "warning")
          NULL
        }
      )
      if (is.null(new_draft)) {
        layout_version(layout_version() + 1L) # put the inputs back as the draft has them
        return(FALSE)
      }
      # A block that was marked as checked and has been edited is no longer checked.
      if (!keep_checked) new_draft <- draft_service_uncheck_edited(draft(), new_draft)
      new_draft <- draft_service_renumber_questions(new_draft)
      if (!identical(new_draft, draft())) {
        undo_stack(c(list(draft()), head(undo_stack(), undo_depth - 1)))
        draft(new_draft)
        if (!typed) layout_version(layout_version() + 1L)
      }
      TRUE
    }

    # What the outline module is given.
    state <- list(
      draft = reactive(req(draft())),
      change = change_draft,
      selected = selected,
      problems = problems,
      layout_version = layout_version,
      suggestions = suggestions
    )

    observeEvent(input$undo, {
      req(length(undo_stack()) > 0)
      draft(undo_stack()[[1]])
      undo_stack(undo_stack()[-1])
      layout_version(layout_version() + 1L)
      if (!isTRUE(selected() %in% draft()$components$id)) selected(NULL)
    })

    observeEvent(input$save, save_draft())

    # The page saves what has changed since it opened the draft or last saved it (`saved()`); for an
    # AI-Powered draft that is also kept as the reviewer's edits (`extraction_service_save_draft()`).
    save_draft <- function() {
      tryCatch(
        {
          extraction_service_save_draft(connection, record(), draft(), saved(), me)
          saved(draft())
          extractions$changed()
          showNotification("Saved. Nothing is added to the MATR data until the draft is pushed.", type = "message")
          TRUE
        },
        error = function(problem) {
          show_problem(problem, "Couldn't save")
          FALSE
        }
      )
    }

    # ------ Opening a draft ---------------------------------------------------
    # Opens `opened` (an extraction record), unless the draft on screen has
    # unsaved changes: then staff choose whether to save them first. Returns
    # FALSE if the draft couldn't be opened.
    open <- function(opened) {
      if (identical(opened$id, isolate(record_id()))) {
        return(TRUE)
      }
      if (!isolate(has_unsaved_changes())) {
        return(show_draft(opened))
      }
      waiting(opened)
      showModal(modalDialog(
        title = sprintf("Save your changes to %s?", isolate(heading()$title)),
        "They aren't saved yet. Save them before opening the next draft?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("discard_and_open"), "Don't save", class = "btn-outline-dark"),
          actionButton(ns("save_and_open"), "Save", class = "btn-primary")
        )
      ))
      TRUE
    }

    show_draft <- function(opened) {
      loaded <- tryCatch(extraction_service_load_draft(connection, opened), error = function(problem) {
        show_problem(problem, "Couldn't open this draft")
        NULL
      })
      if (is.null(loaded)) {
        return(FALSE)
      }
      # The questions are numbered by their place in the outline; the saved copy gets the numbers on the next save.
      loaded <- draft_service_renumber_questions(loaded)
      record_id(opened$id)
      draft(loaded)
      saved(loaded)
      undo_stack(list())
      selected(NULL)
      layout_version(layout_version() + 1L)
      read_recorded(opened)
      fill_recorded_matr_ids()
      TRUE
    }

    # ------ What the recorded questionnaire has already -----------------------------
    # For a new wave of a recorded questionnaire: the words its blocks have (offered while typing) and
    # its questions' MATR IDs (filled in where the question is worded the same). Read when a draft opens
    # and when its details change; a read that fails leaves nothing to offer, with a warning.
    read_recorded <- function(opened) {
      recorded(tryCatch(
        extraction_service_recorded(connection, opened),
        error = function(problem) {
          show_problem(problem, "Couldn't read what this questionnaire has already", type = "warning")
          NULL
        }
      ))
    }

    # Blank MATR IDs of questions the recorded questionnaire already asks are filled in. It is a change
    # like any other (Undo takes it back, Save keeps it) that doesn't take a check off.
    fill_recorded_matr_ids <- function() {
      known <- recorded()$matr_ids
      if (is.null(known) || nrow(known) == 0 || is.null(draft())) {
        return(invisible(FALSE))
      }
      change_draft(draft_service_fill_recorded_matr_ids(draft(), known), keep_checked = TRUE)
    }

    observeEvent(input$save_and_open, {
      removeModal()
      if (save_draft()) show_draft(waiting())
    })

    observeEvent(input$discard_and_open, {
      removeModal()
      show_draft(waiting())
    })

    # ------ About the questionnaire and wave -----------------------------------
    # What staff typed while the PDF was being read, changed in a dialog. It
    # isn't part of the questionnaire, so the dialog's Save writes it straight
    # to the draft's record, and Undo and the page's Save leave it alone.
    observeEvent(input$edit_info, {
      showModal(modalDialog(
        title = "About the questionnaire & wave",
        size = "l",
        extraction_info_ui(ns("info")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_info"), "Save", class = "btn-primary")
        )
      ))
      info_form$show(record())
    })

    observeEvent(input$save_info, {
      tryCatch(
        {
          saved_record <- extraction_service_save_info(connection, record_id(), info_form$info(), me)
          extractions$changed()
          removeModal()
          showNotification("Questionnaire and wave details saved.", type = "message")
          # the questionnaire may be another one now, with other blocks and MATR IDs to offer
          read_recorded(saved_record)
          fill_recorded_matr_ids()
        },
        error = function(problem) {
          show_problem(problem, "Couldn't save")
        }
      )
    })

    # ------ MATR IDs ----------------------------------------------------------
    # The CSV lists one MATR ID per variable, in printed order. Like the wave
    # details, it belongs to the draft's record, so adding one saves it at once.
    # Filling the variables from it changes the questionnaire, so Undo and the
    # page's Save apply.
    show_matr_ids <- function() {
      known <- recorded()$matr_ids
      showModal(modalDialog(
        title = "Variable IDs",
        if (!is.null(known) && nrow(known) > 0) {
          tagList(
            p(
              "This is a new wave of a recorded questionnaire, so a question worded the same as one it already asks ",
              "gets the MATR IDs that question has. That was done when the draft opened; do it again after changing ",
              "a question's words."
            ),
            p(actionButton(ns("fill_recorded"), "Fill from the earlier waves", class = "btn-outline-dark"))
          )
        },
        p(
          "A CSV lists MATR IDs in the order they appear on the questionnaire.",
          "Filling pairs them with the variables from top to bottom: the first",
          "variable gets the first ID, and so on."
        ),
        drop_zone(
          fileInput(ns("matr_csv"), NULL, accept = c(".csv", "text/csv"), buttonLabel = "Choose file") |>
            tagAppendAttributes(class = "quiet-upload"),
          uiOutput(ns("matr_csv_status")),
          icon = "file-lines"
        ),
        uiOutput(ns("matr_fill_note")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("fill_matr_ids"), "Fill MATR IDs",
            class = "btn-primary",
            disabled = length(matr_ids()) == 0
          )
        )
      ))
    }

    observeEvent(input$matr_csv, {
      upload <- input$matr_csv
      tryCatch(
        {
          extraction_service_add_matr_csv(connection, record_id(), upload, me)
          extractions$changed()
        },
        error = function(problem) {
          show_problem(problem, "Couldn't add the CSV")
        }
      )
    })

    observeEvent(matr_ids(), {
      updateActionButton(session, "fill_matr_ids", disabled = length(matr_ids()) == 0)
    })

    observeEvent(input$fill_matr_ids, {
      # Filling from the CSV is its own step, with its own count, so it doesn't take checks off.
      if (change_draft(draft_service_fill_matr_ids(draft(), matr_ids()), keep_checked = TRUE)) removeModal()
    })

    observeEvent(input$fill_recorded, {
      before <- draft()
      fill_recorded_matr_ids()
      if (identical(before, draft())) {
        showNotification("Nothing to fill in: every question worded like a recorded one has its MATR IDs already.", type = "message")
      } else {
        removeModal()
      }
    })

    # The header's buttons: "to fix" jumps to the next block with a problem, Upload MATR IDs opens its dialog.
    observeEvent(input$action, {
      switch(input$action$action,
        next_problem = selected(draft_service_next_problem(draft(), problems(), selected())),
        matr_ids = show_matr_ids()
      )
    })

    observe({
      updateActionButton(session, "undo", disabled = length(undo_stack()) == 0)
      updateActionButton(session, "save", disabled = !has_unsaved_changes())
    })

    # ------ Pushing to the database ---------------------------------------------
    # What is pushed is the saved copy, so anything unsaved is saved first. A STAFF member's push is
    # sent to Change Requests; an ADMIN's is added at once and shows in the Edit history.
    needs_approval <- change_request_service_needs_approval(me, extraction_kind)
    reviewers <- reactive(tryCatch(staff_service_reviewers(connection, me), error = function(problem) character()))

    observeEvent(input$push, {
      req(record_id(), draft())
      if (has_unsaved_changes() && !save_draft()) {
        return()
      }
      show_push_dialog()
    })

    show_push_dialog <- function() {
      blockers <- extraction_service_push_blockers(record(), draft(), lookups, catalog$get(), extractions$all())
      summary <- ""
      if (length(blockers) == 0) {
        # the rows are worked out against the MATR data as it is now, so a clash shows here
        tryCatch(
          summary <- extraction_service_push_summary(extraction_service_push_build(connection, record())$summary),
          error = function(problem) blockers <<- conditionMessage(problem)
        )
      }
      showModal(push_dialog(
        ns, blockers, summary, extraction_service_unchecked(draft()), needs_approval, reviewers(),
        note = record()$info$notes %||% ""
      ))
    }

    observeEvent(input$confirm_push, {
      req(record_id(), draft(), !has_unsaved_changes())
      outcome <- tryCatch(
        extraction_service_push(
          connection, record(), me, catalog$get(),
          note = input$push_note, reviewer = input$push_reviewer
        ),
        error = function(problem) {
          show_problem(problem, if (needs_approval) "Couldn't send it" else "Couldn't add it")
          NULL
        }
      )
      if (is.null(outcome)) {
        return()
      }
      removeModal()
      extractions$changed()
      if (outcome$applied) {
        showNotification("Added to the MATR data. It is in the Edit history.", type = "message")
        close_draft()
        show_page("edit_history")
      } else {
        showNotification("Sent for approval. It is waiting on Change Requests.", type = "message")
        show_page("change_requests")
      }
    })

    # A pushed draft is done with: the page goes back to showing nothing.
    close_draft <- function() {
      record_id(NULL)
      draft(NULL)
      saved(NULL)
      undo_stack(list())
      selected(NULL)
    }

    # ------ MODULES -----------------------------------------------------------
    review_outline_server("outline", state, lookups)
    info_form <- extraction_info_server("info", catalog, extractions, record_id, me, connection)

    # ------ OUTPUT ------------------------------------------------------------
    output$title <- renderText(paste("Review", heading()$title))
    output$subtitle <- renderText(heading()$subtitle)

    # The PDF, if the file is on this server, opens in its own window. The address changes with the
    # file, so a different PDF is not served from the browser's cache.
    output$pdf_link <- renderUI({
      pdf <- req(record())$pdf
      if (length(pdf$path) == 0 || !file.exists(pdf$path)) {
        return(NULL)
      }
      pdf_open_button(pdf_address(session, ns("file"), list(path = pdf$path, name = pdf$name, sha256 = pdf$sha256)))
    })

    output$badges <- renderUI({
      current <- req(record())
      tagList(
        row_badge(if (identical(current$method, extraction_manual)) "Typed by hand" else "AI-Powered"),
        if (!identical(current$status, "Draft")) row_badge(current$status, class = "is-warning")
      )
    })

    output$banner <- renderUI(review_banner(req(record())))

    # Not for a request already waiting for approval: it is edited and saved, not pushed again.
    output$push_button <- renderUI({
      if (req(record())$status %in% c("Draft", "Sent Back")) {
        actionButton(ns("push"), "Push to database", icon = icon("cloud-arrow-up"), class = "btn-primary")
      }
    })

    # How far the checking has got, and a button to the next block with a problem while there is one.
    output$progress <- renderUI({
      counts <- draft_service_counts(req(draft()))
      n_to_fix <- length(unique(problems()$id))
      tagList(
        structure_progress_ui(counts$checked, counts$blocks),
      )
    })

    output$matr_csv_status <- renderUI({
      csv <- record()$matr_csv
      ids <- matr_ids()
      if (is.null(csv)) {
        return(tagList(
          strong("Drop the CSV of MATR IDs here"),
          div("One per row, in the order they appear on the questionnaire.")
        ))
      }
      tagList(
        strong(csv$name),
        div(
          plural(length(ids), "MATR ID"),
          if (length(ids) > 0) tagList(" · ", code(ids[1]), " to ", code(ids[length(ids)]))
        )
      )
    })

    # Whether the CSV and the draft agree on how many variables there are, and
    # what filling would replace.
    output$matr_fill_note <- renderUI({
      n_ids <- length(matr_ids())
      if (n_ids == 0) {
        return(NULL)
      }
      boxes <- req(draft())$variables
      mismatch <- draft_service_matr_id_mismatch(n_ids, nrow(boxes))
      n_filled <- sum(!is.na(boxes$matr_id))
      tagList(
        if (is.null(mismatch)) {
          p(icon("check"), sprintf(" One MATR ID for each of the %d variables.", nrow(boxes)))
        } else {
          p(class = "callout", mismatch, "Check the outline against the CSV before filling.")
        },
        if (n_filled > 0) {
          p(sprintf("This replaces the %s already filled in. Undo brings them back.", plural(n_filled, "MATR ID")))
        }
      )
    })

    output$save_status <- renderUI({
      if (has_unsaved_changes()) {
        span(class = "save-status is-dirty", "Unsaved changes")
      } else {
        span(class = "save-status", "All changes saved")
      }
    })

    # ------ RETURN ------------------------------------------------------------
    list(open = open)
  })
}

# ------ The PDF, served to this session ------------------------------------------------------------
#' The address the browser gets a PDF from, for this session only.
#'
#' The file is only kept on the server that received it (`uploads/pdfs/`), so it is handed to the
#' browser at an address that works for this session only.
#'
#' @param session The Shiny session.
#' @param name What to call it, in the module's namespace.
#' @param data `path`, `name` and `sha256` of the file; `pdf_response()` is given it.
#' @return The address.
pdf_address <- function(session, name, data) {
  session$registerDataObj(name = name, data = data, filterFunc = pdf_response)
}

#' The browser's request for the PDF.
#'
#' @param data From `pdf_address()`: `path` and `name` of the file.
#' @param req The request.
#' @return A `shiny::httpResponse()`: the file, shown in the browser rather than downloaded.
pdf_response <- function(data, req) {
  shiny::httpResponse(
    200L, "application/pdf",
    content = readBin(data$path, "raw", file.size(data$path)),
    headers = list(
      `Content-Disposition` = sprintf('inline; filename="%s"', gsub("[^A-Za-z0-9._ -]", "_", data$name)),
      `Cache-Control` = "private, max-age=3600"
    )
  )
}
