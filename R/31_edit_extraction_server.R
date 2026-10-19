# How many changes Undo can take back.
undo_depth <- 50

#' The Edit Extraction page's server, for one extraction draft.
#'
#' The whole questionnaire as it was extracted, every block edited where it is
#' (`extraction_outline_server()`). Other pages open a draft here with the `open` function this
#' returns. Changes stay in this session until Save writes them to the draft's reviewed copy in the
#' database.
#'
#' Preview Change Request shows the questionnaire the way Change Requests shows it to a reviewer;
#' previewing writes nothing. Submit Change Request saves anything unsaved and sends it to Change
#' Requests, where its Basic Info (registry, study, questionnaire, wave) is filled in; then one other
#' staff member approves it, or an ADMIN adds their own (`service_extraction_push.R`). Each question
#' shows its place among the questions (kept up to date as blocks move) and the number printed beside
#' it, which staff type; the wave asks it as the printed number, or as its place if none is typed. A
#' draft going into a recorded questionnaire gets the MATR IDs its questions already have.
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
edit_extraction_server <- function(
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
    recorded <- reactiveVal(NULL) # what the recorded questionnaire prints already; see extraction_service_recorded()
    show_variables <- reactiveVal(0L) # ticks when every question's variables should be shown
    problems <- reactive(draft_service_check(req(draft()), lookups))
    matr_ids <- reactive(extraction_service_matr_ids(record()$matr_csv$text %||% "")) # from the CSV
    available_matr_ids <- reactive(draft_service_available_matr_ids(req(draft()), matr_ids()))
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
      suggestions = suggestions,
      matr_ids = available_matr_ids,
      show_variables = show_variables
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
          showNotification("Saved. Nothing is added to the MATR metadata database until its change request is approved.", type = "message")
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
    # For a wave of a recorded questionnaire: the words its blocks have (offered while typing) and its
    # questions' MATR IDs (filled in where the question is worded the same). Read when a draft opens,
    # from the Basic Info saved with it; a read that fails leaves nothing to offer, with a warning.
    read_recorded <- function(opened, info = extraction_service_info(opened)) {
      recorded(tryCatch(
        extraction_service_recorded(connection, list(info = info)),
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

    # ------ MATR IDs ----------------------------------------------------------
    # The CSV lists one MATR ID per variable, in the order they appear. It belongs to the draft's
    # record, so adding one saves it at once. Giving the IDs out changes the questionnaire, so Undo and
    # the page's Save apply: all at once in order (Auto Apply), or one by one from each variable's
    # MATR ID drop-down, which offers the uploaded IDs no other variable has (Manual Assignment). Or they
    # are taken from a recorded questionnaire that asks the same questions (From a questionnaire), which
    # starts on the questionnaire the extraction goes into, once that is known.
    show_matr_ids <- function() {
      showModal(matr_ids_dialog(
        ns,
        can_apply = length(matr_ids()) > 0,
        questionnaires = questionnaire_service_all_choices(catalog$get()),
        picked = extraction_service_info(record())$questionnaire_id
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
      updateActionButton(session, "auto_apply", disabled = length(matr_ids()) == 0)
      updateActionButton(session, "manual_assign", disabled = length(matr_ids()) == 0)
    })

    observeEvent(input$auto_apply, {
      # Giving out the IDs is its own step, with its own count, so it doesn't take checks off.
      applied <- change_draft(
        draft_service_auto_apply_matr_ids(draft(), matr_ids(), replace = isTRUE(input$matr_replace)),
        keep_checked = TRUE
      )
      if (applied) removeModal()
    })

    observeEvent(input$manual_assign, {
      removeModal()
      show_variables(show_variables() + 1L)
      free <- plural(length(available_matr_ids()), "uploaded ID is", plural_form = "uploaded IDs are")
      showNotification(sprintf("Pick each variable's MATR ID from its drop-down, or type one. %s still free.", free), type = "message")
    })

    observeEvent(input$matr_questionnaire, {
      updateActionButton(session, "import_matr_ids", disabled = !has_text(input$matr_questionnaire))
    })

    # A questionnaire that asks the same questions: its MATR IDs go to the questions worded the same. Like
    # any change it can be undone, and it doesn't take a check off.
    observeEvent(input$import_matr_ids, {
      questionnaire_id <- input$matr_questionnaire %||% ""
      source <- questionnaire_service_get(catalog$get(), questionnaire_id)
      known <- tryCatch(extraction_service_questionnaire_matr_ids(connection, questionnaire_id), error = function(problem) {
        show_problem(problem, "Couldn't read that questionnaire's MATR IDs")
        NULL
      })
      if (is.null(known)) {
        return()
      }
      name <- if (is.null(source)) "that questionnaire" else source$abbreviation
      if (nrow(known) == 0) {
        showNotification(sprintf("%s has no variables with MATR IDs yet, so there is nothing to take.", name), type = "warning")
        return()
      }
      before <- draft()
      change_draft(draft_service_fill_recorded_matr_ids(draft(), known, from = name), keep_checked = TRUE)
      if (identical(before, draft())) {
        showNotification(sprintf(
          "Nothing to fill in: no question without MATR IDs is worded like one of %s's.", name
        ), type = "message")
      } else {
        removeModal()
      }
    })

    # The header's buttons: "to fix" jumps to the next block with a problem, MATR IDs opens its dialog.
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

    # ------ Previewing and submitting the change request ---------------------------------------
    # The preview shows the questionnaire as it is on screen, the way its change request shows it:
    # nothing is saved or sent while it is open. Where it goes (its Basic Info) is filled in on
    # Change Requests, after it is submitted.
    needs_approval <- change_request_service_needs_approval(me, extraction_kind)
    reviewers <- reactive(tryCatch(staff_service_reviewers(connection, me), error = function(problem) character()))
    preview_open <- reactiveVal(FALSE)

    show_preview <- function() {
      preview_open(TRUE)
      showModal(preview_request_dialog(ns))
    }

    observeEvent(input$request, {
      req(record_id(), draft())
      show_preview()
    })

    preview_blockers <- reactive({
      req(preview_open(), draft())
      extraction_service_draft_blockers(draft(), lookups)
    })

    output$preview <- renderUI({
      req(preview_open(), draft())
      blockers <- preview_blockers()
      tagList(
        if (length(blockers) > 0) {
          edit_callout("warning", title = "Before it can be submitted", tags$ul(lapply(blockers, tags$li)))
        },
        extraction_request_ui(list(unchecked = extraction_service_unchecked(draft())), draft())
      )
    })

    output$preview_blocker <- renderUI({
      blockers <- preview_blockers()
      if (length(blockers) > 0) span(class = "form-text", blockers[1])
    })

    observe({
      req(preview_open())
      updateActionButton(session, "submit_request", disabled = length(preview_blockers()) > 0)
    })

    # Submitting: the submission dialog takes the preview's place. Back puts the preview back.
    observeEvent(input$submit_request, {
      req(length(preview_blockers()) == 0)
      preview_open(FALSE)
      showModal(submit_request_dialog(
        ns, extraction_service_draft_summary(draft()), extraction_service_unchecked(draft()), needs_approval, reviewers()
      ))
    })

    observeEvent(input$back_to_preview, show_preview())

    observeEvent(input$confirm_request, {
      req(record_id(), draft())
      if (has_unsaved_changes() && !save_draft()) {
        return()
      }
      outcome <- tryCatch(
        extraction_service_push(connection, record(), me, catalog$get(), reviewer = input$request_reviewer),
        error = function(problem) {
          show_problem(problem, "Couldn't send it")
          NULL
        }
      )
      if (is.null(outcome)) {
        return()
      }
      removeModal()
      extractions$changed()
      showNotification("Sent to Change Requests. Fill in its Basic Info there.", type = "message")
      show_page("change_requests")
    })

    # Closing the preview stops it laying the questionnaire out.
    observeEvent(input$preview_cancel, {
      preview_open(FALSE)
      removeModal()
    })

    # ------ MODULES -----------------------------------------------------------
    extraction_outline_server("outline", state, lookups)

    # ------ OUTPUT ------------------------------------------------------------
    output$title <- renderText(paste("Edit Extraction:", heading()$title))
    output$subtitle <- renderText(heading()$subtitle)

    # The PDF, if the file is on this server, opens in its own window. The address changes with the
    # file, so a different PDF is not served from the browser's cache.
    record_pdf_url <- reactive({
      pdf <- req(record())$pdf
      if (length(pdf$path) == 0 || !file.exists(pdf$path)) {
        return(NULL)
      }
      pdf_address(session, ns("file"), list(path = pdf$path, name = pdf$name, sha256 = pdf$sha256))
    })
    output$pdf_link <- renderUI(pdf_open_button(record_pdf_url()))

    output$badges <- renderUI({
      current <- req(record())
      tagList(
        if (identical(current$method, extraction_manual)) row_badge("Typed by hand"),
        if (!identical(current$status, "Draft")) row_badge(current$status, class = "is-warning")
      )
    })

    output$banner <- renderUI(edit_extraction_banner(req(record())))

    # Not for a request already waiting for approval: it is edited and saved, not submitted again.
    output$request_button <- renderUI({
      if (req(record())$status %in% c("Draft", "Sent Back")) {
        actionButton(ns("request"), "Preview Change Request", icon = icon("eye"), class = "btn-primary")
      }
    })

    # How far the checking has got.
    output$progress <- renderUI(structure_progress_ui(draft_service_counts(req(draft()))))

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

    # Whether the CSV and the draft agree on how many variables there are, and how many IDs are still free.
    output$matr_fill_note <- renderUI({
      n_ids <- length(draft_service_uploaded_matr_ids(matr_ids()))
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
          p(class = "callout", mismatch, " Nothing is lost: what is left over is still offered in the drop-downs.")
        },
        p(sprintf(
          "%s of the variables have a MATR ID; %s still free.",
          format(n_filled, big.mark = ","), plural(length(available_matr_ids()), "uploaded ID is", plural_form = "uploaded IDs are")
        ))
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
