# State kept outside reactive values: `dry_run_for`, what the push's dry run was last tried for, a plain variable
# inside the module, so each session has its own and it goes when the session ends. Nothing else reads it. Shiny
# runs a session's code one piece at a time, so nothing changes it while something else is reading it; it isn't
# made to be shared between threads or sessions, and shouldn't be.

# How many changes Undo can take back.
undo_depth <- 50

# How long staff have to have done nothing (no change, no block picked, no key pressed or pointer moved) before
# the draft is saved by itself, in seconds. Long enough that it is never saved while they work: the server does
# nothing else while it saves, so the page can't be used then.
autosave_delay <- 60

# The Edit Extraction page's server, for one extraction draft.
# The whole questionnaire as it was extracted, every block edited where it is
# (`extraction_outline_server()`). Other pages open a draft here with the `open` function this
# returns. Changes are saved to the draft's reviewed copy in the database by themselves, once staff
# have done nothing for a minute (`autosave_delay`), or at once with Save; after a save is refused, only
# Save tries again.
# Preview and push opens one dialog: where the extraction goes (its Basic Info: registry, study,
# questionnaire and wave, `extraction_info_server()`), what pushing it would add (the database tries the
# push and undoes it: `extraction_service_push_preview()`) and the questionnaire laid out as a table. Push
# writes it all into the MATR metadata database at once (`service_extraction_push.R`); nothing is added
# before. Each question shows its place among the questions (worked out from the outline, and kept up to
# date as blocks move) and the number printed beside it (read off the PDF, and corrected by staff); the wave
# records both. A draft going into a recorded questionnaire gets the MATR IDs its questions already have.
# One person at a time works on a draft (see `service_draft_row.R`). Opening one here claims it; while
# someone else has it, opening it asks whether to take it over instead. The page renews the claim while
# staff are active on the site, and lets it go when they leave the page with nothing unsaved, close it,
# or do nothing for a while; then it only shows the draft until it has it again, which it tries as soon
# as they come back. Save is refused if someone else saved the draft after this page opened it, so
# nobody's work is overwritten.
# Re-extract reads an AI-Powered draft's PDF again with the latest extraction prompt, in place of the questionnaire
# on the page (`extraction_service_reextract()`); the page lets the draft go and shows the reading with `open_reading`.
# active: A reactive: whether this page is the one showing.
# open_reading: A function taking a draft being read that shows how reading it is going (New Extraction's), or `NULL`
#   to go to New Extraction.
# Returns: A list with `open`, a function taking a record (and `take_over`, whether to take it from
#   whoever is working on it) that opens it, unless the draft on screen has unsaved changes and staff
#   choose to stay, or someone else is working on it; it returns whether it opened.
edit_extraction_server <- function(
  id,
  extractions,
  catalog,
  connection,
  me,
  show_page,
  active = reactive(TRUE),
  open_reading = NULL
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
    original <- reactiveVal(NULL) # what the AI returned for it (NULL for a Manual extraction); see extraction_service_open_draft()
    undo_stack <- reactiveVal(list()) # earlier drafts, newest first
    selected <- reactiveVal(NULL) # id of the selected block
    layout_version <- reactiveVal(0L) # ticks when the outline must rebuild its inputs
    waiting <- reactiveVal(NULL) # a draft to open once unsaved changes are dealt with
    waiting_take_over <- reactiveVal(FALSE) # whether to take that draft over when it opens
    loaded_revision <- reactiveVal(NULL) # the draft's revision as this page opened it or last saved it
    offered_revision <- reactiveVal(NULL) # a later revision someone else saved, which staff were asked about once
    claim <- reactiveVal(NULL) # NULL while this page has the draft to work on; otherwise why not (see below)
    claiming <- reactiveVal(NULL) # a draft someone else has, while staff are asked whether to take it over
    last_active <- reactiveVal(Sys.time()) # when staff last did something on the site
    recorded <- reactiveVal(NULL) # what the recorded questionnaire prints already; see extraction_service_recorded()
    loads <- reactiveVal(0L) # ticks each time a draft is opened on this page
    problems <- reactive(draft_service_check(req(draft()), lookups))
    has_unsaved_changes <- reactive(!identical(draft(), saved()))
    saved_at <- reactiveVal(NULL) # when this page last saved the draft
    autosave_off <- reactiveVal(FALSE) # a save was refused: only Save tries again
    has_draft <- reactive(!is.null(record_id()) && is.null(claim())) # to work on: no one else can change it
    heading <- reactive(extraction_service_heading(record(), catalog$get())) # not the draft: typing doesn't change it
    suggestions <- reactive(draft_service_text_suggestions(req(draft()), recorded()$texts))

    # Every change to the draft comes through here so it can be undone. Service
    # functions explain themselves: their stop() and message() texts are shown
    # to staff. `typed` is for words typed into a field, which the field already
    # shows; rebuilding it would move the cursor. `redraw` is FALSE for any other change the outline
    # marks without drawing it again (marking a block as checked). A block that was marked as checked and has
    # been edited is no longer checked, unless `keep_checked`; a section or group is checked once
    # everything in it is (draft_service_check_containers()). The questions are numbered again by
    # their place in the outline. Returns TRUE if it worked.
    change_draft <- function(new_draft, typed = FALSE, keep_checked = FALSE, redraw = !typed) {
      if (!can_change()) {
        layout_version(layout_version() + 1L) # put the inputs back as the draft has them
        return(FALSE)
      }
      new_draft <- try_change(new_draft)
      if (is.null(new_draft)) {
        layout_version(layout_version() + 1L) # put the inputs back as the draft has them
        return(FALSE)
      }
      # A block that was marked as checked and has been edited is no longer checked.
      if (!keep_checked) new_draft <- draft_service_uncheck_edited(draft(), new_draft)
      new_draft <- draft_service_renumber_questions(new_draft)
      new_draft <- draft_service_check_containers(new_draft) # checked once everything inside is
      if (!identical(new_draft, draft())) {
        undo_stack(c(list(draft()), head(undo_stack(), undo_depth - 1)))
        draft(new_draft)
        if (redraw) layout_version(layout_version() + 1L)
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
      original = original,
      loaded = loads
    )

    observeEvent(input$undo, {
      req(length(undo_stack()) > 0, can_change())
      draft(undo_stack()[[1]])
      undo_stack(undo_stack()[-1])
      layout_version(layout_version() + 1L)
      if (!isTRUE(selected() %in% draft()$components$id)) selected(NULL)
    })

    observeEvent(input$save, save_draft())

    # Saved by itself once staff have done nothing for a minute: no change, no block picked, and no key
    # pressed or pointer moved on the site (www/js/review.js sends `active` at most every 20 seconds).
    autosaves <- reactive(!autosave_off())
    activity <- reactive(list(draft(), selected(), input$active))
    settled <- debounce(activity, autosave_delay * 1000)
    observeEvent(settled(), {
      if (has_unsaved_changes() && autosaves()) save_draft(quietly = TRUE)
    }, ignoreInit = TRUE)

    # The page saves the questionnaire as it is on screen; for an AI-Powered draft, how it differs from
    # what the AI returned (`original()`) is kept with it (`extraction_service_save_draft()`). It is
    # refused if someone else saved the draft after this page opened it (`loaded_revision()`): then staff
    # may open the draft as it was saved instead. Saved by itself (`quietly`), it says nothing when it
    # works; when it doesn't, it says so once, and leaves saving to Save.
    save_draft <- function(quietly = FALSE) {
      if (!can_change()) {
        return(FALSE)
      }
      tryCatch(
        {
          revision <- extraction_service_save_draft(
            connection, record(), draft(), original(), me, expected_revision = loaded_revision()
          )
          loaded_revision(revision)
          saved(draft())
          saved_at(Sys.time())
          autosave_off(FALSE)
          # Save reads the list of drafts again at once; a save by itself leaves that to the next page that
          # lists drafts, so the page isn't held up by it while staff work
          if (!quietly) extractions$changed()
          if (!quietly) {
            showNotification("Saved. Nothing is added to the MATR metadata database until you push it.", type = "message")
          }
          TRUE
        },
        error = function(problem) {
          autosave_off(TRUE)
          if (grepl("changed after you opened it", conditionMessage(problem), fixed = TRUE)) {
            offer_saved_version("Saving now would overwrite their changes, so yours weren't saved.")
          } else {
            show_problem(problem, "Couldn't save")
          }
          FALSE
        }
      )
    }

    # Someone else saved the draft after this page opened it, and there are changes here that aren't
    # saved: they can't be saved over theirs, so staff may open the draft as it was saved instead.
    offer_saved_version <- function(why) {
      showModal(confirm_dialog(
        "Someone saved this draft after you opened it",
        p(why),
        p("Open the draft as it was saved? Your changes that weren't saved are lost."),
        confirm_id = ns("reopen"), confirm_label = "Open the saved version", keep_label = "Not now"
      ))
    }

    observeEvent(input$reopen, {
      removeModal()
      reload()
    })

    # ------ Opening a draft ---------------------------------------------------
    # Opens `opened` (an extraction record), unless the draft on screen has
    # unsaved changes: then staff choose whether to save them first. Returns
    # FALSE if the draft couldn't be opened.
    open <- function(opened, take_over = FALSE) {
      if (identical(opened$id, isolate(record_id()))) {
        if (!isolate(has_draft())) isolate(resume(take_over))
        return(TRUE)
      }
      if (!isolate(has_unsaved_changes())) {
        return(show_draft(opened, take_over))
      }
      waiting(opened)
      waiting_take_over(take_over)
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

    # It is claimed first: if someone else is working on it, staff are asked whether to take it over.
    show_draft <- function(opened, take_over = FALSE) {
      got <- claim_draft(opened$id, take_over)
      if (is.null(got)) {
        return(FALSE)
      }
      if (!got$claimed) {
        ask_take_over(opened, got)
        return(FALSE)
      }
      previous <- isolate(record_id())
      if (!put_on_page(opened, got$revision)) {
        if (!identical(previous, opened$id)) release_draft(opened$id)
        return(FALSE)
      }
      if (!is.null(previous) && !identical(previous, opened$id)) release_draft(previous)
      TRUE
    }

    # Loads `opened`'s questionnaire as it was saved at `revision`, and what the AI returned for it, and
    # shows it. Returns whether it could.
    put_on_page <- function(opened, revision) {
      loaded <- try_showing(extraction_service_open_draft(connection, opened), "Couldn't open this draft")
      if (is.null(loaded)) {
        return(FALSE)
      }
      original(loaded$original)
      loaded <- draft_service_check_containers(loaded$draft) # drafts saved before sections were worked out
      record_id(opened$id)
      loaded_revision(revision)
      offered_revision(NULL)
      claim(NULL)
      last_active(Sys.time())
      draft(loaded)
      saved(loaded)
      saved_at(NULL)
      autosave_off(FALSE)
      undo_stack(list())
      selected(NULL)
      loads(loads() + 1L)
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
      recorded(try_showing(
        extraction_service_recorded(connection, list(info = info)),
        "Couldn't read what this questionnaire has already", type = "warning"
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
      if (save_draft()) show_draft(waiting(), waiting_take_over())
    })

    observeEvent(input$discard_and_open, {
      removeModal()
      show_draft(waiting(), waiting_take_over())
    })

    # ------ One person at a time ----------------------------------------------
    # `claim()` is NULL while this page has the draft to work on. Otherwise it says why not, and the
    # page only shows the draft: `why` is "taken" (someone else has it: `by`, since `at`), "idle"
    # (staff did nothing for a while) or "left" (they went to another page with nothing unsaved).
    # The claim is renewed every minute while staff are active anywhere on the site (www/js/review.js
    # sends `active`), so a page left open overnight doesn't keep the draft from anyone.
    claim_draft <- function(id, take_over = FALSE) {
      try_showing(draft_row_service_claim(connection, id, me, take_over), "Couldn't open this draft")
    }

    release_draft <- function(id) {
      if (!is.null(id)) tryCatch(draft_row_service_release(connection, id, me), error = function(problem) FALSE)
    }

    let_go <- function(why) {
      release_draft(record_id())
      claim(list(why = why))
    }

    # Whether the draft can be changed here now; if not, says why.
    can_change <- function() {
      if (has_draft()) {
        return(TRUE)
      }
      held <- claim()
      showNotification(
        if (identical(held$why, "taken")) {
          sprintf("%s is working on this draft, so it can't be changed here. Take it over to work on it.", held$by)
        } else {
          "This draft was let go while you were away. Choose Keep working to change it again."
        },
        type = "warning"
      )
      FALSE
    }

    # Someone else is working on `opened`: take it over?
    ask_take_over <- function(opened, held) {
      claiming(opened)
      extractions$changed() # the list of drafts shows who has it
      showModal(confirm_dialog(
        sprintf("%s is working on this draft", held$by),
        p(sprintf(
          "They were active on it %s. Only one person at a time can work on a draft, so nobody's changes are overwritten.",
          time_ago(parse_time(held$at))
        )),
        p(sprintf(
          "If you take it over, %s can still see it but not change it, and anything they haven't saved can't be saved.",
          held$by
        )),
        confirm_id = ns("take_over"), confirm_label = "Take over", keep_label = "Cancel"
      ))
    }

    observeEvent(input$take_over, {
      removeModal()
      opened <- claiming()
      claiming(NULL)
      req(opened)
      if (identical(opened$id, record_id())) {
        resume(take_over = TRUE)
      } else if (show_draft(opened, take_over = TRUE)) {
        show_page("edit_extraction")
      }
    })

    # The banner's Take over asks first, as opening it would have.
    observeEvent(input$take_over_banner, ask_take_over(record(), claim()))
    observeEvent(input$keep_working, resume())

    # Has the draft on screen to work on again, if nobody else has it (or `take_over`). If it was saved
    # meanwhile, it is opened again as it was saved. Returns whether this page has it now.
    resume <- function(take_over = FALSE) {
      got <- claim_draft(record_id(), take_over)
      if (is.null(got)) {
        return(FALSE)
      }
      if (!got$claimed) {
        claim(list(why = "taken", by = got$by, at = got$at))
        return(FALSE)
      }
      last_active(Sys.time())
      catch_up(got$revision)
    }

    # This page has the draft, which the database has at `revision`. If someone saved it since this page
    # opened it or last saved it, the page shows what they saved; unless there are changes here that
    # aren't saved, which staff are asked about rather than lose: once for each revision, since the claim
    # is renewed every minute and asking again would put the question over whatever dialog is open.
    # Returns whether this page has it.
    catch_up <- function(revision) {
      claim(NULL)
      if (identical(as.integer(revision), as.integer(loaded_revision()))) {
        return(TRUE)
      }
      if (has_unsaved_changes()) {
        if (!identical(as.integer(revision), offered_revision())) {
          offered_revision(as.integer(revision))
          offer_saved_version("Your changes here can't be saved over theirs.")
        }
        return(TRUE)
      }
      reloaded <- reload(revision)
      if (reloaded) {
        showNotification(
          paste(staff_name(record()$saved_by), "saved this draft since you opened it, so it shows what they saved."),
          type = "warning", duration = 20
        )
      }
      reloaded
    }

    # Opens the draft on screen again, as it was last saved. Returns whether it could.
    reload <- function(revision = NULL) {
      extractions$changed()
      current <- record()
      if (is.null(revision)) {
        got <- claim_draft(current$id)
        if (is.null(got) || !got$claimed) {
          if (!is.null(got)) claim(list(why = "taken", by = got$by, at = got$at))
          return(FALSE)
        }
        revision <- got$revision
      }
      put_on_page(current, revision)
    }

    # Every minute: renewed while staff are active, let go once they have done nothing for a while.
    # A claim someone else took means this page now only shows the draft.
    observe({
      invalidateLater(draft_claim_renew_seconds * 1000)
      isolate(keep_claim())
    })

    keep_claim <- function() {
      if (!has_draft()) {
        return()
      }
      if (as.numeric(difftime(Sys.time(), last_active(), units = "secs")) > draft_claim_idle_seconds) {
        let_go("idle")
        return()
      }
      # One that can't be renewed now (the database can't be reached) is tried again next time. One that
      # is renewed may have been saved by someone else meanwhile (who took it over and let it go again
      # within the minute).
      got <- tryCatch(draft_row_service_claim(connection, record_id(), me), error = function(problem) NULL)
      if (isTRUE(got$claimed)) catch_up(got$revision)
      if (!is.null(got) && !got$claimed) {
        claim(list(why = "taken", by = got$by, at = got$at))
        showNotification(
          sprintf("%s took over this draft. You can still see it, but not change it.", got$by),
          type = "warning", duration = NULL
        )
      }
    }

    observeEvent(input$active, {
      last_active(Sys.time())
      if (identical(claim()$why, "idle")) resume()
    })

    # Going to another page lets the draft go, unless something is unsaved; coming back has it again.
    observeEvent(active(),
      {
        req(record_id())
        if (!active()) {
          if (has_draft() && !has_unsaved_changes()) let_go("left")
        } else if (!has_draft()) {
          resume()
        }
      },
      ignoreInit = TRUE
    )

    session$onSessionEnded(function() {
      if (isolate(has_draft())) release_draft(isolate(record_id()))
    })

    # "to fix" jumps to the next block with a problem; the Normalize dialog's buttons change the draft.
    observeEvent(input$action, {
      action <- input$action
      switch(action$action %||% "",
        next_problem = selected(draft_service_next_problem(draft(), problems(), selected())),
        use_choice = change_draft(normalize_service_use_choice(draft(), action$from, action$to)),
        NULL
      )
    })

    # ------ Normalize -------------------------------------------------------------------------
    # Answer choices worded as the database words them (service_normalize.R). The dialog lists them as the
    # draft is now, so it changes as its buttons are used.
    normalize_open <- reactiveVal(FALSE)

    observeEvent(input$normalize, {
      req(draft())
      normalize_open(TRUE)
      showModal(normalize_dialog(ns))
    })

    output$normalize_body <- renderUI({
      req(normalize_open(), draft())
      normalize_ui(ns, normalize_service_choice_suggestions(draft(), lookups$answer_options))
    })

    observe({
      updateActionButton(session, "undo", disabled = length(undo_stack()) == 0 || !has_draft())
      updateActionButton(session, "save", disabled = !has_unsaved_changes() || !has_draft())
    })

    # ------ Re-extract ---------------------------------------------------------------------------
    # An AI-Powered draft read again from the start, with the latest extraction prompt. What is on the page is put
    # aside for the new reading, so the page lets the draft go and shows the reading instead; the draft opens here
    # again once it is read.
    can_reextract <- reactive(identical(req(record())$method, extraction_ai))

    observeEvent(input$reextract, {
      req(can_reextract(), can_change())
      showModal(reextract_dialog(ns))
    })

    observeEvent(input$confirm_reextract, {
      removeModal()
      req(record_id(), can_change())
      id <- record_id()
      reading <- try_showing(
        extraction_service_reextract(connection, id, me, expected_revision = loaded_revision()),
        "Couldn't read it again"
      )
      if (is.null(reading)) {
        return()
      }
      release_draft(id)
      put_aside()
      extractions$changed()
      showNotification("Reading the PDF again. Open it from Extraction Drafts once it is read.", type = "message")
      if (is.function(open_reading)) open_reading(reading) else show_page("extraction")
    })

    # The page holds no draft: what it showed is gone (read again).
    put_aside <- function() {
      record_id(NULL)
      draft(NULL)
      saved(NULL)
      original(NULL)
      recorded(NULL)
      undo_stack(list())
      selected(NULL)
      claim(NULL)
      saved_at(NULL)
      loaded_revision(NULL)
      offered_revision(NULL)
    }

    output$reextract_button <- renderUI({
      if (can_reextract()) {
        actionButton(
          ns("reextract"), icon_label("reextract", "Re-extract"), class = "btn-outline-dark",
          title = "Read the PDF again with the latest extraction instructions, in place of this questionnaire"
        )
      }
    })

    # ------ Previewing and pushing ------------------------------------------------------------
    # One dialog (`push_dialog()`): what stops the push, if anything; where the extraction goes (its Basic
    # Info, the form of `extraction_info_server()`, filled from the record when the dialog opens); what the
    # push would add, from a dry run the database does and undoes; and the questionnaire laid out as a table.
    # Opening it saves anything unsaved first, so the dry run and the push read what is on screen. Push
    # writes it all into the MATR metadata database at once; until then nothing is added.
    push_open <- reactiveVal(FALSE)
    dry_run <- reactiveVal(NULL) # the counts the dry run returned, the error it raised, or NULL while it isn't tried
    dry_run_for <- NULL # what the dry run was last tried for, so one change is one dry run

    # The pop-up that adds a row from a drop-down takes the dialog's place, so `reopen` puts it back; the form
    # keeps what was picked, and picks the new row itself.
    reopen_dialog <- function() {
      push_open(TRUE)
      showModal(push_dialog(ns, isolate(draft())))
    }
    info <- extraction_info_server("info", catalog, extractions, record_id, me, connection, reopen = reopen_dialog)

    observeEvent(input$request, {
      req(record_id(), draft())
      if (has_unsaved_changes() && !save_draft()) {
        return()
      }
      dry_run_for <<- NULL
      dry_run(NULL)
      reopen_dialog()
      info$show(record())
    })

    # The form's picks, once they have settled: a dry run per keystroke would ask the database too often.
    settled_info <- debounce(info$info, 500)

    # How many questions each wave of the picked questionnaire asks: a wave that asks any has to be emptied
    # first. If it can't be read, the dry run says so instead.
    wave_question_counts <- reactive({
      tryCatch(questionnaire_service_wave_question_counts(connection, settled_info()$questionnaire_id), error = function(problem) integer())
    })

    # What stops the push: the questionnaire first (blocks to fix), then its Basic Info.
    push_blockers <- reactive({
      req(push_open(), draft())
      c(
        extraction_service_draft_blockers(draft(), lookups),
        extraction_service_info_blockers(record(), settled_info(), catalog$get(), extractions$all(), wave_question_counts(), me)
      )
    })

    # With nothing in the way, where it goes is saved to the draft (if it changed) and the push is tried
    # without being kept. Once for each change of the picks or the saved questionnaire: saving the Basic Info
    # reads the drafts again, which would otherwise run it twice.
    observe({
      req(push_open())
      blockers <- push_blockers()
      isolate({
        key <- if (length(blockers) == 0) list(settled_info(), saved())
        if (is.null(key)) {
          dry_run(NULL)
        } else if (!identical(key, dry_run_for)) {
          dry_run(try_dry_run(settled_info()))
        }
        dry_run_for <<- key
      })
    })

    # Saves the Basic Info to the draft when it differs, then asks the database what the push would do.
    # Returns the counts, or the error raised (the database's sentence, or that the save was refused).
    try_dry_run <- function(info) {
      tryCatch(
        {
          current <- utils::modifyList(record(), list(revision = loaded_revision()))
          placed <- extraction_service_save_info(connection, current, info, me, catalog$get())
          if (!identical(as.integer(placed$revision), as.integer(current$revision))) {
            loaded_revision(placed$revision)
            extractions$changed()
          }
          extraction_service_push_preview(connection, placed, me)
        },
        error = function(problem) problem
      )
    }

    # What is in the way, above the form.
    output$push_blockers <- renderUI({
      blockers <- push_blockers()
      if (length(blockers) > 0) {
        edit_callout("warning", title = "Before it can be pushed", tags$ul(lapply(blockers, tags$li)))
      }
    })

    # What the push would add, or why the database would refuse it; nothing while something is in the way.
    output$push_what <- renderUI({
      req(push_open(), draft(), length(push_blockers()) == 0)
      preview <- extraction_service_push_preview_ui(draft(), settled_info(), catalog$get(), dry_run())
      extraction_request_ui(preview, NULL, questionnaire = FALSE)
    })

    output$preview_table <- renderUI({
      req(push_open())
      questionnaire_preview_ui(req(draft()))
    })

    output$push_blocker <- renderUI({
      blockers <- push_blockers()
      if (length(blockers) > 0) span(class = "form-text", blockers[1])
    })

    can_push <- reactive(push_open() && length(push_blockers()) == 0 && !is.null(dry_run()) && !inherits(dry_run(), "error"))

    observe({
      req(push_open())
      updateActionButton(session, "push", disabled = !can_push())
    })

    observeEvent(input$push, {
      req(can_push(), record_id(), can_change())
      if (has_unsaved_changes() && !save_draft()) {
        return()
      }
      # As this page has it: refused if someone else saved it since.
      sent <- utils::modifyList(record(), list(revision = loaded_revision()))
      outcome <- try_showing(extraction_service_push(connection, sent, me, catalog$get()), "Couldn't push it")
      if (is.null(outcome)) {
        return()
      }
      removeModal()
      push_open(FALSE)
      release_draft(record_id())
      put_aside()
      extractions$changed()
      showNotification(paste("Pushed.", extraction_service_push_summary(outcome$counts)), type = "message")
      show_page("drafts")
    })

    # Closing the dialog stops it laying the questionnaire out.
    observeEvent(input$push_cancel, {
      push_open(FALSE)
      removeModal()
    })

    # ------ MODULES -----------------------------------------------------------
    extraction_outline_server("outline", state, lookups)

    # ------ OUTPUT ------------------------------------------------------------
    output$title <- renderText(paste("Edit Extraction:", heading()$title))
    output$subtitle <- renderText(heading()$subtitle)

    # The PDF opens in its own tab, at the picked block's page: the one uploaded with the draft, fetched
    # from Storage if this server hasn't got it (`extraction_service_pdf()`). The address changes with the
    # file, so a different PDF is not served from the browser's cache.
    record_pdf_url <- reactive({
      pdf <- extraction_service_pdf(connection, req(record()))
      if (is.null(pdf)) {
        return(NULL)
      }
      pdf_address(session, ns("file"), list(path = pdf$path, name = pdf$name, sha256 = pdf$sha256))
    })
    output$pdf_link <- renderUI(pdf_open_button(record_pdf_url(), at_picked = TRUE))

    output$badges <- renderUI({
      if (identical(req(record())$method, extraction_manual)) row_badge("Typed by hand")
    })

    output$banner <- renderUI(edit_extraction_claim_banner(ns, claim(), isolate(has_unsaved_changes())))

    output$request_button <- renderUI({
      req(record())
      actionButton(
        ns("request"), icon_label("preview", "Preview and push"), class = "btn-primary",
        title = "See what pushing adds to the MATR metadata database, and push it"
      )
    })

    # How far the checking has got.
    output$progress <- renderUI(structure_progress_ui(draft_service_counts(req(draft()))))

    output$save_status <- renderUI({
      if (has_unsaved_changes()) {
        return(span(
          class = "save-status is-dirty",
          if (autosaves()) "Unsaved changes" else "Unsaved changes: Save to keep them",
          title = if (autosaves()) "Saved by itself after a minute with nothing changed, or now with Save"
        ))
      }
      at <- saved_at()
      if (is.null(at)) {
        return(span(class = "save-status", "All changes saved"))
      }
      invalidateLater(15000) # "Saved 30 s ago" moves on
      span(class = "save-status", title = paste("Saved at", format(at, "%l:%M %p")), saved_ago(at))
    })

    # ------ RETURN ------------------------------------------------------------
    list(open = open)
  })
}

# ------ The PDF, served to this session ------------------------------------------------------------
# The address the browser gets a PDF from, for this session only.
# The file is only kept on the server that received it (`uploads/pdfs/`), so it is handed to the
# browser at an address that works for this session only.
# name: What to call it, in the module's namespace.
# data: `path`, `name` and `sha256` of the file; `pdf_response()` is given it.
# Returns: The address.
pdf_address <- function(session, name, data) {
  session$registerDataObj(name = name, data = data, filterFunc = pdf_response)
}

# How long ago the draft was saved, as the page's header says it.
# at: When it was saved.
# Returns: "Saved just now", "Saved 30 s ago", "Saved 4 min ago", or "Saved at 2:14 PM" after an hour.
saved_ago <- function(at, now = Sys.time()) {
  seconds <- as.numeric(difftime(now, at, units = "secs"))
  if (seconds < 15) {
    return("Saved just now")
  }
  if (seconds < 60) {
    return(sprintf("Saved %d s ago", as.integer(seconds %/% 15 * 15)))
  }
  if (seconds < 3600) {
    return(sprintf("Saved %d min ago", as.integer(seconds %/% 60)))
  }
  paste("Saved at", trimws(format(at, "%l:%M %p")))
}

# The browser's request for the PDF.
# data: From `pdf_address()`: `path` and `name` of the file.
# req: The request.
# Returns: A `shiny::httpResponse()`: the file, shown in the browser rather than downloaded.
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
