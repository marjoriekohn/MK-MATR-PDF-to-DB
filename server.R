function(input, output, session) {
  # ------ SIGNING IN ---------------------------------------------------------
  # Nothing is built until a staff member has signed in: they type their email
  # and password, or have Supabase Auth email them a sign-in link that brings
  # them back with their tokens (R/model_auth.R and R/service_staff.R). The
  # login screen (R/helpers_login.R) covers the site until then. While
  # developing, STAFF_EMAIL in .env signs the site in as that staff member with
  # no password or link.
  auth <- auth_model_connection()
  tokens <- NULL
  # the sign-in's tokens, kept in this session only, for signing out and changing the password

  start_site <- function(me, signed_in_tokens = NULL, development = FALSE) {
    if (!is.null(isolate(signed_in()))) return(invisible()) # already signed in this session
    tokens <<- signed_in_tokens
    removeUI(paste0("#", login_screen_id))
    output$navbar_refresh <- renderUI(refresh_button())
    output$navbar_user <- renderUI(user_menu(me, development))
    build_site(me)
  }

  # However they signed in (a password, a link, or a reload): the browser keeps the refresh token, and
  # the site opens.
  signed_in_as <- function(result) {
    session$sendCustomMessage("auth-keep", result$tokens$refresh_token)
    start_site(result$me, result$tokens)
  }

  # What the login screen and the password dialog say under their buttons. The
  # boxes are hidden while they're empty (helpers_login.R's styles), so they
  # have to render while hidden, or nothing would ever show. Saying something
  # also ends the "Signing in..." the page script showed when a button was
  # pressed (www/js/site.js).
  login_said <- reactiveVal(NULL)
  output$login_message <- renderUI({
    said <- login_said()
    if (!is.null(said)) login_message(said$text, said$kind)
  })
  outputOptions(output, "login_message", suspendWhenHidden = FALSE)
  say <- function(text, kind = "problem") {
    login_said(list(text = text, kind = kind))
    session$sendCustomMessage("login-ready", TRUE)
  }
  password_said <- reactiveVal(NULL)
  output$password_message <- renderUI({
    said <- password_said()
    if (!is.null(said)) login_message(said)
  })
  outputOptions(output, "password_message", suspendWhenHidden = FALSE)

  # Sign in: the email and password typed, if the email is a staff member's.
  observeEvent(input$login_sign_in, {
    result <- tryCatch(
      staff_service_sign_in(database, auth, input$login_email, input$login_password),
      error = function(problem) {
        say(conditionMessage(problem))
        NULL
      }
    )
    if (!is.null(result)) signed_in_as(result)
  })

  # Or Auth emails a sign-in link to the address typed, if it is a staff
  # member's.
  observeEvent(input$login_send_link, {
    here <- site_address(session)
    tryCatch(
      {
        staff_service_send_link(database, auth, input$login_email, here)
        say(paste0(
          "A sign-in link is on its way to ", trimws(input$login_email),
          ". Open it on this computer; if it hasn't come in a few minutes, look in your spam folder."
        ), kind = "note")
      },
      error = function(problem) say(conditionMessage(problem))
    )
  })

  # The link brings the person back here with their tokens (www/js/site.js).
  observeEvent(input$auth_tokens, {
    result <- tryCatch(
      staff_service_from_tokens(database, auth, input$auth_tokens),
      error = function(problem) {
        say(conditionMessage(problem))
        NULL
      }
    )
    if (!is.null(result)) signed_in_as(result)
  })

  # A reload: the browser kept the sign-in's refresh token (www/js/site.js). Auth swaps it for new
  # tokens, the new refresh token goes back to the browser, and they sign the person in as a link's would.
  observeEvent(input$auth_resume, {
    result <- tryCatch(staff_service_resume(database, auth, input$auth_resume), error = function(problem) NULL)
    if (is.null(result)) {
      session$sendCustomMessage("auth-forget", TRUE)
      say("Your sign-in has ended. Sign in again.", kind = "note")
    } else {
      signed_in_as(result)
    }
  })

  # A link that Supabase turned down (already used, or expired) says why instead of failing silently.
  observeEvent(input$auth_problem, {
    say(paste0("That sign-in link didn't work (", input$auth_problem, "). Request a new one."))
  })

  # Setting or changing the password, from the menu.
  observeEvent(input$change_password, {
    password_said(NULL)
    showModal(password_dialog())
  })
  observeEvent(input$password_change_submit, {
    changed <- tryCatch(
      {
        staff_service_change_password(auth, tokens, input$new_password, input$new_password_again)
        TRUE
      },
      error = function(problem) {
        password_said(conditionMessage(problem))
        session$sendCustomMessage("login-ready", TRUE)
        FALSE
      }
    )
    if (!changed) return()
    password_said(NULL)
    removeModal()
    showNotification("Your password is saved.", type = "message", duration = 4)
  })

  # Logging out ends the sign-in at Auth and starts the session again, at the
  # login screen.
  observeEvent(input$log_out, {
    staff_service_sign_out(auth, tokens)
    session$reload()
  })

  signed_in <- reactiveVal(NULL) # who is signed in, once someone is
  site <- NULL
  # what the pages share, once the site is built (the tests read it). The
  # session's sign-in, for the tests: who, and the tokens kept for signing out.
  session_state <- function() list(me = signed_in(), tokens = tokens, site = site)

  # ------ THE SITE, ONCE SOMEONE IS SIGNED IN ----------------------------------
  build_site <- function(me) {
    signed_in(me)
    # The live tables the extraction pages pick from and check against, read once
    # now (`database` is set up in global.R). If they can't be read, the staff
    # are told why and nothing else is built: a page with no database would only
    # fail in confusing ways.
    startup <- tryCatch(questionnaire_service_get_catalog(database), error = function(problem) problem)
    if (inherits(startup, "error")) {
      showModal(modalDialog(
        title = "The site couldn't start",
        p(conditionMessage(startup)),
        footer = actionButton("try_again", "Try again", class = "btn-primary"),
        easyClose = FALSE
      ))
      observeEvent(input$try_again, session$reload())
      return(invisible())
    }

    # The live tables the extraction pages pick from: read when the session
    # starts, again after staff add a study, a Respondent Type or an investigator
    # from a drop-down, and whenever a page that picks from them is opened (what
    # the other pages add has to be there). If they can't be read again the last
    # ones stay.
    catalog_tables <- reactiveVal(startup)
    catalog <- list(
      get = catalog_tables,
      refresh = function() {
        tryCatch(
          catalog_tables(questionnaire_service_get_catalog(database)),
          error = function(problem) show_problem(problem, "Couldn't read the database again", type = "warning")
        )
      }
    )

    # The database isn't asked over and over whether something changed: every read makes the page
    # busy, and the gold bar would never go away. What other staff changed is read when a page that
    # shows it is opened, and when the menu's Refresh is pressed (`refreshed`, which every page that
    # reads the database is given).
    refreshed <- reactiveVal(0L)
    observeEvent(input$refresh_data, {
      refreshed(refreshed() + 1L)
      catalog$refresh()
      showNotification("Read everything again from the database.", type = "message", duration = 3)
    })

    # Every extraction draft. Read again straight after this session changes one, which it says by
    # calling extractions$changed(); when a page that shows drafts is opened, or Refresh is pressed
    # (other staff change them too); and every few seconds while a draft this person started is being
    # read, so they see how far the reading job has got.
    extractions_written <- reactiveVal(0L)
    reading_checked <- reactiveVal(0L)
    # If the database can't be reached the last drafts read stay on screen, with a
    # message, and the next change tries again.
    last_extractions <- list()
    extractions <- list(
      all = reactive({
        extractions_written()
        reading_checked()
        refreshed()
        tryCatch(
          {
            last_extractions <<- extraction_service_get_all(database)
            last_extractions
          },
          error = function(problem) {
            showNotification(conditionMessage(problem), type = "error", id = "database_problem")
            last_extractions
          }
        )
      }),
      changed = function() extractions_written(extractions_written() + 1L)
    )

    # Drafts waiting to be read go to Gemini in a background job. Looked at whenever the drafts are
    # read, which is every few seconds while one this person started is being read (to show how far it
    # has got, and in case its job has died), and only then: the gold bar shows while it is checked.
    my_reading <- reactiveVal(FALSE)
    observe({
      records <- extractions$all()
      tryCatch(
        reading_service_watch(records, database),
        error = function(problem) showNotification(conditionMessage(problem), type = "error", id = "database_problem")
      )
      my_reading(extraction_service_is_reading(records, me))
    })
    waiting <- FALSE # whether the timer below has been started, so its next run is the timer's
    observe({
      if (!my_reading()) {
        waiting <<- FALSE
        return()
      }
      if (waiting) isolate(reading_checked(reading_checked() + 1L))
      waiting <<- TRUE
      invalidateLater(reading_check_ms)
    })

    # Modules call this too, so it names this session: theirs would look for
    # the menu inside the module.
    show_page <- function(page) nav_select("nav", page, session = session)
    on_page <- function(page) reactive(identical(input$nav, page))

    # What the other pages add to the database (an add on Make an edit, an
    # approved request, an extraction an ADMIN added) has to be in the drop-downs the next
    # time they are used, so the catalog is read again whenever a page that picks
    # from it is opened.
    observeEvent(input$nav, if (input$nav %in% c("extraction", "edit_extraction")) catalog$refresh())

    # Opening a page that lists drafts reads them again, since other staff change them too.
    observeEvent(input$nav,
      {
        if (input$nav %in% c("extraction", "edit_extraction", "change_requests")) extractions$changed()
      },
      ignoreInit = TRUE
    )

    # ------ MODULES -----------------------------------------------------------

    # ------ * Home ------------------------------------------------------------
    home_server("home", database, me, show_page, active = on_page("home"), refresh = refreshed)
    observeEvent(input$to_home, show_page("home"))

    # ------ * Extraction ------------------------------------------------------
    # Every way of starting an extraction opens its draft on the Edit Extraction
    # page (set up below).
    open_edit_extraction <- function(record, take_over = FALSE) {
      if (edit_extraction$open(record, take_over)) show_page("edit_extraction")
    }
    extraction_server("extraction", extractions, catalog, database, me, open_edit_extraction)

    # ------ * Edit Extraction -------------------------------------------------
    edit_extraction <- edit_extraction_server(
      "edit_extraction", extractions, catalog, database, me, show_page,
      active = on_page("edit_extraction")
    )

    # ------ * Make an edit ----------------------------------------------------
    make_edit <- make_edit_server("make_edit", database, me, show_page, active = on_page("make_edit"), refresh = refreshed)

    # ------ * Edit history ----------------------------------------------------
    edit_history_server("edit_history", database, active = on_page("edit_history"), refresh = refreshed)

    # ------ * Change Requests -------------------------------------------------
    # Editing a request opens it where it was made: an edit on Make an edit, an
    # extraction on the Edit Extraction page.
    change_requests_server(
      "change_requests", database, me,
      open_edit = function(record) {
        make_edit$open(record)
        show_page("make_edit")
      },
      open_edit_extraction = open_edit_extraction,
      catalog = catalog,
      extractions = extractions,
      active = on_page("change_requests"),
      refresh = refreshed
    )
    site <<- list(catalog = catalog, extractions = extractions)
    invisible()
  }

  # ------ START --------------------------------------------------------------
  # A development sign-in (STAFF_EMAIL) needs no password or link. Otherwise
  # the login screen waits.
  development_email <- trimws(Sys.getenv("STAFF_EMAIL"))
  if (nzchar(development_email)) {
    me <- tryCatch(staff_service_current(database, development_email), error = function(problem) problem)
    if (inherits(me, "error")) {
      say(conditionMessage(me))
    } else {
      start_site(me, development = TRUE)
    }
  }
}
