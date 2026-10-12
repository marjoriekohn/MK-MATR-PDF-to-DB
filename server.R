function(input, output, session) {
  # ------ SIGNING IN ---------------------------------------------------------
  # Nothing is built until a staff member has signed in: they type their email,
  # Supabase Auth emails them a sign-in link, and the link brings them back with
  # their tokens (R/model_auth.R and R/service_staff.R). The login screen
  # (R/helpers_login.R) covers the site until then. While developing,
  # STAFF_EMAIL in .env signs the site in as that staff member with no link.
  auth <- auth_model_connection()
  tokens <- NULL
  # the sign-in's tokens, kept in this session only, for signing out

  start_site <- function(me, signed_in_tokens = NULL, development = FALSE) {
    tokens <<- signed_in_tokens
    removeUI(paste0("#", login_screen_id))
    output$navbar_user <- renderUI(user_menu(me, development))
    build_site(me)
  }

  # What the login screen says under the button. The box is hidden while it's
  # empty (helpers_login.R's styles), so it has to render while hidden, or
  # nothing would ever show. Saying something also ends the "Sending..." the
  # page script showed when the button was pressed (www/js/site.js).
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

  # The button: Auth emails a sign-in link to the address typed, if it is a
  # staff member's.
  observeEvent(input$login_submit, {
    here <- site_address(session)
    tryCatch(
      {
        staff_service_send_link(database, auth, input$login_email, here)
        say(paste0("A sign-in link is on its way to ", trimws(input$login_email), ". Open it on this computer."), kind = "note")
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
    if (!is.null(result)) start_site(result$me, result$tokens)
  })

  # A link that Supabase turned down (already used, or expired) says why instead of failing silently.
  observeEvent(input$auth_problem, {
    say(paste0("That sign-in link didn't work (", input$auth_problem, "). Request a new one."))
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

    # Every extraction draft. Read again when the database's copy changes (other
    # staff and the reading job change it too), and straight after this session
    # changes one, which it says by calling extractions$changed().
    extractions_written <- reactiveVal(0L)
    extractions_stored <- reactivePoll(
      drafts_poll_ms, session,
      checkFunc = function() extraction_service_signature(database),
      valueFunc = function() extraction_service_signature(database)
    )
    # If the database can't be reached the last drafts read stay on screen, with a
    # message, and the next change tries again.
    last_extractions <- list()
    extractions <- list(
      all = reactive({
        extractions_written()
        extractions_stored()
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

    # Drafts waiting to be read go to Gemini in a background job. Looked at
    # whenever the drafts change, and every few seconds while one is being read
    # in case its job has died, or the database couldn't be reached.
    observe({
      watching <- tryCatch(
        reading_service_watch(extractions$all(), database),
        error = function(problem) {
          showNotification(conditionMessage(problem), type = "error", id = "database_problem")
          TRUE
        }
      )
      if (watching) invalidateLater(reading_check_ms)
    })

    # Modules call this too, so it names this session: theirs would look for
    # the menu inside the module.
    show_page <- function(page) nav_select("nav", page, session = session)
    on_page <- function(page) reactive(identical(input$nav, page))

    # What the other pages add to the database (an add on Make an edit, an
    # approved request, a pushed extraction) has to be in the drop-downs the next
    # time they are used, so the catalog is read again whenever a page that picks
    # from it is opened.
    observeEvent(input$nav, if (input$nav %in% c("extraction", "review")) catalog$refresh())

    # ------ MODULES -----------------------------------------------------------

    # ------ * Home ------------------------------------------------------------
    home_server("home", database, me, show_page, active = on_page("home"))
    observeEvent(input$to_home, show_page("home"))

    # ------ * Extraction ------------------------------------------------------
    # Every way of starting an extraction opens its draft on the review page (set
    # up below).
    open_review <- function(record) {
      if (review$open(record)) show_page("review")
    }
    extraction_server("extraction", extractions, catalog, database, me, open_review)

    # ------ * Review extraction -----------------------------------------------
    review <- review_server("review", extractions, catalog, database, me, show_page)

    # ------ * Make an edit ----------------------------------------------------
    make_edit <- make_edit_server("make_edit", database, me, show_page)

    # ------ * Edit history ----------------------------------------------------
    edit_history_server("edit_history", database, active = on_page("edit_history"))

    # ------ * Change Requests -------------------------------------------------
    # Editing a request opens it where it was made: an edit on Make an edit, an
    # extraction on the review page.
    change_requests_server(
      "change_requests", database, me,
      open_edit = function(record) {
        make_edit$open(record)
        show_page("make_edit")
      },
      open_review = open_review
    )
    site <<- list(catalog = catalog, extractions = extractions)
    invisible()
  }

  # ------ START --------------------------------------------------------------
  # A development sign-in (STAFF_EMAIL) needs no link. Otherwise the login
  # screen waits.
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
