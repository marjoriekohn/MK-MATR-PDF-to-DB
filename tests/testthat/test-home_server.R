# The Home page: this person's work in progress, the requests waiting for them and the newest changes, and what
# their buttons open. The database is the real one.

home_args <- function(me, active = NULL, connection = test_connection, extractions = test_extractions(connection),
                      catalog = test_live_catalog(connection)) {
  did <- new.env()
  did$pages <- character()
  did$extractions <- list()
  did$edits <- list()
  did$reviews <- character()
  list(
    args = list(
      connection = connection, me = me, show_page = function(page) did$pages <- c(did$pages, page),
      extractions = extractions, catalog = catalog,
      open_extraction = function(record) did$extractions[[length(did$extractions) + 1]] <- record,
      open_edit = function(record) did$edits[[length(did$edits) + 1]] <- record,
      review_request = function(id) did$reviews <- c(did$reviews, id),
      active = active
    ),
    did = did
  )
}

press <- function(session, action, id, kind = NULL) {
  session$setInputs(action = Filter(Negate(is.null), list(action = action, id = as.character(id), kind = kind)))
}

test_that("New Extraction and the buttons under the lists open the pages they name", {
  me <- local_staff()
  page <- home_args(me)
  shiny::testServer(home_server, args = page$args, {
    for (name in c("new_extraction", "extraction_drafts", "edit_history", "change_requests")) do.call(session$setInputs, stats::setNames(list(1), name))
  })
  expect_equal(page$did$pages, c("extraction", "drafts", "edit_history", "change_requests"))
})

test_that("your work in progress is your extraction drafts and saved edits, and Continue opens each where it is worked on", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  extraction <- local_read_extraction(me, info = picked_basic_info(tree))
  edit <- local_edit(me)
  theirs <- local_edit(colleague)
  sent <- local_edit(me, status = "Awaiting Review")
  page <- home_args(me)
  shiny::testServer(home_server, args = page$args, {
    errors <- collect_errors(session)
    ids <- vapply(work(), function(item) item$record$id, character(1))
    expect_setequal(ids, c(extraction$id, edit$id)) # not a colleague's, nor one sent for approval
    listed <- text_of(output$work)
    expect_match(listed, edit$summary, fixed = TRUE)
    expect_match(listed, paste0(tree$questionnaire$name, " (ZZQ) · ", tree$wave$name), fixed = TRUE)
    expect_match(listed, "Continue")
    press(session, "continue", extraction$id, "extraction")
    press(session, "continue", edit$id, "edit")
    expect_length(errors(), 0)
  })
  expect_equal(page$did$extractions[[1]]$id, extraction$id)
  expect_equal(page$did$edits[[1]]$id, edit$id)
  expect_false(theirs$id %in% vapply(page$did$edits, function(record) record$id, character(1)))
})

test_that("waiting for you are the requests you can approve, with Review; one you sent isn't", {
  me <- local_staff()
  colleague <- local_staff()
  theirs <- local_edit(colleague, status = "Awaiting Review")
  mine <- local_edit(me, status = "Awaiting Review")
  page <- home_args(me)
  shiny::testServer(home_server, args = page$args, {
    errors <- collect_errors(session)
    ids <- vapply(requests()$shown, function(record) record$id, character(1))
    expect_false(mine$id %in% ids)
    waiting <- change_request_service_filter(change_request_service_waiting(test_connection), me, "me")
    expect_equal(requests()$total, length(waiting))
    expect_true(theirs$id %in% names(waiting))
    if (theirs$id %in% ids) expect_match(text_of(output$requests), paste("Sent by", colleague$name))
    expect_match(text_of(output$change_requests_more), "^See (more|all [0-9]+)$")
    press(session, "review", theirs$id)
    expect_length(errors(), 0)
  })
  expect_equal(page$did$reviews, theirs$id)
})

test_that("recent changes are the newest applied, and their button says how many there are", {
  me <- local_staff()
  admin <- local_staff("ADMIN")
  applied <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("Listed"))
  shiny::testServer(home_server, args = home_args(me)$args, {
    history_text <- text_of(output$history)
    expect_match(history_text, history_service_get_all(test_connection)[[as.character(applied)]]$summary, fixed = TRUE)
    expect_match(history_text, paste0("made by ", admin$name))
    expect_match(text_of(output$edit_history_more), "^See (more|all [0-9]+)$")
    expect_lte(length(history()$shown), home_recent_n)
  })
})

test_that("coming back to the page reads its lists again", {
  me <- local_staff()
  colleague <- local_staff()
  active <- shiny::reactiveVal(TRUE)
  here <- environment() # rows made inside the session are removed when this test ends
  shiny::testServer(home_server, args = home_args(me, active = active)$args, {
    session$flushReact() # the page's observers start (an observer's first run is its start-up)
    before <- requests()$total
    local_edit(colleague, status = "Awaiting Review", env = here)
    expect_equal(requests()$total, before) # nothing has asked yet
    active(FALSE)
    active(TRUE)
    session$flushReact()
    expect_equal(requests()$total, before + 1)
  })
})

test_that("a database that can't be reached says so in each box", {
  me <- local_staff()
  seen <- local_browser()
  down <- list(url = "http://127.0.0.1:1", key = "sb_secret_x")
  none <- list(all = shiny::reactive(list()), changed = function() NULL)
  shiny::testServer(home_server, args = home_args(me, connection = down, extractions = none, catalog = fixed_catalog())$args, {
    errors <- collect_errors(session)
    expect_match(text_of(output$work), "Nothing in progress")
    expect_match(text_of(output$requests), "couldn't be read")
    expect_match(text_of(output$history), "couldn't be read")
    expect_match(text_of(output$change_requests_more), "See more")
    expect_match(notice_texts(seen), "Couldn't read the", all = FALSE)
    expect_length(errors(), 0)
  })
})

test_that("the home page is drawn with New Extraction, its three boxes and a place for each list", {
  html <- htmltools::renderTags(home_ui("home"))$html
  expect_match(html, 'id="home-new_extraction"')
  expect_match(html, 'id="home-work"')
  expect_match(html, 'id="home-requests"')
  expect_match(html, 'id="home-history"')
  expect_match(html, "Your work in progress")
  expect_match(html, "Waiting for you")
  expect_match(html, "Recent changes")
  expect_equal(lengths(regmatches(html, gregexpr("home-box-title", html))), 3)
})
