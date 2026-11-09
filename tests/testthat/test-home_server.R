# The Home page: its banner (with how much this person has waiting), the cards for each part of the site and the
# steps an extraction takes, and what their buttons open. The database is the real one.

home_args <- function(me, active = NULL, connection = test_connection, extractions = fixed_extractions()) {
  did <- new.env()
  did$pages <- character()
  list(
    args = list(
      connection = connection, me = me, show_page = function(page) did$pages <- c(did$pages, page),
      extractions = extractions, active = active
    ),
    did = did
  )
}

test_that("the banner's buttons and the cards under What you can do here open the pages they name", {
  me <- list(staff_id = 1, name = "Maggie Kohn", role = "STAFF")
  local_stub("home_service_changes_added", function(connection) 0L)
  page <- home_args(me)
  shiny::testServer(home_server, args = page$args, {
    for (name in c("new_extraction", "browse_metadata", "go_extraction", "go_drafts", "go_metadata", "go_history", "go_monitor")) {
      do.call(session$setInputs, stats::setNames(list(1), name))
    }
  })
  expect_equal(page$did$pages, c("extraction", "make_edit", "extraction", "drafts", "make_edit", "edit_history", "ai_monitor"))
})

test_that("the banner counts this person's drafts in progress and the changes added", {
  me <- local_staff()
  local_history(me, "Added")
  record <- function(id, status, by = me$staff_id) list(id = id, status = status, prepared_by_id = by)
  drafts <- fixed_extractions(list(
    "1" = record("1", "Draft"),
    "2" = record("2", "Draft", by = me$staff_id + 1), # a colleague's isn't yours
    "3" = record("3", "Pushed") # nor is one in the database already
  ))
  page <- home_args(me, extractions = drafts)
  shiny::testServer(home_server, args = page$args, {
    errors <- collect_errors(session)
    stats <- text_of(output$stats)
    expect_match(stats, "1\\s*Your drafts in progress")
    expect_match(stats, "[1-9][0-9]*\\s*Changes added")
    expect_false(grepl("Waiting for you", stats)) # nothing waits for approval any more
    expect_length(errors(), 0)
  })
})

test_that("coming back to the page counts again", {
  me <- list(staff_id = 1, name = "Maggie Kohn", role = "STAFF")
  active <- shiny::reactiveVal(TRUE)
  reads <- 0L
  local_stub("home_service_changes_added", function(connection) {
    reads <<- reads + 1L
    reads
  })
  shiny::testServer(home_server, args = home_args(me, active = active, connection = NULL)$args, {
    session$flushReact() # the page's observers start (an observer's first run is its start-up)
    before <- reads
    expect_gt(before, 0)
    session$flushReact()
    expect_equal(reads, before) # nothing has asked yet
    active(FALSE)
    active(TRUE)
    session$flushReact()
    expect_gt(reads, before)
  })
})

test_that("a database that can't be reached leaves its count a dash, and says why", {
  me <- list(staff_id = 1, name = "Maggie Kohn", role = "STAFF")
  seen <- local_browser()
  down <- list(url = "http://127.0.0.1:1", key = "sb_secret_x")
  shiny::testServer(home_server, args = home_args(me, connection = down)$args, {
    errors <- collect_errors(session)
    stats <- text_of(output$stats)
    expect_match(stats, "0\\s*Your drafts in progress") # the drafts are the site's, read already
    expect_match(stats, "–\\s*Changes added")
    expect_match(notice_texts(seen), "Couldn't read the Edit History", all = FALSE)
    expect_length(errors(), 0)
  })
})

test_that("the home page is the banner, a card for each page and a step for each part of an extraction's way in", {
  html <- htmltools::renderTags(home_ui("home"))$html
  expect_match(html, 'class="home-hero"')
  expect_match(html, 'id="home-new_extraction"')
  expect_match(html, 'id="home-browse_metadata"')
  expect_match(html, 'id="home-stats"')
  expect_match(html, "logo_lg_transparent.png", fixed = TRUE)
  # a card for each page in the menu: New Extraction, Drafts, Metadata, History, AI Monitor; none for approvals
  expect_equal(lengths(regmatches(html, gregexpr('action-card"', html))), 5)
  for (id in c("go_extraction", "go_drafts", "go_metadata", "go_history", "go_monitor")) {
    expect_match(html, sprintf('id="home-%s"', id), fixed = TRUE, info = id)
  }
  expect_no_match(html, "go_requests|Change Request|approve")
  expect_equal(lengths(regmatches(html, gregexpr('class="step-card"', html))), 4)
  expect_match(html, "Push it into the database", fixed = TRUE)
  # the cards three to a row on a wide window, the steps four; two on a tablet and one on a phone (bslib's grid)
  expect_equal(lengths(regmatches(html, gregexpr('col-widths-xl="4"', html))), 1)
  expect_equal(lengths(regmatches(html, gregexpr('col-widths-xl="3"', html))), 1)
  # no lists of drafts or changes: the pages that list them are a click away
  expect_no_match(html, "Your work in progress</h2>|Recent changes|home-box")
})

test_that("a count that couldn't be read is a dash, not a number", {
  html <- htmltools::renderTags(home_stats(2L, NULL))$html
  expect_match(html, "2</span>")
  expect_match(html, "–</span>")
})

test_that("What you can do here has no heading of its own, and How it works has its heading and line", {
  html <- htmltools::renderTags(home_ui("home"))$html
  sections <- regmatches(html, gregexpr("(?s)<section class=\"home-section\">.*?</section>", html, perl = TRUE))[[1]]
  expect_length(sections, 2)
  expect_no_match(sections[1], "<h2")
  expect_match(sections[2], '<h2 class="section-title">How it works</h2>', fixed = TRUE)
  # nothing on the page is a heading holding a grid of cards
  expect_no_match(html, "<h2[^>]*>\\s*<div", perl = TRUE)
})
