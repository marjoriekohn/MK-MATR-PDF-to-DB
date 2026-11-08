# The Home page: its banner (with how much this person has waiting), the cards for each part of the site and the
# steps an extraction takes, and what their buttons open. The database is the real one.

home_args <- function(me, active = NULL, connection = test_connection, extractions = test_extractions(connection)) {
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
  me <- local_staff()
  page <- home_args(me)
  shiny::testServer(home_server, args = page$args, {
    for (name in c("new_extraction", "browse_metadata", "go_extraction", "go_drafts", "go_metadata", "go_requests")) {
      do.call(session$setInputs, stats::setNames(list(1), name))
    }
  })
  expect_equal(page$did$pages, c("extraction", "make_edit", "extraction", "drafts", "make_edit", "change_requests"))
})

test_that("the banner counts this person's work in progress, what is waiting for them and the changes added", {
  me <- local_staff()
  colleague <- local_staff()
  local_edit(me)
  local_edit(colleague) # a colleague's isn't yours
  local_edit(me, status = "Awaiting Review") # nor is one sent for approval
  page <- home_args(me)
  shiny::testServer(home_server, args = page$args, {
    errors <- collect_errors(session)
    stats <- text_of(output$stats)
    expect_match(stats, "1\\s*Your work in progress")
    expect_match(stats, "[0-9]+\\s*Waiting for you")
    expect_match(stats, "[0-9]+\\s*Changes added")
    expect_length(errors(), 0)
  })
})

test_that("coming back to the page counts again", {
  me <- local_staff()
  active <- shiny::reactiveVal(TRUE)
  # the reads of what is waiting are counted (not what they find: other runs of the tests send requests meanwhile)
  reads <- 0L
  real <- local_stub("change_request_service_waiting", function(connection) {
    reads <<- reads + 1L
    real(connection)
  })
  shiny::testServer(home_server, args = home_args(me, active = active)$args, {
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

test_that("a database that can't be reached leaves its counts a dash, and says why", {
  me <- local_staff()
  seen <- local_browser()
  down <- list(url = "http://127.0.0.1:1", key = "sb_secret_x")
  none <- list(all = shiny::reactive(list()), changed = function() NULL)
  shiny::testServer(home_server, args = home_args(me, connection = down, extractions = none)$args, {
    errors <- collect_errors(session)
    stats <- text_of(output$stats)
    expect_match(stats, "0\\s*Your work in progress") # nothing read means nothing saved
    expect_match(stats, "–\\s*Waiting for you")
    expect_match(stats, "–\\s*Changes added")
    expect_match(notice_texts(seen), "Couldn't read the", all = FALSE)
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
  expect_equal(lengths(regmatches(html, gregexpr('action-card"', html))), 4)
  expect_equal(lengths(regmatches(html, gregexpr('class="step-card"', html))), 4)
  # four to a row on a wide window, two on a tablet and one on a phone (bslib's grid)
  expect_equal(lengths(regmatches(html, gregexpr('col-widths-xl="3"', html))), 2)
  # no lists of drafts, requests or changes: the pages that list them are a click away
  expect_no_match(html, "Your work in progress</h2>|Recent changes|home-box")
})

test_that("a count that couldn't be read is a dash, not a number", {
  html <- htmltools::renderTags(home_stats(2L, NULL, 9L))$html
  expect_match(html, "2</span>")
  expect_match(html, "–</span>")
  expect_match(html, "9</span>")
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
