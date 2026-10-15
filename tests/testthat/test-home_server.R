# The Home page: the two boxes of newest requests and changes, and the buttons that open the other pages.
# The database is the real one.

# home_args <- function(me, active = NULL, connection = test_connection) {
#   shown <- character()
#   list(
#     args = list(connection = connection, me = me, show_page = function(page) shown <<- c(shown, page), active = active),
#     shown = function() shown
#   )
# }
#
# test_that("each button on the home page opens the page it names", {
#   me <- local_staff()
#   page <- home_args(me)
#   shiny::testServer(home_server, args = page$args, {
#     for (name in c("extraction", "make_edit", "edit_history", "change_requests")) do.call(session$setInputs, stats::setNames(list(1), name))
#   })
#   expect_equal(page$shown(), c("extraction", "make_edit", "edit_history", "change_requests"))
# })
#
# test_that("the two boxes list the newest requests and changes, and their buttons say how many there are", {
#   me <- local_staff()
#   colleague <- local_staff()
#   admin <- local_staff("ADMIN")
#   request <- local_edit(colleague, status = "Awaiting Review")
#   applied <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("Listed"))
#   shiny::testServer(home_server, args = home_args(me)$args, {
#     errors <- collect_errors(session)
#     requests_text <- text_of(output$requests)
#     expect_match(requests_text, request$summary, fixed = TRUE)
#     expect_match(requests_text, paste("Sent by", colleague$name))
#     expect_match(text_of(output$change_requests_more), "^See (more|all [0-9]+)$")
#
#     history_text <- text_of(output$history)
#     expect_match(history_text, history_service_get_all(test_connection)[[as.character(applied)]]$summary, fixed = TRUE)
#     expect_match(history_text, paste0("made by ", admin$name))
#     expect_match(text_of(output$edit_history_more), "^See (more|all [0-9]+)$")
#     expect_lte(length(history()$shown), home_recent_n)
#     expect_length(errors(), 0)
#   })
# })
#
# test_that("a request this person sent says so, and coming back to the page reads again", {
#   me <- local_staff()
#   active <- shiny::reactiveVal(TRUE)
#   here <- environment() # rows made inside the session are removed when this test ends
#   shiny::testServer(home_server, args = home_args(me, active = active)$args, {
#     session$flushReact() # the page's observers start (an observer's first run is its start-up)
#     before <- requests()$total
#     mine <- local_edit(me, status = "Awaiting Review", env = here)
#     expect_equal(requests()$total, before) # nothing has asked yet
#     active(FALSE)
#     active(TRUE)
#     session$flushReact()
#     expect_equal(requests()$total, before + 1)
#     expect_match(text_of(output$requests), "Sent by you")
#   })
# })
#
# test_that("a database that can't be reached says so under the lists", {
#   me <- local_staff()
#   seen <- local_browser()
#   down <- list(url = "http://127.0.0.1:1", key = "sb_secret_x")
#   shiny::testServer(home_server, args = home_args(me, connection = down)$args, {
#     errors <- collect_errors(session)
#     expect_match(text_of(output$requests), "couldn't be read")
#     expect_match(text_of(output$history), "couldn't be read")
#     expect_match(text_of(output$change_requests_more), "See more")
#     expect_match(notice_texts(seen), "Couldn't read the", all = FALSE)
#     expect_length(errors(), 0)
#   })
# })

test_that("the home page is drawn with its two buttons, its two boxes and a place for each list", {
  html <- htmltools::renderTags(home_ui("home"))$html
  expect_false(grepl("value-box", html))
  expect_match(html, 'id="home-extraction"')
  expect_match(html, 'id="home-make_edit"')
  expect_match(html, 'id="home-requests"')
  expect_match(html, 'id="home-history"')
  expect_match(html, "Change Requests")
  expect_match(html, "Edit history")
  expect_equal(lengths(regmatches(html, gregexpr("home-box-title", html))), 2) # two boxes
})
