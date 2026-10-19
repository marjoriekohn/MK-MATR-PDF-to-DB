# server.R: signing in, who is using the site, the drafts every page shares, and the menu. The pages
# themselves are tested on their own; this is what ties them together. The database is the real one.
#
# test_that("with a development sign-in the site starts as the staff member STAFF_EMAIL names, and says who in the menu", {
#   me <- local_staff("ADMIN")
#   seen <- local_browser()
#   withr::local_envvar(STAFF_EMAIL = toupper(me$email))
#   shiny::testServer(site_server(), {
#     expect_equal(signed_in()$name, me$name)
#     expect_equal(signed_in()$staff_id, me$staff_id)
#     expect_equal(signed_in()$role, "ADMIN")
#     expect_null(session_state()$tokens)
#     menu <- text_of(output$navbar_user)
#     expect_match(menu, me$name, fixed = TRUE)
#     expect_match(menu, "development sign-in")
#     expect_match(menu, "Report a problem")
#     expect_match(output$navbar_user$html, report_problem_url, fixed = TRUE)
#     expect_match(menu, "Log out")
#     expect_true("registries" %in% names(session_state()$site$catalog$get()))
#   })
#   expect_length(seen$modals, 0)
# })
#
# test_that("a STAFF_EMAIL that names nobody, or someone inactive, leaves the login screen up and says why", {
#   seen <- local_browser()
#   withr::local_envvar(STAFF_EMAIL = "zztest-nobody@example.test")
#   shiny::testServer(site_server(), {
#     expect_null(signed_in())
#     expect_match(text_of(output$login_message), "no staff member with the email zztest-nobody@example.test")
#     expect_match(text_of(output$login_message), "check STAFF_EMAIL")
#   })
#   former <- local_staff(active = FALSE)
#   withr::local_envvar(STAFF_EMAIL = former$email)
#   shiny::testServer(site_server(), {
#     expect_null(signed_in())
#     expect_match(text_of(output$login_message), "no longer active")
#   })
# })
#
# test_that("without a development sign-in the site waits at the login screen, and asking for a link needs a staff member's email", {
#   seen <- local_browser()
#   withr::local_envvar(STAFF_EMAIL = "")
#   shiny::testServer(site_server(), {
#     expect_null(signed_in())
#     expect_null(session_state()$site)
#     session$setInputs(login_email = "", login_submit = 1)
#     expect_match(text_of(output$login_message), "Type your email")
#     session$setInputs(login_email = "zztest-nobody@example.test", login_submit = 2)
#     expect_match(text_of(output$login_message), "no staff member")
#     expect_null(signed_in())
#   })
# })
#
# test_that("the tokens a sign-in link brings back sign the person in, the login screen goes, and logging out ends the sign-in", {
#   skip_without_auth()
#   me <- local_staff()
#   account <- local_auth_user(email = me$email)
#   signed <- test_sign_in(account)
#   seen <- local_browser()
#   withr::local_envvar(STAFF_EMAIL = "")
#   signed_out <- NULL
#   local_stub("staff_service_sign_out", function(auth, tokens) signed_out <<- tokens)
#   shiny::testServer(site_server(), {
#     session$setInputs(auth_tokens = list(access_token = "made.up", refresh_token = "x", type = "magiclink"))
#     expect_null(signed_in())
#     expect_match(text_of(output$login_message), "isn't valid any more")
#     session$setInputs(auth_tokens = list(access_token = signed$access_token, refresh_token = signed$refresh_token, type = "magiclink"))
#     expect_equal(signed_in()$staff_id, me$staff_id)
#     expect_equal(session_state()$tokens$access_token, signed$access_token)
#     expect_match(text_of(output$navbar_user), me$name, fixed = TRUE)
#     expect_false(grepl("development sign-in", text_of(output$navbar_user)))
#     session$setInputs(log_out = 1)
#     expect_equal(signed_out$access_token, signed$access_token)
#   })
# })
#
# test_that("the menu's title and the home page's buttons show the pages they name, and opening a page reads the catalog again", {
#   me <- local_staff()
#   seen <- local_browser()
#   withr::local_envvar(STAFF_EMAIL = me$email)
#   here <- environment() # rows made inside the session are removed when this test ends
#   shiny::testServer(site_server(), {
#     session$setInputs(to_home = 1)
#     session$setInputs(`home-make_edit` = 1)
#     session$setInputs(`home-extraction` = 1)
#     catalog <- session_state()$site$catalog
#     before <- nrow(catalog$get()$registries)
#     added <- local_registry(env = here)
#     expect_equal(nrow(catalog$get()$registries), before) # nothing has asked yet
#     session$setInputs(nav = "extraction")
#     expect_equal(nrow(catalog$get()$registries), before + 1)
#   })
#   expect_equal(seen$pages, c("home", "make_edit", "extraction"))
# })
#
# test_that("the drafts are read from the database, and a draft added elsewhere is noticed when the database is next asked", {
#   me <- local_staff()
#   seen <- local_browser()
#   withr::local_envvar(STAFF_EMAIL = me$email)
#   first <- local_read_extraction(me)
#   here <- environment() # rows made inside the session are removed when this test ends
#   shiny::testServer(site_server(), {
#     errors <- collect_errors(session)
#     extractions <- session_state()$site$extractions
#     expect_true(first$id %in% names(extractions$all()))
#     second <- local_read_extraction(me, env = here) # another session, or the reading job
#     expect_false(second$id %in% names(extractions$all())) # nothing has asked yet
#     session$elapse(drafts_poll_ms + 100)
#     expect_true(second$id %in% names(extractions$all()))
#
#     third <- local_read_extraction(me, env = here)
#     extractions$changed() # a draft this session changes is read again straight away
#     session$flushReact()
#     expect_true(third$id %in% names(extractions$all()))
#     expect_length(errors(), 0)
#   })
# })
#
# test_that("a database that can't be reached leaves the last drafts on screen, with a message, until it is back", {
#   me <- local_staff()
#   seen <- local_browser()
#   withr::local_envvar(STAFF_EMAIL = me$email)
#   first <- local_read_extraction(me)
#   net <- new.env()
#   net$down <- FALSE
#   real <- extraction_service_get_all
#   local_stub("extraction_service_get_all", function(connection) {
#     if (net$down) stop("Couldn't connect to the server", call. = FALSE)
#     real(connection)
#   })
#   here <- environment() # rows made inside the session are removed when this test ends
#   shiny::testServer(site_server(), {
#     extractions <- session_state()$site$extractions
#     expect_true(first$id %in% names(extractions$all()))
#     net$down <- TRUE
#     extractions$changed()
#     session$flushReact()
#     expect_true(first$id %in% names(extractions$all())) # the last drafts stay
#     expect_match(notice_texts(seen), "Couldn't connect", all = FALSE)
#     expect_equal(utils::tail(seen$notifications, 1)[[1]]$type, "error")
#     net$down <- FALSE
#     second <- local_read_extraction(me, env = here)
#     session$elapse(drafts_poll_ms + 100)
#     expect_true(second$id %in% names(extractions$all()))
#   })
# })

test_that("the site is drawn with the login screen over it, and the menu's place in the top right", {
  withr::local_dir(app_file())
  env <- shiny::loadSupport()
  source("global.R", local = env)
  rendered <- htmltools::renderTags(eval(parse("ui.R", keep.source = FALSE, encoding = "UTF-8"), env))
  html <- rendered$html
  expect_match(html, 'id="login_screen"')
  expect_match(html, 'id="navbar_user"')
  expect_match(paste(rendered$head, html), "js/site.js")
  expect_true(regexpr('id="login_screen"', html) < regexpr('id="home-extraction"', html))
  # while the server works, the page says so: Shiny's busy indicators, and the pressed button's spinner
  expect_match(paste(rendered$head, html), "js/busy.js")
  expect_match(paste(rendered$head, html), "js/variables.js")
  expect_match(paste(rendered$head, html), "shinyBusySpinners", fixed = TRUE)
  expect_match(paste(rendered$head, html), "shinyBusyPulse", fixed = TRUE)
})
