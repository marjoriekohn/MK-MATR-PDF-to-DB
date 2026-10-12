# Who is using the site, found in the staff table of the real database. Each test makes the staff it needs.

# test_that("whoever STAFF_EMAIL names is the person using the site, whatever its case", {
#   me <- local_staff()
#   expect_equal(
#     staff_service_current(test_connection, me$email),
#     list(staff_id = me$staff_id, name = me$name, role = "STAFF", email = me$email)
#   )
#   expect_equal(
#     staff_service_current(test_connection, paste0("  ", toupper(me$email), " ")),
#     list(staff_id = me$staff_id, name = me$name, role = "STAFF", email = me$email)
#   )
# })
#
# test_that("the person's role comes with them", {
#   admin <- local_staff("ADMIN")
#   expect_equal(staff_service_current(test_connection, admin$email)$role, "ADMIN")
#   expect_true(all(staff_roles %in% c("STAFF", "ADMIN")))
# })

# test_that("STAFF_EMAIL is read from the environment by default", {
#   me <- local_staff()
#   withr::local_envvar(STAFF_EMAIL = me$email)
#   expect_equal(staff_service_current(test_connection)$staff_id, me$staff_id)
# })
#
# test_that("a missing, unknown or inactive email is a sentence that says what to do", {
#   former <- local_staff(active = FALSE)
#   expect_error(staff_service_current(test_connection, ""), "Add STAFF_EMAIL")
#   expect_error(
#     staff_service_current(test_connection, "zztest-nobody@example.test"),
#     "no staff member with the email zztest-nobody@example.test.*check STAFF_EMAIL"
#   )
#   expect_error(staff_service_current(test_connection, former$email), paste(former$name, "is marked as no longer active"))
# })
#
# test_that("wildcard characters in an email never match someone else's", {
#   me <- local_staff() # an email like zztest0712ab-3@example.test
#   with_underscore <- sub("-", "_", me$email, fixed = TRUE)
#   expect_error(staff_service_current(test_connection, with_underscore), "no staff member")
#   expect_error(staff_service_current(test_connection, "%@example.test"), "no staff member")
#   expect_error(staff_service_current(test_connection, "*"), "no staff member")
# })
#
# test_that("a staff table the key can't read is the database's own sentence", {
#   skip_if(!nzchar(Sys.getenv("SUPABASE_PUBLISHABLE_KEY")), "SUPABASE_PUBLISHABLE_KEY isn't set")
#   me <- local_staff()
#   publishable <- list(url = test_connection$url, key = Sys.getenv("SUPABASE_PUBLISHABLE_KEY"))
#   expect_error(staff_service_current(publishable, me$email), "won't let the site use this table")
# })
#
# test_that("a review can be asked of any active staff member but the person asking", {
#   me <- local_staff()
#   colleague <- local_staff(name = paste(test_marker, "Colleague"))
#   admin <- local_staff("ADMIN")
#   former <- local_staff(active = FALSE)
#   reviewers <- staff_service_reviewers(test_connection, me)
#   expect_equal(reviewers[[colleague$name]], as.character(colleague$staff_id))
#   expect_equal(reviewers[[admin$name]], as.character(admin$staff_id))
#   expect_false(me$name %in% names(reviewers))
#   expect_false(former$name %in% names(reviewers))
#   expect_type(reviewers, "character")
# })
#
# # ------ Signing in ----------------------------------------------------------------------------------------
# test_that("the tokens a sign-in link brings back say who the person is for the site", {
#   skip_without_auth()
#   me <- local_staff("ADMIN")
#   account <- local_auth_user(email = me$email)
#   signed <- staff_service_from_tokens(test_connection, test_auth(), test_sign_in(account))
#   expect_equal(signed$me, list(staff_id = me$staff_id, name = me$name, role = "ADMIN", email = me$email))
#   expect_named(signed$tokens, c("access_token", "refresh_token"))
#   expect_true(nzchar(signed$tokens$access_token))
#   expect_invisible(staff_service_sign_out(test_auth(), signed$tokens))
# })
#
# test_that("an account whose email isn't an active staff member's is told, and signed out again", {
#   skip_without_auth()
#   stranger <- local_auth_user()
#   expect_error(
#     staff_service_from_tokens(test_connection, test_auth(), test_sign_in(stranger)),
#     "no staff member with the email .*Ask an administrator"
#   )
#   former <- local_staff(active = FALSE)
#   former_account <- local_auth_user(email = former$email)
#   expect_error(staff_service_from_tokens(test_connection, test_auth(), test_sign_in(former_account)), "no longer active")
# })
#
# test_that("a bad token and a missing token are sentences, before anything is asked of Auth", {
#   unreachable <- list(url = "http://127.0.0.1:1", key = "sb_publishable_x")
#   expect_error(staff_service_from_tokens(test_connection, unreachable, list()), "didn't bring a sign-in")
#   skip_without_auth()
#   expect_error(staff_service_from_tokens(test_connection, test_auth(), list(access_token = "made.up.token")), "isn't valid any more")
# })
#
# test_that("a sign-in link is only asked for a staff member's email, so Auth never hears of anyone else", {
#   unreachable <- list(url = "http://127.0.0.1:1", key = "sb_publishable_x")
#   here <- "http://localhost:5599/"
#   expect_error(staff_service_send_link(test_connection, unreachable, "", here), "Type your email")
#   expect_error(staff_service_send_link(test_connection, unreachable, "   ", here), "Type your email")
#   expect_error(staff_service_send_link(test_connection, unreachable, "zztest-nobody@example.test", here), "no staff member")
#   me <- local_staff()
#   expect_error(staff_service_send_link(test_connection, unreachable, me$email, here), "Couldn't reach the sign-in service")
# })

test_that("signing out with no tokens, or with tokens Auth has forgotten, is quiet", {
  expect_invisible(staff_service_sign_out(list(url = "http://127.0.0.1:1", key = "x"), NULL))
  expect_invisible(staff_service_sign_out(list(url = "http://127.0.0.1:1", key = "x"), list(access_token = "gone")))
})
