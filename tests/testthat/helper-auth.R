# ------ Sign-in accounts for the tests -----------------------------------------------------------------
# Signing in is tested against the test project's Supabase Auth, with a throwaway account made through the
# admin API (the secret key) and removed when the test ends. An emailed sign-in link can't be opened by a
# test, so the account is given a password and test_sign_in() turns it into the tokens a link would bring
# back. The test project's publishable key (TEST_SUPABASE_PUBLISHABLE_KEY) has
# to be in .env; without it these tests are skipped.

# The test project's publishable key. Read from TEST_SUPABASE_PUBLISHABLE_KEY itself, not SUPABASE_PUBLISHABLE_KEY:
# anything that reads .env again (a script a test sources) puts the real database's key back in that one.
test_publishable_key <- function() Sys.getenv("TEST_SUPABASE_PUBLISHABLE_KEY")

skip_without_auth <- function() {
  testthat::skip_if(!nzchar(test_publishable_key()), "TEST_SUPABASE_PUBLISHABLE_KEY isn't in .env, so signing in can't be tried")
}

# The Auth connection the site would use: the publishable key.
test_auth <- function() auth_model_connection(test_connection$url, test_publishable_key())

# A password for a throwaway account (the tests' way to its tokens): long, mixed, different every time,
# never a real one.
test_password <- function() paste0("Zz!", paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE), collapse = ""))

#' A sign-in account for the test, removed when the test ends.
#'
#' @param email Its email; a marked one by default.
#' @param password Its password.
#' @param env The test's environment.
#' @return A list: `id`, `email` and `password`.
local_auth_user <- function(email = sprintf("%s-auth-%d@example.test", tolower(test_marker), test_counter()),
                            password = test_password(), env = parent.frame()) {
  id <- auth_model_admin_create_user(test_connection, email, password)
  withr::defer(try(auth_model_admin_delete_user(test_connection, id), silent = TRUE), envir = env)
  list(id = id, email = email, password = password)
}

#' The tokens of a throwaway account, as a sign-in link would bring them back.
#'
#' @param account From `local_auth_user()`.
#' @return A list: `access_token` and `refresh_token`.
test_sign_in <- function(account) {
  body <- auth_model_sign_in(test_auth(), account$email, account$password)
  list(access_token = body$access_token, refresh_token = body$refresh_token)
}
