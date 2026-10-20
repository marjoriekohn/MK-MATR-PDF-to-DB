#' # ------ Sign-in accounts for the tests -----------------------------------------------------------------
#' # Signing in is tested against the test project's Supabase Auth, with a throwaway account made through the
#' # admin API (the secret key) and removed when the test ends. The site signs people in with an emailed
#' # link, which no test can open, so the account is given a password and test_sign_in() turns it into the
#' # tokens a link would bring back. The test project's publishable key (TEST_SUPABASE_PUBLISHABLE_KEY) has
#' # to be in .env; without it these tests are skipped.
#'
#' skip_without_auth <- function() {
#'   testthat::skip_if(!nzchar(Sys.getenv("SUPABASE_PUBLISHABLE_KEY")), "SUPABASE_PUBLISHABLE_KEY isn't set, so signing in can't be tried")
#' }
#'
#' # The Auth connection the site would use: the publishable key.
#' test_auth <- function() auth_model_connection(test_connection$url, Sys.getenv("SUPABASE_PUBLISHABLE_KEY"))
#'
#' # A password for a throwaway account (the tests' way to its tokens): long, mixed, different every time,
#' # never a real one.
#' test_password <- function() paste0("Zz!", paste(sample(c(letters, LETTERS, 0:9), 20, replace = TRUE), collapse = ""))
#'
#' #' A sign-in account for the test, removed when the test ends.
#' #'
#' #' @param email Its email; a marked one by default.
#' #' @param password Its password.
#' #' @param env The test's environment.
#' #' @return A list: `id`, `email` and `password`.
#' local_auth_user <- function(email = sprintf("%s-auth-%d@example.test", tolower(test_marker), test_counter()),
#'                             password = test_password(), env = parent.frame()) {
#'   id <- auth_model_admin_create_user(test_connection, email, password)
#'   withr::defer(try(auth_model_admin_delete_user(test_connection, id), silent = TRUE), envir = env)
#'   list(id = id, email = email, password = password)
#' }
#'
#' #' The tokens of a throwaway account, as a sign-in link would bring them back.
#' #'
#' #' Auth's password grant, which only the tests use: the site itself has no passwords.
#' #'
#' #' @param account From `local_auth_user()`.
#' #' @return A list: `access_token` and `refresh_token`.
#' test_sign_in <- function(account) {
#'   body <- auth_request(test_auth(), "token") |>
#'     httr2::req_url_query(grant_type = "password") |>
#'     httr2::req_body_json(list(email = account$email, password = account$password), auto_unbox = TRUE) |>
#'     auth_perform()
#'   list(access_token = body$access_token, refresh_token = body$refresh_token)
#' }
