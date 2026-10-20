# ------ Signing in: Supabase Auth ---------------------------------------------------------
# Staff sign in to their Supabase Auth account (the project's Authentication section) with its
# password, or with a link emailed to its address; once signed in they can set or change the
# password from the menu. The site talks to Auth's REST API
# (`{SUPABASE_URL}/auth/v1/...`) with the publishable key, which is meant to be public. Who a
# signed-in user is for the site (their staff row and role) is decided in service_staff.R; this
# only talks to Auth. Errors are sentences staff can act on.
#
# The admin calls at the end need the secret key (`connection`, as the rest of the site uses it)
# and are for making and removing accounts: the tests use them for a throwaway account, and an
# ADMIN page could later.

auth_timeout_seconds <- 20

#' The connection to Supabase Auth.
#'
#' @param url The project's address; defaults to `SUPABASE_URL`.
#' @param key The publishable key; defaults to `SUPABASE_PUBLISHABLE_KEY`.
#' @return `list(url, key)`, as `connection` is for the database.
auth_model_connection <- function(url = Sys.getenv("SUPABASE_URL"), key = Sys.getenv("SUPABASE_PUBLISHABLE_KEY")) {
  list(url = url, key = key)
}

#' Email a sign-in link.
#'
#' Auth sends the email; clicking the link brings the person back to `redirect_to` with their tokens
#' after `#`, which the page script hands to the server (`staff_service_from_tokens()`). Only an
#' existing account gets a link: nobody signs up this way.
#'
#' @param auth The Auth connection, `list(url, key)`; see `auth_model_connection()`.
#' @param email The email.
#' @param redirect_to Where the link brings them back to: the site's own address. Auth only accepts
#'   addresses listed in its settings (Authentication, URL Configuration).
#' @return Called for its effect. An error with a sentence if Auth refused.
auth_model_send_link <- function(auth, email, redirect_to) {
  auth_request(auth, "otp") |>
    httr2::req_url_query(redirect_to = redirect_to) |>
    httr2::req_body_json(list(email = email, create_user = FALSE), auto_unbox = TRUE) |>
    auth_perform()
  invisible()
}

#' Sign in with an email and password.
#'
#' @param auth The Auth connection, `list(url, key)`; see `auth_model_connection()`.
#' @param email The email.
#' @param password The password.
#' @return A list with `access_token` and `refresh_token`, as a sign-in link brings back. An error with
#'   a sentence if Auth refused.
auth_model_sign_in <- function(auth, email, password) {
  auth_request(auth, "token") |>
    httr2::req_url_query(grant_type = "password") |>
    httr2::req_body_json(list(email = email, password = password), auto_unbox = TRUE) |>
    auth_perform()
}

#' Who a token belongs to.
#'
#' Asks Auth, so a token that was made up, has expired or was revoked is turned down.
#'
#' @param auth The Auth connection, `list(url, key)`; see `auth_model_connection()`.
#' @param access_token The access token.
#' @return A list: `id` and `email`. An error with a sentence if the token isn't good.
auth_model_user <- function(auth, access_token) {
  user <- auth_request(auth, "user") |>
    httr2::req_headers(Authorization = paste("Bearer", access_token), .redact = "Authorization") |>
    auth_perform()
  list(id = user$id, email = user$email)
}

#' Change the signed-in person's password.
#'
#' If the project asks for a recent sign-in before a password is changed (Authentication, Providers,
#' Email, Secure password change), a sign-in more than a day old is turned down; signing in again fixes it.
#'
#' @param auth The Auth connection, `list(url, key)`; see `auth_model_connection()`.
#' @param access_token The access token.
#' @param password The new password.
#' @return Called for its effect. An error with a sentence if Auth refused (a password that is too weak
#'   or the same as before, for instance).
auth_model_set_password <- function(auth, access_token, password) {
  auth_request(auth, "user") |>
    httr2::req_method("PUT") |>
    httr2::req_headers(Authorization = paste("Bearer", access_token), .redact = "Authorization") |>
    httr2::req_body_json(list(password = password), auto_unbox = TRUE) |>
    auth_perform()
  invisible()
}

#' Swap a refresh token for new tokens.
#'
#' Auth stops accepting the refresh token it was given, so the new one has to be kept instead.
#'
#' @param auth The Auth connection, `list(url, key)`; see `auth_model_connection()`.
#' @param refresh_token The refresh token.
#' @return A list with `access_token` and `refresh_token`. An error with a sentence if Auth refused.
auth_model_refresh <- function(auth, refresh_token) {
  auth_request(auth, "token") |>
    httr2::req_url_query(grant_type = "refresh_token") |>
    httr2::req_body_json(list(refresh_token = refresh_token), auto_unbox = TRUE) |>
    auth_perform()
}

#' Sign out.
#'
#' Auth revokes the session's refresh token, so the link or sign-in can't be used again.
#'
#' @param auth The Auth connection, `list(url, key)`; see `auth_model_connection()`.
#' @param access_token The access token.
#' @return Called for its effect. An error with a sentence if Auth refused.
auth_model_sign_out <- function(auth, access_token) {
  auth_request(auth, "logout") |>
    httr2::req_method("POST") |>
    httr2::req_headers(Authorization = paste("Bearer", access_token), .redact = "Authorization") |>
    auth_perform(expect_body = FALSE)
  invisible()
}

# ------ Accounts (the secret key) -------------------------------------------------------------
#' Make an account, as the project's administrator.
#'
#' The email is marked confirmed, so the account can sign in at once.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`. The secret key.
#' @param email The account's email.
#' @param password Its password.
#' @return The account's `id`.
auth_model_admin_create_user <- function(connection, email, password) {
  user <- auth_request(connection, "admin/users") |>
    httr2::req_headers(Authorization = paste("Bearer", connection$key), .redact = "Authorization") |>
    httr2::req_body_json(list(email = email, password = password, email_confirm = TRUE), auto_unbox = TRUE) |>
    auth_perform()
  user$id
}

#' Remove an account, as the project's administrator.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`. The secret key.
#' @param user_id The account's id.
#' @return Called for its effect.
auth_model_admin_delete_user <- function(connection, user_id) {
  if (!grepl("^[A-Za-z0-9-]+$", user_id %||% "")) stop("That isn't an account id.", call. = FALSE)
  auth_request(connection, paste0("admin/users/", user_id)) |>
    httr2::req_method("DELETE") |>
    httr2::req_headers(Authorization = paste("Bearer", connection$key), .redact = "Authorization") |>
    auth_perform(expect_body = FALSE)
  invisible()
}

# ------ Requests ------------------------------------------------------------------------------------
auth_request <- function(auth, path) {
  if (!nzchar(auth$url %||% "") || !nzchar(auth$key %||% "")) {
    stop(
      "The site can't sign anyone in: add SUPABASE_URL and SUPABASE_PUBLISHABLE_KEY to the .env file and restart the site.",
      call. = FALSE
    )
  }
  httr2::request(auth$url) |>
    httr2::req_url_path_append("auth", "v1", path) |>
    httr2::req_headers(apikey = auth$key, .redact = "apikey") |>
    httr2::req_timeout(auth_timeout_seconds) |>
    httr2::req_error(is_error = function(response) FALSE)
}

# Sends the request and returns the reply's body as a list, or stops with a sentence.
auth_perform <- function(request, expect_body = TRUE) {
  response <- tryCatch(httr2::req_perform(request), error = function(problem) {
    stop("Couldn't reach the sign-in service: ", conditionMessage(problem), call. = FALSE)
  })
  if (httr2::resp_status(response) >= 400) stop(auth_problem(response), call. = FALSE)
  if (!expect_body) {
    return(invisible(NULL))
  }
  jsonlite::fromJSON(httr2::resp_body_string(response), simplifyVector = FALSE)
}

# Auth's errors, as sentences. Auth has said the same thing two ways over the years (error_description, msg).
auth_problem <- function(response) {
  status <- httr2::resp_status(response)
  body <- tryCatch(httr2::resp_body_json(response), error = function(problem) list())
  said <- body$msg %||% body$error_description %||% body$message %||% body$error %||% ""
  code <- body$error_code %||% ""
  if (identical(code, "otp_disabled") || grepl("signups not allowed|otp_disabled", said, ignore.case = TRUE)) {
    return("There's no sign-in for this email. Ask an administrator to invite you.")
  }
  if (identical(code, "over_email_send_rate_limit") || grepl("email rate limit", said, ignore.case = TRUE)) {
    return("Too many sign-in emails were asked for. Wait a minute and try again.")
  }
  if (identical(code, "over_request_rate_limit") || grepl("rate limit", said, ignore.case = TRUE)) {
    return("Too many sign-in attempts. Wait a few minutes and try again.")
  }
  if (identical(code, "invalid_credentials") || grepl("invalid login credentials", said, ignore.case = TRUE)) {
    return("That email and password don't match. Try again, or have a sign-in link emailed to you.")
  }
  if (identical(code, "weak_password")) {
    return(if (nzchar(said)) paste0("Choose a stronger password: ", said) else "Choose a stronger password.")
  }
  if (identical(code, "same_password")) {
    return("Choose a password different from the one you have now.")
  }
  if (identical(code, "reauthentication_needed")) {
    return("Your sign-in is too old to change the password. Log out, sign in again, and change it then.")
  }
  if (status %in% c(401, 403)) {
    return(if (nzchar(said)) paste0("The sign-in isn't valid any more (", said, "). Sign in again.") else "The sign-in isn't valid any more. Sign in again.")
  }
  sprintf("The sign-in service had a problem (%s): %s", status, said)
}
