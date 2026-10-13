# ------ Who is using the site ------------------------------------------------------
# Staff sign in with a link emailed to their Supabase Auth account (model_auth.R); the email
# of that account has to be the email of an active row in the staff table, which says who they
# are for the site and their role. The rest of the site gets `me`, list(staff_id, name, role,
# email).
#
# While developing, STAFF_EMAIL (in .env) signs the site in as that staff member without a
# link. It must be empty where the site serves real use (docs/environment.md).

# The two roles. An ADMIN never needs approval to change the database; see
# service_change_request.R for what a STAFF member needs it for.
staff_roles <- c("STAFF", "ADMIN")

#' The staff member an email belongs to.
#'
#' Errors are sentences saying what to do.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param email The email, as the sign-in gave it.
#' @return `list(staff_id, name, role, email)`; the role is `"STAFF"` or `"ADMIN"`. An error if the
#'   email is unknown or the person is inactive.
staff_service_for_email <- function(connection, email) {
  email <- trimws(email %||% "")
  staff <- if (nzchar(email)) staff_model_find_by_email(connection, email)
  if (is.null(staff)) {
    stop(
      sprintf("There's no staff member with the email %s in the MATR database. ", email),
      "Ask an administrator to add you to the staff table.",
      call. = FALSE
    )
  }
  if (!isTRUE(staff$active)) {
    stop(sprintf("%s is marked as no longer active in the staff table.", staff$name), call. = FALSE)
  }
  list(staff_id = staff$staff_id, name = staff$name, role = staff$role %||% "STAFF", email = staff$email)
}

#' Who is using the site, while developing.
#'
#' The staff member whose email is in `STAFF_EMAIL`, with no sign-in link. Errors are sentences saying
#' what to do.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param email The email; defaults to `STAFF_EMAIL`.
#' @return `list(staff_id, name, role, email)`. An error if the email is missing, unknown or inactive.
staff_service_current <- function(connection, email = Sys.getenv("STAFF_EMAIL")) {
  email <- trimws(email)
  if (!nzchar(email)) {
    stop(
      "The site doesn't know who is using it. Add STAFF_EMAIL (your email in the ",
      "staff table) to the .env file and restart the site.",
      call. = FALSE
    )
  }
  tryCatch(staff_service_for_email(connection, email), error = function(problem) {
    stop(sub("Ask an administrator to add you to the staff table.", "Add them to the staff table, or check STAFF_EMAIL in the .env file.", conditionMessage(problem)), call. = FALSE)
  })
}

#' Sign in with the tokens a sign-in link brought back.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param auth The Auth connection, `list(url, key)`; see `auth_model_connection()`.
#' @param tokens A list with `access_token` and, if there is one, `refresh_token`: from a sign-in link, or from swapping a kept refresh token on a reload (`auth_model_refresh()`).
#' @return A list: `me` (`list(staff_id, name, role, email)`) and `tokens` (`access_token`,
#'   `refresh_token`), which the session keeps for signing out. An error with a sentence if the token
#'   isn't good or the person isn't an active staff member.
staff_service_from_tokens <- function(connection, auth, tokens) {
  access_token <- tokens$access_token %||% ""
  if (!nzchar(access_token)) stop("The sign-in link didn't bring a sign-in with it. Ask for another.", call. = FALSE)
  user <- auth_model_user(auth, access_token)
  me <- tryCatch(staff_service_for_email(connection, user$email), error = function(problem) {
    try(auth_model_sign_out(auth, access_token), silent = TRUE)
    stop(problem)
  })
  list(me = me, tokens = list(access_token = access_token, refresh_token = tokens$refresh_token))
}

#' Sign in again with a refresh token the browser kept, after a reload.
#'
#' Auth swaps it for new tokens and stops accepting the old one, so the browser has to keep the
#' new `refresh_token` this returns.
#'
#' @inheritParams staff_service_from_tokens
#' @param refresh_token The refresh token the browser kept.
#' @return As `staff_service_from_tokens()`. An error with a sentence if Auth refused or the person
#'   isn't an active staff member.
staff_service_resume <- function(connection, auth, refresh_token) {
  fresh <- auth_model_refresh(auth, refresh_token)
  staff_service_from_tokens(connection, auth, list(
    access_token = fresh$access_token, refresh_token = fresh$refresh_token
  ))
}

#' Email a sign-in link.
#'
#' Only to an email that is an active staff member's, so the site never asks Auth about anyone else.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param auth The Auth connection, `list(url, key)`; see `auth_model_connection()`.
#' @param email The email.
#' @param redirect_to The site's own address, which the link brings them back to.
#' @return Called for its effect. An error with a sentence if the email is blank or isn't a staff member's,
#'   or Auth refused.
staff_service_send_link <- function(connection, auth, email, redirect_to) {
  if (!has_text(email)) stop("Type your email.", call. = FALSE)
  me <- staff_service_for_email(connection, email)
  auth_model_send_link(auth, me$email, redirect_to)
  invisible()
}

#' Sign out.
#'
#' @param auth The Auth connection, `list(url, key)`; see `auth_model_connection()`.
#' @param tokens The session's tokens, from signing in; `NULL` for a development sign-in, which has none.
#' @return Called for its effect. A refusal from Auth is not an error: the session ends either way.
staff_service_sign_out <- function(auth, tokens) {
  if (!is.null(tokens$access_token)) try(auth_model_sign_out(auth, tokens$access_token), silent = TRUE)
  invisible()
}

#' The staff a request can be sent to for review.
#'
#' Active staff other than the person asking.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @return A named character vector for a drop-down: the names are shown, the values are the
#'   staff ids; empty if there is no one else.
staff_service_reviewers <- function(connection, me) {
  staff <- staff_model_active(connection)
  staff <- staff[staff$staff_id != me$staff_id, , drop = FALSE]
  stats::setNames(as.character(staff$staff_id), staff$name)
}
