# Signing in with Supabase Auth: the project's Authentication section, tried with a throwaway account that is
# made and removed through the admin API (helper-auth.R).

test_that("a token says who it is, and signing out is quiet", {
  skip_without_auth()
  account <- local_auth_user()
  signed <- test_sign_in(account)
  expect_match(signed$access_token, "^[A-Za-z0-9_.-]+$")
  expect_true(nzchar(signed$refresh_token))

  who <- auth_model_user(test_auth(), signed$access_token)
  expect_equal(tolower(who$email), account$email)
  expect_equal(who$id, account$id)

  expect_invisible(auth_model_sign_out(test_auth(), signed$access_token))
})

test_that("a sign-in link is only sent to an email that has an account", {
  skip_without_auth()
  expect_error(
    auth_model_send_link(test_auth(), "zztest-nobody@example.test", "http://localhost:5599/"),
    "Ask an administrator to invite you"
  )
})

test_that("a token that was made up is turned down", {
  skip_without_auth()
  expect_error(auth_model_user(test_auth(), "not.a.token"), "isn't valid any more")
})

test_that("an account id has to look like one before it is sent", {
  expect_error(auth_model_admin_delete_user(test_connection, "1 or 1=1"), "isn't an account id")
  expect_error(auth_model_admin_delete_user(test_connection, NULL), "isn't an account id")
})

test_that("missing keys and an unreachable service are sentences that say what to do", {
  here <- "http://localhost:5599/"
  unreachable <- list(url = "http://127.0.0.1:1", key = "sb_publishable_x")
  expect_error(auth_model_send_link(list(url = "", key = ""), "a@b.c", here), "add SUPABASE_URL and SUPABASE_PUBLISHABLE_KEY")
  expect_error(auth_model_send_link(unreachable, "a@b.c", here), "Couldn't reach the sign-in service")
})

test_that("what Auth says is turned into a sentence, in either of the ways it says it", {
  reply <- function(status, body) {
    httr2::response(status_code = status, headers = list("content-type" = "application/json"), body = charToRaw(body))
  }
  no_account <- "Ask an administrator to invite you"
  expect_match(auth_problem(reply(422, '{"error_code":"otp_disabled","msg":"Signups not allowed for otp"}')), no_account)
  expect_match(auth_problem(reply(422, '{"error":"otp_disabled","error_description":"Signups not allowed for otp"}')), no_account)
  expect_match(auth_problem(reply(429, '{"error_code":"over_email_send_rate_limit","msg":"email rate limit exceeded"}')), "Wait a minute")
  expect_match(auth_problem(reply(401, '{"msg":"invalid JWT"}')), "isn't valid any more \\(invalid JWT\\)")
  expect_match(auth_problem(reply(500, "not json")), "^The sign-in service had a problem \\(500\\)")
})
