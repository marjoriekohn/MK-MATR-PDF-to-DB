# The login screen and the menu under the signed-in name: drawn with their inputs and links.

me <- list(staff_id = 1L, name = "Maggie Kohn", role = "STAFF", email = "maggie@example.edu")

test_that("the login screen asks for an email and password, or offers a sign-in link instead", {
  login <- htmltools::renderTags(login_ui())$html
  for (id in c("login_screen", "login_email", "login_password", "login_sign_in", "login_send_link", "login_message")) {
    expect_match(login, sprintf('id="%s"', id))
  }
  expect_match(login, 'type="password"')
  expect_match(login, 'autocomplete="current-password"')
  expect_match(login, "Email me a sign-in link instead")
  expect_match(login, "spam folder")
  expect_match(login, 'data-busy-label="Signing in…"', fixed = TRUE)
  expect_false(grepl("There is no password", login))
})

test_that("the menu's password dialog asks for the new password twice", {
  html <- htmltools::renderTags(password_dialog())$html
  for (id in c("new_password", "new_password_again", "password_change_submit", "password_message")) {
    expect_match(html, sprintf('id="%s"', id))
  }
  expect_match(html, 'autocomplete="new-password"')
  expect_match(html, sprintf("At least %d characters", password_min_length))
})

test_that("what the login screen says is a problem or a note", {
  expect_match(htmltools::renderTags(login_message("Wrong."))$html, "is-problem")
  expect_match(htmltools::renderTags(login_message("Sent.", kind = "note"))$html, "is-note")
})

test_that("the menu names the person, links to the problem form in a new tab, and logs out", {
  menu <- htmltools::renderTags(user_menu(me))$html
  expect_match(menu, "Maggie Kohn")
  expect_match(menu, "maggie@example.edu")
  expect_match(menu, 'id="log_out"')
  expect_match(menu, 'id="change_password"')
  expect_match(menu, "Report a problem")
  expect_match(menu, sprintf('href="%s"', report_problem_url), fixed = TRUE)
  expect_match(menu, 'target="_blank"')
  expect_match(menu, 'rel="noopener"')
  expect_false(grepl("report_problem", menu))
  expect_false(grepl("mailto:", menu))
  expect_false(grepl("development sign-in", menu))
  development <- htmltools::renderTags(user_menu(me, development = TRUE))$html
  expect_match(development, "development sign-in")
  expect_false(grepl("change_password", development))
})

test_that("the problem form's address is the Google Form", {
  expect_match(report_problem_url, "^https://forms\\.gle/")
})

test_that("the menu's Refresh asks the server to read everything again", {
  button <- htmltools::renderTags(refresh_button())$html
  expect_match(button, 'id="refresh_data"')
  expect_match(button, "Refresh")
  expect_match(button, "navbar-refresh")
})
