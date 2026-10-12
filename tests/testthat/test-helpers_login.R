# The login screen and the menu under the signed-in name: drawn with their inputs and links.

me <- list(staff_id = 1L, name = "Maggie Kohn", role = "STAFF", email = "maggie@example.edu")

test_that("the login screen asks for an email and offers a sign-in link, and nothing else", {
  login <- htmltools::renderTags(login_ui())$html
  for (id in c("login_screen", "login_email", "login_submit", "login_message")) {
    expect_match(login, sprintf('id="%s"', id))
  }
  expect_match(login, "Email me a sign-in link")
  expect_false(grepl('type="password"', login))
  expect_false(grepl("login_password", login))
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
  expect_match(menu, "Report a problem")
  expect_match(menu, sprintf('href="%s"', report_problem_url), fixed = TRUE)
  expect_match(menu, 'target="_blank"')
  expect_match(menu, 'rel="noopener"')
  expect_false(grepl("report_problem", menu))
  expect_false(grepl("mailto:", menu))
  expect_false(grepl("development sign-in", menu))
  expect_match(htmltools::renderTags(user_menu(me, development = TRUE))$html, "development sign-in")
})

test_that("the problem form's address is the Google Form", {
  expect_match(report_problem_url, "^https://forms\\.gle/")
})
