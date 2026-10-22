# ------ Signing in, and the menu under the signed-in name --------------------------------------
# The login screen covers the site until server.R has a signed-in staff member, then goes. The menu
# in the top right names who is signed in and holds "Update Password", "Report a problem" (a
# link to the problem form) and "Log out". All are drawn here; what they do is
# in server.R. While a button's request is on its way it says what is happening (`data-busy-label`,
# which www/js/site.js reads) and can't be pressed again.

login_screen_id <- "login_screen"

# Where "Report a problem" goes: a Google Form, opened in a new tab.
report_problem_url <- "https://forms.gle/YhRLMXYgLUKbjFHB9"

#' The login screen: an email and password, or a sign-in link emailed instead.
#'
#' Its inputs are `login_email`, `login_password`, `login_sign_in` (the password), `login_send_link`
#' and `login_message` (what went wrong, or that a link was sent).
#'
#' @return A UI element that covers the page.
login_ui <- function() {
  login_card(
    id = login_screen_id,
    title = "Sign in",
    p(
      class = "login-text",
      "Sign in with your VCU email and your password. No password yet, or forgotten it? ",
      "Have a sign-in link emailed to you instead (check your spam folder if it doesn't come), ",
      "then set one under your name in the top right."
    ),
    textInput("login_email", NULL, width = "100%", placeholder = "you@vcu.edu") |>
      tag_input_attributes("input", `aria-label` = "Email", autocomplete = "email") |>
      tagAppendAttributes(class = "login-field"),
    passwordInput("login_password", NULL, width = "100%", placeholder = "Password") |>
      tag_input_attributes("input", `aria-label` = "Password", autocomplete = "current-password") |>
      tagAppendAttributes(class = "login-field"),
    login_button("login_sign_in", "Sign in", "Signing in…"),
    login_button("login_send_link", "Email me a sign-in link instead", "Sending a sign-in link…", primary = FALSE),
    uiOutput("login_message", class = "login-message")
  )
}

#' Setting or changing the password from the menu, once signed in.
#'
#' @return A modal dialog with `password_fields()`.
password_dialog <- function() {
  modalDialog(
    title = "Update Password",
    password_fields(),
    footer = modalButton("Cancel"),
    size = "s",
    easyClose = TRUE
  )
}

#' The new password, twice, and the button that saves it.
#'
#' Its inputs are `new_password`, `new_password_again`, `password_change_submit` and
#' `password_message` (what went wrong).
#'
#' @return A UI element.
password_fields <- function() {
  div(
    class = "password-fields",
    passwordInput("new_password", NULL, width = "100%", placeholder = "New password") |>
      tag_input_attributes("input", `aria-label` = "New password", autocomplete = "new-password") |>
      tagAppendAttributes(class = "login-field"),
    passwordInput("new_password_again", NULL, width = "100%", placeholder = "The new password again") |>
      tag_input_attributes("input", `aria-label` = "The new password again", autocomplete = "new-password") |>
      tagAppendAttributes(class = "login-field"),
    p(class = "login-hint", sprintf("At least %d characters.", password_min_length)),
    login_button("password_change_submit", "Save the password", "Saving…"),
    uiOutput("password_message", class = "login-message")
  )
}

# The card the login screen is drawn on, covering the page.
login_card <- function(id, title, ...) {
  div(
    id = id, class = "login-screen",
    div(
      class = "login-card",
      div(class = "login-brand", span(class = "login-brand-mark"), "MATR Database Admin"),
      h1(class = "login-title", title),
      ...
    )
  )
}

# A button on the login card. `busy` is what it says while its request is on its way.
login_button <- function(id, label, busy, primary = TRUE) {
  actionButton(
    id, label,
    class = paste(if (primary) "btn-primary" else "btn-outline-secondary", "login-button"),
    width = "100%", `data-busy-label` = busy
  )
}

#' What the login screen says under the button.
#'
#' @param text The sentence.
#' @param kind `"problem"` or `"note"`.
#' @return A UI element.
login_message <- function(text, kind = "problem") {
  p(class = paste0("login-note is-", kind), if (kind == "problem") app_icon("problem") else app_icon("email"), " ", text)
}

#' The menu under the signed-in name, in the top right.
#'
#' "Update Password" is the input `change_password` (not for a development sign-in, which has no
#' password); "Report a problem" is a link to the problem form (`report_problem_url`), opened in a new
#' tab; "Log out" is the input `log_out`.
#'
#' @param me Who is signed in: `list(staff_id, name, role, email)`.
#' @param development Whether this is a development sign-in (STAFF_EMAIL), which the menu says.
#' @return A UI element.
user_menu <- function(me, development = FALSE) {
  div(
    class = "dropdown navbar-user",
    tags$button(
      type = "button", class = "btn navbar-user-button dropdown-toggle",
      `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      app_icon("account"), span(class = "navbar-user-name", me$name)
    ),
    tags$ul(
      class = "dropdown-menu dropdown-menu-end navbar-user-menu",
      tags$li(div(
        class = "dropdown-item-text navbar-user-who",
        strong(me$name),
        if (has_text(me$email)) div(class = "navbar-user-email", me$email),
        div(class = "navbar-user-role", paste0(me$role %||% "STAFF", if (development) " · development sign-in"))
      )),
      tags$li(tags$hr(class = "dropdown-divider")),
      if (!development) tags$li(actionLink("change_password", tagList(app_icon("password"), " Update Password"), class = "dropdown-item")),
      tags$li(tags$a(
        href = report_problem_url, target = "_blank", rel = "noopener", class = "dropdown-item",
        app_icon("report"), " Report a problem"
      )),
      tags$li(actionLink("log_out", tagList(app_icon("log_out"), " Log out"), class = "dropdown-item"))
    )
  )
}

#' The site's own address, for a sign-in link to bring the person back to.
#'
#' @param session The Shiny session.
#' @return A string such as `"https://matr.example.edu/admin/"`, or `""` if the browser hasn't said.
site_address <- function(session) {
  tryCatch(
    {
      client <- session$clientData
      port <- client$url_port %||% ""
      paste0(
        client$url_protocol %||% "", "//", client$url_hostname %||% "", if (nzchar(port)) paste0(":", port),
        client$url_pathname %||% ""
      )
    },
    error = function(problem) ""
  )
}
