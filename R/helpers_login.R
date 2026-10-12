# ------ Signing in, and the menu under the signed-in name --------------------------------------
# The login screen covers the site until server.R has a signed-in staff member, then goes. The menu in
# the top right names who is signed in and holds "Report a problem" (a link to the problem form) and
# "Log out". Both are drawn here; what they do is in server.R.

login_screen_id <- "login_screen"

# Where "Report a problem" goes: a Google Form, opened in a new tab.
report_problem_url <- "https://forms.gle/YhRLMXYgLUKbjFHB9"

#' The login screen: an email, and a sign-in link sent to it.
#'
#' Its inputs are `login_email`, `login_submit` and `login_message` (what went wrong, or that a link
#' was sent).
#'
#' @return A UI element that covers the page.
login_ui <- function() {
  div(
    id = login_screen_id, class = "login-screen",
    div(
      class = "login-card",
      div(class = "login-brand", span(class = "login-brand-mark"), "MATR Database Admin"),
      h1(class = "login-title", "Sign in"),
      p(
        class = "login-text",
        "Type the email of your MATR staff account. A sign-in link will be emailed to you; opening it signs you in. ",
        "There is no password."
      ),
      textInput("login_email", "Email", width = "100%", placeholder = "you@vcu.edu") |>
        tagAppendAttributes(class = "login-field"),
      actionButton("login_submit", "Email me a sign-in link", class = "btn-primary login-button", width = "100%"),
      uiOutput("login_message", class = "login-message")
    )
  )
}

#' What the login screen says under the button.
#'
#' @param text The sentence.
#' @param kind `"problem"` or `"note"`.
#' @return A UI element.
login_message <- function(text, kind = "problem") {
  p(class = paste0("login-note is-", kind), if (kind == "problem") icon("triangle-exclamation") else icon("envelope"), " ", text)
}

#' The menu under the signed-in name, in the top right.
#'
#' "Report a problem" is a link to the problem form (`report_problem_url`), opened in a new tab;
#' "Log out" is the input `log_out`.
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
      icon("circle-user"), span(class = "navbar-user-name", me$name)
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
      tags$li(tags$a(
        href = report_problem_url, target = "_blank", rel = "noopener", class = "dropdown-item",
        icon("bug"), " Report a problem"
      )),
      tags$li(actionLink("log_out", tagList(icon("right-from-bracket"), " Log out"), class = "dropdown-item"))
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
