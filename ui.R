page_navbar(
  id = "nav",
  # The title is the way back to the home page.
  title = actionLink("to_home", "MATR Database Admin"),
  window_title = "MATR Database Admin",
  theme = vcu_theme,
  fillable = "edit_extraction",
  navbar_options = navbar_options(theme = "dark", collapsible = FALSE),
  header = tagList(
    tags$head(
      merriweather_link,
      tags$script(src = "js/site.js"),
      tags$script(src = "js/review.js"),
      tags$script(src = "js/extraction.js"),
      tags$script(src = "js/edit.js"),
      tags$script(src = "js/busy.js"),
      tags$script(src = "js/variables.js")
    ),
    # While the server is working: a gold bar across the top of the page, a spinner on each output being
    # redrawn, and (busy.js) a spinner on the button that was pressed.
    useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = TRUE),
    busyIndicatorOptions(
      spinner_type = "ring2", spinner_color = "#FFB300", spinner_size = "2rem", spinner_delay = "300ms",
      pulse_background = "linear-gradient(90deg, #FFB300, #000000, #FFB300)", pulse_height = "5px"
    ),
    # Covers the site until someone signs in (server.R takes it away); see
    # helpers_login.R.
    login_ui()
  ),

  # The pages' menu links are hidden (_page.scss): the home page's buttons lead
  # to them, and other pages open the Edit Extraction page. nav_panel_hidden() can't be
  # selected in a navbar.
  nav_panel("Home", home_ui("home"), value = "home"),
  nav_panel("Extraction", extraction_ui("extraction"), value = "extraction"),
  nav_panel("Edit Extraction", edit_extraction_ui("edit_extraction"), value = "edit_extraction"),
  nav_panel("Make an edit", make_edit_ui("make_edit"), value = "make_edit"),
  nav_panel("Edit history", edit_history_ui("edit_history"), value = "edit_history"),
  nav_panel("Change Requests", change_requests_ui("change_requests"), value = "change_requests"),
  nav_spacer(),
  # Refresh (reads everything again, with what other staff changed), then the signed-in name, with the
  # menu that reports a problem and logs out. Both are drawn once someone has signed in.
  nav_item(uiOutput("navbar_refresh", inline = TRUE)),
  nav_item(uiOutput("navbar_user", inline = TRUE))
)
