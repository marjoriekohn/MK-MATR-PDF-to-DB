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
      tags$script(src = "js/edit.js")
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
  # The signed-in name, with the menu that reports a problem and logs out
  nav_item(uiOutput("navbar_user", inline = TRUE))
)
