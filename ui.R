page_navbar(
  id = "nav",
  # The title is the way back to the home page.
  # TODO: add logo (logo is in www/img)
  title = actionLink("to_home", "Metadata Database"),
  window_title = "MATR Metadata Database",
  theme = vcu_theme,
  fillable = c("edit_extraction", "make_edit"), # an outline beside its details, filling the window
  # The pages' links, in the bar; on a narrow window they fold into a menu button (☰)
  navbar_options = navbar_options(theme = "dark", collapsible = TRUE),
  header = tagList(
    tags$head(
      merriweather_link,
      page_script("js/site.js"),
      page_script("js/review.js"),
      page_script("js/extraction.js"),
      page_script("js/busy.js")
    ),
    # TODO: move CSS to SASS files
    # While the server is working: a gold bar across the top of the page, and nothing else (no spinners,
    # no fading); busy.js holds the button that was pressed so it isn't pressed twice.
    useBusyIndicators(spinners = FALSE, pulse = TRUE, fade = FALSE),
    busyIndicatorOptions(
      pulse_background = "linear-gradient(90deg, #FFB300, #000000, #FFB300)", pulse_height = "5px"
    ),
    # Covers the site until someone signs in (server.R takes it away); see
    # helpers_login.R.
    login_ui()
  ),

  # Home (the title goes there) and Edit Extraction (opened from a draft) have no link of their own in the bar
  # (_header.scss). nav_panel_hidden() can't be selected in a navbar.
  nav_panel("Home", home_ui("home"), value = "home"),
  nav_panel("New Extraction", extraction_ui("extraction"), value = "extraction"),
  nav_panel("Drafts", extraction_drafts_page_ui("drafts"), value = "drafts"),
  nav_panel("Edit Extraction", edit_extraction_ui("edit_extraction"), value = "edit_extraction"),
  nav_panel("Metadata", make_edit_ui("make_edit"), value = "make_edit"),
  nav_panel("Requests", change_requests_ui("change_requests"), value = "change_requests"),
  nav_panel("History", edit_history_ui("edit_history"), value = "edit_history"),
  nav_spacer(),

  # The signed-in name, with the menu that reports a problem and logs out, drawn once someone has signed in.
  nav_item(uiOutput("navbar_user", inline = TRUE))
)
