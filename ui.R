page_navbar(
  id = "nav",
  # The title, the MATR logo beside the site's name, is the way back to the home page.
  title = actionLink(
    "to_home",
    tagList(
      tags$img(src = "img/logo_sm_transparent.png", class = "brand-logo", alt = ""),
      span(class = "brand-text", span(class = "brand-name", "MATR Metadata Database"), span(class = "brand-tagline", "Mid-Atlantic Twin Registry"))
    ),
    `aria-label` = "MATR Metadata Database: home"
  ),
  window_title = "MATR Metadata Database",
  theme = vcu_theme,
  fillable = c("edit_extraction", "make_edit"), # an outline beside its details, filling the window
  # The pages' links, in the bar; on a narrow window they fold into a menu button (☰)
  # A light bar; the page picked is a tinted pill (_header.scss), not underlined.
  navbar_options = navbar_options(theme = "light", collapsible = TRUE, underline = FALSE),
  header = tagList(
    tags$head(
      merriweather_link,
      page_script("js/site.js"),
      page_script("js/review.js"),
      page_script("js/split.js"),
      page_script("js/extraction.js"),
      page_script("js/busy.js")
    ),
    # While the server is working: a bar across the top of the page, and nothing else (no spinners, no
    # fading); its colours are components/_busy.scss's. busy.js holds the button that was pressed so it isn't
    # pressed twice.
    useBusyIndicators(spinners = FALSE, pulse = TRUE, fade = FALSE),
    # Covers the site until someone signs in (server.R takes it away); see
    # helpers_login.R.
    login_ui()
  ),

  # Home (the title goes there) and Edit Extraction (opened from a draft) have no link of their own in the bar
  # (_header.scss). nav_panel_hidden() can't be selected in a navbar.
  nav_panel("Home", home_ui("home"), value = "home"),
  nav_panel("New Extraction", extraction_ui("extraction"), value = "extraction", icon = app_icon("upload")),
  nav_panel("Drafts", extraction_drafts_page_ui("drafts"), value = "drafts", icon = app_icon("drafts")),
  nav_panel("Edit Extraction", edit_extraction_ui("edit_extraction"), value = "edit_extraction"),
  nav_panel("Metadata", make_edit_ui("make_edit"), value = "make_edit", icon = app_icon("metadata")),
  nav_panel("Requests", change_requests_ui("change_requests"), value = "change_requests", icon = app_icon("change_requests")),
  nav_panel("History", edit_history_ui("edit_history"), value = "edit_history", icon = app_icon("history")),
  nav_spacer(),

  # Running on the test database (MATR_DATABASE=test in .env; global.R) says so, so nobody mistakes it for live.
  if (identical(matr_database, "test")) nav_item(span(class = "badge navbar-test-database", "Test database")),

  # The signed-in name, with the menu that reports a problem and logs out, drawn once someone has signed in.
  nav_item(uiOutput("navbar_user", inline = TRUE))
)
