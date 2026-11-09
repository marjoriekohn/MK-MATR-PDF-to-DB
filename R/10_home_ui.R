# The Home page: a banner saying what the site is for, with how much this person has waiting, then a card for
# each part of the site and the steps an extraction takes into the database. The menu leads to every page.
home_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page home-page",
    `data-ns` = ns(""),
    home_hero(ns),
    home_section(
      # three to a row on a wide window, two by two on a tablet, one under another on a phone
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6, xl = 4), fill = FALSE,
        home_action(ns("go_extraction"), "upload", "New Extraction", "Upload a questionnaire PDF for Gemini to read, or type one in yourself."),
        home_action(ns("go_drafts"), "drafts", "Extraction Drafts", "Pick up a draft, yours or a colleague's, that isn't pushed yet."),
        home_action(ns("go_metadata"), "metadata", "Metadata", "View, add, change or delete registries, studies, questionnaires, waves and more."),
        home_action(ns("go_history"), "history", "Edit History", "Every change made to the database through this site, newest first."),
        home_action(ns("go_monitor"), "ai_monitor", "AI Monitor", "How often staff correct what the AI read, and where.")
      )
    ),
    home_section(
      title = "How it works",
      text = "From a questionnaire PDF to the MATR metadata database.",
      layout_columns(
        col_widths = breakpoints(sm = 12, md = 6, xl = 3), fill = FALSE,
        home_step(1, "upload", "Upload the questionnaire", "Choose its PDF on New Extraction. Gemini reads it in the background."),
        home_step(2, "edit", "Check every block", "On Edit Extraction, fix what was read and mark each block as checked."),
        home_step(3, "metadata", "Push it into the database", "Give its registry, study, questionnaire and wave in Basic Info, then push it. It goes in at once."),
        home_step(4, "history", "Keep it up to date", "On Metadata, change what is there or empty a wave and extract it again. History keeps every change.")
      )
    )
  )
}

# The banner across the top: what the site is for, the two ways in, and how much this person has waiting
# (`output$stats`, home_stats()). Dark, with the MATR logo on the right of a wide window.
home_hero <- function(ns) {
  div(
    class = "home-hero",
    div(
      class = "home-hero-text",
      span(class = "eyebrow is-on-dark", app_icon("metadata"), "Mid-Atlantic Twin Registry"),
      h1("MATR Metadata Database"),
      p(
        class = "home-hero-lead",
        "Turn questionnaire PDFs into structured metadata, and keep the registry's studies, questionnaires and ",
        "waves up to date, all in one place."
      ),
      div(
        class = "home-hero-actions",
        actionButton(ns("new_extraction"), tagList("Start a New Extraction", app_icon("next_step")), class = "btn-primary"),
        actionButton(ns("browse_metadata"), "Browse Metadata", class = "btn-on-dark")
      ),
      uiOutput(ns("stats"), class = "home-stats")
    ),
    div(
      class = "home-hero-art", `aria-hidden` = "true",
      tags$img(src = "img/logo_lg_transparent.png", alt = ""),
      div(class = "home-hero-caption", app_icon("metadata"), "Questionnaires and variables in one database")
    )
  )
}

# How much this person has waiting, in the banner: their drafts in progress and the changes added so far. A count
# that couldn't be read is a dash.
home_stats <- function(work, changes) {
  stat <- function(n, label) div(class = "home-stat", span(class = "home-stat-value", n %||% "–"), span(class = "home-stat-label", label))
  tagList(stat(work, "Your drafts in progress"), stat(changes, "Changes added"))
}

# A part of the page, under a heading of its own if it has one, with a line under the heading if that has one.
home_section <- function(..., title = NULL, text = NULL) {
  tags$section(
    class = "home-section",
    if (!is.null(title)) h2(class = "section-title", title),
    if (!is.null(text)) p(class = "subhead section-text", text),
    ...
  )
}

# A card that opens a page: its icon in a tile, its name and what it is for. The whole card is the link.
# input_id: The link's input id; the server opens the page.
home_action <- function(input_id, icon_name, title, text) {
  actionLink(
    input_id,
    tagList(
      span(class = "icon-tile", app_icon(icon_name)),
      span(class = "action-card-title", title),
      span(class = "action-card-text", text),
      span(class = "action-card-go", "Open", app_icon("next_step"))
    ),
    class = "action-card"
  )
}

# A step of how an extraction gets into the database: its number, its icon, what it is and a line about it.
home_step <- function(number, icon_name, title, text) {
  div(
    class = "step-card",
    span(class = "icon-tile is-dark", app_icon(icon_name)),
    span(class = "step-number", `aria-hidden` = "true", number),
    h3(class = "step-title", span(class = "visually-hidden", paste0("Step ", number, ": ")), title),
    p(class = "step-text", text)
  )
}
