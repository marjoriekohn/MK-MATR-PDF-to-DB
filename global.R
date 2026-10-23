# ------ R version -------------------------------------------------------------
# The code uses `%||%`, which base R has had since 4.4.
if (getRversion() < "4.4.0") {
  stop("This site needs R 4.4 or newer; this is R ", getRversion(), ".", call. = FALSE)
}

# ------ Load packages ---------------------------------------------------------
library(shiny)
library(bslib)
library(sass)

# ------ R options -------------------------------------------------------------
# Questionnaire PDFs are larger than Shiny's 5 MB upload limit. This code
# increases the maximum allowed file upload size in an R Shiny web application
# from the default 5 MB to 50 MB.
options(shiny.maxRequestSize = 50 * 1024^2)

# The Gemini API key (GEMINI_KEY) that reads PDFs, the MATR database's address
# and keys (SUPABASE_URL, SUPABASE_SECRET_KEY, and SUPABASE_PUBLISHABLE_KEY for
# signing in) and, while developing, who is using the site (STAFF_EMAIL) live
# in .env, which git ignores. Reading jobs inherit them.
if (file.exists(".env")) readRenviron(".env")

# ------ Global variables ------------------------------------------------------
# VCU brand theme: Bootstrap variables in defaults.scss, styles in rules.scss. The stylesheet is compiled once and
# kept (sass's cache) by what it is made of, but sass only looks at the two files named here, not the ones they
# import: a fingerprint of every file in www/sass goes in too, so an edit to any of them is compiled, not an old copy.
vcu_theme <- bs_theme(
  version = 5,
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto")
) |>
  bs_bundle(sass_layer(
    defaults = sass_file("www/sass/defaults.scss"),
    rules = list(
      sass_file("www/sass/rules.scss"),
      sprintf("/* www/sass %s */", digest::digest(lapply(
        sort(list.files("www/sass", pattern = "\\.scss$", recursive = TRUE, full.names = TRUE)), readLines, warn = FALSE
      )))
    )
  ))

# Merriweather (subheads/callouts), loaded in ui.R via head tag
merriweather_link <- tags$link(
  rel = "stylesheet",
  href = "https://fonts.googleapis.com/css2?family=Merriweather:wght@400;700&display=swap"
)

# ------ The MATR database -----------------------------------------------------
# Everything the site shows and saves is in the MATR database on Supabase: the
# live tables (registries, studies, questionnaires, waves...) and the drafts and
# edit proposals staff are still working on. The server talks to it through
# Supabase REST API (R/model_database.R). `database` is made once here and
# passed to the modules as `connection`. If the two variables below are missing
# the site says so when it starts.
database <- list(url = Sys.getenv("SUPABASE_URL"),
                 key = Sys.getenv("SUPABASE_SECRET_KEY"))

# The site doesn't keep asking the database whether something changed: each time
# it asks, the page shows it is busy. Pages read again when they are opened, and a
# reload of the browser page reads everything again. The one exception: while a draft a
# staff member started is being read by Gemini, their drafts are read every
# `reading_check_ms` milliseconds, so they see how far it has got (and a reading
# job that died is noticed).
reading_check_ms <- 5000
