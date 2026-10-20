# How much of the site's code the unit tests run. From the project folder:
#   Rscript tests/coverage/coverage.R
# GitHub runs it on every push to main (.github/workflows/coverage.yaml) and puts the overall number
# in the README's coverage badge. It needs the `covr` package, which the site itself doesn't (so it
# isn't in renv.lock): install.packages("covr"). It is in tests/coverage/ and not tests/ because
# shiny::runTests() runs every script directly in tests/.
#
# covr measures a package; this is an app, so the files are given to it directly: global.R
# and everything in R/ are the code, and the helpers and tests in tests/testthat/ are what
# runs it. The numbers are for lines of code that were run at least once. They say nothing
# about whether the tests check the right things; they show what no test has run at all.
# (server.R and ui.R aren't in the count: they are evaluated by the tests, not sourced.)
# The tests that need the database are commented out, so the model_ files count as barely run.
#
# A test that fails doesn't stop the count: it is listed, and the numbers are still given. (The
# tests workflow is the one that says whether they pass.)
#
# Set COVERAGE_BADGE to a file name to have the badge drawn there as an SVG, e.g.
#   COVERAGE_BADGE=coverage.svg Rscript tests/coverage/coverage.R

# covr reads the files in the session's encoding; the code is UTF-8 (it prints "·", "—"), and read
# any other way its strings can't be sorted or compared.
if (!l10n_info()[["UTF-8"]]) {
  for (locale in c("C.UTF-8", "en_US.UTF-8", "UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", locale)))) break
  }
}

suppressMessages({
  library(shiny)
  library(covr)
  library(testthat)
})

# paths are from the project folder
code <- c("global.R", sort(list.files("R", pattern = "\\.R$", full.names = TRUE)))

# covr runs each test file as a script, and the first test that failed would end the run. So it is
# given one script instead, which has testthat run the folder (helpers first, as shiny::runTests()
# does), carrying on past failures, in the environment covr has put the code in.
runner <- tempfile("run_tests_", fileext = ".R")
writeLines(c(
  "assign('.test_results', testthat::test_dir(",
  "  'tests/testthat', env = environment(), reporter = testthat::SummaryReporter$new(),",
  "  stop_on_failure = FALSE, stop_on_warning = FALSE",
  "), envir = globalenv())"
), runner)
coverage <- covr::file_coverage(code, runner)

results <- as.data.frame(.test_results)
failed <- sum(results$failed) + sum(results$error)

lines <- as.data.frame(coverage)
lines <- aggregate(value ~ filename + first_line, lines, max)
by_file <- aggregate(cbind(lines = 1, run = value > 0) ~ filename, lines, sum)
by_file$percent <- round(100 * by_file$run / by_file$lines)
by_file$kind <- ifelse(grepl("^R/model_", by_file$filename), "model",
  ifelse(grepl("^R/service_", by_file$filename), "service",
    ifelse(grepl("^R/helpers_", by_file$filename), "helpers",
      ifelse(grepl("^R/[0-9]", by_file$filename), "module", "other")
    )
  )
)
by_file <- by_file[order(by_file$kind, by_file$percent), c("kind", "filename", "lines", "run", "percent")]
cat("\n")
print(by_file, row.names = FALSE)

by_kind <- aggregate(cbind(lines, run) ~ kind, by_file, sum)
by_kind$percent <- round(100 * by_kind$run / by_kind$lines)
cat("\nBy kind of file:\n")
print(by_kind, row.names = FALSE)

overall <- covr::percent_coverage(coverage)
cat(sprintf("\nOverall: %.1f%% of lines\n", overall))
if (failed > 0) cat(sprintf("%d test(s) failed or stopped with an error; see above.\n", failed))

# ------ The badge -------------------------------------------------------------------------------
# Drawn like the shields.io badges: "coverage" on grey, the number on a colour that goes from red
# to green as it rises.
coverage_badge <- function(percent, path) {
  value <- sprintf("%d%%", as.integer(floor(percent)))
  colour <- if (percent >= 90) "#4c1" else if (percent >= 75) "#97ca00" else if (percent >= 60) "#a4a61d" else if (percent >= 40) "#dfb317" else if (percent >= 20) "#fe7d37" else "#e05d44"
  label_width <- 61
  value_width <- 7 * nchar(value) + 12
  width <- label_width + value_width
  text <- function(x, words) {
    sprintf(
      '<text x="%s" y="15" fill="#010101" fill-opacity=".3">%s</text><text x="%s" y="14">%s</text>',
      x, words, x, words
    )
  }
  writeLines(c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="20" role="img" aria-label="coverage: %s">', width, value),
    sprintf("<title>coverage: %s</title>", value),
    '<linearGradient id="s" x2="0" y2="100%"><stop offset="0" stop-color="#bbb" stop-opacity=".1"/><stop offset="1" stop-opacity=".1"/></linearGradient>',
    sprintf('<clipPath id="r"><rect width="%d" height="20" rx="3" fill="#fff"/></clipPath>', width),
    sprintf(
      '<g clip-path="url(#r)"><rect width="%d" height="20" fill="#555"/><rect x="%d" width="%d" height="20" fill="%s"/><rect width="%d" height="20" fill="url(#s)"/></g>',
      label_width, label_width, value_width, colour, width
    ),
    '<g fill="#fff" text-anchor="middle" font-family="Verdana,Geneva,DejaVu Sans,sans-serif" font-size="11">',
    text(label_width / 2, "coverage"),
    text(label_width + value_width / 2, value),
    "</g></svg>"
  ), path)
}

badge <- Sys.getenv("COVERAGE_BADGE")
if (nzchar(badge)) coverage_badge(overall, badge)

# On GitHub, the numbers go on the run's summary page too.
summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_file)) {
  rows <- function(table, name) {
    c(
      sprintf("| %s | Lines | Run | %% |", name), "|---|---:|---:|---:|",
      sprintf("| %s | %d | %d | %d |", table[[1]], table$lines, table$run, table$percent)
    )
  }
  cat(
    sprintf("## Coverage: %.1f%% of lines", overall), "",
    if (failed > 0) c(sprintf("%d test(s) failed or stopped with an error.", failed), ""),
    rows(by_kind, "Kind"), "",
    rows(by_file[c("filename", "lines", "run", "percent")], "File"),
    file = summary_file, sep = "\n", append = TRUE
  )
}
