# How much of the site's code the unit tests run. From the project folder:
#   Rscript tests/tools/coverage.R
# It needs the `covr` package, which the site itself doesn't (so it isn't in renv.lock). It is in
# tests/tools/ and not tests/ because shiny::runTests() runs every script directly in tests/.
#
# covr measures a package; this is an app, so the files are given to it directly: global.R
# and everything in R/ are the code, and the helpers and tests in tests/testthat/ are what
# runs it. The numbers are for lines of code that were run at least once. They say nothing
# about whether the tests check the right things; they show what no test has run at all.
# (server.R and ui.R aren't in the count: they are evaluated by the tests, not sourced.)

suppressMessages({
  library(shiny)
  library(covr)
  library(testthat)
})

# paths are from the project folder
code <- c("global.R", sort(list.files("R", pattern = "\\.R$", full.names = TRUE)))
tests <- c(
  sort(list.files("tests/testthat", pattern = "^helper-.*\\.R$", full.names = TRUE)),
  sort(list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE))
)
coverage <- covr::file_coverage(code, tests)

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
print(by_file, row.names = FALSE)

by_kind <- aggregate(cbind(lines, run) ~ kind, by_file, sum)
by_kind$percent <- round(100 * by_kind$run / by_kind$lines)
cat("\nBy kind of file:\n")
print(by_kind, row.names = FALSE)
cat(sprintf("\nOverall: %.1f%% of lines\n", covr::percent_coverage(coverage)))
