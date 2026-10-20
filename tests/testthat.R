# Unit tests for the functions in R/.
# Run them from the project folder with `shiny::runTests()`.
#
# Each test is named as it starts and given its time as it ends, so a slow one can be seen while it runs:
#
#   ── test-home_server.R ───────────────────────────────────────────────────
#     ✔  0.8 s  each button on the home page opens the page it names
#     ✖  2.1 s  the two boxes list the newest requests and changes  (1 failed)
#     ↷  0.0 s  a request this person sent says so  (skipped: there is no test database ...)
#
# (a test that asks the database says how many requests it made), and at the end come the problems in full,
# then the slowest tests. Two environment variables change it:
#   TEST_FILTER    only the files whose names match, e.g. Sys.setenv(TEST_FILTER = "home|history")
#   TEST_REPORTER  "steps" (this one, the default), "progress" (testthat's table of files),
#                  "slow" (testthat's list of the slowest tests) or any other testthat reporter's name
# e.g. Sys.setenv(TEST_FILTER = "edit_extraction"); shiny::runTests(); Sys.unsetenv("TEST_FILTER")

# ------ The "steps" reporter ------------------------------------------------------------------------------
# How many requests the tests have made so far (counted by helper-00_realdb.R; 0 before it is loaded).
requests_made <- function() {
  counter <- getOption("matr.test_requests")
  if (is.null(counter)) 0L else counter$count
}

# "1 request", "12 requests", lined up.
count_of <- function(n, word) formatC(paste(n, if (n == 1) word else paste0(word, "s")), width = -14)

# What an expectation's result was: "success", "failure", "error", "skip" or "warning".
result_kind <- function(result) {
  kinds <- c("failure", "error", "skip", "warning", "success")
  found <- kinds[inherits(result, paste0("expectation_", kinds), which = TRUE) > 0]
  if (length(found) > 0) found[1] else "success"
}

StepsReporter <- R6::R6Class("StepsReporter", # nolint: object_name_linter. (testthat's reporters are R6 classes)
  inherit = testthat::Reporter,
  public = list(
    current_file = NULL, # the file now running
    started = NULL, # when the test now running started
    requests_before = 0L, # how many requests had been made when it started (helper-00_realdb.R counts them)
    test_results = NULL, # what the test now running has found: its expectations
    times = NULL, # each test's time, file and name
    problems = NULL, # each failure and error, to repeat at the end
    counts = NULL,
    run_started = NULL,

    initialize = function(...) {
      super$initialize(...)
      self$times <- data.frame(file = character(), test = character(), seconds = numeric(), requests = integer())
      self$problems <- list()
      self$counts <- c(passed = 0L, failed = 0L, skipped = 0L, warnings = 0L)
    },
    start_reporter = function(...) {
      self$run_started <- proc.time()[["elapsed"]]
    },
    start_file = function(filename) {
      self$current_file <- filename
      self$cat_line("")
      self$cat_line(cli::rule(left = basename(filename)))
    },
    start_test = function(context, test) {
      self$started <- proc.time()[["elapsed"]]
      self$requests_before <- requests_made()
      self$test_results <- list()
      # In a console the test is named while it runs, and its line is finished when it ends; in a log
      # (GitHub's) only the finished line is written.
      # (cut to the console's width, so the finished line can be written over it)
      if (cli::is_dynamic_tty()) self$cat_tight(substr(paste0("  \u2026        ", test), 1, cli::console_width() - 1))
    },
    add_result = function(context, test, result) {
      self$test_results[[length(self$test_results) + 1]] <- result
    },
    end_test = function(context, test) {
      seconds <- proc.time()[["elapsed"]] - self$started
      requests <- requests_made() - self$requests_before
      kinds <- vapply(self$test_results, result_kind, character(1))
      failed <- sum(kinds %in% c("failure", "error"))
      skipped <- any(kinds == "skip")
      warned <- sum(kinds == "warning")
      self$counts[["passed"]] <- self$counts[["passed"]] + sum(kinds == "success")
      self$counts[["failed"]] <- self$counts[["failed"]] + failed
      self$counts[["warnings"]] <- self$counts[["warnings"]] + warned
      if (skipped) self$counts[["skipped"]] <- self$counts[["skipped"]] + 1L
      mark <- if (failed > 0) cli::col_red("\u2716") else if (skipped) cli::col_blue("\u21b7") else if (warned > 0) cli::col_yellow("!") else cli::col_green("\u2714")
      why <- c(
        if (requests > 0) cli::col_grey(sprintf("%d request%s", requests, if (requests > 1) "s" else "")),
        if (failed > 0) cli::col_red(sprintf("%d failed", failed)),
        if (warned > 0) cli::col_yellow(sprintf("%d warning%s", warned, if (warned > 1) "s" else "")),
        if (skipped) cli::col_blue(paste("skipped:", sub("^Reason: ", "", conditionMessage(self$test_results[[which(kinds == "skip")[1]]]))))
      )
      self$cat_line(
        if (cli::is_dynamic_tty()) "\r", "  ", mark, " ", formatC(seconds, format = "f", digits = 1, width = 5), " s  ", test,
        if (length(why)) paste0("  (", paste(why, collapse = ", "), ")")
      )
      for (result in self$test_results[kinds %in% c("failure", "error", "warning")]) {
        self$problems[[length(self$problems) + 1]] <- list(file = basename(self$current_file), test = test, result = result)
      }
      self$times[nrow(self$times) + 1, ] <- list(basename(self$current_file), test, seconds, requests)
    },
    end_reporter = function() {
      total <- proc.time()[["elapsed"]] - self$run_started
      self$cat_line("")
      if (length(self$problems) > 0) {
        self$cat_line(cli::rule(left = "Problems", line_col = "red"))
        for (problem in self$problems) {
          result <- problem$result
          where <- if (!is.null(result$srcref)) sprintf(" (%s:%d)", problem$file, result$srcref[1]) else sprintf(" (%s)", problem$file)
          self$cat_line(cli::style_bold(paste0(toupper(result_kind(result)), ": ", problem$test, where)))
          self$cat_line(conditionMessage(result))
          self$cat_line("")
        }
      }
      if (nrow(self$times) > 0) {
        slowest <- utils::head(self$times[order(-self$times$seconds), ], 10)
        self$cat_line(cli::rule(left = "Slowest tests"))
        for (i in seq_len(nrow(slowest))) {
          self$cat_line(sprintf("  %5.1f s  %s  %s: %s", slowest$seconds[i], count_of(slowest$requests[i], "request"), slowest$file[i], slowest$test[i]))
        }
        by_file <- sort(tapply(self$times$seconds, self$times$file, sum), decreasing = TRUE)
        file_requests <- tapply(self$times$requests, self$times$file, sum)
        self$cat_line("")
        self$cat_line(cli::rule(left = "Slowest files"))
        for (name in utils::head(names(by_file), 5)) {
          self$cat_line(sprintf("  %5.1f s  %s  %s", by_file[[name]], count_of(file_requests[[name]], "request"), name))
        }
      }
      self$cat_line("")
      self$cat_line(cli::rule(left = sprintf(
        "%d passed, %d failed, %d tests skipped, %d warnings, %d requests, in %.0f s",
        self$counts[["passed"]], self$counts[["failed"]], self$counts[["skipped"]], self$counts[["warnings"]],
        sum(self$times$requests), total
      )))
    }
  )
)

# ------ Running them -----------------------------------------------------------------------------------
# testthat's progress table stops the run after 10 failures; this lets it carry on to the end.
options(testthat.progress.max_fails = 10000)
reporter <- Sys.getenv("TEST_REPORTER", "steps")
reporter <- if (identical(reporter, "steps")) StepsReporter$new() else testthat::find_reporter(reporter)
filter <- Sys.getenv("TEST_FILTER")
testthat::test_dir(
  "testthat",
  env = shiny::loadSupport(),
  filter = if (nzchar(filter)) filter,
  # "fail" makes the run itself fail when a test did, so GitHub marks it as failing
  reporter = testthat::MultiReporter$new(list(reporter, testthat::FailReporter$new()))
)
