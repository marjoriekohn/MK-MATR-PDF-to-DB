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
# then the slowest tests.
#
# Most of a run's time is its requests to the test database, one after another, so the files run in several
# R processes at once (8 unless TEST_WORKERS says otherwise). Each process shows a file's tests when the file
# has finished, and the summary at the end is of them all. Each has a marker of its own for the rows it makes
# (helper-00_realdb.R), so none touches another's. The files in `serial_files` run on their own afterwards:
# they count what is in the whole database, which the others change while they run. The files are shared out
# by how long each took last time (kept in `timing_file`).
#
# Environment variables change it:
#   TEST_FILTER    only the files whose names match, e.g. Sys.setenv(TEST_FILTER = "home|history")
#   TEST_WORKERS   how many processes run files at once; 1 runs them one after another in this one
#   TEST_REPORTER  "steps" (this one, the default), "progress" (testthat's table of files),
#                  "slow" (testthat's list of the slowest tests) or any other testthat reporter's name;
#                  any but "steps" runs the files one after another
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

# A failure, error or warning as the summary repeats it: a heading saying where, and its message.
problem_text <- function(file, test, result) {
  where <- if (!is.null(result$srcref)) sprintf(" (%s:%d)", file, result$srcref[1]) else sprintf(" (%s)", file)
  list(heading = paste0(toupper(result_kind(result)), ": ", test, where), message = conditionMessage(result))
}

# The end of a run: the problems in full, the slowest tests and files, and the totals.
#
# `times` has a row for each test (`file`, `test`, `seconds`, `requests`), `problems` is a list of
# problem_text()s, and `counts` is c(passed, failed, skipped, warnings). `line` writes a line.
print_summary <- function(times, problems, counts, seconds, line) {
  line("")
  if (length(problems) > 0) {
    line(cli::rule(left = "Problems", line_col = "red"))
    for (problem in problems) {
      line(cli::style_bold(problem$heading))
      line(problem$message)
      line("")
    }
  }
  if (nrow(times) > 0) {
    slowest <- utils::head(times[order(-times$seconds), ], 10)
    line(cli::rule(left = "Slowest tests"))
    for (i in seq_len(nrow(slowest))) {
      line(sprintf("  %5.1f s  %s  %s: %s", slowest$seconds[i], count_of(slowest$requests[i], "request"), slowest$file[i], slowest$test[i]))
    }
    by_file <- sort(tapply(times$seconds, times$file, sum), decreasing = TRUE)
    file_requests <- tapply(times$requests, times$file, sum)
    line("")
    line(cli::rule(left = "Slowest files"))
    for (name in utils::head(names(by_file), 5)) {
      line(sprintf("  %5.1f s  %s  %s", by_file[[name]], count_of(file_requests[[name]], "request"), name))
    }
  }
  line("")
  line(cli::rule(left = sprintf(
    "%d passed, %d failed, %d tests skipped, %d warnings, %d requests, in %.0f s",
    counts[["passed"]], counts[["failed"]], counts[["skipped"]], counts[["warnings"]], sum(times$requests), seconds
  )))
}

# With `results_file`, the reporter is one of several processes' (see "Running them in parallel" below): it
# writes each file's lines when the file has finished, followed by `file_done`, and saves what it found to
# `results_file` for the summary instead of writing one.
file_done <- "@@ file done @@"

StepsReporter <- R6::R6Class("StepsReporter", # nolint: object_name_linter. (testthat's reporters are R6 classes)
  inherit = testthat::Reporter,
  public = list(
    current_file = NULL, # the file now running
    started = NULL, # when the test now running started
    requests_before = 0L, # how many requests had been made when it started (helper-00_realdb.R counts them)
    test_results = NULL, # what the test now running has found: its expectations
    times = NULL, # each test's time, file and name
    problems = NULL, # each failure and error, to repeat at the end (problem_text())
    counts = NULL,
    run_started = NULL,
    results_file = NULL, # where a process that runs some of the files saves what it found
    held = NULL, # that process's lines for the file now running

    initialize = function(..., results_file = NULL) {
      super$initialize(...)
      self$times <- data.frame(file = character(), test = character(), seconds = numeric(), requests = integer())
      self$problems <- list()
      self$counts <- c(passed = 0L, failed = 0L, skipped = 0L, warnings = 0L)
      self$results_file <- results_file
      self$held <- character()
    },
    # A line of the report: written now, or kept until the file has finished.
    say = function(...) {
      if (is.null(self$results_file)) self$cat_line(...) else self$held <- c(self$held, paste0(...))
    },
    start_reporter = function(...) {
      self$run_started <- proc.time()[["elapsed"]]
    },
    start_file = function(filename) {
      self$current_file <- filename
      self$say("")
      self$say(cli::rule(left = basename(filename)))
    },
    end_file = function() {
      if (is.null(self$results_file)) {
        return(invisible())
      }
      self$cat_line(paste(c(self$held, file_done), collapse = "\n"))
      self$held <- character()
    },
    start_test = function(context, test) {
      self$started <- proc.time()[["elapsed"]]
      self$requests_before <- requests_made()
      self$test_results <- list()
      # In a console the test is named while it runs, and its line is finished when it ends; in a log
      # (GitHub's), or when several processes run, only the finished line is written.
      # (cut to the console's width, so the finished line can be written over it)
      if (is.null(self$results_file) && cli::is_dynamic_tty()) {
        self$cat_tight(substr(paste0("  …        ", test), 1, cli::console_width() - 1))
      }
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
      mark <- if (failed > 0) cli::col_red("✖") else if (skipped) cli::col_blue("↷") else if (warned > 0) cli::col_yellow("!") else cli::col_green("✔")
      why <- c(
        if (requests > 0) cli::col_grey(sprintf("%d request%s", requests, if (requests > 1) "s" else "")),
        if (failed > 0) cli::col_red(sprintf("%d failed", failed)),
        if (warned > 0) cli::col_yellow(sprintf("%d warning%s", warned, if (warned > 1) "s" else "")),
        if (skipped) cli::col_blue(paste("skipped:", sub("^Reason: ", "", conditionMessage(self$test_results[[which(kinds == "skip")[1]]]))))
      )
      self$say(
        if (is.null(self$results_file) && cli::is_dynamic_tty()) "\r", "  ", mark, " ",
        formatC(seconds, format = "f", digits = 1, width = 5), " s  ", test,
        if (length(why)) paste0("  (", paste(why, collapse = ", "), ")")
      )
      for (result in self$test_results[kinds %in% c("failure", "error", "warning")]) {
        self$problems[[length(self$problems) + 1]] <- problem_text(basename(self$current_file), test, result)
      }
      self$times[nrow(self$times) + 1, ] <- list(basename(self$current_file), test, seconds, requests)
    },
    end_reporter = function() {
      if (!is.null(self$results_file)) {
        saveRDS(list(times = self$times, problems = self$problems, counts = self$counts), self$results_file)
        return(invisible())
      }
      print_summary(self$times, self$problems, self$counts, proc.time()[["elapsed"]] - self$run_started, self$cat_line)
    }
  )
)

# ------ Running them in parallel ---------------------------------------------------------------------------
# The files that run on their own once the others have finished, by name (without "test-" and ".R"): those that
# count what is in the whole database, and the check that the run left nothing behind, which has to come last.
serial_files <- c("model_history", "service_home", "zz_leftovers")

# How long each file took last time, in seconds, by name: what the files are shared out by.
timing_file <- file.path(tools::R_user_dir("matr_tests", "cache"), "file_seconds.rds")

# A file's name as TEST_FILTER is matched against it, as testthat does: "test-home_server.R" is "home_server".
test_file_name <- function(file) sub("^test[-_]?", "", sub("\\.[rR]$", "", basename(file)))

# Share the files out among `workers` processes so that each has about as long to run: the longest first,
# each to the process with least to do so far.
# names: The files' names (test_file_name()).
# workers: How many processes.
# seconds: How long each file took last time, by name; a file with no time is guessed from its size.
# Returns: A list of character vectors of names, one for each process that has any.
share_out <- function(names, workers, seconds = numeric()) {
  guess <- if (length(seconds) > 0) stats::median(seconds) else NA
  weight <- vapply(names, function(name) {
    if (name %in% names(seconds)) {
      return(seconds[[name]])
    }
    if (!is.na(guess)) guess else file.size(file.path("testthat", paste0("test-", name, ".R")))
  }, numeric(1))
  groups <- replicate(min(workers, length(names)), character(), simplify = FALSE)
  load <- numeric(length(groups))
  for (name in names[order(-weight)]) {
    least <- which.min(load)
    groups[[least]] <- c(groups[[least]], name)
    load[least] <- load[least] + weight[[name]]
  }
  groups
}

# Starts a process that runs the files `names` with this script, in the folder it is in. `marker` names its rows.
start_worker <- function(names, marker) {
  worker <- new.env()
  worker$names <- names
  worker$results_file <- tempfile("test_results_", fileext = ".rds")
  worker$held <- character()
  worker$process <- callr::r_bg(
    function(folder) {
      setwd(folder)
      source("testthat.R", encoding = "UTF-8")
    },
    args = list(folder = getwd()),
    env = c(
      TEST_FILTER = paste0("^(", paste(names, collapse = "|"), ")$"), TEST_WORKERS = "1", TEST_REPORTER = "steps",
      TEST_RESULTS_FILE = worker$results_file, TEST_RUN_MARKER = marker,
      R_CLI_NUM_COLORS = as.character(cli::num_ansi_colors()) # its output isn't a console, but this one is
    ),
    stdout = "|", stderr = "2>&1", supervise = TRUE
  )
  worker
}

# Writes what the processes have written so far, a whole file's lines at a time, until they have all ended.
follow <- function(workers) {
  take <- function(worker, lines) {
    for (line in lines) {
      if (identical(line, file_done)) {
        cat(worker$held, sep = "\n")
        worker$held <- character()
      } else {
        worker$held <- c(worker$held, line)
      }
    }
  }
  repeat {
    running <- FALSE
    for (worker in workers) {
      take(worker, worker$process$read_output_lines())
      running <- running || worker$process$is_alive()
    }
    if (!running) break
    Sys.sleep(0.25)
  }
  for (worker in workers) {
    take(worker, worker$process$read_all_output_lines())
    if (length(worker$held) > 0) cat(worker$held, sep = "\n") # what a process wrote after its last file, or before it failed
  }
}

# Run the files `names` in `workers` processes at once, then those in `serial_files` on their own, and write
# one summary of them all. Stops if a test failed or a process did, so the run is marked as failing.
# names: The files' names (test_file_name()).
# workers: How many processes run at once.
run_in_parallel <- function(names, workers) {
  started <- proc.time()[["elapsed"]]
  seconds <- if (file.exists(timing_file)) tryCatch(readRDS(timing_file), error = function(problem) numeric()) else numeric()
  stages <- c(
    list(share_out(setdiff(names, serial_files), workers, seconds)),
    if (any(names %in% serial_files)) list(list(intersect(serial_files, names)))
  )
  marker <- paste0("ZZTEST", format(Sys.time(), "%d%H%M"), paste(sample(letters, 2), collapse = ""))
  everyone <- list()
  on.exit(for (worker in everyone) if (worker$process$is_alive()) worker$process$kill())
  for (stage in stages) {
    if (length(stage) == 0) next
    before <- length(everyone) # each process's marker ends with a letter of its own
    workers_now <- lapply(seq_along(stage), function(i) start_worker(stage[[i]], paste0(marker, letters[before + i])))
    everyone <- c(everyone, workers_now)
    follow(workers_now)
  }

  times <- data.frame(file = character(), test = character(), seconds = numeric(), requests = integer())
  problems <- list()
  counts <- c(passed = 0L, failed = 0L, skipped = 0L, warnings = 0L)
  for (worker in everyone) {
    if (!file.exists(worker$results_file)) {
      problems[[length(problems) + 1]] <- list(
        heading = paste("ERROR: the process running", paste(worker$names, collapse = ", "), "stopped before it finished"),
        message = sprintf("It ended with status %s; what it wrote is above.", worker$process$get_exit_status())
      )
      counts[["failed"]] <- counts[["failed"]] + 1L
      next
    }
    found <- readRDS(worker$results_file)
    times <- rbind(times, found$times)
    problems <- c(problems, found$problems)
    counts <- counts + found$counts
  }
  print_summary(times, problems, counts, proc.time()[["elapsed"]] - started, function(...) cat(..., "\n", sep = ""))

  # The times are kept only from a run that skipped nothing: one with no test database skips its tests in no time.
  if (nrow(times) > 0 && counts[["skipped"]] == 0) {
    by_file <- tapply(times$seconds, sub("^test[-_]?(.*)\\.[rR]$", "\\1", times$file), sum)
    seconds[names(by_file)] <- as.numeric(by_file)
    tryCatch(
      {
        dir.create(dirname(timing_file), recursive = TRUE, showWarnings = FALSE)
        saveRDS(seconds, timing_file)
      },
      error = function(problem) NULL # only a guide for the next run
    )
  }
  if (counts[["failed"]] > 0) stop("Some tests failed.", call. = FALSE)
  invisible()
}

# ------ Running them -----------------------------------------------------------------------------------
# testthat's progress table stops the run after 10 failures; this lets it carry on to the end.
options(testthat.progress.max_fails = 10000)
# testthat's 3rd edition (its expect_equal() says what differs). A package says so in its DESCRIPTION; this
# site has none, so it is said here, for this process and the ones it starts.
Sys.setenv(TESTTHAT_EDITION = "3")
reporter_name <- Sys.getenv("TEST_REPORTER", "steps")
workers <- suppressWarnings(as.integer(Sys.getenv("TEST_WORKERS", "8")))
filter <- Sys.getenv("TEST_FILTER")
results_file <- Sys.getenv("TEST_RESULTS_FILE")
run_names <- test_file_name(list.files("testthat", pattern = "^test.*\\.[rR]$"))
if (nzchar(filter)) run_names <- run_names[grepl(filter, run_names)]

if (!nzchar(results_file) && identical(reporter_name, "steps") && isTRUE(workers > 1) && length(run_names) > 1) {
  run_in_parallel(run_names, workers)
} else {
  reporter <- if (identical(reporter_name, "steps")) {
    StepsReporter$new(results_file = if (nzchar(results_file)) results_file)
  } else {
    testthat::find_reporter(reporter_name)
  }
  testthat::test_dir(
    "testthat",
    # (the packages global.R loads say so once in each process, which would be noise among the files' lines)
    env = if (nzchar(results_file)) suppressPackageStartupMessages(shiny::loadSupport()) else shiny::loadSupport(),
    filter = if (nzchar(filter)) filter,
    stop_on_failure = !nzchar(results_file),
    # "fail" makes the run itself fail when a test did, so GitHub marks it as failing. A process running some
    # of the files leaves that to the one that started it.
    reporter = if (nzchar(results_file)) reporter else testthat::MultiReporter$new(list(reporter, testthat::FailReporter$new()))
  )
}
