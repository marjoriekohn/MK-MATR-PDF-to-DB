# ------ The reading job's machinery ---------------------------------------------------
# What the reading job needs from outside R: the prompt Gemini is given, kept as
# a file in the project, and the background R process (callr) that reads one
# draft while staff carry on. When to start and stop a job, and what it does, are
# decided in service_reading.R.

#' The prompt Gemini is given.
#'
#' @param path The prompt's file.
#' @return Its text, with lines joined by newlines.
reading_model_prompt <- function(path) {
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

#' Start a background R process that reads one draft.
#'
#' It loads the code in `R/`, then runs `reading_service_run()`. What it prints goes to
#' `log_path`, and it outlives the session that started it (and the site).
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param reading_started The reading's start time, as the draft records it.
#' @param log_path Where the process's output goes.
#' @return A callr process.
reading_model_start <- function(connection, id, reading_started, log_path) {
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  callr::r_bg(
    function(connection, id, reading_started) {
      for (path in list.files("R", "\\.[Rr]$", full.names = TRUE)) source(path)
      reading_service_run(connection, id, reading_started)
    },
    args = list(connection, id, reading_started),
    stdout = log_path, stderr = "2>&1",
    cleanup = FALSE
  )
}

#' A process's id.
#'
#' @param process A callr process, from `reading_model_start()`.
#' @return The id.
reading_model_pid <- function(process) process$get_pid()

#' Whether a process is still running.
#'
#' @param process A callr process, from `reading_model_start()`.
#' @return `TRUE` or `FALSE`.
reading_model_is_running <- function(process) process$is_alive()

#' Stop a process.
#'
#' @param process A callr process, from `reading_model_start()`.
#' @return Called for its effect.
reading_model_kill <- function(process) process$kill()

#' Whether a process with this id is running on this machine.
#'
#' @param pid A process id, or `NULL`.
#' @return `TRUE` or `FALSE`; `FALSE` for `NULL`.
reading_model_pid_alive <- function(pid) {
  if (is.null(pid)) {
    return(FALSE)
  }
  tryCatch(ps::ps_is_running(ps::ps_handle(as.integer(pid))), error = function(problem) FALSE)
}
