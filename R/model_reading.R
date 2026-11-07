# ------ The reading job's machinery ---------------------------------------------------
# What the reading job needs from outside R: the prompt Gemini is given, kept as
# a file in the project, and the background R process (callr) that reads one
# draft while staff carry on. When to start and stop a job, and what it does, are
# decided in service_reading.R.

# The prompt Gemini is given.
# path: The prompt's file.
# Returns: Its text, with lines joined by newlines.
reading_model_prompt <- function(path) {
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

# Start a background R process that reads one draft.
# It loads the code in `R/`, then runs `reading_service_run()`. What it prints goes to
# `log_path`, and it outlives the session that started it (and the site).
# It doesn't run `global.R`, so it doesn't read `.env`. The database is given to it (`connection`), but Gemini's
# key it reads from its environment (`GEMINI_KEY`, in `gemini_model_generate()`): callr starts it with this
# process's environment variables, adding its own (`env`, `rcmd_safe_env()` by default) on top, so it has what
# `global.R`'s `readRenviron()` set, or on Connect the content's variables. If a callr ever started it without
# them, reading would stop with "There's no Gemini API key".
# log_path: Where the process's output goes.
# Returns: A callr process.
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

# A process's id.
# Returns: The id.
reading_model_pid <- function(process) process$get_pid()

# Whether a process is still running.
reading_model_is_running <- function(process) process$is_alive()

# Stop a process.
reading_model_kill <- function(process) process$kill()

# Whether a process with this id is running on this machine.
# Only this machine's processes are looked at, so it says whether a reading job is alive only on the machine
# that started it: the site runs on one (see the top of `service_reading.R`). Any process with the id counts,
# including one that took the id over after the job's process ended.
# pid: A process id, or `NULL`.
# Returns: `TRUE` or `FALSE`; `FALSE` for `NULL`.
reading_model_pid_alive <- function(pid) {
  if (is.null(pid)) {
    return(FALSE)
  }
  tryCatch(ps::ps_is_running(ps::ps_handle(as.integer(pid))), error = function(problem) FALSE)
}
