test_that("the prompt is read from its file", {
  path <- withr::local_tempfile(fileext = ".md")
  writeLines(c("Read the form.", "Then answer."), path)
  expect_equal(reading_model_prompt(path), "Read the form.\nThen answer.")
  expect_match(reading_model_prompt(app_file("prompts", "extraction.md")), "Reading a questionnaire")
})

test_that("a process id is alive only while its process runs", {
  expect_true(reading_model_pid_alive(Sys.getpid()))
  expect_false(reading_model_pid_alive(NULL))
  expect_false(reading_model_pid_alive(999999L))
  finished <- callr::r_bg(function() Sys.getpid())
  finished$wait()
  expect_false(reading_model_pid_alive(finished$get_result()))
})

test_that("a job runs in its own R process that loads the site's code and logs what it says", {
  log <- withr::local_tempfile(fileext = ".log")
  # A connection with no address: the job gets as far as asking the database
  # (so it has loaded R/ and started reading_service_run) and says why it can't.
  process <- withr::with_dir(app_file(), reading_model_start(list(url = "", key = ""), "1", "2026-10-02T10:00:00Z", log))
  expect_true(is.numeric(reading_model_pid(process)))
  process$wait(60000)
  expect_false(reading_model_is_running(process))
  expect_match(paste(readLines(log), collapse = "\n"), "doesn't know where the MATR database is")
})

test_that("a job that is still running can be stopped", {
  process <- callr::r_bg(function() Sys.sleep(60))
  expect_true(reading_model_is_running(process))
  reading_model_kill(process)
  process$wait(5000)
  expect_false(reading_model_is_running(process))
})
