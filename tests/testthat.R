# Unit tests for the functions in R/.
# Run them from the project folder with `shiny::runTests()`.
testthat::test_dir(
  "testthat",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
