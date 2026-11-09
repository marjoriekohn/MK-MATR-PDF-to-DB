# Runs last (testthat runs the files in alphabetical order). Every test removes the rows it made, so
# nothing the tests name with their marker may still be in the database: not in the catalog, and no
# extractions, history or staff either (the test project's own functions remove staff, history and the
# vocabulary a push adds, which the database otherwise keeps; tests/database/test_functions.sql). If
# something is, a test made a row it didn't clean up (or an earlier run crashed): tests/database/clean_test_rows.R
# removes them.

test_that("the tests left no marked rows: not in the catalog, and no extractions, history or staff", {
  marked <- c(
    staff = "name", registry = "name", principal_investigator = "name", study = "name",
    respondent_type = "name", questionnaire = "name", wave = "name", component = "text",
    tag = "name", answer_option = "standardized_label", measurement_scale = "name",
    extraction = "summary", history = "summary"
  )
  left <- list()
  for (table in names(marked)) {
    column <- marked[[table]]
    rows <- database_model_get(
      test_connection, table, column, order_by = column,
      filters = stats::setNames(list("ilike.ZZTEST*"), column)
    )
    if (nrow(rows) > 0) left[[table]] <- rows[[column]]
  }
  expect_equal(left, list(), info = "marked rows were left in the database; run tests/database/clean_test_rows.R")
})

test_that("after every test, the site's address and keys are still the test project's, not the real database's", {
  # a test that reads .env (sourcing global.R or a script in tests/database/) has to put them back:
  # use_test_database() in helper-00_realdb.R
  expect_equal(
    Sys.getenv(names(test_env_names), names = FALSE),
    Sys.getenv(unname(test_env_names), names = FALSE)
  )
  expect_false(grepl(real_project_ref, Sys.getenv("SUPABASE_URL"), fixed = TRUE))
})
