# Runs last (testthat runs the files in alphabetical order). Every test removes the rows it made, so
# nothing the tests name with their marker may still be in the live tables. If something is, a test
# made a row it didn't clean up (or an earlier run crashed): tests/database/clean_test_rows.R removes
# them. What stays on purpose: applied drafts (the database keeps the record of every applied change,
# and refuses to delete it), and the marked staff members those records point at, set inactive.

test_that("the tests left no marked rows in the live tables, and no unapplied drafts", {
  marked <- c(
    registry = "name", principal_investigator = "name", study = "name",
    respondent_type = "name", questionnaire = "name", wave = "name", component = "text",
    category = "term", answer_option = "standardized_label", subject = "name", measurement_scale = "name",
    option_set = "name"
  )
  left <- list()
  for (table in names(marked)) {
    column <- marked[[table]]
    rows <- database_model_get(
      test_connection, table, column, order_by = column,
      filters = stats::setNames(list("like.ZZTEST*"), column)
    )
    if (nrow(rows) > 0) left[[table]] <- rows[[column]]
  }
  drafts <- database_model_select(
    test_connection, "draft", c("draft_id", "status", "summary"),
    filters = list(summary = "like.ZZTEST*", status = paste0("neq.", history_applied_status))
  )
  if (length(drafts) > 0) left$draft <- vapply(drafts, function(row) row$summary, character(1))
  active <- database_model_get(
    test_connection, "staff", "name", order_by = "name",
    filters = list(name = "like.ZZTEST*", active = "is.true")
  )
  if (nrow(active) > 0) left$staff <- active$name
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
