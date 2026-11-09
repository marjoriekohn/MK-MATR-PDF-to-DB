test_that("the tests never reach the real database: its address and keys are swapped for the test project's", {
  withr::defer(use_test_database()) # last, once the variables below are put back
  withr::local_envvar(
    SUPABASE_URL = "https://realproject.supabase.co", SUPABASE_SECRET_KEY = "sb_secret_real",
    SUPABASE_PUBLISHABLE_KEY = "sb_publishable_real",
    TEST_SUPABASE_URL = "https://testproject.supabase.co", TEST_SUPABASE_SECRET_KEY = "sb_secret_test",
    TEST_SUPABASE_PUBLISHABLE_KEY = "sb_publishable_test"
  )
  had_database <- exists("database", envir = globalenv(), inherits = FALSE)
  use_test_database()
  expect_equal(Sys.getenv("SUPABASE_URL"), "https://testproject.supabase.co")
  expect_equal(Sys.getenv("SUPABASE_SECRET_KEY"), "sb_secret_test")
  expect_equal(Sys.getenv("SUPABASE_PUBLISHABLE_KEY"), "sb_publishable_test")
  if (had_database) expect_equal(get("database", envir = globalenv()), list(url = "https://testproject.supabase.co", key = "sb_secret_test"))

  # with no test project in .env, there is no database at all, not the real one
  withr::local_envvar(TEST_SUPABASE_URL = NA, TEST_SUPABASE_SECRET_KEY = NA, TEST_SUPABASE_PUBLISHABLE_KEY = NA)
  use_test_database()
  expect_equal(Sys.getenv(c("SUPABASE_URL", "SUPABASE_SECRET_KEY", "SUPABASE_PUBLISHABLE_KEY"), names = FALSE), c("", "", ""))
})

test_that("while the tests run, the site's address isn't the real database's", {
  expect_false(grepl("nvjyiaehomrysoprfuyl", Sys.getenv("SUPABASE_URL"), fixed = TRUE))
  if (exists("database", envir = globalenv(), inherits = FALSE)) {
    expect_false(grepl("nvjyiaehomrysoprfuyl", get("database", envir = globalenv())$url, fixed = TRUE))
  }
})

test_that("with no test project in .env, a test that needs the database is skipped, and says why", {
  withr::local_envvar(TEST_SUPABASE_URL = NA, TEST_SUPABASE_SECRET_KEY = NA, CI = NA)
  expect_condition(test_connection, "there is no test database", class = "skip")
})

# ------ The staff members a file shares (stand-ins for the database) -------------------------------------------
shared_staff_stubs <- function(log, env = parent.frame()) {
  local_stub("new_staff", function(role, active, name, env) {
    log$made <- log$made + 1L
    list(staff_id = 9000L + log$made, name = name %||% paste("Stand-in", role, log$made), role = role, email = "x@example.test")
  }, env = env)
  local_stub("test_forget_work", function(staff_id) log$cleared <- c(log$cleared, staff_id), env = env)
}
pool_log <- new.env()
pool_log$made <- 0L
pool_log$cleared <- integer()

test_that("a test file's first test makes the staff it asks for, one for each time it asks", {
  shared_staff_stubs(pool_log)
  first <- local_staff()
  second <- local_staff()
  admin <- local_staff("ADMIN")
  expect_equal(c(first$staff_id, second$staff_id, admin$staff_id), 9001:9003)
  expect_equal(admin$role, "ADMIN")
  # a named or inactive one, or one for something that isn't a test, is the test's own
  expect_equal(local_staff(name = "Named")$staff_id, 9004L)
  expect_equal(local_staff(active = FALSE)$staff_id, 9005L)
  expect_equal((function() local_staff(env = environment()))()$staff_id, 9006L)
  expect_equal(pool_log$made, 6L)
})

test_that("the file's next test is given the same staff, in the same order, with what they did forgotten", {
  withr::defer(rm(list = ".matr_shared_staff", envir = parent.env(environment()))) # the stand-ins go with this test
  expect_setequal(pool_log$cleared, 9001:9003) # when the last test ended
  shared_staff_stubs(pool_log)
  expect_equal(local_staff()$staff_id, 9001L)
  expect_equal(local_staff("ADMIN")$staff_id, 9003L)
  expect_equal(local_staff()$staff_id, 9002L)
  expect_equal(local_staff()$staff_id, 9007L) # a third STAFF member: made now
  expect_equal(pool_log$made, 7L)
})

# The helpers every test of the database relies on: the check that the database is the test project (or
# one on this computer), and the rows they make and remove.

test_that("only the test project, or a database on this computer, is allowed, and never the real one", {
  expect_true(is_test_database(paste0("https://", test_project_ref, ".supabase.co")))
  expect_true(is_test_database(paste0("https://", toupper(test_project_ref), ".supabase.co/")))
  expect_true(is_test_database("http://127.0.0.1:3000"))
  expect_true(is_test_database("http://localhost:54321/"))
  expect_false(is_test_database(paste0("https://", real_project_ref, ".supabase.co")))
  expect_false(is_test_database("https://someotherprojectref.supabase.co"))
  expect_false(is_test_database("http://localhost.example.com")) # a name that only starts like localhost
  expect_false(is_test_database(paste0("https://", test_project_ref, ".supabase.co.example.com")))
  expect_false(is_test_database(""))
})

test_that("a test refuses to run against another project, and says why", {
  withr::local_envvar(CI = NA)
  withr::local_envvar(TEST_SUPABASE_URL = "https://someotherprojectref.supabase.co", TEST_SUPABASE_SECRET_KEY = "sb_secret_x")
  expect_error(test_database(), "Refusing to run: TEST_SUPABASE_URL is https://someotherprojectref.supabase.co")
  expect_error(test_connection, "Refusing to run")
  withr::local_envvar(TEST_SUPABASE_URL = paste0("https://", real_project_ref, ".supabase.co"))
  expect_error(test_database(), "Refusing to run")
  withr::local_envvar(TEST_SUPABASE_URL = "", TEST_SUPABASE_SECRET_KEY = "")
  expect_error(test_database(), "run against the test Supabase project")
  withr::local_envvar(CI = "true") # on GitHub a missing test project fails the run
  expect_error(test_connection, "run against the test Supabase project")
  withr::local_envvar(TEST_SUPABASE_URL = "http://127.0.0.1:3000", TEST_SUPABASE_SECRET_KEY = "sb_secret_x")
  expect_equal(test_database(), list(url = "http://127.0.0.1:3000", key = "sb_secret_x"))
})

test_that("every name a test makes has the run's marker and is different each time", {
  a <- test_name("Study")
  b <- test_name("Study")
  expect_match(a, paste0("^", test_marker, " Study "))
  expect_false(identical(a, b))
})

# Whether no row of `table` has `value` in `column`.
gone <- function(table, column, value) {
  nrow(database_model_get(test_connection, table, column, filters = stats::setNames(list(database_eq(value)), column))) == 0
}

test_that("a staff member is made for the test, and removed with the extractions and history that name them", {
  # Everything the helpers make is removed when the function they were called in ends; this one ends by hand.
  made <- (function() {
    scope <- environment()
    me <- local_staff(env = scope)
    admin <- local_staff("ADMIN", env = scope)
    tree <- local_tree(env = scope)
    # an extraction aimed at the tree's wave, and a history row, as the site writes them
    extraction <- database_model_insert(test_connection, "extraction", list(
      method = "Manual", prepared_by = me$staff_id, saved_by = me$staff_id, summary = test_name("Extraction"),
      info = list(wave_id = as.character(tree$wave$wave_id))
    ), select = "extraction_id")
    history <- database_model_insert(test_connection, "history", list(
      staff_id = admin$staff_id, action = "Added", entity = "respondent_type",
      record_id = as.character(tree$respondent_type$respondent_type_id), summary = test_name("Added a respondent type")
    ), select = "history_id")
    withr::deferred_run(scope)
    list(me = me, admin = admin, tree = tree, extraction = extraction, history = history)
  })()
  expect_match(made$me$name, test_marker)
  expect_equal(made$me$role, "STAFF")
  expect_equal(made$admin$role, "ADMIN")
  expect_match(as.character(made$extraction$extraction_id), "^[0-9]+$")

  # the database keeps staff and the history for good, but the test project's own functions remove them
  expect_true(gone("staff", "staff_id", made$me$staff_id))
  expect_true(gone("staff", "staff_id", made$admin$staff_id))
  expect_true(gone("extraction", "extraction_id", made$extraction$extraction_id))
  expect_true(gone("history", "history_id", made$history$history_id))
  expect_true(gone("wave", "wave_id", made$tree$wave$wave_id))
  expect_true(gone("study", "study_id", made$tree$study$study_id))
  expect_true(gone("registry", "registry_id", made$tree$registry$registry_id))
  # an extraction that has gone already is no error to remove again
  expect_no_error(test_delete_extraction(made$extraction$extraction_id))
})

test_that("the vocabulary a push adds, and extractions, are removed by their marker", {
  # what a push would add, made here by hand under a marker of this test's own, so nothing else of the run's is touched
  marker <- paste(test_marker, "vocabulary")
  me <- local_staff()
  tag <- database_model_insert(test_connection, "tag", list(kind = "unit", name = paste(marker, "unit", test_counter())))
  option <- database_model_insert(test_connection, "answer_option", list(standardized_label = tolower(paste(marker, "choice", test_counter()))))
  component <- database_model_insert(test_connection, "component", list(kind = "Note", text = tolower(paste(marker, "note", test_counter()))))
  extraction <- database_model_insert(test_connection, "extraction", list(
    method = "Manual", prepared_by = me$staff_id, saved_by = me$staff_id, summary = paste(marker, "extraction", test_counter())
  ), select = "extraction_id")
  # (should the removal fail, they are removed the plain way)
  withr::defer({
    test_delete("tag", list(tag_id = database_eq(tag$tag_id)))
    test_delete("answer_option", list(answer_option_id = database_eq(option$answer_option_id)))
    test_delete("component", list(component_id = database_eq(component$component_id)))
    test_delete_extraction(extraction$extraction_id)
  })

  expect_error(test_forget_marked("not a marker"), "ZZTEST")
  removed <- test_forget_marked(marker)
  expect_equal(unlist(removed[c("tags", "answer_options", "components", "extractions")]), c(tags = 1, answer_options = 1, components = 1, extractions = 1))
  expect_true(gone("tag", "tag_id", tag$tag_id))
  expect_true(gone("answer_option", "answer_option_id", option$answer_option_id))
  expect_true(gone("component", "component_id", component$component_id))
  expect_true(gone("extraction", "extraction_id", extraction$extraction_id))
})
