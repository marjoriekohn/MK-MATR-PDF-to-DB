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
  local_stub("test_clear_drafts", function(staff_id) log$cleared <- c(log$cleared, staff_id), env = env)
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

test_that("the file's next test is given the same staff, in the same order, with what pointed at them cleared", {
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

test_that("a staff member is made for the test, and removed with the drafts and history that point at them", {
  # Everything the helpers make is removed when the function they were called in ends; this one ends by hand.
  made <- (function() {
    scope <- environment()
    me <- local_staff(env = scope)
    admin <- local_staff("ADMIN", env = scope)
    tree <- local_tree(env = scope)
    record <- local_extraction(me, env = scope)
    kind <- edit_entities$respondent_type
    name <- test_name("Respondent Type")
    snap <- edit_service_load(test_connection, kind)
    fields <- edit_service_fields(kind, snap, "add")
    values <- utils::modifyList(edit_service_initial(kind, fields, snap, "add"), list(respondent_type__name = name))
    # an edit applied by the ADMIN is in the history, and what it added is in the tables
    added <- edit_service_quick_add(test_connection, kind, snap, fields, values, admin)
    local_applied(added$history_id, env = scope)
    present <- nrow(database_model_get(test_connection, "respondent_type", "name", filters = list(name = database_eq(name)))) == 1
    withr::deferred_run(scope)
    list(me = me, admin = admin, tree = tree, record = record, added = added, name = name, present = present)
  })()
  expect_match(made$me$name, test_marker)
  expect_equal(made$me$role, "STAFF")
  expect_equal(made$admin$role, "ADMIN")
  expect_match(made$record$id, "^[0-9]+$")
  expect_match(made$added$key, "^[0-9]+$")
  expect_true(made$present)

  gone <- function(table, column, value) {
    nrow(database_model_get(test_connection, table, column, filters = stats::setNames(list(database_eq(value)), column))) == 0
  }
  expect_true(gone("staff", "staff_id", made$me$staff_id))
  expect_true(gone("draft", "draft_id", made$record$id))
  expect_true(gone("respondent_type", "name", made$name))
  # the applied draft is the record of what was done: the database keeps it, and so the ADMIN who prepared it
  # stays too, set inactive
  kept <- database_model_get(test_connection, "staff", c("staff_id", "active"), filters = list(staff_id = database_eq(made$admin$staff_id)))
  expect_equal(nrow(kept), 1)
  expect_false(kept$active)
  expect_false(gone("draft", "draft_id", made$added$history_id))
  expect_equal(nrow(history_model_get_items(test_connection, as.character(made$added$history_id))), 1)
  expect_true(gone("study", "study_id", made$tree$study$study_id))
  expect_true(gone("registry", "registry_id", made$tree$registry$registry_id))
})
