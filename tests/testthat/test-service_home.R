# The Home page's counts: this person's drafts in progress, and the changes added.

test_that("your work in progress counts the extraction drafts you started and haven't pushed, not a colleague's or a pushed one", {
  me <- list(staff_id = 7, name = "Me", role = "STAFF")
  record <- function(id, status, by = 7) list(id = id, status = status, saved = "2026-10-01T10:00:00Z", prepared_by_id = by)
  extractions <- list(
    e1 = record("e1", "Draft"),
    e2 = record("e2", "Draft"),
    e3 = record("e3", "Draft", by = 8), # a colleague's
    e4 = record("e4", "Pushed") # in the database already
  )
  expect_equal(home_service_my_work(extractions, me), 2L)
  expect_equal(home_service_my_work(list(), me), 0L)
})

test_that("changes added counts the Edit History", {
  me <- local_staff()
  local_history(me, "Added")
  # not compared with a count from before: other runs of the tests add and remove changes meanwhile, but this one
  # stays until the test ends
  expect_gte(home_service_changes_added(test_connection), 1)
  expect_equal(home_service_changes_added(test_connection), history_service_count(test_connection))
})
