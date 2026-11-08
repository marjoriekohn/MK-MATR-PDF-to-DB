# The Home page's counts: this person's work in progress, the requests waiting for them, and the changes added.

test_that("your work in progress counts your open drafts of both kinds, sent back ones too, not a colleague's or one sent for approval", {
  me <- list(staff_id = 7, name = "Me", role = "STAFF")
  record <- function(id, status, by = 7) list(id = id, status = status, saved = "2026-10-01T10:00:00Z", prepared_by_id = by)
  extractions <- list(
    e1 = record("e1", "Draft"),
    e2 = record("e2", "Sent Back"),
    e3 = record("e3", "Draft", by = 8), # a colleague's
    e4 = record("e4", "Awaiting Review") # sent for approval
  )
  edits <- list(d1 = record("d1", "Draft"), d2 = record("d2", "Awaiting Review"), d3 = record("d3", "Sent Back"))
  expect_equal(home_service_my_work(extractions, edits, me), 4L)
  expect_equal(home_service_my_work(list(), list(), me), 0L)
})

test_that("waiting for you counts the Change Requests page's Waiting for me", {
  me <- local_staff()
  colleague <- local_staff()
  for (i in 1:2) local_edit(colleague, status = "Awaiting Review")
  # one read of what is waiting, for both: other runs of the tests send and remove requests meanwhile
  waiting <- change_request_service_waiting(test_connection)
  local_stub("change_request_service_waiting", function(connection) waiting)
  counted <- home_service_waiting_for_me(test_connection, me)
  expect_equal(counted, length(change_request_service_filter(waiting, me, "me")))
  expect_gte(counted, 2)
})

test_that("changes added counts the Edit History", {
  admin <- local_staff("ADMIN")
  local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("Counted"))
  # not compared with a count from before: other runs of the tests add and remove applied changes meanwhile,
  # but this one stays until the test ends
  expect_gte(home_service_changes_added(test_connection), 1)
})
