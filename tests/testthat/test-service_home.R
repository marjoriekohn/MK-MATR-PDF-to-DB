# The Home page's lists: this person's work in progress, the requests waiting for them, and the newest changes.

test_that("your work in progress is your open drafts of both kinds, those sent back first, then the newest saved", {
  me <- list(staff_id = 7, name = "Me", role = "STAFF")
  record <- function(id, status, saved, by = 7) list(id = id, status = status, saved = saved, prepared_by_id = by)
  extractions <- list(
    e1 = record("e1", "Draft", "2026-10-01T10:00:00Z"),
    e2 = record("e2", "Sent Back", "2026-09-01T10:00:00Z"),
    e3 = record("e3", "Draft", "2026-10-05T10:00:00Z", by = 8), # a colleague's
    e4 = record("e4", "Awaiting Review", "2026-10-06T10:00:00Z") # sent for approval
  )
  edits <- list(
    d1 = record("d1", "Draft", "2026-10-03T10:00:00Z"),
    d2 = record("d2", "Awaiting Review", "2026-10-04T10:00:00Z")
  )
  work <- home_service_my_work(extractions, edits, me)
  expect_equal(vapply(work, function(item) item$record$id, ""), c("e2", "d1", "e1"))
  expect_equal(vapply(work, function(item) item$kind, ""), c("extraction", "edit", "extraction"))
  expect_equal(home_service_my_work(list(), list(), me), list())
})

test_that("waiting for you is the Change Requests page's For me, a few at most, with how many there are", {
  me <- local_staff()
  colleague <- local_staff()
  for (i in 1:5) local_edit(colleague, status = "Awaiting Review")
  everything <- home_service_waiting_for_me(test_connection, me, n = 1000)
  expect_equal(everything$total, length(change_request_service_filter(change_request_service_waiting(test_connection), me, "me")))
  expect_gte(everything$total, 5)
  few <- home_service_waiting_for_me(test_connection, me)
  expect_length(few$shown, home_recent_n)
  expect_equal(few$total, everything$total)
})

test_that("the newest applied changes come first, with how many there are", {
  admin <- local_staff("ADMIN")
  first <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("First"))
  second <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("Second"))
  everything <- home_service_recent_history(test_connection, n = 1000)
  ids <- vapply(everything$shown, function(record) record$id, character(1))
  expect_lt(match(as.character(second), ids), match(as.character(first), ids))
  expect_equal(everything$total, history_model_count(test_connection))
  expect_lte(length(home_service_recent_history(test_connection)$shown), home_recent_n)
})

test_that("the button under a list says how many there are in all", {
  expect_equal(home_service_more_label(12, 4), "See all 12")
  expect_equal(home_service_more_label(4, 4), "See more")
  expect_equal(home_service_more_label(0, 0), "See more")
})
