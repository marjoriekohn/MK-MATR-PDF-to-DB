# The Home page's lists: the newest requests and changes.

test_that("the newest change requests come first, a few at most, with how many are waiting", {
  me <- local_staff()
  times <- as.POSIXct("2026-10-01 10:00:00", tz = "UTC") + (1:5) * 3600
  sent <- character()
  for (when in as.list(times)) sent <- c(sent, local_edit(me, status = "Awaiting Review", now = when)$id)
  everything <- home_service_recent_requests(test_connection, n = 1000)
  ours <- everything$shown[vapply(everything$shown, function(record) record$id %in% sent, logical(1))]
  expect_equal(vapply(ours, function(record) record$id, character(1)), rev(sent)) # newest first
  expect_gte(everything$total, 5)

  few <- home_service_recent_requests(test_connection)
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
