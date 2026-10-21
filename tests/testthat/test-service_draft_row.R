# What extraction drafts and edits have in common: whose they are, and who is working on one now.

test_that("a draft someone else claimed lately is theirs to work on, until their claim lapses", {
  me <- list(staff_id = 1)
  now <- as.POSIXct("2026-10-20 12:00:00", tz = "UTC")
  claimed <- function(by_id, at, by = "Sam") list(editing = list(by_id = by_id, by = by, at = at))

  editor <- draft_row_service_editor(claimed(2, "2026-10-20T11:59:00Z"), me, now)
  expect_equal(editor$by, "Sam")
  expect_equal(editor$at, "2026-10-20T11:59:00Z")
  expect_null(draft_row_service_editor(claimed(1, "2026-10-20T11:59:00Z"), me, now)) # this person's own
  expect_null(draft_row_service_editor(claimed(2, "2026-10-20T11:56:00Z"), me, now)) # not renewed for 4 minutes
  expect_null(draft_row_service_editor(list(editing = NULL), me, now))
  expect_equal(draft_row_service_editor(claimed(2, "2026-10-20T11:59:30Z", by = NULL), me, now)$by, "Someone no longer listed")
})

test_that("a claim lapses once it hasn't been renewed for a few minutes", {
  now <- as.POSIXct("2026-10-20 12:00:00", tz = "UTC")
  expect_false(draft_row_service_lapsed("2026-10-20T11:58:00Z", now))
  expect_true(draft_row_service_lapsed(format_time(now - draft_claim_stale_seconds - 1), now))
  expect_true(draft_row_service_lapsed(NULL, now))
  # renewed often enough that one missed renewal doesn't lose it, and let go long before it would lapse by idling
  expect_gt(draft_claim_stale_seconds, 2 * draft_claim_renew_seconds)
  expect_gt(draft_claim_idle_seconds, draft_claim_stale_seconds)
})
