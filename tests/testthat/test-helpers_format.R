test_that("times are kept in UTC and read back from the app's or the database's form", {
  time <- as.POSIXct("2026-09-29 14:03:22", tz = "UTC")
  expect_equal(format_time(time), "2026-09-29T14:03:22Z")
  expect_equal(parse_time("2026-09-29T14:03:22Z"), time)
  expect_equal(parse_time("2026-09-29T14:03:22.5+00:00"), time + 0.5) # fractions are kept...
  expect_equal(format_time(parse_time("2026-09-29T14:03:22.5+00:00")), "2026-09-29T14:03:22Z") # ...and written as whole seconds
  expect_equal(parse_time("2026-09-29T10:03:22-04:00"), time)
  expect_true(is.na(parse_time(NA_character_)))
})

test_that("how long ago is said in words", {
  now <- as.POSIXct("2026-10-03 15:00:00")
  expect_equal(time_ago(now - 20, now), "just now")
  expect_equal(time_ago(now - 60, now), "1 minute ago")
  expect_equal(time_ago(now - 5 * 60, now), "5 minutes ago")
  expect_equal(time_ago(now - 2 * 3600, now), "2 hours ago")
  expect_equal(time_ago(as.POSIXct("2026-10-02 12:00:00"), now), "yesterday")
  expect_equal(time_ago(as.POSIXct("2026-09-29 12:00:00"), now), "on Sep 29")
  expect_equal(day_phrase(as.POSIXct("2026-10-03 09:15:00"), now), "today at 9:15 AM")
})

test_that("durations and sizes are short words", {
  expect_equal(duration_text(40), "40 s")
  expect_equal(duration_text(100.7), "1 min 40 s")
  expect_equal(file_size(640 * 1024), "640 KB")
  expect_equal(file_size(2.1 * 1024^2), "2.1 MB")
})

test_that("counts take their noun, with a plural that can be irregular and thousands that can be marked", {
  expect_equal(plural(1, "page"), "1 page")
  expect_equal(plural(3, "page"), "3 pages")
  expect_equal(plural(0, "page"), "0 pages")
  expect_equal(plural(2, "hour", "ago"), "2 hours ago")
  expect_equal(plural(2, "study", plural_form = "studies"), "2 studies")
  expect_equal(plural(1200, "wave"), "1200 waves")
  expect_equal(plural(1200, "wave", big_mark = ","), "1,200 waves")
})

test_that("the edit pages' counts say what they said before they shared plural()", {
  expect_equal(edit_count(1, "study", "studies"), "1 study")
  expect_equal(edit_count(3, "study", "studies"), "3 studies")
  expect_equal(edit_count(2, "variable", "variables"), "2 variables")
  expect_equal(edit_count(1200, "wave"), "1,200 waves")
  expect_equal(plural_noun(1, "wave"), "1 wave")
  expect_equal(plural_noun(2, "study"), "2 studies")
  expect_equal(plural_noun(2, "principal_investigator"), "2 investigators")
})

test_that("words are listed with commas and an 'and'", {
  expect_equal(word_list(character()), "")
  expect_equal(word_list("study"), "study")
  expect_equal(word_list(c("study", "wave name")), "study and wave name")
  expect_equal(word_list(c("registry", "study", "wave name")), "registry, study and wave name")
})

test_that("text counts only if it has something besides spaces, and nothing is nothing", {
  expect_equal(has_text(c("", "  ", "a", " b ")), c(FALSE, FALSE, TRUE, TRUE))
  expect_false(has_text(NULL))
})
