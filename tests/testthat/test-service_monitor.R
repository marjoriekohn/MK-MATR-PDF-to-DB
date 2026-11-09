# The AI Monitor's sums: the corrections in pushed extractions, flattened, counted by kind and by the same
# before and after, totalled, by prompt and over time. Records are made here, as monitor_model_get_pushed()
# gives them; nothing reads the database.

# A correction, as draft_service_review_edits() keeps it.
monitor_edit <- function(change, item, field = NULL, before = NULL, after = NULL, where = "Question 1") {
  list(change = change, item = item, ref = "c1", where = where, field = field, before = before, after = after)
}

# A pushed extraction's record.
monitor_record <- function(id, applied, edits = list(), prompt_version = "aaa111", summary = paste("Add Questionnaire", id)) {
  list(
    id = id, summary = summary, pdf_name = paste0(id, ".pdf"), applied = applied,
    model = "gemini-3.8-flash", prompt_version = prompt_version, edits = edits
  )
}

monitor_records <- function() {
  years <- monitor_edit("edited", "variable", "unit", "years old", "years", where = "Question 26 > variable 1")
  list(
    "1" = monitor_record("1", "2026-10-01T12:00:00Z", list(
      years,
      monitor_edit("edited", "variable", "unit", "years old", "years", where = "Question 31 > variable 1"),
      monitor_edit("deleted", "block", before = "component_type: Instruction; text: please answer.")
    ), prompt_version = "old000"),
    "2" = monitor_record("2", "2026-10-03T12:00:00Z", list(
      years,
      monitor_edit("edited", "block", "component_type", "Group", "Grid"),
      monitor_edit("edited", "questionnaire", "name", NULL, "Kid Questionnaire", where = "The questionnaire")
    )),
    "3" = monitor_record("3", "2026-10-20T12:00:00Z")
  )
}

test_that("every correction is a row, with its kind in words and the extraction it is in", {
  rows <- monitor_service_corrections(monitor_records())
  expect_equal(nrow(rows), 6)
  expect_equal(rows$extraction_id, c("1", "1", "1", "2", "2", "2"))
  expect_equal(rows$extraction[1], "Add Questionnaire 1")
  expect_equal(unique(rows$label), c(
    "Changed a variable's unit", "Removed a block", "Changed a block's kind", "Changed the questionnaire's name"
  ))
  expect_true(is.na(rows$field[3])) # a removal is of the whole block
  expect_true(is.na(rows$after[3]))
  expect_true(is.na(rows$before[6])) # the AI read no name
  expect_s3_class(rows$applied, "POSIXct")
})

test_that("with no corrections there are no rows, but the columns are there", {
  rows <- monitor_service_corrections(list(monitor_record("3", "2026-10-20T12:00:00Z")))
  expect_equal(nrow(rows), 0)
  expect_true(all(c("kind", "label", "before", "after") %in% names(rows)))
  expect_length(monitor_service_common(rows, 1), 0)
})

test_that("a kind of correction in words, for each change", {
  expect_equal(
    monitor_kind_label(c("edited", "added", "deleted", "moved", "edited"), c("variable", "choice", "block", "block", "choice"), c("time_frame", NA, NA, "position", "answer_option")),
    c("Changed a variable's time frame", "Added a choice", "Removed a block", "Moved a block", "Changed a choice's wording")
  )
})

test_that("the commonest kind comes first, with how many extractions have it, and the same change made again counts together", {
  records <- monitor_records()
  common <- monitor_service_common(monitor_service_corrections(records), length(records))
  expect_equal(common[[1]]$label, "Changed a variable's unit")
  expect_equal(common[[1]]$count, 3)
  expect_equal(common[[1]]$extractions, 2)
  expect_equal(common[[1]]$share, 2 / 3)
  # "years old" to "years" three times, in both extractions: one example
  examples <- common[[1]]$examples
  expect_equal(nrow(examples), 1)
  expect_equal(examples$before, "years old")
  expect_equal(examples$after, "years")
  expect_equal(examples$count, 3)
  expect_equal(examples$extractions, 2)
  expect_equal(examples$where, "Question 26 > variable 1")
  expect_equal(examples$extraction_id, "1")
  # the rest, one each, in the order of their names
  expect_equal(vapply(common[-1], function(kind) kind$label, character(1)), c(
    "Changed a block's kind", "Changed the questionnaire's name", "Removed a block"
  ))
})

test_that("only the commonest few of a kind's corrections are kept", {
  edits <- lapply(1:8, function(i) monitor_edit("edited", "block", "text", paste("was", i), paste("now", i)))
  edits <- c(edits, list(monitor_edit("edited", "block", "text", "was 8", "now 8")))
  rows <- monitor_service_corrections(list(monitor_record("1", "2026-10-01T12:00:00Z", edits)))
  examples <- monitor_service_common(rows, 1, shown = 3)[[1]]$examples
  expect_equal(nrow(examples), 3)
  expect_equal(examples$before[1], "was 8")
  expect_equal(examples$count[1], 2)
})

test_that("the headline numbers count the extractions, their corrections, and those pushed as read", {
  totals <- monitor_service_totals(monitor_records())
  expect_equal(totals$extractions, 3)
  expect_equal(totals$corrections, 6)
  expect_equal(totals$per_extraction, 2)
  expect_equal(totals$untouched, 1)
  expect_true(is.na(monitor_service_totals(list())$per_extraction))
})

test_that("each prompt's extractions and corrections, the prompt used last first", {
  rows <- monitor_service_by_prompt(monitor_records())
  expect_equal(rows$prompt_version, c("aaa111", "old000"))
  expect_equal(rows$extractions, c(2, 1))
  expect_equal(rows$corrections, c(3, 3))
  expect_equal(rows$per_extraction, c(1.5, 3))
  expect_equal(rows$model[1], "gemini-3.8-flash")
  expect_equal(nrow(monitor_service_by_prompt(list())), 0)
})

test_that("over time: a bar per day while the extractions span a month, every day in between counted, none as nought", {
  series <- monitor_service_over_time(monitor_records(), tz = "UTC")
  expect_equal(attr(series, "unit"), "day")
  expect_equal(nrow(series), 20) # Oct 1 to Oct 20
  expect_equal(series$label[c(1, 3, 20)], c("Oct 1", "Oct 3", "Oct 20"))
  expect_equal(series$corrections[c(1, 2, 3, 20)], c(3, 0, 3, 0))
  expect_equal(series$extractions[c(1, 2, 3, 20)], c(1, 0, 1, 1))
  expect_equal(sum(series$corrections), 6)
})

test_that("over time: by week, from Monday, for half a year, and by month for longer", {
  records <- monitor_records()
  records[["3"]]$applied <- "2026-12-15T12:00:00Z"
  weeks <- monitor_service_over_time(records, tz = "UTC")
  expect_equal(attr(weeks, "unit"), "week")
  expect_equal(weeks$start[1], as.Date("2026-09-28")) # the Monday before Oct 1
  expect_equal(weeks$corrections[1], 6) # Oct 1 and Oct 3 are in one week
  expect_equal(monitor_period_text(weeks)[1], "Week of Sep 28: 6 corrections in 2 extractions")

  records[["3"]]$applied <- "2027-06-15T12:00:00Z"
  months <- monitor_service_over_time(records, tz = "UTC")
  expect_equal(attr(months, "unit"), "month")
  expect_equal(months$label[1:2], c("Oct 2026", "Nov 2026"))
  expect_equal(nrow(months), 9) # Oct 2026 to Jun 2027
  expect_equal(nrow(monitor_service_over_time(list())), 0)
})

test_that("the chart has a bar for each period, saying what it counts, and nothing to chart says so", {
  series <- monitor_service_over_time(monitor_records(), unit = "week", tz = "UTC")
  html <- as.character(monitor_chart_ui(series))
  expect_equal(lengths(regmatches(html, gregexpr("<rect", html))), nrow(series))
  expect_match(html, "<title>Week of Sep 28: 6 corrections in 2 extractions</title>", fixed = TRUE)
  expect_match(html, 'role="img"', fixed = TRUE)
  expect_match(as.character(monitor_chart_ui(monitor_service_over_time(list()))), "Nothing has been pushed yet")
})
