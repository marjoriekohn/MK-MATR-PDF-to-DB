# The AI Monitor's sums: the corrections in pushed extractions, flattened, counted by kind and by the same
# before and after, totalled, by prompt and over time. Records are made here, as monitor_model_get_pushed()
# gives them; nothing reads the database.

# A correction, as draft_service_review_edits() keeps it.
monitor_edit <- function(change, item, field = NULL, before = NULL, after = NULL, where = "Question 1") {
  list(change = change, item = item, ref = "c1", where = where, field = field, before = before, after = after)
}

# A pushed extraction's record.
monitor_record <- function(id, applied, edits = list(), prompt_version = "aaa111", summary = paste("Add Questionnaire", id), questionnaire = "") {
  list(
    id = id, summary = summary, pdf_name = paste0(id, ".pdf"), applied = applied,
    model = "gemini-3.8-flash", prompt_version = prompt_version, edits = edits, questionnaire = questionnaire
  )
}

monitor_records <- function() {
  years <- monitor_edit("edited", "variable", "unit", "years old", "years", where = "Question 26 > variable 1")
  list(
    "1" = monitor_record("1", "2026-10-01T12:00:00Z", list(
      years,
      monitor_edit("edited", "variable", "unit", "years old", "years", where = "Question 31 > variable 1"),
      monitor_edit("deleted", "block", before = "component_type: Instruction; text: please answer.")
    ), prompt_version = "old000", questionnaire = "MQ"),
    "2" = monitor_record("2", "2026-10-03T12:00:00Z", list(
      years,
      monitor_edit("edited", "block", "component_type", "Group", "Grid"),
      monitor_edit("edited", "questionnaire", "name", NULL, "Kid Questionnaire", where = "The questionnaire")
    ), questionnaire = "KQ"),
    "3" = monitor_record("3", "2026-10-20T12:00:00Z")
  )
}

test_that("every correction is a row, with its kind in words and the extraction it is in", {
  rows <- monitor_service_corrections(monitor_records())
  expect_equal(nrow(rows), 6)
  expect_equal(rows$extraction_id, c("1", "1", "1", "2", "2", "2"))
  expect_equal(rows$extraction[1], "Add Questionnaire 1")
  expect_equal(unique(rows$label), c(
    "Changed a variable's unit", "Removed an instruction", "Changed the kind of block", "Changed the questionnaire's name"
  ))
  expect_equal(unique(rows$questionnaire), c("MQ", "KQ"))
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
    monitor_kind_label(
      c("edited", "added", "deleted", "moved", "edited", "edited", "edited", "edited", "edited"),
      c("variable", "choice", "instruction", "question", "choice", "question", "block", "questionnaire", "question"),
      c("time_frame", NA, NA, "position", "answer_option", "parent_question_id", "component_type", "name", "text")
    ),
    c(
      "Changed a variable's time frame", "Added a choice", "Removed an instruction", "Moved a question",
      "Changed a choice's wording", "Changed the question a question depends on", "Changed the kind of block",
      "Changed the questionnaire's name", "Changed a question's wording"
    )
  )
})

test_that("a correction to a block is named by the kind of block, as far as the correction says", {
  records <- list(monitor_record("1", "2026-10-01T12:00:00Z", list(
    monitor_edit("edited", "block", "subject", "twin a", "twin b", where = "Question 23g, 39i"),
    monitor_edit("edited", "block", "text", "Health", "Your health", where = 'Section "Your health"'),
    monitor_edit("deleted", "block", before = "component_type: Interviewer Script; text: read this aloud", where = 'Interviewer Script "read this aloud"'),
    monitor_edit("added", "block", after = "component_type: Question; text: how old are you?", where = "Question 4"),
    monitor_edit("moved", "block", "position", "at the top level, position 2", "inside A group, position 1", where = "A group"),
    monitor_edit("edited", "block", "text", NULL, "see the back", where = "A note with no heading"),
    monitor_edit("edited", "block", "component_type", "Question", "Instruction", where = 'Instruction "please answer"'),
    monitor_edit("edited", "block", "text", "a", "b", where = "(a block that is gone)")
  )))
  expect_equal(monitor_service_corrections(records)$label, c(
    "Changed a question's subject", "Changed a section's wording", "Removed an interviewer script", "Added a question",
    "Moved a group", "Changed a note's wording", "Changed the kind of block", "Changed a block's wording"
  ))
})

test_that("a row removed or added is said by its words, not by the names of its fields", {
  expect_equal(monitor_said("choice", "answer_option: certainly applies"), "certainly applies")
  expect_equal(monitor_said("block", "component_type: Question; text: how many cigarettes; subject: twin a"), "how many cigarettes · subject: twin a")
  expect_equal(monitor_said("variable", "variable_type: closed_ended; subject: twin b"), "closed ended · subject: twin b")
  expect_equal(monitor_said("block", "component_type: Group"), "(no words)")
  expect_true(is.na(monitor_said("choice", NA)))
})

test_that("what's in a question removed or added counts as part of it, and a row made again in its place counts as what changed", {
  edit <- function(change, item, where, said) {
    list(change = change, item = item, where = where, before = if (change == "deleted") said, after = if (change == "added") said)
  }
  # Question 35 deleted and made again with new words: its variable and a choice made again as they were, a choice reworded
  remade <- list(
    edit("deleted", "block", "Question 35", "component_type: Question; text: about how many"),
    edit("deleted", "variable", "Question 35 > variable 1", "variable_type: closed_ended"),
    edit("deleted", "choice", "Question 35 > variable 1 > choice “none”", "answer_option: none"),
    edit("deleted", "choice", "Question 35 > variable 1 > choice “1-2”", "answer_option: 1-2"),
    edit("added", "block", "Question 35", "component_type: Question; text: how many"),
    edit("added", "variable", "Question 35 > variable 1", "variable_type: closed_ended"),
    edit("added", "choice", "Question 35 > variable 1 > choice “none”", "answer_option: none"),
    edit("added", "choice", "Question 35 > variable 1 > choice “1 or 2”", "answer_option: 1 or 2")
  )
  counted <- monitor_counted_edits(remade)
  said <- vapply(counted, function(edit) paste(edit$change, edit$item, edit$field %||% "", edit$before %||% "", edit$after %||% ""), character(1))
  expect_equal(said, c("edited block text about how many how many", "deleted choice  answer_option: 1-2 ", "added choice   answer_option: 1 or 2"))
  expect_equal(counted[[1]]$where, "Question 35")

  # a question that shouldn't be there, and one the AI missed: what's in them goes with them
  whole <- list(
    edit("deleted", "block", "Question 23g", "component_type: Question; text: comforts"),
    edit("deleted", "variable", "Question 23g > variable 1", "variable_type: closed_ended; subject: twin a"),
    edit("deleted", "choice", "Question 23g > variable 1 > choice “rarely”", "answer_option: rarely"),
    edit("added", "variable", "Question 23i > variable 2", "variable_type: closed_ended; subject: twin b"),
    edit("added", "choice", "Question 23i > variable 2 > choice “rarely”", "answer_option: rarely"),
    edit("deleted", "choice", "Question 2 > variable 1 > choice “none”", "answer_option: none"), # its question is still there
    edit("deleted", "choice", "Question 23 > variable 1 > choice “no”", "answer_option: no") # "Question 23g" isn't Question 23
  )
  expect_equal(monitor_counted_edits(whole), whole[c(1, 4, 6, 7)])
  expect_equal(monitor_counted_edits(list()), list())

  # a choice added where a removed question's choice of the same words was (questions renumbered after the removal):
  # the removed one goes with its question, and the added one still counts
  renumbered <- list(
    edit("deleted", "block", "Question 2", "component_type: Question; text: do you smoke"),
    edit("deleted", "variable", "Question 2 > variable 1", "variable_type: closed_ended"),
    edit("deleted", "choice", "Question 2 > variable 1 > choice “yes”", "answer_option: yes"),
    edit("added", "choice", "Question 2 > variable 1 > choice “yes”", "answer_option: yes")
  )
  expect_equal(monitor_counted_edits(renumbered), renumbered[c(1, 4)])
})

test_that("the pushed extractions are counted by their corrections, not by every row their differences list", {
  edit <- function(change, item, where, said) {
    list(change = change, item = item, where = where, before = if (change == "deleted") said, after = if (change == "added") said)
  }
  # as the database keeps them: a question that shouldn't be there lists its variable and choices too
  as_kept <- list(
    "7" = list(id = "7", applied = "2026-10-01T12:00:00Z", prompt_version = "abc", edits = list(
      edit("deleted", "block", "Question 23g", "component_type: Question; text: comforts"),
      edit("deleted", "variable", "Question 23g > variable 1", "variable_type: closed_ended"),
      edit("deleted", "choice", "Question 23g > variable 1 > choice “rarely”", "answer_option: rarely"),
      edit("deleted", "choice", "Question 23g > variable 1 > choice “often”", "answer_option: often")
    )),
    "8" = list(id = "8", applied = "2026-10-02T12:00:00Z", prompt_version = "abc", edits = list())
  )
  local_stub("monitor_model_get_pushed", function(connection) as_kept)
  records <- monitor_service_get_all(NULL)
  expect_equal(names(records), c("7", "8"))
  expect_equal(records[["7"]]$edits, as_kept[["7"]]$edits[1])
  totals <- monitor_service_totals(records)
  expect_equal(totals[c("extractions", "corrections", "per_extraction", "untouched")], list(extractions = 2L, corrections = 1, per_extraction = 0.5, untouched = 1L))
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
  expect_equal(examples$questionnaires, "KQ, MQ")
  expect_equal(examples$where, "Question 26 > variable 1")
  # the rest, one each, in the order of their names
  expect_equal(vapply(common[-1], function(kind) kind$label, character(1)), c(
    "Changed the kind of block", "Changed the questionnaire's name", "Removed an instruction"
  ))
})

test_that("a kind keeps every one of its corrections, the commonest first, each saying which questionnaires it was in", {
  edits <- lapply(1:8, function(i) monitor_edit("edited", "block", "text", paste("was", i), paste("now", i)))
  edits <- c(edits, list(monitor_edit("edited", "block", "text", "was 8", "now 8")))
  rows <- monitor_service_corrections(list(monitor_record("1", "2026-10-01T12:00:00Z", edits, summary = "", questionnaire = "")))
  examples <- monitor_service_common(rows, 1)[[1]]$examples
  expect_equal(nrow(examples), 8)
  expect_equal(examples$before[1], "was 8")
  expect_equal(examples$count[1], 2)
  expect_equal(examples$questionnaires[1], "1.pdf") # with no questionnaire known, the extraction's name
})

test_that("a kind's card starts shut, holds every correction, and says how often and in which questionnaire", {
  edits <- c(
    lapply(1:7, function(i) monitor_edit("edited", "block", "text", paste("was", i), paste("now", i))),
    list(monitor_edit("edited", "block", "text", "was 1", "now 1")),
    list(monitor_edit("deleted", "choice", before = "answer_option: certainly applies"))
  )
  rows <- monitor_service_corrections(list(monitor_record("1", "2026-10-01T12:00:00Z", edits, questionnaire = "MQ")))
  kinds <- monitor_service_common(rows, 1)
  html <- as.character(monitor_kind_ui(kinds[[1]]))
  expect_no_match(html, "<details[^>]* open", perl = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr('class="monitor-example"', html, fixed = TRUE))), 7)
  expect_no_match(html, "Show all", fixed = TRUE)
  expect_match(html, "2 times in MQ", fixed = TRUE)
  expect_match(html, "Once in MQ", fixed = TRUE)
  expect_match(html, "row-badge is-count", fixed = TRUE) # the count is one colour for every kind of change
  removed <- as.character(monitor_kind_ui(kinds[[2]]))
  expect_match(removed, ">certainly applies<", fixed = TRUE)
  expect_no_match(removed, "answer_option", fixed = TRUE)
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

test_that("a push is counted on the day it was in the site's own time zone, not the day it was in UTC", {
  # 10 pm in New York on Oct 1 is 2 am on Oct 2 in UTC
  late <- list("1" = monitor_record("1", "2026-10-02T02:00:00Z"), "2" = monitor_record("2", "2026-10-03T15:00:00Z"))
  expect_equal(monitor_service_over_time(late)$label, c("Oct 1", "Oct 2", "Oct 3")) # the site's: America/New_York
  expect_equal(monitor_service_over_time(late)$extractions, c(1, 0, 1))
  expect_equal(monitor_service_over_time(late, tz = "UTC")$label, c("Oct 2", "Oct 3"))
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
