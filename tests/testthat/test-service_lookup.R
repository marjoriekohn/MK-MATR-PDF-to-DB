# The pick lists: the fixed kinds, and the tags, answer choices and measurement scales read from the real database.

test_that("the pick lists come from the tag and answer option tables, and the kinds are fixed", {
  subject <- local_row("tag", list(kind = "subject", name = test_name("Subject")), "tag_id")
  frame <- local_row("tag", list(kind = "time frame", name = tolower(test_name("in the past"))), "tag_id")
  unit <- local_row("tag", list(kind = "unit", name = tolower(test_name("units"))), "tag_id")
  option <- local_row("answer_option", list(standardized_label = tolower(test_name("Answer"))), "answer_option_id") # a choice is in lower case
  scale <- local_row("measurement_scale", list(name = test_name("Scale"), abbreviation = test_abbreviation()), "scale_id")
  lookups <- lookup_service_get_all(test_connection)
  expect_equal(lookups$component_types, c("Section", "Group", "Grid", "Question", "Instruction", "Introduction", "Definition", "Note", "Interviewer Script"))
  expect_true(subject$name %in% lookups$subjects)
  expect_false(frame$name %in% lookups$subjects) # each list is its kind
  expect_true(frame$name %in% lookups$time_frames)
  expect_true(unit$name %in% lookups$units)
  expect_true(option$standardized_label %in% lookups$answer_options)
  # a scale is offered by its abbreviation, named by what it is short for
  expect_equal(names(lookups$scales)[lookups$scales == scale$abbreviation], scale$name)
  expect_equal(lookups$answer_data_types, c("NUMBER", "TEXT", "DATE", "TIME"))
  expect_length(lookups$problems, 0)
})

test_that("the respondent is a blank, not a subject to pick", {
  local_stub("lookup_model_get_tags", function(connection) data.frame(tag_id = 1:3, kind = "subject", name = c("Respondent", "Twin A", "Self")))
  local_stub("lookup_model_get_answer_options", function(connection) data.frame(answer_option_id = integer(), standardized_label = character()))
  local_stub("lookup_model_get_scales", function(connection) data.frame(scale_id = 1, name = "Short Mood Scale", abbreviation = "SMS"))
  lookups <- lookup_service_get_all(NULL)
  expect_equal(lookups$subjects, "Twin A")
  expect_equal(lookups$scales, c("Short Mood Scale" = "SMS"))
  expect_length(lookups$problems, 0)
})

test_that("a list that can't be read is empty, with the reason", {
  # a key the database turns down: every list fails the same way, and the fixed lists are still returned
  lookups <- lookup_service_get_all(list(url = test_connection$url, key = "sb_secret_not_the_key"))
  expect_equal(lookups$component_types, edit_component_kinds)
  expect_equal(lookups$subjects, character())
  expect_equal(lookups$time_frames, character())
  expect_equal(lookups$units, character())
  expect_equal(lookups$answer_options, character())
  expect_length(lookups$scales, 0)
  expect_equal(lookups$answer_data_types, c("NUMBER", "TEXT", "DATE", "TIME"))
  expect_length(lookups$problems, 3)
  expect_match(lookups$problems[1], "^The list of subjects, time frames and units couldn't be read. The CARA database turned down the key")
  expect_match(lookups$problems[2], "^The list of answer options couldn't be read")
  expect_match(lookups$problems[3], "^The list of measurement scales couldn't be read")
})
