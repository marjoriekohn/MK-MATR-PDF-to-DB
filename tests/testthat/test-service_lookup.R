# The pick lists, read from the real lookup tables.

test_that("the pick lists come from the lookup tables", {
  subject <- local_row("subject", list(name = test_name("Subject")), "subject_id")
  option <- local_row("answer_option", list(standardized_label = test_name("Answer")), "answer_option_id")
  lookups <- lookup_service_get_all(test_connection)
  expect_true(all(c("Section", "Question") %in% lookups$component_types))
  expect_true(subject$name %in% lookups$subjects)
  expect_true(option$standardized_label %in% lookups$answer_options)
  expect_equal(lookups$answer_data_types, c("NUMBER", "TEXT", "DATE", "TIME"))
  expect_length(lookups$problems, 0)
})

test_that("a list that can't be read is empty, with the reason", {
  # a key the database turns down: every list fails the same way, and the rest of the lookups are still returned
  lookups <- lookup_service_get_all(list(url = test_connection$url, key = "sb_secret_not_the_key"))
  expect_equal(lookups$component_types, character())
  expect_equal(lookups$subjects, character())
  expect_equal(lookups$answer_options, character())
  expect_equal(lookups$answer_data_types, c("NUMBER", "TEXT", "DATE", "TIME"))
  expect_length(lookups$problems, 3)
  expect_match(lookups$problems[1], "^The list of component types couldn't be read. The MATR database turned down the key")
  expect_match(lookups$problems[2], "^The list of subjects couldn't be read")
})
