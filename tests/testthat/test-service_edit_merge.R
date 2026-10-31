# Merging one answer choice into another on Make an edit: updating a choice to the words of another
# (service_edit_entities.R, edit_merge_items()).

# "1 to 2" (on variable 10, with a count of answers, in set 100 with "3-4") and "1-2" (on variable 11, in set 200
# with "3-4"): once merged, set 100 has the same choices as set 200, which more variables offer.
merge_snap <- function() {
  list(
    answer_option = data.frame(answer_option_id = c(1, 2, 3), standardized_label = c("1 to 2", "1-2", "3-4")),
    response_option = data.frame(
      variable_key = c(10, 11, 11), answer_option_id = c(1, 2, 3), display_order = c(1, 1, 2), code_value = c("1", "1", "2"),
      skip_instruction = c("Go to 5", NA, NA), skip_to_component_id = NA_real_, skip_to_end = FALSE
    ),
    response_option_count = data.frame(wave_id = 40, variable_key = 10, answer_option_id = 1, respondent_count = 7),
    answer_option_set = data.frame(option_set_id = c(100, 100, 200, 200), answer_option_id = c(1, 3, 2, 3)),
    option_set = data.frame(option_set_id = c(100, 200), name = c("1 to 2 / 3-4", "1-2 / 3-4")),
    closed_ended = data.frame(variable_key = c(10, 11, 12), option_set_id = c(100, 200, 200)),
    question_scale = data.frame(scale_id = 5, component_id = 50, expected_option_set_id = 100, item_number = "1"),
    answer_option_alias = data.frame(alias = "one to two", answer_option_id = 1)
  )
}

test_that("merging moves each response option, its counts, the sets and the merged wordings, then deletes the choice", {
  lines <- item_lines(edit_merge_items(merge_snap(), 1, 2))
  expect_equal(lines, c(
    'Delete response_option_count {"answer_option_id":1,"variable_key":10,"wave_id":40} NA: - -> -',
    'Delete response_option {"answer_option_id":1,"variable_key":10} NA: - -> -',
    'Insert response_option {"answer_option_id":2,"variable_key":10} display_order: - -> 1',
    'Insert response_option {"answer_option_id":2,"variable_key":10} code_value: - -> 1',
    'Insert response_option {"answer_option_id":2,"variable_key":10} skip_instruction: - -> Go to 5',
    'Insert response_option {"answer_option_id":2,"variable_key":10} skip_to_end: - -> false',
    'Insert response_option_count {"answer_option_id":2,"variable_key":10,"wave_id":40} respondent_count: - -> 7',
    # set 100 now has set 200's choices: its variable and scale item use 200, and it goes
    'Update closed_ended {"variable_key":10} option_set_id: 100 -> 200',
    'Update question_scale {"component_id":50,"scale_id":5} expected_option_set_id: 100 -> 200',
    'Delete answer_option_set {"answer_option_id":1,"option_set_id":100} NA: - -> -',
    'Delete answer_option_set {"answer_option_id":3,"option_set_id":100} NA: - -> -',
    'Delete option_set {"option_set_id":100} name: 1 to 2 / 3-4 -> -',
    'Update answer_option_alias {"alias":"one to two"} answer_option_id: 1 -> 2',
    'Insert answer_option_alias {"alias":"1 to 2"} answer_option_id: - -> 2',
    'Delete answer_option {"answer_option_id":1} standardized_label: 1 to 2 -> -'
  ))
})

test_that("a set that stays different gets the other choice in place of the merged one", {
  snap <- merge_snap()
  snap$answer_option_set <- data.frame(option_set_id = c(100, 200, 200), answer_option_id = c(1, 2, 3))
  lines <- item_lines(edit_merge_items(snap, 1, 2))
  expect_true('Delete answer_option_set {"answer_option_id":1,"option_set_id":100} NA: - -> -' %in% lines)
  expect_true('Insert answer_option_set {"answer_option_id":2,"option_set_id":100} NA: - -> -' %in% lines)
  expect_false(any(grepl("^Delete option_set", lines)))
})

test_that("updating a choice to another's words is a merge, said as one; a variable offering both stops it", {
  snap <- merge_snap()
  entity <- edit_entities[["answer_option"]]
  row <- snap$answer_option[1, ]
  fields <- edit_service_fields(entity, snap, "update", row)
  values <- list(answer_option__standardized_label = "1 - 2") # written "1-2"
  found <- edit_service_check(fields, values)
  expect_length(found$problems, 0)
  expect_match(found$warnings[[1]], "merges “1 to 2” into it")
  built <- edit_service_build(entity, "update", snap, row, NULL, fields, values)
  expect_equal(built$summary, "Merged the answer choice “1 to 2” into “1-2”")
  expect_true(any(built$items$entity_name == "answer_option_alias"))

  # new words of its own: a plain rename
  renamed <- edit_service_build(entity, "update", snap, row, NULL, fields, list(answer_option__standardized_label = "one or two"))
  expect_equal(unique(renamed$items$entity_name), "answer_option")

  snap$response_option$variable_key[1] <- 11 # variable 11 offers "1 to 2" and "1-2"
  clash <- edit_service_check(edit_service_fields(entity, snap, "update", row), values)
  expect_match(clash$problems[[1]], "can't be merged")
})

test_that("a wording merged into a choice can't be added as a choice of its own", {
  snap <- merge_snap()
  entity <- edit_entities[["answer_option"]]
  fields <- edit_service_fields(entity, snap, "add")
  found <- edit_service_check(fields, list(answer_option__standardized_label = "One to two"))
  expect_equal(found$problems[[1]], "“one to two” was merged into “1 to 2”. Use that choice.")
})
