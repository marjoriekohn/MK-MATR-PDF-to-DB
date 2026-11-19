# Merging one row into another of its kind (service_edit_merge.R): two questions that ask the same thing in
# slightly different words, two headings, two answer choices, two tags, two scales, two investigators. Everything
# that used the one merged uses the other instead, and the one merged goes. The sample tables are described in
# helper-fixtures.R.

# The plan for merging the row `from` of kind `key` into the row `into`, both by key.
merge_plan <- function(key, from, into, change = identity) {
  entity <- edit_entities[[key]]
  snap <- test_snapshot(edit_service_tables(entity), change)
  edit_service_merge_plan(entity, snap, edit_service_row(entity, snap, as.character(from)), as.character(into))
}

test_that("a kind says what a row can be merged into: others like it, never itself", {
  candidates <- function(key, row_key) {
    entity <- edit_entities[[key]]
    snap <- test_snapshot(edit_service_tables(entity))
    edit_merge_candidates(entity, snap, edit_service_row(entity, snap, as.character(row_key)))
  }
  # a question into another question, a section into another section
  expect_setequal(candidates("component", 4)$component_id, c(3, 5, 7, 9, 11, 14))
  expect_setequal(candidates("component", 1)$component_id, c(6, 10, 12))
  # there is one group, so nothing to merge it into
  expect_equal(nrow(candidates("component", 16)), 0)
  # a tag into another of its kind
  expect_true(all(candidates("tag", 12)$kind == "time frame"))
  expect_false(12 %in% candidates("tag", 12)$tag_id)
  expect_equal(nrow(candidates("answer_option", 1)), 38)
  expect_setequal(candidates("investigator", 1)$principal_investigator_id, c(2, 3, 4))
  # what can't be merged says so by having no candidates at all
  expect_null(candidates("questionnaire", 1))
  expect_true(all(vapply(edit_entities[c("component", "tag", "answer_option", "measurement_scale", "investigator", "respondent_type")], edit_can_merge, logical(1))))
  expect_false(any(vapply(edit_entities[c("registry", "study", "questionnaire", "wave", "placement", "question_scale", "pdf_file", "publication")], edit_can_merge, logical(1))))
})

test_that("merging a question into another points every place that prints it at the other, and it goes", {
  plan <- merge_plan("component", 11, 4)
  expect_equal(item_lines(plan$items), c(
    'Update placement {"placement_id":411} component_id: 11 -> 4',
    'Delete component {"component_id":11} kind: Question -> -',
    'Delete component {"component_id":11} text: did you smoke during your pregnancy? -> -'
  ))
  expect_equal(plan$summary, paste(
    "Merged the question “did you smoke during your pregnancy?” into “do you now smoke cigarettes?”,",
    "moving 1 block in a questionnaire"
  ))
  expect_equal(plan$key, "4") # the one that is left, to show afterwards
  expect_length(plan$blocked, 0)
  expect_equal(plan$moves[[1]][c("table", "n")], list(table = "placement", n = 1))
  expect_match(plan$sentence, "1 block in a questionnaire moves to “do you now smoke cigarettes?”, and “did you smoke during your pregnancy?” is removed.", fixed = TRUE)
})

test_that("a question's place in a measurement scale moves with it, once", {
  plan <- merge_plan("component", 9, 7)
  expect_equal(item_lines(plan$items), c(
    'Update placement {"placement_id":209} component_id: 9 -> 7',
    'Update placement {"placement_id":309} component_id: 9 -> 7',
    'Delete question_scale {"component_id":9,"scale_id":1} NA: - -> -',
    'Insert question_scale {"component_id":7,"scale_id":1} NA: - -> -',
    'Delete component {"component_id":9} kind: Question -> -',
    'Delete component {"component_id":9} text: how often in the past week did you feel sad? -> -'
  ))
  expect_match(plan$summary, "moving 2 blocks in questionnaires and 1 scale item$")
  # the other is in the scale already: it isn't put there twice
  both <- merge_plan("component", 9, 7, function(snap) {
    snap$question_scale <- rbind(snap$question_scale, data.frame(scale_id = 1, component_id = 7))
    snap
  })
  expect_false(any(both$items$action == "Insert"))
  expect_equal(sum(both$items$entity_name == "question_scale"), 1)
})

test_that("a heading merges like a question: each place keeps what is inside it", {
  plan <- merge_plan("component", 12, 10)
  expect_equal(item_lines(plan$items), c(
    'Update placement {"placement_id":412} component_id: 12 -> 10',
    'Delete component {"component_id":12} kind: Section -> -',
    'Delete component {"component_id":12} text: pregnancy -> -'
  ))
  expect_match(plan$summary, "^Merged the section “pregnancy” into “mood”")
  # nothing prints component 15: it is only removed
  unused <- merge_plan("component", 15, 15, function(snap) {
    snap$component <- rbind(snap$component, data.frame(component_id = 99, kind = "Note", text = "thanks."))
    snap
  })
  expect_null(unused) # not into itself
  unused <- merge_plan("component", 15, 99, function(snap) {
    snap$component <- rbind(snap$component, data.frame(component_id = 99, kind = "Note", text = "thanks."))
    snap
  })
  expect_equal(unused$items$action, c("Delete", "Delete"))
  expect_equal(unused$summary, "Merged the note “thank you for taking part.” into “thanks.”")
  expect_match(unused$sentence, "Nothing uses “thank you for taking part.”, so it is only removed.", fixed = TRUE)
  # a row of another kind, or one that isn't there, is no plan
  expect_null(merge_plan("component", 12, 4))
  expect_null(merge_plan("component", 12, 12345))
})

test_that("an answer choice merged into another keeps each variable's code and order, and its tags", {
  tagged <- function(snap) {
    snap$response_option_tag <- rbind(snap$response_option_tag, data.frame(variable_key = 2, answer_option_id = 2, tag_id = 11))
    snap
  }
  plan <- merge_plan("answer_option", 2, 16, tagged)
  expect_equal(item_lines(plan$items)[1:5], c(
    'Delete response_option {"answer_option_id":2,"variable_key":2} display_order: 2 -> -',
    'Delete response_option {"answer_option_id":2,"variable_key":2} code_value: 0 -> -',
    'Insert response_option {"answer_option_id":16,"variable_key":2} display_order: - -> 2',
    'Insert response_option {"answer_option_id":16,"variable_key":2} code_value: - -> 0',
    'Insert response_option_tag {"answer_option_id":16,"tag_id":11,"variable_key":2} NA: - -> -'
  ))
  expect_equal(utils::tail(item_lines(plan$items), 1), 'Delete answer_option {"answer_option_id":2} standardized_label: no -> -')
  expect_equal(sum(plan$items$entity_name == "response_option" & plan$items$action == "Insert"), 6) # three variables, two fields each
  expect_equal(plan$summary, "Merged the answer choice “no” into “never”, moving 3 response options for variables")
  expect_length(plan$blocked, 0)
  # nothing offers it: only removed
  expect_equal(item_lines(merge_plan("answer_option", 39, 14)$items), 'Delete answer_option {"answer_option_id":39} standardized_label: maybe or somewhat -> -')
})

test_that("two answer choices one variable offers both of can't be merged, and the plan says which variables", {
  plan <- merge_plan("answer_option", 14, 13)
  expect_length(plan$blocked, 1)
  expect_match(plan$blocked[[1]]$text, "3 variables offer both")
  expect_equal(plan$blocked[[1]]$labels, c("AH03DR · doctor visits", "KP01SD · felt sad", "KQ01SD · felt sad"))
  expect_match(plan$blocked[[1]]$fix, "Take one of the two off")
  expect_equal(NROW(plan$items), 0) # nothing to apply
})

test_that("a tag merged from its details is the same change as one renamed to another tag's name", {
  entity <- edit_entities$tag
  snap <- tagged_snapshot()
  plan <- edit_service_merge_plan(entity, snap, edit_service_row(entity, snap, "13"), "12")
  renamed <- form_items(form_for("tag", "update", "13", snap = snap, tag__name = "Past Year"))
  expect_equal(plan$items, renamed$items)
  expect_equal(plan$summary, renamed$summary)
  # a tag only merges into one of its own kind
  expect_null(edit_service_merge_plan(entity, snap, edit_service_row(entity, snap, "13"), "1"))
})

test_that("a scale's items move to the scale it is merged into, and an investigator's studies to the other investigator", {
  two_scales <- function(snap) {
    snap$measurement_scale <- rbind(snap$measurement_scale, data.frame(
      scale_id = 2, name = "SMS", abbreviation = "SMS-2", version = NA_character_, description = NA_character_, citation = NA_character_
    ))
    snap$question_scale <- rbind(snap$question_scale, data.frame(scale_id = c(2, 2), component_id = c(9, 7)))
    snap
  }
  scale <- merge_plan("measurement_scale", 2, 1, two_scales)
  expect_equal(item_lines(scale$items)[1:3], c(
    'Delete question_scale {"component_id":9,"scale_id":2} NA: - -> -', # scale 1 lists question 9 already
    'Delete question_scale {"component_id":7,"scale_id":2} NA: - -> -',
    'Insert question_scale {"component_id":7,"scale_id":1} NA: - -> -'
  ))
  expect_equal(scale$items$attribute_name[4:8], c("name", "abbreviation", "version", "description", "citation"))
  expect_equal(scale$summary, "Merged the measurement scale “SMS” into “Short Mood Scale”, moving 2 scale items")

  lead <- merge_plan("investigator", 2, 3)
  expect_equal(item_lines(lead$items), c(
    'Update study {"study_id":2} principal_investigator_id: 2 -> 3',
    'Update study {"study_id":2} contact_principal_investigator_id: 2 -> 3',
    'Delete principal_investigator {"principal_investigator_id":2} name: Sam Patel -> -',
    'Delete principal_investigator {"principal_investigator_id":2} email: sam.patel@example.edu -> -'
  ))
  expect_equal(lead$summary, "Merged the investigator “Sam Patel” into “Lee Morgan”, moving 1 study") # one study, though it names him twice
})

test_that("a drop-down of what to merge into names each candidate so near-duplicates can be told apart", {
  entity <- edit_entities$component
  snap <- test_snapshot(edit_service_tables(entity))
  choices <- edit_merge_choices(entity, snap, edit_service_row(entity, snap, "11"))
  expect_equal(unname(choices[1]), "")
  expect_true("4" %in% choices)
  expect_equal(names(choices)[choices == "4"], "do you now smoke cigarettes? · AHP, MQ")
  expect_equal(names(choices)[choices == "14"], "how many children do you have? · MQ")
  tags <- edit_merge_choices(edit_entities$tag, tagged_snapshot(), edit_service_row(edit_entities$tag, tagged_snapshot(), "13"))
  expect_true("12" %in% tags)
  expect_equal(names(tags)[tags == "12"], "past year")
})
