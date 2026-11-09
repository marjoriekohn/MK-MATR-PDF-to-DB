# ------ Update ---------------------------------------------------------------------------------------
test_that("an update is one item per field that changed", {
  form <- form_for("registry", "update", "1", registry__name = "Mid-Atlantic Twin Registry (VCU)")
  built <- form_items(form)
  expect_equal(
    item_lines(built$items),
    'Update registry {"registry_id":1} name: Mid-Atlantic Twin Registry -> Mid-Atlantic Twin Registry (VCU)'
  )
  expect_equal(built$items$item_number, 1)
  expect_equal(built$summary, "Changed the name of the registry “Mid-Atlantic Twin Registry”")
})

test_that("several changed fields are several items, in the form's order", {
  form <- form_for("wave", "update", "2", wave__name = "Round 2", wave__administered_date = "05/01/2022", wave__respondent_count = "")
  built <- form_items(form)
  expect_equal(item_lines(built$items), c(
    'Update wave {"wave_id":2} name: Wave 2 -> Round 2',
    'Update wave {"wave_id":2} administered_date: 2022-04-15 -> 2022-05-01',
    'Update wave {"wave_id":2} respondent_count: 1210 -> -'
  ))
  expect_equal(built$items$item_number, 1:3)
  expect_equal(built$summary, "Changed the wave name, date administered and number of respondents of the wave “AHP Wave 2”")
})

test_that("an update with nothing changed has no items, and one with a problem is refused", {
  built <- form_items(form_for("registry", "update", "1"))
  expect_equal(nrow(built$items), 0)
  expect_named(built$items, c("item_number", "entity_name", "record_identifier", "action", "attribute_name", "old_value", "new_value"))
  expect_error(form_items(form_for("registry", "update", "1", registry__participant_count = "lots")), "Type a whole number")
  expect_error(form_items(form_for("registry", "add", registry__name = "Half typed")), "Fill in the abbreviation and participants")
})

test_that("changing a shared component's words is one item on the component", {
  form <- form_for("component", "update", "4", component__text = "Do you currently smoke cigarettes?")
  expect_equal(
    item_lines(form_items(form)$items),
    'Update component {"component_id":4} text: do you now smoke cigarettes? -> do you currently smoke cigarettes?'
  )
  expect_equal(form_items(form)$summary, "Changed the words of the component “do you now smoke cigarettes?”")
})

test_that("a locked field can't be changed, whatever's sent", {
  form <- form_for("wave", "update", "2", wave__questionnaire_id = "4")
  expect_equal(nrow(form_items(form)$items), 0)
  form <- form_for("tag", "update", "17", tag__kind = "unit", tag__name = "in the past 7 days")
  expect_equal(item_lines(form_items(form)$items), 'Update tag {"tag_id":17} name: in the past week -> in the past 7 days')
})

# ------ Add ------------------------------------------------------------------------------------------
test_that("an add is one Insert per field filled in, on a row with no ID yet", {
  form <- form_for("registry", "add", registry__name = "New Registry", registry__abbreviation = "NR", registry__participant_count = "1,000")
  built <- form_items(form)
  expect_equal(item_lines(built$items), c(
    'Insert registry {"registry_id":"new:registry:1"} name: - -> New Registry',
    'Insert registry {"registry_id":"new:registry:1"} abbreviation: - -> NR',
    'Insert registry {"registry_id":"new:registry:1"} participant_count: - -> 1000'
  ))
  expect_equal(built$summary, "Added the registry “New Registry”")
})

test_that("a study is added by itself, without a questionnaire or a wave", {
  form <- form_for("study", "add", study__registry_id = "1", study__principal_investigator_id = "2", study__name = "New Study", study__status = "Active")
  items <- form_items(form)$items
  expect_setequal(unique(items$entity_name), "study")
  expect_equal(items$attribute_name, c("registry_id", "principal_investigator_id", "name", "status"))
})

test_that("a tag is added with its kind, its name written that kind's way", {
  form <- form_for("tag", "add", tag__kind = "time frame", tag__name = "During the last 3 months")
  expect_equal(item_lines(form_items(form)$items), c(
    'Insert tag {"tag_id":"new:tag:1"} kind: - -> time frame',
    'Insert tag {"tag_id":"new:tag:1"} name: - -> in the past 3 months'
  ))
  expect_equal(form_items(form)$summary, "Added the tag “During the last 3 months”")
  form <- form_for("tag", "add", tag__kind = "subject", tag__name = "Respondent’s Twin")
  expect_equal(form_items(form)$items$new_value[2], "Respondent's Twin")
})

test_that("a component is added with its kind and words, in lower case; a group with none", {
  form <- form_for("component", "add", component__kind = "Instruction", component__text = "  Please Answer  Every Question. ")
  expect_equal(item_lines(form_items(form)$items), c(
    'Insert component {"component_id":"new:component:1"} kind: - -> Instruction',
    'Insert component {"component_id":"new:component:1"} text: - -> please answer every question.'
  ))
  form <- form_for("component", "add", component__kind = "Group")
  expect_equal(item_lines(form_items(form)$items), 'Insert component {"component_id":"new:component:1"} kind: - -> Group')
})

test_that("a scale item is one row, named by its scale and question", {
  form <- form_for("question_scale", "add", scope = "1", question_scale__component_id = "7", question_scale__item_number = " 2a ")
  built <- form_items(form)
  expect_equal(item_lines(built$items), 'Insert question_scale {"component_id":7,"scale_id":1} item_number: - -> 2a')
  expect_equal(built$summary, "Added “how often do you see a doctor?” to the measurement scale “Short Mood Scale” as item 2a")
  # only a question can be an item, and once
  choices <- Filter(function(field) field$name == "component_id", form$fields)[[1]]$choices
  expect_setequal(unname(choices), c("", "3", "4", "5", "7", "9", "11", "14"))
  again <- form_for("question_scale", "add", scope = "1", question_scale__component_id = "9", question_scale__item_number = "1")
  problems <- edit_service_check(again$fields, again$values)$problems
  expect_equal(problems$question_scale__component_id, "This question is already item 1 of this scale.")
  expect_equal(problems$question_scale__item_number, "Item 1 of this scale is already “how often in the past week did you feel sad?”.")
  changed <- form_for("question_scale", "update", "1|9", scope = "1", question_scale__item_number = "3")
  expect_equal(item_lines(form_items(changed)$items), 'Update question_scale {"component_id":9,"scale_id":1} item_number: 1 -> 3')
})

# ------ Delete ---------------------------------------------------------------------------------------
test_that("a delete lists every field of the row deleted, to check them when it is applied, and leaves the cascade to the database", {
  entity <- edit_entities$questionnaire
  snap <- test_snapshot(edit_service_tables(entity))
  row <- edit_service_row(entity, snap, "2")
  built <- edit_service_build(entity, "delete", snap, row)
  expect_setequal(unique(built$items$entity_name), "questionnaire")
  expect_equal(built$items$action, rep("Delete", 6))
  expect_equal(built$items$attribute_name, c("study_id", "respondent_type_id", "name", "abbreviation", "participant_count", "description"))
  expect_equal(built$items$old_value[3], "Kid Questionnaire")
  # what goes with it is said, not written: the database takes it along
  expect_equal(built$summary, "Deleted the questionnaire “Kid Questionnaire (KQ)” and 14 connected records")
  expect_equal(built$plan$deleted, list(table = "questionnaire", row = '{"questionnaire_id":2}', name = "Kid Questionnaire (KQ)"))
})

test_that("a row that nothing points at is deleted alone, and its summary says so", {
  entity <- edit_entities$tag
  snap <- test_snapshot(edit_service_tables(entity))
  row <- edit_service_row(entity, snap, "2")
  built <- edit_service_build(entity, "delete", snap, row)
  expect_equal(item_lines(built$items), c(
    'Delete tag {"tag_id":2} kind: subject -> -',
    'Delete tag {"tag_id":2} name: Respondent\'s Family -> -'
  ))
  expect_equal(built$summary, "Deleted the tag “Respondent's Family”")
  expect_false(built$plan$far_reaching)
})

test_that("what's emptied rather than deleted is written first", {
  entity <- edit_entities$investigator
  snap <- test_snapshot(edit_service_tables(entity))
  built <- edit_service_build(entity, "delete", snap, edit_service_row(entity, snap, "4"))
  expect_equal(item_lines(built$items)[1], 'Update study {"study_id":1} contact_principal_investigator_id: 4 -> -')
  expect_equal(built$items$action[-1], c("Delete", "Delete"))
  expect_equal(built$summary, "Deleted the investigator “Jo Park”")
  expect_true(built$plan$far_reaching) # something changes
})

test_that("deleting a question from a questionnaire empties what depends on it and numbers the waves' questions again", {
  entity <- edit_entities$placement
  snap <- test_snapshot(edit_service_tables(entity))
  built <- edit_service_build(entity, "delete", snap, edit_service_row(entity, snap, "104"))
  expect_equal(item_lines(built$items), c(
    'Update placement {"placement_id":105} depends_on_placement_id: 104 -> -',
    'Update wave_question {"placement_id":105,"wave_id":1} question_number: 3 -> 2',
    'Update wave_question {"placement_id":107,"wave_id":1} question_number: 4 -> 3',
    'Update wave_question {"placement_id":105,"wave_id":2} question_number: 3 -> 2',
    'Update wave_question {"placement_id":107,"wave_id":2} question_number: 4 -> 3',
    'Delete placement {"placement_id":104} questionnaire_id: 1 -> -',
    'Delete placement {"placement_id":104} component_id: 4 -> -',
    'Delete placement {"placement_id":104} parent_placement_id: 101 -> -',
    'Delete placement {"placement_id":104} display_order: 3 -> -',
    'Delete placement {"placement_id":104} depends_on_placement_id: - -> -'
  ))
  expect_equal(built$summary, "Deleted the block “AHP · do you now smoke cigarettes?” and 6 connected records")
})

test_that("deleting a section numbers the questions left in each wave from 1", {
  entity <- edit_entities$placement
  snap <- test_snapshot(edit_service_tables(entity))
  built <- edit_service_build(entity, "delete", snap, edit_service_row(entity, snap, "101"))
  expect_equal(item_lines(built$items)[1:2], c(
    'Update wave_question {"placement_id":107,"wave_id":1} question_number: 4 -> 1',
    'Update wave_question {"placement_id":107,"wave_id":2} question_number: 4 -> 1'
  ))
})

test_that("deleting a wave writes no numbers: its questions go with it", {
  entity <- edit_entities$wave
  snap <- test_snapshot(edit_service_tables(entity))
  built <- edit_service_build(entity, "delete", snap, edit_service_row(entity, snap, "1"))
  expect_setequal(unique(built$items$entity_name), "wave")
  expect_equal(built$summary, "Deleted the wave “AHP Wave 1” and 4 connected records")
})

test_that("a delete's plan says what's in the way, and then has no items to apply", {
  entity <- edit_entities$registry
  snap <- test_snapshot(edit_service_tables(entity))
  plan <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "1"))
  expect_length(plan$blocked, 1)
  expect_match(plan$blocked[[1]]$text, "^2 studies are in the way")
  expect_match(plan$blocked[[1]]$fix, "^Empty the registry")
  expect_equal(edit_service_blockers("delete", plan = plan), "This can't be deleted yet: see what's in the way above.")
  # a component printed somewhere is in the way of itself
  entity <- edit_entities$component
  snap <- test_snapshot(edit_service_tables(entity))
  plan <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "9"))
  expect_equal(vapply(plan$blocked, function(b) b$table, character(1)), c("placement", "question_scale"))
  expect_equal(plan$blocked[[1]]$labels, c("KQ · how often in the past week did you feel sad?", "KQP · how often in the past week did you feel sad?"))
})

# ------ Moving a block ------------------------------------------------------------------------------
test_that("a block moved to another section makes room there and closes the gap behind it", {
  placed <- edit_service_placements(test_snapshot(c("placement", "component")), 1)
  moves <- edit_service_move(placed, 103, 106, 1)
  expect_equal(moves$placement_id, c(103, 107, 104, 105))
  expect_equal(moves$new_parent, c(106, 106, 101, 101))
  expect_equal(moves$new_order, c(1, 2, 2, 3))
})

test_that("a block moved up its own section shifts the ones it passes, and one put back where it was moves nothing", {
  placed <- edit_service_placements(test_snapshot(c("placement", "component")), 1)
  up <- edit_service_move(placed, 105, 101, 2)
  expect_equal(up$placement_id, c(105, 103, 104))
  expect_equal(up$new_order, c(2, 3, 4))
  down <- edit_service_move(placed, 102, 101, 3)
  expect_equal(down$placement_id, c(103, 104, 102))
  expect_equal(down$new_order, c(1, 2, 3))
  expect_equal(nrow(edit_service_move(placed, 103, 101, 2)), 0)
  # a position past the end means last, and the top level takes blocks out of a section
  out <- edit_service_move(placed, 103, NA, 9)
  expect_equal(out$new_order[out$placement_id == 103], 3)
  expect_true(is.na(out$new_parent[out$placement_id == 103]))
})

test_that("a block can't go inside itself or what's inside it, and a question depends on any other question but its own followers", {
  snap <- test_snapshot(c("placement", "component", "questionnaire"))
  placed <- edit_service_placements(snap, 4)
  choices <- edit_service_parent_choices(snap, placed, 412)
  expect_equal(unname(choices), "") # the section holds the group, so neither can hold it
  choices <- edit_service_parent_choices(snap, placed, 414)
  expect_equal(unname(choices), c("", "412", "416"))
  expect_equal(names(choices)[3], "— (group) how many children do you have?")
  # any other question, printed before or after (the same change may move it), but not itself or what depends on it
  depends <- edit_service_depends_choices(snap, 1, 105)
  expect_equal(unname(depends), c("103", "104", "107"))
  expect_equal(unname(edit_service_depends_choices(snap, 1, 104)), c("103", "107")) # not 105, which depends on it
  expect_equal(unname(edit_service_depends_choices(snap, 1)), c("103", "104", "105", "107"))
})

test_that("moving a block is Update items for it and for the ones that move over, and each wave numbers its questions again", {
  form <- form_for("placement", "update", "107", scope = "1", placement__parent_placement_id = "101", placement__display_order = "1")
  expect_equal(item_lines(form_items(form)$items), c(
    'Update placement {"placement_id":107} parent_placement_id: 106 -> 101',
    'Update placement {"placement_id":107} display_order: 1 -> 1',
    'Update placement {"placement_id":102} display_order: 1 -> 2',
    'Update placement {"placement_id":103} display_order: 2 -> 3',
    'Update placement {"placement_id":104} display_order: 3 -> 4',
    'Update placement {"placement_id":105} display_order: 4 -> 5',
    'Update wave_question {"placement_id":103,"wave_id":1} question_number: 1 -> 2',
    'Update wave_question {"placement_id":104,"wave_id":1} question_number: 2 -> 3',
    'Update wave_question {"placement_id":105,"wave_id":1} question_number: 3 -> 4',
    'Update wave_question {"placement_id":107,"wave_id":1} question_number: 4 -> 1',
    'Update wave_question {"placement_id":103,"wave_id":2} question_number: 1 -> 2',
    'Update wave_question {"placement_id":104,"wave_id":2} question_number: 2 -> 3',
    'Update wave_question {"placement_id":105,"wave_id":2} question_number: 3 -> 4',
    'Update wave_question {"placement_id":107,"wave_id":2} question_number: 4 -> 1'
  )[-2]) # (a block that keeps its position has no Update for it)
})

# ------ Rewording one place ----------------------------------------------------------------------------
test_that("changing a block's words in one place points that place at a new component, keeping its numbers and variables", {
  form <- form_for("placement", "update", "104", scope = "1", placement__text = "Do you currently smoke cigarettes?")
  expect_equal(item_lines(form_items(form)$items), c(
    'Insert component {"component_id":"new:component:1"} kind: - -> Question',
    'Insert component {"component_id":"new:component:1"} text: - -> do you currently smoke cigarettes?',
    'Update placement {"placement_id":104} component_id: 4 -> new:component:1'
  ))
  expect_equal(form_items(form)$summary, "Changed the words of the block “AHP · do you now smoke cigarettes?”")
})

test_that("words another component of that kind has already are that component, not a copy", {
  form <- form_for("placement", "update", "104", scope = "1", placement__text = "How often do you see a doctor?")
  expect_equal(item_lines(form_items(form)$items), 'Update placement {"placement_id":104} component_id: 4 -> 7')
  # a section with a question's words is a new section
  form <- form_for("placement", "update", "101", scope = "1", placement__text = "How often do you see a doctor?")
  expect_equal(item_lines(form_items(form)$items)[1], 'Insert component {"component_id":"new:component:1"} kind: - -> Section')
})

test_that("a question's dependency and its number in each wave are changed one by one", {
  form <- form_for("placement", "update", "105", scope = "1", placement__depends_on_placement_id = "", placement__qn_2 = "2b")
  expect_equal(item_lines(form_items(form)$items), c(
    'Update placement {"placement_id":105} depends_on_placement_id: 104 -> -',
    'Update wave_question {"placement_id":105,"wave_id":2} printed_number: 3 -> 2b'
  ))
  expect_equal(form_items(form)$summary, "Changed the question it depends on and number printed in Wave 2 of the block “AHP · how many cigarettes do you smoke on a typical day?”")
  form <- form_for("placement", "update", "104", scope = "1", placement__depends_on_placement_id = "103", placement__qn_1 = "")
  expect_equal(item_lines(form_items(form)$items), c(
    'Update placement {"placement_id":104} depends_on_placement_id: - -> 103',
    'Update wave_question {"placement_id":104,"wave_id":1} printed_number: 2 -> -'
  ))
})

# ------ Adding a block to a questionnaire --------------------------------------------------------------------
test_that("a block that's new is added as a component and placed, at the end unless a position is given", {
  form <- form_for("placement", "add", scope = "1", placement__kind = "Note", placement__text = "Thank you!", placement__parent_placement_id = "106")
  expect_equal(item_lines(form_items(form)$items), c(
    'Insert component {"component_id":"new:component:1"} kind: - -> Note',
    'Insert component {"component_id":"new:component:1"} text: - -> thank you!',
    'Insert placement {"placement_id":"new:placement:1"} questionnaire_id: - -> 1',
    'Insert placement {"placement_id":"new:placement:1"} component_id: - -> new:component:1',
    'Insert placement {"placement_id":"new:placement:1"} parent_placement_id: - -> 106',
    'Insert placement {"placement_id":"new:placement:1"} display_order: - -> 2'
  ))
  expect_equal(form_items(form)$summary, "Added the note “Thank you!” to AHP")
})

test_that("a block with words a component of that kind has prints that component, and a group the one group", {
  form <- form_for("placement", "add", scope = "2", placement__kind = "Section", placement__text = "Mood")
  expect_equal(item_lines(form_items(form)$items), c(
    'Insert placement {"placement_id":"new:placement:1"} questionnaire_id: - -> 2',
    'Insert placement {"placement_id":"new:placement:1"} component_id: - -> 10',
    'Insert placement {"placement_id":"new:placement:1"} display_order: - -> 2'
  ))
  form <- form_for("placement", "add", scope = "1", placement__kind = "Group", placement__parent_placement_id = "106")
  expect_equal(form_items(form)$items$new_value[2], "16")
  expect_equal(form_items(form)$summary, "Added the group “(group)” to AHP")
})

test_that("a block placed among others pushes the ones after it along", {
  form <- form_for("placement", "add", scope = "1", placement__kind = "Instruction", placement__text = "Read this", placement__parent_placement_id = "101", placement__display_order = "2")
  lines <- item_lines(form_items(form)$items)
  expect_equal(lines[3:5], c(
    'Update placement {"placement_id":103} display_order: 2 -> 3',
    'Update placement {"placement_id":104} display_order: 3 -> 4',
    'Update placement {"placement_id":105} display_order: 4 -> 5'
  ))
  expect_equal(lines[length(lines)], 'Insert placement {"placement_id":"new:placement:1"} display_order: - -> 2')
  # not a question: the waves' numbers don't change
  expect_false(any(grepl("wave_question", lines)))
})

# ------ Adding a question, whole ---------------------------------------------------------------------------
variables_json <- function(...) as.character(jsonlite::toJSON(list(...), auto_unbox = TRUE))
choice <- function(code, words) list(code_value = code, answer_option = words)

test_that("a new question is added with its place, its dependency, its numbers in each wave that asks it, and its variables", {
  vars <- variables_json(
    list(variable_type = "closed_ended", matr_id = "AH09NW", subject = "Respondent's Twin", label = "", time_frame = "in the past week", response_options = list(choice("1", "Yes"), choice("0", "Never"))),
    list(variable_type = "open_ended", matr_id = "AH09HT", subject = "", label = "Height", answer_data_type = "NUMBER", unit = "inches")
  )
  form <- form_for("placement", "add", scope = "1", placement__kind = "Question", placement__text = "How tall are you?",
    placement__parent_placement_id = "101", placement__display_order = "2", placement__depends_on_placement_id = "103",
    placement__asked_in = "1,2", placement__qn_1 = "1a", placement__variables = vars
  )
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
  built <- form_items(form)
  lines <- item_lines(built$items)
  expect_equal(built$summary, "Added the question “How tall are you?” to AHP")
  # the wording, then its place (the blocks after it moving over)
  expect_equal(lines[1:2], c(
    'Insert component {"component_id":"new:component:1"} kind: - -> Question',
    'Insert component {"component_id":"new:component:1"} text: - -> how tall are you?'
  ))
  expect_true('Insert placement {"placement_id":"new:placement:1"} depends_on_placement_id: - -> 103' %in% lines)
  # each wave: the questions after it numbered again, and its own number and printed number
  expect_true(all(c(
    'Update wave_question {"placement_id":103,"wave_id":1} question_number: 1 -> 2',
    'Update wave_question {"placement_id":107,"wave_id":2} question_number: 4 -> 5',
    'Insert wave_question {"placement_id":"new:placement:1","wave_id":1} question_number: - -> 1',
    'Insert wave_question {"placement_id":"new:placement:1","wave_id":1} printed_number: - -> 1a',
    'Insert wave_question {"placement_id":"new:placement:1","wave_id":2} question_number: - -> 1'
  ) %in% lines))
  expect_false(any(grepl('"wave_id":2} printed_number', lines))) # none printed there
  # the variables: a choice in the list is used (yes, never), a tag in the list is used (in the past week), new ones are added once
  variables <- built$items[built$items$entity_name == "variable", ]
  expect_equal(variables$new_value[variables$attribute_name == "matr_id"], c("AH09NW", "AH09HT"))
  expect_equal(variables$new_value[variables$attribute_name == "variable_type"], c("closed_ended", "open_ended"))
  expect_equal(variables$new_value[variables$attribute_name == "answer_data_type"], "NUMBER")
  expect_equal(variables$new_value[variables$attribute_name == "label"], "height")
  expect_equal(variables$new_value[variables$attribute_name == "placement_id"], rep("new:placement:1", 2))
  expect_true(all(c(
    'Insert response_option {"answer_option_id":1,"variable_key":"new:variable:1"} code_value: - -> 1',
    'Insert response_option {"answer_option_id":16,"variable_key":"new:variable:1"} display_order: - -> 2',
    'Insert tag {"tag_id":"new:tag:1"} kind: - -> subject',
    'Insert tag {"tag_id":"new:tag:1"} name: - -> Respondent\'s Twin',
    'Insert variable_tag {"tag_id":"new:tag:1","variable_key":"new:variable:1"} NA: - -> -',
    'Insert variable_tag {"tag_id":17,"variable_key":"new:variable:1"} NA: - -> -',
    'Insert tag {"tag_id":"new:tag:2"} kind: - -> unit',
    'Insert tag {"tag_id":"new:tag:2"} name: - -> inches',
    'Insert variable_tag {"tag_id":"new:tag:2","variable_key":"new:variable:2"} NA: - -> -'
  ) %in% lines))
  expect_false(any(grepl("answer_option \\{", lines))) # no new answer choice: both are in the list
  # what's new is added before what points at it
  expect_lt(match('Insert tag {"tag_id":"new:tag:1"} kind: - -> subject', lines), match('Insert variable_tag {"tag_id":"new:tag:1","variable_key":"new:variable:1"} NA: - -> -', lines))
})

test_that("a new answer choice and a tag used twice are added once, and a variable about the respondent has no subject tag", {
  vars <- variables_json(
    list(variable_type = "closed_ended", matr_id = "A1", subject = "Respondent", response_options = list(choice("1", "Agree."), choice("2", "Disagree"))),
    list(variable_type = "closed_ended", matr_id = "A2", subject = "respondent", response_options = list(choice("1", "agree"), choice("2", "Disagree")))
  )
  form <- form_for("placement", "add", scope = "1", placement__kind = "Question", placement__text = "Twins agree?", placement__asked_in = "1", placement__variables = vars)
  lines <- item_lines(form_items(form)$items)
  expect_equal(sum(grepl('^Insert answer_option .* standardized_label: - -> agree$', lines)), 1)
  expect_equal(sum(grepl('^Insert answer_option .* standardized_label: - -> disagree$', lines)), 1)
  expect_true('Insert response_option {"answer_option_id":"new:answer_option:1","variable_key":"new:variable:2"} display_order: - -> 1' %in% lines)
  expect_false(any(grepl("variable_tag|Insert tag", lines)))
})

test_that("what stops a question being added is said under the variables field", {
  problem <- function(...) {
    form <- form_for("placement", "add", scope = "1", placement__kind = "Question", placement__text = "Q", placement__asked_in = "1", placement__variables = variables_json(...))
    edit_service_check(form$fields, form$values)$problems$placement__variables
  }
  expect_equal(problem(list(variable_type = "closed_ended", matr_id = "", response_options = list(choice("1", "Yes")))), "Variable 1: fill in its MATR ID.")
  expect_equal(problem(list(variable_type = "closed_ended", matr_id = "AH02SM", response_options = list(choice("1", "Yes")))), "Variable 1: AH02SM is already a MATR ID in this questionnaire. Give it one of its own.")
  expect_equal(problem(list(variable_type = "open_ended", matr_id = "X1", answer_data_type = "")), "Variable 1: pick the kind of answer it takes.")
  expect_equal(problem(list(variable_type = "closed_ended", matr_id = "X1", response_options = list())), "Variable 1: add its choices.")
  expect_equal(problem(list(variable_type = "closed_ended", matr_id = "X1", response_options = list(choice("1", "Yes"), choice("1", "No")))), "Variable 1: give each choice its own code.")
  expect_equal(problem(list(variable_type = "closed_ended", matr_id = "X1", response_options = list(choice("1", "Yes"), choice("2", "yes")))), "Variable 1: list each choice once.")
  expect_equal(problem(
    list(variable_type = "open_ended", matr_id = "X1", answer_data_type = "TEXT"),
    list(variable_type = "open_ended", matr_id = "X1", answer_data_type = "TEXT")
  ), "Variable 2: give it a MATR ID no other variable has.")
  expect_null(problem(list(variable_type = "open_ended", matr_id = "X1", answer_data_type = "TEXT")))
  # a question needs a wave that asks it; a questionnaire with none can't have one yet
  form <- form_for("placement", "add", scope = "1", placement__kind = "Question", placement__text = "Q", placement__asked_in = "")
  expect_equal(edit_service_check(form$fields, form$values)$missing, "waves that ask it")
  no_waves <- test_snapshot(edit_service_tables(edit_entities$placement))
  no_waves$wave <- no_waves$wave[0, ]
  no_waves$wave_question <- no_waves$wave_question[0, ]
  form <- form_for("placement", "add", scope = "1", snap = no_waves, placement__kind = "Question", placement__text = "Q")
  expect_match(edit_service_check(form$fields, form$values)$problems$placement__asked_in, "has no waves yet")
})

test_that("the variables start as one, about the respondent, with one blank choice, and go to the editor's draft and back", {
  start <- edit_variables_parse(edit_variables_default())
  expect_length(start, 1)
  expect_equal(start[[1]]$variable_type, "closed_ended")
  expect_equal(start[[1]]$response_options[[1]]$code_value, "1")
  vars <- variables_json(
    list(variable_type = "closed_ended", matr_id = "A1", subject = "Respondent's Twin", label = "twin", time_frame = "in the past week", response_options = list(choice("1", "yes"), choice("0", "no"))),
    list(variable_type = "open_ended", matr_id = "A2", subject = "", label = "", answer_data_type = "NUMBER", unit = "inches")
  )
  back <- edit_variables_json(edit_variables_draft(edit_variables_parse(vars)))
  expect_equal(jsonlite::fromJSON(back, simplifyVector = FALSE), jsonlite::fromJSON(vars, simplifyVector = FALSE))
  expect_error(edit_variables_parse('{"matr_id": "A1"}'), "aren't a list")
  expect_equal(edit_variables_parse(""), list())
})

test_that("the variable editor offers the tags there are, by kind, and the answer choices", {
  options <- edit_variables_options(test_snapshot(c("tag", "answer_option")))
  expect_equal(options$subjects, c("Respondent's Children", "Respondent's Family", "Respondent's Siblings", "Twins (compared)")) # not the respondent, who is blank
  expect_equal(options$time_frames[1], "in the past 12 months")
  expect_equal(options$units, c("cigarettes", "years"))
  expect_equal(options$answer_data_types, c("NUMBER", "TEXT", "DATE", "TIME"))
  expect_true("yes" %in% options$answer_options)
})
