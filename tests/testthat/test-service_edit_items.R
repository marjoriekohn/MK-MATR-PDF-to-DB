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

test_that("an update with nothing changed has no items", {
  built <- form_items(form_for("registry", "update", "1"))
  expect_equal(nrow(built$items), 0)
  expect_named(built$items, c("item_number", "entity_name", "record_identifier", "action", "attribute_name", "old_value", "new_value"))
})

test_that("changing a shared question's words is one item on the component", {
  smoke <- "4"
  form <- form_for("question", "update", smoke, question__text = "Do you currently smoke cigarettes?")
  built <- form_items(form)
  expect_equal(
    item_lines(built$items),
    'Update component {"component_id":4} text: Do you now smoke cigarettes? -> Do you currently smoke cigarettes?'
  )
})

test_that("a locked field can't be changed, whatever's sent", {
  form <- form_for("wave", "update", "2", wave__questionnaire_id = "2")
  expect_equal(nrow(form_items(form)$items), 0)
  form <- form_for("study", "update", "1", study__principal_investigator_id = "2")
  expect_equal(nrow(form_items(form)$items), 0)
})

# ------ Add -------------------------------------------------------------------------------------------
test_that("an add is one Insert per field filled in, on a row with no ID yet", {
  form <- form_for("registry", "add", registry__name = "New Registry", registry__abbreviation = "NR", registry__participant_count = "1,200")
  built <- form_items(form)
  expect_equal(item_lines(built$items), c(
    'Insert registry {"registry_id":"new:registry:1"} name: - -> New Registry',
    'Insert registry {"registry_id":"new:registry:1"} abbreviation: - -> NR',
    'Insert registry {"registry_id":"new:registry:1"} participant_count: - -> 1200'
  ))
  expect_equal(built$summary, "Added the registry “New Registry”")
})

test_that("a study is added by itself, without a questionnaire or a wave", {
  form <- form_for(
    "study", "add",
    study__registry_id = "1", study__principal_investigator_id = "2", study__name = "New Study", study__status = "Ongoing"
  )
  built <- form_items(form)
  lines <- item_lines(built$items)
  expect_equal(lines, c(
    'Insert study {"study_id":"new:study:1"} registry_id: - -> 1',
    'Insert study {"study_id":"new:study:1"} principal_investigator_id: - -> 2',
    'Insert study {"study_id":"new:study:1"} name: - -> New Study',
    'Insert study {"study_id":"new:study:1"} status: - -> Ongoing'
  ))
})

test_that("a section can be added without words", {
  section <- as.character(edit_type_id(test_snapshot("component_type"), "Section"))
  built <- form_items(form_for("component", "add", component__component_type_id = section, component__text = ""))
  expect_equal(item_lines(built$items), 'Insert component {"component_id":"new:component:1"} component_type_id: - -> 1')
})

test_that("a new question is a component of the kind Question, and its question row", {
  built <- form_items(form_for("question", "add", question__text = "Do you exercise?"))
  expect_equal(item_lines(built$items), c(
    'Insert component {"component_id":"new:component:1"} component_type_id: - -> 2',
    'Insert component {"component_id":"new:component:1"} text: - -> Do you exercise?',
    'Insert question {"component_id":"new:component:1"} component_id: - -> new:component:1'
  ))
})

# ------ Delete ------------------------------------------------------------------------------------------
delete_items_for <- function(key, row_key, scope = NULL) {
  entity <- edit_entities[[key]]
  snap <- test_snapshot(edit_service_tables(entity))
  edit_service_build(entity, "delete", snap, edit_service_row(entity, snap, row_key), scope)
}

test_that("a delete writes every field of every row that goes, children before parents", {
  built <- delete_items_for("question", "3")
  tables <- unique(built$items$entity_name)
  expect_equal(
    tables,
    c("open_ended", "variable", "wave_question", "questionnaire_question", "questionnaire_component", "question", "component")
  )
  expect_true(all(built$items$action == "Delete"))
  expect_true(all(is.na(built$items$new_value)))
  expect_equal(built$items$item_number, seq_len(nrow(built$items)))

  variable <- built$items[built$items$entity_name == "variable", ]
  expect_equal(
    variable$attribute_name,
    c("questionnaire_id", "component_id", "matr_id", "display_order", "label", "respondent_count", "subject_id")
  )
  expect_equal(variable$old_value, c("1", "3", "AH01DB", "1", "Date of birth", "1450", "1"))
  expect_equal(unique(variable$record_identifier), '{"variable_key":1}')
})

test_that("a row with nothing but a key is still deleted", {
  built <- delete_items_for("question", "3")
  rows <- built$items[built$items$entity_name == "question", ]
  expect_equal(item_lines(rows), 'Delete question {"component_id":3} NA: - -> -')
})

test_that("what's emptied rather than deleted is written first", {
  built <- delete_items_for("investigator", "4")
  expect_equal(item_lines(built$items), c(
    'Update study {"study_id":1} contact_principal_investigator_id: 4 -> -',
    'Delete principal_investigator {"principal_investigator_id":4} name: Jo Park -> -',
    'Delete principal_investigator {"principal_investigator_id":4} email: jo.park@example.edu -> -'
  ))
})

test_that("what's inside a section is deleted before the section", {
  built <- delete_items_for("questionnaire_component", "1|1", scope = "1")
  placed <- built$items[built$items$entity_name == "questionnaire_component" & built$items$attribute_name == "display_order", ]
  ids <- sub('.*"component_id":([0-9]+).*', "\\1", placed$record_identifier)
  expect_equal(ids[length(ids)], "1")
  expect_equal(sort(ids), sort(c("1", "2", "3", "4", "5")))
})

test_that("a delete's plan says what's in the way, and whether anything else goes", {
  smoke <- delete_items_for("question", "4")
  expect_length(smoke$plan$blocked, 1)
  expect_true(smoke$plan$far_reaching)

  unused <- delete_items_for("component", "15")
  expect_length(unused$plan$blocked, 0)
  expect_false(unused$plan$far_reaching)
  expect_equal(unused$summary, "Deleted the component “Thank you for taking part.”")

  shared <- delete_items_for("question", "9")
  expect_equal(shared$summary, "Deleted the question “How often in the past week did you feel sad?” and 18 connected records")
})

test_that("the two built-in component types can't be deleted", {
  section <- delete_items_for("component_type", "1")
  expect_match(section$plan$blocked[[1]]$text, "built into this site")
  expect_length(section$plan$blocked, 2)
  note <- delete_items_for("component_type", "7")
  expect_length(note$plan$blocked, 1) # components still use it
})

test_that("deleting a question from one questionnaire leaves the component for the others", {
  built <- delete_items_for("questionnaire_question", "2|9", scope = "2")
  expect_false("component" %in% built$items$entity_name)
  expect_true(all(c("questionnaire_component", "questionnaire_question", "wave_question", "variable") %in% built$items$entity_name))
  expect_true(all(grepl(
    '"questionnaire_id":2',
    built$items$record_identifier[built$items$entity_name %in% c("questionnaire_component", "questionnaire_question")]
  )))
})

# ------ Moving a block inside a questionnaire --------------------------------------------------------------
placed_of <- function(q) {
  snap <- test_snapshot(c("questionnaire_component", "component", "component_type"))
  edit_service_placements(snap, q)
}

test_that("a block moved to another section makes room there and closes the gap behind it", {
  placed <- placed_of(1)
  move <- edit_service_move(placed, 7, 1, 2)
  expect_equal(move$component_id, c(7, 3, 4, 5))
  expect_equal(move$new_parent, c(1, 1, 1, 1))
  expect_equal(move$old_order, c(1, 2, 3, 4))
  expect_equal(move$new_order, c(2, 3, 4, 5))
})

test_that("a block moved up its own section shifts the ones it passes", {
  placed <- placed_of(1)
  move <- edit_service_move(placed, 5, 1, 2)
  expect_equal(move$component_id, c(5, 3, 4))
  expect_equal(move$new_order, c(2, 3, 4))
  expect_equal(move$old_order, c(4, 2, 3))
})

test_that("a block moved down shifts the ones it passes the other way", {
  placed <- placed_of(1)
  move <- edit_service_move(placed, 2, 1, 3)
  expect_equal(move$component_id, c(3, 4, 2))
  expect_equal(move$new_order, c(1, 2, 3))
})

test_that("a block put back where it was moves nothing", {
  expect_equal(nrow(edit_service_move(placed_of(1), 3, 1, 2)), 0)
})

test_that("a position past the end means last, and the top level takes sections out of a section", {
  placed <- placed_of(1)
  expect_equal(edit_service_move(placed, 2, 1, 50)$new_order[edit_service_move(placed, 2, 1, 50)$component_id == 2], 4)
  out <- edit_service_move(placed, 2, NA, 1)
  expect_equal(out$component_id[1:3], c(2, 1, 6))
  expect_equal(out$new_order[1:3], c(1, 2, 3))
  # the section it left closes up: 3, 4, 5 are now 1, 2, 3
  expect_equal(out$new_order[out$component_id %in% c(3, 4, 5)], c(1, 2, 3))
})

test_that("a block can't go inside itself or what's inside it", {
  snap <- test_snapshot(c("questionnaire_component", "component", "component_type"))
  placed <- edit_service_placements(snap, 1)
  expect_equal(edit_service_descendants(placed, 1), c(2, 3, 4, 5))
  choices <- edit_service_parent_choices(snap, placed, 1)
  expect_equal(unname(choices), c("", "6"))
  expect_equal(unname(edit_service_parent_choices(snap, placed)), c("", "1", "6"))
})

test_that("a question can only follow up on one printed before it, and not on its own follow-ups", {
  snap <- test_snapshot(c("questionnaire_component", "component", "component_type", "questionnaire_question"))
  expect_equal(unname(edit_service_parent_question_choices(snap, 1, 4)), "3")
  expect_equal(unname(edit_service_parent_question_choices(snap, 1, 5)), c("3", "4"))
  expect_equal(unname(edit_service_parent_question_choices(snap, 1, 3)), character())
})

# ------ A questionnaire component ---------------------------------------------------------------------------------
test_that("moving a block is Update items for it and for the ones that move over", {
  form <- form_for("questionnaire_component", "update", "1|7",
    scope = "1",
    questionnaire_component__parent_component_id = "1", questionnaire_component__display_order = "2"
  )
  built <- form_items(form)
  expect_equal(item_lines(built$items), c(
    'Update questionnaire_component {"component_id":7,"questionnaire_id":1} parent_component_id: 6 -> 1',
    'Update questionnaire_component {"component_id":7,"questionnaire_id":1} display_order: 1 -> 2',
    'Update questionnaire_component {"component_id":3,"questionnaire_id":1} display_order: 2 -> 3',
    'Update questionnaire_component {"component_id":4,"questionnaire_id":1} display_order: 3 -> 4',
    'Update questionnaire_component {"component_id":5,"questionnaire_id":1} display_order: 4 -> 5'
  ))
})

test_that("changing a block's words in one questionnaire adds a component and swaps it in", {
  form <- form_for("questionnaire_component", "update", "1|2",
    scope = "1",
    questionnaire_component__text = "Please answer every question."
  )
  built <- form_items(form)
  expect_equal(item_lines(built$items), c(
    'Insert component {"component_id":"new:component:1"} component_type_id: - -> 4',
    'Insert component {"component_id":"new:component:1"} text: - -> Please answer every question.',
    'Insert questionnaire_component {"component_id":"new:component:1","questionnaire_id":1} parent_component_id: - -> 1',
    'Insert questionnaire_component {"component_id":"new:component:1","questionnaire_id":1} display_order: - -> 1',
    'Delete questionnaire_component {"component_id":2,"questionnaire_id":1} parent_component_id: 1 -> -',
    'Delete questionnaire_component {"component_id":2,"questionnaire_id":1} display_order: 1 -> -'
  ))
  # the old component stays for anyone else who prints it
  expect_false(any(built$items$entity_name == "component" & built$items$action == "Delete"))
})

test_that("a new section keeps what was inside the old one", {
  form <- form_for("questionnaire_component", "update", "1|1", scope = "1", questionnaire_component__text = "A. About you and your family")
  lines <- item_lines(form_items(form)$items)
  expect_true('Update questionnaire_component {"component_id":3,"questionnaire_id":1} parent_component_id: 1 -> new:component:1' %in% lines)
  expect_equal(sum(grepl("parent_component_id: 1 -> new:component:1", lines)), 4)
  # a top-level section has no parent to insert
  expect_false(any(grepl("Insert questionnaire_component .* parent_component_id", lines)))
  expect_equal(lines[length(lines)], 'Delete questionnaire_component {"component_id":1,"questionnaire_id":1} display_order: 1 -> -')
})

test_that("changing a shared question's words in one questionnaire carries its settings, numbers and variables to the new one", {
  form <- form_for(
    "questionnaire_component",
    "update",
    "4|4",
    scope = "4",
    questionnaire_component__text = "Do you currently smoke cigarettes?"
  )
  lines <- item_lines(form_items(form)$items)
  expect_equal(lines[1:3], c(
    'Insert component {"component_id":"new:component:1"} component_type_id: - -> 2',
    'Insert component {"component_id":"new:component:1"} text: - -> Do you currently smoke cigarettes?',
    'Insert question {"component_id":"new:component:1"} component_id: - -> new:component:1'
  ))
  expect_true('Insert questionnaire_question {"component_id":"new:component:1","questionnaire_id":4} self_contained: - -> true' %in% lines)
  expect_true('Update wave_question {"component_id":4,"questionnaire_id":4,"wave_id":5} component_id: 4 -> new:component:1' %in% lines)
  expect_true('Update variable {"variable_key":8} component_id: 4 -> new:component:1' %in% lines)
  expect_true('Insert question_category {"component_id":"new:component:1","mesh_id":"D012907"} mesh_id: - -> D012907' %in% lines)
  # only MQ's rows are touched: AHP still has the original wording
  expect_false(any(grepl('"questionnaire_id":1', lines)))
  expect_false(any(grepl("variable_key\":2", lines)))
  # the old rows go last
  expect_match(lines[length(lines)], "Delete questionnaire_component")
  expect_true(all(startsWith(lines[(length(lines) - 4):length(lines)], "Delete")))
})

test_that("the follow-ups of a reworded question follow the new one", {
  form <- form_for("questionnaire_component", "update", "1|4", scope = "1", questionnaire_component__text = "Do you currently smoke?")
  lines <- item_lines(form_items(form)$items)
  expect_true('Update questionnaire_question {"component_id":5,"questionnaire_id":1} parent_question_id: 4 -> new:component:1' %in% lines)
})

test_that("a question can't change kind from its placement, nor a section with blocks inside", {
  question <- form_for("questionnaire_component", "update", "1|4", scope = "1")
  kind <- Filter(function(field) field$name == "component_type_id", question$fields)[[1]]
  expect_match(kind$locked, "A question stays a question")
  section <- form_for("questionnaire_component", "update", "1|1", scope = "1")
  kind <- Filter(function(field) field$name == "component_type_id", section$fields)[[1]]
  expect_match(kind$locked, "blocks inside")
  instruction <- form_for("questionnaire_component", "update", "1|2", scope = "1")
  expect_null(Filter(function(field) field$name == "component_type_id", instruction$fields)[[1]]$locked)
})

test_that("a block's new section can't be itself or something inside it", {
  form <- form_for("questionnaire_component", "update", "1|1", scope = "1")
  parent <- Filter(function(field) field$name == "parent_component_id", form$fields)[[1]]
  expect_equal(unname(parent$choices), c("", "6"))
})

test_that("a position past the end is warned about", {
  form <- form_for("questionnaire_component", "update", "1|2", scope = "1", questionnaire_component__display_order = "9")
  expect_match(edit_service_check(form$fields, form$values)$warnings$questionnaire_component__display_order, "only 4 places there")
})

test_that("an existing component is placed at the end unless a position is given", {
  form <- form_for("questionnaire_component", "add", scope = "1", questionnaire_component__component_id = "15")
  lines <- item_lines(form_items(form)$items)
  expect_equal(lines, 'Insert questionnaire_component {"component_id":15,"questionnaire_id":1} display_order: - -> 3')
})

test_that("a block placed among others pushes the ones after it along", {
  form <- form_for("questionnaire_component", "add",
    scope = "1", questionnaire_component__component_id = "15",
    questionnaire_component__parent_component_id = "1", questionnaire_component__display_order = "2"
  )
  lines <- item_lines(form_items(form)$items)
  expect_equal(lines, c(
    'Update questionnaire_component {"component_id":3,"questionnaire_id":1} display_order: 2 -> 3',
    'Update questionnaire_component {"component_id":4,"questionnaire_id":1} display_order: 3 -> 4',
    'Update questionnaire_component {"component_id":5,"questionnaire_id":1} display_order: 4 -> 5',
    'Insert questionnaire_component {"component_id":15,"questionnaire_id":1} parent_component_id: - -> 1',
    'Insert questionnaire_component {"component_id":15,"questionnaire_id":1} display_order: - -> 2'
  ))
})

test_that("a block that's new is added as a component and placed", {
  form <- form_for("questionnaire_component", "add",
    scope = "2", questionnaire_component__source = "new",
    questionnaire_component__component_type_id = "4", questionnaire_component__text = "Answer for each twin.",
    questionnaire_component__parent_component_id = "10", questionnaire_component__display_order = "1"
  )
  lines <- item_lines(form_items(form)$items)
  expect_equal(lines[1:2], c(
    'Insert component {"component_id":"new:component:1"} component_type_id: - -> 4',
    'Insert component {"component_id":"new:component:1"} text: - -> Answer for each twin.'
  ))
  expect_match(lines[3], "Update questionnaire_component")
  expect_true(paste0(
    'Insert questionnaire_component {"component_id":"new:component:1","questionnaire_id":2} ',
    "parent_component_id: - -> 10"
  ) %in% lines)
  expect_true('Update questionnaire_component {"component_id":8,"questionnaire_id":2} display_order: 1 -> 2' %in% lines)
})

# ------ A questionnaire question -----------------------------------------------------------------------------------------
test_that("a follow-up made to stand on its own loses its parent", {
  form <- form_for("questionnaire_question", "update", "1|5", scope = "1", questionnaire_question__self_contained = "TRUE")
  lines <- item_lines(form_items(form)$items)
  expect_equal(lines, c(
    'Update questionnaire_question {"component_id":5,"questionnaire_id":1} self_contained: false -> true',
    'Update questionnaire_question {"component_id":5,"questionnaire_id":1} parent_question_id: 4 -> -'
  ))
})

test_that("a question made a follow-up needs its parent", {
  form <- form_for("questionnaire_question", "update", "1|7", scope = "1", questionnaire_question__self_contained = "FALSE")
  expect_equal(edit_service_check(form$fields, form$values)$missing, "which question does it follow up on?")
  form <- form_for(
    "questionnaire_question",
    "update",
    "1|7",
    scope = "1",
    questionnaire_question__self_contained = "FALSE",
    questionnaire_question__parent_question_id = "4"
  )
  expect_equal(item_lines(form_items(form)$items), c(
    'Update questionnaire_question {"component_id":7,"questionnaire_id":1} self_contained: true -> false',
    'Update questionnaire_question {"component_id":7,"questionnaire_id":1} parent_question_id: - -> 4'
  ))
})

test_that("a question's condition and its number in a wave are changed one by one", {
  form <- form_for("questionnaire_question", "update", "1|5",
    scope = "1",
    questionnaire_question__condition = "If yes", questionnaire_question__qn_2 = "2b"
  )
  lines <- item_lines(form_items(form)$items)
  expect_equal(lines, c(
    paste0(
      'Update questionnaire_question {"component_id":5,"questionnaire_id":1} condition: If you ',
      "answered Yes to the previous question -> If yes"
    ),
    'Update wave_question {"component_id":5,"questionnaire_id":1,"wave_id":2} question_number: 3 -> 2b'
  ))
})

test_that("each wave that asked a question has its own number to change", {
  form <- form_for("questionnaire_question", "update", "1|4", scope = "1")
  ids <- vapply(form$fields, function(field) field$id, character(1))
  expect_true(all(c("questionnaire_question__qn_1", "questionnaire_question__qn_2") %in% ids))
  labels <- vapply(Filter(function(field) startsWith(field$name, "qn_"), form$fields), function(field) field$label, character(1))
  expect_equal(labels, c("Question number in Wave 1", "Question number in Wave 2"))
})

test_that("a question number that another question in the wave has is a problem", {
  form <- form_for("questionnaire_question", "update", "1|5", scope = "1", questionnaire_question__qn_2 = "2")
  problem <- edit_service_check(form$fields, form$values)$problems$questionnaire_question__qn_2
  expect_match(problem, "Question 2 in AHP Wave 2 is already “Do you now smoke cigarettes\\?”")
  # its own number isn't a clash
  form <- form_for("questionnaire_question", "update", "1|5", scope = "1")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
  # nor is the same number in another questionnaire's wave
  form <- form_for("questionnaire_question", "update", "2|9", scope = "2", questionnaire_question__qn_3 = "2")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
})

test_that("changing a question's wording and its number together changes the new question's number", {
  form <- form_for("questionnaire_question", "update", "4|11",
    scope = "4",
    questionnaire_question__text = "Did you smoke while pregnant?", questionnaire_question__qn_5 = "1a"
  )
  lines <- item_lines(form_items(form)$items)
  expect_equal(lines[1:3], c(
    'Insert component {"component_id":"new:component:1"} component_type_id: - -> 2',
    'Insert component {"component_id":"new:component:1"} text: - -> Did you smoke while pregnant?',
    'Insert question {"component_id":"new:component:1"} component_id: - -> new:component:1'
  ))
  expect_true(paste0(
    'Update wave_question {"component_id":"new:component:1","questionnaire_id":4,"wave_id":5} ',
    "question_number: 1 -> 1a"
  ) %in% lines)
})

test_that("a question's new questionnaire-only wording keeps its MeSH categories but not its scale membership", {
  form <- form_for(
    "questionnaire_question",
    "update",
    "2|9",
    scope = "2",
    questionnaire_question__text = "How often this week did you feel sad?"
  )
  lines <- item_lines(form_items(form)$items)
  expect_true('Insert question_category {"component_id":"new:component:1","mesh_id":"D003863"} mesh_id: - -> D003863' %in% lines)
  expect_false(any(grepl("question_scale", lines)))
})

# ------ Proposals ----------------------------------------------------------------------------------------------------------------
test_that("a proposal records who, what, why and how to open it again", {
  built <- form_items(form_for("registry", "update", "1", registry__name = "MATR"))
  form <- list(action = "update", entity = "registry", scope = "", target = "1", values = list(registry__name = "MATR"))
  now <- as.POSIXct("2026-10-02 15:30:12", tz = "UTC")
  record <- edit_service_record(fixture_staff, form, built, "Draft", now = now)

  expect_null(record$id) # the database numbers it when it's first saved
  expect_equal(record$status, "Draft")
  expect_equal(record$prepared_by, "Maggie Kohn")
  expect_equal(record$prepared_by_id, 1L)
  expect_null(record$note) # no reason is asked for or kept
  expect_null(record$expected_revision) # a new proposal has no revision to check against
  expect_null(record$submitted) # the database records when it is sent for review
  expect_equal(record$summary, "Changed the name of the registry “Mid-Atlantic Twin Registry”")
  expect_equal(nrow(record$items), 1)

  record$id <- "5" # as saved
  record$revision <- 3L
  later <- edit_service_record(fixture_other_staff, form, built, "Awaiting Review", previous = record, now = now + 60)
  expect_equal(later$id, record$id)
  expect_equal(later$prepared_by, "Maggie Kohn")
  expect_equal(later$prepared_by_id, 1L)
  expect_equal(later$started, record$started)
  expect_equal(later$expected_revision, 3L) # the one the person was looking at, which the database checks
  expect_equal(later$status, "Awaiting Review")
})

test_that("another open proposal that changes the same row is a conflict", {
  sam <- list(staff_id = 9L, name = "Sam")
  saved_as <- function(record, id) utils::modifyList(record, list(id = id))
  mine <- form_items(form_for("registry", "update", "1", registry__name = "A"))$items
  theirs <- saved_as(
    edit_service_record(sam, list(), form_items(form_for("registry", "update", "1", registry__abbreviation = "X")), "Awaiting Review"),
    "10"
  )
  other_row <- saved_as(
    edit_service_record(sam, list(), form_items(form_for("registry", "update", "2", registry__name = "Z")), "Draft"),
    "11"
  )
  withdrawn <- utils::modifyList(theirs, list(id = "12", status = "Withdrawn"))
  applied <- utils::modifyList(theirs, list(id = "13", status = "Applied"))

  conflicts <- edit_service_conflicts(list(theirs, other_row, withdrawn, applied), mine)
  expect_length(conflicts, 1)
  expect_equal(conflicts[[1]]$prepared_by, "Sam")
  # saving your own again isn't a conflict with itself
  expect_length(edit_service_conflicts(list(theirs), mine, except = theirs$id), 0)
})

test_that("the changes to a swapped-in question come before the old rows are removed", {
  form <- form_for("questionnaire_question", "update", "1|5",
    scope = "1",
    questionnaire_question__text = "On a typical day, how many cigarettes do you smoke?",
    questionnaire_question__self_contained = "TRUE", questionnaire_question__qn_2 = "3b"
  )
  built <- form_items(form)
  actions <- built$items$action
  last_new <- max(which(actions != "Delete"))
  first_delete <- min(which(actions == "Delete"))
  expect_lt(last_new, first_delete)
  expect_true(all(actions[first_delete:length(actions)] == "Delete"))
  expect_equal(
    item_lines(built$items)[(first_delete - 3):(first_delete - 1)],
    c(
      'Update questionnaire_question {"component_id":"new:component:1","questionnaire_id":1} self_contained: false -> true',
      'Update questionnaire_question {"component_id":"new:component:1","questionnaire_id":1} parent_question_id: 4 -> -',
      'Update wave_question {"component_id":"new:component:1","questionnaire_id":1,"wave_id":2} question_number: 3 -> 3b'
    )
  )
  expect_equal(
    built$summary,
    paste0(
      "Changed the wording, whether it stands on its own, the question it follows up on and question ",
      "number in Wave 2 of the questionnaire question “AHP · How many cigarettes do you smoke on a ",
      "typical day?”"
    )
  )
})

# ------ Adding one question to a questionnaire -----------------------------------------------------------------
variables_json <- function(...) as.character(jsonlite::toJSON(list(...), auto_unbox = TRUE))
yes_no_variable <- function(matr_id = "AH05EX", choices = list(list(code_value = "1", answer_option = "Yes"), list(code_value = "0", answer_option = "No"))) {
  list(variable_type = "closed_ended", matr_id = matr_id, subject = "Self", label = "", response_options = choices)
}

test_that("a question can be added to a questionnaire, whole: no sentence says to go to Extraction", {
  entity <- edit_entities$questionnaire_question
  snap <- test_snapshot(edit_service_tables(entity))
  expect_null(edit_service_can(entity, snap, "add"))
  expect_null(entity$redirect)
  expect_match(entity$about, "A question as it's printed in one questionnaire")
  expect_true(all(c("subject", "answer_option", "option_set", "answer_option_set") %in% edit_service_tables(entity)))
  form <- form_for("questionnaire_question", "add", scope = "1")
  names <- vapply(form$fields, function(field) field$name, character(1))
  expect_true(all(c("source", "text", "component_id", "parent_component_id", "display_order", "self_contained",
                    "parent_question_id", "condition", "qn_1", "qn_2", "variables") %in% names))
  # an existing question can be picked, but not one the questionnaire asks already
  existing <- Filter(function(field) field$name == "component_id", form$fields)[[1]]
  expect_true("11" %in% existing$choices) # Mother Questionnaire's pregnancy question
  expect_false(any(c("3", "4", "5", "7") %in% existing$choices))
})

test_that("a new question is added with its place, its number in each wave that asks it, and its variables", {
  form <- form_for(
    "questionnaire_question", "add", scope = "1",
    questionnaire_question__text = "Do you exercise?",
    questionnaire_question__parent_component_id = "6", questionnaire_question__display_order = "1",
    questionnaire_question__qn_2 = "1a4",
    questionnaire_question__variables = variables_json(
      yes_no_variable(),
      list(variable_type = "open_ended", matr_id = "AH05HR", subject = "Self", label = "Hours a week", answer_data_type = "NUMBER")
    )
  )
  found <- edit_service_check(form$fields, form$values)
  expect_length(found$missing, 0)
  expect_length(found$problems, 0)
  built <- form_items(form)
  lines <- item_lines(built$items)
  expect_match(built$summary, "^Added the question “Do you exercise\\?” to AHP$")
  expect_true('Insert component {"component_id":"new:component:1"} text: - -> Do you exercise?' %in% lines)
  expect_true('Insert question {"component_id":"new:component:1"} component_id: - -> new:component:1' %in% lines)
  # it goes first in section B, and what was there moves over
  expect_true('Update questionnaire_component {"component_id":7,"questionnaire_id":1} display_order: 1 -> 2' %in% lines)
  expect_true('Insert questionnaire_component {"component_id":"new:component:1","questionnaire_id":1} parent_component_id: - -> 6' %in% lines)
  expect_true('Insert questionnaire_question {"component_id":"new:component:1","questionnaire_id":1} self_contained: - -> true' %in% lines)
  # asked only in the wave it has a number in
  waves <- built$items[built$items$entity_name == "wave_question", ]
  expect_equal(nrow(waves), 1)
  expect_match(waves$record_identifier, '"wave_id":2')
  expect_equal(waves$new_value, "1a4")
  # its variables, as an extraction adds them: Yes and No are used again, and so is the set they make
  expect_true('Insert variable {"variable_key":"new:variable:1"} matr_id: - -> AH05EX' %in% lines)
  expect_true('Insert closed_ended {"variable_key":"new:variable:1"} option_set_id: - -> 1' %in% lines)
  expect_true('Insert open_ended {"variable_key":"new:variable:2"} answer_data_type: - -> NUMBER' %in% lines)
  expect_false(any(built$items$entity_name %in% c("answer_option", "option_set")))
  expect_equal(sum(built$items$entity_name == "response_option" & built$items$attribute_name == "code_value"), 2)
  # the rows are added before what points at them
  order_of <- function(table) min(which(built$items$entity_name == table))
  expect_lt(order_of("component"), order_of("questionnaire_component"))
  expect_lt(order_of("questionnaire_question"), order_of("wave_question"))
  expect_lt(order_of("questionnaire_question"), order_of("variable"))
})

test_that("a lone check box is a variable with one choice, and a new answer choice makes a new set", {
  form <- form_for(
    "questionnaire_question", "add", scope = "1",
    questionnaire_question__text = "All of my life", questionnaire_question__qn_1 = "9",
    questionnaire_question__variables = variables_json(yes_no_variable(choices = list(list(code_value = "1", answer_option = "Always"))))
  )
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
  lines <- item_lines(form_items(form)$items)
  expect_true('Insert answer_option {"answer_option_id":"new:answer_option:1"} standardized_label: - -> Always' %in% lines)
  expect_true('Insert option_set {"option_set_id":"new:option_set:1"} name: - -> Always' %in% lines)
})

test_that("a question already in the database can be added, keeping its wording shared", {
  form <- form_for(
    "questionnaire_question", "add", scope = "1",
    questionnaire_question__source = "existing", questionnaire_question__component_id = "11",
    questionnaire_question__self_contained = "FALSE", questionnaire_question__parent_question_id = "4",
    questionnaire_question__condition = "Women only", questionnaire_question__qn_1 = "2b",
    questionnaire_question__variables = variables_json(yes_no_variable())
  )
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
  built <- form_items(form)
  lines <- item_lines(built$items)
  expect_false("component" %in% built$items$entity_name) # the wording is the one that is there
  expect_true('Insert questionnaire_component {"component_id":11,"questionnaire_id":1} display_order: - -> 3' %in% lines)
  expect_true('Insert questionnaire_question {"component_id":11,"questionnaire_id":1} parent_question_id: - -> 4' %in% lines)
  expect_true('Insert questionnaire_question {"component_id":11,"questionnaire_id":1} condition: - -> Women only' %in% lines)
  expect_match(built$summary, "Did you smoke during your pregnancy")
})

test_that("what stops a question being added is said under the field at fault", {
  check <- function(...) {
    form <- form_for("questionnaire_question", "add", scope = "1", questionnaire_question__text = "Do you exercise?", ...)
    edit_service_check(form$fields, form$values)
  }
  problems <- function(...) check(...)$problems
  # no wave number: a question is asked in at least one wave
  expect_match(problems(questionnaire_question__variables = variables_json(yes_no_variable()))$questionnaire_question__qn_1, "at least one wave")
  # a number the wave has already
  expect_match(problems(questionnaire_question__qn_1 = "2", questionnaire_question__variables = variables_json(yes_no_variable()))$questionnaire_question__qn_1, "already “Do you now smoke cigarettes")
  variables_problem <- function(...) problems(questionnaire_question__qn_1 = "9", questionnaire_question__variables = variables_json(...))$questionnaire_question__variables
  expect_match(variables_problem(yes_no_variable(matr_id = "")), "Variable 1: fill in its MATR ID")
  expect_match(variables_problem(yes_no_variable(matr_id = "AH01DB")), "AH01DB is already a MATR ID in this questionnaire")
  expect_match(variables_problem(yes_no_variable(), yes_no_variable()), "Variable 2: give it a MATR ID no other variable has")
  expect_match(variables_problem(yes_no_variable(choices = list())), "add its choices")
  expect_match(variables_problem(yes_no_variable(choices = list(list(code_value = "1", answer_option = "")))), "blank choice")
  expect_match(variables_problem(yes_no_variable(choices = list(list(code_value = "1", answer_option = "Yes"), list(code_value = "1", answer_option = "No")))), "its own code")
  expect_match(variables_problem(list(variable_type = "open_ended", matr_id = "X1", subject = "Self")), "kind of answer")
  expect_match(variables_problem(list(variable_type = "open_ended", matr_id = "X1", subject = "Nobody", answer_data_type = "TEXT")), "who it's about")
  expect_match(problems(questionnaire_question__qn_1 = "9", questionnaire_question__variables = "[]")$questionnaire_question__variables, "at least one variable")
  expect_match(problems(questionnaire_question__qn_1 = "9", questionnaire_question__variables = "not json")$questionnaire_question__variables, "couldn't be read")
  expect_null(variables_problem(yes_no_variable()))
  # a questionnaire with no waves can't have a question yet
  no_waves <- form_for("questionnaire_question", "add", scope = "1", snap = test_snapshot(
    edit_service_tables(edit_entities$questionnaire_question),
    change = function(snap) {
      snap$wave <- snap$wave[snap$wave$questionnaire_id != 1, ]
      snap
    }
  ))
  expect_match(edit_service_check(no_waves$fields, no_waves$values)$problems$questionnaire_question__qn_none, "no waves yet")
})

test_that("the variables start as one, about the respondent, with one blank choice", {
  snap <- test_snapshot(c("subject"))
  variables <- edit_variables_parse(edit_variables_default(snap))
  expect_length(variables, 1)
  expect_equal(variables[[1]]$subject, "Self")
  expect_length(variables[[1]]$response_options, 1)
  html <- htmltools::renderTags(edit_field_ui(
    shiny::NS("change"), Filter(function(field) field$name == "variables", form_for("questionnaire_question", "add", scope = "1")$fields)[[1]] |>
      utils::modifyList(list(id = "questionnaire_question__variables", part = "questionnaire_question")),
    edit_variables_default(snap), "change-note_1"
  ))$html
  expect_match(html, 'class="edit-variables"')
  expect_match(html, 'data-input="change-questionnaire_question__variables"')
  expect_match(html, 'data-subjects="\\[&quot;Respondent')
  expect_match(html, "edit-variables-add")
})
