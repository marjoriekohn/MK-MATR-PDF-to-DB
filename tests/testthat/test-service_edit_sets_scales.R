# The Option Set, Measurement Scale, Scale Item and PDF File kinds of Make an edit. In the sample tables, option set 1
# (Yes / No) and 2 (Often / Sometimes / Rarely / Never) are both offered by variables; measurement scale 1 (SMS) has
# one item, question 9, which expects set 2; one PDF was uploaded twice, under two names, and another for no draft.

# A sample database with a third option set, Agree / Disagree, that no variable offers.
with_unused_set <- function(snap) {
  snap$answer_option <- rbind(snap$answer_option, data.frame(answer_option_id = c(40, 41), standardized_label = c("Agree", "Disagree")))
  snap$option_set <- rbind(snap$option_set, data.frame(option_set_id = 3, name = "Agree / Disagree", description = NA_character_))
  snap$answer_option_set <- rbind(snap$answer_option_set, data.frame(option_set_id = c(3, 3), answer_option_id = c(40, 41)))
  snap
}
unused_set_snap <- function() with_unused_set(test_snapshot(edit_service_tables(edit_entities$option_set)))

# ------ A field of several choices -----------------------------------------------------------------
test_that("a field of several choices holds their values joined by commas, in order, once each", {
  field <- edit_field("ids", "Answer choices", "several", choices = c(Yes = "1", No = "2", Often = "13"))
  expect_equal(edit_service_normalize(field, "13, 2,1,2"), "1,2,13")
  expect_true(is.na(edit_service_normalize(field, "")))
  expect_equal(edit_several_text(c("13", "1")), "1,13") # as the page sends it
  expect_equal(edit_several_text(NULL), "")
  expect_equal(edit_display_value(field, "1,13"), "Yes / Often")
  expect_equal(edit_type_problem(field, "1,99"), "Pick from the list.")
  expect_null(edit_type_problem(field, "1,13"))
})

# ------ Option sets --------------------------------------------------------------------------------
test_that("every option set is a card, with its choices and how many variables offer it", {
  entity <- edit_entities$option_set
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  expect_equal(cards$title, c("Often / Sometimes / Rarely / Never", "Yes / No"))
  expect_equal(cards$subtitle, c("Often / Sometimes / Rarely / Never", "Yes / No"))
  expect_equal(cards$facts[[1]], c("offered by 3 variables", "expected by 1 scale item"))
  expect_equal(cards$facts[[2]], "offered by 3 variables")
})

test_that("a new option set needs a name not taken and some choices, and says if a set has them already", {
  empty <- form_for("option_set", "add")
  expect_equal(edit_service_check(empty$fields, empty$values)$missing, c("name", "answer choices"))

  taken <- form_for("option_set", "add", option_set__name = " yes / NO ", option_set__answer_option_ids = "1")
  expect_equal(edit_service_check(taken$fields, taken$values)$problems$option_set__name, "There's already an option set called “Yes / No”.")

  same <- form_for("option_set", "add", option_set__name = "Yes or no", option_set__answer_option_ids = "2,1")
  found <- edit_service_check(same$fields, same$values)
  expect_length(found$problems, 0)
  expect_match(found$warnings$option_set__answer_option_ids, "“Yes / No” already has exactly these choices")
})

test_that("adding an option set adds it and its choices, which point at it before it has an ID", {
  form <- form_for("option_set", "add", option_set__name = "Agreement", option_set__answer_option_ids = "13,1")
  expect_equal(item_lines(form_items(form)$items), c(
    "Insert option_set {\"option_set_id\":\"new:option_set:1\"} name: - -> Agreement",
    "Insert answer_option_set {\"answer_option_id\":1,\"option_set_id\":\"new:option_set:1\"} NA: - -> -",
    "Insert answer_option_set {\"answer_option_id\":13,\"option_set_id\":\"new:option_set:1\"} NA: - -> -"
  ))
  expect_equal(form_items(form)$summary, "Added the option set “Agreement”")
})

test_that("a set variables offer keeps its choices, but can be renamed", {
  form <- form_for("option_set", "update", "1", option_set__name = "Yes or No")
  choices <- Filter(function(field) field$name == "answer_option_ids", form$fields)[[1]]
  expect_match(choices$locked, "^3 variables offer exactly these choices")
  expect_equal(form$initial$option_set__answer_option_ids, "1,2")
  expect_equal(item_lines(form_items(form)$items), "Update option_set {\"option_set_id\":1} name: Yes / No -> Yes or No")
})

test_that("a set no variable offers can change its choices: the ones taken out go, the new ones are added", {
  form <- form_for("option_set", "update", "3", snap = unused_set_snap(), option_set__answer_option_ids = "41,1")
  expect_null(Filter(function(field) field$name == "answer_option_ids", form$fields)[[1]]$locked)
  expect_equal(item_lines(form_items(form)$items), c(
    "Delete answer_option_set {\"answer_option_id\":40,\"option_set_id\":3} NA: - -> -",
    "Insert answer_option_set {\"answer_option_id\":1,\"option_set_id\":3} NA: - -> -"
  ))
  expect_equal(form_items(form)$summary, "Changed the choices of the option set “Agree / Disagree”")
})

test_that("a set variables offer can't be deleted", {
  entity <- edit_entities$option_set
  snap <- test_snapshot(edit_service_tables(entity))
  plan <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "1"))
  expect_length(plan$blocked, 1)
  expect_match(plan$blocked[[1]]$text, "^3 closed-ended variables are in the way")
})

test_that("deleting a set no variable offers takes its choices out of it, and scale items stop expecting it", {
  entity <- edit_entities$option_set
  snap <- unused_set_snap()
  snap$question_scale$expected_option_set_id <- 3
  plan <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "3"))
  expect_length(plan$blocked, 0)
  expect_equal(item_lines(edit_service_build(entity, "delete", snap, edit_service_row(entity, snap, "3"), plan = plan)$items), c(
    "Update question_scale {\"component_id\":9,\"scale_id\":1} expected_option_set_id: 3 -> -",
    "Delete answer_option_set {\"answer_option_id\":40,\"option_set_id\":3} NA: - -> -",
    "Delete answer_option_set {\"answer_option_id\":41,\"option_set_id\":3} NA: - -> -",
    "Delete option_set {\"option_set_id\":3} name: Agree / Disagree -> -",
    "Delete option_set {\"option_set_id\":3} description: - -> -"
  ))
})

test_that("an answer choice in a set says which set it's in when it can't be deleted", {
  entity <- edit_entities$answer_option
  snap <- with_unused_set(test_snapshot(edit_service_tables(entity)))
  plan <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "40"))
  expect_equal(plan$blocked[[1]]$labels, "“Agree” in “Agree / Disagree”")
  expect_match(plan$blocked[[1]]$fix, "Option Sets, then Edit")
})

# ------ Measurement scales and their items ------------------------------------------------------------
test_that("a measurement scale's card counts its items, and its name and abbreviation are its own", {
  entity <- edit_entities$measurement_scale
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  expect_equal(cards$title, "Short Mood Scale")
  expect_equal(cards$subtitle, "SMS")
  expect_equal(cards$facts[[1]], "1 item")

  form <- form_for("measurement_scale", "add", measurement_scale__name = "short mood scale", measurement_scale__abbreviation = "sms")
  problems <- edit_service_check(form$fields, form$values)$problems
  expect_equal(problems$measurement_scale__name, "“Short Mood Scale” already has this name.")
  expect_equal(problems$measurement_scale__abbreviation, "“Short Mood Scale” already has this abbreviation.")
})

test_that("deleting a measurement scale takes its items, and leaves the questions", {
  entity <- edit_entities$measurement_scale
  snap <- test_snapshot(edit_service_tables(entity))
  plan <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "1"))
  expect_length(plan$blocked, 0)
  expect_equal(plan$cascade$removed$question_scale, "1|9")
  expect_null(plan$cascade$removed$component)
})

test_that("a scale's items are listed in it by their number, with the choices they expect", {
  entity <- edit_entities$question_scale
  snap <- test_snapshot(edit_service_tables(entity))
  cards <- edit_service_cards(entity, snap, "1")
  expect_equal(cards$key, "1|9")
  expect_equal(cards$title, "How often in the past week did you feel sad?")
  expect_equal(cards$subtitle, "Item 1")
  expect_equal(cards$facts[[1]], "expects “Often / Sometimes / Rarely / Never”")
  expect_equal(nrow(edit_service_cards(entity, snap, "")), 0)
  expect_equal(edit_entities$question_scale$scope$table, "measurement_scale")
})

test_that("an item's question and number are each in the scale once", {
  again <- form_for("question_scale", "add", scope = "1", question_scale__component_id = "9", question_scale__item_number = " 1 ")
  problems <- edit_service_check(again$fields, again$values)$problems
  expect_equal(problems$question_scale__component_id, "This question is already item 1 of this scale.")
  expect_equal(problems$question_scale__item_number, "Item 1 of this scale is already “How often in the past week did you feel sad?”.")

  # a question can only be picked from the questions, not other blocks
  choices <- Filter(function(field) field$name == "component_id", again$fields)[[1]]$choices
  expect_setequal(unname(choices), c("", "3", "4", "5", "7", "9", "11", "14"))
})

test_that("adding an item to a scale is one row, named by its scale and question", {
  form <- form_for(
    "question_scale", "add", scope = "1",
    question_scale__component_id = "7", question_scale__item_number = "2a", question_scale__expected_option_set_id = "2"
  )
  built <- form_items(form)
  expect_equal(item_lines(built$items), c(
    "Insert question_scale {\"component_id\":7,\"scale_id\":1} item_number: - -> 2a",
    "Insert question_scale {\"component_id\":7,\"scale_id\":1} expected_option_set_id: - -> 2"
  ))
  expect_equal(built$summary, "Added “How often do you see a doctor?” to the measurement scale “Short Mood Scale” as item 2a")
})

test_that("an item keeps its question, and its number and expected choices can change", {
  form <- form_for("question_scale", "update", "1|9", scope = "1", question_scale__item_number = "3", question_scale__expected_option_set_id = "")
  expect_match(Filter(function(field) field$name == "component_id", form$fields)[[1]]$locked, "^An item is its question")
  expect_equal(item_lines(form_items(form)$items), c(
    "Update question_scale {\"component_id\":9,\"scale_id\":1} item_number: 1 -> 3",
    "Update question_scale {\"component_id\":9,\"scale_id\":1} expected_option_set_id: 2 -> -"
  ))
})

# ------ PDFs ----------------------------------------------------------------------------------------------
test_that("a PDF is listed by the names it was uploaded as, with its pages and size", {
  entity <- edit_entities$pdf_file
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  expect_equal(cards$title, c("AHP copy.pdf / AHP_Redacted.pdf", "PDF b2b2b2b2b2b2"))
  expect_equal(cards$subtitle, c("12 pages · 2.1 MB", "640 KB"))
  expect_equal(cards$facts[[1]], "read for 2 extractions")
  expect_equal(cards$facts[[2]], "read for no extractions")
})

test_that("a PDF is only shown: it can't be added, changed or deleted here, and says why", {
  entity <- edit_entities$pdf_file
  snap <- test_snapshot(edit_service_tables(entity))
  expect_true(entity$view_only)
  expect_match(edit_service_can(entity, snap, "add"), "Extraction page")
  expect_match(edit_service_can(entity, snap, "update"), "facts about the file")
  expect_match(edit_service_can(entity, snap, "delete"), "record of what extractions were read from")
})

test_that("a PDF's details are its names, pages, size and fingerprint, and the extractions read from it", {
  form <- form_for("pdf_file", "update", strrep("a1", 32))
  expect_equal(form$initial$pdf_file__pdf_name, "AHP copy.pdf / AHP_Redacted.pdf")
  expect_equal(form$initial$pdf_file__pdf_pages, "12")
  expect_equal(form$initial$pdf_file__pdf_bytes, "2.1 MB")
  expect_equal(form$initial$pdf_file__pdf_sha256, strrep("a1", 32))
  usage <- edit_service_usage(form$entity, form$snap, form$row)
  expect_equal(usage$pointing[[1]]$text, "2 extractions")
  expect_equal(usage$pointing[[1]]$examples, c("AHP_Redacted.pdf (Applied)", "AHP copy.pdf (Draft)"))
})

test_that("the new kinds are sections of Make an edit's list", {
  expect_equal(make_edit_kinds$option_set, c("Option Sets", "Option Set"))
  expect_equal(make_edit_kinds$measurement_scale, c("Measurement Scales", "Measurement Scale"))
  expect_equal(make_edit_kinds$question_scale, c("Scale Items", "Scale Item"))
  expect_equal(make_edit_kinds$pdf_file, c("PDF Files", "PDF File"))
})

test_that("a new option set with no answer choices to pick from says to add one first", {
  entity <- edit_entities$option_set
  snap <- test_snapshot(edit_service_tables(entity))
  snap$answer_option <- snap$answer_option[0, , drop = FALSE]
  notes <- htmltools::renderTags(htmltools::tagList(edit_empty_choices_ui(edit_service_fields(entity, snap, "add"))))$html
  expect_match(notes, "Nothing to pick for “Answer choices” yet")
  expect_match(notes, 'data-kind="answer_option"')
})

test_that("a scale item with no questions to pick from says to add one first", {
  entity <- edit_entities$question_scale
  snap <- test_snapshot(edit_service_tables(entity))
  snap$question <- snap$question[0, , drop = FALSE]
  notes <- htmltools::renderTags(htmltools::tagList(edit_empty_choices_ui(edit_service_fields(entity, snap, "add", NULL, "1"))))$html
  expect_match(notes, "Nothing to pick for “Question” yet")
  expect_match(notes, 'data-kind="question"')
})
