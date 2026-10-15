# Adding something that is already there is a warning, not a block: the form shows the rows it looks like, ignoring
# capitals and extra spaces. What counts as the same is each kind's own rule.

found_for <- function(key, ...) {
  form <- form_for(key, "add", ...)
  edit_service_duplicates(form$entity, form$snap, form$fields, form$values)
}

titles <- function(found) unlist(lapply(found, function(match) match$cards$title))

test_that("a registry with the name of one that is there is flagged, whatever its capitals and spaces", {
  found <- found_for("registry", registry__name = "  mid-atlantic  TWIN registry ")
  expect_length(found, 1)
  expect_equal(found[[1]]$why, "A registry with this name")
  expect_equal(found[[1]]$cards$title, "Mid-Atlantic Twin Registry")
  expect_equal(found[[1]]$entity$key, "registry")
  expect_length(found_for("registry", registry__name = "A registry nobody has"), 0)
  expect_length(found_for("registry"), 0) # nothing typed, nothing to compare
})

test_that("a respondent type is the same when its age range is, and only when an age is filled in", {
  # Adult and Parent are both 18 and over
  found <- found_for("respondent_type", respondent_type__name = "Anything", respondent_type__minimum_age = "18")
  expect_equal(found[[1]]$why, "A respondent type with the same age range")
  expect_setequal(titles(found), c("Adult (18+)", "Parent (18+)"))
  expect_equal(titles(found_for("respondent_type", respondent_type__minimum_age = "5", respondent_type__maximum_age = "17")), "Child (5-17)")
  expect_length(found_for("respondent_type", respondent_type__minimum_age = "5", respondent_type__maximum_age = "16"), 0) # not the same range
  # no age at all: two such types are told apart by their names, so nothing is flagged
  expect_length(found_for("respondent_type", respondent_type__name = "Adult"), 0)
  # the oldest age on its own counts too
  expect_length(found_for("respondent_type", respondent_type__maximum_age = "17"), 0)
})

test_that("an investigator is flagged by name or by email, either alone", {
  by_name <- found_for("investigator", investigator__name = "dana reyes", investigator__email = "other@vcu.edu")
  expect_equal(vapply(by_name, function(match) match$why, character(1)), "An investigator with this name")
  email <- fixture_table("principal_investigator")$email[1]
  by_email <- found_for("investigator", investigator__name = "Someone New", investigator__email = toupper(email))
  expect_equal(vapply(by_email, function(match) match$why, character(1)), "An investigator with this email")
  both <- found_for("investigator", investigator__name = "Dana Reyes", investigator__email = email)
  expect_length(both, 2)
})

test_that("a study is the same only within its registry, a questionnaire only within its study", {
  studies <- fixture_table("study")
  name <- studies$name[studies$study_id == 1]
  registry <- studies$registry_id[studies$study_id == 1]
  other_registry <- setdiff(fixture_table("registry")$registry_id, registry)[1]
  expect_equal(titles(found_for("study", study__name = toupper(name), study__registry_id = as.character(registry))), name)
  expect_length(found_for("study", study__name = name, study__registry_id = as.character(other_registry)), 0)
  expect_equal(titles(found_for("study", study__name = name)), name) # no registry chosen yet: anywhere

  questionnaires <- fixture_table("questionnaire")
  first <- questionnaires[1, ]
  in_study <- found_for("questionnaire", questionnaire__name = first$name, questionnaire__abbreviation = first$abbreviation, questionnaire__study_id = as.character(first$study_id))
  expect_equal(vapply(in_study, function(match) match$why, character(1)), c("A questionnaire with this name in this study", "A questionnaire with this abbreviation in this study"))
  other_study <- setdiff(fixture_table("study")$study_id, first$study_id)[1]
  expect_length(found_for("questionnaire", questionnaire__name = first$name, questionnaire__study_id = as.character(other_study)), 0)
})

test_that("a block or a question with exactly these words is flagged", {
  components <- fixture_table("component")
  words <- components$text[!is.na(components$text)][1]
  found <- found_for("component", component__text = words)
  expect_equal(found[[1]]$why, "A block with exactly these words")
  question_ids <- fixture_table("question")$component_id
  question_words <- components$text[components$component_id %in% question_ids][1]
  expect_equal(found_for("question", question__text = question_words)[[1]]$why, "A question with exactly these words")
  # a block's words that are only used by a section aren't a duplicate question
  expect_length(found_for("question", question__text = "Words nobody uses"), 0)
})

test_that("a kind with no rule has nothing to say, and a study is checked by its own fields", {
  expect_length(found_for("wave", wave__name = "Wave 1"), 0) # a wave's name is checked by its own field
  form <- form_for("study", "add", study__name = "Adult Health Study", study__registry_id = "1")
  found <- edit_service_duplicates(form$entity, form$snap, form$fields, form$values)
  expect_true("A study with this name" %in% vapply(found, function(match) match$why, character(1)))
})

test_that("what is there is shown the way a card shows it, in the warning", {
  found <- found_for("registry", registry__name = "Example Registry")
  html <- htmltools::renderTags(edit_duplicates_inline(found))$html
  expect_match(html, "This looks like something that&#39;s already there|This looks like something that's already there")
  expect_match(html, "A registry with this name")
  expect_match(html, "Example Registry")
  dialog <- text_of(edit_duplicates_dialog(found, "confirm"))
  expect_match(dialog, "Add it anyway")
  expect_match(dialog, "Go back")
})

test_that("the rows an add looks like are found by a column, ignoring case and spaces at either end", {
  snap <- test_snapshot("registry")
  found <- edit_duplicate_rows(snap, "registry", "name", " example REGISTRY", "Same name")
  expect_equal(found$why, "Same name")
  expect_equal(found$rows$registry_id, 2)
  expect_null(edit_duplicate_rows(snap, "registry", "name", "   ", "Same name"))
  expect_null(edit_duplicate_rows(snap, "registry", "name", "No such registry", "Same name"))
})
