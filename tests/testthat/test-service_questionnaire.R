# What the extraction pages know about the live tables: the catalog is read from the real database, and the
# drop-down choices are made of it.

# test_that("the catalog joins each study's investigator and each questionnaire's respondents and waves", {
#   tree <- local_tree()
#   local_wave(tree$questionnaire)
#   catalog <- questionnaire_service_get_catalog(test_connection)
#   expect_named(catalog, c("registries", "studies", "questionnaires", "waves", "respondent_types"))
#
#   study <- questionnaire_service_get_study(catalog, tree$study$study_id)
#   expect_equal(study$principal_investigator, tree$investigator$name)
#   questionnaire <- questionnaire_service_get(catalog, tree$questionnaire$questionnaire_id)
#   expect_equal(questionnaire$respondent_type, tree$respondent_type$name)
#   expect_equal(questionnaire$n_waves, 2)
#   expect_true(tree$registry$name %in% catalog$registries$name)
#   expect_true(tree$respondent_type$respondent_type_id %in% catalog$respondent_types$respondent_type_id)
# })
#
# test_that("a catalog can be read again, and then has what was added", {
#   before <- questionnaire_service_get_catalog(test_connection)
#   tree <- local_tree()
#   after <- questionnaire_service_get_catalog(test_connection)
#   expect_false(tree$study$study_id %in% before$studies$study_id)
#   expect_true(tree$study$study_id %in% after$studies$study_id)
# })

test_that("a database with only registries in it still makes a catalog with empty lists", {
  empty <- function(columns) as.data.frame(stats::setNames(rep(list(logical()), length(columns)), columns))
  catalog <- questionnaire_service_catalog(list(
    registries = data.frame(registry_id = 1L, name = "Registry A", abbreviation = "RA"),
    studies = empty(c("study_id", "registry_id", "principal_investigator_id", "name")),
    investigators = empty(c("principal_investigator_id", "name")),
    questionnaires = empty(c("questionnaire_id", "study_id", "respondent_type_id", "name", "abbreviation")),
    waves = empty(c("wave_id", "questionnaire_id", "name", "administered_date", "respondent_count")),
    respondent_types = empty(c("respondent_type_id", "name", "minimum_age", "maximum_age"))
  ))
  expect_equal(questionnaire_service_registry_choices(catalog), c("Registry A" = "1"))
  expect_length(questionnaire_service_study_choices(catalog, "1"), 0)
  expect_length(questionnaire_service_respondent_type_choices(catalog), 0)
  expect_length(questionnaire_service_choices(catalog, "1"), 0)
})

# The drop-down choices are functions of the catalog, so they are tested on the sample tables.
test_that("a registry lists only its studies, each with who leads it", {
  catalog <- test_catalog()
  expect_equal(
    questionnaire_service_registry_choices(catalog),
    c("Example Registry" = "2", "Mid-Atlantic Twin Registry" = "1")
  )
  expect_equal(
    questionnaire_service_study_choices(catalog, "1"),
    c("Adult Health Study (Dana Reyes)" = "1", "Child Development Study (Sam Patel)" = "2")
  )
  expect_equal(questionnaire_service_study_choices(catalog, "2"), c("Example Study (Lee Morgan)" = "3"))
  expect_length(questionnaire_service_study_choices(catalog, ""), 0)
})

test_that("respondent types show their ages, or that they have none", {
  expect_equal(
    questionnaire_service_respondent_type_choices(test_catalog()),
    c("Adult (18+)" = "1", "Child (5-17)" = "2", "Parent (18+)" = "3")
  )
  catalog <- test_catalog()
  catalog$respondent_types <- rbind(
    catalog$respondent_types,
    data.frame(respondent_type_id = 9, name = "Parent of Twins", minimum_age = NA, maximum_age = NA)
  )
  expect_equal(unname(questionnaire_service_respondent_type_choices(catalog)[4]), "9")
  expect_equal(names(questionnaire_service_respondent_type_choices(catalog))[4], "Parent of Twins (any age)")
})

test_that("a study lists only its questionnaires", {
  catalog <- test_catalog()
  expect_equal(
    questionnaire_service_choices(catalog, "1"),
    c("Adult Health and Personality (AHP) · Adult · 2 waves so far" = "1")
  )
  expect_equal(unname(questionnaire_service_choices(catalog, "2")), c("2", "3", "4"))
  expect_length(questionnaire_service_choices(catalog, "3"), 0)
})

test_that("every questionnaire can be picked to take MATR IDs from, named with its study", {
  choices <- questionnaire_service_all_choices(test_catalog())
  expect_length(choices, nrow(test_catalog()$questionnaires))
  expect_equal(unname(choices[names(choices) == "Adult Health and Personality (AHP) · Adult Health Study"]), "1")
  expect_equal(names(choices), sort(names(choices), method = "radix"))
})
