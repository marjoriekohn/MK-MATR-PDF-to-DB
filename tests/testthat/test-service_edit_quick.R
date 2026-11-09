# Adding a row from inside an extraction drop-down: the Metadata page's form for one row, applied at once.

form_values <- function(entity, snap, ...) {
  fields <- edit_service_fields(entity, snap, "add")
  list(fields = fields, values = utils::modifyList(edit_service_initial(entity, fields, snap, "add"), list(...)))
}

test_that("a registry, study, questionnaire or wave, and what they need, can be added this way", {
  expect_setequal(edit_quick_kinds, c("registry", "study", "questionnaire", "wave", "respondent_type", "investigator"))
  expect_error(edit_service_quick_entity("component"), "can't be added from here")
  expect_match(edit_service_quick_entity("wave")$about, "no questions")
  expect_match(edit_service_quick_entity("study")$about, "when it is pushed")
  expect_false(grepl("approv", edit_service_quick_entity("study")$about))
  expect_equal(unname(edit_quick_needs$questionnaire), "questionnaire__respondent_type_id")
  expect_equal(edit_service_quick_entity("respondent_type")$table, "respondent_type")
  study <- edit_service_quick_entity("study")
  expect_length(edit_service_fields(study, test_snapshot(edit_service_tables(study)), "add"), 8)
})

test_that("a row in the catalog is found by its key", {
  catalog <- test_catalog()
  expect_equal(edit_service_quick_row(catalog, "respondent_type", "2")$name, "Child")
  expect_equal(edit_service_quick_row(catalog, "study", "3")$registry_id, 2)
  expect_null(edit_service_quick_row(catalog, "respondent_type", "99"))
  expect_null(edit_service_quick_row(catalog, "investigator", "1")) # the catalog doesn't list them
})

test_that("after an add the drop-downs pick the new row and what it belongs to, or nothing if the catalog lacks it", {
  catalog <- test_catalog()
  before <- list(registry_id = "1", study_id = "1", questionnaire_id = "1", wave_id = "2")
  expect_equal(
    edit_service_quick_picks(catalog, "study", "3", before),
    list(registry_id = "2", study_id = "3", questionnaire_id = "", wave_id = "") # study 3 is in registry 2
  )
  expect_equal(edit_service_quick_picks(catalog, "registry", "2", before), list(registry_id = "2", study_id = "", questionnaire_id = "", wave_id = ""))
  # the catalog couldn't be read again, or the add made no row: nothing to pick it from
  expect_null(edit_service_quick_picks(catalog, "study", "99", before))
  expect_null(edit_service_quick_picks(catalog, "study", NA_character_, before))
})

test_that("a respondent type is added at once, by STAFF and by an ADMIN alike, and is in the history", {
  for (role in c("STAFF", "ADMIN")) {
    me <- local_staff(role)
    entity <- edit_service_quick_entity("respondent_type")
    snap <- edit_service_load(test_connection, entity)
    name <- test_name("Quick type")
    typed <- form_values(entity, snap, respondent_type__name = name, respondent_type__minimum_age = "5")
    added <- edit_service_quick_add(test_connection, entity, snap, typed$fields, typed$values, me)
    local_added_rows(list("new:respondent_type:1" = as.numeric(added$key)))

    expect_match(added$key, "^[0-9]+$")
    rows <- table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(added$key)))
    expect_equal(rows$name, name)
    expect_equal(rows$minimum_age, 5)
    expect_true(is.na(rows$maximum_age)) # ages are optional
    expect_equal(added$summary, paste0("Added the respondent type “", name, "”"))
    entry <- database_model_select(test_connection, "history", "action,staff_id,summary", filters = list(history_id = database_eq(added$history_id)))[[1]]
    expect_equal(entry$action, "Added")
    expect_equal(entry$staff_id, me$staff_id)
    expect_equal(entry$summary, added$summary)
  }
})

test_that("an investigator and then a study led by them are added, and the study has no questionnaire yet", {
  me <- local_staff()
  registry <- local_registry()
  investigator_entity <- edit_service_quick_entity("investigator")
  snap <- edit_service_load(test_connection, investigator_entity)
  typed <- form_values(investigator_entity, snap, investigator__name = test_name("Dr Quick"), investigator__email = "quick@example.test")
  investigator <- edit_service_quick_add(test_connection, investigator_entity, snap, typed$fields, typed$values, me)
  withr::defer(test_delete("principal_investigator", list(principal_investigator_id = database_eq(investigator$key))))

  study_entity <- edit_service_quick_entity("study")
  snap <- edit_service_load(test_connection, study_entity)
  name <- test_name("Quick study")
  typed <- form_values(
    study_entity, snap, study__registry_id = as.character(registry$registry_id),
    study__principal_investigator_id = investigator$key, study__name = name, study__status = "Active"
  )
  study <- edit_service_quick_add(test_connection, study_entity, snap, typed$fields, typed$values, me)
  withr::defer(test_delete("study", list(study_id = database_eq(study$key))), priority = "first")

  row <- table_model_get(test_connection, "study", list(study_id = database_eq(study$key)))
  expect_equal(row$name, name)
  expect_equal(row$status, "Active")
  expect_equal(row$principal_investigator_id, as.numeric(investigator$key))
  expect_equal(row$registry_id, registry$registry_id)
  expect_equal(nrow(table_model_get(test_connection, "questionnaire", list(study_id = database_eq(study$key)))), 0)
  # the catalog the drop-downs use has it once it is read again
  catalog <- questionnaire_service_get_catalog(test_connection)
  expect_equal(edit_service_quick_row(catalog, "study", study$key)$name, name)
})

test_that("study status is Active or Ended, with a tooltip", {
  expect_equal(study_statuses, c(Active = "Active", Ended = "Ended"))
  entity <- edit_entities$study
  fields <- edit_service_fields(entity, test_snapshot(edit_service_tables(entity)), "add")
  status <- Filter(function(field) field$name == "status", fields)[[1]]
  expect_equal(unname(status$choices), c("", "Active", "Ended"))
  expect_equal(status$tooltip, study_status_tooltip)
  expect_false(isTRUE(status$required)) # empty when not known
})

test_that("an add the form or the database refuses is a sentence, and nothing is added", {
  me <- local_staff()
  entity <- edit_service_quick_entity("respondent_type")
  snap <- edit_service_load(test_connection, entity)
  name <- test_name("Refused")
  # the youngest age can't be older than the oldest: the form's own check stops it ...
  typed <- form_values(entity, snap, respondent_type__name = name, respondent_type__minimum_age = "30", respondent_type__maximum_age = "20")
  expect_error(edit_service_quick_add(test_connection, entity, snap, typed$fields, typed$values, me), "The oldest age can't be younger")
  # ... and the database stops an age over 120, which the form doesn't know about
  typed <- form_values(entity, snap, respondent_type__name = name, respondent_type__maximum_age = "150")
  expect_error(edit_service_quick_add(test_connection, entity, snap, typed$fields, typed$values, me), "respondent_type_maximum_age_check")
  expect_equal(nrow(table_model_get(test_connection, "respondent_type", list(name = database_eq(name)))), 0)
})
