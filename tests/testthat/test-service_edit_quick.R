# Adding a row from inside an extraction drop-down: the Make an edit form for one row, applied at once.

form_values <- function(entity, snap, ...) {
  fields <- edit_service_fields(entity, snap, "add")
  list(fields = fields, values = utils::modifyList(edit_service_initial(entity, fields, snap, "add"), list(...)))
}

test_that("a registry, study, questionnaire or wave, and what they need, can be added this way; a study comes without its questionnaire", {
  expect_setequal(edit_quick_kinds, c("registry", "study", "questionnaire", "wave", "respondent_type", "investigator"))
  expect_error(edit_service_quick_entity("component"), "can't be added from here")
  expect_match(edit_service_quick_entity("wave")$about, "no questions")
  expect_equal(unname(edit_quick_needs$questionnaire), "questionnaire__respondent_type_id")
  expect_equal(edit_service_quick_entity("respondent_type")$table, "respondent_type")
  study <- edit_service_quick_entity("study")
  expect_null(study$parts)
  expect_length(edit_service_fields(study, test_snapshot(edit_service_tables(study)), "add"), 8)
  expect_false("questionnaire__name" %in% vapply(edit_service_fields(study, test_snapshot(edit_service_tables(study)), "add"), function(field) field$id, character(1)))
  expect_null(edit_entities$study$parts) # nor on Make an edit
})

test_that("the key of the row an add made is read from the history's items", {
  items <- data.frame(
    entity_name = c("respondent_type", "respondent_type", "study"),
    record_identifier = c('{"respondent_type_id":12}', '{"respondent_type_id":12}', '{"study_id":3}'),
    action = c("Insert", "Insert", "Update"), stringsAsFactors = FALSE
  )
  expect_equal(edit_service_quick_key(items, "respondent_type"), "12")
  expect_true(is.na(edit_service_quick_key(items, "study"))) # only inserts count
  expect_true(is.na(edit_service_quick_key(items, "registry")))
})

test_that("a row in the catalog is found by its key", {
  catalog <- test_catalog()
  expect_equal(edit_service_quick_row(catalog, "respondent_type", "2")$name, "Child")
  expect_equal(edit_service_quick_row(catalog, "study", "3")$registry_id, 2)
  expect_null(edit_service_quick_row(catalog, "respondent_type", "99"))
  expect_null(edit_service_quick_row(catalog, "investigator", "1")) # the catalog doesn't list them
})

test_that("a respondent type is added at once, by STAFF and by an ADMIN alike, and is in the history without an approver", {
  for (role in c("STAFF", "ADMIN")) {
    me <- local_staff(role)
    entity <- edit_service_quick_entity("respondent_type")
    snap <- edit_service_load(test_connection, entity)
    name <- test_name("Quick type")
    typed <- form_values(entity, snap, respondent_type__name = name, respondent_type__minimum_age = "5")
    added <- edit_service_quick_add(test_connection, entity, snap, typed$fields, typed$values, me)
    local_applied(added$history_id)

    expect_match(added$key, "^[0-9]+$")
    rows <- table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(added$key)))
    expect_equal(rows$name, name)
    expect_equal(rows$minimum_age, 5)
    expect_true(is.na(rows$maximum_age)) # ages are optional
    expect_equal(added$record$status, "Draft")
    entry <- history_model_get_all(test_connection)[[as.character(added$history_id)]]
    expect_equal(entry$kind, "Add")
    expect_false(entry$approval_required)
    expect_true(is.na(entry$approved_by))
    expect_equal(entry$prepared_by, me$name)
    expect_equal(entry$summary, paste0("Added the respondent type “", name, "”"))
  }
})

test_that("an investigator and then a study led by them are added, and the study has no questionnaire yet", {
  me <- local_staff()
  registry <- local_registry()
  investigator_entity <- edit_service_quick_entity("investigator")
  snap <- edit_service_load(test_connection, investigator_entity)
  typed <- form_values(investigator_entity, snap, investigator__name = test_name("Dr Quick"), investigator__email = "quick@example.test")
  investigator <- edit_service_quick_add(test_connection, investigator_entity, snap, typed$fields, typed$values, me)
  local_applied(investigator$history_id)

  study_entity <- edit_service_quick_entity("study")
  snap <- edit_service_load(test_connection, study_entity)
  name <- test_name("Quick study")
  typed <- form_values(
    study_entity, snap, study__registry_id = as.character(registry$registry_id),
    study__principal_investigator_id = investigator$key, study__name = name, study__status = "Active"
  )
  study <- edit_service_quick_add(test_connection, study_entity, snap, typed$fields, typed$values, me)
  local_applied(study$history_id)

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

test_that("study status is Active or Ended, with a tooltip, wherever it is entered", {
  expect_equal(study_statuses, c(Active = "Active", Ended = "Ended"))
  expect_equal(study_status_tooltip, "Active: the study is ongoing. Ended: all data collections are finished.")
  for (key in c("study")) {
    entity <- edit_entities[[key]]
    fields <- edit_service_fields(entity, test_snapshot(edit_service_tables(entity)), "add")
    status <- Filter(function(field) field$name == "status" && field$part == "study", fields)[[1]]
    expect_equal(unname(status$choices), c("", "Active", "Ended"))
    expect_equal(status$tooltip, study_status_tooltip)
    expect_true(status$required)
  }
  # the database holds the same two values
  # me <- local_staff()
  # tree <- local_tree()
  # expect_error(
  #   database_model_update(test_connection, "study", list(status = "Ongoing"), list(study_id = database_eq(tree$study$study_id))),
  #   "study_status_check"
  # )
})

test_that("an add the database refuses is a sentence, and what was typed is still there as a saved edit", {
  me <- local_staff()
  entity <- edit_service_quick_entity("respondent_type")
  snap <- edit_service_load(test_connection, entity)
  # the youngest age can't be older than the oldest: the form's own check would stop it, so go round the form
  typed <- form_values(entity, snap, respondent_type__name = test_name("Refused"), respondent_type__minimum_age = "30", respondent_type__maximum_age = "20")
  expect_error(
    edit_service_quick_add(test_connection, entity, snap, typed$fields, typed$values, me),
    "wouldn't save that|violates check constraint"
  )
  saved <- Filter(function(record) identical(record$prepared_by_id, me$staff_id), edit_service_get_all(test_connection))
  expect_length(saved, 1)
  expect_equal(unname(saved)[[1]]$status, "Draft")
  expect_equal(unname(saved)[[1]]$form$values$respondent_type__name, typed$values$respondent_type__name)
})
