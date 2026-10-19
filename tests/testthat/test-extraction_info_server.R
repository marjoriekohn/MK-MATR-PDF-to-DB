# The form that says where an extraction goes (its Basic Info: registry, study, questionnaire, wave), in
# the Change Requests dialog. Its inputs are named after the fields (`registry_id`, `wave_id`...). It
# works on the live tables as the page read them, so these tests give it the sample tables and drafts;
# what the pop-ups do with the database is in test-quick_add_server.R.

info_args <- function(extractions = fixed_extractions(), record_id = shiny::reactiveVal(NULL), catalog = fixed_catalog()) {
  list(catalog = catalog, extractions = extractions, record_id = record_id, me = fixture_staff, connection = list(url = "", key = ""))
}

test_that("the form holds the four picks, always a recorded questionnaire's wave", {
  shiny::testServer(extraction_info_server, args = info_args(), {
    expect_equal(names(info()), extraction_info_fields)
    expect_equal(extraction_info_fields, c("registry_id", "study_id", "questionnaire_id", "wave_id"))
    session$setInputs(registry_id = "1", study_id = "1", questionnaire_id = "1", wave_id = "2")
    expect_equal(unlist(info()), c(registry_id = "1", study_id = "1", questionnaire_id = "1", wave_id = "2"))
  })
})

test_that("the drop-downs still blank are named, and fewer as they are picked", {
  shiny::testServer(extraction_info_server, args = info_args(), {
    session$setInputs(registry_id = "", study_id = "", questionnaire_id = "", wave_id = "")
    expect_equal(missing(), c("registry", "study", "questionnaire", "wave"))
    session$setInputs(registry_id = "1", study_id = "1", questionnaire_id = "1")
    expect_equal(missing(), "wave")
    session$setInputs(wave_id = "2")
    expect_length(missing(), 0)
  })
})

test_that("a wave has to be the questionnaire's, and is written under the wave drop-down when it isn't", {
  shiny::testServer(extraction_info_server, args = info_args(), {
    session$setInputs(registry_id = "1", study_id = "1", questionnaire_id = "1", wave_id = "3") # another questionnaire's wave
    expect_equal(problems()$wave_id, "Pick one of this questionnaire's waves.")
    expect_match(text_of(output$wave_id_note), "Pick one of this questionnaire's waves")
    session$setInputs(wave_id = "2")
    expect_null(problems()$wave_id)
  })
})

test_that("a drop-down still blank is marked, so it is outlined in red, and the mark goes once it is picked", {
  shiny::testServer(extraction_info_server, args = info_args(), {
    session$setInputs(registry_id = "", study_id = "", questionnaire_id = "", wave_id = "")
    expect_match(text_of(output$registry_id_note), "Required")
    expect_match(text_of(output$wave_id_note), "Required")
    session$setInputs(registry_id = "1")
    expect_equal(text_of(output$registry_id_note), "")
  })
})

test_that("the form is four drop-downs in one row, with no labels and no new-or-existing toggles", {
  html <- htmltools::renderTags(extraction_info_ui("info"))$html
  expect_false(grepl('class="control-label"', html, fixed = TRUE)) # Shiny keeps an empty, hidden label (shiny-label-null)
  expect_false(grepl("Existing questionnaire|New questionnaire|info-kind|info-wave_choice|info-wave_name|data-display-if", html))
  selects <- regmatches(html, gregexpr("<select[^>]*>", html))[[1]]
  ids <- sub('.*id="info-([a-z_]+)".*', "\\1", selects)
  expect_equal(ids, c("registry_id", "study_id", "questionnaire_id", "wave_id"))
  expect_match(html, "basic-info-row")
  expect_match(html, 'aria-label="Registry"')
  expect_match(html, 'id="info-registry_id_note"')
})

test_that("showing a draft fills the form from it, with the study and registry of its questionnaire", {
  seen <- local_browser()
  record <- list(info = list(questionnaire_id = "1", wave_id = "2"))
  shiny::testServer(extraction_info_server, args = info_args(), {
    show(record)
    expect_equal(last_update(seen, "registry_id")$selected, "1")
    study <- last_update(seen, "study_id")
    expect_equal(study$selected, "1")
    expect_equal(names(study$choices), c("Select study", "Adult Health Study (Dana Reyes)", "Child Development Study (Sam Patel)", "+ Add new study"))
    questionnaire <- last_update(seen, "questionnaire_id")
    expect_equal(questionnaire$selected, "1")
    expect_match(names(questionnaire$choices)[2], "Adult Health and Personality")
    expect_equal(tail(names(questionnaire$choices), 1), "+ Add new questionnaire")
    wave <- last_update(seen, "wave_id")
    expect_equal(wave$selected, "2")
    expect_equal(tail(unname(wave$choices), 1), add_new_choice)
  })
})

test_that("an older draft that typed a new questionnaire keeps only what the form can hold", {
  seen <- local_browser()
  record <- list(info = list(kind = "new_questionnaire", registry_id = "1", study_id = "1", name = "Typed", wave_name = "Wave 1"))
  shiny::testServer(extraction_info_server, args = info_args(), {
    show(record)
    expect_equal(last_update(seen, "study_id")$selected, "1")
    expect_equal(last_update(seen, "questionnaire_id")$selected, "")
    expect_equal(last_update(seen, "wave_id")$selected, "")
  })
})

test_that("showing no draft empties the form, and each drop-down offers what the one before it holds", {
  seen <- local_browser()
  shiny::testServer(extraction_info_server, args = info_args(), {
    show(NULL)
    expect_equal(
      names(last_update(seen, "registry_id")$choices),
      c("Select registry", "Example Registry", "Mid-Atlantic Twin Registry", "+ Add new registry")
    )
    expect_equal(last_update(seen, "study_id")$choices, c("Select a registry first" = ""))
    expect_equal(last_update(seen, "questionnaire_id")$choices, c("Select a study first" = ""))
    expect_equal(last_update(seen, "wave_id")$choices, c("Select a questionnaire first" = ""))
  })
})

test_that("choosing a registry offers its studies, keeps a study that is in it and clears one that isn't", {
  seen <- local_browser()
  shiny::testServer(extraction_info_server, args = info_args(), {
    # (the browser starts the drop-downs blank; the first value an observer sees is its start-up)
    session$setInputs(registry_id = "", study_id = "")
    session$setInputs(study_id = "1")
    expect_match(names(last_update(seen, "questionnaire_id")$choices)[2], "Adult Health and Personality")
    session$setInputs(registry_id = "1")
    expect_equal(last_update(seen, "study_id")$selected, "1")
    session$setInputs(registry_id = "2")
    study <- last_update(seen, "study_id")
    expect_equal(unname(study$choices), c("", "3", add_new_choice))
    expect_equal(study$selected, "") # study 1 isn't in registry 2

    session$setInputs(study_id = "3")
    expect_equal(
      last_update(seen, "questionnaire_id")$choices,
      c("No questionnaires in this study yet" = "", "+ Add new questionnaire" = add_new_choice)
    )
  })
})

test_that("+ Add new isn't a pick: a pop-up that can't open puts the drop-down back as it was", {
  seen <- local_browser()
  shiny::testServer(extraction_info_server, args = info_args(), {
    session$setInputs(registry_id = "", study_id = "")
    session$setInputs(registry_id = "1", study_id = "1")
    session$setInputs(study_id = add_new_choice) # the test has no database, so the pop-up can't read its tables
    expect_equal(info()$study_id, "")
    expect_equal(last_update(seen, "study_id")$selected, "1")
  })
})

test_that("with nothing recorded, each drop-down says so and still offers to add one", {
  seen <- local_browser()
  empty_table <- function(columns) as.data.frame(stats::setNames(rep(list(logical()), length(columns)), columns))
  empty <- questionnaire_service_catalog(list(
    registries = empty_table(c("registry_id", "name", "abbreviation")),
    studies = empty_table(c("study_id", "registry_id", "principal_investigator_id", "name")),
    investigators = empty_table(c("principal_investigator_id", "name")),
    questionnaires = empty_table(c("questionnaire_id", "study_id", "respondent_type_id", "name", "abbreviation")),
    waves = empty_table(c("wave_id", "questionnaire_id", "name", "administered_date", "respondent_count")),
    respondent_types = empty_table(c("respondent_type_id", "name", "minimum_age", "maximum_age"))
  ))
  shiny::testServer(extraction_info_server, args = info_args(catalog = fixed_catalog(empty)), {
    show(NULL)
    expect_equal(names(last_update(seen, "registry_id")$choices), c("No registries recorded yet", "+ Add new registry"))
  })
})

test_that("a catalog that is read again is what the drop-downs offer next", {
  seen <- local_browser()
  later <- test_catalog()
  later$registries <- rbind(later$registries, data.frame(registry_id = 9, name = "Zebra Registry", abbreviation = "ZR"))
  catalog <- fixed_catalog()
  shiny::testServer(extraction_info_server, args = info_args(catalog = catalog), {
    show(NULL)
    expect_length(last_update(seen, "registry_id")$choices, 4)
    catalog$get(later)
    show(NULL)
    expect_equal(names(last_update(seen, "registry_id")$choices)[4], "Zebra Registry")
  })
})
