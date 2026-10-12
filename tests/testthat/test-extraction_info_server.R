# The questionnaire-and-wave form, shared by the Extraction page's modes and the review page's
# "About" dialog. Its inputs are named after the fields (`wave_name`, `study_id`...). It works on the live
# tables as the page read them, so these tests give it the sample tables and drafts; what it does with
# the database (adding a study from a drop-down) is in test-quick_add_server.R.

info_args <- function(extractions = fixed_extractions(), record_id = shiny::reactiveVal(NULL), catalog = fixed_catalog()) {
  list(catalog = catalog, extractions = extractions, record_id = record_id, me = fixture_staff, connection = list(url = "", key = ""))
}

test_that("the form holds what was typed, and empty text for what wasn't", {
  shiny::testServer(extraction_info_server, args = info_args(), {
    expect_equal(names(info()), extraction_info_fields)
    expect_true(all(unlist(info()) == ""))
    session$setInputs(kind = "new_wave", wave_name = "Wave 3", respondent_count = "1,200")
    expect_equal(info()$wave_name, "Wave 3")
    expect_equal(info()$respondent_count, "1,200")
    expect_equal(info()$name, "")
  })
})

test_that("the required fields still blank are named, and fewer as they are filled in", {
  shiny::testServer(extraction_info_server, args = info_args(), {
    session$setInputs(kind = "new_wave")
    expect_equal(missing(), c("registry", "study", "questionnaire", "wave name"))
    session$setInputs(registry_id = "1", study_id = "1", questionnaire_id = "1")
    expect_equal(missing(), "wave name")
    session$setInputs(wave_name = "Wave 3")
    expect_length(missing(), 0)

    session$setInputs(kind = "new_questionnaire")
    expect_equal(missing(), c("questionnaire name", "abbreviation", "respondent type"))
    session$setInputs(registry_id = "", study_id = "")
    expect_equal(missing(), c("registry", "study", "questionnaire name", "abbreviation", "respondent type"))
  })
})

test_that("a wave name that is taken is a problem, unless the draft that has it is this one", {
  other <- list(
    id = "50", status = "Draft", prepared_by = "Alex Rivera", prepared_by_id = 2L,
    info = list(kind = "new_wave", questionnaire_id = "1", wave_name = "Wave 3")
  )
  record_id <- shiny::reactiveVal(NULL)
  shiny::testServer(extraction_info_server, args = info_args(extractions = fixed_extractions(list("50" = other)), record_id = record_id), {
    errors <- collect_errors(session)
    session$setInputs(kind = "new_wave", questionnaire_id = "1", wave_name = "Wave 1")
    expect_match(problems()$wave_name, "AHP already has a Wave 1")

    session$setInputs(wave_name = "Wave 3")
    expect_match(problems()$wave_name, "Alex Rivera already started a draft of AHP Wave 3")

    record_id("50")
    session$flushReact()
    expect_null(problems()$wave_name)
    expect_length(errors(), 0)
  })
})

test_that("a new questionnaire's name and abbreviation are checked against its study", {
  shiny::testServer(extraction_info_server, args = info_args(), {
    session$setInputs(kind = "new_questionnaire", study_id = "1", name = "adult health and personality ", abbreviation = "XYZ")
    expect_match(problems()$name, "already has a questionnaire called Adult Health and Personality")
    expect_null(problems()$abbreviation)
    session$setInputs(name = "Something new", abbreviation = "ahp")
    expect_null(problems()$name)
    expect_match(problems()$abbreviation, "Already used for Adult Health and Personality")
  })
})

test_that("a date and a count that can't be read are problems, and blanks are fine", {
  shiny::testServer(extraction_info_server, args = info_args(), {
    session$setInputs(kind = "new_wave", administered_date = "", respondent_count = "")
    expect_length(problems(), 0)
    session$setInputs(administered_date = "2022-04-15", respondent_count = "about 1,200")
    expect_match(problems()$administered_date, "MM/DD/YYYY")
    expect_match(problems()$respondent_count, "whole number")
    session$setInputs(administered_date = "04/15/2022", respondent_count = "1,210")
    expect_length(problems(), 0)
  })
})

test_that("each problem is written under its own field", {
  shiny::testServer(extraction_info_server, args = info_args(), {
    session$setInputs(kind = "new_wave", administered_date = "tomorrow")
    expect_match(text_of(output$administered_date_note), "MM/DD/YYYY")
    expect_equal(text_of(output$respondent_count_note), "")
  })
})

test_that("showing a draft fills the form from it, with the study and registry of its questionnaire", {
  seen <- local_browser()
  record <- list(info = list(kind = "new_wave", questionnaire_id = "1", wave_name = "Wave 3", notes = "From the box in the hall"))
  shiny::testServer(extraction_info_server, args = info_args(), {
    show(record)
    expect_equal(last_update(seen, "registry_id")$selected, "1")
    study <- last_update(seen, "study_id")
    expect_equal(study$selected, "1")
    expect_equal(names(study$choices), c("Select study", "Adult Health Study (Dana Reyes)", "Child Development Study (Sam Patel)"))
    expect_equal(last_update(seen, "kind")$selected, "new_wave")
    questionnaire <- last_update(seen, "questionnaire_id")
    expect_equal(questionnaire$selected, "1")
    expect_match(names(questionnaire$choices)[2], "Adult Health and Personality")
    expect_equal(last_update(seen, "wave_name")$value, "Wave 3")
    expect_equal(last_update(seen, "notes")$value, "From the box in the hall")
  })
})

test_that("showing no draft empties the form, and the drop-downs offer what the database has", {
  seen <- local_browser()
  shiny::testServer(extraction_info_server, args = info_args(), {
    show(NULL)
    expect_equal(names(last_update(seen, "registry_id")$choices), c("Select registry", "Example Registry", "Mid-Atlantic Twin Registry"))
    expect_equal(last_update(seen, "study_id")$choices, c("Select a registry first" = ""))
    expect_equal(last_update(seen, "questionnaire_id")$choices, c("Select a study first" = ""))
    expect_equal(
      names(last_update(seen, "respondent_type_id")$choices),
      c("Select Respondent Type", "Adult (18+)", "Child (5-17)", "Parent (18+)")
    )
    expect_equal(last_update(seen, "name")$value, "")
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
    expect_equal(unname(study$choices), c("", "3"))
    expect_equal(study$selected, "") # study 1 isn't in registry 2

    session$setInputs(study_id = "3")
    expect_equal(last_update(seen, "questionnaire_id")$choices, c("No questionnaires recorded for this study" = ""))
  })
})

test_that("with nothing recorded, the drop-downs say to add it first", {
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
    expect_equal(names(last_update(seen, "registry_id")$choices), "No registries recorded yet. Add one on Make an edit first")
    expect_equal(names(last_update(seen, "respondent_type_id")$choices), "No Respondent Types recorded yet. Add one with the link below")
  })
})

test_that("the form shows the study once a registry is picked, and the questionnaire and wave once a study is", {
  html <- htmltools::renderTags(extraction_info_ui("info"))$html
  expect_match(html, 'data-display-if="input.registry_id != &#39;&#39;"', fixed = TRUE)
  expect_match(html, 'data-display-if="input.registry_id != &#39;&#39; &amp;&amp; input.study_id != &#39;&#39;"', fixed = TRUE)
  # the study's drop-down is inside the first condition, the wave name inside the second
  expect_lt(regexpr("input.registry_id", html), regexpr('id="info-study_id"', html))
  expect_lt(regexpr("input.study_id", html), regexpr('id="info-wave_name"', html))
})

test_that("the form is drawn with a link under Study and under Respondent Type that adds what is missing", {
  html <- htmltools::renderTags(extraction_info_ui("info"))$html
  expect_match(html, 'id="info-add_study"')
  expect_match(html, "Not in the list\\? Add a study")
  expect_match(html, 'id="info-add_respondent_type"')
  expect_match(html, "Add a Respondent Type")
  expect_match(html, "Respondent Type")
  expect_no_match(html, "Who answers it")
})

test_that("a catalog that is read again is what the drop-downs offer next", {
  seen <- local_browser()
  later <- test_catalog()
  later$respondent_types <- rbind(later$respondent_types, data.frame(respondent_type_id = 9, name = "Twin Parent", minimum_age = 30, maximum_age = NA))
  catalog <- fixed_catalog()
  shiny::testServer(extraction_info_server, args = info_args(catalog = catalog), {
    show(NULL)
    expect_length(last_update(seen, "respondent_type_id")$choices, 4)
    catalog$get(later)
    show(NULL)
    expect_equal(names(last_update(seen, "respondent_type_id")$choices)[5], "Twin Parent (30+)")
  })
})
