cascade_of <- function(snap, ...) edit_service_cascade(snap, list(...))

test_that("rows are identified by their key columns", {
  snap <- test_snapshot("questionnaire_component")
  expect_equal(edit_key(snap$questionnaire_component[1:2, ], c("questionnaire_id", "component_id")), c("1|1", "1|2"))
  expect_equal(edit_key(snap$questionnaire_component[0, ], "component_id"), character())
  expect_equal(edit_key(data.frame(id = c(100000, 3)), "id"), c("100000", "3"))
})

test_that("a question that nothing else leans on takes only what's attached to it", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, component = "3")

  expect_equal(cascade$removed$component, "3")
  expect_equal(cascade$removed$question, "3")
  expect_equal(cascade$removed$questionnaire_component, "1|3")
  expect_equal(cascade$removed$questionnaire_question, "1|3")
  expect_equal(sort(cascade$removed$wave_question), c("1|1|3", "1|2|3"))
  expect_equal(cascade$removed$variable, "1")
  expect_equal(cascade$removed$open_ended, "1")
  expect_length(cascade$cleared, 0)
  expect_length(cascade$blocked, 0)
})

test_that("a shared question is removed from every questionnaire that prints it", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, component = "9")

  expect_equal(sort(cascade$removed$questionnaire_component), c("2|9", "3|9"))
  expect_equal(sort(cascade$removed$variable), c("5", "6"))
  expect_equal(length(cascade$removed$response_option), 8)
  expect_equal(cascade$removed$question_scale, "1|9")
  expect_equal(cascade$removed$question_category, "9|D003863")
})

test_that("a question with a follow-up can't go until the follow-up does", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, component = "4")

  expect_length(cascade$blocked, 1)
  expect_match(cascade$blocked[[1]]$text, "1 question in a questionnaire is in the way")
  expect_equal(cascade$blocked[[1]]$labels, "AHP · How many cigarettes do you smoke on a typical day?")
  expect_match(cascade$blocked[[1]]$fix, "Delete the follow-ups first")
})

test_that("a follow-up that's going too isn't in the way", {
  snap <- test_snapshot()
  section_a <- cascade_of(snap, questionnaire_component = "1|1")

  expect_length(section_a$blocked, 0)
  expect_equal(sort(section_a$removed$questionnaire_component), sort(c("1|1", "1|2", "1|3", "1|4", "1|5")))
  expect_equal(sort(section_a$removed$questionnaire_question), c("1|3", "1|4", "1|5"))
})

test_that("a question's placement and its settings go as one", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, questionnaire_component = "1|5", questionnaire_question = "1|5")

  expect_equal(cascade$removed$questionnaire_component, "1|5")
  expect_equal(cascade$removed$questionnaire_question, "1|5")
  expect_equal(sort(cascade$removed$wave_question), c("1|1|5", "1|2|5"))
  expect_equal(cascade$removed$variable, "3")
  expect_length(cascade$blocked, 0)
  # the component itself stays: other questionnaires may print it
  expect_null(cascade$removed[["component"]])
})

test_that("deleting a questionnaire takes its waves, blocks and variables, but not the components", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, questionnaire = "2")

  expect_equal(cascade$removed$wave, "3")
  expect_equal(sort(cascade$removed$questionnaire_component), c("2|10", "2|8", "2|9"))
  expect_equal(cascade$removed$variable, "5")
  expect_null(cascade$removed[["component"]])
  expect_length(cascade$blocked, 0)
})

test_that("a wave takes the questions that were asked in nothing else", {
  snap <- test_snapshot()
  first_wave <- cascade_of(snap, wave = "1")
  expect_equal(length(first_wave$removed$wave_question), 4)
  expect_null(first_wave$removed$questionnaire_question)
  expect_length(first_wave$blocked, 0)

  # question 9 of KQ was asked only in Wave 1 of KQ, so it goes from KQ
  only_wave <- cascade_of(snap, wave = "3")
  expect_equal(only_wave$removed$questionnaire_question, "2|9")
  expect_equal(only_wave$removed$questionnaire_component, "2|9")
  expect_equal(only_wave$removed$variable, "5")
})

test_that("an investigator who leads a study can't be deleted, one who is only a contact can", {
  snap <- test_snapshot()
  lead <- cascade_of(snap, principal_investigator = "1")
  expect_match(lead$blocked[[1]]$text, "1 study is in the way")
  expect_equal(lead$blocked[[1]]$labels, "Adult Health Study")

  contact <- cascade_of(snap, principal_investigator = "4")
  expect_length(contact$blocked, 0)
  expect_equal(contact$cleared[[1]]$table, "study")
  expect_equal(contact$cleared[[1]]$columns, "contact_principal_investigator_id")
  expect_equal(contact$cleared[[1]]$keys, "1")
})

test_that("a respondent type in use can't be deleted", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, respondent_type = "3")
  expect_match(cascade$blocked[[1]]$text, "2 questionnaires are in the way")
  expect_length(cascade_of(snap, respondent_type = "99")$blocked, 0)
})

test_that("a study's last questionnaire and a questionnaire's last wave can be deleted", {
  snap <- test_snapshot(change = function(snap) {
    snap$questionnaire <- snap$questionnaire[snap$questionnaire$questionnaire_id != 3 & snap$questionnaire$questionnaire_id != 4, ]
    snap
  })
  only <- cascade_of(snap, questionnaire = "2")
  expect_length(only$blocked, 0)
  expect_equal(only$removed$questionnaire, "2")
  expect_null(only$removed$study) # the study stays, with no questionnaires until one is added

  last_wave <- cascade_of(snap, wave = "3") # questionnaire 2's only wave
  expect_length(last_wave$blocked, 0)
  expect_null(last_wave$removed$questionnaire) # the questionnaire stays, with no waves until one is added
  expect_equal(last_wave$removed$questionnaire_question, "2|9") # questions asked in no other wave go (BR69)

  whole <- cascade_of(snap, study = "2")
  expect_length(whole$blocked, 0)
  expect_equal(whole$removed$questionnaire, "2")
})

test_that("a registry takes everything inside it, and nothing outside", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, registry = "1")

  expect_equal(sort(cascade$removed$study), c("1", "2"))
  expect_equal(sort(cascade$removed$questionnaire), c("1", "2", "3", "4"))
  expect_equal(length(cascade$removed$variable), 9)
  expect_length(cascade$blocked, 0)
  expect_null(cascade$removed$principal_investigator)
  expect_null(cascade$removed$respondent_type)
})

test_that("a category goes from every question it tags, without deleting them", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, category = "D012907")
  expect_equal(length(cascade$removed$question_category), 3)
  expect_null(cascade$removed[["question"]])
})

test_that("an answer choice that variables offer can't be deleted", {
  snap <- test_snapshot()
  expect_match(cascade_of(snap, answer_option = "1")$blocked[[1]]$text, "response options for variables are in the way|in the way")
  expect_length(cascade_of(snap, answer_option = "39")$blocked, 0)
})

test_that("the reach says how many of each kind go, and hides the detail rows", {
  snap <- test_snapshot()
  reach <- edit_service_reach(snap, cascade_of(snap, component = "9"))
  texts <- vapply(reach$lines, function(line) line$text, character(1))

  expect_equal(texts, c(
    "2 components in questionnaires", "2 questions in questionnaires", "2 questions in waves",
    "2 variables", "8 response options for variables", "1 MeSH tag on a question",
    "1 measurement scale membership"
  ))
  expect_equal(reach$total, 18)
  # how it falls on each questionnaire
  expect_equal(vapply(reach$by_questionnaire, function(q) q$questionnaire, character(1)), c("KQ", "KQP"))
  expect_equal(
    reach$by_questionnaire[[1]]$lines,
    c("1 component in a questionnaire", "1 question in a questionnaire", "1 question in a wave", "1 variable")
  )
})

test_that("the starting rows aren't counted among what goes with them", {
  snap <- test_snapshot()
  reach <- edit_service_reach(snap, cascade_of(snap, component = "15"))
  expect_length(reach$lines, 0)
  expect_equal(reach$total, 0)
})

test_that("what's emptied rather than removed is listed apart", {
  snap <- test_snapshot()
  reach <- edit_service_reach(snap, cascade_of(snap, principal_investigator = "4"))
  expect_equal(reach$changed[[1]]$text, "1 study will lose their contact investigator")
})

test_that("rows are named in staff's words", {
  snap <- test_snapshot()
  expect_equal(edit_label(snap, "wave", snap$wave[2, ]), "AHP Wave 2")
  expect_equal(edit_label(snap, "questionnaire", snap$questionnaire[1, ]), "Adult Health and Personality (AHP)")
  expect_equal(edit_label(snap, "component", snap$component[snap$component$component_id == 1, ]), "A. About you")
  untitled <- snap$component[1, ]
  snap$component$text[1] <- NA
  expect_equal(edit_label(snap, "component", snap$component[1, ]), "(untitled section)")
  expect_equal(edit_label(snap, "variable", snap$variable[1, ]), "AH01DB · Date of birth")
  expect_equal(edit_label(snap, "wave", snap$wave[0, ]), character())
})

test_that("the tables to read for a delete include the ones its names come from, and the parents that need what goes", {
  # a wave's questions asked in no other wave go with it, and so do their variables
  expect_true(all(c(
    "wave",
    "wave_question",
    "questionnaire_question",
    "questionnaire_component",
    "variable",
    "response_option",
    "questionnaire",
    "component",
    "component_type"
  ) %in% edit_tables_to_read("wave")))
  # a registry reaches everything below it, though not what hangs off the global question
  expect_true(all(c("study", "questionnaire", "wave_question", "variable", "response_option") %in% edit_tables_to_read("registry")))
  expect_false("question_scale" %in% edit_tables_to_read("registry"))
})

test_that("a delete reaches the same rows whether the whole database was read or only the tables its kind reads", {
  whole <- test_snapshot()
  for (entity in edit_entities) {
    scopes <- if (!is.null(entity$scope)) c("1", "2", "4") else list(NULL)
    for (scope in scopes) {
      snap <- test_snapshot(edit_service_tables(entity))
      for (key in edit_service_cards(entity, snap, scope)$key) {
        row <- edit_service_row(entity, snap, key)
        part <- edit_service_delete_plan(entity, snap, row)
        full <- edit_service_delete_plan(entity, whole, edit_service_row(entity, whole, key))
        label <- paste(entity$key, key)
        expect_equal(part$cascade$removed, full$cascade$removed, info = label)
        expect_equal(part$cascade$cleared, full$cascade$cleared, info = label)
        expect_equal(vapply(part$blocked, function(b) b$text, ""), vapply(full$blocked, function(b) b$text, ""), info = label)
        expect_equal(part$items, full$items, info = label)
      }
    }
  }
})
