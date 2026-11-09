cascade_of <- function(snap, ...) edit_service_cascade(snap, list(...))

test_that("rows are identified by their key columns", {
  snap <- test_snapshot(c("placement", "wave_question"))
  expect_equal(edit_key(snap$wave_question[1:2, ], c("wave_id", "placement_id")), c("1|103", "1|104"))
  expect_equal(edit_key(snap$placement[0, ], "component_id"), character())
  expect_equal(edit_key(data.frame(id = c(100000, 3)), "id"), c("100000", "3"))
})

test_that("the links follow the database's foreign keys: what cascades, what is cleared, what blocks", {
  of <- function(child, parent) Filter(function(r) r$child == child && r$parent == parent, edit_relations)
  expect_equal(of("wave", "questionnaire")[[1]]$on_delete, "remove")
  expect_equal(of("placement", "questionnaire")[[1]]$on_delete, "remove")
  expect_setequal(vapply(of("placement", "placement"), function(r) r$on_delete, character(1)), c("remove", "clear"))
  expect_equal(of("wave_question", "wave")[[1]]$on_delete, "remove")
  expect_equal(of("variable", "placement")[[1]]$on_delete, "remove")
  expect_equal(of("response_option", "variable")[[1]]$on_delete, "remove")
  expect_equal(of("variable_tag", "variable")[[1]]$on_delete, "remove")
  expect_equal(of("study", "registry")[[1]]$on_delete, "block")
  expect_equal(of("questionnaire", "study")[[1]]$on_delete, "block")
  expect_equal(of("placement", "component")[[1]]$on_delete, "block")
  expect_equal(of("response_option", "answer_option")[[1]]$on_delete, "block")
  expect_equal(of("variable_tag", "tag")[[1]]$on_delete, "block")
  expect_equal(of("question_scale", "measurement_scale")[[1]]$on_delete, "block")
  expect_setequal(vapply(of("study", "principal_investigator"), function(r) r$on_delete, character(1)), c("block", "clear"))
  # every link names real tables and columns
  for (relation in edit_relations) {
    expect_true(all(names(relation$by) %in% names(database_tables[[relation$child]]$columns)), info = paste(relation$child, relation$parent))
    expect_true(all(unname(relation$by) %in% names(database_tables[[relation$parent]]$columns)), info = paste(relation$child, relation$parent))
  }
  expect_setequal(edit_delete_order, names(database_tables))
})

test_that("a question deleted from a questionnaire takes its numbers, variables, choices and tags, and frees what depended on it", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, placement = "104")
  expect_equal(sort(cascade$removed$wave_question), c("1|104", "2|104"))
  expect_equal(cascade$removed$variable, "2")
  expect_equal(sort(cascade$removed$response_option), c("2|1", "2|2"))
  expect_equal(cascade$removed$variable_tag, "2|1")
  expect_equal(cascade$cleared[[1]]$table, "placement")
  expect_equal(cascade$cleared[[1]]$keys, "105")
  expect_equal(cascade$cleared[[1]]$columns, "depends_on_placement_id")
  expect_length(cascade$blocked, 0)
  # MQ prints the component too, so it isn't forgotten
  expect_length(cascade$forgotten, 0)
})

test_that("what's inside a section goes with it, and a question that goes too needn't be cleared", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, placement = "101")
  expect_equal(sort(cascade$removed$placement), c("101", "102", "103", "104", "105"))
  expect_equal(length(cascade$removed$wave_question), 6)
  expect_equal(sort(cascade$removed$variable), c("1", "2", "3"))
  expect_length(cascade$cleared, 0) # 105 depended on 104, but goes too
  # the components only AHP prints are forgotten by the database; question 4 stays for MQ
  expect_equal(sort(cascade$forgotten), c("1", "2", "3", "5"))
})

test_that("deleting a questionnaire takes its waves, blocks, numbers, variables and tags, and the components only it prints are forgotten", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, questionnaire = "2")
  expect_equal(cascade$removed$wave, "3")
  expect_equal(sort(cascade$removed$placement), c("208", "209", "210"))
  expect_equal(cascade$removed$variable, "5")
  expect_equal(cascade$removed$placement_tag, "209|17")
  expect_length(cascade$blocked, 0)
  # KQP prints the section Mood (10) and question 9 too
  expect_equal(cascade$forgotten, "8")
})

test_that("a component no questionnaire prints any more stays while a scale lists it", {
  only_kq <- function(snap) {
    # KQ alone prints question 9 (a scale item) and the section Mood
    snap$placement <- snap$placement[snap$placement$questionnaire_id != 3, ]
    snap
  }
  snap <- test_snapshot(change = only_kq)
  expect_equal(sort(cascade_of(snap, questionnaire = "2")$forgotten), c("10", "8"))
  unscaled <- test_snapshot(change = function(snap) {
    snap <- only_kq(snap)
    snap$question_scale <- snap$question_scale[0, ]
    snap
  })
  expect_equal(sort(cascade_of(unscaled, questionnaire = "2")$forgotten), c("10", "8", "9"))
})

test_that("a wave takes only its numbers: the questions stay in the questionnaire", {
  snap <- test_snapshot()
  cascade <- cascade_of(snap, wave = "3")
  expect_equal(cascade$removed$wave_question, "3|209")
  expect_null(cascade$removed$placement)
  expect_null(cascade$removed$variable)
  expect_length(cascade$forgotten, 0)
})

test_that("a registry with studies, a study with questionnaires and a scale with items can't be deleted", {
  snap <- test_snapshot()
  registry <- cascade_of(snap, registry = "1")
  expect_match(registry$blocked[[1]]$text, "^2 studies are in the way")
  expect_match(registry$blocked[[1]]$fix, "Empty the registry")
  study <- cascade_of(snap, study = "2")
  expect_match(study$blocked[[1]]$text, "^3 questionnaires are in the way")
  expect_length(cascade_of(snap, study = "3")$blocked, 0) # nothing in it
  scale <- cascade_of(snap, measurement_scale = "1")
  expect_match(scale$blocked[[1]]$text, "^1 scale item is in the way")
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

test_that("a respondent type in use, a component printed somewhere, a choice offered and a tag carried can't be deleted", {
  snap <- test_snapshot()
  expect_match(cascade_of(snap, respondent_type = "3")$blocked[[1]]$text, "2 questionnaires are in the way")
  expect_length(cascade_of(snap, respondent_type = "99")$blocked, 0)
  printed <- cascade_of(snap, component = "4")
  expect_match(printed$blocked[[1]]$text, "^2 blocks in questionnaires are in the way")
  expect_length(cascade_of(snap, component = "15")$blocked, 0)
  expect_match(cascade_of(snap, answer_option = "1")$blocked[[1]]$text, "^3 response options for variables are in the way")
  expect_length(cascade_of(snap, answer_option = "39")$blocked, 0)
  tag <- cascade_of(snap, tag = "17")
  expect_equal(vapply(tag$blocked, function(b) b$table, character(1)), c("placement_tag", "variable_tag"))
  expect_match(tag$blocked[[1]]$text, "Questions carry this tag")
  expect_length(cascade_of(snap, tag = "2")$blocked, 0)
})

test_that("the reach says how many of each kind go, the components the database forgets, and how it falls on each questionnaire", {
  snap <- test_snapshot()
  reach <- edit_service_reach(snap, cascade_of(snap, questionnaire = "2"))
  texts <- vapply(reach$lines, function(line) line$text, character(1))
  expect_equal(texts, c(
    "1 wave", "3 blocks in questionnaires", "1 question in a wave", "1 variable", "4 response options for variables",
    "1 tag on a question", "2 tags on variables", "1 component no questionnaire prints any more (forgotten by the database)"
  ))
  expect_equal(reach$total, 14)
  expect_equal(reach$lines[[8]]$examples, "the next questions are about your mood.")
  expect_length(reach$by_questionnaire, 0) # the questionnaire itself goes: no breakdown
  section <- edit_service_reach(snap, cascade_of(snap, placement = "101"))
  expect_equal(vapply(section$by_questionnaire, function(q) q$questionnaire, character(1)), "AHP")
  expect_equal(section$by_questionnaire[[1]]$lines, c("5 blocks in questionnaires", "6 questions in waves", "3 variables")) # the section itself among them
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
  reach <- edit_service_reach(snap, cascade_of(snap, placement = "104"))
  expect_equal(reach$changed[[1]]$text, "1 block in a questionnaire will lose their question they depend on")
  expect_equal(reach$changed[[1]]$examples, "AHP · how many cigarettes do you smoke on a typical day?")
})

test_that("rows are named in staff's words", {
  snap <- test_snapshot()
  expect_equal(edit_label(snap, "wave", snap$wave[2, ]), "AHP Wave 2")
  expect_equal(edit_label(snap, "questionnaire", snap$questionnaire[1, ]), "Adult Health and Personality (AHP)")
  expect_equal(edit_label(snap, "component", snap$component[snap$component$component_id == 1, ]), "a. about you")
  expect_equal(edit_label(snap, "component", snap$component[snap$component$component_id == 16, ]), "(group)")
  untitled <- snap
  untitled$component$text[1] <- NA
  expect_equal(edit_label(untitled, "component", untitled$component[1, ]), "(untitled section)")
  expect_equal(edit_label(snap, "variable", snap$variable[1, ]), "AH01DB · date of birth")
  expect_equal(edit_label(snap, "placement", snap$placement[snap$placement$placement_id == 104, ]), "AHP · do you now smoke cigarettes?")
  expect_equal(edit_label(snap, "wave_question", snap$wave_question[3, ]), "AHP Wave 1 · question 2a · how many cigarettes do you smoke on a typical day?")
  expect_equal(edit_label(snap, "response_option", snap$response_option[1, ]), "AH02SM · yes (code 1)")
  expect_equal(edit_label(snap, "tag", snap$tag[snap$tag$tag_id == 17, ]), "in the past week")
  expect_equal(edit_label(snap, "variable_tag", snap$variable_tag[snap$variable_tag$tag_id == 22, ]), "cigarettes (unit) on AH02CG")
  expect_equal(edit_label(snap, "placement_tag", snap$placement_tag[1, ]), "in the past week (time frame) on “how often in the past week did you feel sad?”")
  expect_equal(edit_label(snap, "question_scale", snap$question_scale[1, ]), "Short Mood Scale item 1 · how often in the past week did you feel sad?")
  expect_equal(edit_label(snap, "pdf_file", snap$pdf_file), c("AHP_Redacted.pdf", "PDF b2b2b2b2b2b2"))
  expect_equal(edit_label(snap, "wave", snap$wave[0, ]), character())
})

test_that("the tables to read for a delete include the ones its names come from, and what keeps a component", {
  expect_true(all(c("wave", "wave_question", "questionnaire", "placement", "component") %in% edit_tables_to_read("wave")))
  expect_true(all(c(
    "wave", "placement", "wave_question", "variable", "response_option", "variable_tag", "placement_tag", "response_option_tag",
    "component", "question_scale", "tag", "answer_option"
  ) %in% edit_tables_to_read("questionnaire")))
  expect_false("study" %in% edit_tables_to_read("questionnaire")) # nothing above a questionnaire goes with it
  expect_true(all(c("study", "principal_investigator") %in% edit_tables_to_read("principal_investigator")))
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
        expect_equal(part$cascade$forgotten, full$cascade$forgotten, info = label)
        expect_equal(vapply(part$blocked, function(b) b$text, ""), vapply(full$blocked, function(b) b$text, ""), info = label)
        expect_equal(part$items, full$items, info = label)
      }
    }
  }
})
