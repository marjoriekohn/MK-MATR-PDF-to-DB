# Pushing an extraction to the database: what stops it, the rows it adds (worked out from the questionnaire as
# saved and the MATR metadata database as it is), and what happens when a STAFF member or an ADMIN pushes it. The rows are
# worked out by pure functions, tested here with data given to them, and added by the database, tested with a
# real push that is removed again.

test_marker <- "ZZTEST"

# ------ A small questionnaire ------------------------------------------------------------------------------------
# A section holding an instruction, a closed question and an open one. Every block's words carry the run's marker,
# so what a push adds can be told from real data (and a push that wasn't cleaned up is noticed).
small_draft <- function(question = paste(test_marker, "Do you smoke?"), labels = c("Yes", "No"), subject = "Self", matr = c("zz1", "zz2")) {
  draft <- draft_service_from_json(list(
    questionnaire = list(name = "Small", abbreviation = "SM"),
    components = list(list(component_type = "Section", display_order = 1, text = paste(test_marker, "Part A"), children = list(
      list(component_type = "Instruction", display_order = 1, text = paste(test_marker, "Please answer.")),
      list(
        component_type = "Question", display_order = 2, text = question, question_number = "1", self_contained = TRUE,
        variables = list(list(
          variable_type = "closed_ended", display_order = 1, subject = subject,
          response_options = lapply(seq_along(labels), function(i) list(display_order = i, answer_option = labels[i], printed = TRUE))
        ))
      ),
      list(
        component_type = "Question", display_order = 3, text = paste(test_marker, "How many?"), question_number = "2", self_contained = TRUE,
        variables = list(list(variable_type = "open_ended", display_order = 1, subject = subject, answer_data_type = "NUMBER"))
      )
    )))
  ))
  draft$variables$matr_id <- paste0(matr, "-", tolower(test_marker))
  draft
}

# What staff typed for a new questionnaire in the tree's study.
# new_questionnaire_typed <- function(tree, wave_name = "Wave 1") {
#   list(
#     kind = "new_questionnaire", registry_id = as.character(tree$registry$registry_id), study_id = as.character(tree$study$study_id),
#     name = test_name("Pushed"), abbreviation = "ZP", respondent_type_id = as.character(tree$respondent_type$respondent_type_id),
#     wave_name = wave_name, administered_date = "04/15/2022", respondent_count = "1,200"
#   )
# }

new_wave_typed <- function(tree, wave_name = "Wave 2") {
  list(
    kind = "new_wave", registry_id = as.character(tree$registry$registry_id), study_id = as.character(tree$study$study_id),
    questionnaire_id = as.character(tree$questionnaire$questionnaire_id), wave_name = wave_name
  )
}

# A draft typed by hand, with this questionnaire saved in it, ready to push.
# saved_for_push <- function(me, typed, draft = small_draft()) {
#   record <- extraction_service_create_manual(test_connection, me, extraction_service_info(list(info = typed)))
#   draft_service_save(test_connection, record$id, draft, me$staff_id)
#   extraction_model_get(test_connection, record$id)
# }

# rows_where <- function(table, ...) {
#   nrow(database_model_get(test_connection, table, names(list(...))[1], filters = lapply(list(...), database_eq)))
# }

# ------ Before pushing -----------------------------------------------------------------------------------------------
complete_info <- function() {
  list(info = list(
    kind = "new_wave", questionnaire_id = "1", wave_name = "Wave 9", registry_id = "1", study_id = "1"
  ))
}

test_that("what stops a push is said in the order to do it", {
  catalog <- test_catalog()
  lookups <- test_lookups()
  draft <- gold_draft()
  draft$variables$matr_id <- paste0("q", seq_len(nrow(draft$variables)))
  blockers <- function(record = complete_info(), draft = draft, ...) {
    extraction_service_push_blockers(record, draft, lookups, catalog, ...)
  }
  expect_equal(blockers(draft = draft), character())

  expect_equal(blockers(list(info = list()), draft = draft)[1], "Fill in the registry, study, questionnaire and wave name under Where it goes.")
  taken <- complete_info()
  taken$info$wave_name <- "Wave 1" # the questionnaire has a Wave 1 already
  expect_equal(blockers(taken, draft)[1], "Fix the fields outlined in red under Where it goes.")
  expect_equal(blockers(draft = draft_service_empty()), "Add the questionnaire's blocks and questions first.")

  no_question <- draft
  no_question$components <- no_question$components[no_question$components$component_type != "Question", ]
  no_question$variables <- no_question$variables[0, ]
  no_question$response_options <- no_question$response_options[0, ]
  expect_equal(blockers(draft = no_question), "Add at least one question.")

  missing_ids <- draft
  missing_ids$variables$matr_id[1:3] <- NA # a blank MATR ID is among the blocks to fix
  expect_match(blockers(draft = missing_ids), "^Fix the [1-3] blocks? marked to fix\\.$")

  broken <- draft
  broken$components$text[broken$components$component_type == "Question"][1] <- NA # a question needs its words
  expect_match(blockers(draft = broken), "^Fix the 1 block marked to fix\\.$")
})

test_that("a questionnaire recorded without any blocks matches none of the draft's, and is no error", {
  nothing <- list(
    placements = no_rows(c("component_id", "parent_component_id", "display_order")),
    components = no_rows(c("component_id", "component_type_id", "text"))
  )
  ids <- list(component_types = data.frame(name = character(), component_type_id = integer()))
  expect_length(push_match_blocks(gold_draft(), nothing, ids), 0)
})

test_that("blocks not yet marked as checked are counted, but don't stop a push", {
  draft <- gold_draft()
  expect_equal(extraction_service_unchecked(draft), nrow(draft$components))
  checked <- with_checked(draft, draft$components$id[1:10])
  expect_equal(extraction_service_unchecked(checked), nrow(draft$components) - 10)
  expect_equal(extraction_service_unchecked(with_checked(draft, draft$components$id)), 0)
})

# ------ The rows, worked out ------------------------------------------------------------------------------------
build_for <- function(draft, info, existing = NULL, options = no_options) {
  extraction_service_build_items(draft, extraction_service_info(list(info = info)), ids = no_ids, existing = existing, options = options)
}
no_ids <- list(
  component_types = data.frame(component_type_id = 1:7, name = c("Section", "Question", "Introduction", "Instruction", "Definition", "Interviewer Script", "Note")),
  subjects = data.frame(subject_id = 1:2, name = c("Self", "Respondent's Family"))
)
no_options <- list(
  answer_options = data.frame(answer_option_id = numeric(), standardized_label = character()),
  option_sets = data.frame(option_set_id = numeric(), name = character()),
  members = data.frame(option_set_id = numeric(), answer_option_id = numeric())
)

test_that("a new questionnaire adds the questionnaire, its wave, every block, every question's numbers and its variables", {
  built <- build_for(small_draft(), list(
    kind = "new_questionnaire", study_id = "3", respondent_type_id = "2", name = "Small One", abbreviation = "SO", wave_name = "Wave 1",
    administered_date = "04/15/2022", respondent_count = "1,200"
  ))
  items <- built$items
  expect_equal(items$item_number, seq_len(nrow(items)))
  expect_true(all(items$action == "Insert")) # an extraction only adds rows
  expect_equal(unique(items$entity_name)[1:2], c("questionnaire", "wave"))
  expect_equal(built$summary$questionnaire, TRUE)
  expect_equal(built$summary[c("blocks", "questions", "variables", "choices")], list(blocks = 4L, questions = 2L, variables = 2L, choices = 2L))
  expect_equal(built$summary[c("answer_options", "option_sets")], list(answer_options = 2L, option_sets = 1L))

  lines <- item_lines(items)
  expect_true('Insert questionnaire {"questionnaire_id":"new:questionnaire:1"} name: - -> Small One' %in% lines)
  expect_true('Insert wave {"wave_id":"new:wave:1"} questionnaire_id: - -> new:questionnaire:1' %in% lines)
  expect_true('Insert wave {"wave_id":"new:wave:1"} respondent_count: - -> 1200' %in% lines)
  expect_true('Insert wave {"wave_id":"new:wave:1"} administered_date: - -> 2022-04-15' %in% lines)
  expect_equal(sum(items$entity_name == "component" & items$attribute_name == "text"), 4)
  expect_equal(sum(items$entity_name == "wave_question"), 2)
  expect_equal(sum(items$entity_name == "question"), 2)
  # the answer choices Yes and No make one set, named after them
  expect_true("Insert option_set {\"option_set_id\":\"new:option_set:1\"} name: - -> Yes / No" %in% lines)
  # blocks are added section first, then what is inside it, numbered from 1 in their section
  orders <- items$new_value[items$entity_name == "questionnaire_component" & items$attribute_name == "display_order"]
  expect_equal(orders, c("1", "1", "2", "3"))
})

test_that("an answer choice and a set of choices that are there already are used, not added again", {
  options <- list(
    answer_options = data.frame(answer_option_id = c(10, 11), standardized_label = c("Yes", "No")),
    option_sets = data.frame(option_set_id = 5, name = "No / Yes"),
    members = data.frame(option_set_id = c(5, 5), answer_option_id = c(11, 10))
  )
  built <- build_for(small_draft(), list(kind = "new_questionnaire", study_id = "3", respondent_type_id = "2", name = "Small One", abbreviation = "SO", wave_name = "Wave 1"), options = options)
  expect_equal(built$summary[c("answer_options", "option_sets", "option_sets_found")], list(answer_options = 0L, option_sets = 0L, option_sets_found = 1L))
  closed <- built$items[built$items$entity_name == "closed_ended", ]
  expect_equal(closed$new_value, "5") # the set that is there, whatever order its choices are in
  expect_false("answer_option" %in% built$items$entity_name)
  expect_equal(built$items$record_identifier[built$items$entity_name == "response_option"][1], '{"answer_option_id":10,"variable_key":"new:variable:1"}')

  # a different set of the same choices needs its own name if the first is taken
  expect_equal(push_option_set_name(c("Yes", "No"), "Yes / No"), "Yes / No (2)")
  expect_equal(nchar(push_option_set_name(paste("choice", 1:60), character())), 150)
})

test_that("a new wave of a recorded questionnaire finds its blocks where it prints them, and adds the rest", {
  existing <- list(
    placements = data.frame(component_id = c(100, 101, 102), parent_component_id = c(NA, 100, 100), display_order = c(1, 1, 2)),
    components = data.frame(component_id = c(100, 101, 102), component_type_id = c(1, 4, 2), text = paste(test_marker, c("Part A", "Please answer.", "Do you smoke?"))),
    questions = data.frame(component_id = 102),
    variables = data.frame(variable_key = 900, component_id = 102, matr_id = paste0("zz1-", tolower(test_marker)), display_order = 1),
    waves = data.frame(name = "Wave 1")
  )
  built <- build_for(small_draft(), list(kind = "new_wave", questionnaire_id = "7", wave_name = "Wave 2"), existing = existing)
  expect_false(built$summary$questionnaire)
  expect_equal(built$summary$blocks_found, 3L) # the section, the instruction and the smoking question
  expect_equal(built$summary$blocks, 1L) # the question about how many is new
  expect_equal(built$summary$variables_found, 1L)
  expect_equal(built$summary$variables, 1L)
  expect_equal(built$summary$questions, 2L) # both are asked in the wave
  wave_questions <- built$items[built$items$entity_name == "wave_question", ]
  expect_true('{"component_id":102,"questionnaire_id":7,"wave_id":"new:wave:1"}' %in% wave_questions$record_identifier)
  # the new block goes after what is in its section already
  order <- built$items$new_value[built$items$entity_name == "questionnaire_component" & built$items$attribute_name == "display_order"]
  expect_equal(order, "3")
  parent <- built$items$new_value[built$items$entity_name == "questionnaire_component" & built$items$attribute_name == "parent_component_id"]
  expect_equal(parent, "100")
})

test_that("a MATR ID that another question already has, or nothing to add, is a sentence", {
  existing <- list(
    placements = data.frame(component_id = 102, parent_component_id = NA_real_, display_order = 1),
    components = data.frame(component_id = 102, component_type_id = 2, text = "Some other question"),
    questions = data.frame(component_id = 102),
    variables = data.frame(variable_key = 900, component_id = 102, matr_id = paste0("zz1-", tolower(test_marker)), display_order = 1),
    waves = data.frame(name = "Wave 1")
  )
  expect_error(
    build_for(small_draft(), list(kind = "new_wave", questionnaire_id = "7", wave_name = "Wave 2"), existing = existing),
    "already used by another question of this questionnaire. Give the variable a MATR ID of its own"
  )
  expect_error(build_for(draft_service_empty(), list(kind = "new_wave", questionnaire_id = "7")), "nothing in this questionnaire to add")
})

test_that("a push is described in words", {
  built <- build_for(small_draft(), list(kind = "new_questionnaire", study_id = "3", respondent_type_id = "2", name = "Small One", abbreviation = "SO", wave_name = "Wave 1"))
  expect_equal(
    extraction_service_push_summary(built$summary),
    "Adds the questionnaire, the wave, 4 new blocks, 2 questions asked in the wave, 2 new variables, 2 response options, 2 new answer choices and 1 new set of choices."
  )
  summary <- built$summary
  summary$questionnaire <- FALSE
  summary$blocks <- 0L
  summary$blocks_found <- 4L
  summary$variables_found <- 2L
  summary$variables <- 0L
  expect_match(extraction_service_push_summary(summary), "^Adds the wave, 4 blocks already there, 2 questions asked in the wave, 2 variables already there")
})

test_that("an extraction can go into a recorded wave that asks no questions yet, and not one that does", {
  existing <- list(
    placements = no_rows(c("component_id", "parent_component_id", "display_order")),
    components = no_rows(c("component_id", "component_type_id", "text")),
    questions = no_rows("component_id"),
    variables = no_rows(c("variable_key", "component_id", "matr_id", "display_order")),
    waves = data.frame(wave_id = c(40, 41), name = c("Wave 1", "Wave 2")),
    wave_questions = data.frame(wave_id = 40, component_id = 5)
  )
  info <- list(kind = "new_wave", questionnaire_id = "7", wave_choice = "existing", wave_id = "41")
  built <- build_for(small_draft(), info, existing = existing)
  expect_false(built$summary$wave)
  expect_false("wave" %in% built$items$entity_name) # the wave is there already
  wave_questions <- built$items[built$items$entity_name == "wave_question", ]
  expect_true(all(grepl('"wave_id":41', wave_questions$record_identifier)))
  expect_match(extraction_service_push_summary(built$summary), "^Adds 4 new blocks, 2 questions asked in the wave")

  expect_error(build_for(small_draft(), utils::modifyList(info, list(wave_id = "40")), existing = existing), "already asks 1 question")
  expect_error(build_for(small_draft(), utils::modifyList(info, list(wave_id = "99")), existing = existing), "isn't one of this questionnaire's waves")
  # a new questionnaire's wave is always new
  new_one <- build_for(small_draft(), list(kind = "new_questionnaire", study_id = "3", respondent_type_id = "2", name = "N", abbreviation = "N", wave_choice = "existing", wave_id = "41", wave_name = "Wave 1"))
  expect_true(new_one$summary$wave)
})

test_that("repeated choices are new response options, but each answer choice and set of choices is added or reused once", {
  # three variables offering Yes / No: three times two response options, one per variable and choice
  draft <- small_draft()
  question <- draft$components$id[draft$components$component_type == "Question"][1]
  draft <- draft_service_add_box(draft, question)
  draft <- draft_service_add_box(draft, question)
  draft$variables$matr_id <- paste0("zz", seq_len(nrow(draft$variables)))
  info <- list(kind = "new_questionnaire", study_id = "3", respondent_type_id = "2", name = "Small One", abbreviation = "SO", wave_name = "Wave 1")

  added <- build_for(draft, info)
  expect_equal(added$summary$choices, 6L)
  expect_equal(added$summary[c("answer_options", "option_sets")], list(answer_options = 2L, option_sets = 1L))
  expect_equal(sum(added$items$entity_name == "answer_option"), 2) # Yes and No once each
  expect_equal(length(unique(added$items$record_identifier[added$items$entity_name == "response_option"])), 6)

  options <- list(
    answer_options = data.frame(answer_option_id = c(10, 11), standardized_label = c("Yes", "No")),
    option_sets = data.frame(option_set_id = 5, name = "Yes / No"),
    members = data.frame(option_set_id = c(5, 5), answer_option_id = c(10, 11))
  )
  reused <- build_for(draft, info, options = options)
  expect_equal(reused$summary[c("answer_options", "answer_options_found", "option_sets", "option_sets_found")],
    list(answer_options = 0L, answer_options_found = 2L, option_sets = 0L, option_sets_found = 1L))
  expect_equal(reused$summary$choices, 6L) # still one per variable and choice: the links are new

  cards <- extraction_service_what_cards(reused$summary)
  kinds <- vapply(cards, function(card) card$kind, character(1))
  answer_card <- cards[[which(kinds == "Answer choices")]]
  expect_false(answer_card$is_new)
  expect_equal(answer_card$facts, "2 already in the database, used again")
  links <- cards[[which(kinds == "Response options")]]
  expect_true(links$is_new)
  expect_equal(links$title, "6 response options")
})

test_that("the preview says where it goes, recorded or NEW, and what it adds, without writing anything", {
  catalog <- test_catalog()
  context <- list(ids = no_ids, options = no_options)
  info <- extraction_service_info(list(info = list(
    kind = "new_questionnaire", registry_id = "1", study_id = "1", respondent_type_id = as.character(catalog$respondent_types$respondent_type_id[1]),
    name = "Small One", abbreviation = "SO", wave_name = "Wave 1"
  )))
  preview <- extraction_service_request_preview(small_draft(), info, catalog, context)
  where <- vapply(preview$where, function(card) card$kind, character(1))
  expect_equal(where, c("Registry", "Study", "Questionnaire", "Wave"))
  expect_equal(vapply(preview$where, function(card) isTRUE(card$is_new), logical(1)), c(FALSE, FALSE, TRUE, TRUE))
  expect_null(preview$problem)
  expect_match(preview$summary, "^Adds the questionnaire, the wave")
  html <- htmltools::renderTags(extraction_request_ui(preview, small_draft()))$html
  expect_match(html, "Where it goes")
  expect_match(html, "fa-certificate")
  expect_match(html, "NEW")
  expect_false(grepl("Why", html))

  # until it is placed, the cards say what is still to pick, and nothing is worked out
  unplaced <- extraction_service_request_preview(small_draft(), extraction_service_info(), catalog, context)
  expect_true(all(vapply(unplaced$where[1:2], function(card) isTRUE(card$is_missing), logical(1))))
  expect_length(unplaced$what, 0)
})

test_that("a push is titled for the request that carries it", {
  record <- list(pdf = list(name = "x.pdf"), info = new_wave_typed(list(questionnaire = list(questionnaire_id = 1))))
  record$info$questionnaire_id <- "1"
  expect_equal(extraction_service_push_title(record, test_catalog()), "Add Adult Health and Personality (AHP) · Wave 2")
})

# ------ Pushing, for real ----------------------------------------------------------------------------------------------
# test_that("an ADMIN's push of a new questionnaire is added at once, and everything it adds is in the history", {
#   admin <- local_staff("ADMIN")
#   tree <- local_tree()
#   subject <- local_row("subject", list(name = test_name("Subject")), "subject_id")
#   yes <- local_row("answer_option", list(standardized_label = paste(test_marker, "Yes")), "answer_option_id")
#   typed <- new_questionnaire_typed(tree)
#   record <- saved_for_push(admin, typed, small_draft(labels = paste(test_marker, c("Yes", "No")), subject = subject$name))
#
#   built <- extraction_service_push_build(test_connection, record)
#   expect_equal(built$summary$answer_options, 1L) # Yes is there, No is new
#   outcome <- extraction_service_push(test_connection, record, admin, questionnaire_service_get_catalog(test_connection), note = "  From the PDF ")
#   local_applied(outcome$history_id)
#
#   expect_true(outcome$applied)
#   expect_equal(outcome$record$status, "Applied")
#   entry <- history_model_get_all(test_connection)[[as.character(outcome$history_id)]]
#   expect_equal(entry$kind, "Add")
#   expect_false(entry$approval_required)
#   expect_true(is.na(entry$approved_by))
#   expect_match(entry$summary, paste0("^Add ", typed$name, " \\(ZP\\) · Wave 1$|^Add .*", test_marker))
#   expect_equal(entry$note, "From the PDF")
#
#   questionnaire <- database_model_get(test_connection, "questionnaire", c("questionnaire_id", "study_id", "respondent_type_id"), filters = list(name = database_eq(typed$name)))
#   expect_equal(nrow(questionnaire), 1)
#   expect_equal(questionnaire$study_id, tree$study$study_id)
#   expect_equal(rows_where("wave", questionnaire_id = questionnaire$questionnaire_id), 1)
#   wave <- database_model_get(test_connection, "wave", c("name", "respondent_count", "administered_date"), filters = list(questionnaire_id = database_eq(questionnaire$questionnaire_id)))
#   expect_equal(wave$name, "Wave 1")
#   expect_equal(wave$respondent_count, 1200)
#   expect_equal(wave$administered_date, "2022-04-15")
#   expect_equal(rows_where("questionnaire_component", questionnaire_id = questionnaire$questionnaire_id), 4)
#   expect_equal(rows_where("wave_question", questionnaire_id = questionnaire$questionnaire_id), 2)
#   expect_equal(rows_where("variable", questionnaire_id = questionnaire$questionnaire_id), 2)
#   options <- database_model_get(test_connection, "answer_option", "standardized_label", filters = list(standardized_label = paste0("like.", test_marker, "*")))
#   expect_setequal(options$standardized_label, paste(test_marker, c("Yes", "No"))) # No was added, Yes used
#
#   # the draft is done with
#   expect_equal(extraction_model_get(test_connection, record$id)$status, "Applied")
# })
#
# test_that("a STAFF member's push waits for approval, adds nothing, and is added when someone else approves it", {
#   me <- local_staff()
#   approver <- local_staff()
#   tree <- local_tree()
#   subject <- local_row("subject", list(name = test_name("Subject")), "subject_id")
#   typed <- new_questionnaire_typed(tree)
#   record <- saved_for_push(me, typed, small_draft(labels = paste(test_marker, c("Yes", "No")), subject = subject$name))
#
#   outcome <- extraction_service_push(test_connection, record, me, questionnaire_service_get_catalog(test_connection), note = "Please check page 2", reviewer = approver$staff_id)
#   expect_false(outcome$applied)
#   expect_null(outcome$history_id)
#   expect_equal(outcome$record$status, "Awaiting Review")
#   expect_equal(outcome$record$note, "Please check page 2")
#   expect_equal(outcome$record$requested_reviewer, list(id = approver$staff_id, name = approver$name))
#   expect_match(outcome$record$summary, "^Add ")
#   expect_equal(rows_where("questionnaire", name = typed$name), 0) # nothing added yet
#   expect_true(outcome$record$id %in% names(change_request_service_waiting(test_connection)))
#
#   # the person who prepared it can't approve it
#   expect_error(change_request_service_approve(test_connection, outcome$record, me), "You prepared this")
#   history_id <- change_request_service_approve(test_connection, outcome$record, approver)
#   local_applied(history_id)
#   expect_equal(rows_where("questionnaire", name = typed$name), 1)
#   entry <- history_model_get_all(test_connection)[[history_id]]
#   expect_true(entry$approval_required)
#   expect_equal(entry$approved_by, approver$name)
#   expect_equal(extraction_model_get(test_connection, record$id)$status, "Applied")
# })
#
# test_that("a new wave of a questionnaire that is recorded adds only the wave and what isn't printed already", {
#   admin <- local_staff("ADMIN")
#   tree <- local_tree()
#   subject <- local_row("subject", list(name = test_name("Subject")), "subject_id")
#   draft <- small_draft(labels = paste(test_marker, c("Yes", "No")), subject = subject$name)
#   first <- extraction_service_push(
#     test_connection, saved_for_push(admin, modifyList(new_questionnaire_typed(tree), list(name = test_name("Recorded"))), draft),
#     admin, questionnaire_service_get_catalog(test_connection)
#   )
#   local_applied(first$history_id)
#   recorded <- database_model_get(test_connection, "questionnaire", "questionnaire_id", filters = list(name = paste0("like.", test_marker, " Recorded*")))
#   expect_equal(nrow(recorded), 1)
#
#   typed <- list(kind = "new_wave", questionnaire_id = as.character(recorded$questionnaire_id), wave_name = "Wave 2")
#   record <- saved_for_push(admin, typed, draft)
#   built <- extraction_service_push_build(test_connection, record)
#   expect_equal(built$summary$blocks, 0L)
#   expect_equal(built$summary$blocks_found, 4L)
#   expect_equal(built$summary$variables_found, 2L)
#   second <- extraction_service_push(test_connection, record, admin, questionnaire_service_get_catalog(test_connection))
#   local_applied(second$history_id)
#   expect_equal(rows_where("wave", questionnaire_id = recorded$questionnaire_id), 2)
#   expect_equal(rows_where("wave_question", questionnaire_id = recorded$questionnaire_id), 4)
#   expect_equal(rows_where("variable", questionnaire_id = recorded$questionnaire_id), 2) # no new variables
#   expect_equal(rows_where("questionnaire_component", questionnaire_id = recorded$questionnaire_id), 4) # no new blocks
#
#   # a wave with that name is there now, so another push of it is refused by the database as well as by the page
#   again <- saved_for_push(admin, typed, draft)
#   expect_error(extraction_service_push(test_connection, again, admin, questionnaire_service_get_catalog(test_connection)), "wouldn't save that")
# })
#
# test_that("a push refused by the database leaves nothing behind", {
#   admin <- local_staff("ADMIN")
#   tree <- local_tree()
#   # two variables with one MATR ID: the database refuses it only when it gets there, after the rest was added
#   record <- saved_for_push(admin, new_questionnaire_typed(tree), small_draft(matr = c("zz1", "zz1")))
#   expect_error(
#     extraction_service_push(test_connection, record, admin, questionnaire_service_get_catalog(test_connection)),
#     "wouldn't save that"
#   )
#   expect_equal(extraction_model_get(test_connection, record$id)$status, "Draft")
#   expect_equal(rows_where("questionnaire", name = record$info$name), 0)
#   expect_equal(nrow(database_model_get(test_connection, "component", "component_id", filters = list(text = paste0("like.", test_marker, "*")))), 0)
# })
