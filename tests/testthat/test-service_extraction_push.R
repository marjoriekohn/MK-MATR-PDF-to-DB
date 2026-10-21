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
        component_type = "Question", display_order = 2, text = question, printed_number = "1", self_contained = TRUE,
        variables = list(list(
          variable_type = "closed_ended", display_order = 1, subject = subject,
          response_options = lapply(seq_along(labels), function(i) list(display_order = i, answer_option = labels[i], printed = TRUE))
        ))
      ),
      list(
        component_type = "Question", display_order = 3, text = paste(test_marker, "How many?"), printed_number = "2", self_contained = TRUE,
        variables = list(list(variable_type = "open_ended", display_order = 1, subject = subject, answer_data_type = "NUMBER"))
      )
    )))
  ))
  draft$variables$matr_id <- paste0(matr, "-", tolower(test_marker))
  draft
}

# ------ Before pushing -----------------------------------------------------------------------------------------------
# AHP (questionnaire 1) and its Wave 2, which asks no questions yet.
complete_info <- function() {
  list(info = list(registry_id = "1", study_id = "1", questionnaire_id = "1", wave_id = "2"))
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

  expect_equal(blockers(list(info = list()), draft = draft)[1], "Fill in the registry, study, questionnaire and wave under Basic Info.")
  asking <- blockers(draft = draft, question_counts = c("2" = 4L)) # the wave asks questions already
  expect_equal(asking[1], "Fix the fields outlined in red under Basic Info.")
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
build_for <- function(draft, info = list(questionnaire_id = "7", wave_id = "41"), existing = empty_questionnaire(), options = no_options) {
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
# `existing` with some of its tables replaced.
with_tables <- function(existing, tables) {
  existing[names(tables)] <- tables
  existing
}

# Questionnaire 7, recorded with nothing printed yet, and its waves 40 (asking one question) and 41 (none).
empty_questionnaire <- function() {
  list(
    placements = no_rows(c("component_id", "parent_component_id", "display_order")),
    components = no_rows(c("component_id", "component_type_id", "text")),
    questions = no_rows("component_id"),
    variables = no_rows(c("variable_key", "component_id", "matr_id", "display_order")),
    waves = data.frame(wave_id = c(40, 41), name = c("Wave 1", "Wave 2")),
    wave_questions = data.frame(wave_id = 40, component_id = 5)
  )
}

test_that("a questionnaire with nothing printed yet gets every block, every question's number in the wave, and its variables", {
  built <- build_for(small_draft())
  items <- built$items
  expect_equal(items$item_number, seq_len(nrow(items)))
  expect_true(all(items$action == "Insert")) # an extraction only adds rows
  expect_false(any(c("questionnaire", "wave") %in% items$entity_name)) # both are recorded already
  expect_equal(built$summary[c("blocks", "questions", "variables", "choices")], list(blocks = 4L, questions = 2L, variables = 2L, choices = 2L))
  expect_equal(built$summary[c("answer_options", "option_sets")], list(answer_options = 2L, option_sets = 1L))

  lines <- item_lines(items)
  expect_equal(sum(items$entity_name == "component" & items$attribute_name == "text"), 4)
  expect_equal(sum(items$entity_name == "wave_question" & items$attribute_name == "question_number"), 2)
  expect_true(all(grepl('"wave_id":41', items$record_identifier[items$entity_name == "wave_question"])))
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
  built <- build_for(small_draft(), options = options)
  expect_equal(built$summary[c("answer_options", "option_sets", "option_sets_found")], list(answer_options = 0L, option_sets = 0L, option_sets_found = 1L))
  closed <- built$items[built$items$entity_name == "closed_ended", ]
  expect_equal(closed$new_value, "5") # the set that is there, whatever order its choices are in
  expect_false("answer_option" %in% built$items$entity_name)
  expect_equal(built$items$record_identifier[built$items$entity_name == "response_option"][1], '{"answer_option_id":10,"variable_key":"new:variable:1"}')

  # a different set of the same choices needs its own name if the first is taken
  expect_equal(push_option_set_name(c("Yes", "No"), "Yes / No"), "Yes / No (2)")
  expect_equal(nchar(push_option_set_name(paste("choice", 1:60), character())), 150)
})

test_that("a recorded questionnaire's blocks are found where it prints them, and the rest are added", {
  existing <- with_tables(empty_questionnaire(), list(
    placements = data.frame(component_id = c(100, 101, 102), parent_component_id = c(NA, 100, 100), display_order = c(1, 1, 2)),
    components = data.frame(component_id = c(100, 101, 102), component_type_id = c(1, 4, 2), text = paste(test_marker, c("Part A", "Please answer.", "Do you smoke?"))),
    questions = data.frame(component_id = 102),
    variables = data.frame(variable_key = 900, component_id = 102, matr_id = paste0("zz1-", tolower(test_marker)), display_order = 1)
  ))
  built <- build_for(small_draft(), existing = existing)
  expect_equal(built$summary$blocks_found, 3L) # the section, the instruction and the smoking question
  expect_equal(built$summary$blocks, 1L) # the question about how many is new
  expect_equal(built$summary$variables_found, 1L)
  expect_equal(built$summary$variables, 1L)
  expect_equal(built$summary$questions, 2L) # both are asked in the wave
  wave_questions <- built$items[built$items$entity_name == "wave_question", ]
  expect_true('{"component_id":102,"questionnaire_id":7,"wave_id":41}' %in% wave_questions$record_identifier)
  # the new block goes after what is in its section already
  order <- built$items$new_value[built$items$entity_name == "questionnaire_component" & built$items$attribute_name == "display_order"]
  expect_equal(order, "3")
  parent <- built$items$new_value[built$items$entity_name == "questionnaire_component" & built$items$attribute_name == "parent_component_id"]
  expect_equal(parent, "100")
})

test_that("a MATR ID that another question already has, or nothing to add, is a sentence", {
  existing <- with_tables(empty_questionnaire(), list(
    placements = data.frame(component_id = 102, parent_component_id = NA_real_, display_order = 1),
    components = data.frame(component_id = 102, component_type_id = 2, text = "Some other question"),
    questions = data.frame(component_id = 102),
    variables = data.frame(variable_key = 900, component_id = 102, matr_id = paste0("zz1-", tolower(test_marker)), display_order = 1)
  ))
  expect_error(
    build_for(small_draft(), existing = existing),
    "already used by another question of this questionnaire. Give the variable a MATR ID of its own"
  )
  expect_error(build_for(draft_service_empty()), "nothing in this questionnaire to add")
})

test_that("a push is described in words", {
  built <- build_for(small_draft())
  expect_equal(
    extraction_service_push_summary(built$summary),
    "Adds 4 new blocks, 2 questions asked in the wave, 2 new variables, 2 response options, 2 new answer choices and 1 new set of choices."
  )
  summary <- built$summary
  summary$blocks <- 0L
  summary$blocks_found <- 4L
  summary$variables_found <- 2L
  summary$variables <- 0L
  expect_match(extraction_service_push_summary(summary), "^Adds 4 blocks already there, 2 questions asked in the wave, 2 variables already there")
})

test_that("an extraction goes into a recorded wave that asks no questions yet, and not one that does", {
  built <- build_for(small_draft())
  expect_false("wave" %in% built$items$entity_name) # the wave is there already
  expect_error(build_for(small_draft(), list(questionnaire_id = "7", wave_id = "40")), "already asks 1 question")
  expect_error(build_for(small_draft(), list(questionnaire_id = "7", wave_id = "99")), "isn't one of this questionnaire's waves")
  expect_error(build_for(small_draft(), list(questionnaire_id = "7", wave_id = "41"), existing = NULL), "isn't one of this questionnaire's waves")
})

test_that("repeated choices are new response options, but each answer choice and set of choices is added or reused once", {
  # three variables offering Yes / No: three times two response options, one per variable and choice
  draft <- small_draft()
  question <- draft$components$id[draft$components$component_type == "Question"][1]
  draft <- draft_service_add_box(draft, question)
  draft <- draft_service_add_box(draft, question)
  draft$variables$matr_id <- paste0("zz", seq_len(nrow(draft$variables)))
  added <- build_for(draft)
  expect_equal(added$summary$choices, 6L)
  expect_equal(added$summary[c("answer_options", "option_sets")], list(answer_options = 2L, option_sets = 1L))
  expect_equal(sum(added$items$entity_name == "answer_option"), 2) # Yes and No once each
  expect_equal(length(unique(added$items$record_identifier[added$items$entity_name == "response_option"])), 6)

  options <- list(
    answer_options = data.frame(answer_option_id = c(10, 11), standardized_label = c("Yes", "No")),
    option_sets = data.frame(option_set_id = 5, name = "Yes / No"),
    members = data.frame(option_set_id = c(5, 5), answer_option_id = c(10, 11))
  )
  reused <- build_for(draft, options = options)
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

test_that("the preview says where it goes and what it adds, without writing anything", {
  catalog <- test_catalog()
  context <- list(ids = no_ids, options = no_options)
  info <- extraction_service_info(complete_info())
  existing <- with_tables(empty_questionnaire(), list(waves = data.frame(wave_id = c(1, 2), name = c("Wave 1", "Wave 2"))))
  preview <- extraction_service_request_preview(small_draft(), info, catalog, context, existing)
  where <- vapply(preview$where, function(card) card$kind, character(1))
  expect_equal(where, c("Registry", "Study", "Questionnaire", "Wave"))
  expect_equal(vapply(preview$where, function(card) isTRUE(card$is_new), logical(1)), rep(FALSE, 4)) # all recorded
  expect_equal(preview$where[[4]]$title, "Wave 2")
  expect_null(preview$problem)
  expect_match(preview$summary, "^Adds 4 new blocks")
  html <- htmltools::renderTags(extraction_request_ui(preview, small_draft()))$html
  expect_false(grepl("Where it goes", html)) # Basic Info is the form on Change Requests now
  expect_match(html, "What it adds")
  expect_match(html, "fa-certificate") # the blocks it adds are NEW
  expect_match(html, "NEW")
  expect_false(grepl("Why", html))

  # until it is placed, the cards say what is still to pick, and nothing is worked out
  unplaced <- extraction_service_request_preview(small_draft(), extraction_service_info(), catalog, context)
  expect_true(all(vapply(unplaced$where, function(card) isTRUE(card$is_missing), logical(1))))
  expect_length(unplaced$what, 0)
})

test_that("submitting needs only the questionnaire; its Basic Info is filled in afterwards", {
  lookups <- test_lookups()
  draft <- gold_draft()
  draft$variables$matr_id <- paste0("q", seq_len(nrow(draft$variables)))
  expect_equal(extraction_service_draft_blockers(draft, lookups), character())
  expect_equal(extraction_service_draft_blockers(draft_service_empty(), lookups), "Add the questionnaire's blocks and questions first.")
  expect_match(extraction_service_info_blockers(list(id = "1", info = list()), catalog = test_catalog())[1], "under Basic Info")
  expect_match(extraction_service_draft_summary(draft), "^[0-9,]+ blocks, with [0-9,]+ questions and [0-9,]+ variables\\.$")
})

test_that("a push is titled for the request that carries it", {
  record <- list(pdf = list(name = "x.pdf"), info = complete_info()$info)
  expect_equal(extraction_service_push_title(record, test_catalog()), "Add Adult Health and Personality (AHP) · Wave 2")
})

test_that("the wave records each question's place among the questions, and the number printed beside it if any", {
  draft <- draft_service_renumber_questions(small_draft())
  first <- draft$components$id[draft$components$component_type == "Question"][1]
  draft <- draft_service_update(draft, "components", first, "printed_number", "1a4")
  second <- draft$components$id[draft$components$component_type == "Question"][2]
  draft <- draft_service_update(draft, "components", second, "printed_number", "")
  items <- build_for(draft)$items
  asked <- items[items$entity_name == "wave_question", ]
  expect_equal(asked$new_value[asked$attribute_name == "question_number"], c("1", "2"))
  expect_equal(asked$new_value[asked$attribute_name == "printed_number"], "1a4")

  # printed numbers can repeat, as when each section starts again at 1
  draft <- draft_service_update(draft, "components", second, "printed_number", "1a4")
  asked <- build_for(draft)$items
  expect_equal(asked$new_value[asked$entity_name == "wave_question" & asked$attribute_name == "printed_number"], c("1a4", "1a4"))
})

test_that("who a variable is about: a blank is the respondent, a known subject is used, and a new one is added once", {
  draft <- small_draft(subject = NA)
  items <- build_for(draft)$items
  expect_equal(unique(items$new_value[items$entity_name == "variable" & items$attribute_name == "subject_id"]), "1") # Self
  expect_false("subject" %in% items$entity_name)

  boxes <- draft$variables$id
  draft <- draft_service_update(draft, "variables", boxes[1], "subject", "respondent's family ")
  draft <- draft_service_update(draft, "variables", boxes[2], "subject", "Respondent's Twin")
  built <- build_for(draft)
  items <- built$items
  subject_ids <- items$new_value[items$entity_name == "variable" & items$attribute_name == "subject_id"]
  expect_equal(subject_ids[1], "2") # the one there, whatever its capitals
  expect_equal(items$new_value[items$entity_name == "subject"], "Respondent's Twin")
  expect_equal(subject_ids[2], "new:subject:1")
  expect_equal(built$summary$subjects, 1L)
  expect_match(extraction_service_push_summary(built$summary), "1 new subject")
})
