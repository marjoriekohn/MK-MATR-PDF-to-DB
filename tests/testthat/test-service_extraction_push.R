# Pushing an extraction into the MATR metadata database: what stops it, what the push comes to in words and
# cards, and the push itself. The database does the push (push_extraction in sql/4_functions.sql), so it is
# tested with a real one, into a tree of the test's own that is removed again when the test ends; what stops a
# push and how it is described are pure functions, tested with data given to them.

# ------ Before pushing -----------------------------------------------------------------------------------------------
# An extraction going into AHP (questionnaire 1 of the sample tables) and its Wave 2, which asks no questions.
complete_info <- function() {
  list(id = "9", info = list(registry_id = "1", study_id = "1", questionnaire_id = "1", wave_id = "2"))
}

test_that("what stops a push is said in the order to do it", {
  catalog <- test_catalog()
  lookups <- test_lookups()
  draft <- test_questionnaire_draft()
  expect_equal(extraction_service_draft_blockers(draft, lookups), character())
  expect_equal(extraction_service_draft_blockers(draft_service_empty(), lookups), "Add the questionnaire's blocks and questions first.")

  no_question <- draft
  no_question$components <- no_question$components[no_question$components$component_type != "Question", ]
  no_question$variables <- no_question$variables[0, ]
  no_question$response_options <- no_question$response_options[0, ]
  expect_equal(extraction_service_draft_blockers(no_question, lookups), "Add at least one question.")

  # the blocks the checks mark to fix are counted, a blank MATR ID among them
  missing_id <- test_questionnaire_draft(matr = c(NA, "zz2"))
  expect_equal(length(unique(draft_service_check(missing_id, lookups)$id)), 1)
  expect_equal(extraction_service_draft_blockers(missing_id, lookups), "Fix the 1 block marked to fix.")
  broken <- test_questionnaire_draft(matr = c(NA, NA))
  broken$components$text[broken$components$component_type == "Instruction"] <- NA # an instruction needs its words
  expect_equal(length(unique(draft_service_check(broken, lookups)$id)), 3)
  expect_equal(extraction_service_draft_blockers(broken, lookups), "Fix the 3 blocks marked to fix.")

  # its Basic Info: everything has to be picked, and the wave has to be free
  expect_equal(
    extraction_service_info_blockers(list(id = "9", info = list()), catalog = catalog),
    "Fill in the registry, study, questionnaire and wave under Basic Info."
  )
  expect_equal(
    extraction_service_info_blockers(list(id = "9", info = list(registry_id = "1")), catalog = catalog),
    "Fill in the study, questionnaire and wave under Basic Info."
  )
  expect_equal(extraction_service_info_blockers(complete_info(), catalog = catalog), character())
  expect_equal(
    extraction_service_info_blockers(complete_info(), catalog = catalog, question_counts = c("2" = 12L)),
    "Wave 2 already asks 12 questions. Empty the wave first (Metadata page), or pick a wave without questions."
  )
  elsewhere <- complete_info()
  elsewhere$info$wave_id <- "3" # KQ's wave
  expect_equal(extraction_service_info_blockers(elsewhere, catalog = catalog), "Pick one of this questionnaire's waves.")

  # another draft going into the same wave is in the way; a pushed extraction isn't
  other <- list(
    id = "10", status = "Draft", prepared_by = fixture_other_staff$name, prepared_by_id = fixture_other_staff$staff_id,
    info = complete_info()$info
  )
  expect_equal(
    extraction_service_info_blockers(complete_info(), catalog = catalog, records = list(other), me = fixture_staff),
    "Alex Rivera already has a draft going into Wave 2. Pick another wave, or continue that draft."
  )
  mine <- other
  mine$prepared_by_id <- fixture_staff$staff_id
  expect_match(
    extraction_service_info_blockers(complete_info(), catalog = catalog, records = list(mine), me = fixture_staff),
    "^You already have a draft going into Wave 2\\."
  )
  pushed <- other
  pushed$status <- "Pushed"
  expect_equal(
    extraction_service_info_blockers(complete_info(), catalog = catalog, records = list(pushed, complete_info()), me = fixture_staff),
    character()
  )
})

test_that("blocks not yet marked as checked are counted, but don't stop a push; the questionnaire is summed up", {
  draft <- test_questionnaire_draft()
  expect_equal(extraction_service_unchecked(draft), 4)
  expect_equal(extraction_service_unchecked(with_checked(draft, draft$components$id[1:3])), 1)
  expect_equal(extraction_service_unchecked(with_checked(draft, draft$components$id)), 0)
  expect_equal(extraction_service_draft_blockers(draft, test_lookups()), character())

  expect_equal(extraction_service_draft_summary(draft), "4 blocks, with 2 questions and 2 variables.")
  expect_equal(extraction_service_draft_summary(draft_service_empty()), "0 blocks, with 0 questions and 0 variables.")
  expect_match(extraction_service_draft_summary(gold_draft()), "^[0-9]{1,3}(,[0-9]{3})* blocks, with [0-9,]+ questions and [0-9,]+ variables\\.$")
})

# ------ Said in words and cards ------------------------------------------------------------------------------------
# What the database answers for a questionnaire pushed for the first time, and for one pushed into a second wave
# of a questionnaire that prints it already.
first_counts <- list(
  component_new = 4L, placement_new = 4L, wave_question_new = 2L, variable_new = 2L, response_option_new = 2L,
  answer_option_new = 2L, tag_new = 1L, questions = 2L, extraction_id = 5, wave_id = 41, questionnaire_id = 7
)
again_counts <- list(
  component_reused = 4L, placement_reused = 4L, wave_question_new = 2L, variable_reused = 2L, questions = 2L,
  extraction_id = 6, wave_id = 42, questionnaire_id = 7
)

test_that("a push is described in words", {
  expect_equal(
    extraction_service_push_summary(first_counts),
    "Adds 4 new blocks, 2 questions asked in the wave, 2 new variables, 2 response options, 2 new answer choices and 1 new subject, time frame or unit."
  )
  expect_equal(
    extraction_service_push_summary(again_counts),
    "Adds 4 blocks already there, 4 blocks the questionnaire prints already, 2 questions asked in the wave and 2 variables already there."
  )
  mixed <- list(component_new = 1L, component_reused = 3L, questions = 2L, answer_option_reused = 2L, tag_new = 2L, tag_reused = 1L)
  expect_equal(
    extraction_service_push_summary(mixed),
    paste(
      "Adds 1 new block, 3 blocks already there, 2 questions asked in the wave, 2 answer choices already there,",
      "2 new subjects, time frames or units and 1 subject, time frame or unit already there."
    )
  )
  expect_equal(extraction_service_push_summary(list()), "Adds nothing.")
})

test_that("the cards for what a push adds count the rows added and the rows found, and are NEW when any is added", {
  cards <- extraction_service_what_cards(first_counts)
  expect_equal(vapply(cards, function(card) card$kind, character(1)), c("Blocks", "Variables", "Answer choices", "Subjects, time frames and units"))
  expect_equal(cards[[1]][c("title", "facts", "is_new")], list(title = "4 blocks", facts = "4 new", is_new = TRUE))
  expect_equal(cards[[2]][c("title", "facts", "is_new")], list(title = "2 variables", facts = "2 new", is_new = TRUE))
  expect_equal(cards[[3]][c("title", "facts", "is_new")], list(title = "2 different answer choices", facts = "2 new", is_new = TRUE))
  expect_equal(cards[[4]][c("title", "facts", "is_new")], list(title = "1 tag", facts = "1 new", is_new = TRUE))
  expect_true(all(vapply(cards, function(card) identical(card$subtitle, ""), logical(1))))

  # a second wave of a questionnaire adds no block, and finds the blocks the questionnaire prints
  again <- extraction_service_what_cards(again_counts)
  expect_equal(again[[1]]$title, "4 blocks")
  expect_equal(again[[1]]$facts, c("4 already in the database", "4 printed by this questionnaire already"))
  expect_false(again[[1]]$is_new)
  expect_equal(again[[2]][c("title", "facts", "is_new")], list(title = "2 variables", facts = "2 already recorded", is_new = FALSE))
  expect_equal(again[[3]]$title, "0 different answer choices")
  expect_length(again[[3]]$facts, 0)
  expect_false(again[[3]]$is_new)

  mixed <- extraction_service_what_cards(list(component_new = 1L, component_reused = 3L, tag_reused = 2L))
  expect_equal(mixed[[1]][c("title", "facts", "is_new")], list(title = "4 blocks", facts = c("1 new", "3 already in the database"), is_new = TRUE))
  expect_equal(mixed[[4]][c("title", "facts", "is_new")], list(title = "2 tags", facts = "2 already in the database", is_new = FALSE))
  # a card is drawn with its NEW badge only when it adds rows
  expect_match(as.character(request_card_ui(cards[[1]])), "NEW")
  expect_false(grepl("NEW", as.character(request_card_ui(again[[1]]))))
})

test_that("the cards for where a push goes name what is picked, and say what isn't picked yet", {
  catalog <- test_catalog()
  where <- extraction_service_where_cards(extraction_service_info(complete_info()), catalog)
  expect_equal(vapply(where, function(card) card$kind, character(1)), c("Registry", "Study", "Questionnaire", "Wave"))
  expect_false(any(vapply(where, function(card) isTRUE(card$is_new) || isTRUE(card$is_missing), logical(1)))) # all recorded
  expect_equal(where[[1]]$title, "Mid-Atlantic Twin Registry (MATR)")
  expect_equal(where[[2]][c("title", "subtitle")], list(title = "Adult Health Study", subtitle = "Led by Dana Reyes"))
  expect_equal(
    where[[3]][c("title", "subtitle", "facts")],
    list(title = "Adult Health and Personality (AHP)", subtitle = "For Adult · already in the database", facts = "2 waves")
  )
  expect_equal(
    where[[4]][c("title", "subtitle", "facts")],
    list(title = "Wave 2", subtitle = "Already in the database, with no questions yet", facts = c("Given 04/15/2022", "1,210 respondents"))
  )

  nothing <- extraction_service_where_cards(extraction_service_info(), catalog)
  expect_true(all(vapply(nothing, function(card) isTRUE(card$is_missing), logical(1))))
  expect_equal(unique(vapply(nothing, function(card) card$title, character(1))), "Not picked yet")
  # picked in order: the registry first
  some <- extraction_service_where_cards(extraction_service_info(list(info = list(registry_id = "1"))), catalog)
  expect_equal(vapply(some, function(card) isTRUE(card$is_missing), logical(1)), c(FALSE, TRUE, TRUE, TRUE))
})

test_that("the preview puts the cards, the sentence and the problem together, and says when nothing was tried", {
  catalog <- test_catalog()
  draft <- test_questionnaire_draft()
  info <- extraction_service_info(complete_info())

  preview <- extraction_service_push_preview_ui(draft, info, catalog, first_counts)
  expect_equal(names(preview), c("where", "what", "summary", "problem", "unchecked"))
  expect_length(preview$where, 4)
  expect_length(preview$what, 4)
  expect_match(preview$summary, "^Adds 4 new blocks, ")
  expect_null(preview$problem)
  expect_equal(preview$unchecked, 4)
  html <- htmltools::renderTags(extraction_request_ui(preview, draft, questionnaire = FALSE))$html
  expect_match(html, "What it adds")
  expect_match(html, "NEW")
  expect_false(grepl("can't be pushed", html, fixed = TRUE))

  # the database refused: its sentence is the problem, and there is nothing to add
  refusal <- "The wave Wave 2 of Adult Health and Personality already asks 12 question(s). Empty it first (Metadata > the wave > Empty), or pick a wave that asks none."
  refused <- extraction_service_push_preview_ui(with_checked(draft, draft$components$id), info, catalog, simpleError(refusal))
  expect_equal(refused$problem, refusal)
  expect_length(refused$what, 0)
  expect_equal(refused$summary, "")
  expect_equal(refused$unchecked, 0)
  html <- htmltools::renderTags(extraction_request_ui(refused, draft, questionnaire = FALSE))$html
  expect_match(html, "This can't be pushed as it is", fixed = TRUE)
  expect_match(html, "Empty it first", fixed = TRUE)
  expect_false(grepl("What it adds", html, fixed = TRUE))

  # not tried, as the Basic Info isn't complete: the cards say what to pick, and nothing is worked out
  not_tried <- extraction_service_push_preview_ui(draft, extraction_service_info(), catalog, NULL)
  expect_true(all(vapply(not_tried$where, function(card) isTRUE(card$is_missing), logical(1))))
  expect_length(not_tried$what, 0)
  expect_equal(not_tried$summary, "")
  expect_null(not_tried$problem)
})

# ------ The push -------------------------------------------------------------------------------------------------------
# The small questionnaire with words of its own: a run's tests all push test_questionnaire_draft(), and an answer
# choice or a tag is kept once for the whole database until the run ends, so a push of the same words would find
# them there. Each block, choice and unit still starts with the run's marker, so it is removed when the run ends.
marked_draft <- function(matr = c("zz1", "zz2")) {
  draft <- test_questionnaire_draft(matr)
  own <- test_name("push")
  draft$components$text <- paste(draft$components$text, own)
  draft$response_options$answer_option <- paste(draft$response_options$answer_option, own)
  draft$variables$unit <- ifelse(is.na(draft$variables$unit), NA_character_, paste(draft$variables$unit, own))
  draft
}

# A draft typed by hand going into the tree's wave (or the wave given), its questionnaire saved: as the Edit
# Extraction page leaves it before Push.
saved_draft <- function(me, tree, draft, wave_id = tree$wave$wave_id, env = parent.frame()) {
  typed <- picked_basic_info(tree, wave_id = wave_id)
  record <- do.call(local_manual_extraction, c(list(me, env = env), typed))
  draft_service_save(test_connection, record$id, draft, me$staff_id)
  extraction_model_get(test_connection, record$id)
}

test_that("a push records the questionnaire in its wave, and the extraction is kept as Pushed", {
  me <- local_staff()
  tree <- local_tree()
  draft <- marked_draft()
  pushed <- local_pushed_questionnaire(me, tree, draft)
  counts <- pushed$counts

  expect_equal(pushed$record$status, "Pushed")
  expect_false(is.null(pushed$record$pushed))
  expect_equal(counts$questions, 2)
  expect_equal(counts$component_new, 4) # the section, the instruction and the two questions
  expect_equal(counts$placement_new, 4)
  expect_equal(counts$wave_question_new, 2)
  expect_equal(counts$variable_new, 2)
  expect_equal(counts$response_option_new, 2)
  expect_equal(counts$answer_option_new, 2) # yes and no, as marked
  expect_equal(counts$tag_new, 1) # the unit
  expect_null(counts$component_reused)
  expect_null(counts$variable_reused)
  expect_equal(as.character(counts$wave_id), as.character(tree$wave$wave_id))
  expect_equal(as.character(counts$extraction_id), pushed$id)

  # the wave asks the two questions, each with its place and the number printed beside it
  wave <- list(wave_id = database_eq(tree$wave$wave_id))
  expect_equal(database_model_count(test_connection, "wave_question", wave), 2)
  asked <- database_model_get(test_connection, "wave_question", c("question_number", "printed_number"), filters = wave)
  expect_equal(asked$question_number, c("1", "2"))
  expect_equal(asked$printed_number, c("1", "2"))
  # the questionnaire records the variables by their MATR IDs
  variables <- database_model_get(
    test_connection, "variable", c("matr_id", "variable_type"),
    filters = list(questionnaire_id = database_eq(tree$questionnaire$questionnaire_id))
  )
  expect_setequal(variables$matr_id, draft$variables$matr_id)
  expect_setequal(variables$variable_type, c("closed_ended", "open_ended"))

  # it has left the drafts, and is still there as Pushed
  expect_false(pushed$id %in% names(extraction_service_get_all(test_connection)))
  kept <- extraction_model_get(test_connection, pushed$id)
  expect_equal(kept$status, "Pushed")
  expect_true(same_staff(kept$saved_by_id, me$staff_id))
  # and can't be pushed twice
  catalog <- questionnaire_service_get_catalog(test_connection)
  expect_match(error_text(extraction_service_push(test_connection, kept, me, catalog)), "pushed already")
})

test_that("a wave that asks questions already, and a questionnaire with none, are refused with what to do", {
  me <- local_staff()
  tree <- local_tree()
  catalog <- questionnaire_service_get_catalog(test_connection)
  local_pushed_questionnaire(me, tree, marked_draft())

  # the wave asks questions now: another draft can't go into it until it is emptied
  other <- saved_draft(me, tree, marked_draft())
  refused <- error_text(extraction_service_push(test_connection, other, me, catalog))
  expect_match(refused, "already asks 2 question")
  expect_match(refused, "Empty it first (Metadata > the wave > Empty)", fixed = TRUE)
  expect_match(error_text(extraction_service_push_preview(test_connection, other, me)), "Empty it first", fixed = TRUE)
  expect_equal(extraction_model_get(test_connection, other$id)$status, "Draft") # nothing was written
  expect_equal(database_model_count(test_connection, "wave_question", list(wave_id = database_eq(tree$wave$wave_id))), 2)

  # a draft typed by hand whose questionnaire was never given a question
  free <- local_wave(tree$questionnaire)
  typed <- do.call(local_manual_extraction, c(list(me), picked_basic_info(tree, wave_id = free$wave_id)))
  expect_match(error_text(extraction_service_push(test_connection, typed, me, catalog)), "no questions")
  expect_equal(database_model_count(test_connection, "wave_question", list(wave_id = database_eq(free$wave_id))), 0)
})

test_that("the preview says what the push would do, and writes nothing", {
  me <- local_staff()
  tree <- local_tree()
  record <- saved_draft(me, tree, marked_draft())

  tried <- extraction_service_push_preview(test_connection, record, me)
  expect_equal(tried$questions, 2)
  expect_equal(tried$component_new, 4)
  expect_equal(tried$variable_new, 2)
  expect_equal(tried$answer_option_new, 2)
  expect_equal(tried$tag_new, 1)
  expect_equal(database_model_count(test_connection, "wave_question", list(wave_id = database_eq(tree$wave$wave_id))), 0)
  expect_equal(database_model_count(test_connection, "placement", list(questionnaire_id = database_eq(tree$questionnaire$questionnaire_id))), 0)
  expect_equal(extraction_model_get(test_connection, record$id)$status, "Draft")

  # the push then does exactly what the preview said
  done <- extraction_service_push(test_connection, record, me, questionnaire_service_get_catalog(test_connection))
  did <- c("component_new", "placement_new", "wave_question_new", "variable_new", "response_option_new", "answer_option_new", "tag_new", "questions")
  expect_equal(done$counts[did], tried[did])
  expect_equal(done$record$status, "Pushed")
})

test_that("a second wave of the same questionnaire reuses its blocks and variables, and only asks the questions anew", {
  me <- local_staff()
  tree <- local_tree()
  draft <- marked_draft() # the same words and MATR IDs both times
  local_pushed_questionnaire(me, tree, draft)
  later <- local_wave(tree$questionnaire)
  again <- saved_draft(me, tree, draft, wave_id = later$wave_id)

  outcome <- extraction_service_push(test_connection, again, me, questionnaire_service_get_catalog(test_connection))
  counts <- outcome$counts
  expect_equal(counts$component_reused, 4)
  expect_equal(counts$placement_reused, 4)
  expect_equal(counts$variable_reused, 2)
  expect_equal(counts$wave_question_new, 2)
  expect_equal(counts$questions, 2)
  expect_null(counts$component_new)
  expect_null(counts$placement_new)
  expect_null(counts$variable_new)
  expect_null(counts$answer_option_new) # a variable found again keeps the choices it has
  expect_equal(outcome$record$status, "Pushed")
  expect_match(extraction_service_push_summary(counts), "^Adds 4 blocks already there, 4 blocks the questionnaire prints already, 2 questions asked in the wave and 2 variables already there\\.$")

  # the questionnaire prints the four blocks once, with two variables, and each wave asks the two questions
  questionnaire <- list(questionnaire_id = database_eq(tree$questionnaire$questionnaire_id))
  expect_equal(database_model_count(test_connection, "placement", questionnaire), 4)
  expect_equal(database_model_count(test_connection, "variable", questionnaire), 2)
  expect_equal(database_model_count(test_connection, "wave_question", list(wave_id = database_eq(tree$wave$wave_id))), 2)
  expect_equal(database_model_count(test_connection, "wave_question", list(wave_id = database_eq(later$wave_id))), 2)
})

test_that("saving the Basic Info writes where the extraction goes and what it is called, and nothing when nothing changed", {
  me <- local_staff()
  tree <- local_tree()
  catalog <- questionnaire_service_get_catalog(test_connection)
  record <- local_manual_extraction(me, questionnaire_id = "", wave_id = "")
  expect_equal(extraction_service_info(record), extraction_service_info())
  expect_null(record$summary)

  typed <- picked_basic_info(tree)
  saved <- extraction_service_save_info(test_connection, record, typed, me, catalog)
  expect_equal(extraction_service_info(saved), typed)
  expect_equal(saved$summary, paste(name_with_abbreviation(tree$questionnaire$name, tree$questionnaire$abbreviation), tree$wave$name, sep = " · "))
  expect_gt(saved$revision, record$revision)
  expect_true(same_staff(saved$saved_by_id, me$staff_id))

  # the same again changes nothing, and asks the database nothing
  before <- test_requests$count
  again <- extraction_service_save_info(test_connection, saved, typed, me, catalog)
  expect_equal(test_requests$count, before)
  expect_equal(again$revision, saved$revision)
  expect_equal(extraction_model_get(test_connection, record$id)$revision, saved$revision)

  # not over a save made since the record was read
  expect_match(error_text(extraction_service_save_info(test_connection, record, typed, me, catalog)), "changed after you opened it")
})
