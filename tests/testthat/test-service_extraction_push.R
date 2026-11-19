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
  answer_option_new = 2L, tag_new = 1L, question_new = 2L, option_set_new = 1L, questions = 2L, extraction_id = 5, wave_id = 41, questionnaire_id = 7,
  names = list(answer_option_new = list("no", "yes"), tag_new = list("years"), option_set_new = list("yes | no"))
)
again_counts <- list(
  component_reused = 4L, placement_reused = 4L, wave_question_new = 2L, variable_reused = 2L, question_reused = 2L, option_set_reused = 1L,
  questions = 2L, extraction_id = 6, wave_id = 42, questionnaire_id = 7,
  names = list(question_reused = list("do you smoke?", "how tall are you?"), option_set_reused = list("yes | no"))
)

test_that("a push is described in words", {
  expect_equal(
    extraction_service_push_summary(first_counts),
    "Adds 4 new blocks, 2 questions asked in the wave, 2 new variables, 2 response options, 1 new option set, 2 new answer choices and 1 new subject, time frame or unit."
  )
  expect_equal(
    extraction_service_push_summary(again_counts),
    "Adds 4 blocks already there, 4 blocks the questionnaire prints already, 2 questions asked in the wave, 2 variables already there and 1 option set already there."
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

test_that("the cards for what a push adds count each question, answer choice and tag once, new or already there, and name them", {
  cards <- extraction_service_what_cards(first_counts)
  expect_equal(vapply(cards, function(card) card$kind, character(1)), c("Questions", "Option sets", "Answer choices", "Subjects, time frames and units"))
  expect_equal(cards[[1]][c("title", "facts", "is_new")], list(title = "2 different questions", facts = "2 new", is_new = TRUE))
  expect_equal(cards[[2]][c("title", "facts", "is_new")], list(title = "1 option set", facts = "1 new", is_new = TRUE))
  expect_equal(cards[[3]][c("title", "facts", "is_new")], list(title = "2 different answer choices", facts = "2 new", is_new = TRUE))
  expect_equal(cards[[4]][c("title", "facts", "is_new")], list(title = "1 tag", facts = "1 new", is_new = TRUE))
  expect_true(all(vapply(cards, function(card) identical(card$subtitle, ""), logical(1))))
  # what each chip counts is listed for pointing at it: the option set by its choices, the choices and the tag by name
  expect_equal(cards[[2]]$lists, list("1 new" = "yes | no"))
  expect_equal(cards[[3]]$lists, list("2 new" = c("no", "yes")))
  expect_equal(cards[[4]]$lists, list("1 new" = "years"))
  expect_length(cards[[1]]$lists, 0) # new questions aren't listed: they are the questionnaire below

  # a second wave of a questionnaire adds no question, and finds the ones the questionnaire prints
  again <- extraction_service_what_cards(again_counts)
  expect_equal(again[[1]]$title, "2 different questions")
  expect_equal(again[[1]]$facts, c("2 already in the database", "4 blocks printed by this questionnaire already"))
  expect_equal(again[[1]]$lists, list("2 already in the database" = c("do you smoke?", "how tall are you?")))
  expect_false(again[[1]]$is_new)
  expect_equal(again[[2]][c("title", "facts", "is_new")], list(title = "1 option set", facts = "1 already in the database", is_new = FALSE))
  expect_equal(again[[3]]$title, "0 different answer choices")
  expect_length(again[[3]]$facts, 0)
  expect_false(again[[3]]$is_new)

  # the tags the questionnaire uses, each once: 5 it adds and 7 the database has, however many variables use them
  mq <- extraction_service_what_cards(list(
    question_new = 234L, questions = 239L, tag_new = 5L, tag_reused = 7L,
    names = list(tag_new = list("past month", "spouse/partner"), tag_reused = list("lifetime", "twin a", "twin b"))
  ))
  expect_equal(mq[[4]][c("title", "facts", "is_new")], list(title = "12 tags", facts = c("5 new", "7 already in the database"), is_new = TRUE))
  expect_equal(mq[[4]]$lists[["7 already in the database"]], c("lifetime", "twin a", "twin b"))
  expect_equal(mq[[1]]$title, "234 different questions")

  # a card is drawn with its NEW badge only when it adds rows, and a chip that has a list says it in a tooltip
  html <- as.character(request_card_ui(mq[[4]]))
  expect_match(html, "NEW")
  expect_false(grepl("NEW", as.character(request_card_ui(again[[1]]))))
  # every name has a bullet of its own, in the tooltip of the chip that counts it
  expect_match(html, "<bslib-tooltip[^>]*>.*<li>lifetime</li>\\s*<li>twin a</li>\\s*<li>twin b</li>.*7 already in the database")
  expect_match(html, 'class="row-badge has-list" tabindex="0"', fixed = TRUE)
  expect_match(as.character(request_card_ui(again[[1]])), "<li>do you smoke\\?</li>")
  expect_match(as.character(request_card_ui(again[[2]])), "<li>yes \\| no</li>")
  # a long list is shown whole, however long a name is, in columns of at most 30 so it stays on the screen
  long <- strrep("a very long answer choice ", 8)
  many <- as.character(request_names_ui(c(sprintf("choice %03d", 1:149), long)))
  expect_equal(lengths(regmatches(many, gregexpr("<li>", many))), 150)
  expect_match(many, trimws(long), fixed = TRUE)
  expect_false(grepl("more|\u2026", many))
  expect_match(many, 'class="list-tooltip-lines is-rows-30"', fixed = TRUE) # 150 names: five columns of thirty
  expect_match(as.character(request_names_ui(sprintf("choice %d", 1:31))), "is-rows-16") # two even columns, not 30 and 1
  expect_match(as.character(request_names_ui(c("yes", "no"))), "is-rows-2")
  # the stylesheet has a class for every length a column can be
  styles <- paste(readLines(app_file("www", "sass", "layout", "_history.scss"), warn = FALSE), collapse = "\n")
  expect_match(styles, sprintf("@for $rows from 1 through %d", request_names_per_column), fixed = TRUE)

  # the database's older answer (no questions counted apart) still says how many the wave asks
  older <- extraction_service_what_cards(list(component_new = 4L, questions = 2L))
  expect_equal(older[[1]][c("title", "facts")], list(title = "2 questions", facts = character()))
})

test_that("a push that puts questions in a measurement scale says so, with a card that names the scales", {
  counts <- list(
    questions = 3L, question_new = 3L, measurement_scale_new = 1L, measurement_scale_reused = 1L, scale_item_new = 3L,
    names = list(measurement_scale_new = list("XYZ"), measurement_scale_reused = list("EPQ-R"))
  )
  expect_match(extraction_service_push_summary(counts), "1 new measurement scale and 3 questions put in a measurement scale\\.$")
  cards <- extraction_service_what_cards(counts)
  expect_length(cards, 5)
  expect_equal(cards[[5]]$kind, "Measurement scales")
  expect_equal(cards[[5]]$title, "2 measurement scales")
  expect_equal(cards[[5]]$facts, c("1 new", "1 already in the database"))
  expect_equal(cards[[5]]$lists, list("1 new" = "XYZ", "1 already in the database" = "EPQ-R"))
  expect_true(cards[[5]]$is_new)
  # a questionnaire that names no scale has no such card, and its sentence says nothing of scales
  expect_length(extraction_service_what_cards(list(questions = 3L, question_new = 3L)), 4)
  expect_false(grepl("scale", extraction_service_push_summary(list(questions = 3L))))
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
  expect_length(preview$what, 4) # questions, option sets, answer choices, tags
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

test_that("a question whose extraction names a measurement scale is put in it, and a scale the database hasn't got is added", {
  me <- local_staff()
  tree <- local_tree()
  known <- local_row("measurement_scale", list(name = test_name("Mood Scale"), abbreviation = test_abbreviation()), "scale_id")
  unknown <- test_abbreviation()
  draft <- test_questionnaire_draft()
  questions <- draft$components$id[draft$components$component_type == "Question"]
  draft$components$scale[draft$components$id == questions[1]] <- tolower(known$abbreviation) # found whatever its capitals
  draft$components$scale[draft$components$id == questions[2]] <- unknown
  # the items go before the scales and the tree's components can
  withr::defer({
    added <- database_model_get(test_connection, "measurement_scale", "scale_id", filters = list(abbreviation = database_eq(unknown)))
    for (scale in c(known$scale_id, added$scale_id)) test_delete("question_scale", list(scale_id = database_eq(scale)))
    test_delete("measurement_scale", list(abbreviation = database_eq(unknown)))
  })
  pushed <- local_pushed_questionnaire(me, tree, draft)
  expect_equal(pushed$counts$measurement_scale_reused, 1)
  expect_equal(pushed$counts$measurement_scale_new, 1)
  expect_equal(pushed$counts$scale_item_new, 2)
  expect_equal(unlist(pushed$counts$names$measurement_scale_new), unknown)
  expect_equal(unlist(pushed$counts$names$measurement_scale_reused), known$abbreviation)

  # each question's wording is an item of its scale, and the new scale is named by its abbreviation for now
  item_of <- function(scale_id) {
    items <- database_model_get(test_connection, "question_scale", c("scale_id", "component_id"), filters = list(scale_id = database_eq(scale_id)))
    database_model_get(test_connection, "component", c("component_id", "text"), filters = list(component_id = database_in(items$component_id)))$text
  }
  expect_equal(tolower(item_of(known$scale_id)), tolower(paste(test_marker, "Do you smoke?")))
  added <- database_model_get(test_connection, "measurement_scale", c("scale_id", "name", "abbreviation"), filters = list(abbreviation = database_eq(unknown)))
  expect_equal(added$name, unknown)
  expect_equal(tolower(item_of(added$scale_id)), tolower(paste(test_marker, "How many?")))
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
  # each question, choice and tag is counted once, and the choices and tags it adds are named for the cards
  expect_equal(tried$question_new, 2)
  expect_null(tried$question_reused)
  expect_length(unlist(tried$names$answer_option_new), 2)
  expect_length(unlist(tried$names$tag_new), 1)
  expect_equal(tried$option_set_new, 1) # the one variable with choices offers one set of them
  expect_length(unlist(tried$names$option_set_new), 1)
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
  expect_equal(counts$question_reused, 2) # both questions are the database's already, and are named
  expect_length(unlist(counts$names$question_reused), 2)
  expect_equal(counts$option_set_reused, 1) # and so is the set of choices the variable found again offers
  expect_equal(outcome$record$status, "Pushed")
  expect_match(extraction_service_push_summary(counts), "^Adds 4 blocks already there, 4 blocks the questionnaire prints already, 2 questions asked in the wave, 2 variables already there and 1 option set already there\\.$")

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

# A draft reopened from a wave: two blocks from one component (printed twice in the wave), one from a component
# another questionnaire prints too, one printed nowhere else, and one staff added.
reopened_draft <- function() {
  asked <- function(text, component_id, number) {
    list(component_type = "Question", text = text, component_id = component_id, question_number = number, variables = list(
      list(variable_type = "open_ended", answer_data_type = "NUMBER", matr_id = paste0("zz", number))
    ))
  }
  draft_service_from_json(list(schema_version = 2L, questionnaire = list(), components = list(
    asked("i accomplished less than i wanted to.", 11L, "1"), asked("i accomplished less than i wanted to.", 11L, "2"),
    asked("do you smoke?", 22L, "3"), asked("how tall are you?", 33L, "4"),
    list(component_type = "Instruction", text = "count cigars too.")
  )))
}
catalog_words <- function() {
  list(
    components = data.frame(component_id = c("11", "22", "33"), kind = "Question", text = c("i accomplished less than i wanted to.", "do you smoke?", "how tall are you?")),
    places = data.frame(component_id = c("11", "11", "22", "22", "22", "33"), questionnaire_id = c("1", "1", "1", "2", "3", "1"))
  )
}

test_that("a reopened block reworded while other places print its component is listed for a choice: everywhere, or only here", {
  catalog <- list(questionnaires = data.frame(questionnaire_id = c("1", "2", "3"), name = c("Adult Health", "Kid Questionnaire", "Mother's Questionnaire")))
  draft <- reopened_draft()
  id_of <- function(number) draft$components$id[draft$components$question_number %in% number]
  expect_equal(nrow(extraction_service_rewordings(draft, catalog_words(), catalog, "1")), 0) # nothing reworded yet

  draft <- draft_service_update(draft, "components", id_of("1"), "text", "I got less done than I wanted to.")
  draft <- draft_service_update(draft, "components", id_of("3"), "text", "Do you smoke cigarettes?")
  draft <- draft_service_update(draft, "components", id_of("4"), "text", "What is your height?") # printed nowhere else: no choice to make
  choices <- extraction_service_rewordings(draft, catalog_words(), catalog, "1")
  expect_equal(choices$id, c(id_of("1"), id_of("3")))
  expect_equal(choices$was, c("i accomplished less than i wanted to.", "do you smoke?"))
  expect_equal(choices$now, c("i got less done than i wanted to.", "do you smoke cigarettes?"))
  expect_equal(choices$everywhere, c(FALSE, FALSE)) # only here until chosen
  expect_equal(choices$elsewhere, c("1 other place in this questionnaire", "Kid Questionnaire and Mother's Questionnaire"))

  # chosen everywhere, the other block of this draft follows and the list says so; a kind changed is a new block
  everywhere <- draft_service_reword(draft, id_of("1"), TRUE)
  expect_equal(extraction_service_rewordings(everywhere, catalog_words(), catalog, "1")$everywhere, c(TRUE, FALSE))
  as_note <- draft_service_update(draft, "components", id_of("3"), "component_type", "Note")
  expect_equal(extraction_service_rewordings(as_note, catalog_words(), catalog, "1")$id, id_of("1"))

  html <- htmltools::renderTags(rewordings_ui(choices))$html
  expect_match(html, "Kid Questionnaire and Mother(&#39;|')s Questionnaire")
  expect_match(html, sprintf('data-action="reword" data-id="%s" data-everywhere="true"', id_of("1")), fixed = TRUE)
  expect_match(html, sprintf('data-action="reword" data-id="%s" data-everywhere="false"', id_of("3")), fixed = TRUE)
  expect_null(rewordings_ui(choices[0, ]))
})

test_that("a push that replaces what a wave asked says so", {
  counts <- list(questions = 188L, questions_before = 190L, component_reused = 219L, variable_new = 336L)
  expect_match(extraction_service_push_summary(counts), "^Replaces the 190 questions the wave asks now\\. Adds 219 blocks already there, ")
  expect_false(grepl("Replaces", extraction_service_push_summary(counts[-2])))
})

test_that("a pushed wave is reopened as a draft of what it asks, and pushing that replaces it, keeping its components", {
  me <- local_staff()
  tree <- local_tree()
  draft <- marked_draft()
  local_pushed_questionnaire(me, tree, draft)
  wave <- list(wave_id = database_eq(tree$wave$wave_id))

  reopened <- extraction_service_reopen_wave(test_connection, tree$wave$wave_id, me)
  expect_equal(reopened$status, "Draft")
  expect_equal(extraction_service_updates_wave(reopened), as.character(tree$wave$wave_id))
  expect_equal(extraction_service_reopen_wave(test_connection, tree$wave$wave_id, me)$id, reopened$id) # asked for twice: the same draft
  copy <- draft_service_load(test_connection, reopened$id)
  expect_equal(copy$components$text, draft$components$text)
  expect_setequal(copy$variables$matr_id, draft$variables$matr_id)
  expect_equal(nrow(copy$response_options), nrow(draft$response_options))
  expect_false(anyNA(copy$components$catalog_id)) # each block names the component it came from
  expect_true(all(copy$components$checked))
  # the wave's own questions aren't in its way
  catalog <- questionnaire_service_get_catalog(test_connection)
  asked <- questionnaire_service_wave_question_counts(test_connection, tree$questionnaire$questionnaire_id)
  expect_length(extraction_service_info_blockers(reopened, catalog = catalog, records = list(reopened), question_counts = asked, me = me), 0)

  # a question reworded: nothing else prints it, so there is no choice to make and its component takes the words
  question <- copy$components$id[copy$components$component_type == "Question"][1]
  component <- copy$components$catalog_id[copy$components$id == question]
  reworded <- paste(copy$components$text[copy$components$id == question], "again")
  copy <- draft_service_update(copy, "components", question, "text", reworded)
  expect_equal(nrow(extraction_service_read_rewordings(test_connection, reopened, copy, catalog)), 0)
  draft_service_save(test_connection, reopened$id, copy, me$staff_id)
  outcome <- extraction_service_push(test_connection, extraction_model_get(test_connection, reopened$id), me, catalog)

  expect_equal(outcome$record$status, "Pushed")
  expect_equal(outcome$counts$questions, 2)
  expect_equal(outcome$counts$questions_before, 2)
  expect_null(outcome$counts$component_new) # the reworded component is the same one, with new words
  expect_equal(component_model_places(test_connection, component)$components$text, normal_text(reworded)) # kept in lower case
  expect_equal(database_model_count(test_connection, "wave_question", wave), 2)
  variables <- database_model_get(
    test_connection, "variable", "matr_id", filters = list(questionnaire_id = database_eq(tree$questionnaire$questionnaire_id))
  )
  expect_setequal(variables$matr_id, draft$variables$matr_id)

  # a wave that asks nothing has nothing to reopen
  database_model_call(test_connection, "empty_wave", list(p_wave_id = database_number(tree$wave$wave_id), p_staff_id = database_number(me$staff_id)))
  expect_match(error_text(extraction_service_reopen_wave(test_connection, tree$wave$wave_id, me)), "asks no questions yet")
})

test_that("a wave the AI read is reopened as an AI-Powered draft with that reading, so a block can still be put back to it", {
  me <- local_staff()
  # The PDF's row is made first, so it is removed last: once pushed, the tree's wave names it (and so do the two
  # extractions, which are removed before the tree).
  pdf <- local_row("pdf_file", list(pdf_sha256 = test_sha256(), pdf_pages = 2, pdf_bytes = 51), "pdf_sha256")
  tree <- local_tree()
  read <- extraction_model_create(test_connection, sample_extraction(
    me, extraction_status = "ready", info = picked_basic_info(tree),
    pdf = list(name = "AHP_wave3.pdf", path = NULL, pages = 2L, bytes = 51L, sha256 = pdf$pdf_sha256)
  ))
  withr::defer(test_delete_extraction(read$id))
  database_model_update(
    test_connection, extraction_table, list(ai_original = draft_service_to_json(test_questionnaire_draft())),
    filters = list(extraction_id = database_eq(read$id)), select = "extraction_id"
  )
  # reviewed before it was pushed: the question printed as 1 is reworded
  first <- extraction_service_open_draft(test_connection, extraction_model_get(test_connection, read$id))
  asked_first <- function(draft) draft$components$id[draft$components$printed_number %in% "1"]
  reviewed <- draft_service_update(first$draft, "components", asked_first(first$draft), "text", paste(test_marker, "Do you smoke now?"))
  extraction_service_save_draft(test_connection, read, reviewed, first$original, me)
  catalog <- questionnaire_service_get_catalog(test_connection)
  extraction_service_push(test_connection, extraction_model_get(test_connection, read$id), me, catalog)

  reopened <- extraction_service_reopen_wave(test_connection, tree$wave$wave_id, me)
  withr::defer(test_delete_extraction(reopened$id))
  expect_equal(reopened$method, extraction_ai) # not typed by hand
  expect_equal(reopened$extraction_status, "ready") # nothing to read
  expect_equal(as.character(reopened$info$reopened_from), as.character(read$id))

  # its questionnaire came back from the database without the rows' refs: they are matched through the pushed copy
  opened <- extraction_service_open_draft(test_connection, reopened)
  expect_false(is.null(opened$original))
  expect_false(anyNA(opened$draft$components$ref))
  reworded <- asked_first(opened$draft)
  expect_length(reworded, 1)
  expect_equal(component(opened$draft, reworded)$text, component(reviewed, asked_first(reviewed))$text)
  expect_true(reworded %in% draft_service_changed_blocks(opened$original, opened$draft)) # as it differed when it was pushed
  back <- suppressMessages(draft_service_revert_block(opened$draft, opened$original, reworded))
  expect_false(reworded %in% draft_service_changed_blocks(opened$original, back))
  expect_equal(component(back, reworded)$text, component(first$original, asked_first(first$original))$text) # the AI's words
  expect_setequal(stats::na.omit(back$variables$matr_id), stats::na.omit(opened$draft$variables$matr_id)) # MATR IDs aren't the AI's: kept

  # saved, its rows say which of the AI's they are, and the corrections are worked out from the same reading
  extraction_service_save_draft(test_connection, reopened, back, opened$original, me)
  again <- extraction_service_open_draft(test_connection, extraction_model_get(test_connection, reopened$id))
  expect_false(anyNA(again$draft$components$ref))
  expect_false(any(vapply(review_edits_of(test_connection, reopened$id), function(edit) identical(edit$field, "text"), logical(1))))

  # it isn't read again from here (that is the wave's own Re-extract), and the AI Monitor counts the reading once
  expect_match(error_text(extraction_service_reextract(test_connection, reopened$id, me)), "Re-extract on the wave")
  outcome <- extraction_service_push(
    test_connection, extraction_model_get(test_connection, reopened$id), me, questionnaire_service_get_catalog(test_connection)
  )
  expect_equal(outcome$record$status, "Pushed")
  listed <- names(monitor_model_get_pushed(test_connection))
  expect_true(as.character(read$id) %in% listed)
  expect_false(as.character(reopened$id) %in% listed)
})
