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

test_that("what the database has already is put the way the database names it, for the table's NEW marks", {
  known <- extraction_service_known(first_counts)
  expect_equal(known$answer_new, c("no", "yes"))
  expect_equal(known$option_set_new, extraction_service_option_set_key(c("no", "yes"))) # its choices, in any order
  expect_equal(known$tag_new, "years")
  expect_length(known$question_found, 0) # a first push finds no question: every wording is new
  again <- extraction_service_known(again_counts)
  expect_equal(again$question_found, c("do you smoke?", "how tall are you?"))
  expect_length(again$answer_new, 0)
  expect_equal(extraction_service_tag_key(c(" Spouse\u2019s  Parents ", "TWIN A")), c("spouse's parents", "twin a"))
  # the blocks that aren't questions, by kind and words, when the catalog's were read
  expect_null(known$block_found)
  read <- extraction_service_known(first_counts, data.frame(kind = "Note", text = " see  page 2 "))
  expect_equal(read$block_found, extraction_service_block_key("Note", "see page 2"))
  expect_false(extraction_service_block_key("Instruction", "see page 2") %in% read$block_found) # another kind
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

test_that("the preview says why the database would refuse the push, and nothing more: what is new is in the table", {
  catalog <- test_catalog()
  draft <- test_questionnaire_draft()
  info <- extraction_service_info(complete_info())

  preview <- extraction_service_push_preview_ui(draft, info, catalog, first_counts)
  expect_equal(names(preview), c("where", "problem", "unchecked"))
  expect_length(preview$where, 4)
  expect_null(preview$problem)
  expect_equal(preview$unchecked, 4)
  html <- htmltools::renderTags(extraction_request_ui(preview, draft, questionnaire = FALSE))$html
  # no sentence of counts, and no cards: they count rows, not what changes; what is new is marked in the table
  expect_false(grepl("What it adds|request-card|Adds ", html))
  expect_false(grepl("can't be pushed", html, fixed = TRUE))

  # the database refused: its sentence is the problem, and there is nothing to add
  refusal <- "The wave Wave 2 of Adult Health and Personality already asks 12 question(s). Empty it first (Metadata > the wave > Empty), or pick a wave that asks none."
  refused <- extraction_service_push_preview_ui(with_checked(draft, draft$components$id), info, catalog, simpleError(refusal))
  expect_equal(refused$problem, refusal)
  expect_equal(refused$unchecked, 0)
  html <- htmltools::renderTags(extraction_request_ui(refused, draft, questionnaire = FALSE))$html
  expect_match(html, "This can't be pushed as it is", fixed = TRUE)
  expect_match(html, "Empty it first", fixed = TRUE)
  expect_false(grepl("What it adds", html, fixed = TRUE))

  # not tried, as the Basic Info isn't complete: the cards say what to pick, and nothing is worked out
  not_tried <- extraction_service_push_preview_ui(draft, extraction_service_info(), catalog, NULL)
  expect_true(all(vapply(not_tried$where, function(card) isTRUE(card$is_missing), logical(1))))
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
  expect_equal(counts$component_reused, 4) # its four wordings are the database's already
  expect_equal(counts$placement_reused, 4) # and the questionnaire prints all four already
  expect_equal(counts$variable_reused, 2)

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
  expect_match(html, "Similar Wording")
  expect_match(html, "Choose whether to update only this questionnaire's wording or update the wording in every questionnaire.", fixed = TRUE)
  expect_match(html, "Kid Questionnaire and Mother(&#39;|')s Questionnaire")
  expect_match(html, sprintf('data-action="reword" data-id="%s" data-everywhere="true"', id_of("1")), fixed = TRUE)
  expect_match(html, sprintf('data-action="reword" data-id="%s" data-everywhere="false"', id_of("3")), fixed = TRUE)
  expect_null(rewordings_ui(choices[0, ]))
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
  is_text <- function(edits) vapply(edits, function(edit) identical(edit$field, "text"), logical(1))
  first_push <- monitor_model_get_pushed(test_connection)[[as.character(read$id)]]
  expect_true(any(is_text(first_push$edits))) # the rewording counts against the prompt

  reopened <- extraction_service_reopen_wave(test_connection, tree$wave$wave_id, me)
  withr::defer(test_delete_extraction(reopened$id))
  expect_equal(reopened$method, extraction_ai) # not typed by hand
  expect_equal(reopened$extraction_status, "ready") # nothing to read
  expect_equal(as.character(reopened$info$reopened_from), as.character(read$id))

  # its questionnaire came back from the database without the rows' refs: they are matched through the pushed copy
  opened <- extraction_service_open_draft(test_connection, reopened)
  expect_false(is.null(opened$original))
  expect_false(anyNA(opened$draft$components$ref))
  expect_true(opened$matched) # not saved yet: the page saves it before the push, so its corrections are kept
  expect_length(review_edits_of(test_connection, reopened$id), 0)
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
  expect_false(again$matched)
  expect_false(any(vapply(review_edits_of(test_connection, reopened$id), function(edit) identical(edit$field, "text"), logical(1))))

  # it isn't read again from here (that is the wave's own Re-extract), and the AI Monitor counts the reading once, as
  # of its latest push: the rewording put back no longer counts against the prompt
  expect_match(error_text(extraction_service_reextract(test_connection, reopened$id, me)), "Re-extract on the wave")
  outcome <- extraction_service_push(
    test_connection, extraction_model_get(test_connection, reopened$id), me, questionnaire_service_get_catalog(test_connection)
  )
  expect_equal(outcome$record$status, "Pushed")
  monitor <- monitor_model_get_pushed(test_connection)
  expect_true(as.character(read$id) %in% names(monitor))
  expect_false(as.character(reopened$id) %in% names(monitor))
  latest <- monitor[[as.character(read$id)]]
  expect_false(any(is_text(latest$edits)))
  expect_equal(latest$latest_id, as.character(reopened$id))
  expect_equal(latest[c("applied", "prompt_version")], first_push[c("applied", "prompt_version")])
})
