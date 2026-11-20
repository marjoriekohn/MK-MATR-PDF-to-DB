test_that("a draft turns back into the same JSON the pipeline returned, and a saved copy opens as it was", {
  json <- gold_json()
  saved <- draft_service_to_json(draft_service_from_json(json))
  # Review adds `schema_version`, `checked` and `question_number` (the question's place) to blocks, `ref`
  # to every row, `matr_id` to variables and `code_value` to choices, and names the question a question
  # depends on by its place rather than its printed number; nothing else changes.
  expect_equal(saved$schema_version, draft_schema_version)
  without_review_fields <- function(x) {
    if (!is.list(x)) {
      return(x)
    }
    review_fields <- c("schema_version", "checked", "question_number", "parent_question", "ref", "matr_id", "code_value")
    if (!is.null(names(x))) x <- x[setdiff(names(x), review_fields)]
    lapply(x, without_review_fields)
  }
  as_text <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")
  expect_identical(as_text(without_review_fields(saved)), as_text(without_review_fields(json)))

  # the saved copy opens as the same draft, its rows still matched to the AI's (`ref`)
  expect_identical(draft_service_from_json(saved), gold_draft())
  draft <- gold_draft()
  expect_identical(draft$components$ref, draft$components$id)
  expect_identical(draft$variables$ref, draft$variables$id)
  expect_identical(draft$response_options$ref, draft$response_options$id)
})

test_that("a block keeps the PDF page it was read from, and a block added takes its neighbour's", {
  json <- short_gold_json()
  json$components[[1]]$page <- 2L
  draft <- draft_service_from_json(json)
  first <- draft$components$id[1]
  expect_equal(draft$components$page[1], 2L)
  expect_true(all(is.na(draft$components$page[-1])))
  new_id <- draft_service_next_id(draft, "components")
  added <- draft_service_add_block(draft, "Instruction", new_id, near_id = first)
  expect_equal(added$components$page[added$components$id == new_id], 2L)
  saved <- draft_service_to_json(added)
  expect_equal(saved$components[[1]]$page, 2L)
  expect_identical(draft_service_from_json(draft_service_to_json(draft))$components$page, draft$components$page)
})

test_that("a row keeps its ref however the draft changes, and a row added has none", {
  draft <- gold_draft()
  q1a <- block_numbered(draft, "1a")$id
  added <- draft_service_add_block(draft, "Question", draft_service_next_id(draft, "components"), near_id = q1a)
  added <- draft_service_add_box(added, q1a)
  moved_back <- draft_service_from_json(draft_service_to_json(draft_service_renumber_questions(added)))
  new_block <- moved_back$components[is.na(moved_back$components$ref), ]
  expect_equal(nrow(new_block), 1)
  expect_equal(new_block$component_type, "Question")
  expect_equal(sum(is.na(moved_back$variables$ref)), 1) # the copied variable
  expect_equal(sum(is.na(moved_back$response_options$ref)), 2) # and its two copied choices
  q1a_again <- moved_back$components[moved_back$components$ref %in% q1a, ]
  expect_equal(q1a_again$printed_number, "1a")
})

test_that("blocks are flattened in printed order, numbered within their section", {
  draft <- gold_draft()
  expect_equal(nrow(draft$components), 128)
  expect_equal(nrow(draft$variables), 251)
  expect_equal(nrow(draft$response_options), 330) # printed choices only

  first_three <- draft$components[1:3, ]
  expect_equal(first_three$component_type, c("Group", "Instruction", "Question"))
  expect_equal(first_three$parent_id, c(NA, "c1", "c1"))
  expect_equal(first_three$display_order, c(1, 1, 2))
})

test_that("a question keeps the id of the question it depends on, not its number", {
  draft <- gold_draft()
  expect_equal(block_numbered(draft, "8a")$parent_question_id, block_numbered(draft, "7")$id)

  # the reviewed copy names the question it depends on by its place among the questions
  renumbered <- draft
  renumbered$components$question_number[renumbered$components$id == block_numbered(draft, "7")$id] <- "70"
  find_question <- function(blocks, number) {
    for (block in blocks) {
      if (identical(block$printed_number, number)) {
        return(block)
      }
      found <- find_question(block$children, number)
      if (!is.null(found)) {
        return(found)
      }
    }
  }
  q8a <- find_question(draft_service_to_json(renumbered)$components, "8a")
  expect_equal(q8a$parent_question, "70")
})

test_that("a draft opens as the AI read it, then as staff last saved it", {
  me <- local_staff()
  unread <- local_extraction(me)
  expect_error(draft_service_load(test_connection, unread$id), "hasn't been read yet")

  record <- local_read_extraction(me)
  expect_identical(draft_service_load(test_connection, record$id), short_gold_draft())

  draft <- short_gold_draft()
  draft$components$checked[3] <- TRUE
  draft$response_options$code_value[1:2] <- c("1", "a") # codes are text: a letter too
  draft$variables$matr_id[1] <- "q1a"
  expect_equal(draft_service_save(test_connection, record$id, draft, me$staff_id), record$revision + 1)
  expect_identical(draft_service_load(test_connection, record$id), draft)
  # what the AI returned is never touched by saving
  expect_identical(draft_service_from_json(extraction_model_questionnaire(test_connection, record$id)$original), short_gold_draft())
})

test_that("a field the AI answered with an empty list is blank, not a reading that can't be loaded", {
  json <- list(questionnaire = list(), components = list(list(
    component_type = "Question", text = "How old are you?", printed_number = list(), subject = list(),
    variables = list(list(variable_type = "open_ended", answer_data_type = "NUMBER", unit = list()))
  )))
  draft <- draft_service_from_json(json)
  expect_equal(draft$components$text, "How old are you?")
  expect_true(is.na(draft$components$printed_number))
  expect_true(is.na(draft$components$subject))
  expect_true(is.na(draft$variables$unit))
  expect_equal(draft$components$question_number, "1")
})

test_that("a draft that isn't in the database is a sentence", {
  me <- local_staff()
  expect_error(draft_service_load(test_connection, "0"), extraction_gone, fixed = TRUE)
  expect_error(draft_service_save(test_connection, "0", short_gold_draft(), me$staff_id), extraction_gone, fixed = TRUE)
})

test_that("a draft typed by hand starts empty and can be saved and opened again", {
  me <- local_staff()
  record <- local_manual_extraction(me)
  empty <- draft_service_load(test_connection, record$id)
  expect_equal(nrow(empty$components), 0)
  expect_equal(nrow(empty$variables), 0)
  expect_equal(nrow(empty$response_options), 0)
  expect_identical(empty, draft_service_empty())

  typed <- gold_draft()
  draft_service_save(test_connection, record$id, typed, me$staff_id)
  expect_identical(draft_service_load(test_connection, record$id), typed)
})

test_that("choices are coded 1, 2, 3... in each variable", {
  choices <- gold_draft()$response_options
  expect_false(anyNA(choices$code_value))
  expect_identical(choices$code_value, as.character(choices$display_order))
})

test_that("M and F are the sexes only where a variable offers nothing else: among grades, F is a grade", {
  asking <- function(text, ...) {
    list(component_type = "Question", text = text, variables = list(list(
      variable_type = "closed_ended", response_options = lapply(c(...), function(answer) list(answer_option = answer))
    )))
  }
  draft <- draft_service_from_json(list(components = list(
    asking("What is your sex?", "M", "F"),
    asking("What is your average school grade?", "A", "D-", "F"),
    asking("What is your twin's sex?", "Male", "F."),
    asking("Which day is it?", "M", "T", "W", "Th", "F")
  )))
  answers <- split(draft$response_options$answer_option, draft$response_options$variable_id)
  expect_equal(unname(answers), list(c("male", "female"), c("a", "d-", "f"), c("male", "female"), c("m", "t", "w", "th", "f")))
})

test_that("variables are listed question by question in printed order", {
  draft <- gold_draft()
  boxes <- draft$variables[match(draft_service_boxes_in_order(draft), draft$variables$id), ]
  called <- function(draft, ids) draft_service_question_number(draft$components[match(ids, draft$components$id), ])
  numbers <- called(draft, boxes$component_id)
  expect_equal(head(numbers, 2), c("1a", "1b"))
  expect_equal(tail(numbers, 2), c("17", "112")) # the follow-up of 17 has no printed number: its place

  # Moving a question (a drag and drop in the outline) moves its boxes with it.
  q1a <- block_numbered(draft, "1a")$id
  q1b <- block_numbered(draft, "1b")$id
  printed <- draft_service_printed_order(draft)
  swapped <- printed
  swapped[match(c(q1a, q1b), printed)] <- c(q1b, q1a)
  moved <- draft_service_apply_order(draft, swapped, draft$components$parent_id[match(swapped, draft$components$id)])
  moved_boxes <- draft_service_boxes_in_order(moved)
  expect_equal(called(moved, moved$variables$component_id[match(moved_boxes[1:2], moved$variables$id)]), c("1b", "1a"))
})

test_that("printed order walks into each section before moving on", {
  draft <- gold_draft()
  printed <- draft_service_printed_order(draft)
  expect_equal(printed[1:3], c("c1", "c2", "c3"))
  expect_equal(length(printed), nrow(draft$components))
  expect_equal(draft_service_ancestors(draft, "c3"), "c1")
  expect_equal(draft_service_ancestors(draft, "c1"), character())
})

test_that("a draft counts how far its review has got", {
  draft <- gold_draft()
  counts <- draft_service_counts(draft)
  expect_equal(counts$blocks, nrow(draft$components))
  expect_equal(counts$boxes, nrow(draft$variables))
  expect_equal(counts$checked, sum(draft$components$checked))

  draft$components$checked[1:3] <- TRUE
  draft$variables$matr_id[1:2] <- c("q1a", "q1b")
  changed <- draft_service_counts(draft)
  expect_equal(changed$checked, counts$checked + 3 - sum(gold_draft()$components$checked[1:3]))
  expect_equal(changed$matr_ids, sum(!is.na(gold_draft()$variables$matr_id)) + 2 - sum(!is.na(gold_draft()$variables$matr_id[1:2])))
})

test_that("a save is refused if someone saved the draft after the person opened it", {
  me <- local_staff()
  record <- local_read_extraction(me)
  opened <- record$revision
  first <- short_gold_draft()
  first$components$checked[3] <- TRUE
  expect_equal(draft_service_save(test_connection, record$id, first, me$staff_id, expected_revision = opened), opened + 1)
  # the model says so in a sentence, and the first save stands
  stale <- draft_service_update(short_gold_draft(), "components", block_numbered(short_gold_draft(), "1a")$id, "text", "Overwritten?")
  expect_error(
    draft_service_save(test_connection, record$id, stale, me$staff_id, expected_revision = opened),
    "This draft was changed after you opened it. Open it again to see the new version.",
    fixed = TRUE
  )
  expect_identical(draft_service_load(test_connection, record$id), first)
  expect_equal(extraction_model_get(test_connection, record$id)$revision, opened + 1)
})

test_that("a draft saved before skips, conditions and respondent counts went opens without them", {
  json <- list(components = list(
    list(component_type = "Group", skip_condition = "No", skip_when = "every", skip_instruction = "Go to Question 2", children = list(
      list(
        component_type = "Question", text = "Do you smoke?", printed_number = "1", self_contained = TRUE, condition = "Adults",
        variables = list(list(
          variable_type = "closed_ended",
          response_options = list(list(answer_option = "Yes", respondent_count = 12L), list(answer_option = "No", skip_instruction = "Go to 3", skip_to = 3L))
        ))
      )
    )),
    list(component_type = "Question", text = "When did you start?", printed_number = "2", parent_question = "1", variables = list(list(
      variable_type = "open_ended", answer_data_type = "DATE", skip_condition = "Never", skip_instruction = "Go to 3", skip_to_end = TRUE
    ))),
    list(component_type = "Question", text = "Do you drink?", printed_number = "3", variables = list(list(variable_type = "closed_ended")))
  ))
  draft <- draft_service_from_json(json)
  gone <- c("self_contained", "condition", "parent_from_skip", "stem", "skip_condition", "skip_when", "skip_instruction", "skip_to_id", "respondent_count")
  for (table in names(parent_columns)) expect_length(intersect(names(draft[[table]]), gone), 0)
  # what the questionnaire still says: 2 depends on 1
  expect_equal(block_numbered(draft, "2")$parent_question_id, block_numbered(draft, "1")$id)
  saved <- jsonlite::toJSON(draft_service_to_json(draft), auto_unbox = TRUE)
  expect_false(grepl("skip|self_contained|condition|respondent_count|stem", saved))
})


# ------ Grids --------------------------------------------------------------------------------------------------------
# Questions that share a printed lead-in which is part of each question: the lead-in is the Grid's words, each
# question's are the rest.
grid_draft <- function(lead_in = "In the past 4 weeks, how often", children = NULL) {
  question <- function(text, number) {
    list(component_type = "Question", text = text, printed_number = number, variables = list(list(
      variable_type = "closed_ended", response_options = list(list(answer_option = "Often"), list(answer_option = "Never"))
    )))
  }
  draft_service_from_json(list(questionnaire = list(), components = list(
    list(component_type = "Section", text = "Part A", children = list(
      list(component_type = "Grid", text = lead_in, children = children %||% list(
        list(component_type = "Instruction", text = "Mark one for each line."),
        question("have you felt calm and peaceful?", "20a"),
        question("have you had a lot of energy?", "20b")
      )),
      question("Overall, how is your health?", "21")
    ))
  )))
}

test_that("a grid keeps its words and its questions when saved and opened again", {
  draft <- grid_draft()
  again <- draft_service_from_json(draft_service_to_json(draft))
  kept <- again$components[again$components$component_type == "Grid", ]
  expect_equal(kept$text, "In the past 4 weeks, how often")
  expect_equal(numbers_in(again, kept$id), c("20a", "20b"))
  expect_identical(again, draft)
})

test_that("a grid holds only its questions with their instructions and definitions, and a section holds a grid", {
  draft <- grid_draft()
  grid <- draft$components$id[draft$components$component_type == "Grid"]
  part <- draft$components$id[draft$components$component_type == "Section"]
  instruction <- draft$components$id[draft$components$component_type == "Instruction"]
  expect_error(draft_service_update(draft, "components", instruction, "component_type", "Section"), "A grid holds only")
  expect_error(draft_service_update(draft, "components", instruction, "component_type", "Group"), "A grid holds only")
  # a grid with a section in it can't be made, nor one holding a group
  expect_error(draft_service_update(draft, "components", part, "component_type", "Grid"), "A grid holds only")
  # a grid can become a section, keeping its questions; its words stop it becoming a group
  expect_equal(numbers_in(draft_service_update(draft, "components", grid, "component_type", "Section"), grid), c("20a", "20b"))
  expect_error(draft_service_update(draft, "components", grid, "component_type", "Group"), "has no words of its own")
})

test_that("who a variable is about is the relation alone, in lower case, and the form's words have straight quotes, however they are typed", {
  expect_equal(normal_subject(c("Respondent’s Siblings", " “Twin A” ", "Respondent")), c("siblings", '"twin a"', NA))
  expect_equal(normal_text("the child’s “sex”"), "the child's \"sex\"")
  draft <- draft_service_update(gold_draft(), "variables", "v1", "subject", "Respondent’s Twin")
  expect_equal(draft$variables$subject[1], "twin")
})

test_that("a year and a month are each written one way, whatever the form calls them", {
  expect_equal(
    normal_time_frame(c(
      "in the last 12 months", "Over the past twelve months", "in the past year", "past 12 months",
      "during the last 30 days", "past month", "Past 30 days", "in the past 30 days"
    )),
    rep(c("past year", "past month"), each = 4)
  )
  # other spans keep their own length
  expect_equal(
    normal_time_frame(c("in the past 4 weeks", "past 6 months", "last 2 years", "past 31 days", "in your lifetime", "since age 18")),
    c("past four weeks", "past six months", "past two years", "past 31 days", "lifetime", "since age 18")
  )
})

test_that("a write-in's unit is written one way, kept with the draft, and changed like any field", {
  expect_equal(
    normal_unit(c("yrs.", "LBS", "oz", "Hrs", " ")),
    c("years", "pounds", "ounces", "hours", NA)
  )
  # a unit is what is counted, never a rate: how often is the question's to say
  expect_equal(
    normal_unit(c("Cigarettes per day", "days per week", "hrs/day", "times a week", "drinks each day", "per day", "times")),
    c("cigarettes", "days", "hours", "times", "drinks", NA, "times")
  )
  draft <- gold_draft()
  box <- draft$variables$id[draft$variables$variable_type == "open_ended"][1]
  draft <- draft_service_update(draft, "variables", box, "unit", "Lbs.")
  expect_equal(draft$variables$unit[draft$variables$id == box], "pounds")
  again <- draft_service_from_json(draft_service_to_json(draft))
  expect_equal(again$variables$unit[again$variables$id == box], "pounds")
})

# A draft of one question, "have you smoked?", with these variables; `question` adds fields of the question's own.
tagged_question <- function(..., question = list()) {
  draft_service_from_json(list(questionnaire = list(), components = list(c(list(
    component_type = "Question", text = "have you smoked?", printed_number = "1", variables = list(...)
  ), question))))
}
write_in <- function(...) c(list(variable_type = "open_ended", answer_data_type = "NUMBER"), list(...))

test_that("a tag is on the question when it is about all of it, on a variable when it tells the variables apart, and is said once", {
  # what a question's only variable says, or all of them say, is the question's
  one <- tagged_question(write_in(time_frame = "in your lifetime", subject = "Twins"))
  expect_equal(one$components$time_frame, "lifetime")
  expect_equal(one$components$subject, "twins")
  expect_equal(one$variables$time_frame, NA_character_)
  expect_equal(one$variables$subject, NA_character_)

  # the same question about each twin (MQ 8 and 24): who each variable is about stays on it, the time they share goes up
  each <- tagged_question(write_in(subject = "twin a", time_frame = "lifetime"), write_in(subject = "twin b", time_frame = "lifetime"))
  expect_equal(each$components$subject, NA_character_)
  expect_equal(each$variables$subject, c("twin a", "twin b"))
  expect_equal(each$components$time_frame, "lifetime")
  expect_equal(each$variables$time_frame, c(NA_character_, NA_character_))

  # the respondent (a blank) and someone else aren't the same, so nothing goes up
  mixed <- tagged_question(write_in(), write_in(subject = "spouse/partner"))
  expect_equal(mixed$components$subject, NA_character_)
  expect_equal(mixed$variables$subject, c(NA, "spouse/partner"))

  # a tag the question has is kept, and a variable doesn't say it again
  tagged <- tagged_question(write_in(subject = "twins"), write_in(), question = list(subject = "Twins", time_frame = "In the last 12 months"))
  expect_equal(tagged$components$subject, "twins")
  expect_equal(tagged$components$time_frame, "past year")
  expect_equal(tagged$variables$subject, c(NA_character_, NA_character_))
  # once one variable is about someone else, each variable says who it is about and the question doesn't: a
  # second variable added to a question about twin a and tagged twin b (the same question for each twin)
  split <- tagged_question(write_in(), write_in(subject = "twin b"), question = list(subject = "twin a"))
  expect_equal(split$components$subject, NA_character_)
  expect_equal(split$variables$subject, c("twin a", "twin b"))
  expect_identical(draft_service_place_tags(split), split)

  # a unit is a write-in's own, never the question's
  expect_equal(tagged_question(write_in(unit = "yrs."))$variables$unit, "years")
  expect_false("unit" %in% names(one$components))

  # saved and opened again, the tags are where they were, and the JSON says each once
  json <- draft_service_to_json(each)
  question <- json$components[[1]]
  expect_equal(question$time_frame, "lifetime")
  expect_null(question$subject)
  expect_equal(vapply(question$variables, function(v) v$subject, ""), c("twin a", "twin b"))
  expect_null(question$variables[[1]]$time_frame)
  again <- draft_service_from_json(json)
  expect_equal(again$components$time_frame, "lifetime")
  expect_equal(again$variables$subject, c("twin a", "twin b"))

  # only a question has tags: a section's are not kept
  section <- draft_service_from_json(list(questionnaire = list(), components = list(list(component_type = "Section", text = "About twin a", subject = "twin a"))))
  expect_equal(section$components$subject, NA_character_)
})

test_that("a tag typed on a question or a variable is kept as typed, and said once when the draft is saved", {
  draft <- tagged_question(write_in(subject = "twin a"), write_in(subject = "twin b"))
  question <- draft$components$id[1]
  draft <- draft_service_update(draft, "components", question, "time_frame", "During the past 30 days")
  expect_equal(draft$components$time_frame, "past month")
  draft <- draft_service_update(draft, "components", question, "subject", "Respondent’s Twins")
  expect_equal(draft$components$subject, "twins")
  # typed again on a variable, it is there while the page is open (a tag half typed is never moved)...
  draft <- draft_service_update(draft, "variables", draft$variables$id[1], "time_frame", "past 30 days")
  expect_equal(draft$variables$time_frame[1], "past month")
  # ...and the saved copy says it once
  saved <- draft_service_to_json(draft)$components[[1]]
  expect_equal(saved$time_frame, "past month")
  expect_null(saved$variables[[1]]$time_frame)
  # a blank takes it away
  expect_equal(draft_service_update(draft, "components", question, "subject", "")$components$subject, NA_character_)
  expect_error(draft_service_update(draft, "components", question, "unit", "years"), "can't be changed here")
})

test_that("a label that only says a tag again is left out", {
  labelled <- function(label, ...) tagged_question(write_in(label = label, ...), write_in(label = "age started", ...))$variables$label[1]
  expect_equal(labelled("Mother", subject = "mother"), NA_character_)
  expect_equal(labelled("your spouse/partner", subject = "spouse/partner"), NA_character_)
  expect_equal(labelled("ever in his lifetime", time_frame = "lifetime"), NA_character_)
  expect_equal(labelled("In the past 12 months", time_frame = "past 12 months"), NA_character_)
  expect_equal(labelled("Yrs.", unit = "years"), NA_character_)
  expect_equal(labelled("Yourself"), NA_character_) # the respondent is the blank subject
  # the question's tags count too: both variables say "mother", so the question does
  expect_equal(labelled("mother", subject = "mother"), NA_character_)
  # a label that says more is kept
  expect_equal(labelled("Mother's age", subject = "mother"), "Mother's age")
  expect_equal(labelled("age started", time_frame = "lifetime"), "age started")
})

test_that("a block reopened from the catalog keeps the component it came from, and the choice to reword it everywhere", {
  reopened <- list(schema_version = 2L, questionnaire = list(name = "Mother's Questionnaire"), components = list(
    list(component_type = "Question", text = "have you smoked?", component_id = 123456789L, question_number = "1", checked = TRUE, variables = list(
      list(variable_type = "open_ended", answer_data_type = "NUMBER", matr_id = "mq1")
    ))
  ))
  draft <- draft_service_from_json(reopened)
  expect_equal(draft$components$catalog_id, "123456789")
  expect_equal(draft$components$reword, NA_character_)
  # saved as it came, the block still names its component; reworded everywhere, it says so
  expect_equal(draft_service_to_json(draft)$components[[1]]$component_id, "123456789")
  expect_null(draft_service_to_json(draft)$components[[1]]$reword)
  everywhere <- draft_service_reword(draft, draft$components$id, TRUE)
  expect_equal(draft_service_to_json(everywhere)$components[[1]]$reword, "everywhere")
  expect_equal(draft_service_from_json(draft_service_to_json(everywhere))$components$reword, "everywhere")
  expect_error(draft_service_update(draft, "components", draft$components$id, "reword", "everywhere"), "can't be changed here")
  # a block staff add comes from no component, and what the AI reads never does
  added <- draft_service_add_block(draft, "Question", draft_service_next_id(draft, "components"))
  expect_equal(added$components$catalog_id, c("123456789", NA))
  expect_true(all(is.na(gold_draft()$components$catalog_id)))
  expect_error(draft_service_reword(added, added$components$id[2], TRUE), "didn't come from the database")
})

test_that("a wording changed everywhere is changed in the draft's other blocks from the same component, and taken back with the choice", {
  statement <- function(number) {
    list(component_type = "Question", text = "i accomplished less than i wanted to.", component_id = 303171936L, question_number = number, variables = list(
      list(variable_type = "open_ended", answer_data_type = "NUMBER", matr_id = paste0("ah", number))
    ))
  }
  draft <- draft_service_from_json(list(schema_version = 2L, questionnaire = list(), components = list(statement("1"), statement("2"))))
  first <- draft$components$id[1]
  reworded <- draft_service_update(draft, "components", first, "text", "I got less done than I wanted to.")
  expect_equal(reworded$components$text, c("i got less done than i wanted to.", "i accomplished less than i wanted to.")) # only here, until chosen
  everywhere <- draft_service_reword(reworded, first, TRUE)
  expect_equal(everywhere$components$text, rep("i got less done than i wanted to.", 2))
  expect_equal(everywhere$components$reword, rep("everywhere", 2))
  # only here again: the other block has the catalog's words back, and this one keeps its new ones
  here <- draft_service_reword(everywhere, first, FALSE, was = "i accomplished less than i wanted to.")
  expect_identical(here, reworded)
})
