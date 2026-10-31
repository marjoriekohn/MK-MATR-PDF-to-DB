# Wording things one way before an extraction is added: answer choices written as the database writes them, and
# questions that differ only in who they ask about, linked (service_normalize.R).

# A questionnaire asked about each twin in turn: in Twin A's part, a question naming Twin A, one whose choices name
# Twin A, and one worded the same in both parts; then the same in Twin B's part, and a question comparing them.
twins_draft <- function() {
  closed <- function(...) list(variable_type = "closed_ended", response_options = lapply(c(...), function(words) list(answer_option = words)))
  question <- function(text, number, ...) list(component_type = "Question", text = text, printed_number = number, variables = list(closed(...)))
  part <- function(twin, numbers) {
    list(component_type = "Section", text = paste("About", twin), children = list(
      question(paste("Has", twin, "ever repeated a grade?"), numbers[1], "Yes", "No"),
      question(paste0("How well does this describe ", twin, "?"), numbers[2], paste("Describes", twin, "very well"), paste("Doesn't describe", twin, "at all")),
      question("My child smiles often.", numbers[3], "Often", "Never")
    ))
  }
  draft_service_from_json(list(questionnaire = list(), components = list(
    part("Twin A", c("13a", "14", "20d")),
    part("Twin B", c("29a", "30", "36d")),
    question("Who is taller, Twin A or Twin B?", "37", "Twin A", "Twin B")
  )))
}
twins <- c("Respondent's Children", "Twin A", "Twin B")

test_that("ranges, dashes and abbreviations in answer choices are written one way", {
  expect_equal(normal_answer(c("1 to 2", "1 - 2", "1–2", "1-2")), rep("1-2", 4))
  expect_equal(normal_answer(c("1-2 hrs.", "More than 1 HR", "10 mins")), c("1-2 hours", "more than 1 hour", "10 minutes"))
  expect_equal(normal_answer("Married – living apart now"), "married - living apart now")
  expect_equal(normal_answer(c("age 16 to 17", "1 to 2 to 3")), c("age 16-17", "1-2-3"))
  expect_equal(normal_answer(c("Not too bad", "to 5", "Yes, most of the time.")), c("not too bad", "to 5", "yes, most of the time"))
})

test_that("a choice looks like another worded the same but for punctuation, capitals, \"&\" and plurals", {
  labels <- c("yes, most of the time", "2 close friends", "black and white", "a little", "age 15")
  expect_equal(normalize_look_alikes("yes most of the time", labels), "yes, most of the time")
  expect_equal(normalize_look_alikes("2 close friend", labels), "2 close friends")
  expect_equal(normalize_look_alikes("black & white", labels), "black and white")
  expect_equal(normalize_look_alikes("a little bit", labels), character()) # different words aren't alike
  expect_equal(normalize_look_alikes("age 16", labels), character())
  expect_equal(normalize_look_alikes("a little", labels), character()) # not itself
})

# One question with a variable for each vector of choices.
choices_draft <- function(...) {
  boxes <- lapply(list(...), function(words) {
    list(variable_type = "closed_ended", response_options = lapply(words, function(word) list(answer_option = word)))
  })
  draft_service_from_json(list(questionnaire = list(), components = list(
    list(component_type = "Question", text = "How often?", printed_number = "1", variables = boxes)
  )))
}

test_that("choices that could use a wording the database has, or one merged into it, are suggested", {
  draft <- choices_draft(c("Yes most of the time", "Not ever", "Never"))
  aliases <- data.frame(alias = "not ever", label = "never")
  suggested <- normalize_service_choice_suggestions(draft, c("yes, most of the time", "never"), aliases)
  expect_equal(suggested$from, c("not ever", "yes most of the time"))
  expect_equal(suggested$to, c("never", "yes, most of the time"))
  expect_equal(suggested$why, c("merged", "alike"))
  expect_equal(nrow(normalize_service_choice_suggestions(choices_draft(c("Yes", "No")), c("yes", "no"))), 0)
})

test_that("two wordings of a choice in the draft alone suggest the one more variables offer", {
  draft <- choices_draft(c("Yes most of the time", "No"), c("Yes, most of the time", "No"), c("Yes, most of the time", "No"))
  suggested <- normalize_service_choice_suggestions(draft, character())
  expect_equal(suggested$from, "yes most of the time")
  expect_equal(suggested$to, "yes, most of the time")
  expect_false(suggested$in_database)
})

test_that("using another wording changes it everywhere, unless a variable would then offer it twice", {
  draft <- choices_draft(c("Yes most of the time", "No"), c("Yes most of the time", "Never"))
  used <- normalize_service_use_choice(draft, "yes most of the time", "yes, most of the time")
  expect_equal(used$response_options$answer_option, c("yes, most of the time", "no", "yes, most of the time", "never"))
  expect_error(normalize_service_use_choice(draft, "yes most of the time", "no"), "offers both")
})

test_that("questions that differ only in who they ask about, or are worded the same in parts about different people, are found", {
  groups <- normalize_service_question_groups(twins_draft(), twins)
  expect_equal(vapply(groups, function(group) group$stem, ""), c(
    "Has [subject] ever repeated a grade?", "How well does this describe [subject]?", "My child smiles often."
  ))
  expect_true(all(vapply(groups, function(group) identical(group$subjects, c("Twin A", "Twin B")), TRUE)))
  expect_false(any(vapply(groups, function(group) group$linked, TRUE)))
  # a question naming both twins compares them: it is no one's
  expect_false(any(vapply(groups, function(group) "c9" %in% group$ids, TRUE)))
  # without subjects to find, there is nothing
  expect_length(normalize_service_question_groups(twins_draft(), character()), 0)
})

test_that("linking gives questions their stem and subjects, and choices that name the twin [subject], so they share a set", {
  draft <- twins_draft()
  for (group in normalize_service_question_groups(draft, twins)) draft <- normalize_service_link(draft, group, twins)
  questions <- draft$components[draft$components$component_type == "Question", ]
  expect_equal(questions$stem, c(
    "Has [subject] ever repeated a grade?", "How well does this describe [subject]?", "My child smiles often.",
    "Has [subject] ever repeated a grade?", "How well does this describe [subject]?", "My child smiles often.", NA
  ))
  expect_equal(draft$variables$subject, c(rep("Twin A", 3), rep("Twin B", 3), NA))
  described <- function(id) draft$response_options$answer_option[draft$response_options$variable_id == id]
  expect_equal(described("v2"), c("describes [subject] very well", "doesn't describe [subject] at all"))
  expect_equal(described("v5"), described("v2"))
  expect_equal(described("v7"), c("twin a", "twin b")) # comparing them, so left as printed
  expect_true(all(vapply(normalize_service_question_groups(draft, twins), function(group) group$linked, TRUE)))

  # the stem is kept with the draft, and unlinking takes it off (who they are about stays)
  again <- draft_service_from_json(draft_service_to_json(draft))
  expect_equal(again$components$stem, draft$components$stem)
  unlinked <- normalize_service_unlink(draft, c("c2", "c6"))
  expect_true(all(is.na(unlinked$components$stem[unlinked$components$id %in% c("c2", "c6")])))
  expect_equal(unlinked$variables$subject, draft$variables$subject)
})

test_that("[subject] is filled in with who a variable is about", {
  expect_equal(normalize_service_fill("describes [subject] very well", "Twin A"), "describes Twin A very well")
  expect_equal(normalize_service_fill("Has [subject] ever smoked?", NA), "Has the respondent ever smoked?")
})
