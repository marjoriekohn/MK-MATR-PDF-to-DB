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
  expect_equal(normal_answer(c("1-2 hrs.", "More than 1 HR", "10 mins")), c("1-2 [hours]", "more than 1 [hour]", "10 [minutes]"))
  expect_equal(normal_answer(c("Married – living apart now", "a – b – c")), c("married [living apart now]", "a [b - c]"))
  expect_equal(normal_answer(c("age 16 to 17", "1 to 2 to 3")), c("age 16-17", "1-2-3"))
  expect_equal(normal_answer(c("Not too bad", "to 5", "Yes, most of the time.")), c("not too bad", "to 5", "yes [most of the time]"))
})

test_that("a choice's qualifier is written after it in brackets, so the choices sharing an answer are found together", {
  expect_equal(normal_answer(c("Yes", "Yes, all episodes during pregnancy", "No, never", "yes; but only once")), c(
    "yes", "yes [all episodes during pregnancy]", "no [never]", "yes [but only once]"
  ))
  # a number or range and what it counts
  expect_equal(
    normal_answer(c("1-2 hours", "1 to 2 hrs.", "3 cups a day", "2+ times")),
    c("1-2 [hours]", "1-2 [hours]", "3 [cups a day]", "2 or more [times]")
  )
  # a closing remark in brackets
  expect_equal(normal_answer(c("Biological (Blood)", "don't know(DK)")), c("biological [blood]", "don't know"))
  # typed with brackets already: tidied, not split again
  expect_equal(normal_answer(c("Yes [ Most of the time. ]", "yes[never]", "yes [all, or most]", "yes [most, during pregnancy]")), c(
    "yes [most of the time]", "yes [never]", "yes [all, or most]", "yes [most, during pregnancy]"
  ))
  # left as they are: a remark in the middle, a long or numbers-only first part, "or" after the comma, a number
  # that isn't counting something, and [subject]
  unchanged <- c(
    "i have smoked fewer than 50 cigarettes (2 1/2 packs) altogether in my life",
    "never, or almost never", "1, 2 or 3", "1 in 10", "once", "every day",
    "describes [subject] very well", "age 16-17", "(none)", "more than 1,000"
  )
  expect_equal(normal_answer(unchanged), unchanged)
  # written once, it stays written so
  every <- normal_answer(c("Yes, all episodes during pregnancy", "1 to 2 hrs.", "Biological (Blood)", unchanged, "yes [[subject]]"))
  expect_equal(normal_answer(every), every)
})

test_that("a choice printed as \"?\" is uncertain, and ranges are written one way however they are printed", {
  # "If you are uncertain, please circle ?" (Pink Q75)
  expect_equal(normal_answer(c("?", " ? ")), c("uncertain", "uncertain"))
  expect_equal(normal_answer(c("Between 3 and 5 times", "between 11 and 20 times", "Once or twice")), c("3-5 [times]", "11-20 [times]", "once or twice"))
  # the same amount in other units says nothing more (VAQ3 Repro Q22)
  expect_equal(normal_answer(c("21-35 days (3-5 weeks).", "less than 21 days (3 weeks).", "more than 35 days (5 weeks)")), c(
    "21-35 [days]", "less than 21 [days]", "more than 35 [days]"
  ))
  # a closing remark that isn't an amount stays
  expect_equal(normal_answer(c("Adult (18+)", "Less than $1,000 (including loss)", "cigarettes (20 per pack)")), c(
    "adult [18 or more]", "less than $1,000 [including loss]", "cigarettes [20 per pack]"
  ))
})

test_that("an amount and what it counts are split however the amount is compared", {
  expect_equal(normal_answer(c(
    "Less than 21 days", "about 2-3 times a week", "Fewer than 10 drinks", "at least 3 times", "Within 30 minutes of waking",
    "about 1,200 dollars", "Up to 5 years"
  )), c(
    "less than 21 [days]", "about 2-3 [times a week]", "fewer than 10 [drinks]", "at least 3 [times]", "within 30 [minutes of waking]",
    "about 1,200 [dollars]", "up to 5 [years]"
  ))
  # not an amount followed by what it counts
  expect_equal(normal_answer(c("less than 5", "1 in 10")), c("less than 5", "1 in 10"))
  # once and twice are amounts too, and so is "or more" (with "4+" written out)
  expect_equal(normal_answer(c(
    "more than once a week", "Once or twice a month", "4 or more times a week", "4+", "11+ times", "1 or 2 hours",
    "less than once a month, but at least once a year"
  )), c(
    "more than once [a week]", "once or twice [a month]", "4 or more [times a week]", "4 or more", "11 or more [times]",
    "1 or 2 [hours]", "less than once [a month, but at least once a year]"
  ))
})

test_that("abbreviations in a choice are written out", {
  # "R" is the respondent (TSA C4); "HH" the household (AAATS Parent B6)
  expect_equal(normal_answer(c("R first", "Cotwin first", "Married - Spouse in HH", "DK", "N/A", "M", "F")), c(
    "respondent first", "cotwin first", "married [spouse in household]", "don't know", "not applicable", "male", "female"
  ))
  expect_equal(normal_answer(c("2-3 wks", "6 mos", "10 yrs")), c("2-3 [weeks]", "6 [months]", "10 [years]"))
  # whole words only, and a lone letter only as the whole choice
  expect_equal(normal_answer(c("grade 12", "brother", "vitamin d", "f-150")), c("grade 12", "brother", "vitamin d", "f-150"))
})

test_that("a choice looks like another worded the same but for punctuation, capitals, \"&\" and plurals", {
  labels <- c("yes, most of the time", "2 close friends", "black and white", "a little", "age 15")
  expect_equal(normalize_look_alikes("yes most of the time", labels), "yes, most of the time")
  expect_equal(normalize_look_alikes("yes most of the time", "yes [most of the time]"), "yes [most of the time]")
  expect_equal(normalize_look_alikes("2 close friend", labels), "2 close friends")
  expect_equal(normalize_look_alikes("black & white", labels), "black and white")
  expect_equal(normalize_look_alikes("a little bit", labels), character()) # different words aren't alike
  expect_equal(normalize_look_alikes("age 16", labels), character())
  expect_equal(normalize_look_alikes("a little", labels), character()) # not itself
  # short words keep their "s": "yes" isn't "ye"
  expect_equal(normalize_look_alikes("yes", c("ye", "no", "yes.")), "yes.")
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
  expect_equal(suggested$to, "yes [most of the time]") # the draft's "Yes, most of the time", as it is kept
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
  # without subjects to find, there is nothing; and a one-letter subject isn't looked for in every "a"
  expect_length(normalize_service_question_groups(twins_draft(), character()), 0)
  expect_length(normalize_service_question_groups(twins_draft(), "A"), 0)
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

# ------ Questions about different times -------------------------------------------------------------------------
# The same question asked about three times, as AAATS H.3-H.5 asks it, and one asked about each twin over a year.
times_draft <- function() {
  question <- function(text, number) {
    list(component_type = "Question", text = text, printed_number = number,
         variables = list(list(variable_type = "open_ended", answer_data_type = "NUMBER")))
  }
  draft_service_from_json(list(questionnaire = list(), components = list(
    question("on how many occasions have you smoked cigarettes in your lifetime?", "H.3"),
    question("on how many occasions have you smoked cigarettes during the last 12 months?", "H.4"),
    question("on how many occasions have you smoked cigarettes in the past 30 days?", "H.5"),
    question("over the past year, how would you say your relationship with twin a has been?", "8"),
    question("over the past year, how would you say your relationship with twin b has been?", "9")
  )))
}

test_that("a time frame is written one way however it is printed", {
  expect_equal(
    normal_time_frame(c(
      "During the LAST 12 months", "over the past twelve months", "in the past year", "within the last 30 days",
      "in your life", "during your lifetime", "in the past 4 weeks", "past week", "In the last month"
    )),
    c(
      "in the past 12 months", "in the past 12 months", "in the past year", "in the past 30 days",
      "in your lifetime", "in your lifetime", "in the past 4 weeks", "in the past week", "in the past month"
    )
  )
  expect_equal(normal_time_frame("Since your last birthday"), "since your last birthday") # one it doesn't know: as typed
  expect_true(is.na(normal_time_frame(" ")))
})

test_that("questions that differ only in when they ask about are found, and so are those about different people", {
  groups <- normalize_service_question_groups(times_draft(), twins)
  expect_equal(vapply(groups, function(group) group$stem, ""), c(
    "on how many occasions have you smoked cigarettes [time frame]?",
    "[time frame], how would you say your relationship with [subject] has been?"
  ))
  expect_equal(groups[[1]]$time_frames, c("in your lifetime", "in the past 12 months", "in the past 30 days"))
  expect_true(all(is.na(groups[[1]]$subjects))) # all about the respondent
  expect_equal(groups[[2]]$subjects, c("Twin A", "Twin B"))
  expect_equal(groups[[2]]$time_frames, c("in the past year", "in the past year"))
  # asked twice about the same time, they aren't different questions
  twice <- times_draft()
  twice$components$text[2] <- "on how many occasions have you smoked cigarettes in your life?"
  expect_equal(vapply(normalize_service_question_groups(twice, twins), function(group) length(group$ids), 1), 2)
})

test_that("linking gives each variable its time frame, and a subject only to questions about different people", {
  draft <- times_draft()
  draft$variables$subject[1:3] <- "Respondent's Siblings"
  for (group in normalize_service_question_groups(draft, twins)) draft <- normalize_service_link(draft, group, twins)
  expect_equal(draft$variables$time_frame, c(
    "in your lifetime", "in the past 12 months", "in the past 30 days", "in the past year", "in the past year"
  ))
  expect_equal(draft$variables$subject, c(rep("Respondent's Siblings", 3), "Twin A", "Twin B"))
  expect_true(all(vapply(normalize_service_question_groups(draft, twins), function(group) group$linked, TRUE)))
  # the time frame is kept with the draft
  again <- draft_service_from_json(draft_service_to_json(draft))
  expect_equal(again$variables$time_frame, draft$variables$time_frame)
})

test_that("[time frame] is filled in with the time a variable is about", {
  stem <- "[time frame], how would you say your relationship with [subject] has been?"
  expect_equal(
    normalize_service_fill(stem, "Twin A", "in the past year"),
    "in the past year, how would you say your relationship with Twin A has been?"
  )
  expect_equal(normalize_service_fill("have you smoked [time frame]?", NA, NA), "have you smoked [time frame]?") # none to fill
})
