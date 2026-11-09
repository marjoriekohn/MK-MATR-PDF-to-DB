# Wording answer choices one way before an extraction is pushed: as the database writes them, and like the choices
# it has (service_normalize.R); and the time frames the draft writes one way (service_draft.R).

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

test_that("choices that could use a wording the database has are suggested", {
  draft <- choices_draft(c("Yes most of the time", "Not ever", "Never"))
  suggested <- normalize_service_choice_suggestions(draft, c("yes, most of the time", "never"))
  expect_equal(suggested$from, "yes most of the time") # "not ever" looks like nothing the database has
  expect_equal(suggested$to, "yes, most of the time")
  expect_equal(suggested$why, "alike")
  expect_true(suggested$in_database)
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

# ------ Time frames ------------------------------------------------------------------------------
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
