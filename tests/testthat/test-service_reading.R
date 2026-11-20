# Gemini is never called here: stand-ins answer in its place. tests/golden.R
# checks real readings against the hand-checked GNA.


# A questionnaire as Gemini writes it: no display_order, every field on every block.
as_gemini_wrote_it <- function(json) {
  blocks <- function(blocks) {
    lapply(blocks, function(block) {
      list(
        component_type = block$component_type, text = block$text,
        printed_number = block$printed_number, parent_question = block$parent_question,
        subject = block$subject, time_frame = block$time_frame, # a question's own tags
        variables = lapply(block$variables, function(box) {
          list(
            variable_type = box$variable_type, label = box$label, subject = box$subject, time_frame = box$time_frame,
            answer_data_type = box$answer_data_type, unit = box$unit,
            response_options = lapply(box$response_options, function(choice) choice[names(choice) != "display_order"])
          )
        }),
        children = blocks(block$children)
      )
    })
  }
  text <- jsonlite::toJSON(
    list(questionnaire = json$questionnaire, components = blocks(json$components)),
    auto_unbox = TRUE, null = "null"
  )
  as.character(text)
}

question <- function(number, boxes = list(yes_no_box())) {
  list(
    component_type = "Question", text = paste("Question", number), printed_number = number,
    parent_question = NULL, variables = boxes, children = list()
  )
}

yes_no_box <- function() {
  choice <- function(answer) list(answer_option = answer, printed = TRUE)
  list(
    variable_type = "closed_ended", label = NULL, subject = NULL, answer_data_type = NULL,
    response_options = list(choice("Yes"), choice("No"))
  )
}

answer_with <- function(...) {
  list(text = as.character(jsonlite::toJSON(
    list(questionnaire = list(name = NULL, abbreviation = NULL), components = list(...)),
    auto_unbox = TRUE, null = "null"
  )), model = "gemini-test", input_tokens = 100L, output_tokens = 50L, thinking_tokens = 20L)
}

group_of <- function(..., continued = NULL) {
  list(component_type = "Group", continued = continued, text = NULL, children = list(...))
}

section_of <- function(..., text = "Part A", continued = NULL) {
  list(component_type = "Section", continued = continued, text = text, children = list(...))
}

# Answers each call with the next of `calls`: an answer for every page range,
# a list of answers (one per range), or a function that answers (or fails).
# Notes what each prompt asked after the instructions, one line per page range.
stand_in_gemini <- function(...) {
  calls <- list(...)
  asked <- character()
  generate <- function(pdf_path, prompts, model) {
    asked <<- c(asked, paste(sub(".*## Your pages\n\n", "", prompts), collapse = "\n"))
    answer <- calls[[length(asked)]]
    if (is.function(answer)) {
      return(answer())
    }
    if (!is.null(answer$text)) rep(list(answer), length(prompts)) else answer
  }
  list(generate = generate, asked = function() asked)
}

# The reading, and the statuses it reported along the way. The prompt is read
# from the project folder, as the site does.
read_with <- function(gemini, pages = 1, pdf_path = "unused.pdf") {
  statuses <- character()
  root <- app_file()
  lookups <- test_lookups()
  reading <- withr::with_dir(root, reading_service_read(
    pdf_path, lookups,
    generate = gemini$generate, pages = pages,
    progress = function(status) statuses <<- c(statuses, status)
  ))
  c(reading, list(statuses = statuses))
}

test_that("what Gemini writes is tidied into exactly the Edit Extraction page's JSON", {
  gold <- gold_json()
  tidied <- reading_service_tidy(jsonlite::fromJSON(as_gemini_wrote_it(gold), simplifyVector = FALSE))
  expect_identical(tidied, gold)
})

test_that("tidying numbers each list, cleans the text and keeps only each block's fields", {
  reading <- list(questionnaire = list(name = "  Twin  Diet Survey ", abbreviation = ""), components = list(
    list(
      component_type = "Instruction", text = "Mark “Yes” or “No”…", printed_number = NULL,
      variables = list(), children = list()
    ),
    question("12 b.", boxes = list(list(
      variable_type = "closed_ended", label = NULL, subject = "Self", answer_data_type = NULL,
      response_options = list(
        list(answer_option = "Male", printed = TRUE, printed_label = "M"), # an old field: dropped
        list(answer_option = "FEMALE.", printed = TRUE)
      )
    )))
  ))
  tidied <- reading_service_tidy(reading)

  expect_equal(tidied$questionnaire, list(name = "Twin Diet Survey")) # blank abbreviation left out
  instruction <- tidied$components[[1]]
  expect_equal(names(instruction), c("component_type", "display_order", "text"))
  expect_equal(instruction$text, "mark \"yes\" or \"no\"...") # and in lower case
  # words quoted with single marks are quoted with double ones, so the two copies of a question read alike; an
  # apostrophe isn't a quotation mark
  expect_equal(clean_text("interrupt or 'butt in' on the twins' games, and don't stop"), "interrupt or \"butt in\" on the twins' games, and don't stop")
  asked <- tidied$components[[2]]
  expect_equal(asked$display_order, 2)
  expect_equal(asked$printed_number, "12b")
  expect_null(asked$variables[[1]]$subject) # about the respondent: no subject
  choices <- asked$variables[[1]]$response_options
  expect_equal(vapply(choices, function(choice) choice$display_order, integer(1)), 1:2)
  expect_null(choices[[1]]$printed_label) # how it was printed isn't kept
  expect_equal(vapply(choices, function(choice) choice$answer_option, ""), c("male", "female")) # lower case, no full stop
  expect_null(choices[[1]][["printed"]]) # every choice kept is printed, so it isn't said
})

test_that("tidying writes M and F out as the sexes, but leaves F a grade among grades", {
  box <- function(...) {
    list(
      variable_type = "closed_ended", label = NULL, subject = NULL, answer_data_type = NULL,
      response_options = lapply(c(...), function(answer) list(answer_option = answer, printed = TRUE))
    )
  }
  tidied <- reading_service_tidy(list(questionnaire = list(name = "School"), components = list(
    question("1", boxes = list(box("M", "F"))), question("2", boxes = list(box("A+", "D-", "F")))
  )))
  answers <- function(asked) vapply(asked$variables[[1]]$response_options, function(choice) choice$answer_option, "")
  expect_equal(answers(tidied$components[[1]]), c("male", "female"))
  expect_equal(answers(tidied$components[[2]]), c("a+", "d-", "f"))
})

test_that("a reading's words are kept in lower case, without a figure that repeats the number word before it", {
  # an older reading's skips and conditions come along too: they are left behind
  asked <- question("4", boxes = list(list(
    variable_type = "open_ended", label = "Age Started", unit = "Yrs.", subject = "siblings",
    time_frame = "During the LAST 12 months", answer_data_type = "NUMBER", skip_condition = "None",
    skip_instruction = "Go to Question 9"
  )))
  asked$text <- "Do you have more than FIVE (5) brothers and sisters?"
  asked$condition <- "Twins"
  asked$self_contained <- TRUE
  grid <- list(
    component_type = "Grid", text = "During the WORST time:", skip_condition = "No", skip_when = "every",
    skip_instruction = "Go to Question 15", children = list(asked)
  )
  heading <- list(component_type = "Section", text = "Section B: Your Health", children = list(grid))
  reading <- list(questionnaire = list(name = "General Nutrition Assessment", abbreviation = "GNA"), components = list(heading))

  tidied <- reading_service_tidy(reading)
  section <- tidied$components[[1]]
  grid <- section$children[[1]]
  asked <- grid$children[[1]]
  box <- asked$variables[[1]]
  expect_equal(section$text, "section b: your health")
  expect_equal(grid$text, "during the worst time:")
  expect_equal(asked$text, "do you have more than five brothers and sisters?")
  expect_equal(box$label, "age started")
  expect_equal(box$unit, "years") # its own field, abbreviation written out
  # names the database lists, numbers and the questionnaire's own name aren't the form's words
  expect_equal(box$subject, "siblings")
  expect_equal(box$time_frame, "past year") # written one way (normal_time_frame())
  expect_equal(tidied$questionnaire, list(name = "General Nutrition Assessment", abbreviation = "GNA"))
  old_fields <- c("skip_condition", "skip_when", "skip_instruction", "condition", "self_contained")
  expect_false(any(old_fields %in% c(names(grid), names(asked), names(box))))
  # a figure that isn't the number word's stays
  expect_equal(normal_text("Ages 5 (5-9) and Seven (8)"), "ages 5 (5-9) and seven (8)")
})

test_that("a write-in's unit is kept, written one way; a box with choices has none", {
  write_in <- list(variable_type = "open_ended", answer_data_type = "NUMBER", unit = "yrs.")
  choices <- yes_no_box()
  choices$unit <- "times"
  tidied <- reading_service_tidy(list(components = list(question("1", boxes = list(write_in, choices)))))
  boxes <- tidied$components[[1]]$variables
  expect_equal(boxes[[1]]$unit, "years")
  expect_null(boxes[[2]]$unit)
})

test_that("a choice the reading says isn't printed is left out: a lone check box has the one choice", {
  reading <- list(components = list(question("5", boxes = list(list(
    variable_type = "closed_ended", label = "City", subject = "Self",
    response_options = list(
      list(answer_option = "Yes", printed = TRUE),
      list(answer_option = "No", printed = FALSE)
    )
  )))))
  choices <- reading_service_tidy(reading)$components[[1]]$variables[[1]]$response_options
  expect_length(choices, 1)
  expect_equal(choices[[1]]$answer_option, "yes")
  expect_equal(choices[[1]]$display_order, 1)
  # and the checks take one choice as enough
  expect_false(any(grepl("choices", reading_service_problems(reading_service_tidy(reading), test_lookups()))))
})

test_that("a block put inside one that isn't a section moves out to follow it", {
  misplaced <- question("1")
  misplaced$children <- list(question("2"))
  tidied <- reading_service_tidy(list(components = list(misplaced, question("3"))))
  numbers <- vapply(tidied$components, function(block) block$printed_number, "")
  expect_equal(numbers, c("1", "2", "3"))
  expect_null(tidied$components[[1]]$children)
})

test_that("a section or grid recorded twice in a row with the same words is one", {
  # its questions ran onto the next page, and Gemini started it again without saying it carried on
  section <- function(heading, ...) list(component_type = "Section", text = heading, children = list(...))
  grid <- function(lead_in, ...) list(component_type = "Grid", text = lead_in, children = list(...))
  tidied <- reading_service_tidy(list(questionnaire = list(name = "Twin Diet Survey"), components = list(
    section("About alcohol", question("1"), grid("How often", question("2a"))),
    section("ABOUT ALCOHOL", grid("How often", question("2b")), question("3")),
    section("About tobacco", question("4")),
    section("About alcohol", question("5")) # not straight after it: another section of that name
  )))
  expect_equal(vapply(tidied$components, function(block) block$text, ""), c("about alcohol", "about tobacco", "about alcohol"))
  alcohol <- tidied$components[[1]]$children
  expect_equal(vapply(alcohol, function(block) block$component_type, ""), c("Question", "Grid", "Question"))
  expect_equal(vapply(alcohol[[2]]$children, function(block) block$printed_number, ""), c("2a", "2b"))
  expect_equal(alcohol[[3]]$printed_number, "3")
  expect_equal(vapply(alcohol, function(block) block$display_order, integer(1)), 1:3)
})

test_that("a question's measurement scale is kept as its abbreviation, out of its words and off anything else", {
  yes_no <- list(list(variable_type = "closed_ended", response_options = list(list(answer_option = "Yes"), list(answer_option = "No"))))
  tidied <- reading_service_tidy(list(questionnaire = list(name = "Q"), components = list(
    list(component_type = "Question", text = "Are you a worrier?", scale = " (EPQ-R) ", variables = yes_no),
    list(component_type = "Question", text = "Do you own a bicycle?", scale = "", variables = yes_no),
    list(component_type = "Instruction", text = "Do not include your twin.", scale = "EPQ-R")
  )))
  expect_equal(tidied$components[[1]]$scale, "EPQ-R") # capitals stay: it is an abbreviation, not the form's words
  expect_equal(tidied$components[[1]]$text, "are you a worrier?")
  expect_null(tidied$components[[2]]$scale)
  expect_null(tidied$components[[3]]$scale) # only a question is an item of a scale
  expect_equal(
    normal_scale(c("EPQ-R", "[PHQ-9]", "(SF-36).", "  cfq  11 ", " ", NA, "SF-36 (v2)")),
    c("EPQ-R", "PHQ-9", "SF-36", "cfq 11", NA, NA, "SF-36 (v2)")
  )
  # a question in more than one scale has them all, each once, however they were written
  expect_equal(
    normal_scale(c("(FTND) (CDS-12)", "FTND; CDS-12", "(FTND, CDS-12)", " ftnd ,FTND, CDS-12 ", "(FTND)(CDS-12)")),
    c("FTND, CDS-12", "FTND, CDS-12", "FTND, CDS-12", "ftnd, CDS-12", "FTND, CDS-12")
  )
  expect_equal(normal_scale(character()), character())
})

test_that("the prompt lists what the lookup tables hold", {
  instructions <- reading_service_instructions(test_lookups(), app_file("prompts", "extraction.md"))
  expect_false(grepl("{{", instructions, fixed = TRUE))
  expect_match(instructions, '"Interviewer Script"', fixed = TRUE)
  expect_match(instructions, '"Grid"', fixed = TRUE)
  expect_match(instructions, '"siblings"', fixed = TRUE)
  expect_match(instructions, '"past month"', fixed = TRUE)
  expect_match(instructions, "\n  - SMS: Short Mood Scale\n", fixed = TRUE) # a scale by its abbreviation, then what it is short for
  expect_equal(scale_lines(c("Short Mood Scale" = "SMS", "EPQ-R")), "  - SMS: Short Mood Scale\n  - EPQ-R")
  expect_equal(scale_lines(character()), "  - (the registry lists none yet)")
  # the lists are shown as the site writes them now, each name once: a tag kept under an older name isn't offered
  expect_equal(as_written(normal_time_frame, c("past 30 days", "past month", "Past 12 months", "lifetime", NA), "none"), c("past month", "past year", "lifetime"))
  expect_equal(as_written(normal_unit, c("cigarettes per day", "cigarettes", "days per week"), "none"), c("cigarettes", "days"))
  expect_equal(as_written(normal_unit, character(), c("years", "pounds")), c("years", "pounds"))
  expect_match(instructions, "  - not difficult at all", fixed = TRUE)
})

test_that("the prompt's example is a draft with no problems", {
  prompt <- paste(readLines(app_file("prompts", "extraction.md"), warn = FALSE), collapse = "\n")
  example <- sub("(?s).*```json\n(.*?)\n```.*", "\\1", prompt, perl = TRUE)
  draft <- draft_service_from_json(list(questionnaire = list(), components = jsonlite::fromJSON(example, simplifyVector = FALSE)))
  problems <- draft_service_check(draft, test_lookups(), matr_ids = FALSE)
  expect_equal(nrow(problems), 0, info = paste(problems$message, collapse = "; "))
})

test_that("the JSON is found in Gemini's answer, fenced or not", {
  expect_equal(json_in('```json\n{"components": [{"component_type": "Note"}]}\n```')$components[[1]]$component_type, "Note")
  expect_equal(json_in('{"a": {"b": 1}}'), list(a = list(b = 1L)))
  # a bracket too many at the end (Gemini loses count closing a deep nest) is left off; a brace inside words isn't one
  expect_equal(json_in('{"a": [{"b": "1 } or \\"2\\""}]}]}'), list(a = list(list(b = '1 } or "2"'))))
  expect_error(json_in('{"a": [{"b": 1}'), "parse error|premature") # cut short: nothing to recover
  expect_error(json_in("I couldn't read it."), "no JSON object")
})

test_that("a reading with problems is read again, told what they were", {
  gemini <- stand_in_gemini(
    answer_with(question("1"), question("2", boxes = list())),
    answer_with(question("1"), question("2"))
  )
  reading <- read_with(gemini)

  expect_length(reading$problems, 0)
  expect_length(reading$json$components[[2]]$variables, 1)
  expect_match(gemini$asked()[2], "- Question 2: Add at least one variable.", fixed = TRUE)
  expect_equal(reading$statuses, c("sent", "received", "sent", "received"))
  expect_equal(reading$model, "gemini-test")
  expect_match(reading$prompt_version, "^[0-9a-f]{12}$")
})

test_that("the reading with fewer problems is kept, and a good first reading is the only one", {
  worse <- answer_with(question("1", boxes = list()), question("2", boxes = list()))
  better <- answer_with(question("1"), question("2", boxes = list()))
  gemini <- stand_in_gemini(better, worse)
  expect_equal(read_with(gemini)$problems, "Question 2: Add at least one variable.")

  good <- stand_in_gemini(answer_with(question("1")))
  read_with(good)
  expect_length(good$asked(), 1)

  busy <- function() stop("Gemini is busy right now. Try reading again in a few minutes.")
  expect_equal(read_with(stand_in_gemini(better, busy))$problems, "Question 2: Add at least one variable.")
  expect_error(read_with(stand_in_gemini(busy)), "Gemini is busy")
})

test_that("a section heading with its blocks after it, not inside it, is a problem", {
  heading <- section_of(text = "Part A. Your Sleep")
  problems <- reading_service_problems(reading_service_tidy(list(components = list(heading, question("1")))), test_lookups())
  expect_equal(problems, 'Section "part a. your sleep": has nothing in it. Put the blocks printed under it inside it, as its children.')
})

test_that("a reading with no questions is a problem", {
  empty <- list(component_type = "Group", text = NULL, variables = list(), children = list())
  problems <- reading_service_problems(reading_service_tidy(list(components = list(empty))), test_lookups())
  expect_equal(problems[1], "No questions were recorded. Record every page of the questionnaire.")
  expect_match(problems[2], "^A group: has nothing in it.")
})

test_that("a section without its heading, a group with words, and a section inside a group are problems", {
  untitled <- section_of(question("1"), text = NULL)
  worded <- group_of(question("2"))
  worded$text <- "Lead-in"
  nested <- group_of(section_of(question("3"), text = "Part B"))
  problems <- reading_service_problems(reading_service_tidy(list(components = list(untitled, worded, nested))), test_lookups())
  expect_true(any(grepl("^A section with no heading: Add the section's printed heading", problems)))
  expect_true(any(grepl("group has no words of its own", problems)))
  expect_true(any(grepl("can't be inside a group", problems)))
})

test_that("printed numbers can repeat or be missing, and a parent is the nearest earlier question with its number", {
  first <- question("1")
  again <- question("1")
  follow_up <- question("2")
  follow_up$parent_question <- "1"
  unnumbered <- question("1")
  unnumbered$printed_number <- NULL
  json <- reading_service_tidy(list(components = list(section_of(first, text = "Part A"), section_of(again, follow_up, unnumbered, text = "Part B"))))
  expect_length(reading_service_problems(json, test_lookups()), 0)
  draft <- draft_service_from_json(json)
  questions <- draft$components[draft$components$component_type == "Question", ]
  expect_equal(questions$question_number, c("1", "2", "3", "4")) # their places, worked out
  expect_equal(questions$printed_number, c("1", "1", "2", NA))
  expect_equal(questions$parent_question_id[3], questions$id[2]) # the "1" in its own section, not the first
})

test_that("a questionnaire is read two pages at a time, and the pieces joined", {
  pages_1_2 <- answer_with(question("1"), group_of(question("2a")))
  page_3 <- answer_with(group_of(question("2b"), continued = TRUE), question("3"))
  gemini <- stand_in_gemini(list(pages_1_2, page_3))
  reading <- read_with(gemini, pages = 3)

  expect_equal(gemini$asked(), "This PDF has 3 pages. Record pages 1-2.\nThis PDF has 3 pages. Record page 3.")
  blocks <- reading$json$components
  expect_equal(vapply(blocks, function(block) block$component_type, ""), c("Question", "Group", "Question"))
  expect_equal(vapply(blocks[[2]]$children, function(block) block$printed_number, ""), c("2a", "2b"))
  expect_null(blocks[[2]]$continued)
  # each block notes the first page of the range it was read with; a group carried on keeps its first
  expect_equal(vapply(blocks, function(block) block$page, 1), c(1, 1, 3))
  expect_equal(vapply(blocks[[2]]$children, function(block) block$page, 1), c(1, 3))
  expect_equal(reading$tokens$output, 100)
  expect_equal(reading$tokens$most_in_one_answer, 70)
})

test_that("a long PDF goes to Gemini cut to each request's pages and two on either side", {
  cuts <- list()
  local_stub("extraction_model_pdf_pages", function(path, from, to) {
    cuts[[length(cuts) + 1]] <<- c(from, to)
    sprintf("pages_%d-%d.pdf", from, to)
  })
  local_stub("reading_whole_pdf_pages", 4)
  sent_to <- NULL
  three <- function() lapply(1:3, function(i) answer_with(question(as.character(i))))
  gemini <- stand_in_gemini(three, three) # a reading with a cut PDF, then one without
  generate <- gemini$generate
  gemini$generate <- function(pdf_path, prompts, model) {
    sent_to <<- pdf_path
    generate(pdf_path, prompts, model)
  }
  read_with(gemini, pages = 5, pdf_path = "long.pdf")
  expect_equal(cuts, list(c(1, 4), c(1, 5), c(3, 5)))
  expect_equal(sent_to, c("pages_1-4.pdf", "pages_1-5.pdf", "pages_3-5.pdf"))
  expect_equal(strsplit(gemini$asked(), "\n")[[1]][3], paste(
    "This PDF is pages 3-5 of a questionnaire of 5 pages. Record page 5 of the 5: this PDF's page 3.",
    "Its other pages are there only to show what comes before and after them."
  ))

  # without qpdf to cut it, every request gets the whole PDF
  local_stub("extraction_model_pdf_pages", function(path, from, to) NULL)
  expect_message(read_with(gemini, pages = 5, pdf_path = "long.pdf"), "qpdf isn't installed")
  expect_equal(sent_to, "long.pdf")
})

test_that("a section carried on inside a carried-on section joins its first part", {
  earlier <- list(section_of(question("1"), group_of(question("2a"))))
  later <- list(section_of(group_of(question("2b"), continued = TRUE), question("3"), continued = TRUE))
  joined <- join_page_ranges(list(list(components = earlier), list(components = later)))$components
  outer <- joined[[1]]$children
  expect_length(joined, 1)
  expect_equal(vapply(outer, function(block) block$component_type, ""), c("Question", "Group", "Question"))
  expect_equal(vapply(outer[[2]]$children, function(block) block$printed_number, ""), c("2a", "2b"))
})

test_that("Gemini counts the pages of a PDF that doesn't say how many it has", {
  packed <- fake_upload(text = "%PDF-1.5\n(the list of pages is compressed)\n")$datapath
  gemini <- stand_in_gemini(list(text = "3"), list(answer_with(question("1")), answer_with(question("2"))))
  read_with(gemini, pages = NULL, pdf_path = packed)
  expect_equal(gemini$asked(), c(
    "How many pages does this PDF have? Answer with the number only.",
    "This PDF has 3 pages. Record pages 1-2.\nThis PDF has 3 pages. Record page 3."
  ))
})

test_that("a parent named by a number no question has is a problem", {
  orphan <- question("5")
  orphan$parent_question <- "4"
  problems <- reading_service_problems(reading_service_tidy(list(components = list(orphan))), test_lookups())
  expect_equal(problems, "Question 5: names question 4 as its parent, but no question has that number.")
})

test_that("an answer that isn't JSON is asked for again, and two of them fail", {
  cut_off <- list(text = '{"questionnaire": {"name": null', model = "gemini-test")
  gemini <- stand_in_gemini(cut_off, answer_with(question("1")))
  expect_length(read_with(gemini)$problems, 0)
  expect_match(gemini$asked()[2], "wasn't complete JSON")

  expect_error(read_with(stand_in_gemini(cut_off, cut_off)), "couldn't be read as a questionnaire")
})

test_that("Gemini's errors become sentences staff can act on", {
  expect_error(gemini_model_parse(list(error = list(message = "overloaded")), 503), "Gemini is busy right now")
  expect_error(gemini_model_parse(list(), 403), "Check GEMINI_KEY")
  expect_error(gemini_model_parse(list(candidates = list(list(finishReason = "MAX_TOKENS")))), "too long")
  expect_error(gemini_model_parse(list(promptFeedback = list(blockReason = "SAFETY"))), "wouldn't read this PDF")
  expect_error(gemini_model_generate("unused.pdf", "", api_key = ""), "Add GEMINI_KEY")

  answer <- gemini_model_parse(list(
    modelVersion = "gemini-3.8-flash",
    usageMetadata = list(promptTokenCount = 10L, candidatesTokenCount = 5L),
    candidates = list(list(finishReason = "STOP", content = list(parts = list(
      list(text = "thinking...", thought = TRUE),
      list(text = '{"a": 1}')
    ))))
  ))
  expect_equal(answer$text, '{"a": 1}')
  expect_equal(answer$model, "gemini-3.8-flash")
})

test_that("requests Gemini turns away as busy are sent again, for a while", {
  replies <- c(503, 200)
  httr2::local_mocked_responses(function(req) {
    status <- replies[1]
    replies <<- replies[-1]
    httr2::response(status_code = status)
  })
  request <- httr2::request("https://gemini.test") |> httr2::req_error(is_error = function(response) FALSE)
  expect_equal(httr2::resp_status(gemini_model_perform(list(request), pause = 0.01)[[1]]), 200)

  httr2::local_mocked_responses(function(req) httr2::response(status_code = 503))
  expect_equal(httr2::resp_status(gemini_model_perform(list(request), give_up_after = 0.1, pause = 0.01)[[1]]), 503)
})

test_that("the request sends the PDF and then the prompt, and hides the key", {
  pdf <- fake_upload()$datapath
  request <- gemini_model_request(pdf, "the prompt", "gemini-3.8-flash", "secret")
  body <- request$body$data
  expect_match(request$url, "models/gemini-3.8-flash:generateContent$")
  parts <- body$contents[[1]]$parts
  expect_equal(parts[[1]]$inlineData$mimeType, "application/pdf")
  expect_equal(parts[[1]]$inlineData$data, base64enc::base64encode(pdf))
  expect_equal(parts[[2]]$text, "the prompt")
  expect_equal(body$generationConfig, list(maxOutputTokens = 65536L))
  expect_false(any(grepl("secret", capture.output(print(request)))))
})

test_that("a request about a file given as text sends the prompt alone", {
  request <- gemini_model_request(NULL, "the prompt and the file", "gemini-3.8-flash", "secret")
  parts <- request$body$data$contents[[1]]$parts
  expect_length(parts, 1)
  expect_equal(parts[[1]]$text, "the prompt and the file")
})

# ------ Variable IDs and codes from a second file ---------------------------------------------
test_that("a variable is described to the code file's reader with who and when it is about, its own or its question's", {
  described <- assign_variables(list(components = list(list(
    component_type = "Question", text = "on how many occasions have the twins smoked?", printed_number = "49", subject = "twins",
    variables = list(
      list(variable_type = "open_ended", time_frame = "lifetime"),
      list(variable_type = "open_ended", time_frame = "past 12 months", subject = "twin a")
    )
  ))))
  expect_equal(vapply(described, `[[`, "", "when"), c("lifetime", "past 12 months"))
  expect_equal(vapply(described, `[[`, "", "about"), c("twins", "twin a"))
})

test_that("a reading's problems name the right blocks when questions about each twin were joined", {
  asked <- function(text, number, ...) {
    c(list(component_type = "Question", text = text, printed_number = number, variables = list(
      list(variable_type = "open_ended", answer_data_type = "NUMBER")
    )), list(...))
  }
  json <- list(questionnaire = list(), components = list(
    asked("How old are the twins?", "8", subject = "twin a"),
    asked("How old are the twins?", "24", subject = "twin b"), # joined with 8 when the reading is opened
    asked("Do you smoke?", "40"),
    asked("If yes, how much?", "41", parent_question = "99") # no question 99
  ))
  expect_no_warning(problems <- reading_service_problems(json, test_lookups()))
  expect_equal(problems, "Question 41: names question 99 as its parent, but no question has that number.")
})

assign_prompt <- app_file("prompts", "assign_codes.md")

test_that("the file of Variable IDs and codes gives the variables their IDs and the choices their codes", {
  json <- reading_service_tidy(gold_json())
  variables <- assign_variables(json)
  expect_equal(variables[[1]]$id, "v1")
  expect_equal(variables[[1]]$choices[[1]]$id, "o1")
  n_choices <- length(variables[[1]]$choices)
  expect_equal(variables[[2]]$choices[[1]]$id, paste0("o", n_choices + 1)) # choices count on across variables

  sent <- NULL
  generate <- function(pdf_path, prompts, model) {
    sent <<- list(pdf_path = pdf_path, prompt = prompts)
    list(list(text = jsonlite::toJSON(list(variables = list(
      list(id = "v1", matr_id = "AH01", choices = lapply(seq_len(n_choices), function(i) list(id = paste0("o", i), code = letters[i]))),
      list(id = "v2", matr_id = "AH01"), # an ID already given: neither gets it
      list(id = "v3", matr_id = "AH03", scale = " (SMS) "), # the file names its question's scale
      list(id = "v99", matr_id = "nobody") # no such variable
    )), auto_unbox = TRUE)))
  }
  filled <- reading_service_assign(
    json, list(name = "codes.csv", text = "AH01,1,yes"),
    generate = generate, prompt_path = assign_prompt, scales = test_lookups()$scales
  )
  expect_null(sent$pdf_path) # text goes in the prompt
  expect_match(sent$prompt, "\n  - SMS: Short Mood Scale\n", fixed = TRUE) # the scales it can name
  expect_false(any(grepl("{{", sent$prompt, fixed = TRUE)))
  expect_match(sent$prompt, "## The file (codes.csv)", fixed = TRUE)
  expect_match(sent$prompt, '"id":"v1"', fixed = TRUE)

  # only Variable IDs, codes and scales are added: the questionnaire is otherwise exactly as it was read
  strip <- function(x) {
    if (!is.list(x)) return(x)
    if (!is.null(names(x))) x <- x[setdiff(names(x), c("matr_id", "code_value", "scale"))]
    lapply(x, strip)
  }
  expect_identical(strip(filled), strip(json))

  draft <- draft_service_from_json(filled)
  expect_equal(draft$variables$matr_id[1:3], c(NA, NA, "AH03"))
  # the question of the variable whose entry names a scale is in it, and no other
  expect_equal(draft$components$scale[draft$components$id == draft$variables$component_id[3]], "SMS")
  expect_equal(sum(!is.na(draft$components$scale)), 1)
  # a scale the questionnaire itself printed stands; any of a question's variables' entries can name one
  tiny <- list(components = list(
    list(component_type = "Question", text = "a", scale = "PHQ-9", variables = list(list(variable_type = "open_ended"))),
    list(component_type = "Question", text = "b", variables = list(list(variable_type = "open_ended"), list(variable_type = "open_ended")))
  ))
  named <- assign_into(tiny, list(variables = list(
    list(id = "v1", scale = "EPQ-R"), list(id = "v2", scale = "(FTND) (CDS-12)"), list(id = "v3", scale = "[cds-12]")
  )))
  expect_equal(named$components[[1]]$scale, "PHQ-9")
  expect_equal(named$components[[2]]$scale, "FTND, CDS-12") # every scale its variables' entries name, each once
  first <- draft_service_children(draft, "response_options", draft$variables$id[1])
  expect_equal(first$code_value, letters[seq_len(n_choices)]) # codes can be letters
  # a variable the file didn't code is counted 1, 2, 3... as before
  second <- draft_service_children(draft, "response_options", draft$variables$id[2])
  expect_equal(second$code_value, as.character(seq_len(nrow(second))))
})

test_that("a long questionnaire's variables go in several requests, each with the whole file, and the answers are joined", {
  local_stub("reading_assign_batch", 2)
  json <- reading_service_tidy(gold_json())
  n_variables <- length(assign_variables(json))
  sent <- NULL
  generate <- function(pdf_path, prompts, model) {
    sent <<- prompts
    lapply(seq_along(prompts), function(i) {
      first <- 2 * i - 1 # each answer names its own batch's variables
      list(text = sprintf('{"variables":[{"id":"v%d","matr_id":"ID%d"}]}', first, first))
    })
  }
  filled <- draft_service_from_json(reading_service_assign(json, list(name = "codes.csv", text = "the file"), generate = generate, prompt_path = assign_prompt))
  expect_length(sent, ceiling(n_variables / 2))
  expect_true(all(grepl("## The file (codes.csv)", sent, fixed = TRUE)))
  # (the instructions' own example names v1: only what follows the last "## The variables" is the batch)
  batch <- sub("(?s).*## The variables", "", sent[2], perl = TRUE)
  expect_match(batch, '"id":"v3"', fixed = TRUE)
  expect_false(grepl('"id":"v1"', batch, fixed = TRUE))
  expect_equal(filled$variables$matr_id[c(1, 3, 5)], c("ID1", "ID3", "ID5"))
})

test_that("a PDF of Variable IDs and codes is sent as the request's file, and codes that repeat are left out", {
  json <- reading_service_tidy(gold_json())
  sent <- NULL
  generate <- function(pdf_path, prompts, model) {
    sent <<- pdf_path
    list(list(text = '{"variables":[{"id":"v1","choices":[{"id":"o1","code":"1"},{"id":"o2","code":"1"}]}]}'))
  }
  filled <- reading_service_assign(json, list(name = "codebook.pdf", path = "codebook.pdf"), generate = generate, prompt_path = assign_prompt)
  expect_equal(sent, "codebook.pdf")
  first <- draft_service_children(draft_service_from_json(filled), "response_options", "v1")
  expect_false(anyDuplicated(first$code_value) > 0)
  expect_equal(first$code_value[1:2], c("1", "2")) # counted, not the repeated "1"
})

closed <- function(..., label = NULL, subject = NULL) {
  answers <- c(...)
  without_blanks(list(
    variable_type = "closed_ended", label = label, subject = subject,
    response_options = lapply(seq_along(answers), function(i) list(display_order = i, answer_option = answers[i]))
  ))
}
asking <- function(...) list(component_type = "Question", text = "q", variables = list(...))
answers_of <- function(variable) vapply(variable$response_options, function(choice) choice$answer_option, "")
codes_of <- function(variable) vapply(variable$response_options, function(choice) choice$code_value %||% NA_character_, "")

test_that("choices the file codes that the questionnaire doesn't print are added after the printed ones", {
  json <- list(components = list(
    asking(closed("yes")), # a lone box
    asking(closed("yes", "no")),
    asking(closed("yes", "no")),
    asking(list(variable_type = "open_ended", answer_data_type = "NUMBER"))
  ))
  filled <- assign_into(json, list(variables = list(
    list(id = "v1", choices = list(list(id = "o1", code = "1")), more_choices = list(list(answer = "No", code = "2"))),
    list(
      id = "v2", choices = list(list(id = "o2", code = "1"), list(id = "o3", code = "2")),
      more_choices = list(
        list(answer = "No answer.", code = "99"),
        list(answer = "no", code = "3"), # the questionnaire has it already
        list(answer = "refused", code = "2") # its code is taken
      )
    ),
    # a printed choice the file doesn't code would be counted, and could clash: nothing is added
    list(id = "v3", choices = list(list(id = "o4", code = "1")), more_choices = list(list(answer = "no answer", code = "2"))),
    list(id = "v4", more_choices = list(list(answer = "missing", code = "-9"))) # written in: no choices
  )))
  variables <- lapply(filled$components, function(block) block$variables[[1]])
  expect_equal(answers_of(variables[[1]]), c("yes", "no"))
  expect_equal(codes_of(variables[[1]]), c("1", "2"))
  expect_equal(variables[[1]]$response_options[[2]]$display_order, 2)
  expect_equal(answers_of(variables[[2]]), c("yes", "no", "no answer"))
  expect_equal(codes_of(variables[[2]]), c("1", "2", "99"))
  expect_equal(answers_of(variables[[3]]), c("yes", "no"))
  expect_null(variables[[4]]$response_options)
})

test_that("a check-all list the file codes as one variable is joined into one, a choice for each box", {
  other <- list(variable_type = "open_ended", label = "please describe", answer_data_type = "TEXT")
  json <- list(components = list(
    asking(closed("yes", "no", label = "my own paycheck"), closed("yes", "no", label = "other"), other,
           closed("yes", "no", label = "prefer not to answer")),
    asking(closed("yes", "no", subject = "mother"), closed("yes", "no", subject = "father"), closed("yes", "no", label = "someone else")), # people, and words
    asking(closed("yes", "no", label = "dog"), closed("yes", "no", label = "cat")) # two boxes, one code
  ))
  box <- function(id, name, code) list(id = id, matr_id = name, box_code = code)
  filled <- assign_into(json, list(variables = list(
    box("v1", "income", "1"), box("v2", "income", "6"), list(id = "v3", matr_id = "income_o"), box("v4", "income", "999"),
    box("v5", "lives", "1"), box("v6", "lives", "2"), box("v7", "lives", "7"),
    box("v8", "pets", "1"), box("v9", "pets", "1")
  )))
  income <- filled$components[[1]]$variables
  expect_length(income, 2)
  expect_equal(income[[1]]$matr_id, "income") # the field's name, not a box's ("income___1")
  expect_equal(answers_of(income[[1]]), c("my own paycheck", "other", "prefer not to answer"))
  expect_equal(codes_of(income[[1]]), c("1", "6", "999"))
  expect_null(income[[1]]$label)
  expect_equal(income[[2]]$matr_id, "income_o") # the write-in stays, after it
  expect_equal(vapply(income, function(variable) variable$display_order, 1), 1:2)

  lives <- filled$components[[2]]$variables
  expect_length(lives, 1)
  expect_equal(answers_of(lives[[1]]), c("mother", "father", "someone else"))
  expect_equal(codes_of(lives[[1]]), c("1", "2", "7"))
  expect_null(lives[[1]]$subject)

  pets <- filled$components[[3]]$variables # can't be joined: kept as read, without the field's name
  expect_length(pets, 2)
  expect_null(pets[[1]]$matr_id)
  expect_equal(answers_of(pets[[1]]), c("yes", "no"))

  # the same field's name on boxes of two questions is given twice, and neither keeps it
  twice <- assign_into(
    list(components = list(asking(closed("yes", "no", label = "a")), asking(closed("yes", "no", label = "b")))),
    list(variables = list(box("v1", "same", "1"), box("v2", "same", "2")))
  )
  expect_null(twice$components[[1]]$variables[[1]]$matr_id)
  expect_null(twice$components[[2]]$variables[[1]]$matr_id)
})

test_that("the questionnaire PDF as the file is named as such, and a long one is cut to each batch's pages", {
  local_stub("reading_assign_batch", 2)
  local_stub("reading_whole_pdf_pages", 4)
  local_stub("extraction_model_pdf_pages", function(path, from, to) sprintf("pages_%d-%d.pdf", from, to))
  json <- list(components = lapply(c(1, 3, 7), function(page) c(asking(closed("yes", "no")), page = page)))
  sent <- NULL
  generate <- function(pdf_path, prompts, model) {
    sent <<- list(paths = pdf_path, prompts = prompts)
    lapply(prompts, function(prompt) list(text = '{"variables":[]}'))
  }
  questionnaire <- list(name = "survey.pdf", path = "survey.pdf", pages = 10, questionnaire = TRUE)
  reading_service_assign(json, questionnaire, generate = generate, prompt_path = assign_prompt, answer_options = c("yes", "no answer"))
  expect_equal(sent$paths, c("pages_1-6.pdf", "pages_7-10.pdf")) # from the first page to two past the pair
  expect_match(sent$prompts[1], "The file is the questionnaire PDF itself: its pages 1-6, of 10.", fixed = TRUE)
  expect_match(sent$prompts[1], "\n  - no answer\n", fixed = TRUE) # the registry's wordings
  expect_false(any(grepl('"page"', sent$prompts, fixed = TRUE))) # the page isn't sent

  reading_service_assign(json, modifyList(questionnaire, list(pages = 4)), generate = generate, prompt_path = assign_prompt)
  expect_equal(sent$paths, "survey.pdf") # a short one goes whole
  expect_match(sent$prompts[1], "The file is the questionnaire PDF itself.", fixed = TRUE)
})

# ------ The job ------------------------------------------------------------------------
# Every reading ends by looking for Variable IDs and codes (in the draft's file of them, or else its PDF), which would
# call Gemini: jobs whose tests aren't about that get none.
no_codes <- function(json, ...) json

# The watcher is given the drafts of the staff the test made only: the database holds other people's too, and it would
# start (and stop) jobs for them.
my_drafts <- function(...) {
  who <- vapply(list(...), function(staff) staff$staff_id, numeric(1))
  Filter(function(record) record$prepared_by_id %in% who, extraction_service_get_all(test_connection))
}

test_that("the job saves what Gemini read and marks the draft ready", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  seen <- character()
  read <- function(pdf_path, lookups, progress) {
    progress("sent")
    seen <<- extraction_model_get(test_connection, record$id)$extraction_status
    progress("received")
    list(json = short_gold_json(), model = "gemini-3.8-flash", prompt_version = "abc123")
  }
  reading_service_run(test_connection, record$id, record$reading_started, read = read, assign = no_codes)

  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(seen, "sent")
  expect_equal(saved$extraction_status, "ready")
  expect_equal(saved$prompt_version, "abc123")
  expect_equal(saved$model, "gemini-3.8-flash")
  expect_identical(draft_service_from_json(extraction_model_questionnaire(test_connection, record$id)$original), short_gold_draft())
  expect_equal(nrow(extraction_service_load_draft(test_connection, saved)$components), nrow(short_gold_draft()$components))
})

test_that("the job reads the file of Variable IDs and codes once the questionnaire is read, and carries on without it if it fails", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me, supplement = fake_csv_upload(c("AH01", "AH02"), "codes.csv"))
  expect_equal(record$supplement$name, "codes.csv")
  read <- function(pdf_path, lookups, progress) list(json = short_gold_json(), model = "m", prompt_version = "p")
  given <- NULL
  assign <- function(json, supplement, ...) {
    given <<- supplement
    json$components[[1]]$variables[[1]]$matr_id <- "AH01"
    json
  }
  reading_service_run(test_connection, record$id, record$reading_started, read = read, assign = assign)
  expect_equal(given, list(name = "codes.csv", text = "AH01\nAH02"))
  original <- draft_service_from_json(extraction_model_questionnaire(test_connection, record$id)$original)
  expect_equal(original$variables$matr_id[1], "AH01")

  again <- extraction_service_create(test_connection, fake_upload(), me, supplement = fake_csv_upload("AH01", "codes.csv"))
  messages <- capture_messages(reading_service_run(
    test_connection, again$id, again$reading_started, read = read, assign = function(json, supplement, ...) stop("Gemini is busy")
  ))
  expect_match(messages, "couldn't be applied: Gemini is busy", all = FALSE)
  read_anyway <- extraction_model_get(test_connection, again$id)
  expect_equal(read_anyway$extraction_status, "ready")
  expect_equal(read_anyway$error, "Gemini is busy") # shown on the step, which says to type the IDs on Edit Extraction
})

test_that("without a file of Variable IDs and codes, the job looks for them in the questionnaire PDF itself", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  read <- function(pdf_path, lookups, progress) list(json = short_gold_json(), model = "m", prompt_version = "p")
  given <- NULL
  wordings <- NULL
  assign <- function(json, supplement, scales = NULL, answer_options = NULL) {
    given <<- supplement
    wordings <<- answer_options
    json
  }
  reading_service_run(test_connection, record$id, record$reading_started, read = read, assign = assign)
  expect_true(given$questionnaire)
  expect_equal(given$name, record$pdf$name)
  expect_identical(readBin(given$path, "raw", 1e4), readBin(record$pdf$path, "raw", 1e4))
  expect_true(length(wordings) > 0) # the registry's wordings, for choices the file codes that the PDF doesn't print
  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$extraction_status, "ready")
  expect_null(saved$error)
})

test_that("Re-extract reads a draft's PDF again, putting aside the questionnaire staff reviewed", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  read <- function(pdf_path, lookups, progress) list(json = short_gold_json(), model = "m", prompt_version = "p")
  reading_service_run(test_connection, record$id, record$reading_started, read = read, assign = no_codes)
  checked <- draft_service_update(short_gold_draft(), "components", "c3", "checked", TRUE)
  revision <- draft_service_save(test_connection, record$id, checked, me$staff_id)
  expect_false(is.null(extraction_model_questionnaire(test_connection, record$id)$questionnaire))
  # not over a save someone else made since it was opened
  expect_error(extraction_service_reextract(test_connection, record$id, me, expected_revision = revision - 1L))

  again <- extraction_service_reextract(test_connection, record$id, me, expected_revision = revision)
  expect_equal(again$extraction_status, "queued")
  expect_null(extraction_model_questionnaire(test_connection, record$id)$questionnaire)
  # once read again, it opens as the new reading, nothing marked as checked
  reading_service_run(test_connection, record$id, again$reading_started, read = read, assign = no_codes)
  expect_false(any(extraction_service_load_draft(test_connection, again)$components$checked))

  typed <- extraction_service_create_manual(test_connection, me)
  expect_error(extraction_service_reextract(test_connection, typed$id, me), "typed by hand")
})

test_that("a file of Variable IDs and codes that isn't a PDF, CSV or text is turned away", {
  path <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("not really", path)
  expect_error(store_supplement(NULL, data.frame(name = "codes.xlsx", size = 10, datapath = path)), "Save a spreadsheet as CSV")
})

test_that("the job reads the pick lists from the database for Gemini", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  given <- NULL
  read <- function(pdf_path, lookups, progress) {
    given <<- lookups
    list(json = short_gold_json(), model = "m", prompt_version = "p")
  }
  subject <- local_row("tag", list(kind = "subject", name = test_name("Subject")), "tag_id")
  unit <- local_row("tag", list(kind = "unit", name = test_name("unit")), "tag_id")
  reading_service_run(test_connection, record$id, record$reading_started, read = read, assign = no_codes)
  expect_true(subject$name %in% given$subjects)
  expect_true(unit$name %in% given$units)
  expect_true("Section" %in% given$component_types)
})

test_that("a reading that fails says why", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  read <- function(...) stop("Gemini is busy right now. Try reading again in a few minutes.")
  reading_service_run(test_connection, record$id, record$reading_started, read = read, assign = no_codes)

  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$extraction_status, "failed")
  expect_equal(saved$error, "Gemini is busy right now. Try reading again in a few minutes.")
})

test_that("a PDF this server hasn't got is fetched from Storage to read", {
  me <- local_staff()
  local_pdf_cache()
  upload <- fake_upload()
  record <- extraction_service_create(test_connection, upload, me)
  withr::defer(test_delete_pdf(record$pdf$sha256))
  unlink(record$pdf$path) # as a hosted site that restarted
  read_from <- NULL
  read <- function(pdf_path, lookups, progress) {
    read_from <<- pdf_path
    list(json = short_gold_json(), model = "gemini-3.8-flash", prompt_version = "abc123")
  }
  reading_service_run(test_connection, record$id, record$reading_started, read = read, assign = no_codes)
  expect_equal(extraction_model_get(test_connection, record$id)$extraction_status, "ready")
  expect_identical(readBin(read_from, "raw", 1e4), readBin(upload$datapath, "raw", 1e4))
})

test_that("a PDF that is no longer anywhere fails the reading with a way forward", {
  me <- local_staff()
  local_pdf_cache()
  # a PDF of its own: other runs of the tests upload the usual fake PDF to Storage meanwhile
  upload <- fake_upload(text = paste0("%PDF-1.4\n% ", test_name("gone"), "\n<< /Type /Page >>\n"))
  record <- extraction_service_create(test_connection, upload, me)
  withr::defer(test_delete_pdf(record$pdf$sha256), priority = "last") # after the draft that points at it
  unlink(record$pdf$path)
  test_delete_stored_pdf(record$pdf$sha256) # uploaded before PDFs were kept in Storage
  reading_service_run(test_connection, record$id, record$reading_started, read = function(...) stop("not reached"))

  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$extraction_status, "failed")
  expect_match(saved$error, "isn't on this site any more. Choose Replace PDF")
})

test_that("a job whose reading was restarted saves nothing", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me, now = Sys.time() - 60)
  read <- function(pdf_path, lookups, progress) {
    extraction_service_retry(test_connection, record$id, me) # staff press Try reading again meanwhile
    list(json = short_gold_json(), model = "gemini-3.8-flash", prompt_version = "abc123")
  }
  reading_service_run(test_connection, record$id, record$reading_started, read = read, assign = no_codes)

  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$extraction_status, "queued")
  expect_null(extraction_model_questionnaire(test_connection, record$id)$original)
})

test_that("a reading whose job has died is marked failed", {
  me <- local_staff()
  colleague <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  extraction_service_reading_update(test_connection, record$id, record$reading_started, list(
    extraction_status = "sent", reading_pid = 999999L, # no such process
    reading_claimed_at = format_time(Sys.time())
  ))
  # a draft discarded meanwhile has left the list, so there is no reading of it to watch
  discarded <- extraction_service_create(test_connection, fake_upload(), colleague, now = Sys.time() - 60)
  extraction_service_discard(test_connection, discarded$id, colleague)

  still_reading <- reading_service_watch(my_drafts(me, colleague), test_connection)
  expect_true(still_reading)
  saved <- extraction_service_get_all(test_connection)
  expect_equal(saved[[record$id]]$extraction_status, "failed")
  expect_equal(saved[[record$id]]$error, "Reading stopped before it finished.")
  expect_false(discarded$id %in% names(saved))
  expect_error(extraction_model_get(test_connection, discarded$id), "no longer in the database") # a discarded draft is deleted
})

test_that("a draft another copy of the site has claimed isn't started again", {
  me <- local_staff()
  colleague <- local_staff()
  withr::local_envvar(GEMINI_KEY = "") # a job started by mistake couldn't call Gemini
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  extraction_model_claim_reading(test_connection, record$id, record$reading_started)
  extraction_service_reading_update(test_connection, record$id, record$reading_started, list(reading_pid = Sys.getpid()))

  expect_true(reading_service_watch(my_drafts(me, colleague), test_connection))
  expect_null(reading_jobs[[record$id]])
  expect_equal(extraction_model_get(test_connection, record$id)$extraction_status, "queued")
})

test_that("a claimed draft whose job hasn't recorded its process yet gets a short grace", {
  claimed <- "2026-10-02T10:00:00Z"
  record <- list(reading_pid = NULL, reading_claimed_at = claimed)
  expect_false(job_is_lost(record, now = as.POSIXct("2026-10-02 10:00:30", tz = "UTC")))
  expect_true(job_is_lost(record, now = as.POSIXct("2026-10-02 10:05:00", tz = "UTC")))
  expect_true(job_is_lost(list(reading_pid = 999999L, reading_claimed_at = claimed)))
  expect_false(job_is_lost(list(reading_pid = Sys.getpid(), reading_claimed_at = claimed)))
})

test_that("a draft that loses the race to claim it isn't started by this site", {
  me <- local_staff()
  withr::local_envvar(GEMINI_KEY = "")
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  # Another site claims it between this site reading the list and starting a job.
  stale <- my_drafts(me)
  extraction_model_claim_reading(test_connection, record$id, record$reading_started)

  expect_true(reading_service_watch(stale, test_connection))
  expect_null(reading_jobs[[record$id]])
})

# ------ Gemini's other answers ---------------------------------------------------------
test_that("the rest of Gemini's refusals, and an answer that stops early or says nothing, are sentences too", {
  expect_error(gemini_model_parse(list(error = list(message = "bad PDF")), 400), "Gemini couldn't take the request: bad PDF")
  expect_error(gemini_model_parse(list(error = list(message = "no such model")), 404), "Gemini doesn't know that model: no such model")
  expect_error(gemini_model_parse(list(error = list(message = "oops")), 500), "Gemini had a problem \\(500\\): oops")
  expect_error(gemini_model_parse(list(), 401), "Check GEMINI_KEY")
  expect_error(gemini_model_parse(list(), 429), "Gemini is busy right now")
  expect_error(
    gemini_model_parse(list(candidates = list(list(finishReason = "RECITATION")))),
    "Gemini stopped before the end \\(RECITATION\\)"
  )
  expect_error(gemini_model_parse(list(candidates = list(list(content = list(parts = list()))))), "stopped before the end \\(UNKNOWN\\)")
  empty <- list(candidates = list(list(finishReason = "STOP", content = list(parts = list(list(text = "", thought = FALSE))))))
  expect_error(gemini_model_parse(empty), "Gemini sent back an empty answer")
})

test_that("each prompt gets its own answer, in order, and a request that couldn't be made is a sentence", {
  pdf <- fake_upload()$datapath
  httr2::local_mocked_responses(function(req) {
    prompt <- req$body$data$contents[[1]]$parts[[2]]$text
    httr2::response_json(body = list(
      modelVersion = "gemini-3.8-flash",
      candidates = list(list(finishReason = "STOP", content = list(parts = list(list(text = paste("answer to", prompt))))))
    ))
  })
  answers <- gemini_model_generate(pdf, c("first", "second"), api_key = "secret")
  expect_equal(vapply(answers, function(answer) answer$text, character(1)), c("answer to first", "answer to second"))
  expect_equal(answers[[1]]$input_tokens, 0L)

  local_stub("gemini_model_perform", function(requests, ...) list(simpleError("Couldn't resolve host")))
  expect_error(gemini_model_generate(pdf, "first", api_key = "secret"), "Couldn't reach Gemini: Couldn't resolve host")
})

# ------ The jobs ------------------------------------------------------------------------
# A job is an R process that reads one draft. These tests give the watcher a stand-in for the
# process, so none is started; what it does to the database and to its list of jobs is real.
watch <- function(...) reading_service_watch(my_drafts(...), test_connection)

local_jobs <- function(env = parent.frame()) {
  jobs <- new.env()
  jobs$started <- list()
  jobs$killed <- list()
  jobs$running <- TRUE
  local_stub("reading_model_start", function(connection, id, reading_started, log_path) {
    jobs$started[[length(jobs$started) + 1]] <- list(id = id, reading_started = reading_started, log_path = log_path)
    list(number = length(jobs$started))
  }, env = env)
  local_stub("reading_model_pid", function(process) 4000L + process$number, env = env)
  local_stub("reading_model_is_running", function(process) jobs$running, env = env)
  local_stub("reading_model_kill", function(process) jobs$killed[[length(jobs$killed) + 1]] <- process$number, env = env)
  withr::defer(rm(list = ls(reading_jobs), envir = reading_jobs), envir = env)
  jobs
}

test_that("a draft waiting to be read gets a job, and the job's process id is kept with the draft", {
  me <- local_staff()
  local_pdf_cache()
  jobs <- local_jobs()
  record <- extraction_service_create(test_connection, fake_upload(), me)

  expect_true(watch(me))
  expect_length(jobs$started, 1)
  expect_equal(jobs$started[[1]]$id, record$id)
  expect_equal(jobs$started[[1]]$reading_started, record$reading_started)
  expect_equal(jobs$started[[1]]$log_path, file.path("logs", paste0(record$id, ".log")))
  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$reading_pid, 4001L)
  expect_false(is.null(saved$reading_claimed_at))
  expect_equal(saved$extraction_status, "queued") # the job itself says when it has started
  expect_false(is.null(reading_jobs[[record$id]]))
})

test_that("a draft whose job is running isn't started again", {
  me <- local_staff()
  local_pdf_cache()
  jobs <- local_jobs()
  extraction_service_create(test_connection, fake_upload(), me)
  expect_true(watch(me))
  expect_true(watch(me))
  expect_true(watch(me))
  expect_length(jobs$started, 1)
})

test_that("a job that has stopped running marks its draft failed", {
  me <- local_staff()
  local_pdf_cache()
  jobs <- local_jobs()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  watch(me)
  jobs$running <- FALSE
  watch(me)
  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$extraction_status, "failed")
  expect_equal(saved$error, "Reading stopped before it finished.")
})

test_that("the job of a draft that was discarded is stopped, and forgotten", {
  me <- local_staff()
  local_pdf_cache()
  jobs <- local_jobs()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  watch(me)
  extraction_service_discard(test_connection, record$id, me)
  expect_false(watch(me))
  expect_equal(jobs$killed, list(1L))
  expect_null(reading_jobs[[record$id]])
})

test_that("a draft whose reading was started over has its old job stopped and a new one started", {
  me <- local_staff()
  local_pdf_cache()
  jobs <- local_jobs()
  record <- extraction_service_create(test_connection, fake_upload(), me, now = Sys.time() - 60)
  watch(me)
  extraction_service_retry(test_connection, record$id, me)
  expect_true(watch(me))
  expect_equal(jobs$killed, list(1L))
  expect_length(jobs$started, 2)
  expect_false(identical(jobs$started[[1]]$reading_started, jobs$started[[2]]$reading_started))
  expect_equal(extraction_model_get(test_connection, record$id)$reading_pid, 4002L)
})

test_that("a draft whose reading has finished is no longer watched", {
  me <- local_staff()
  local_pdf_cache()
  jobs <- local_jobs()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  watch(me)
  extraction_service_reading_update(test_connection, record$id, record$reading_started, list(extraction_status = "ready"))
  expect_false(watch(me))
  expect_null(reading_jobs[[record$id]])
  expect_length(jobs$killed, 0) # it finished by itself
})

test_that("a job that can't be started is a failed reading with the reason, not a crash", {
  me <- local_staff()
  local_pdf_cache()
  jobs <- local_jobs()
  local_stub("reading_model_start", function(...) stop("couldn't find Rscript"))
  record <- extraction_service_create(test_connection, fake_upload(), me)
  expect_true(watch(me))
  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$extraction_status, "failed")
  expect_equal(saved$error, "couldn't find Rscript")
  expect_null(reading_jobs[[record$id]])
})
