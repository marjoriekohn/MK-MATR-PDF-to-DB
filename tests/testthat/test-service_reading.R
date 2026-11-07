# Gemini is never called here: stand-ins answer in its place. tests/golden.R
# checks real readings against the hand-checked GNA.


# A questionnaire as Gemini writes it: no display_order, every field on every block.
as_gemini_wrote_it <- function(json) {
  blocks <- function(blocks) {
    lapply(blocks, function(block) {
      list(
        component_type = block$component_type, text = block$text,
        printed_number = block$printed_number, self_contained = block$self_contained,
        condition = block$condition, parent_question = block$parent_question,
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
    self_contained = TRUE, condition = NULL, parent_question = NULL, variables = boxes, children = list()
  )
}

yes_no_box <- function() {
  choice <- function(answer) list(answer_option = answer, printed = TRUE, skip_instruction = NULL)
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
        list(answer_option = "Male", printed = TRUE, printed_label = "M", skip_instruction = NULL), # an old field: dropped
        list(answer_option = "FEMALE.", printed = TRUE, skip_instruction = NULL)
      )
    )))
  ))
  tidied <- reading_service_tidy(reading)

  expect_equal(tidied$questionnaire, list(name = "Twin Diet Survey")) # blank abbreviation left out
  instruction <- tidied$components[[1]]
  expect_equal(names(instruction), c("component_type", "display_order", "text"))
  expect_equal(instruction$text, "mark \"yes\" or \"no\"...") # and in lower case
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

test_that("a reading's words are kept in lower case, without a figure that repeats the number word before it", {
  asked <- question("4", boxes = list(list(
    variable_type = "open_ended", label = "Age Started", unit = "Yrs.", subject = "Respondent's Siblings",
    time_frame = "During the LAST 12 months", answer_data_type = "NUMBER", skip_condition = "None",
    skip_instruction = "Go to Question 9"
  )))
  asked$text <- "Do you have more than FIVE (5) brothers and sisters?"
  asked$condition <- "Twins"
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
  expect_equal(c(grid$text, grid$skip_condition, grid$skip_instruction), c("during the worst time:", "no", "go to question 15"))
  expect_equal(c(asked$text, asked$condition), c("do you have more than five brothers and sisters?", "twins"))
  expect_equal(c(box$label, box$skip_condition, box$skip_instruction), c("age started", "none", "go to question 9"))
  expect_equal(box$unit, "years") # its own field, abbreviation written out
  # names the database lists, numbers and the questionnaire's own name aren't the form's words
  expect_equal(box$subject, "Respondent's Siblings")
  expect_equal(box$time_frame, "in the past 12 months") # written one way (normal_time_frame())
  expect_equal(tidied$questionnaire, list(name = "General Nutrition Assessment", abbreviation = "GNA"))
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

test_that("a section, grid or group keeps who answers everything in it; other blocks have no condition", {
  heading <- section_of(list(component_type = "Instruction", text = "Answer these.", condition = "Female"), question("1"), text = "Women")
  heading$condition <- "Female"
  tidied <- reading_service_tidy(list(components = list(heading)))
  expect_equal(tidied$components[[1]]$condition, "female")
  expect_null(tidied$components[[1]]$children[[1]]$condition)
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

test_that("the prompt lists what the lookup tables hold", {
  instructions <- reading_service_instructions(test_lookups(), app_file("prompts", "extraction.md"))
  expect_false(grepl("{{", instructions, fixed = TRUE))
  expect_match(instructions, '"Interviewer Script"', fixed = TRUE)
  expect_match(instructions, '"Grid"', fixed = TRUE)
  expect_match(instructions, '"Respondent\'s Siblings"', fixed = TRUE)
  expect_match(instructions, '"in the past 30 days"', fixed = TRUE)
  expect_match(instructions, '"Twins (compared)"', fixed = TRUE)
  expect_match(instructions, "  - Not Difficult At All", fixed = TRUE)
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

# ------ MATR IDs and codes from a second file ---------------------------------------------
assign_prompt <- app_file("prompts", "assign_codes.md")

test_that("the file of MATR IDs and codes gives the variables their IDs and the choices their codes", {
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
      list(id = "v3", matr_id = "AH03"),
      list(id = "v99", matr_id = "nobody") # no such variable
    )), auto_unbox = TRUE)))
  }
  filled <- reading_service_assign(json, list(name = "codes.csv", text = "AH01,1,yes"), generate = generate, prompt_path = assign_prompt)
  expect_null(sent$pdf_path) # text goes in the prompt
  expect_match(sent$prompt, "## The file (codes.csv)", fixed = TRUE)
  expect_match(sent$prompt, '"id":"v1"', fixed = TRUE)

  # only MATR IDs and codes are added: the questionnaire is otherwise exactly as it was read
  strip <- function(x) {
    if (!is.list(x)) return(x)
    if (!is.null(names(x))) x <- x[setdiff(names(x), c("matr_id", "code_value"))]
    lapply(x, strip)
  }
  expect_identical(strip(filled), strip(json))

  draft <- draft_service_from_json(filled)
  expect_equal(draft$variables$matr_id[1:3], c(NA, NA, "AH03"))
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

test_that("a PDF of MATR IDs and codes is sent as the request's file, and codes that repeat are left out", {
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

# ------ The job ------------------------------------------------------------------------
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
  reading_service_run(test_connection, record$id, record$reading_started, read = read)

  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(seen, "sent")
  expect_equal(saved$extraction_status, "ready")
  expect_equal(saved$prompt_version, "abc123")
  expect_equal(saved$model, "gemini-3.8-flash")
  expect_identical(draft_service_from_json(draft_model_get_both(test_connection, record$id)$original), short_gold_draft())
  expect_equal(nrow(extraction_service_load_draft(test_connection, saved)$components), nrow(short_gold_draft()$components))
})

test_that("the job reads the file of MATR IDs and codes once the questionnaire is read, and carries on without it if it fails", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me, supplement = fake_csv_upload(c("AH01", "AH02"), "codes.csv"))
  expect_equal(record$supplement$name, "codes.csv")
  read <- function(pdf_path, lookups, progress) list(json = short_gold_json(), model = "m", prompt_version = "p")
  given <- NULL
  assign <- function(json, supplement) {
    given <<- supplement
    json$components[[1]]$variables[[1]]$matr_id <- "AH01"
    json
  }
  reading_service_run(test_connection, record$id, record$reading_started, read = read, assign = assign)
  expect_equal(given, list(name = "codes.csv", text = "AH01\nAH02"))
  original <- draft_service_from_json(draft_model_get_both(test_connection, record$id)$original)
  expect_equal(original$variables$matr_id[1], "AH01")

  again <- extraction_service_create(test_connection, fake_upload(), me, supplement = fake_csv_upload("AH01", "codes.csv"))
  messages <- capture_messages(reading_service_run(
    test_connection, again$id, again$reading_started, read = read, assign = function(json, supplement) stop("Gemini is busy")
  ))
  expect_match(messages, "couldn't be applied: Gemini is busy", all = FALSE)
  read_anyway <- extraction_model_get(test_connection, again$id)
  expect_equal(read_anyway$extraction_status, "ready")
  expect_equal(read_anyway$error, "Gemini is busy") # shown on the step, which says to type the IDs on Edit Extraction
})

test_that("a file of MATR IDs and codes that isn't a PDF, CSV or text is turned away", {
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
  subject <- local_row("subject", list(name = test_name("Subject")), "subject_id")
  reading_service_run(test_connection, record$id, record$reading_started, read = read)
  expect_true(subject$name %in% given$subjects)
  expect_true("Section" %in% given$component_types)
})

test_that("a reading that fails says why", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
  read <- function(...) stop("Gemini is busy right now. Try reading again in a few minutes.")
  reading_service_run(test_connection, record$id, record$reading_started, read = read)

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
  reading_service_run(test_connection, record$id, record$reading_started, read = read)
  expect_equal(extraction_model_get(test_connection, record$id)$extraction_status, "ready")
  expect_identical(readBin(read_from, "raw", 1e4), readBin(upload$datapath, "raw", 1e4))
})

test_that("a PDF that is no longer anywhere fails the reading with a way forward", {
  me <- local_staff()
  local_pdf_cache()
  record <- extraction_service_create(test_connection, fake_upload(), me)
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
  reading_service_run(test_connection, record$id, record$reading_started, read = read)

  saved <- extraction_model_get(test_connection, record$id)
  expect_equal(saved$extraction_status, "queued")
  expect_null(draft_model_get_both(test_connection, record$id)$original)
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
  withdrawn <- extraction_service_create(test_connection, fake_upload(), colleague, now = Sys.time() - 60)
  extraction_service_discard(test_connection, withdrawn$id, colleague)

  still_reading <- reading_service_watch(my_drafts(me, colleague), test_connection)
  expect_true(still_reading)
  saved <- extraction_service_get_all(test_connection)
  expect_equal(saved[[record$id]]$extraction_status, "failed")
  expect_equal(saved[[record$id]]$error, "Reading stopped before it finished.")
  expect_error(extraction_model_get(test_connection, withdrawn$id), "no longer in the database") # discarded drafts are gone
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
