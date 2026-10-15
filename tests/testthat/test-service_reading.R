# Gemini is never called here: stand-ins answer in its place. tests/golden.R
# checks real readings against the hand-checked GNA.


# A questionnaire as Gemini writes it: no display_order, every field on every block.
as_gemini_wrote_it <- function(json) {
  blocks <- function(blocks) {
    lapply(blocks, function(block) {
      list(
        component_type = block$component_type, text = block$text,
        question_number = block$question_number, self_contained = block$self_contained,
        condition = block$condition, parent_question = block$parent_question,
        variables = lapply(block$variables, function(box) {
          list(
            variable_type = box$variable_type, label = box$label, subject = box$subject,
            answer_data_type = box$answer_data_type,
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
    component_type = "Question", text = paste("Question", number), question_number = number,
    self_contained = TRUE, condition = NULL, parent_question = NULL, variables = boxes, children = list()
  )
}

yes_no_box <- function() {
  choice <- function(answer) list(answer_option = answer, printed = TRUE, printed_label = NULL, skip_instruction = NULL)
  list(
    variable_type = "closed_ended", label = NULL, subject = "Self", answer_data_type = NULL,
    response_options = list(choice("Yes"), choice("No"))
  )
}

answer_with <- function(...) {
  list(text = as.character(jsonlite::toJSON(
    list(questionnaire = list(name = NULL, abbreviation = NULL), components = list(...)),
    auto_unbox = TRUE, null = "null"
  )), model = "gemini-test", input_tokens = 100L, output_tokens = 50L, thinking_tokens = 20L)
}

section_of <- function(..., continued = NULL) {
  list(component_type = "Section", continued = continued, text = NULL, children = list(...))
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
      component_type = "Instruction", text = "Mark “Yes” or “No”…", question_number = NULL,
      variables = list(), children = list()
    ),
    question("12 b.", boxes = list(list(
      variable_type = "closed_ended", label = NULL, subject = "Self", answer_data_type = NULL,
      response_options = list(
        list(answer_option = "Male", printed = TRUE, printed_label = "M", skip_instruction = NULL),
        list(answer_option = "Female", printed = TRUE, printed_label = "Female", skip_instruction = NULL)
      )
    )))
  ))
  tidied <- reading_service_tidy(reading)

  expect_equal(tidied$questionnaire, list(name = "Twin Diet Survey")) # blank abbreviation left out
  instruction <- tidied$components[[1]]
  expect_equal(names(instruction), c("component_type", "display_order", "text"))
  expect_equal(instruction$text, "Mark \"Yes\" or \"No\"...")
  asked <- tidied$components[[2]]
  expect_equal(asked$display_order, 2)
  expect_equal(asked$question_number, "12b")
  choices <- asked$variables[[1]]$response_options
  expect_equal(vapply(choices, function(choice) choice$display_order, integer(1)), 1:2)
  expect_equal(choices[[1]]$printed_label, "M")
  expect_null(choices[[2]]$printed_label) # printed as the standard wording
})

test_that("a block put inside one that isn't a section moves out to follow it", {
  misplaced <- question("1")
  misplaced$children <- list(question("2"))
  tidied <- reading_service_tidy(list(components = list(misplaced, question("3"))))
  numbers <- vapply(tidied$components, function(block) block$question_number, "")
  expect_equal(numbers, c("1", "2", "3"))
  expect_null(tidied$components[[1]]$children)
})

test_that("the prompt lists what the lookup tables hold", {
  instructions <- reading_service_instructions(test_lookups(), app_file("prompts", "extraction.md"))
  expect_false(grepl("{{", instructions, fixed = TRUE))
  expect_match(instructions, '"Interviewer Script"', fixed = TRUE)
  expect_match(instructions, '"Respondent\'s Siblings"', fixed = TRUE)
  expect_match(instructions, "  - Not Difficult At All", fixed = TRUE)
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
  heading <- section_of()
  heading$text <- "Part A. Your Sleep"
  problems <- reading_service_problems(reading_service_tidy(list(components = list(heading, question("1")))), test_lookups())
  expect_equal(problems, 'Section "Part A. Your Sleep": has nothing in it. Put the blocks printed under it inside it, as its children.')
})

test_that("a reading with no questions is a problem", {
  empty <- list(component_type = "Section", text = NULL, variables = list(), children = list())
  problems <- reading_service_problems(reading_service_tidy(list(components = list(empty))), test_lookups())
  expect_equal(problems[1], "No questions were recorded. Record every page of the questionnaire.")
  expect_match(problems[2], "^A section with no heading: has nothing in it.")
})

test_that("a questionnaire is read two pages at a time, and the pieces joined", {
  pages_1_2 <- answer_with(question("1"), section_of(question("2a")))
  page_3 <- answer_with(section_of(question("2b"), continued = TRUE), question("3"))
  gemini <- stand_in_gemini(list(pages_1_2, page_3))
  reading <- read_with(gemini, pages = 3)

  expect_equal(gemini$asked(), "This PDF has 3 pages. Record pages 1-2.\nThis PDF has 3 pages. Record page 3.")
  blocks <- reading$json$components
  expect_equal(vapply(blocks, function(block) block$component_type, ""), c("Question", "Section", "Question"))
  expect_equal(vapply(blocks[[2]]$children, function(block) block$question_number, ""), c("2a", "2b"))
  expect_null(blocks[[2]]$continued)
  expect_equal(reading$tokens$output, 100)
  expect_equal(reading$tokens$most_in_one_answer, 70)
})

test_that("a section carried on inside a carried-on section joins its first part", {
  earlier <- list(section_of(question("1"), section_of(question("2a"))))
  later <- list(section_of(section_of(question("2b"), continued = TRUE), question("3"), continued = TRUE))
  joined <- join_page_ranges(list(list(components = earlier), list(components = later)))$components
  outer <- joined[[1]]$children
  expect_length(joined, 1)
  expect_equal(vapply(outer, function(block) block$component_type, ""), c("Question", "Section", "Question"))
  expect_equal(vapply(outer[[2]]$children, function(block) block$question_number, ""), c("2a", "2b"))
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

# ------ The job ------------------------------------------------------------------------
# The watcher is given the drafts of the staff the test made only: the database holds other people's too, and it would
# start (and stop) jobs for them.
# my_drafts <- function(...) {
#   who <- vapply(list(...), function(staff) staff$staff_id, numeric(1))
#   Filter(function(record) record$prepared_by_id %in% who, extraction_service_get_all(test_connection))
# }

# test_that("the job saves what Gemini read and marks the draft ready", {
#   me <- local_staff()
#   local_pdf_cache()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   seen <- character()
#   read <- function(pdf_path, lookups, progress) {
#     progress("sent")
#     seen <<- extraction_model_get(test_connection, record$id)$extraction_status
#     progress("received")
#     list(json = gold_json(), model = "gemini-3.8-flash", prompt_version = "abc123")
#   }
#   reading_service_run(test_connection, record$id, record$reading_started, read = read)
#
#   saved <- extraction_model_get(test_connection, record$id)
#   expect_equal(seen, "sent")
#   expect_equal(saved$extraction_status, "ready")
#   expect_equal(saved$prompt_version, "abc123")
#   expect_equal(saved$model, "gemini-3.8-flash")
#   expect_identical(draft_service_from_json(draft_model_get_record(test_connection, record$id)$original), gold_draft())
#   expect_equal(nrow(extraction_service_load_draft(test_connection, saved)$components), nrow(gold_draft()$components))
# })

# test_that("the job reads the pick lists from the database for Gemini", {
#   me <- local_staff()
#   local_pdf_cache()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   given <- NULL
#   read <- function(pdf_path, lookups, progress) {
#     given <<- lookups
#     list(json = gold_json(), model = "m", prompt_version = "p")
#   }
#   subject <- local_row("subject", list(name = test_name("Subject")), "subject_id")
#   reading_service_run(test_connection, record$id, record$reading_started, read = read)
#   expect_true(subject$name %in% given$subjects)
#   expect_true("Section" %in% given$component_types)
# })

# test_that("a reading that fails says why", {
#   me <- local_staff()
#   local_pdf_cache()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   read <- function(...) stop("Gemini is busy right now. Try reading again in a few minutes.")
#   reading_service_run(test_connection, record$id, record$reading_started, read = read)
#
#   saved <- extraction_model_get(test_connection, record$id)
#   expect_equal(saved$extraction_status, "failed")
#   expect_equal(saved$error, "Gemini is busy right now. Try reading again in a few minutes.")
# })

# test_that("a PDF that is no longer on this site fails the reading with a way forward", {
#   me <- local_staff()
#   local_pdf_cache()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   unlink(record$pdf$path)
#   reading_service_run(test_connection, record$id, record$reading_started, read = function(...) stop("not reached"))
#
#   saved <- extraction_model_get(test_connection, record$id)
#   expect_equal(saved$extraction_status, "failed")
#   expect_match(saved$error, "isn't on this site any more. Choose Replace PDF")
# })

# test_that("a job whose reading was restarted saves nothing", {
#   me <- local_staff()
#   local_pdf_cache()
#   record <- extraction_service_create(test_connection, fake_upload(), me, now = Sys.time() - 60)
#   read <- function(pdf_path, lookups, progress) {
#     extraction_service_retry(test_connection, record$id, me) # staff press Try reading again meanwhile
#     list(json = gold_json(), model = "gemini-3.8-flash", prompt_version = "abc123")
#   }
#   reading_service_run(test_connection, record$id, record$reading_started, read = read)
#
#   saved <- extraction_model_get(test_connection, record$id)
#   expect_equal(saved$extraction_status, "queued")
#   expect_null(draft_model_get_record(test_connection, record$id)$original)
# })
#
# test_that("a reading whose job has died is marked failed", {
#   me <- local_staff()
#   colleague <- local_staff()
#   local_pdf_cache()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   extraction_service_reading_update(test_connection, record$id, record$reading_started, list(
#     extraction_status = "sent", reading_pid = 999999L, # no such process
#     reading_claimed_at = format_time(Sys.time())
#   ))
#   withdrawn <- extraction_service_create(test_connection, fake_upload(), colleague, now = Sys.time() - 60)
#   extraction_service_withdraw(test_connection, withdrawn$id, me)
#
#   still_reading <- reading_service_watch(my_drafts(me, colleague), test_connection)
#   expect_true(still_reading)
#   saved <- extraction_service_get_all(test_connection)
#   expect_equal(saved[[record$id]]$extraction_status, "failed")
#   expect_equal(saved[[record$id]]$error, "Reading stopped before it finished.")
#   expect_equal(saved[[withdrawn$id]]$extraction_status, "queued") # discarded drafts are left alone
# })
#
# test_that("a draft another copy of the site has claimed isn't started again", {
#   me <- local_staff()
#   colleague <- local_staff()
#   withr::local_envvar(GEMINI_KEY = "") # a job started by mistake couldn't call Gemini
#   local_pdf_cache()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   extraction_model_claim_reading(test_connection, record$id, record$reading_started)
#   extraction_service_reading_update(test_connection, record$id, record$reading_started, list(reading_pid = Sys.getpid()))
#
#   expect_true(reading_service_watch(my_drafts(me, colleague), test_connection))
#   expect_null(reading_jobs[[record$id]])
#   expect_equal(extraction_model_get(test_connection, record$id)$extraction_status, "queued")
# })

test_that("a claimed draft whose job hasn't recorded its process yet gets a short grace", {
  claimed <- "2026-10-02T10:00:00Z"
  record <- list(reading_pid = NULL, reading_claimed_at = claimed)
  expect_false(job_is_lost(record, now = as.POSIXct("2026-10-02 10:00:30", tz = "UTC")))
  expect_true(job_is_lost(record, now = as.POSIXct("2026-10-02 10:05:00", tz = "UTC")))
  expect_true(job_is_lost(list(reading_pid = 999999L, reading_claimed_at = claimed)))
  expect_false(job_is_lost(list(reading_pid = Sys.getpid(), reading_claimed_at = claimed)))
})

# test_that("a draft that loses the race to claim it isn't started by this site", {
#   me <- local_staff()
#   withr::local_envvar(GEMINI_KEY = "")
#   local_pdf_cache()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   # Another site claims it between this site reading the list and starting a job.
#   stale <- my_drafts(me)
#   extraction_model_claim_reading(test_connection, record$id, record$reading_started)
#
#   expect_true(reading_service_watch(stale, test_connection))
#   expect_null(reading_jobs[[record$id]])
# })

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
# watch <- function(...) reading_service_watch(my_drafts(...), test_connection)
#
# local_jobs <- function(env = parent.frame()) {
#   jobs <- new.env()
#   jobs$started <- list()
#   jobs$killed <- list()
#   jobs$running <- TRUE
#   local_stub("reading_model_start", function(connection, id, reading_started, log_path) {
#     jobs$started[[length(jobs$started) + 1]] <- list(id = id, reading_started = reading_started, log_path = log_path)
#     list(number = length(jobs$started))
#   }, env = env)
#   local_stub("reading_model_pid", function(process) 4000L + process$number, env = env)
#   local_stub("reading_model_is_running", function(process) jobs$running, env = env)
#   local_stub("reading_model_kill", function(process) jobs$killed[[length(jobs$killed) + 1]] <- process$number, env = env)
#   withr::defer(rm(list = ls(reading_jobs), envir = reading_jobs), envir = env)
#   jobs
# }
#
# test_that("a draft waiting to be read gets a job, and the job's process id is kept with the draft", {
#   me <- local_staff()
#   local_pdf_cache()
#   jobs <- local_jobs()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#
#   expect_true(watch(me))
#   expect_length(jobs$started, 1)
#   expect_equal(jobs$started[[1]]$id, record$id)
#   expect_equal(jobs$started[[1]]$reading_started, record$reading_started)
#   expect_equal(jobs$started[[1]]$log_path, file.path("logs", paste0(record$id, ".log")))
#   saved <- extraction_model_get(test_connection, record$id)
#   expect_equal(saved$reading_pid, 4001L)
#   expect_false(is.null(saved$reading_claimed_at))
#   expect_equal(saved$extraction_status, "queued") # the job itself says when it has started
#   expect_false(is.null(reading_jobs[[record$id]]))
# })
#
# test_that("a draft whose job is running isn't started again", {
#   me <- local_staff()
#   local_pdf_cache()
#   jobs <- local_jobs()
#   extraction_service_create(test_connection, fake_upload(), me)
#   expect_true(watch(me))
#   expect_true(watch(me))
#   expect_true(watch(me))
#   expect_length(jobs$started, 1)
# })
#
# test_that("a job that has stopped running marks its draft failed", {
#   me <- local_staff()
#   local_pdf_cache()
#   jobs <- local_jobs()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   watch(me)
#   jobs$running <- FALSE
#   watch(me)
#   saved <- extraction_model_get(test_connection, record$id)
#   expect_equal(saved$extraction_status, "failed")
#   expect_equal(saved$error, "Reading stopped before it finished.")
# })
#
# test_that("the job of a draft that was discarded is stopped, and forgotten", {
#   me <- local_staff()
#   local_pdf_cache()
#   jobs <- local_jobs()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   watch(me)
#   extraction_service_withdraw(test_connection, record$id, me)
#   expect_false(watch(me))
#   expect_equal(jobs$killed, list(1L))
#   expect_null(reading_jobs[[record$id]])
# })
#
# test_that("a draft whose reading was started over has its old job stopped and a new one started", {
#   me <- local_staff()
#   local_pdf_cache()
#   jobs <- local_jobs()
#   record <- extraction_service_create(test_connection, fake_upload(), me, now = Sys.time() - 60)
#   watch(me)
#   extraction_service_retry(test_connection, record$id, me)
#   expect_true(watch(me))
#   expect_equal(jobs$killed, list(1L))
#   expect_length(jobs$started, 2)
#   expect_false(identical(jobs$started[[1]]$reading_started, jobs$started[[2]]$reading_started))
#   expect_equal(extraction_model_get(test_connection, record$id)$reading_pid, 4002L)
# })
#
# test_that("a draft whose reading has finished is no longer watched", {
#   me <- local_staff()
#   local_pdf_cache()
#   jobs <- local_jobs()
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   watch(me)
#   extraction_service_reading_update(test_connection, record$id, record$reading_started, list(extraction_status = "ready"))
#   expect_false(watch(me))
#   expect_null(reading_jobs[[record$id]])
#   expect_length(jobs$killed, 0) # it finished by itself
# })
#
# test_that("a job that can't be started is a failed reading with the reason, not a crash", {
#   me <- local_staff()
#   local_pdf_cache()
#   jobs <- local_jobs()
#   local_stub("reading_model_start", function(...) stop("couldn't find Rscript"))
#   record <- extraction_service_create(test_connection, fake_upload(), me)
#   expect_true(watch(me))
#   saved <- extraction_model_get(test_connection, record$id)
#   expect_equal(saved$extraction_status, "failed")
#   expect_equal(saved$error, "couldn't find Rscript")
#   expect_null(reading_jobs[[record$id]])
# })
