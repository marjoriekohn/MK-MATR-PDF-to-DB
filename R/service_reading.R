# ------ Reading a questionnaire PDF ------------------------------------------------
# Gemini reads the PDF with MATR's extraction prompt (prompts/extraction.md),
# answering with JSON in the shape the prompt describes: the questionnaire's
# blocks, nested as printed. Gemini can only write so much in one answer, so it
# reads two pages per request, the requests sent at once, and their answers are
# joined. reading_service_tidy() turns the whole into the JSON the Edit Extraction page
# loads (tests/testthat/fixtures/gna_gold.json is a hand-checked example), and the draft checks
# look it over. If they find problems, Gemini reads it once more, told what
# they were, and the reading with fewer problems is kept.

reading_gemini_model <- "gemini-3.8-flash"
reading_prompt_path <- "prompts/extraction.md"
reading_pages_at_once <- 2

#' Read a questionnaire PDF into the JSON the Edit Extraction page loads.
#'
#' Gemini reads a few pages at a time, the requests sent at once, and the answers are
#' joined and tidied. The draft checks look the result over; if they find problems, Gemini
#' reads it once more, told what they were, and the reading with fewer problems is kept.
#'
#' @param pdf_path The PDF.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param progress A function told `"sent"` when the PDF goes to Gemini and `"received"`
#'   when its answers are back.
#' @param generate `gemini_model_generate()` or a stand-in.
#' @param model The Gemini model to use.
#' @param pages The PDF's page count; counted from the PDF if not given.
#' @return A list: `json` (the questionnaire), `problems` the checks still find, `model`,
#'   `prompt_version` and `tokens` used.
reading_service_read <- function(pdf_path, lookups, progress = function(status) NULL,
                                 generate = gemini_model_generate, model = reading_gemini_model,
                                 pages = NULL) {
  instructions <- reading_service_instructions(lookups)
  requests <- page_requests(pages %||% count_pages(pdf_path, generate, model))
  feedback <- ""
  best <- NULL
  failure <- NULL

  for (attempt in 1:2) {
    progress("sent")
    # If Gemini fails the second time, the first reading still stands.
    answers <- tryCatch(
      generate(pdf_path, paste0(instructions, "\n\n## Your pages\n\n", requests, feedback), model = model),
      error = function(problem) if (is.null(best)) stop(problem) else NULL
    )
    if (is.null(answers)) break
    progress("received")
    reading <- tryCatch(
      {
        parts <- lapply(answers, function(answer) json_in(answer$text))
        json <- reading_service_tidy(join_page_ranges(parts))
        list(
          json = json, problems = reading_service_problems(json, lookups), model = answers[[1]]$model,
          tokens = tokens_used(answers)
        )
      },
      error = function(problem) {
        failure <<- conditionMessage(problem)
        NULL
      }
    )
    if (!is.null(reading) && (is.null(best) || length(reading$problems) < length(best$problems))) {
      best <- reading
    }
    if (!is.null(best) && length(best$problems) == 0) break
    feedback <- paste(
      "",
      "An earlier reading of this questionnaire had these problems. Fix any on your pages:",
      if (is.null(reading)) "- The answer wasn't complete JSON in the shape described." else paste("-", reading$problems, collapse = "\n"),
      sep = "\n"
    )
  }
  if (is.null(best)) {
    stop("Gemini's answer couldn't be read as a questionnaire (", failure, ").", call. = FALSE)
  }
  best$prompt_version <- reading_service_prompt_version(instructions)
  best
}

# The JSON object in Gemini's answer, which may come wrapped in a ```json fence.
json_in <- function(text) {
  start <- regexpr("{", text, fixed = TRUE)
  ends <- gregexpr("}", text, fixed = TRUE)[[1]]
  if (start < 0 || max(ends) < start) stop("There's no JSON object in the answer.", call. = FALSE)
  jsonlite::fromJSON(substr(text, start, max(ends)), simplifyVector = FALSE)
}

# "This PDF has 5 pages. Record pages 3-4." for each pair of pages.
page_requests <- function(pages) {
  first <- seq(1, pages, by = reading_pages_at_once)
  last <- pmin(first + reading_pages_at_once - 1, pages)
  sprintf(
    "This PDF has %s. Record %s.",
    plural(pages, "page"),
    ifelse(first == last, paste("page", first), paste0("pages ", first, "-", last))
  )
}

# The PDF says how many pages it has, unless it keeps its list of pages
# compressed, in which case Gemini counts them.
count_pages <- function(pdf_path, generate, model) {
  pages <- extraction_model_page_count(pdf_path)
  if (!is.null(pages)) {
    return(pages)
  }
  answer <- generate(pdf_path, "How many pages does this PDF have? Answer with the number only.", model = model)[[1]]
  pages <- suppressWarnings(as.integer(regmatches(answer$text, regexpr("[0-9]+", answer$text))))
  if (length(pages) == 0 || is.na(pages) || pages < 1) stop("Gemini couldn't count the PDF's pages.", call. = FALSE)
  pages
}

# One reading from the readings of each page range. A range that starts by
# carrying on a section or question group from the pages before it (`continued`) adds its blocks
# to it, and to a section or group inside it that carries on too.
join_page_ranges <- function(parts) {
  components <- list()
  for (part in parts) components <- carry_on(components, part$components)
  given <- function(field) {
    Find(Negate(is.null), lapply(parts, function(part) part$questionnaire[[field]]))
  }
  list(questionnaire = list(name = given("name"), abbreviation = given("abbreviation")), components = components)
}

carry_on <- function(blocks, more) {
  last <- length(blocks)
  continues_section <- length(more) > 0 && isTRUE(more[[1]]$continued) && last > 0 &&
    is_container(blocks[[last]]$component_type)
  if (continues_section) {
    blocks[[last]]$children <- carry_on(blocks[[last]]$children, more[[1]]$children)
    more <- more[-1]
  }
  c(blocks, more)
}

tokens_used <- function(answers) {
  each <- function(field) vapply(answers, function(answer) as.numeric(answer[[field]] %||% 0), numeric(1))
  list(
    input = sum(each("input_tokens")), output = sum(each("output_tokens")), thinking = sum(each("thinking_tokens")),
    # Gemini stops at 65,536 for a single answer, thinking included.
    most_in_one_answer = max(each("output_tokens") + each("thinking_tokens"))
  )
}

# The prompt, with the lookup lists filled in, so a type or subject staff add
# reaches Gemini without editing the prompt.
reading_service_instructions <- function(lookups, prompt_path = reading_prompt_path) {
  prompt <- reading_model_prompt(prompt_path)
  quoted <- function(values) paste0('"', values, '"', collapse = ", ")
  lists <- list(
    component_types = quoted(lookups$component_types),
    subjects = quoted(lookups$subjects),
    answer_data_types = quoted(lookups$answer_data_types),
    answer_options = paste0("  - ", lookups$answer_options, collapse = "\n")
  )
  for (name in names(lists)) {
    prompt <- gsub(paste0("{{", name, "}}"), lists[[name]], prompt, fixed = TRUE)
  }
  prompt
}

# Changes whenever the prompt or a lookup list does, so each draft says which
# prompt read it.
reading_service_prompt_version <- function(instructions) {
  substr(digest::digest(instructions, algo = "sha256", serialize = FALSE), 1, 12)
}

# ------ Into the Edit Extraction page's shape ----------------------------------------------------
# Counts display_order 1, 2, 3... down each list, keeps only the fields each kind
# of block has (in tests/testthat/fixtures/gna_gold.json's order, leaving out empty ones), and
# tidies the text: one space between words, straight quotes. A block put inside
# one that isn't a section or question group moves out to sit straight after it. A variable about the
# respondent has no subject.
reading_service_tidy <- function(reading) {
  list(
    questionnaire = without_blanks(list(
      name = clean_text(reading$questionnaire$name),
      abbreviation = clean_text(reading$questionnaire$abbreviation)
    )),
    components = tidy_blocks(reading$components)
  )
}

tidy_blocks <- function(blocks) {
  blocks <- lift_misplaced_children(blocks)
  lapply(seq_along(blocks), function(i) {
    block <- blocks[[i]]
    type <- block$component_type
    if (is_container(type)) {
      return(without_blanks(list(
        component_type = type, display_order = i, text = clean_text(block$text),
        children = tidy_blocks(block$children)
      )))
    }
    if (type != "Question") {
      return(without_blanks(list(component_type = type, display_order = i, text = clean_text(block$text))))
    }
    without_blanks(list(
      component_type = type,
      display_order = i,
      text = clean_text(block$text),
      printed_number = clean_number(block$printed_number),
      self_contained = block$self_contained %||% TRUE,
      condition = clean_text(block$condition),
      parent_question = clean_number(block$parent_question),
      variables = tidy_boxes(block$variables)
    ))
  })
}

lift_misplaced_children <- function(blocks) {
  unlist(lapply(blocks, function(block) {
    if (is_container(block$component_type) || length(block$children) == 0) {
      return(list(block))
    }
    c(list(block), lift_misplaced_children(block$children))
  }), recursive = FALSE)
}

tidy_boxes <- function(boxes) {
  lapply(seq_along(boxes), function(i) {
    box <- boxes[[i]]
    if (box$variable_type == "open_ended") {
      return(without_blanks(list(
        variable_type = box$variable_type, display_order = i, label = clean_text(box$label),
        subject = blank_respondent(clean_text(box$subject) %||% NA), answer_data_type = box$answer_data_type
      )))
    }
    without_blanks(list(
      variable_type = box$variable_type, display_order = i, label = clean_text(box$label),
      subject = blank_respondent(clean_text(box$subject) %||% NA), response_options = tidy_choices(box$response_options)
    ))
  })
}

# A choice that isn't printed on the form (an older answer's "No" for an empty check box) can't be marked,
# so it isn't a response option, and is left out.
tidy_choices <- function(choices) {
  choices <- Filter(function(choice) !isFALSE(choice[["printed"]]), choices)
  lapply(seq_along(choices), function(i) {
    choice <- choices[[i]]
    answer_option <- clean_text(choice$answer_option)
    printed_label <- clean_text(choice$printed_label)
    without_blanks(list(
      display_order = i,
      answer_option = answer_option,
      printed_label = if (!identical(printed_label, answer_option)) printed_label,
      skip_instruction = clean_text(choice$skip_instruction)
    ))
  })
}

# NULL for nothing at all.
clean_text <- function(text) {
  if (is.null(text)) {
    return(NULL)
  }
  text <- gsub("[‘’]", "'", text)
  text <- gsub("[“”]", '"', text)
  text <- gsub("…", "...", text)
  text <- trimws(gsub("[[:space:]]+", " ", text))
  if (nzchar(text)) text
}

# "12 b." is 12b.
clean_number <- function(number) {
  number <- clean_text(number)
  if (is.null(number)) {
    return(NULL)
  }
  number <- sub("[.)]+$", "", gsub(" ", "", number))
  if (nzchar(number)) number
}

# ------ Checking a reading ------------------------------------------------------------
# What the Edit Extraction page's checks would flag, worded for Gemini: each names its
# block ("Question 4: Add at least one variable.").
reading_service_problems <- function(json, lookups) {
  draft <- draft_service_from_json(json)
  blocks <- draft$components
  problems <- draft_service_check(draft, lookups, matr_ids = FALSE) # Gemini never reads MATR IDs
  where <- match(problems$id, blocks$id)
  # A parent named by a number no question has is dropped on loading, so the
  # checks never see it.
  parent_numbers <- pluck(flatten_blocks(json$components), "parent_question")
  lost <- which(!is.na(parent_numbers) & is.na(blocks$parent_question_id))
  empty <- is_container(blocks$component_type) & !blocks$id %in% blocks$parent_id
  c(
    if (!"Question" %in% blocks$component_type) "No questions were recorded. Record every page of the questionnaire.",
    sprintf(
      "%s: has nothing in it. Put the blocks printed under it inside it, as its children.",
      block_name(blocks[empty, ])
    ),
    sprintf("%s: %s", block_name(blocks[where, ]), problems$message),
    sprintf(
      "%s: names question %s as its parent, but no question has that number.",
      block_name(blocks[lost, ]), parent_numbers[lost]
    )
  )
}

# "Question 14a", "Instruction "During the worst time:"", "A question group" or "A section with no heading".
block_name <- function(blocks) {
  numbers <- draft_service_question_number(blocks)
  ifelse(
    !is.na(numbers),
    paste(blocks$component_type, numbers),
    ifelse(
      is.na(blocks$text),
      ifelse(blocks$component_type == "Question Group", "A question group", paste("A", tolower(blocks$component_type), "with no heading")),
      sprintf('%s "%s"', blocks$component_type, substr(blocks$text, 1, 60))
    )
  )
}

# ------ The reading job ---------------------------------------------------------------
# Reading takes a minute or more, so each draft is read by a background R
# process (callr) while staff carry on. The job moves the draft's
# extraction_status along
#   queued -> sent -> received -> ready      or stops at failed
# and saves the questionnaire to the draft. A new PDF or "Try reading again"
# sets it back to queued, which starts a new job. Jobs outlive the session that
# started them, and the site too.

reading_jobs <- new.env() # the jobs this site started: draft id -> list(process, reading_started)

# A job that has claimed a draft but not yet recorded its process id is given
# this long before it counts as lost.
reading_claim_grace_seconds <- 120

reading_log_dir <- "logs"

#' Start, stop and check on the reading jobs.
#'
#' Starts a job for each draft waiting to be read, stops jobs whose reading was restarted
#' or discarded, and marks failed any reading whose job has died.
#'
#' @param records The extraction records, from `extraction_service_get_all()`.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return `TRUE` while any draft is being read, so the caller looks again soon (a job
#'   that dies changes nothing in the database).
reading_service_watch <- function(records, connection) {
  open <- extraction_service_open_drafts(records)
  for (id in setdiff(ls(reading_jobs), names(open))) stop_job(id)
  reading <- FALSE
  for (record in open) {
    job <- reading_jobs[[record$id]]
    if (!is.null(job) && !identical(job$reading_started, record$reading_started)) {
      stop_job(record$id)
      job <- NULL
    }
    # A draft typed by hand has no reading to watch.
    if (!isTRUE(record$extraction_status %in% c("queued", "sent", "received"))) {
      if (!is.null(job)) rm(list = record$id, envir = reading_jobs)
      next
    }
    reading <- TRUE
    stopped <- if (!is.null(job)) {
      if (!reading_model_is_running(job$process)) "Reading stopped before it finished."
    } else if (is.null(record$reading_claimed_at)) {
      start_job(record, connection)
    } else if (job_is_lost(record)) {
      "Reading stopped before it finished."
    }
    if (!is.null(stopped)) {
      extraction_service_reading_update(connection, record$id, record$reading_started, list(
        extraction_status = "failed", error = stopped
      ))
    }
  }
  reading
}

#' What a reading job does, in its own R process.
#'
#' Reads the draft's PDF, saves the questionnaire it got, and moves the draft's
#' `extraction_status` along as it goes; a failure is saved as the reason. Nothing is
#' saved if the reading was restarted meanwhile.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param reading_started The reading's start time the job was started for.
#' @param read `reading_service_read()` or a stand-in.
#' @return `TRUE` invisibly, or `FALSE` if the reading had already restarted.
reading_service_run <- function(connection, id, reading_started, read = reading_service_read) {
  update <- function(...) extraction_service_reading_update(connection, id, reading_started, list(...))
  record <- extraction_model_get(connection, id)
  if (!identical(record$reading_started, reading_started)) {
    return(invisible(FALSE))
  }
  tryCatch(
    {
      pdf <- extraction_service_pdf(connection, record)
      if (is.null(pdf)) {
        stop("The PDF isn't on this site any more. Choose Replace PDF and upload it again.", call. = FALSE)
      }
      reading <- read(
        pdf$path, lookup_service_get_all(connection),
        progress = function(status) update(extraction_status = status)
      )
      if (draft_model_save_reading(connection, id, reading$json, reading_started)) {
        update(
          extraction_status = "ready", error = NULL, model = reading$model,
          prompt_version = reading$prompt_version
        )
      }
    },
    error = function(problem) update(extraction_status = "failed", error = conditionMessage(problem))
  )
  invisible(TRUE)
}

# Claims the draft in the database, which only one site can do, then starts its
# job and saves the job's process id on the draft, which tells any other copy of
# the site (and this one, after a restart) that the draft is taken, and how to
# see whether the job is still alive. Returns NULL, or why the job couldn't
# start.
start_job <- function(record, connection) {
  if (!extraction_model_claim_reading(connection, record$id, record$reading_started)) {
    return(NULL)
  }
  tryCatch(
    {
      process <- reading_model_start(
        connection, record$id, record$reading_started,
        log_path = file.path(reading_log_dir, paste0(record$id, ".log"))
      )
      reading_jobs[[record$id]] <- list(process = process, reading_started = record$reading_started)
      extraction_service_reading_update(connection, record$id, record$reading_started, list(
        reading_pid = reading_model_pid(process)
      ))
      NULL
    },
    error = conditionMessage
  )
}

stop_job <- function(id) {
  reading_model_kill(reading_jobs[[id]]$process)
  rm(list = id, envir = reading_jobs)
}

# A claimed draft whose job can't be found: its process is gone, or it never
# recorded one.
job_is_lost <- function(record, now = Sys.time()) {
  if (is.null(record$reading_pid)) {
    claimed <- parse_time(record$reading_claimed_at)
    return(as.numeric(difftime(now, claimed, units = "secs")) > reading_claim_grace_seconds)
  }
  !reading_model_pid_alive(record$reading_pid)
}
