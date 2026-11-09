# Fills the test project with two sample questionnaires, the way the site adds one.
# From the project folder:
#   Rscript tests/database/seed_test_data.R            (pushes them; a wave extracted before is emptied first)
#   Rscript tests/database/seed_test_data.R --dry-run  (builds and checks them, writes nothing)
#
# The seed (sql/7_seed.sql) gives the test project its registries, studies, questionnaires and waves, with
# nothing extracted. This puts made-up content into two of the seed's waves, shaped like the questionnaires
# staff extract: sections with headings, an introduction and instructions, a grid whose items finish a lead-in,
# a follow-up that depends on the question before it, MATR IDs in the registry's style, open-ended answers with
# units, and the answer choices real questionnaires print.
#
# Each goes in through the path the site uses: an `extraction` row (method Manual, prepared by a sample ADMIN,
# Dana Whitfield, dana.whitfield@example.test, left inactive afterwards) whose `questionnaire` is the JSON the
# Edit Extraction page saves (docs/database_design.md), pushed by the database's own push_extraction(), so the
# rows are the ones the site would write and the History lists the push. It only works on the test project or
# a local database; don't run it while the tests are running.

dry_run <- "--dry-run" %in% commandArgs(trailingOnly = TRUE)

log_info <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
  flush.console()
}

# ------ Loading -----------------------------------------------------------------------------------------------
if (file.exists(".env")) readRenviron(".env")
suppressMessages({
  library(shiny)
  library(testthat)
})
for (path in sort(list.files("R", "\\.[Rr]$", full.names = TRUE))) sys.source(path, envir = globalenv())
source("tests/testthat/helper-00_realdb.R")
connection <- test_database()
log_info(if (dry_run) "Dry run: nothing is written. " else "", "Test database: ", connection$url)

# ------ Writing a questionnaire ---------------------------------------------------------------------------------
# A questionnaire is written as blocks, the way it is printed. A question's `key` names it for a follow-up
# (`follows`). Questions are numbered in order; `printed` is the number printed beside one when it differs.
section <- function(text, ...) list(component_type = "Section", text = text, children = list(...))
grid <- function(text, ...) list(component_type = "Grid", text = text, children = list(...))
introduction <- function(text) list(component_type = "Introduction", text = text)
instruction <- function(text) list(component_type = "Instruction", text = text)
note <- function(text) list(component_type = "Note", text = text)
question <- function(key, text, ..., printed = NULL, follows = NULL) {
  list(component_type = "Question", key = key, text = text, printed = printed, follows = follows, variables = list(...))
}

# A variable chosen from `choices` (code = words).
choose <- function(matr_id, choices, label = NULL, subject = NULL, time_frame = NULL) {
  list(variable_type = "closed_ended", matr_id = matr_id, label = label, subject = subject, time_frame = time_frame, choices = choices)
}
# A variable the respondent writes: a NUMBER (with its unit), TEXT, DATE or TIME.
write_in <- function(matr_id, label = NULL, type = "NUMBER", unit = NULL, subject = NULL, time_frame = NULL) {
  list(variable_type = "open_ended", matr_id = matr_id, label = label, answer_data_type = type, unit = unit, subject = subject, time_frame = time_frame)
}

yes_no <- c("1" = "Yes", "2" = "No")
never_often <- c("0" = "Never", "1" = "Rarely", "2" = "Sometimes", "3" = "Often")
excellent_poor <- c("1" = "Excellent", "2" = "Very good", "3" = "Good", "4" = "Fair", "5" = "Poor")
twin_type <- c("1" = "Identical", "2" = "Fraternal", "3" = "Not sure")

# The extraction's JSON (docs/database_design.md) for blocks written with the functions above. The printed
# words are kept in lower case, as the site keeps them.
# blocks: The top-level blocks.
# Returns: The JSON as nested lists, for the `questionnaire` column of `extraction`.
questionnaire_json <- function(name, abbreviation, blocks) {
  number <- list() # key -> its question_number, 1, 2, 3...
  n <- 0L
  count <- function(blocks) {
    for (block in blocks) {
      if (block$component_type == "Question") {
        n <<- n + 1L
        number[[block$key]] <<- as.character(n)
      }
      count(block$children)
    }
  }
  count(blocks)
  compact <- function(row) Filter(Negate(is.null), row)
  variable_json <- function(box) {
    row <- list(
      variable_type = box$variable_type, matr_id = box$matr_id, label = box$label, subject = box$subject,
      time_frame = box$time_frame, unit = box$unit, answer_data_type = box$answer_data_type
    )
    if (box$variable_type == "closed_ended") {
      row$response_options <- lapply(seq_along(box$choices), function(j) {
        list(answer_option = unname(box$choices[j]), code_value = names(box$choices)[j])
      })
    }
    compact(row)
  }
  block_json <- function(block) {
    row <- list(component_type = block$component_type, text = if (!is.null(block$text)) tolower(block$text), checked = TRUE)
    if (block$component_type == "Question") {
      if (!is.null(block$follows) && is.null(number[[block$follows]])) stop("No question is keyed ", block$follows, call. = FALSE)
      row <- c(row, list(
        question_number = number[[block$key]], printed_number = block$printed %||% number[[block$key]],
        parent_question = if (!is.null(block$follows)) number[[block$follows]],
        variables = lapply(block$variables, variable_json)
      ))
    }
    if (!is.null(block$children)) row$children <- lapply(block$children, block_json)
    compact(row)
  }
  list(questionnaire = list(name = name, abbreviation = abbreviation), components = lapply(blocks, block_json))
}

# ------ The questionnaires ------------------------------------------------------------------------------------
# What goes into which of the seed's waves: the questionnaire by its abbreviation, the wave by its name.
samples <- list(
  list(
    abbreviation = "KQ", wave = "Pilot",
    blocks = list(
      introduction("This questionnaire asks about you, your health and your twin. There are no right or wrong answers. Please answer every question as well as you can."),
      section(
        "About You",
        question("dob", "What is your date of birth?", write_in("KQ1", type = "DATE")),
        question("grade", "What grade are you in at school?", write_in("KQ2", unit = "grade")),
        question("sex", "Are you a boy or a girl?", choose("KQ3", c("1" = "Boy", "2" = "Girl")))
      ),
      section(
        "Your Health",
        instruction("Check one box for each question."),
        question("health", "In general, would you say your health is:", choose("KQ4", excellent_poor)),
        question("asthma", "Has a doctor ever told you that you have asthma?", choose("KQ5", yes_no)),
        question("asthma_age", "How old were you when a doctor first told you that?", write_in("KQ6", unit = "years"), printed = "5a", follows = "asthma"),
        question(
          "sleep", "On a school night, how many hours do you usually sleep?",
          write_in("KQ7", unit = "hours", time_frame = "on a school night")
        )
      ),
      section(
        "You and Your Twin",
        grid(
          "In the past 30 days, how often have you...",
          question("sad", "felt sad or down?", choose("KQ8A", never_often, time_frame = "past 30 days")),
          question("trouble_sleeping", "had trouble falling asleep?", choose("KQ8B", never_often, time_frame = "past 30 days")),
          question("argued", "argued with your twin?", choose("KQ8C", never_often, subject = "twin", time_frame = "past 30 days"))
        ),
        question("same_room", "Do you and your twin share a bedroom?", choose("KQ9", yes_no, subject = "twin"))
      ),
      note("Thank you! Please put this questionnaire in the envelope and seal it.")
    )
  ),
  list(
    abbreviation = "MQ", wave = "Pilot",
    blocks = list(
      introduction("Please answer these questions about your twins. If you are not sure of an answer, give your best guess."),
      section(
        "About the Twins",
        question("twin_type", "Are your twins identical or fraternal?", choose("MQ1", twin_type, subject = "children")),
        question(
          "birth_weight", "How much did each twin weigh at birth?",
          write_in("MQ2A", label = "Twin A", unit = "ounces", subject = "children"),
          write_in("MQ2B", label = "Twin B", unit = "ounces", subject = "children")
        ),
        question("weeks", "How many weeks pregnant were you when the twins were born?", write_in("MQ3", unit = "weeks"))
      ),
      section(
        "The Twins' Health",
        instruction("Answer for each twin."),
        question(
          "asthma", "Has a doctor ever told you that either twin has asthma?",
          choose("MQ4A", yes_no, label = "Twin A", subject = "children"),
          choose("MQ4B", yes_no, label = "Twin B", subject = "children")
        ),
        question("smoker", "Does anyone who lives in the twins' home smoke cigarettes?", choose("MQ5", yes_no, subject = "family")),
        question("cigarettes", "About how many cigarettes a day are smoked in the home?", write_in("MQ6", unit = "cigarettes", time_frame = "per day"), printed = "5a", follows = "smoker"),
        question("doctor_visits", "In the past 12 months, how many times has each twin seen a doctor?",
          write_in("MQ7A", label = "Twin A", unit = "visits", subject = "children", time_frame = "past 12 months"),
          write_in("MQ7B", label = "Twin B", unit = "visits", subject = "children", time_frame = "past 12 months")
        ),
        question("comments", "Is there anything else you would like to tell us about your twins' health?", write_in("MQ8", type = "TEXT"))
      ),
      note("Thank you for taking part.")
    )
  )
)

# ------ Checking them -------------------------------------------------------------------------------------------
# What the push would refuse is caught here first, so a mistake above stops before anything is written.
check <- function(json, label) {
  matr_ids <- character()
  n <- c(blocks = 0L, questions = 0L, variables = 0L, choices = 0L)
  visit <- function(blocks) {
    for (block in blocks) {
      n[["blocks"]] <<- n[["blocks"]] + 1L
      if (block$component_type != "Group" && !nzchar(block$text %||% "")) stop(label, ": a ", block$component_type, " has no words.", call. = FALSE)
      if (block$component_type == "Question") {
        n[["questions"]] <<- n[["questions"]] + 1L
        for (v in block$variables) {
          n[["variables"]] <<- n[["variables"]] + 1L
          matr_ids <<- c(matr_ids, v$matr_id)
          if (v$variable_type == "open_ended" && is.null(v$answer_data_type)) stop(label, ": ", v$matr_id, " has no answer data type.", call. = FALSE)
          if (v$variable_type == "closed_ended") {
            if (length(v$response_options) == 0) stop(label, ": ", v$matr_id, " has no choices.", call. = FALSE)
            n[["choices"]] <<- n[["choices"]] + length(v$response_options)
          }
        }
      }
      visit(block$children)
    }
  }
  visit(json$components)
  repeated <- unique(matr_ids[duplicated(matr_ids)])
  if (length(repeated) > 0) stop(label, " uses a MATR ID twice: ", paste(repeated, collapse = ", "), call. = FALSE)
  n
}

# Text of an id as the extraction's `info` holds it, without R's scientific notation.
id_text <- function(id) format(id, scientific = FALSE, trim = TRUE)

# The wave's row and where it sits, from the seed.
one_row <- function(table, columns, filters, what) {
  rows <- database_model_get(connection, table, columns, filters = filters)
  if (nrow(rows) != 1) stop("Expected one ", what, " in the test database, found ", nrow(rows), ".", call. = FALSE)
  rows
}
for (i in seq_along(samples)) {
  s <- samples[[i]]
  q <- one_row("questionnaire", c("questionnaire_id", "study_id", "name", "abbreviation"), list(abbreviation = database_eq(s$abbreviation)), paste("questionnaire", s$abbreviation))
  st <- one_row("study", c("study_id", "registry_id"), list(study_id = database_eq(id_text(q$study_id))), "study")
  w <- one_row("wave", c("wave_id", "name"), list(questionnaire_id = database_eq(id_text(q$questionnaire_id)), name = database_eq(s$wave)), paste(s$abbreviation, "wave", s$wave))
  samples[[i]]$label <- paste(q$name, s$wave, sep = ", ")
  samples[[i]]$info <- list(
    registry_id = id_text(st$registry_id), study_id = id_text(q$study_id),
    questionnaire_id = id_text(q$questionnaire_id), wave_id = id_text(w$wave_id)
  )
  samples[[i]]$json <- questionnaire_json(q$name, q$abbreviation, s$blocks)
  counts <- check(samples[[i]]$json, samples[[i]]$label)
  log_info(sprintf("%s: %d blocks, %d questions, %d variables, %d choices", samples[[i]]$label, counts[["blocks"]], counts[["questions"]], counts[["variables"]], counts[["choices"]]))
}
if (dry_run) {
  log_info("Dry run finished: the questionnaires are fine. Nothing was written.")
  quit(save = "no")
}

# ------ Pushing them ----------------------------------------------------------------------------------------------
# The sample ADMIN who prepares and pushes them: found by email, active while they work.
admin_email <- "dana.whitfield@example.test"
admin <- database_model_get(connection, "staff", "staff_id", filters = list(email = database_eq(admin_email)))
staff_id <- if (nrow(admin) == 0) {
  database_model_insert(connection, "staff", list(name = "Dana Whitfield", email = admin_email, role = "ADMIN", active = TRUE))$staff_id
} else {
  database_model_update(connection, "staff", list(active = TRUE, role = "ADMIN"), list(email = database_eq(admin_email)), select = "staff_id")
  admin$staff_id[1]
}

tryCatch(
  for (s in samples) {
    # a wave pushed before is emptied first (the site's Empty, which the History lists too)
    asked <- database_model_count(connection, "wave_question", list(wave_id = database_eq(s$info$wave_id)))
    if (asked > 0) {
      gone <- database_model_call(connection, "empty_wave", list(p_wave_id = as.numeric(s$info$wave_id), p_staff_id = staff_id))
      log_info(sprintf("  %s: emptied (%s questions removed)", s$label, gone$questions_removed))
    }
    extraction <- database_model_insert(connection, "extraction", list(
      method = "Manual", prepared_by = staff_id, saved_by = staff_id, summary = paste("Sample extraction of", s$label),
      info = s$info, questionnaire = s$json
    ), select = "extraction_id")
    counts <- database_model_call(connection, "push_extraction", list(p_extraction_id = extraction$extraction_id, p_staff_id = staff_id))
    log_info(sprintf(
      "  %s pushed (extraction %s): %s questions, %s new variables, %s new answer options, %s new tags",
      s$label, id_text(extraction$extraction_id), counts$questions, counts$variable_new %||% 0, counts$answer_option_new %||% 0, counts$tag_new %||% 0
    ))
  },
  finally = database_model_update(connection, "staff", list(active = FALSE), list(email = database_eq(admin_email)), select = "staff_id")
)
log_info("Done.")
