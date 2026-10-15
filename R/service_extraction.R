# ------ Extraction drafts --------------------------------------------------------
# An extraction starts one of two ways. AI-Powered: staff upload a questionnaire PDF, and the
# reading job (Gemini; service_reading.R) moves `extraction_status` along
#   queued -> sent -> received -> ready      or stops at failed
# and saves the questionnaire it read. Manual: staff type the questionnaire themselves, with or
# without a PDF to look at, and there is no reading. Either way staff edit a copy on the Edit
# Extraction page (see draft_service_load()). `status` belongs to the draft: Draft until its
# change request is submitted, Awaiting Review while it waits for approval, Sent Back when a
# reviewer returns it, Withdrawn if discarded, Applied once it is in the MATR metadata database.
# `info` is where it goes in the database: its registry, study, questionnaire and wave (all
# text; rows picked from the live tables are saved as their IDs). It is picked when the change
# request is submitted, so a draft can have none. Records are kept in the database
# (model_extraction.R); `connection` is the one made in global.R.

extraction_open_statuses <- c("Draft", "Sent Back")

# The drafts whose questionnaire and wave another draft mustn't take as well: those still being
# prepared, and those waiting for approval.
extraction_pending_statuses <- c(extraction_open_statuses, "Awaiting Review")

extraction_info_fields <- c(
  "registry_id", "study_id", # the registry and study it belongs to
  "kind", # "new_wave": a recorded questionnaire, or "new_questionnaire"
  "questionnaire_id", # new_wave: which questionnaire
  "name", "abbreviation", # new_questionnaire: about it, and its respondent type
  "respondent_type_id",
  "description",
  "wave_choice", # a recorded questionnaire's wave: "new", or "existing" (a recorded wave with no questions)
  "wave_id", # wave_choice "existing": which wave
  "wave_name", "administered_date", "respondent_count" # a new wave: about it
)

#' Every extraction draft, newest saved first.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of records named by id; see `extraction_model_get_all()`.
extraction_service_get_all <- function(connection) {
  extraction_model_get_all(connection)
}

#' A summary that changes whenever a draft is added or saved.
#'
#' By this session or any other. A database that can't be reached gives a signature of
#' its own, so the page keeps polling instead of failing, and notices when it is back.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A string; compare it with an earlier one.
extraction_service_signature <- function(connection) {
  tryCatch(
    extraction_model_signature(connection),
    error = function(problem) paste("problem:", conditionMessage(problem))
  )
}

#' The drafts still being prepared: not yet submitted, or sent back.
#'
#' @param records A list of extraction records.
#' @param prepared_by_id Only those prepared by this staff member, if given.
#' @return The matching records.
extraction_service_open_drafts <- function(records, prepared_by_id = NULL) {
  draft_row_service_open(records, extraction_open_statuses, prepared_by_id)
}

#' A record's info with every field present.
#'
#' @param record An extraction record, as `extraction_model_get_all()` returns it. `NULL` for an empty form.
#' @return A named list of strings, one per field in `extraction_info_fields`, blanks as
#'   `""`; `kind` is `"new_wave"` and `wave_choice` `"new"` unless the record says otherwise.
extraction_service_info <- function(record = NULL) {
  info <- lapply(extraction_info_fields, function(field) {
    value <- record$info[[field]]
    if (is.null(value)) "" else as.character(value)
  })
  info <- stats::setNames(info, extraction_info_fields)
  if (!nzchar(info$kind)) info$kind <- "new_wave"
  if (!nzchar(info$wave_choice)) info$wave_choice <- "new"
  info
}

#' Whether an extraction goes into a wave that is recorded already.
#'
#' Only a recorded questionnaire's wave can be: a new questionnaire's wave is new too.
#'
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @return `TRUE` or `FALSE`.
extraction_service_uses_recorded_wave <- function(info) {
  identical(info$kind, "new_wave") && identical(info$wave_choice, "existing")
}

#' Fill in the study and registry a recorded questionnaire belongs to.
#'
#' A recorded questionnaire says which study and registry it belongs to, so they are
#' filled in from it when blank (as in drafts saved before the form asked for them), and
#' the questionnaire stays among the drop-down's choices.
#'
#' @param info Where it goes in the database, as `extraction_service_info()` returns it.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @return `info`, with `study_id` and `registry_id` filled in where they were blank.
extraction_service_place <- function(info, catalog) {
  questionnaire <- questionnaire_service_get(catalog, info$questionnaire_id)
  if (info$kind == "new_wave" && !is.null(questionnaire) && !nzchar(info$study_id)) {
    info$study_id <- as.character(questionnaire$study_id)
  }
  study <- questionnaire_service_get_study(catalog, info$study_id)
  if (!is.null(study) && !nzchar(info$registry_id)) info$registry_id <- as.character(study$registry_id)
  info
}

#' Whether the questionnaire is there to review.
#'
#' @param record An extraction record, as `extraction_model_get_all()` returns it.
#' @return `TRUE` when the reading job has finished the draft, and for an extraction typed by
#'   hand, which has nothing to wait for.
extraction_service_is_read <- function(record) {
  identical(record$method, extraction_manual) || identical(record$extraction_status, "ready")
}

# ------ Changing a draft -----------------------------------------------------------
#' Start an AI-Powered draft for an uploaded PDF.
#'
#' Reading starts at once; what staff typed before uploading comes with it. A file that
#' isn't a PDF is turned away with a sentence.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param upload One row of a `fileInput`'s value: `name`, `size`, `datapath`.
#' @param staff Who is preparing it: `list(staff_id, name, role)`.
#' @param info Where it goes in the database, as `extraction_service_info()` returns it.
#' @param now The time to record.
#' @return The record as saved, with its id.
extraction_service_create <- function(connection, upload, staff,
                                      info = extraction_service_info(), now = Sys.time()) {
  record <- list(
    status = "Draft",
    method = extraction_ai,
    prepared_by = staff$name,
    prepared_by_id = staff$staff_id,
    started = format_time(now),
    saved = format_time(now),
    pdf = store_pdf(upload),
    extraction_status = "queued",
    reading_started = format_time(now),
    reading_claimed_at = NULL,
    reading_pid = NULL,
    error = NULL,
    info = info,
    matr_csv = NULL
  )
  extraction_model_create(connection, record)
}

#' Start a Manual draft: the questionnaire is typed by hand.
#'
#' There is no reading. The PDF is optional: if staff give one, it is kept with the draft so the
#' Edit Extraction page can show it beside what they type. The questionnaire starts empty.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param staff Who is preparing it: `list(staff_id, name, role)`.
#' @param info Where it goes in the database, as `extraction_service_info()` returns it.
#' @param upload One row of a `fileInput`'s value: `name`, `size`, `datapath`; or `NULL` for none.
#' @param now The time to record.
#' @return The record as saved, with its id.
extraction_service_create_manual <- function(connection, staff, info = extraction_service_info(),
                                             upload = NULL, now = Sys.time()) {
  record <- list(
    status = "Draft",
    method = extraction_manual,
    prepared_by = staff$name,
    prepared_by_id = staff$staff_id,
    started = format_time(now),
    saved = format_time(now),
    pdf = if (!is.null(upload)) store_pdf(upload) else list(),
    info = info,
    matr_csv = NULL,
    questionnaire = draft_service_to_json(draft_service_empty())
  )
  extraction_model_create(connection, record)
}

#' Save where an extraction goes, by itself.
#'
#' The site saves it with the change request instead (`extraction_service_push()`); the database tests
#' use this to check that saving one field never undoes another.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param info Where it goes in the database, as `extraction_service_info()` returns it.
#' @param me Who is saving: `list(staff_id, name, role)`.
#' @return The record as saved.
extraction_service_save_info <- function(connection, id, info, me) {
  change_record(connection, id, me, function(record) {
    record$info <- info
    record
  })
}

#' Keep an uploaded CSV of MATR IDs with the draft.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param upload One row of a `fileInput`'s value.
#' @param me Who is saving: `list(staff_id, name, role)`.
#' @return The record as saved.
extraction_service_add_matr_csv <- function(connection, id, upload, me) {
  matr_csv <- list(name = upload$name, text = extraction_model_read_csv(upload$datapath))
  change_record(connection, id, me, function(record) {
    record["matr_csv"] <- list(matr_csv)
    record
  })
}

#' Give a draft a different PDF.
#'
#' Reading starts again.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param upload One row of a `fileInput`'s value.
#' @param me Who is saving: `list(staff_id, name, role)`.
#' @return The record as saved.
extraction_service_replace_pdf <- function(connection, id, upload, me) {
  pdf <- store_pdf(upload)
  change_record(connection, id, me, function(record) {
    record$pdf <- pdf
    if (identical(record$method, extraction_manual)) record else restart_reading(record)
  })
}

#' Read a draft again.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param me Who asks: `list(staff_id, name, role)`.
#' @return The record as saved.
extraction_service_retry <- function(connection, id, me) {
  change_record(connection, id, me, restart_reading, new_revision = FALSE)
}

#' The reading job saying how far it has got.
#'
#' If reading has started again since the job began (a new PDF, or "Try reading again"),
#' its news is stale and nothing is saved.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param reading_started The reading's start time the job was started for.
#' @param changes A named list of record fields to their new values.
#' @return Whether it was saved.
extraction_service_reading_update <- function(connection, id, reading_started, changes) {
  record <- extraction_model_get(connection, id)
  if (!identical(record$reading_started, reading_started)) {
    return(FALSE)
  }
  extraction_model_save_reading(connection, id, changes, reading_started)
}

#' Discard a draft.
#'
#' The record is kept, as Withdrawn, so it stays in the history.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param me Who discards it: `list(staff_id, name, role)`.
#' @return The record as saved.
extraction_service_withdraw <- function(connection, id, me) {
  change_record(connection, id, me, function(record) {
    record$status <- "Withdrawn"
    record
  }, new_revision = FALSE)
}

# ------ The questionnaire it read ----------------------------------------------------
#' A draft's questionnaire.
#'
#' The reviewed copy if staff have saved one, else what the reading job returned.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record An extraction record, as `extraction_model_get_all()` returns it.
#' @return A draft; see `draft_service_load()`.
extraction_service_load_draft <- function(connection, record) {
  draft_service_load(connection, record$id)
}

#' Save staff's reviewed copy of the questionnaire.
#'
#' For an AI-Powered extraction it also keeps what the reviewer changed since the last save
#' (see `draft_service_review_edits()`), next to the AI's answer, which is never touched.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The extraction draft's record.
#' @param draft A draft, as the Edit Extraction page edits it.
#' @param base The draft as it was when the page opened it or last saved it; what the
#'   changes are measured against.
#' @param me Who is saving: `list(staff_id, name, role)`.
#' @param now The time to record.
#' @return Called for its effect.
extraction_service_save_draft <- function(connection, record, draft, base, me, now = Sys.time()) {
  edits <- NULL
  if (identical(record$method, extraction_ai)) {
    kept <- draft_model_get_record(connection, record$id)$edits
    edits <- c(kept, draft_service_review_edits(base, draft, me, now))
  }
  draft_service_save(connection, record$id, draft, me$staff_id, edits)
}

# ------ Progress -------------------------------------------------------------------
# Each step has a `state`: done, in_progress, not_started or attention.

reading_steps <- data.frame(
  label = c("Uploaded", "Sent to Gemini", "Reading pages", "Checking the result", "Saved as a draft"),
  detail = c(
    "Stored with this draft so reviewers can see it later",
    "With MATR's standard extraction instructions",
    "Gemini reads every page of the PDF",
    "Makes sure every question has at least one variable",
    "Ready for you to review"
  )
)

#' How far reading the PDF has got, as the Extraction page lists it.
#'
#' @param record An extraction record, as `extraction_model_get_all()` returns it.
#' @return A data frame with `label`, `detail` and `state` (`done`, `in_progress`,
#'   `not_started` or `attention`), one row per step.
extraction_service_reading_steps <- function(record) {
  status <- record$extraction_status
  n_done <- switch(status,
    queued = 1,
    sent = 2,
    received = 3,
    ready = 5,
    failed = 2
  )
  steps <- reading_steps
  steps$state <- ifelse(seq_len(nrow(steps)) <= n_done, "done", "not_started")
  if (n_done < nrow(steps)) {
    steps$state[n_done + 1] <- if (status == "failed") "attention" else "in_progress"
  }
  if (status == "queued") steps$detail[2] <- "Waiting to start"
  if (status == "failed") steps$detail[3] <- record$error %||% "Gemini stopped before the end."
  steps
}

#' The steps on each Existing Draft card.
#'
#' How it was read (or that it was typed by hand), and how much of it has been checked on the Edit
#' Extraction page, as the page's own "N% complete" counts it.
#'
#' @param record An extraction record, as `extraction_model_get_all()` returns it.
#' @param draft The questionnaire it read, or `NULL` until reading has finished.
#' @return A data frame with `label` and `state`, one row per step.
extraction_service_progress <- function(record, draft = NULL) {
  rbind(
    reading_progress(record),
    structure_progress(draft)
  )
}

progress_step <- function(label, state) {
  data.frame(label = label, state = state)
}

reading_progress <- function(record) {
  if (identical(record$method, extraction_manual)) {
    return(progress_step("Typed by hand", "done"))
  }
  switch(record$extraction_status,
    ready = progress_step("Read by Gemini", "done"),
    failed = progress_step("Read by Gemini · failed", "attention"),
    progress_step("Read by Gemini", "in_progress")
  )
}

structure_progress <- function(draft) {
  if (is.null(draft)) {
    return(progress_step("Structure", "not_started"))
  }
  counts <- draft_service_counts(draft)
  progress_step(
    paste0(draft_service_percent_checked(counts), "% reviewed"),
    count_state(counts$checked, counts$blocks)
  )
}


count_state <- function(n_done, n_total) {
  if (n_total > 0 && n_done == n_total) "done" else if (n_done > 0) "in_progress" else "not_started"
}

# ------ Naming ---------------------------------------------------------------------
#' A card's title: where it goes, once that is picked; until then the name the AI read off the
#' PDF, or the PDF's file name.
#'
#' @param record An extraction record, as `extraction_model_get_all()` returns it.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param read The draft's questionnaire, with whichever of `name` and `abbreviation` were printed, or `NULL`.
#' @return A string such as `"Adult Health and Personality (AHP) · Wave 3"`.
extraction_service_title <- function(record, catalog, read = NULL) {
  info <- extraction_service_info(record)
  questionnaire <- extraction_service_questionnaire(info, catalog)
  wave <- extraction_service_wave_name(info, catalog)
  if (is.null(questionnaire) && !nzchar(wave)) {
    read_name <- questionnaire_name(trimws(read$name %||% ""), trimws(read$abbreviation %||% ""))
    if (nzchar(read_name)) {
      return(read_name)
    }
    return(record$pdf$name %||% "Untitled questionnaire")
  }
  paste(c(
    if (!is.null(questionnaire)) questionnaire_name(questionnaire$name, questionnaire$abbreviation) else "Unnamed questionnaire",
    if (nzchar(wave)) wave
  ), collapse = " · ")
}

# The wave's name: the recorded one's, or as typed for a new one; "" if neither is known yet.
extraction_service_wave_name <- function(info, catalog) {
  if (extraction_service_uses_recorded_wave(info)) {
    wave <- catalog$waves[as.character(catalog$waves$wave_id) %in% info$wave_id, , drop = FALSE]
    return(if (nrow(wave) == 1) trimws(wave$name) else "")
  }
  trimws(info$wave_name)
}

#' The Edit Extraction page's heading.
#'
#' Until staff say where it goes, it goes by what Gemini read off the PDF.
#'
#' @param record An extraction record, as `extraction_model_get_all()` returns it.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param read The draft's questionnaire, with whichever of `name` and `abbreviation` were printed.
#' @return A list: `title` (e.g. `"AHP · Wave 3"`) and `subtitle` (the full name, or `""`).
extraction_service_heading <- function(record, catalog, read = NULL) {
  info <- extraction_service_info(record)
  questionnaire <- extraction_service_questionnaire(info, catalog) %||% read
  name <- trimws(questionnaire$name %||% "")
  abbreviation <- trimws(questionnaire$abbreviation %||% "")
  short_name <- if (nzchar(abbreviation)) abbreviation else if (nzchar(name)) name else "Unnamed questionnaire"
  wave <- extraction_service_wave_name(info, catalog)
  list(
    title = paste(c(short_name, if (nzchar(wave)) wave), collapse = " · "),
    subtitle = if (nzchar(abbreviation)) name else ""
  )
}

# The questionnaire's name and abbreviation: the recorded one's, or as typed
# for a new one. NULL if neither is known yet.
extraction_service_questionnaire <- function(info, catalog) {
  if (info$kind == "new_wave") {
    return(questionnaire_service_get(catalog, info$questionnaire_id))
  }
  if (has_text(info$name) || has_text(info$abbreviation)) {
    list(name = trimws(info$name), abbreviation = trimws(info$abbreviation))
  }
}

questionnaire_name <- function(name, abbreviation) {
  if (!nzchar(name)) {
    return(abbreviation)
  }
  if (!nzchar(abbreviation)) {
    return(name)
  }
  paste0(name, " (", abbreviation, ")")
}

# ------ Checks on what staff typed ---------------------------------------------------
#' The required fields left blank.
#'
#' The form goes registry, study, then the questionnaire: a recorded questionnaire needs picking, and a
#' new questionnaire everything the QUESTIONNAIRE row will hold. Then the wave: a recorded one needs
#' picking, and a new one a name.
#'
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @return The names of the fields, as the form labels them; empty if none.
extraction_service_missing <- function(info) {
  required <- c(registry_id = "registry", study_id = "study")
  required <- c(required, if (info$kind == "new_questionnaire") {
    c(name = "questionnaire name", abbreviation = "abbreviation", respondent_type_id = "respondent type")
  } else {
    c(questionnaire_id = "questionnaire")
  })
  required <- c(required, if (extraction_service_uses_recorded_wave(info)) c(wave_id = "wave") else c(wave_name = "wave name"))
  blank <- !vapply(names(required), function(field) has_text(info[[field]]), logical(1))
  unname(required[blank])
}

# The rest return a sentence saying what to do, or NULL if all is well.

#' A problem with a new questionnaire's name, or `NULL`.
#'
#' The name must be new for its study. The database allows repeats (BR14), but two
#' questionnaires named alike would be hard to tell apart.
#' #' The same goes for another draft that is being prepared, or waiting for approval, as a new
#' questionnaire of the same study, so the two don't add it twice.
#'
#' @param info Where it goes, as `extraction_service_info()` returns it.#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param records All the extraction records, to find other drafts of a new questionnaire.
#' @param id This draft's id, so it doesn't clash with itself.
#' @return A sentence saying what to do, or `NULL`.
extraction_service_name_problem <- function(info, catalog, records = list(), id = NULL) {
  same <- same_in_study(info, catalog, "name")
  if (nrow(same) > 0) {
    return(sprintf("This study already has a questionnaire called %s. Pick another name, or choose Existing questionnaire.", same$name[1]))
  }
  typed <- tolower(trimws(info$name))
  if (info$kind != "new_questionnaire" || !nzchar(typed)) {
    return(NULL)
  }
  others <- Filter(function(record) {
    other <- extraction_service_info(record)
    !identical(record$id, id) && other$kind == "new_questionnaire" && identical(other$study_id, info$study_id) &&
      tolower(trimws(other$name)) == typed
  }, draft_row_service_open(records, extraction_pending_statuses))
  if (length(others) > 0) {
    sprintf(
      "%s already has a draft adding %s to this study. Pick another name, or continue that draft.",
      others[[1]]$prepared_by %||% "Someone", trimws(info$name)
    )
  }
}

#' A problem with a new questionnaire's abbreviation, or `NULL`.
#'
#' @param info Where it goes in the database, as `extraction_service_info()` returns it.#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @return A sentence saying what to do, or `NULL`.
extraction_service_abbreviation_problem <- function(info, catalog) {
  same <- same_in_study(info, catalog, "abbreviation")
  if (nrow(same) > 0) sprintf("Already used for %s. Pick another.", same$name[1])
}

# The study's recorded questionnaires whose `field` matches what was typed for
# a new one, ignoring case and spaces at either end.
same_in_study <- function(info, catalog, field) {
  questionnaires <- catalog$questionnaires
  typed <- tolower(trimws(info[[field]]))
  if (info$kind != "new_questionnaire" || !nzchar(typed)) {
    return(questionnaires[0, ])
  }
  questionnaires[
    questionnaires$study_id %in% info$study_id & tolower(trimws(questionnaires[[field]])) %in% typed,
  ]
}

#' A problem with a new wave's name, or `NULL` (BR21).
#'
#' A questionnaire's wave names are unique, among its recorded waves and the drafts
#' being prepared for it or waiting for approval.
#'
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param records All the extraction records, to find other drafts of the same wave.
#' @param id This draft's id, so it doesn't clash with itself.
#' @param me Who is using the site, `list(staff_id, name)`, so a clash with their own
#'   draft says "You".
#' @return A sentence saying what to do, or `NULL`.
extraction_service_wave_name_problem <- function(info, catalog, records, id = NULL, me = NULL) {
  wave_name <- trimws(info$wave_name)
  if (info$kind != "new_wave" || extraction_service_uses_recorded_wave(info) || !nzchar(wave_name)) {
    return(NULL)
  }
  questionnaire <- questionnaire_service_get(catalog, info$questionnaire_id)
  if (is.null(questionnaire)) {
    return(NULL)
  }
  same_name <- function(names) tolower(trimws(names)) == tolower(wave_name)

  waves <- catalog$waves[catalog$waves$questionnaire_id == questionnaire$questionnaire_id, ]
  if (any(same_name(waves$name))) {
    existing <- waves$name[same_name(waves$name)][1]
    return(sprintf("%s already has a %s. Pick another name.", questionnaire$abbreviation, existing))
  }
  others <- Filter(function(record) {
    other <- extraction_service_info(record)
    !identical(record$id, id) && other$kind == "new_wave" && !extraction_service_uses_recorded_wave(other) &&
      identical(other$questionnaire_id, info$questionnaire_id) && same_name(other$wave_name)
  }, draft_row_service_open(records, extraction_pending_statuses))
  if (length(others) > 0) {
    who <- if (draft_row_service_is_mine(others[[1]], me)) "You" else others[[1]]$prepared_by
    return(sprintf(
      "%s already started a draft of %s %s. Pick another name, or continue that draft.",
      who, questionnaire$abbreviation, wave_name
    ))
  }
  NULL
}


#' A problem with the recorded wave an extraction goes into, or `NULL`.
#'
#' It has to be one of the questionnaire's waves, with no questions yet (an extraction adds every
#' question the wave asks), and no other draft being prepared or waiting for approval may be going into
#' it too.
#'
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param question_counts How many questions each of the questionnaire's waves asks: a named integer
#'   vector by wave id (`questionnaire_service_wave_question_counts()`).
#' @param records All the extraction records, to find other drafts going into the same wave.
#' @param id This draft's id, so it doesn't clash with itself.
#' @param me Who is using the site, `list(staff_id, name)`, so a clash with their own draft says "You".
#' @return A sentence saying what to do, or `NULL`.
extraction_service_recorded_wave_problem <- function(info, catalog, question_counts = integer(), records = list(),
                                                     id = NULL, me = NULL) {
  if (!extraction_service_uses_recorded_wave(info) || !has_text(info$wave_id)) {
    return(NULL)
  }
  wave <- catalog$waves[as.character(catalog$waves$wave_id) %in% info$wave_id, , drop = FALSE]
  if (nrow(wave) != 1 || !as.character(wave$questionnaire_id) %in% info$questionnaire_id) {
    return("Pick one of this questionnaire's waves.")
  }
  n_questions <- question_counts[info$wave_id]
  if (isTRUE(n_questions > 0)) {
    return(sprintf(
      "%s already asks %s. Pick a wave without questions, or add a new wave.", wave$name, plural(n_questions, "question")
    ))
  }
  others <- Filter(function(record) {
    other <- extraction_service_info(record)
    !identical(record$id, id) && extraction_service_uses_recorded_wave(other) && identical(other$wave_id, info$wave_id)
  }, draft_row_service_open(records, extraction_pending_statuses))
  if (length(others) > 0) {
    who <- if (draft_row_service_is_mine(others[[1]], me)) "You" else others[[1]]$prepared_by %||% "Someone"
    return(sprintf("%s already has a draft going into %s. Pick another wave, or continue that draft.", who, wave$name))
  }
  NULL
}

#' A problem with a typed date, or `NULL`.
#'
#' @param text What was typed; blank is fine.
#' @return A sentence saying what to do, or `NULL`.
extraction_service_date_problem <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) {
    return(NULL)
  }
  ok <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", text) && !is.na(as.Date(text, "%m/%d/%Y"))
  if (!ok) "Type the date as MM/DD/YYYY, e.g. 04/15/2022."
}

#' A problem with a typed number of respondents, or `NULL`.
#'
#' @param text What was typed; blank is fine.
#' @return A sentence saying what to do, or `NULL`.
extraction_service_count_problem <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) {
    return(NULL)
  }
  if (!grepl("^[0-9][0-9,]*$", text)) "Type a whole number, e.g. 1,200."
}

#' The MATR IDs in a CSV, in order.
#'
#' A first row naming a column `matr_id`, `variable_id` or the like is its heading, and
#' the IDs are in that column; without one, every row is an ID and they are in the first
#' column. Spaces are trimmed and blank rows dropped.
#'
#' @param csv_text The CSV's text.
#' @return A character vector; empty if there are none.
extraction_service_matr_ids <- function(csv_text) {
  csv_text <- sub("^\ufeff", "", csv_text) # the byte order mark Excel starts a CSV with
  rows <- tryCatch(
    utils::read.csv(
      text = csv_text, header = FALSE, colClasses = "character",
      na.strings = character(), blank.lines.skip = TRUE
    ),
    error = function(problem) data.frame()
  )
  if (nrow(rows) == 0) {
    return(character())
  }
  is_heading <- grepl(matr_id_headings, trimws(unlist(rows[1, ])), ignore.case = TRUE)
  column <- if (any(is_heading)) which(is_heading)[1] else 1
  ids <- trimws(rows[[column]])
  if (any(is_heading)) ids <- ids[-1]
  ids[nzchar(ids)]
}

# Headings like matr_id, MATR ID, variable_id, variable, Variable name or id.
matr_id_headings <- "^((matr|variable)[ _.]?(ids?|names?)?|ids?)$"

# ------ What a recorded questionnaire prints already ----------------------------------------
#' The blocks a recorded questionnaire prints, and its questions' MATR IDs.
#'
#' For a new wave of a recorded questionnaire, the Edit Extraction page offers the blocks the questionnaire already
#' prints as suggestions while a block's words are typed, and a question with the same words as one the
#' questionnaire already asks keeps its variables' MATR IDs (BR93: a MATR ID names the variable within its
#' questionnaire), so they are filled in instead of asked for. For a new questionnaire there is nothing yet.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record An extraction record, as `extraction_model_get_all()` returns it.
#' @param component_types The kinds of block with their ids; read from the database unless given.
#' @return A list of two data frames: `texts` (`component_type`, `text`: one row per block the questionnaire
#'   prints) and `matr_ids` (`text`, `matr_id`, `display_order`: one row per variable, with its question's
#'   words and its place in the question). Both have no rows for a new questionnaire.
extraction_service_recorded <- function(connection, record, component_types = NULL) {
  info <- extraction_service_info(record)
  none <- list(
    texts = data.frame(component_type = character(), text = character()),
    matr_ids = data.frame(text = character(), matr_id = character(), display_order = numeric())
  )
  if (!identical(info$kind, "new_wave") || !has_text(info$questionnaire_id)) {
    return(none)
  }
  existing <- push_model_existing(connection, info$questionnaire_id)
  if (is.null(component_types)) component_types <- lookup_model_get_component_types(connection)
  components <- existing$components
  type_of <- component_types$name[match(components$component_type_id, component_types$component_type_id)]
  texts <- data.frame(component_type = as.character(type_of), text = as.character(components$text), stringsAsFactors = FALSE)
  texts <- texts[!is.na(texts$text) & !is.na(texts$component_type), , drop = FALSE]
  variables <- existing$variables
  question_text <- components$text[match(variables$component_id, components$component_id)]
  matr_ids <- data.frame(
    text = as.character(question_text), matr_id = as.character(variables$matr_id),
    display_order = as.numeric(variables$display_order), stringsAsFactors = FALSE
  )
  list(texts = texts, matr_ids = matr_ids[!is.na(matr_ids$text), , drop = FALSE])
}

# ------ Helpers ----------------------------------------------------------------------

# Every change re-reads the record first, and saves only the fields it
# changed, so it never undoes what someone else saved since this page read it.
# `me` is who saves it. `new_revision` is FALSE for a change that isn't to what a
# reviewer would read (restarting a reading, discarding). Returns the record as saved.
change_record <- function(connection, id, me, change, new_revision = TRUE) {
  before <- extraction_model_get(connection, id)
  after <- change(before)
  fields <- union(names(before), names(after))
  changed <- fields[!vapply(fields, function(field) identical(before[[field]], after[[field]]), logical(1))]
  extraction_model_save(connection, after, changed, me$staff_id, new_revision = new_revision)
}

restart_reading <- function(record) {
  record$extraction_status <- "queued"
  record$reading_started <- format_time(Sys.time())
  record["error"] <- list(NULL)
  record["reading_pid"] <- list(NULL)
  record["reading_claimed_at"] <- list(NULL)
  record
}

store_pdf <- function(upload) {
  if (!extraction_model_is_pdf(upload$datapath)) {
    stop(upload$name, " isn't a PDF. Choose the questionnaire's PDF file.", call. = FALSE)
  }
  sha256 <- extraction_model_sha256(upload$datapath)
  list(
    name = upload$name,
    path = extraction_model_save_pdf(upload$datapath, sha256),
    pages = extraction_model_page_count(upload$datapath),
    bytes = upload$size,
    sha256 = sha256
  )
}
