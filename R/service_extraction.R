# ------ Extraction drafts --------------------------------------------------------
# An extraction starts one of two ways. AI-Powered: staff upload a questionnaire PDF, and the
# reading job (Gemini; service_reading.R) moves `extraction_status` along
#   queued -> sent -> received -> ready      or stops at failed
# and saves the questionnaire it read. Manual: staff type the questionnaire themselves, with or
# without a PDF to look at, and there is no reading. Either way staff edit a copy on the Edit
# Extraction page (see draft_service_load()). `status` belongs to the draft: Draft until its
# change request is submitted, Awaiting Review while it waits for approval, Sent Back when a
# reviewer returns it, Withdrawn if discarded, Applied once it is in the MATR metadata database.
# `info` is where it goes in the database (its Basic Info): a recorded registry, study,
# questionnaire and one of that questionnaire's waves that asks no questions yet, each saved as
# its ID (text). It is picked on Change Requests, after the change request is submitted, so a draft
# can have none; anything that isn't recorded yet is added from the form's drop-downs first.
# Records are kept in the database (model_extraction.R); `connection` is the one made in global.R.

extraction_open_statuses <- c("Draft", "Sent Back")

# The drafts whose questionnaire and wave another draft mustn't take as well: those still being
# prepared, and those waiting for approval.
extraction_pending_statuses <- c(extraction_open_statuses, "Awaiting Review")

# Where an extraction goes: the IDs of a recorded registry, study, questionnaire and wave.
extraction_info_fields <- c("registry_id", "study_id", "questionnaire_id", "wave_id")

#' Every extraction draft, newest saved first.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list of records named by id; see `extraction_model_get_all()`.
extraction_service_get_all <- function(connection) {
  extraction_model_get_all(connection)
}

#' Whether a draft this staff member started is being read.
#'
#' While one is, the site reads the drafts again every few seconds, so they see how far the reading job
#' has got; otherwise it only reads them when asked (a page opened, Refresh).
#'
#' @param records The extraction records, from `extraction_service_get_all()`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @return `TRUE` or `FALSE`.
extraction_service_is_reading <- function(records, me) {
  mine <- extraction_service_open_drafts(records, prepared_by_id = me$staff_id)
  any(vapply(mine, function(record) isTRUE(record$extraction_status %in% c("queued", "sent", "received")), logical(1)))
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
#' Only the four picks are read: what drafts saved before the form was cut down to them also hold (a
#' new questionnaire's or new wave's name, and the like) is left out, so such a draft asks for the
#' questionnaire or wave to be picked again.
#'
#' @param record An extraction record, as `extraction_model_get_all()` returns it. `NULL` for an empty form.
#' @return A named list of strings, one per field in `extraction_info_fields`, blanks as `""`.
extraction_service_info <- function(record = NULL) {
  info <- lapply(extraction_info_fields, function(field) {
    value <- record$info[[field]]
    if (is.null(value)) "" else as.character(value)
  })
  stats::setNames(info, extraction_info_fields)
}

#' An extraction's Basic Info, as the Change Requests form holds it.
#'
#' The form picks a recorded registry, study, questionnaire and wave (one that asks no questions yet);
#' anything new is added from its drop-down first, so its "+ Add new …" choice isn't a pick.
#'
#' @param info Where it goes, as `extraction_service_info()` returns it, or the form's picks.
#' @return `info`, in the same shape, with "+ Add new …" as blank.
extraction_service_basic_info <- function(info) {
  basic <- lapply(extraction_info_fields, function(field) {
    value <- as.character(info[[field]] %||% "")
    if (identical(value, add_new_choice)) "" else value
  })
  stats::setNames(basic, extraction_info_fields)
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
  if (!is.null(questionnaire) && !nzchar(info$study_id)) {
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
    pdf = store_pdf(connection, upload),
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
    pdf = if (!is.null(upload)) store_pdf(connection, upload) else list(),
    info = info,
    matr_csv = NULL,
    questionnaire = draft_service_to_json(draft_service_empty())
  )
  extraction_model_create(connection, record)
}

#' Keep an uploaded CSV of MATR IDs with the draft.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param upload One row of a `fileInput`'s value.
#' @param me Who is saving: `list(staff_id, name, role)`.
#' @param expected_revision The revision the person was looking at, or `NULL` not to check.
#' @return The record as saved.
extraction_service_add_matr_csv <- function(connection, id, upload, me, expected_revision = NULL) {
  matr_csv <- list(name = upload$name, text = extraction_model_read_csv(upload$datapath))
  change_record(connection, id, me, function(record) {
    record["matr_csv"] <- list(matr_csv)
    record
  }, expected_revision = expected_revision)
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
  pdf <- store_pdf(connection, upload)
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
#' @param expected_revision The revision the page opened (or last saved): the save is refused if
#'   someone else saved the draft since, so nobody's work is overwritten. `NULL` skips the check.
#' @return The draft's revision after the save, invisibly. An error with a sentence if it was refused.
extraction_service_save_draft <- function(connection, record, draft, base, me, now = Sys.time(),
                                          expected_revision = NULL) {
  edits <- NULL
  if (identical(record$method, extraction_ai)) {
    kept <- draft_model_get_record(connection, record$id)$edits
    edits <- c(kept, draft_service_review_edits(base, draft, me, now))
  }
  draft_service_save(connection, record$id, draft, me$staff_id, edits, expected_revision)
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
#' Extraction page, as the page's own "N% human-reviewed" counts it.
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
    paste0(draft_service_percent_checked(counts), "% human-reviewed"),
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

# The recorded wave's name; "" if none is picked yet.
extraction_service_wave_name <- function(info, catalog) {
  wave <- catalog$waves[as.character(catalog$waves$wave_id) %in% info$wave_id, , drop = FALSE]
  if (nrow(wave) == 1) trimws(wave$name) else ""
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

# The recorded questionnaire picked, or NULL if none is picked yet.
extraction_service_questionnaire <- function(info, catalog) {
  questionnaire_service_get(catalog, info$questionnaire_id)
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

# ------ Checks on what staff picked ---------------------------------------------------
#' The Basic Info fields still blank.
#'
#' The form goes registry, study, questionnaire, then one of the questionnaire's waves.
#'
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @return The names of the fields, as sentences call them; empty if none.
extraction_service_missing <- function(info) {
  unname(extraction_required_fields[extraction_service_missing_fields(info)])
}

#' The Basic Info fields still blank, by their names in `extraction_info_fields`.
#'
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @return A character vector of field names, e.g. `c("registry_id", "wave_id")`.
extraction_service_missing_fields <- function(info) {
  blank <- !vapply(names(extraction_required_fields), function(field) has_text(info[[field]]), logical(1))
  names(extraction_required_fields)[blank]
}

# Every Basic Info field is needed, named by field, with what each is called in sentences.
extraction_required_fields <- c(
  registry_id = "registry", study_id = "study", questionnaire_id = "questionnaire", wave_id = "wave"
)

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
  if (!has_text(info$wave_id)) {
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
    !identical(record$id, id) && identical(other$wave_id, info$wave_id)
  }, draft_row_service_open(records, extraction_pending_statuses))
  if (length(others) > 0) {
    who <- if (draft_row_service_is_mine(others[[1]], me)) "You" else others[[1]]$prepared_by %||% "Someone"
    has <- if (identical(who, "You")) "have" else "has"
    return(sprintf("%s already %s a draft going into %s. Pick another wave, or continue that draft.", who, has, wave$name))
  }
  NULL
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
#' For the questionnaire an extraction goes into, the Edit Extraction page offers the blocks it already
#' prints as suggestions while a block's words are typed, and a question with the same words as one the
#' questionnaire already asks keeps its variables' MATR IDs (BR93: a MATR ID names the variable within its
#' questionnaire), so they are filled in instead of asked for. Until a questionnaire is picked there is
#' nothing.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record An extraction record, as `extraction_model_get_all()` returns it.
#' @param component_types The kinds of block with their ids; read from the database unless given.
#' @return A list of two data frames: `texts` (`component_type`, `text`: one row per block the questionnaire
#'   prints) and `matr_ids` (`text`, `matr_id`, `display_order`: one row per variable, with its question's
#'   words and its place in the question). Both have no rows until a questionnaire is picked.
extraction_service_recorded <- function(connection, record, component_types = NULL) {
  info <- extraction_service_info(record)
  if (!has_text(info$questionnaire_id)) {
    return(extraction_service_printed(NULL, component_types))
  }
  existing <- push_model_existing(connection, info$questionnaire_id)
  if (is.null(component_types)) component_types <- lookup_model_get_component_types(connection)
  extraction_service_printed(existing, component_types)
}

#' The MATR IDs of any recorded questionnaire's questions, to give a draft's questions worded the same.
#'
#' The MATR IDs dialog's "From a questionnaire": a questionnaire asked the same questions before (an
#' earlier version, or another respondent's), so its MATR IDs are used again where the words match.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param questionnaire_id The questionnaire's id.
#' @return A data frame with `text`, `matr_id` and `display_order`: one row per variable, with its
#'   question's words and its place in the question; no rows if it has none.
extraction_service_questionnaire_matr_ids <- function(connection, questionnaire_id) {
  if (!grepl("^[0-9]+$", questionnaire_id %||% "")) {
    stop("Pick a questionnaire first.", call. = FALSE)
  }
  extraction_service_printed(push_model_existing(connection, questionnaire_id))$matr_ids
}

#' What a recorded questionnaire prints: its blocks' words, and its variables' MATR IDs by question.
#'
#' @param existing From `push_model_existing()`, or `NULL` for none.
#' @param component_types The kinds of block with their ids; only needed for `texts`.
#' @return A list of two data frames, `texts` and `matr_ids`; see `extraction_service_recorded()`.
extraction_service_printed <- function(existing, component_types = NULL) {
  none <- list(
    texts = data.frame(component_type = character(), text = character()),
    matr_ids = data.frame(text = character(), matr_id = character(), display_order = numeric())
  )
  if (is.null(existing)) {
    return(none)
  }
  components <- existing$components
  type_of <- component_types$name[match(components$component_type_id, component_types$component_type_id)]
  texts <- if (is.null(component_types)) {
    none$texts
  } else {
    texts <- data.frame(component_type = as.character(type_of), text = as.character(components$text), stringsAsFactors = FALSE)
    texts[!is.na(texts$text) & !is.na(texts$component_type), , drop = FALSE]
  }
  variables <- existing$variables
  question_text <- components$text[match(variables$component_id, components$component_id)]
  matr_ids <- data.frame(
    text = as.character(question_text), matr_id = as.character(variables$matr_id),
    display_order = as.numeric(variables$display_order), stringsAsFactors = FALSE
  )
  list(texts = texts, matr_ids = matr_ids[!is.na(matr_ids$text), , drop = FALSE])
}

#' A draft's PDF, on this server, to show or read.
#'
#' Fetched from Storage first if this server hasn't got it (a hosted site loses its disk when it
#' restarts, and its servers don't share one).
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record An extraction record.
#' @return The record's `pdf` with `path` where the file is on this server, or `NULL` if the draft has
#'   no PDF or it can't be had (Storage can't be reached, or never had it). Never an error.
extraction_service_pdf <- function(connection, record) {
  path <- tryCatch(extraction_model_local_pdf(connection, record$pdf$sha256), error = function(problem) NULL)
  if (!is.null(path)) utils::modifyList(record$pdf, list(path = path))
}

# ------ Helpers ----------------------------------------------------------------------

# Every change re-reads the record first, and saves only the fields it
# changed, so it never undoes what someone else saved since this page read it.
# `me` is who saves it. `new_revision` is FALSE for a change that isn't to what a
# reviewer would read (restarting a reading, discarding). `expected_revision`, if given, is the
# revision the person was looking at: the save is refused if the draft has moved on. Returns the
# record as saved.
change_record <- function(connection, id, me, change, new_revision = TRUE, expected_revision = NULL) {
  before <- extraction_model_get(connection, id)
  after <- change(before)
  fields <- union(names(before), names(after))
  changed <- fields[!vapply(fields, function(field) identical(before[[field]], after[[field]]), logical(1))]
  extraction_model_save(connection, after, changed, me$staff_id, expected_revision = expected_revision, new_revision = new_revision)
}

restart_reading <- function(record) {
  record$extraction_status <- "queued"
  record$reading_started <- format_time(Sys.time())
  record["error"] <- list(NULL)
  record["reading_pid"] <- list(NULL)
  record["reading_claimed_at"] <- list(NULL)
  record
}

store_pdf <- function(connection, upload) {
  if (!extraction_model_is_pdf(upload$datapath)) {
    stop(upload$name, " isn't a PDF. Choose the questionnaire's PDF file.", call. = FALSE)
  }
  sha256 <- extraction_model_sha256(upload$datapath)
  list(
    name = upload$name,
    path = extraction_model_save_pdf(connection, upload$datapath, sha256),
    pages = extraction_model_page_count(upload$datapath),
    bytes = upload$size,
    sha256 = sha256
  )
}
