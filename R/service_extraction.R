# ------ Extraction drafts --------------------------------------------------------
# An extraction starts one of two ways. AI-Powered: staff upload a questionnaire PDF, and the
# reading job (Gemini; service_reading.R) moves `extraction_status` along
#   queued -> sent -> received -> [assigning] -> ready      or stops at failed
# and saves the questionnaire it read. Manual: staff type the questionnaire themselves, with or
# without a PDF to look at, and there is no reading. Either way staff edit a copy on the Edit
# Extraction page (see draft_service_load()), say where it goes (its Basic Info) and push it into the
# MATR metadata database from there (service_extraction_push.R). `status` is Draft until then, and
# Pushed after: a pushed extraction is kept whole, with its PDF and reading (what the AI read and how
# staff changed it: the AI Monitor reads those), and leaves the Drafts page. A draft that is discarded
# is deleted.
# `info` is where it goes in the database (its Basic Info): a recorded registry, study,
# questionnaire and one of that questionnaire's waves that asks no questions yet, each saved as
# its ID (text). It is picked on the Edit Extraction page before the push, so a draft can have none;
# anything that isn't recorded yet is added from the form's drop-downs first.
# Records are kept in the database (model_extraction.R); `connection` is the one made in global.R.

# Where an extraction goes: the IDs of a recorded registry, study, questionnaire and wave.
extraction_info_fields <- c("registry_id", "study_id", "questionnaire_id", "wave_id")

# Every extraction draft, newest saved first.
# Returns: A list of records named by id; see `extraction_model_get_all()`.
extraction_service_get_all <- function(connection) {
  extraction_model_get_all(connection)
}

# Whether a draft this staff member started is being read.
# While one is, the site reads the drafts again every few seconds, so they see how far the reading job
# has got; otherwise it only reads them when asked (a page opened, Refresh).
# records: The extraction records, from `extraction_service_get_all()`.
extraction_service_is_reading <- function(records, me) {
  mine <- extraction_service_open_drafts(records, prepared_by_id = me$staff_id)
  any(vapply(mine, function(record) isTRUE(record$extraction_status %in% reading_statuses), logical(1)))
}

# The drafts still being prepared: not pushed yet.
# records: A list of extraction records.
# prepared_by_id: Only those this staff member started, if given.
extraction_service_open_drafts <- function(records, prepared_by_id = NULL) {
  Filter(function(record) {
    identical(record$status, "Draft") &&
      (is.null(prepared_by_id) || same_staff(record$prepared_by_id, prepared_by_id))
  }, records)
}

# A record's info with every field present.
# Only the four picks are read: what drafts saved before the form was cut down to them also hold (a
# new questionnaire's or new wave's name, and the like) is left out, so such a draft asks for the
# questionnaire or wave to be picked again.
# record: An extraction record, as `extraction_model_get_all()` returns it. `NULL` for an empty form.
# Returns: A named list of strings, one per field in `extraction_info_fields`, blanks as `""`.
extraction_service_info <- function(record = NULL) {
  info <- lapply(extraction_info_fields, function(field) {
    value <- record$info[[field]]
    if (is.null(value)) "" else as.character(value)
  })
  stats::setNames(info, extraction_info_fields)
}

# An extraction's Basic Info, as the form holds it.
# The form picks a recorded registry, study, questionnaire and wave (one that asks no questions yet);
# anything new is added from its drop-down first, so its "+ Add new …" choice isn't a pick.
# info: Where it goes, as `extraction_service_info()` returns it, or the form's picks.
# Returns: `info`, in the same shape, with "+ Add new …" as blank.
extraction_service_basic_info <- function(info) {
  basic <- lapply(extraction_info_fields, function(field) {
    value <- as.character(info[[field]] %||% "")
    if (identical(value, add_new_choice)) "" else value
  })
  stats::setNames(basic, extraction_info_fields)
}

# Fill in the study and registry a recorded questionnaire belongs to.
# A recorded questionnaire says which study and registry it belongs to, so they are
# filled in from it when blank (as in drafts saved before the form asked for them), and
# the questionnaire stays among the drop-down's choices.
# Returns: `info`, with `study_id` and `registry_id` filled in where they were blank.
extraction_service_place <- function(info, catalog) {
  questionnaire <- questionnaire_service_get(catalog, info$questionnaire_id)
  if (!is.null(questionnaire) && !nzchar(info$study_id)) {
    info$study_id <- as.character(questionnaire$study_id)
  }
  study <- questionnaire_service_get_study(catalog, info$study_id)
  if (!is.null(study) && !nzchar(info$registry_id)) info$registry_id <- as.character(study$registry_id)
  info
}

# Whether the questionnaire is there to review.
# Returns: `TRUE` when the reading job has finished the draft, and for an extraction typed by
#   hand, which has nothing to wait for.
extraction_service_is_read <- function(record) {
  identical(record$method, extraction_manual) || identical(record$extraction_status, "ready")
}

# ------ Changing a draft -----------------------------------------------------------
# Start an AI-Powered draft for an uploaded PDF.
# Reading starts at once. A file that isn't a PDF is turned away with a sentence.
# upload: One row of a `fileInput`'s value: `name`, `size`, `datapath`.
# staff: Who is preparing it: `list(staff_id, name, role)`.
# supplement: The optional file of MATR IDs and codes, as `upload`; or `NULL`.
extraction_service_create <- function(connection, upload, staff,
                                      info = extraction_service_info(), now = Sys.time(), supplement = NULL) {
  pdf <- store_pdf(connection, upload)
  stored <- if (!is.null(supplement)) store_supplement(connection, supplement)
  extraction_model_create(connection, new_ai_record(staff, pdf, stored, info, now))
}

# An AI-Powered draft that starts being read at once: its PDF (and file of Variable Names and Coding) kept already.
new_ai_record <- function(staff, pdf, supplement, info, now) {
  list(
    status = "Draft",
    method = extraction_ai,
    prepared_by = staff$name,
    prepared_by_id = staff$staff_id,
    started = format_time(now),
    saved = format_time(now),
    pdf = pdf,
    extraction_status = "queued",
    reading_started = format_time(now),
    reading_claimed_at = NULL,
    reading_pid = NULL,
    error = NULL,
    info = info,
    matr_csv = NULL,
    supplement = supplement
  )
}

# Re-extract a wave (Make an edit): empty it, then start a new AI-Powered extraction of it from the files it
# was extracted from (its PDF, and the Variable Names and Coding if it had them, kept with the wave), with the
# extraction prompt as it is now. The new draft goes into the same registry, study, questionnaire and wave,
# which asks nothing now, and is reviewed and pushed like any other. The emptying is written to the history.
# wave_id: The wave.
# place: Where the wave is: `registry_id`, `study_id` and `questionnaire_id`.
# me: Who starts it: `list(staff_id, name, role)`.
# Returns: The new draft's record, being read. An error with a sentence if the wave has no PDF to read again
#   (then nothing is emptied).
extraction_service_reextract_wave <- function(connection, wave_id, place, me, now = Sys.time()) {
  file <- extraction_model_wave_files(connection, wave_id)
  if (is.null(file) || is.na(file$pdf_sha256 %||% NA)) {
    stop("This wave wasn't extracted from a PDF on this site, so there is nothing to read again. Start a New Extraction with its PDF instead.", call. = FALSE)
  }
  path <- tryCatch(extraction_model_local_pdf(connection, file$pdf_sha256), error = function(problem) NULL)
  if (is.null(path)) stop("The wave's PDF (", file$pdf_name, ") couldn't be fetched from Storage. Start a New Extraction with it instead.", call. = FALSE)
  pdf <- list(
    name = file$pdf_name, path = path, pages = extraction_model_page_count(path), bytes = file.size(path),
    sha256 = file$pdf_sha256
  )
  supplement <- if (!is.na(file$supplement_sha256 %||% NA)) {
    coding <- tryCatch(extraction_model_local_pdf(connection, file$supplement_sha256), error = function(problem) NULL)
    if (is.null(coding)) stop("The wave's Variable Names and Coding (", file$supplement_name, ") couldn't be fetched from Storage.", call. = FALSE)
    list(name = file$supplement_name, sha256 = file$supplement_sha256, bytes = file.size(coding))
  } else if (!is.na(file$supplement_text %||% NA)) {
    list(name = file$supplement_name, text = file$supplement_text)
  }
  database_model_call(connection, "empty_wave", list(p_wave_id = database_number(wave_id), p_staff_id = database_number(me$staff_id)))
  info <- extraction_service_info(list(info = list(
    registry_id = place$registry_id, study_id = place$study_id, questionnaire_id = place$questionnaire_id,
    wave_id = as.character(wave_id)
  )))
  extraction_model_create(connection, new_ai_record(me, pdf, supplement, info, now))
}

# Start a Manual draft: the questionnaire is typed by hand.
# There is no reading. The PDF is optional: if staff give one, it is kept with the draft so the
# Edit Extraction page can show it beside what they type. The questionnaire starts empty.
# staff: Who is preparing it: `list(staff_id, name, role)`.
# upload: One row of a `fileInput`'s value: `name`, `size`, `datapath`; or `NULL` for none.
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

# Read a draft again.
# me: Who asks: `list(staff_id, name, role)`.
# Returns: The record as saved.
extraction_service_retry <- function(connection, id, me) {
  change_record(connection, id, me, restart_reading)
}

# Read an AI-Powered draft's PDF again from the start, with the extraction prompt as it is now (and its Variable
# Names and Coding again, if it has the file): Re-extract on the Edit Extraction page. The questionnaire staff
# reviewed is put aside for the new reading, with what they changed and marked as checked. Not if someone else
# saved it after `expected_revision`.
# me: Who asks: `list(staff_id, name, role)`.
# expected_revision: The revision the person was looking at, or `NULL` not to check.
# Returns: The record as saved, being read. An error with a sentence if it can't be read again.
extraction_service_reextract <- function(connection, id, me, expected_revision = NULL) {
  record <- extraction_model_get(connection, id)
  if (!identical(record$method, extraction_ai)) {
    stop("Only an AI-Powered extraction is read from its PDF. This one was typed by hand.", call. = FALSE)
  }
  if (is.null(extraction_service_pdf(connection, record))) {
    stop("Its PDF isn't on this site any more, so it can't be read again.", call. = FALSE)
  }
  extraction_model_clear_questionnaire(connection, id, me$staff_id, expected_revision)
  change_record(connection, id, me, restart_reading)
}

# The reading job saying how far it has got.
# If reading has started again since the job began ("Try reading again"),
# its news is stale and nothing is saved.
# changes: A named list of record fields to their new values.
# Returns: Whether it was saved.
extraction_service_reading_update <- function(connection, id, reading_started, changes) {
  record <- extraction_model_get(connection, id)
  if (!identical(record$reading_started, reading_started)) {
    return(FALSE)
  }
  extraction_model_save_reading(connection, id, changes, reading_started)
}

# Discard a draft: it is deleted from the database, with its PDF when nothing else uses the file.
# Only a draft: a pushed extraction is kept.
# me: Who discards it: `list(staff_id, name, role)`.
# Returns: Called for its effect. An error with a sentence if the database refused.
extraction_service_discard <- function(connection, id, me) {
  record <- extraction_model_get(connection, id)
  extraction_model_discard(connection, id)
  extraction_service_forget_pdf(connection, record)
  invisible()
}

# Remove a discarded draft's PDF from Storage when no other extraction, and no wave, uses the file.
# The draft is already gone, so a file that can't be removed only stays behind in Storage: that is a
# warning in the log, not an error.
# record: The draft's record, as it was.
extraction_service_forget_pdf <- function(connection, record) {
  for (sha256 in c(record$pdf$sha256, record$supplement$sha256)) {
    tryCatch(
      extraction_model_forget_pdf(connection, sha256),
      error = function(problem) warning("The PDF ", sha256, " stays in Storage: ", conditionMessage(problem), call. = FALSE)
    )
  }
  invisible()
}

# ------ The file of MATR IDs and codes --------------------------------------------------
# An AI-Powered draft can have a second, optional file: a codebook or list that gives each variable its
# MATR ID and each choice its code. Once Gemini has read the questionnaire, it reads this file in a
# separate request and fills them in (reading_service_assign()). A PDF is kept like the questionnaire's
# (Storage and this server, by its SHA-256); a CSV or text file is kept as its text, on the draft.

# What the file can be, by its name's ending: a PDF, or text.
supplement_text_types <- c("csv", "tsv", "txt")

# A draft's file of MATR IDs and codes, ready to send to Gemini.
# Returns: `list(name, path)` for a PDF (on this server, fetched from Storage if need be), `list(name, text)`
#   for text, or `NULL` for none. An error with a sentence if the PDF can't be found.
extraction_service_supplement_file <- function(connection, id) {
  supplement <- extraction_model_supplement(connection, id)
  if (is.null(supplement)) {
    return(NULL)
  }
  if (!is.null(supplement$text)) {
    return(list(name = supplement$name, text = supplement$text))
  }
  path <- extraction_model_local_pdf(connection, supplement$sha256)
  if (is.null(path)) stop("The file of Variable Names and Coding (", supplement$name, ") isn't on this site any more.", call. = FALSE)
  list(name = supplement$name, path = path)
}

# ------ The questionnaire it read ----------------------------------------------------
# A draft's questionnaire.
# The reviewed copy if staff have saved one, else what the reading job returned.
# Returns: A draft; see `draft_service_load()`.
extraction_service_load_draft <- function(connection, record) {
  draft_service_load(connection, record$id)
}

# A draft's questionnaire, for the Edit Extraction page, with what the AI returned to compare it with.
# Both are read at once. What the AI returned is read only when the draft opens, since it never changes.
# Returns: A list: `draft` (see `extraction_service_load_draft()`) and `original`, what the AI returned
#   as a draft (`NULL` for a Manual extraction). An error if reading hasn't finished.
extraction_service_open_draft <- function(connection, record) {
  json <- extraction_model_questionnaire(connection, record$id)
  if (is.null(json$questionnaire %||% json$original)) stop("This draft hasn't been read yet.", call. = FALSE)
  list(
    draft = draft_service_from_json(json$questionnaire %||% json$original),
    original = if (identical(record$method, extraction_ai) && !is.null(json$original)) draft_service_from_json(json$original)
  )
}

# Save staff's reviewed copy of the questionnaire.
# For an AI-Powered extraction it also keeps how the copy differs from what the AI returned (see
# `draft_service_review_edits()`), in place of what was kept before; the AI's answer is never touched.
# record: The extraction draft's record.
# draft: A draft, as the Edit Extraction page edits it.
# original: What the AI returned, from `extraction_service_open_draft()`; `NULL` for a Manual
#   extraction, which keeps no differences.
# me: Who is saving: `list(staff_id, name, role)`.
# expected_revision: The revision the page opened (or last saved): the save is refused if
#   someone else saved the draft since, so nobody's work is overwritten. `NULL` skips the check.
# Returns: The draft's revision after the save, invisibly. An error with a sentence if it was refused.
extraction_service_save_draft <- function(connection, record, draft, original, me, expected_revision = NULL) {
  edits <- if (!is.null(original)) draft_service_review_edits(original, draft)
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

# How far reading the PDF has got, as the Extraction page lists it.
# Returns: A data frame with `label`, `detail` and `state` (`done`, `in_progress`,
#   `not_started` or `attention`), one row per step.
extraction_service_reading_steps <- function(record) {
  status <- record$extraction_status
  steps <- reading_steps
  # A file of Variable Names and Coding is a step of its own, once the questionnaire is read and checked.
  coding <- !is.null(record$supplement)
  if (coding) {
    steps <- rbind(steps[1:4, ], data.frame(
      label = "Applying Variable Names and Coding",
      detail = paste("Applies the Variable Names and Coding in", record$supplement$name)
    ), steps[5, ], make.row.names = FALSE)
  }
  n_done <- switch(status,
    queued = 1,
    sent = 2,
    received = 3,
    assigning = 4,
    ready = nrow(steps),
    failed = 2
  )
  steps$state <- ifelse(seq_len(nrow(steps)) <= n_done, "done", "not_started")
  if (n_done < nrow(steps)) {
    steps$state[n_done + 1] <- if (status == "failed") "attention" else "in_progress"
  }
  if (status == "queued") steps$detail[2] <- "Waiting to start"
  if (status == "failed") steps$detail[3] <- record$error %||% "Gemini stopped before the end."
  # read, but the Variable Names and Coding couldn't be applied: the draft is ready without them
  if (status == "ready" && coding && !is.null(record$error)) {
    steps$state[5] <- "attention"
    steps$detail[5] <- paste0("Couldn't apply them (", record$error, "). Type the MATR IDs on the Edit Extraction page.")
  }
  steps
}

# The steps on each Extraction Drafts card.
# How it was read (or that it was typed by hand), and how much of it has been checked on the Edit
# Extraction page, as the page's own "N% human-reviewed" counts it.
# draft: The questionnaire it read, or `NULL` until reading has finished.
# Returns: A data frame with `label` and `state`, one row per step.
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
  # once read, how many pages were: "8 pages read by Gemini"
  read <- if (is.null(record$pdf$pages)) "Read by Gemini" else plural(record$pdf$pages, "page", "read by Gemini")
  switch(record$extraction_status,
    # read, but the Variable Names and Coding couldn't be applied (extraction_service_reading_steps() says why)
    ready = if (is.null(record$error)) progress_step(read, "done") else progress_step(paste(read, "· codes not applied"), "attention"),
    failed = progress_step("Read by Gemini · failed", "attention"),
    progress_step("Read by Gemini", "in_progress")
  )
}

structure_progress <- function(draft) {
  if (is.null(draft)) {
    return(progress_step("0% human-reviewed", "not_started")) # while it is being read
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
# A card's title: where it goes, once that is picked; until then the PDF's file name.
# The name the AI read off the PDF isn't used: it is often a heading inside the questionnaire.
# Returns: A string such as `"Adult Health and Personality (AHP) · Wave 3"`.
extraction_service_title <- function(record, catalog) {
  info <- extraction_service_info(record)
  questionnaire <- questionnaire_service_get(catalog, info$questionnaire_id) # NULL if none is picked yet
  wave <- extraction_service_wave_name(info, catalog)
  if (is.null(questionnaire) && !nzchar(wave)) {
    return(record$pdf$name %||% "Untitled questionnaire")
  }
  paste(c(
    if (!is.null(questionnaire)) name_with_abbreviation(questionnaire$name, questionnaire$abbreviation) else "Unnamed questionnaire",
    if (nzchar(wave)) wave
  ), collapse = " · ")
}

# The recorded wave's name; "" if none is picked yet.
extraction_service_wave_name <- function(info, catalog) {
  wave <- catalog$waves[as.character(catalog$waves$wave_id) %in% info$wave_id, , drop = FALSE]
  if (nrow(wave) == 1) trimws(wave$name) else ""
}

# The Edit Extraction page's heading.
# Until staff say where it goes, it goes by the PDF's file name (the name the AI read off
# the PDF is often a heading inside it), or "Unnamed questionnaire" for one typed by hand without a PDF.
# Returns: A list: `title` (e.g. `"AHP · Wave 3"`) and `subtitle` (the full name, or `""`).
extraction_service_heading <- function(record, catalog) {
  info <- extraction_service_info(record)
  questionnaire <- questionnaire_service_get(catalog, info$questionnaire_id) # NULL if none is picked yet
  name <- trimws(questionnaire$name %||% "")
  abbreviation <- trimws(questionnaire$abbreviation %||% "")
  short_name <- if (nzchar(abbreviation)) abbreviation else if (nzchar(name)) name else record$pdf$name %||% "Unnamed questionnaire"
  wave <- extraction_service_wave_name(info, catalog)
  list(
    title = paste(c(short_name, if (nzchar(wave)) wave), collapse = " · "),
    subtitle = if (nzchar(abbreviation)) name else ""
  )
}

# ------ Checks on what staff picked ---------------------------------------------------
# The Basic Info fields still blank.
# The form goes registry, study, questionnaire, then one of the questionnaire's waves.
# Returns: The names of the fields, as sentences call them; empty if none.
extraction_service_missing <- function(info) {
  unname(extraction_required_fields[extraction_service_missing_fields(info)])
}

# The Basic Info fields still blank, by their names in `extraction_info_fields`.
# Returns: A character vector of field names, e.g. `c("registry_id", "wave_id")`.
extraction_service_missing_fields <- function(info) {
  blank <- !vapply(names(extraction_required_fields), function(field) has_text(info[[field]]), logical(1))
  names(extraction_required_fields)[blank]
}

# Every Basic Info field is needed, named by field, with what each is called in sentences.
extraction_required_fields <- c(
  registry_id = "registry", study_id = "study", questionnaire_id = "questionnaire", wave_id = "wave"
)

# A problem with the recorded wave an extraction goes into, or `NULL`.
# It has to be one of the questionnaire's waves, with no questions yet (an extraction adds every
# question the wave asks, and the database refuses a push into a wave that asks any: Empty it first on the
# Metadata page), and no other draft may be going into it too.
# question_counts: How many questions each of the questionnaire's waves asks: a named integer
#   vector by wave id (`questionnaire_service_wave_question_counts()`).
# records: All the extraction records, to find other drafts going into the same wave.
# id: This draft's id, so it doesn't clash with itself.
# me: Who is using the site, `list(staff_id, name)`, so a clash with their own draft says "You".
# Returns: A sentence saying what to do, or `NULL`.
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
      "%s already asks %s. Empty the wave first (Metadata page), or pick a wave without questions.",
      wave$name, plural(n_questions, "question")
    ))
  }
  others <- Filter(function(record) {
    other <- extraction_service_info(record)
    !identical(record$id, id) && identical(other$wave_id, info$wave_id)
  }, extraction_service_open_drafts(records))
  if (length(others) > 0) {
    who <- if (draft_row_service_is_mine(others[[1]], me)) "You" else others[[1]]$prepared_by %||% "Someone"
    has <- if (identical(who, "You")) "have" else "has"
    return(sprintf("%s already %s a draft going into %s. Pick another wave, or continue that draft.", who, has, wave$name))
  }
  NULL
}

# ------ What a recorded questionnaire prints already ----------------------------------------
# The blocks a recorded questionnaire prints, and its questions' MATR IDs.
# For the questionnaire an extraction goes into, the Edit Extraction page offers the blocks it already
# prints as suggestions while a block's words are typed, and a question with the same words as one the
# questionnaire already asks keeps its variables' MATR IDs (a MATR ID names the variable within its
# questionnaire), so they are filled in instead of asked for. Until a questionnaire is picked there is
# nothing.
# Returns: A list of two data frames: `texts` (`component_type`, `text`: one row per block the questionnaire
#   prints) and `matr_ids` (`text`, `matr_id`, `display_order`: one row per variable, with its question's
#   words and its place in the question). Both have no rows until a questionnaire is picked.
extraction_service_recorded <- function(connection, record) {
  info <- extraction_service_info(record)
  if (!has_text(info$questionnaire_id)) {
    return(extraction_service_printed(NULL, NULL))
  }
  here <- list(questionnaire_id = database_eq(info$questionnaire_id))
  placements <- database_model_select(
    connection, "placement", c("placement_id", "component:component(kind,text)"), order_by = "placement_id", filters = here
  )
  variables <- database_model_get(
    connection, "variable", c("variable_key", "placement_id", "matr_id", "display_order"), order_by = "variable_key", filters = here
  )
  extraction_service_printed(placements, variables)
}

# What a recorded questionnaire prints: its blocks' words, and its variables' MATR IDs by question.
# placements: The questionnaire's placements, each with its `component` (`kind`, `text`), as
#   `extraction_service_recorded()` reads them; or `NULL` for none.
# variables: Its variables (`placement_id`, `matr_id`, `display_order`), as a data frame.
# Returns: A list of two data frames, `texts` and `matr_ids`; see `extraction_service_recorded()`.
extraction_service_printed <- function(placements, variables) {
  none <- list(
    texts = data.frame(component_type = character(), text = character()),
    matr_ids = data.frame(text = character(), matr_id = character(), display_order = numeric())
  )
  if (length(placements) == 0) {
    return(none)
  }
  placed <- data.frame(
    placement_id = vapply(placements, function(row) as.character(row$placement_id), character(1)),
    component_type = vapply(placements, function(row) as.character(row$component$kind %||% NA), character(1)),
    text = vapply(placements, function(row) as.character(row$component$text %||% NA), character(1)),
    stringsAsFactors = FALSE
  )
  texts <- unique(placed[!is.na(placed$text), c("component_type", "text"), drop = FALSE])
  rownames(texts) <- NULL
  matr_ids <- data.frame(
    text = placed$text[match(as.character(variables$placement_id), placed$placement_id)],
    matr_id = as.character(variables$matr_id),
    display_order = as.numeric(variables$display_order), stringsAsFactors = FALSE
  )
  list(texts = texts, matr_ids = matr_ids[!is.na(matr_ids$text), , drop = FALSE])
}

# A draft's PDF, on this server, to show or read.
# Fetched from Storage first if this server hasn't got it (a hosted site loses its disk when it
# restarts, and its servers don't share one).
# Returns: The record's `pdf` with `path` where the file is on this server, or `NULL` if the draft has
#   no PDF or it can't be had (Storage can't be reached, or never had it). Never an error.
extraction_service_pdf <- function(connection, record) {
  path <- tryCatch(extraction_model_local_pdf(connection, record$pdf$sha256), error = function(problem) NULL)
  if (!is.null(path)) utils::modifyList(record$pdf, list(path = path))
}

# ------ Helpers ----------------------------------------------------------------------

# Every change re-reads the record first, and saves only the fields it
# changed, so it never undoes what someone else saved since this page read it.
# `me` is who saves it. `expected_revision`, if given, is the revision the person was looking at: the
# save is refused if the draft has moved on. Returns the record as saved.
change_record <- function(connection, id, me, change, expected_revision = NULL) {
  before <- extraction_model_get(connection, id)
  after <- change(before)
  fields <- union(names(before), names(after))
  changed <- fields[!vapply(fields, function(field) identical(before[[field]], after[[field]]), logical(1))]
  extraction_model_save(connection, after, changed, me$staff_id, expected_revision = expected_revision)
}

restart_reading <- function(record) {
  record$extraction_status <- "queued"
  record$reading_started <- format_time(Sys.time())
  record["error"] <- list(NULL)
  record["reading_pid"] <- list(NULL)
  record["reading_claimed_at"] <- list(NULL)
  record
}

# A file of MATR IDs and codes: a PDF is kept like the questionnaire's, text as its text.
store_supplement <- function(connection, upload) {
  ending <- tolower(tools::file_ext(upload$name))
  if (extraction_model_is_pdf(upload$datapath)) {
    sha256 <- extraction_model_sha256(upload$datapath)
    extraction_model_save_pdf(connection, upload$datapath, sha256, extraction_model_page_count(upload$datapath))
    return(list(name = upload$name, sha256 = sha256, bytes = upload$size))
  }
  if (!ending %in% supplement_text_types) {
    stop(upload$name, " isn't a PDF, CSV or text file. Save a spreadsheet as CSV first.", call. = FALSE)
  }
  list(name = upload$name, text = extraction_model_read_csv(upload$datapath))
}

store_pdf <- function(connection, upload) {
  if (!extraction_model_is_pdf(upload$datapath)) {
    stop(upload$name, " isn't a PDF. Choose the questionnaire's PDF file.", call. = FALSE)
  }
  sha256 <- extraction_model_sha256(upload$datapath)
  pages <- extraction_model_page_count(upload$datapath)
  list(
    name = upload$name,
    path = extraction_model_save_pdf(connection, upload$datapath, sha256, pages),
    pages = pages,
    bytes = upload$size,
    sha256 = sha256
  )
}
