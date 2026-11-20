# ------ Pushing an extraction into the database --------------------------------------------
# An extraction becomes rows of the CARA metadata database, in a recorded questionnaire's wave that
# asks no questions yet: its blocks as components placed in the questionnaire, its questions and
# their numbers in the wave, each question's variables with their choices, and the subjects, time
# frames and units as tags. The database does all of it in one function, push_extraction()
# (sql/4_functions.sql), from the questionnaire as saved and its Basic Info (`info`): what is there
# already (a wording, an answer choice, a tag, a placement of an earlier wave, a variable with the
# same Variable ID) is used again, the rest is added, and the extraction is marked Pushed. The function
# answers with counts of what it did (`component_new`, `component_reused`, `placement_new`,
# `wave_question_new`, `variable_new`, `variable_reused`, `answer_option_new`, `tag_new`... and
# `questions`), and refuses with a sentence when it can't: a wave that asks questions already
# (Empty it first on the Metadata page), no questions, Basic Info missing.
#
# The push is previewed and done on the Edit Extraction page. The preview is the same function told
# only to try (`p_dry_run`): it does everything and undoes it, so the page knows whether the push would
# be refused, and, from the names that come back with the counts, what in the questionnaire is new to the
# database (its NEW marks).

# ------ Before pushing -----------------------------------------------------------------
# What stops an extraction being pushed because of its questionnaire.
# draft: The questionnaire, as on the Edit Extraction page.
# Returns: A character vector of sentences, in the order to do them; empty when it can be pushed.
extraction_service_draft_blockers <- function(draft, lookups) {
  n_blocks_to_fix <- length(unique(draft_service_check(draft, lookups)$id)) # a blank Variable ID is among them
  as.character(c(
    if (nrow(draft$components) == 0) "Add the questionnaire's blocks and questions first.",
    if (nrow(draft$components) > 0 && !"Question" %in% draft$components$component_type) "Add at least one question.",
    if (n_blocks_to_fix > 0) paste0("Fix the ", plural(n_blocks_to_fix, "block"), " marked to fix.")
  ))
}

# What stops an extraction being pushed because of its Basic Info.
# info: Where it goes, as `extraction_service_info()` returns it; the record's by default.
# records: All the extraction records, to find other drafts of the same questionnaire and wave.
# question_counts: How many questions each of the questionnaire's waves asks, for a recorded
#   wave (`questionnaire_service_wave_question_counts()`).
# Returns: A character vector of sentences; empty when the Basic Info is complete and fits.
extraction_service_info_blockers <- function(record, info = extraction_service_info(record), catalog,
                                             records = list(), question_counts = integer(), me = NULL) {
  missing <- extraction_service_missing(info)
  problem <- extraction_service_recorded_wave_problem(info, catalog, question_counts, records, record$id, me)
  as.character(c(
    if (length(missing) > 0) paste0("Fill in the ", word_list(missing), " under Basic Info."),
    problem
  ))
}

# What an extraction's questionnaire holds, in a sentence.
# draft: The questionnaire.
# Returns: A string, e.g. `"12 blocks, with 30 questions and 45 variables."`.
extraction_service_draft_summary <- function(draft) {
  questions <- sum(draft$components$component_type %in% "Question")
  paste0(
    plural(nrow(draft$components), "block", big_mark = ","), ", with ",
    plural(questions, "question", big_mark = ","), " and ",
    plural(nrow(draft$variables), "variable", big_mark = ","), "."
  )
}

# How many blocks still haven't been marked as checked.
# Not a reason to stop: it is said before the push.
# draft: The questionnaire as saved.
extraction_service_unchecked <- function(draft) {
  sum(!draft$components$checked %in% TRUE)
}

# ------ The push and its preview ------------------------------------------------------------
# Push an extraction into the CARA metadata database.
# The questionnaire has to be saved first (the page saves it); what is pushed is the saved copy, into
# the wave its Basic Info names. Afterwards the extraction is Pushed and leaves the Drafts page.
# me: Who is pushing: `list(staff_id, name, role)`.
# catalog: The live tables the pages pick from, to name the extraction in its summary.
# Returns: A list: `counts` (what was done; see the top of this file) and `record` (as it is now,
#   Pushed). An error with the database's sentence if it refused.
extraction_service_push <- function(connection, record, me, catalog) {
  record <- extraction_service_save_info(connection, record, extraction_service_info(record), me, catalog)
  counts <- database_model_call(connection, "push_extraction", list(
    p_extraction_id = database_number(record$id), p_staff_id = database_number(me$staff_id)
  ))
  list(counts = counts, record = extraction_model_get(connection, record$id))
}

# What pushing an extraction would do, without doing it.
# The database tries the push and undoes it, so the counts are the push's own.
# me: Who asks: `list(staff_id, name, role)`.
# Returns: The counts, as `extraction_service_push()` returns them. An error with the database's
#   sentence if the push would be refused.
extraction_service_push_preview <- function(connection, record, me) {
  database_model_call(connection, "push_extraction", list(
    p_extraction_id = database_number(record$id), p_staff_id = database_number(me$staff_id), p_dry_run = TRUE
  ))
}

# Save an extraction's Basic Info: where it goes in the database, and its summary (what it is called).
# record: The record as the person read it.
# me: Who is saving: `list(staff_id, name, role)`.
# Returns: The record as saved. An error with a sentence if the database refused, e.g. because the
#   draft was changed after the person opened it.
extraction_service_save_info <- function(connection, record, info, me, catalog) {
  placed <- utils::modifyList(record, list(info = info), keep.null = TRUE)
  placed$summary <- extraction_service_title(placed, catalog)
  changed <- c(if (!identical(extraction_service_info(record), info)) "info", if (!identical(record$summary, placed$summary)) "summary")
  if (length(changed) == 0) {
    return(record)
  }
  extraction_model_save(connection, placed, changed, me$staff_id, expected_revision = record$revision)
}

# ------ Wordings other places print too ---------------------------------------------------------------
# The blocks of a reopened draft that staff reworded while other places print the same wording: each needs a
# choice before it is pushed, since a wording is kept once (a component) and shared by every place that prints
# it. Only here (the default) gives this place a wording of its own and leaves the others as they are;
# everywhere changes the component itself, so every questionnaire that prints it shows the new words.
# A block printed nowhere else needs no choice (its component just takes the new words), nor does one whose kind
# changed (it is another block now).
# words: What the catalog has for the draft's components: `components` and `places`, as
#   `component_model_places()` returns them.
# catalog: The live tables, for the other questionnaires' names.
# questionnaire_id: The questionnaire the draft goes into, whose own other places are counted, not named.
# Returns: A data frame, in printed order: `id` (the block), `kind`, `was` and `now` (the words), `everywhere`
#   (whether staff chose that) and `elsewhere` (where else it is printed, in words).
extraction_service_rewordings <- function(draft, words, catalog, questionnaire_id) {
  blocks <- draft$components[match(draft_service_printed_order(draft), draft$components$id), , drop = FALSE]
  known <- match(blocks$catalog_id, words$components$component_id)
  was <- words$components$text[known]
  reworded <- !is.na(known) & blocks$component_type == words$components$kind[known] &
    !is.na(blocks$text) & squish(normal_text(blocks$text)) != squish(normal_text(was))
  # one line for each wording: the first block of the draft that has it
  reworded <- reworded & !duplicated(paste(blocks$catalog_id, blocks$text))
  elsewhere <- vapply(blocks$catalog_id, function(component) {
    printed_by <- words$places$questionnaire_id[words$places$component_id %in% component]
    here <- sum(printed_by %in% questionnaire_id) - 1L
    others <- catalog$questionnaires$name[match(unique(printed_by[!printed_by %in% questionnaire_id]), as.character(catalog$questionnaires$questionnaire_id))]
    others[is.na(others)] <- "another questionnaire"
    word_list(c(
      if (here > 0) paste(plural(here, "other place"), "in this questionnaire"),
      sort(unique(others))
    ))
  }, character(1))
  keep <- reworded & nzchar(elsewhere)
  data.frame(
    id = blocks$id[keep], kind = blocks$component_type[keep], was = was[keep], now = blocks$text[keep],
    everywhere = blocks$reword[keep] %in% "everywhere", elsewhere = unname(elsewhere[keep])
  )
}

# The wordings to choose for, read from the database for the draft on the page.
# record: The extraction's record: only a draft reopened from a wave has any.
# Returns: As `extraction_service_rewordings()`; no rows for a draft that wasn't reopened.
extraction_service_read_rewordings <- function(connection, record, draft, catalog) {
  ids <- draft$components$catalog_id
  words <- if (any(!is.na(ids))) component_model_places(connection, ids) else list(
    components = no_rows(c("component_id", "kind", "text")), places = no_rows(c("component_id", "questionnaire_id"))
  )
  extraction_service_rewordings(draft, words, catalog, extraction_service_info(record)$questionnaire_id)
}

# ------ The preview -------------------------------------------------------------------------------
# The cards for where an extraction goes: its registry, study, questionnaire and wave, all recorded.
# Returns: A list of four cards, each a list: `kind`, `title`, `subtitle`, `facts` (short notes) and
#   `is_new` or `is_missing` (one not picked yet says so).
extraction_service_where_cards <- function(info, catalog) {
  not_picked <- function(kind) list(kind = kind, title = "Not picked yet", subtitle = "", facts = character(), is_missing = TRUE)
  registry <- catalog$registries[as.character(catalog$registries$registry_id) %in% info$registry_id, , drop = FALSE]
  study <- questionnaire_service_get_study(catalog, info$study_id)
  questionnaire <- questionnaire_service_get(catalog, info$questionnaire_id)
  wave <- catalog$waves[as.character(catalog$waves$wave_id) %in% info$wave_id, , drop = FALSE]
  list(
    if (nrow(registry) == 1) {
      list(
        kind = "Registry", title = name_with_abbreviation(registry$name, registry$abbreviation %||% ""),
        subtitle = "Already in the database", facts = character(), is_new = FALSE
      )
    } else {
      not_picked("Registry")
    },
    if (!is.null(study)) {
      list(
        kind = "Study", title = study$name,
        subtitle = if (!is.na(study$principal_investigator)) paste("Led by", study$principal_investigator) else "Already in the database",
        facts = character(), is_new = FALSE
      )
    } else {
      not_picked("Study")
    },
    if (!is.null(questionnaire)) {
      list(
        kind = "Questionnaire", title = name_with_abbreviation(questionnaire$name, questionnaire$abbreviation),
        subtitle = paste0("For ", questionnaire$respondent_type, " · already in the database"),
        facts = plural(questionnaire$n_waves, "wave"), is_new = FALSE
      )
    } else {
      not_picked("Questionnaire")
    },
    if (nrow(wave) == 1) {
      list(
        kind = "Wave", title = wave$name,
        subtitle = "Already in the database, with no questions yet",
        facts = c(
          if (!is.na(wave$administered_date)) paste("Given", format(as.Date(wave$administered_date), "%m/%d/%Y")),
          if (!is.na(wave$respondent_count)) paste(format(wave$respondent_count, big.mark = ","), "respondents")
        ),
        is_new = FALSE
      )
    } else {
      not_picked("Wave")
    }
  )
}

# ------ What is new, row by row ----------------------------------------------------------------------
# Which of the questionnaire's question wordings, option sets, answer choices and tags (subjects, time frames and
# units) a push adds, for the NEW marks in the Push dialog's table. The dry run names them (`counts$names`, from
# push_run() in sql/4_functions.sql): the answer choices, option sets and tags it adds by name, and the question
# wordings the database has already (a question wording it doesn't name is new). They are named as the database
# keeps them, so the questionnaire's words are put the same way before they are looked up: a question's words
# with their spaces squished, a tag also in lower case with straight quotes (tags are matched ignoring case), and
# an option set as its choices, whatever their order ("yes | no" is "no | yes").
# The dry run doesn't name the blocks that aren't questions (an instruction, a definition, a note), so those the
# catalog has are read apart (`component_model_other_blocks()`, before the push, since the dry run is undone): a
# block is there already when the catalog has one of the same kind and words, as the push matches them.
# counts: From `extraction_service_push_preview()`, or an error it raised, or `NULL`.
# blocks: The catalog's other blocks (`kind` and `text`), or `NULL` if they couldn't be read.
# Returns: `NULL` when there is nothing to go by (no dry run, or one that was refused), else a list:
#   `question_found` (the wordings there already), `answer_new`, `option_set_new` (each set's key, as
#   `extraction_service_option_set_key()` makes it), `tag_new` and `block_found` (the other blocks there already,
#   as `extraction_service_block_key()` makes them; `NULL` when they couldn't be read).
extraction_service_known <- function(counts, blocks = NULL) {
  if (!is.list(counts) || inherits(counts, "error") || is.null(counts$names)) {
    return(NULL)
  }
  named <- function(key) as.character(unlist(counts$names[[key]]))
  list(
    question_found = squish(named("question_reused")),
    answer_new = named("answer_option_new"),
    option_set_new = vapply(strsplit(named("option_set_new"), " | ", fixed = TRUE), extraction_service_option_set_key, character(1)),
    tag_new = extraction_service_tag_key(named("tag_new")),
    block_found = if (!is.null(blocks)) extraction_service_block_key(blocks$kind, blocks$text)
  )
}

# A block that isn't a question by its kind and words, as the push matches them (its words with their spaces
# squished).
extraction_service_block_key <- function(kind, text) {
  paste(as.character(kind), squish(as.character(text)), sep = "\r")
}

# An option set by its choices, whatever their order: one string for the same choices.
extraction_service_option_set_key <- function(choices) {
  paste(sort(unique(as.character(choices))), collapse = "\r")
}

# A tag's name as tags are matched: straight quotes, one space between words, lower case.
extraction_service_tag_key <- function(names) {
  tolower(squish(straight_quotes(as.character(names))))
}

# What a push comes to, for `extraction_request_ui()`: whether the database would refuse it. What is new, row
# by row, is marked in the questionnaire's table (`extraction_service_known()`). There is no sentence of counts:
# they are rows, not what changes (a wave reopened and pushed again has every question, variable and choice
# taken out and put back, so they all count, however few staff changed).
# counts: From `extraction_service_push_preview()`, or an error it raised, or `NULL` when it wasn't tried
#   (the Basic Info isn't complete).
# Returns: A list: `where` (a list of cards), `problem` (why the push would be refused, or `NULL`) and
#   `unchecked`.
extraction_service_push_preview_ui <- function(draft, info, catalog, counts = NULL) {
  list(
    where = extraction_service_where_cards(info, catalog),
    problem = if (inherits(counts, "error")) conditionMessage(counts),
    unchecked = extraction_service_unchecked(draft)
  )
}
