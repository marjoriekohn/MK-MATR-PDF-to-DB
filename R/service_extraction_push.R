# ------ Pushing an extraction into the database --------------------------------------------
# An extraction becomes rows of the MATR metadata database, in a recorded questionnaire's wave that
# asks no questions yet: its blocks as components placed in the questionnaire, its questions and
# their numbers in the wave, each question's variables with their choices, and the subjects, time
# frames and units as tags. The database does all of it in one function, push_extraction()
# (sql/4_functions.sql), from the questionnaire as saved and its Basic Info (`info`): what is there
# already (a wording, an answer choice, a tag, a placement of an earlier wave, a variable with the
# same MATR ID) is used again, the rest is added, and the extraction is marked Pushed. The function
# answers with counts of what it did (`component_new`, `component_reused`, `placement_new`,
# `wave_question_new`, `variable_new`, `variable_reused`, `answer_option_new`, `tag_new`... and
# `questions`), and refuses with a sentence when it can't: a wave that asks questions already
# (Empty it first on the Metadata page), no questions, Basic Info missing.
#
# The push is previewed and done on the Edit Extraction page. The preview is the same function told
# only to try (`p_dry_run`): it does everything and undoes it, and the counts come back, so what the
# page shows is exactly what the push will do.

# ------ Before pushing -----------------------------------------------------------------
# What stops an extraction being pushed because of its questionnaire.
# draft: The questionnaire, as on the Edit Extraction page.
# Returns: A character vector of sentences, in the order to do them; empty when it can be pushed.
extraction_service_draft_blockers <- function(draft, lookups) {
  n_blocks_to_fix <- length(unique(draft_service_check(draft, lookups)$id)) # a blank MATR ID is among them
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
# Push an extraction into the MATR metadata database.
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

# ------ Said in words ---------------------------------------------------------------------------
# What a push comes to, in words.
# For example "Adds 14 new blocks, 9 questions asked in the wave, 11 new variables, 20 answer choices
# already there and 2 new subjects, time frames or units."
# counts: From `extraction_service_push()` or `extraction_service_push_preview()`.
extraction_service_push_summary <- function(counts) {
  n <- function(key) as.integer(counts[[key]] %||% 0L)
  parts <- c(
    if (n("component_new") > 0) plural(n("component_new"), "new block"),
    if (n("component_reused") > 0) paste0(plural(n("component_reused"), "block"), " already there"),
    if (n("placement_reused") > 0) paste0(plural(n("placement_reused"), "block"), " the questionnaire prints already"),
    if (n("questions") > 0) paste(plural(n("questions"), "question"), "asked in the wave"),
    if (n("variable_new") > 0) plural(n("variable_new"), "new variable"),
    if (n("variable_reused") > 0) paste0(plural(n("variable_reused"), "variable"), " already there"),
    if (n("response_option_new") > 0) plural(n("response_option_new"), "response option"),
    if (n("option_set_new") > 0) plural(n("option_set_new"), "new option set"),
    if (n("option_set_reused") > 0) paste0(plural(n("option_set_reused"), "option set"), " already there"),
    if (n("answer_option_new") > 0) plural(n("answer_option_new"), "new answer choice"),
    if (n("answer_option_reused") > 0) paste0(plural(n("answer_option_reused"), "answer choice"), " already there"),
    if (n("tag_new") > 0) paste(plural(n("tag_new"), "new subject, time frame or unit", plural_form = "new subjects, time frames or units")),
    if (n("tag_reused") > 0) paste0(plural(n("tag_reused"), "subject, time frame or unit", plural_form = "subjects, time frames or units"), " already there"),
    if (n("measurement_scale_new") > 0) plural(n("measurement_scale_new"), "new measurement scale"),
    if (n("scale_item_new") > 0) paste0(plural(n("scale_item_new"), "question"), " put in a measurement scale")
  )
  # a draft reopened from the wave takes the place of what the wave asks
  replaces <- if (n("questions_before") > 0) paste0("Replaces the ", plural(n("questions_before"), "question"), " the wave asks now. ") else ""
  if (length(parts) == 0) {
    return(paste0(replaces, "Adds nothing."))
  }
  paste0(replaces, "Adds ", word_list(parts), ".")
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
# Returns: A list of four cards (see `request_card_ui()`); one not picked yet says so.
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

# The cards for what a push adds: the questions, option sets, answer choices and tags (subjects, time frames and
# units) the questionnaire uses, and the measurement scales its questions name, each counted once however often
# it is used, as new or as already in the database. A wording, a choice and a tag are each kept once and shared, so "7 already in the database" is seven
# tags the database has, not the places that use them. An option set is the choices one variable offers, whatever
# their order or codes ("yes | no"); it is already in the database when a variable there offers the same ones.
# Each chip lists what it counts when pointed at: the option sets by their choices, the answer choices and tags by
# name, and the questions the database has already (the new ones are the questionnaire itself, laid out
# below). Variables, the wave's questions and response options are new each time, so they are only in the
# sentence above the cards.
# counts: From `extraction_service_push_preview()`: its counts, and `names` (what the lists name).
# Returns: A list of cards (see `request_card_ui()`), each with `lists`: the names a chip stands for, by the
#   chip's words. A card is NEW when its rows are added.
extraction_service_what_cards <- function(counts) {
  n <- function(key) as.integer(counts[[key]] %||% 0L)
  count <- function(x) format(x, big.mark = ",")
  named <- function(key) as.character(unlist(counts$names[[key]]))
  card <- function(kind, what, title, list_new = TRUE) {
    n_new <- n(paste0(what, "_new"))
    n_found <- n(paste0(what, "_reused"))
    new <- if (n_new > 0) paste(count(n_new), "new")
    found <- if (n_found > 0) paste(count(n_found), "already in the database")
    lists <- list()
    if (list_new && !is.null(new) && length(named(paste0(what, "_new"))) > 0) lists[[new]] <- named(paste0(what, "_new"))
    if (!is.null(found) && length(named(paste0(what, "_reused"))) > 0) lists[[found]] <- named(paste0(what, "_reused"))
    list(
      kind = kind, title = plural(n_new + n_found, title, big_mark = ","), subtitle = "",
      facts = as.character(c(new, found)), lists = lists, is_new = n_new > 0
    )
  }
  questions <- card("Questions", "question", "different question", list_new = FALSE)
  if (n("question_new") + n("question_reused") == 0) {
    # an answer from before questions were counted apart: how many the wave asks
    questions$title <- plural(n("questions"), "question", big_mark = ",")
  }
  if (n("placement_reused") > 0) {
    questions$facts <- c(questions$facts, paste(plural(n("placement_reused"), "block", big_mark = ","), "printed by this questionnaire already"))
  }
  c(
    list(
      questions,
      card("Option sets", "option_set", "option set"),
      card("Answer choices", "answer_option", "different answer choice"),
      card("Subjects, time frames and units", "tag", "tag")
    ),
    # only when the questionnaire names a scale: most don't
    if (n("measurement_scale_new") + n("measurement_scale_reused") > 0) {
      list(card("Measurement scales", "measurement_scale", "measurement scale"))
    }
  )
}

# What a push comes to, for `extraction_request_ui()`.
# counts: From `extraction_service_push_preview()`, or an error it raised, or `NULL` when it wasn't tried
#   (the Basic Info isn't complete).
# Returns: A list: `where` and `what` (lists of cards), `summary` (a sentence), `problem` (why the push
#   would be refused, or `NULL`) and `unchecked`.
extraction_service_push_preview_ui <- function(draft, info, catalog, counts = NULL) {
  preview <- list(
    where = extraction_service_where_cards(info, catalog),
    what = list(), summary = "", problem = NULL,
    unchecked = extraction_service_unchecked(draft)
  )
  if (inherits(counts, "error")) {
    preview$problem <- conditionMessage(counts)
  } else if (!is.null(counts)) {
    preview$summary <- extraction_service_push_summary(counts)
    preview$what <- extraction_service_what_cards(counts)
  }
  preview
}
