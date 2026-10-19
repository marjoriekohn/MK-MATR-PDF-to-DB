# ------ Sending an extraction to the database --------------------------------------------
# An extraction becomes rows of the MATR metadata database, in a recorded questionnaire's wave that
# asks no questions yet: its blocks as components placed in the questionnaire, its questions and
# their numbers in the wave, and each question's variables with their choices. It is written the way an edit is, as a list of items that only add rows
# (service_edit.R), so the same database function applies it, in one transaction, and the Edit
# history lists every row it added.
#
# The questionnaire is previewed and submitted on the Edit Extraction page; previewing writes nothing.
# Submitting sends it to Change Requests, where its Basic Info (registry, study, questionnaire, wave:
# the draft's `info`) is filled in and saved. Then one other staff member approves it, or, for an
# ADMIN's own request, the ADMIN adds it (service_change_request.R). The rows are worked out when it
# is applied, from the questionnaire as saved and the database as it is then, not when it was sent.
#
# What is reused, and what is new:
#   * An answer choice is kept once for the whole registry, so a choice whose words are already
#     there (exactly) is the one that is there; the others are added.
#   * A set of choices is kept once, so a variable whose choices are those of an existing option
#     set (in any order) uses it; otherwise a set is added, named after its choices.
#   * The questionnaire's blocks are found where it prints them: a block with the same kind and
#     words, in the same place, is that block (so the wave asks the same question, and finds its
#     variables by MATR ID). The blocks that aren't there are added, after the ones that are in the
#     same section. A questionnaire with nothing printed yet gets every block as a new component.
#   * The wave is used as it is; it has to ask no questions yet. A questionnaire or wave that isn't
#     recorded yet is added first, from the Basic Info form's drop-downs.
#   * Nothing already recorded is changed or deleted.
#   * Each variable's choices are its RESPONSE_OPTION rows: one per variable and answer choice, with
#     its code. They are always new, since the variable is, even where the words repeat; the answer
#     choices and the sets of choices they point at are the ones reused.

# ------ Before pushing -----------------------------------------------------------------
#' What stops an extraction being added: its Basic Info, then its questionnaire.
#'
#' @param record The extraction's record.
#' @param draft The questionnaire, as on the Edit Extraction page.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param records All the extraction records, to find other drafts of the same questionnaire and wave.
#' @param info Where it goes, as `extraction_service_info()` returns it; the record's by default.
#' @param question_counts How many questions each of the questionnaire's waves asks, for a recorded
#'   wave (`questionnaire_service_wave_question_counts()`).
#' @return A character vector of sentences, in the order to do them; empty when it can go.
extraction_service_push_blockers <- function(record, draft, lookups, catalog, records = list(),
                                             info = extraction_service_info(record), question_counts = integer()) {
  c(
    extraction_service_info_blockers(record, info, catalog, records, question_counts),
    extraction_service_draft_blockers(draft, lookups)
  )
}

#' What stops an extraction's change request being submitted from the Edit Extraction page: only its
#' questionnaire. The Basic Info is filled in afterwards, on Change Requests.
#'
#' @param draft The questionnaire, as on the Edit Extraction page.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @return A character vector of sentences, in the order to do them; empty when it can be submitted.
extraction_service_draft_blockers <- function(draft, lookups) {
  n_blocks_to_fix <- length(unique(draft_service_check(draft, lookups)$id)) # a blank MATR ID is among them
  as.character(c(
    if (nrow(draft$components) == 0) "Add the questionnaire's blocks and questions first.",
    if (nrow(draft$components) > 0 && !"Question" %in% draft$components$component_type) "Add at least one question.",
    if (n_blocks_to_fix > 0) paste0("Fix the ", plural(n_blocks_to_fix, "block"), " marked to fix.")
  ))
}

#' What stops an extraction's change request being approved because of its Basic Info.
#'
#' @inheritParams extraction_service_push_blockers
#' @return A character vector of sentences; empty when the Basic Info is complete and fits.
extraction_service_info_blockers <- function(record, info = extraction_service_info(record), catalog,
                                             records = list(), question_counts = integer()) {
  missing <- extraction_service_missing(info)
  problem <- extraction_service_recorded_wave_problem(info, catalog, question_counts, records, record$id)
  as.character(c(
    if (length(missing) > 0) paste0("Fill in the ", word_list(missing), " under Basic Info."),
    if (!is.null(problem)) "Fix the fields outlined in red under Basic Info."
  ))
}

#' What an extraction's questionnaire holds, in a sentence, for submitting it before its Basic Info is
#' known.
#'
#' @param draft The questionnaire.
#' @return A string, e.g. `"12 blocks, with 30 questions and 45 variables."`.
extraction_service_draft_summary <- function(draft) {
  questions <- sum(draft$components$component_type %in% "Question")
  paste0(
    plural(nrow(draft$components), "block", big_mark = ","), ", with ",
    plural(questions, "question", big_mark = ","), " and ",
    plural(nrow(draft$variables), "variable", big_mark = ","), "."
  )
}

#' How many blocks still haven't been marked as checked.
#'
#' Not a reason to stop: it is said before the request is submitted.
#'
#' @param draft The questionnaire as saved.
#' @return A whole number.
extraction_service_unchecked <- function(draft) {
  sum(!draft$components$checked %in% TRUE)
}

# ------ The rows ----------------------------------------------------------------------------
#' The rows an extraction adds.
#'
#' Reads the questionnaire as it is saved (or takes the one given) and the database it has to fit,
#' and works out the items (see the top of this file). Nothing is written.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The extraction's record.
#' @param draft The questionnaire, if not the saved one (the Edit Extraction page's, before it is saved).
#' @param info Where it goes, as `extraction_service_info()` returns it; the record's by default.
#' @return A list: `items` (a data frame in the shape of `edit_item()`, numbered, in the order to
#'   add them) and `summary` (what they come to; see `extraction_service_push_summary()`). An
#'   error with a sentence if it can't be added as it is.
extraction_service_push_build <- function(connection, record, draft = NULL, info = extraction_service_info(record)) {
  # the questions are numbered by their place in the questionnaire, as the Edit Extraction page shows them
  draft <- draft_service_renumber_questions(draft %||% draft_service_load(connection, record$id))
  context <- extraction_service_push_context(connection)
  existing <- extraction_service_push_existing(connection, info)
  extraction_service_build_items(draft, info, context$ids, existing, context$options)
}

#' What any extraction's rows are worked out against: the kinds of block, the subjects, and the
#' answer choices and option sets there are.
#'
#' Read once for a preview, which is worked out again as its fields change.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list: `ids` (`component_types` and `subjects`, as data frames with their ids) and
#'   `options` (from `push_model_options()`).
extraction_service_push_context <- function(connection) {
  list(
    ids = list(
      component_types = lookup_model_get_component_types(connection),
      subjects = lookup_model_get_subjects(connection)
    ),
    options = push_model_options(connection)
  )
}

#' What a recorded questionnaire has printed already, for an extraction that goes into it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @return From `push_model_existing()`, or `NULL` while none is picked.
extraction_service_push_existing <- function(connection, info) {
  if (grepl("^[0-9]+$", info$questionnaire_id)) {
    push_model_existing(connection, info$questionnaire_id)
  }
}

#' The items of pushing an extraction.
#'
#' @inheritParams extraction_service_push_build
#' @return The items, as a data frame; this is what the database function is given.
extraction_service_push_items <- function(connection, record) {
  extraction_service_push_build(connection, record)$items
}

#' The rows to add for a questionnaire as reviewed.
#'
#' Pure: everything it needs is given. See the top of this file for what is reused.
#'
#' @param draft The questionnaire as saved; see `draft_service_from_json()`.
#' @param info Where it goes, from `extraction_service_info()`.
#' @param ids `component_types` and `subjects` as data frames with their ids.
#' @param existing What the questionnaire has printed already, from `push_model_existing()`.
#' @param options The answer choices and option sets there are, from `push_model_options()`.
#' @return A list: `items` and `summary`; see `extraction_service_push_build()`.
extraction_service_build_items <- function(draft, info, ids, existing, options) {
  if (nrow(draft$components) == 0) stop("There is nothing in this questionnaire to add yet.", call. = FALSE)
  push <- push_start(ids, existing, options)
  questionnaire <- as.numeric(info$questionnaire_id)
  wave <- push_recorded_wave(info, existing)

  # ---- the blocks, parents before what is inside them
  blocks <- draft$components
  found <- if (!is.null(existing)) push_match_blocks(draft, existing, ids) else numeric()
  component_of <- list() # block id -> its component: a number (there already) or a name (new:component:3)
  placed_in <- list() # section -> how many new blocks have been put in it
  for (id in draft_service_printed_order(draft)) {
    block <- blocks[blocks$id == id, ]
    if (id %in% names(found)) {
      component_of[[id]] <- found[[id]]
      push$summary$blocks_found <- push$summary$blocks_found + 1L
    } else {
      component <- push$token("component")
      component_of[[id]] <- component
      parent <- if (is.na(block$parent_id)) NA else component_of[[block$parent_id]]
      section <- if (is.na(parent)) "(top)" else as.character(parent)
      placed_in[[section]] <- (placed_in[[section]] %||% 0L) + 1L
      push$insert("component", list(component_id = component), list(
        component_type_id = push$type_id(block$component_type), text = block$text
      ))
      push$insert("questionnaire_component", list(component_id = component, questionnaire_id = questionnaire), list(
        parent_component_id = parent,
        display_order = push_taken(existing, parent) + placed_in[[section]]
      ))
      push$summary$blocks <- push$summary$blocks + 1L
      if (block$component_type == "Question") {
        push$insert("question", list(component_id = component))
        follows <- if (is.na(block$parent_question_id)) NA else component_of[[block$parent_question_id]]
        push$insert("questionnaire_question", list(component_id = component, questionnaire_id = questionnaire), list(
          parent_question_id = follows,
          self_contained = if (is.na(block$self_contained)) NA else tolower(as.character(block$self_contained)),
          condition = block$condition
        ))
      }
    }
    if (block$component_type != "Question") next
    push$summary$questions <- push$summary$questions + 1L
    push$insert("wave_question", list(component_id = component_of[[id]], questionnaire_id = questionnaire, wave_id = wave), list(
      question_number = draft_service_question_number(block)
    ))
    push_variables(push, draft, block, component_of[[id]], questionnaire)
  }
  list(items = edit_number_items(do.call(rbind, push$items())), summary = push$summary)
}

# The tools a push uses while it works: names for rows that don't exist yet (`token`), a way to add a
# row (`insert`), the ids of kinds of block and subjects, and the count of what it has done so far.
push_start <- function(ids, existing, options) {
  items <- list()
  counts <- integer()
  made <- new.env() # answer choices and option sets this push has already added
  push <- new.env()
  push$summary <- list(
    blocks = 0L, blocks_found = 0L, questions = 0L, variables = 0L, variables_found = 0L, choices = 0L,
    answer_options = 0L, answer_options_found = 0L, option_sets = 0L, option_sets_found = 0L
  )
  push$existing <- existing
  push$options <- options
  push$made <- made
  # A name for a row that doesn't exist yet: new:component:3.
  push$token <- function(table) {
    counts[table] <<- (if (table %in% names(counts)) counts[[table]] else 0L) + 1L
    sprintf("new:%s:%d", table, counts[[table]])
  }
  # A row to insert: `key` names it (the column the database numbers, or the columns that make up its
  # key), `fields` are its other columns; blank ones are left out.
  push$insert <- function(table, key, fields = list()) {
    identifier <- edit_identifier(key)
    rows <- lapply(names(fields), function(column) {
      value <- fields[[column]]
      if (length(value) == 1 && !is.na(value)) edit_item(table, identifier, "Insert", column, NA, value)
    })
    rows <- Filter(Negate(is.null), rows)
    # a row that is only its key still has to be said (a question is only its component)
    if (length(rows) == 0) rows <- list(edit_item(table, identifier, "Insert", names(key)[1], NA, key[[1]]))
    items <<- c(items, rows)
  }
  push$items <- function() items
  push$type_id <- function(name) ids$component_types$component_type_id[match(name, ids$component_types$name)]
  push$subject_id <- function(name) ids$subjects$subject_id[match(name, ids$subjects$name)]
  push
}

# The variables of one question. A variable of a recorded questionnaire that has the MATR ID is that
# variable; the rest are added.
push_variables <- function(push, draft, block, component, questionnaire) {
  boxes <- draft_service_children(draft, "variables", block$id)
  existing <- push$existing
  here <- if (!is.null(existing) && is.numeric(component)) {
    existing$variables[existing$variables$component_id == component, , drop = FALSE]
  }
  order_so_far <- if (!is.null(here) && nrow(here) > 0) max(here$display_order, na.rm = TRUE) else 0
  for (i in seq_len(nrow(boxes))) {
    box <- boxes[i, ]
    if (!is.null(existing) && box$matr_id %in% existing$variables$matr_id) {
      elsewhere <- existing$variables[existing$variables$matr_id == box$matr_id, , drop = FALSE]
      if (!(is.numeric(component) && component %in% elsewhere$component_id)) {
        stop(sprintf(
          "The MATR ID %s is already used by another question of this questionnaire. Give the variable a MATR ID of its own.",
          box$matr_id
        ), call. = FALSE)
      }
      push$summary$variables_found <- push$summary$variables_found + 1L
      next
    }
    order_so_far <- order_so_far + 1
    variable <- push$token("variable")
    push$insert("variable", list(variable_key = variable), list(
      questionnaire_id = questionnaire, component_id = component, matr_id = trimws(box$matr_id),
      display_order = order_so_far, label = box$label, subject_id = push$subject_id(box$subject)
    ))
    push$summary$variables <- push$summary$variables + 1L
    if (box$variable_type == "open_ended") {
      push$insert("open_ended", list(variable_key = variable), list(answer_data_type = box$answer_data_type))
      next
    }
    choices <- draft_service_children(draft, "response_options", box$id)
    push$insert("closed_ended", list(variable_key = variable), list(option_set_id = push_option_set(push, choices$answer_option)))
    for (j in seq_len(nrow(choices))) {
      choice <- choices[j, ]
      push$insert("response_option", list(variable_key = variable, answer_option_id = push_answer_option(push, choice$answer_option)), list(
        display_order = j, code_value = if (is.na(choice$code_value)) NA else as.character(choice$code_value),
        printed_label = choice$printed_label,
        skip_instruction = choice$skip_instruction
      ))
      push$summary$choices <- push$summary$choices + 1L
    }
  }
}

# The id (or the name) of an answer choice with these words: the one that is there, or a new one.
push_answer_option <- function(push, label) {
  answer_options <- push$options$answer_options
  there <- answer_options$answer_option_id[answer_options$standardized_label == label]
  if (length(there) == 1) {
    found_key <- paste0("found choice:", label) # counted once, however many variables use it
    if (is.null(push$made[[found_key]])) {
      push$made[[found_key]] <- TRUE
      push$summary$answer_options_found <- push$summary$answer_options_found + 1L
    }
    return(there)
  }
  key <- paste0("choice:", label)
  if (!is.null(push$made[[key]])) {
    return(push$made[[key]])
  }
  name <- push$token("answer_option")
  push$insert("answer_option", list(answer_option_id = name), list(standardized_label = label))
  push$made[[key]] <- name
  push$summary$answer_options <- push$summary$answer_options + 1L
  name
}

# The id (or the name) of an option set with exactly these choices, in any order: the one that is
# there, or a new one named after its choices.
push_option_set <- function(push, labels) {
  labels <- unique(labels)
  key <- paste(sort(labels), collapse = "\r")
  options <- push$options
  member_labels <- options$answer_options$standardized_label[match(
    options$members$answer_option_id,
    options$answer_options$answer_option_id
  )]
  keys <- vapply(split(member_labels, options$members$option_set_id), function(m) paste(sort(m), collapse = "\r"), character(1))
  there <- names(keys)[keys == key]
  if (length(there) > 0) {
    found_key <- paste0("found set:", there[1]) # counted once, however many variables use it
    if (is.null(push$made[[found_key]])) {
      push$made[[found_key]] <- TRUE
      push$summary$option_sets_found <- push$summary$option_sets_found + 1L
    }
    return(as.numeric(there[1]))
  }
  made_key <- paste0("set:", key)
  if (!is.null(push$made[[made_key]])) {
    return(push$made[[made_key]])
  }
  name <- push$token("option_set")
  set_name <- push_option_set_name(labels, c(options$option_sets$name, push$made$set_names))
  push$insert("option_set", list(option_set_id = name), list(name = set_name))
  push$made$set_names <- c(push$made$set_names, set_name)
  for (label in labels) {
    push$insert("answer_option_set", list(answer_option_id = push_answer_option(push, label), option_set_id = name))
  }
  push$made[[made_key]] <- name
  push$summary$option_sets <- push$summary$option_sets + 1L
  name
}

# "Yes / No", "Never / Sometimes / Often", with a number after it if the name is taken.
push_option_set_name <- function(labels, taken) {
  name <- paste(labels, collapse = " / ")
  if (nchar(name) > 150) name <- paste0(substr(name, 1, 147), "...")
  candidate <- name
  n <- 1
  while (candidate %in% taken) {
    n <- n + 1
    candidate <- paste0(name, " (", n, ")")
  }
  candidate
}

# For a new wave: which blocks of the draft are blocks the questionnaire already prints. A block is
# one that is there if it has the same kind and words and sits in the same place (inside the same
# block, or at the top). Each block that is there is found once.
#
# @return A named numeric vector: the draft's block id -> the component that is there.
push_match_blocks <- function(draft, existing, ids) {
  blocks <- draft$components
  placements <- existing$placements
  components <- existing$components
  words <- function(text) trimws(gsub("[[:space:]]+", " ", ifelse(is.na(text), "", text)))
  of_component <- match(placements$component_id, components$component_id)
  placed <- data.frame(
    component_id = placements$component_id,
    parent = placements$parent_component_id,
    order = placements$display_order,
    type = ids$component_types$name[match(components$component_type_id[of_component], ids$component_types$component_type_id)],
    text = words(components$text[of_component]),
    used = rep(FALSE, nrow(placements)), # a questionnaire with no blocks yet has no rows here
    stringsAsFactors = FALSE
  )
  found <- numeric()
  for (id in draft_service_printed_order(draft)) {
    block <- blocks[blocks$id == id, ]
    parent <- NA_real_
    if (!is.na(block$parent_id)) {
      if (!block$parent_id %in% names(found)) next # inside a block that is new: it is new
      parent <- found[[block$parent_id]]
    }
    here <- which(
      !placed$used & placed$type %in% block$component_type & placed$text == words(block$text) &
        (if (is.na(parent)) is.na(placed$parent) else !is.na(placed$parent) & placed$parent == parent)
    )
    if (length(here) == 0) next
    here <- here[which.min(placed$order[here])]
    placed$used[here] <- TRUE
    found[id] <- placed$component_id[here]
  }
  found
}

# The recorded wave an extraction goes into: one of the questionnaire's waves, asking no questions yet.
push_recorded_wave <- function(info, existing) {
  wave <- suppressWarnings(as.numeric(info$wave_id))
  if (is.null(existing) || is.na(wave) || !wave %in% existing$waves$wave_id) {
    stop("The wave picked isn't one of this questionnaire's waves. Pick it again.", call. = FALSE)
  }
  asked <- sum(existing$wave_questions$wave_id %in% wave)
  if (asked > 0) {
    stop(sprintf(
      "The wave picked already asks %s. Pick a wave without questions, or add a new wave.", plural(asked, "question")
    ), call. = FALSE)
  }
  wave
}

# How many places in a section (or at the top) are taken already, so a new block goes after them.
# A section that is new has nothing in it.
push_taken <- function(existing, parent) {
  if (is.null(existing) || (!is.na(parent) && !is.numeric(parent))) {
    return(0L)
  }
  placements <- existing$placements
  in_section <- if (is.na(parent)) is.na(placements$parent_component_id) else placements$parent_component_id %in% parent
  if (!any(in_section)) 0L else as.integer(max(placements$display_order[in_section], na.rm = TRUE))
}

# ------ Said in words ---------------------------------------------------------------------------
#' What an extraction's rows come to, in words.
#'
#' For example "Adds 14 new blocks, 9 questions asked in the wave, 11 new variables and 20 response
#' options."
#'
#' @param summary From `extraction_service_push_build()`.
#' @return A string.
extraction_service_push_summary <- function(summary) {
  parts <- c(
    if (summary$blocks > 0) plural(summary$blocks, "new block"),
    if (summary$blocks_found > 0) paste0(plural(summary$blocks_found, "block"), " already there"),
    if (summary$questions > 0) paste(plural(summary$questions, "question"), "asked in the wave"),
    if (summary$variables > 0) plural(summary$variables, "new variable"),
    if (summary$variables_found > 0) paste0(plural(summary$variables_found, "variable"), " already there"),
    if (summary$choices > 0) plural(summary$choices, "response option"),
    if (summary$answer_options > 0) plural(summary$answer_options, "new answer choice"),
    if (summary$option_sets > 0) plural(summary$option_sets, "new set of choices", plural_form = "new sets of choices")
  )
  paste0("Adds ", word_list(parts), ".")
}

# ------ The preview -------------------------------------------------------------------------------
#' What an extraction's change request comes to, to show before it is submitted and while it waits.
#'
#' Pure: the rows are worked out from what is given (`context` and `existing` are read by the caller),
#' and nothing is written.
#'
#' @param draft The questionnaire.
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param context From `extraction_service_push_context()`.
#' @param existing From `extraction_service_push_existing()`.
#' @return A list for `extraction_request_ui()`: `where` and `what` (lists of cards), `summary` (a
#'   sentence), `problem` (why the rows can't be worked out, or `NULL`) and `unchecked`.
extraction_service_request_preview <- function(draft, info, catalog, context, existing = NULL) {
  preview <- list(
    where = extraction_service_where_cards(info, catalog),
    what = list(), summary = "", problem = NULL,
    unchecked = extraction_service_unchecked(draft)
  )
  if (length(extraction_service_missing(info)) > 0) {
    return(preview)
  }
  built <- tryCatch(
    extraction_service_build_items(draft_service_renumber_questions(draft), info, context$ids, existing, context$options),
    error = function(problem) problem
  )
  if (inherits(built, "error")) {
    preview$problem <- conditionMessage(built)
    return(preview)
  }
  preview$summary <- extraction_service_push_summary(built$summary)
  preview$what <- extraction_service_what_cards(built$summary)
  preview
}

#' The cards for where an extraction goes: its registry, study, questionnaire and wave, all recorded.
#'
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @return A list of four cards (see `request_card_ui()`); one not picked yet says so.
extraction_service_where_cards <- function(info, catalog) {
  not_picked <- function(kind) list(kind = kind, title = "Not picked yet", subtitle = "", facts = character(), is_missing = TRUE)
  registry <- catalog$registries[as.character(catalog$registries$registry_id) %in% info$registry_id, , drop = FALSE]
  study <- questionnaire_service_get_study(catalog, info$study_id)
  questionnaire <- questionnaire_service_get(catalog, info$questionnaire_id)
  wave <- catalog$waves[as.character(catalog$waves$wave_id) %in% info$wave_id, , drop = FALSE]
  list(
    if (nrow(registry) == 1) {
      list(
        kind = "Registry", title = questionnaire_name(registry$name, registry$abbreviation %||% ""),
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
        kind = "Questionnaire", title = questionnaire_name(questionnaire$name, questionnaire$abbreviation),
        subtitle = paste0("For ", questionnaire$respondent_type, " · already in the database"),
        facts = plural(questionnaire$n_waves, "wave"), is_new = FALSE
      )
    } else {
      not_picked("Questionnaire")
    },
    if (nrow(wave) == 1) {
      list(
        kind = "Wave", title = wave$name, subtitle = "Already in the database, with no questions yet",
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

#' The cards for what an extraction adds: for each kind of row, how many are new, how many are already
#' in the database and are used again, and which rows only link the two.
#'
#' @param summary From `extraction_service_push_build()`.
#' @return A list of cards (see `request_card_ui()`); a card is NEW when its rows are added.
extraction_service_what_cards <- function(summary) {
  count <- function(n) format(n, big.mark = ",")
  card <- function(kind, n_new, n_found, title, found_note, subtitle = "") {
    list(
      kind = kind, title = title, subtitle = subtitle,
      facts = c(if (n_new > 0) paste(count(n_new), "new"), if (n_found > 0) paste(count(n_found), found_note)),
      is_new = n_new > 0
    )
  }
  list(
    card(
      "Blocks", summary$blocks, summary$blocks_found,
      plural(summary$blocks + summary$blocks_found, "block", big_mark = ","), "already in the questionnaire"
    ),
    card(
      "Questions in the wave", summary$questions, 0L, plural(summary$questions, "question", big_mark = ","), "",
      subtitle = "Each question the wave asks, with its number"
    ),
    card(
      "Variables", summary$variables, summary$variables_found,
      plural(summary$variables + summary$variables_found, "variable", big_mark = ","), "already recorded"
    ),
    card(
      "Answer choices", summary$answer_options, summary$answer_options_found,
      plural(summary$answer_options + summary$answer_options_found, "different answer choice", big_mark = ","),
      "already in the database, used again",
      subtitle = "Each worded choice is kept once for the whole registry"
    ),
    card(
      "Sets of choices", summary$option_sets, summary$option_sets_found,
      plural(summary$option_sets + summary$option_sets_found, "set of choices", plural_form = "sets of choices", big_mark = ","),
      "already in the database, used again"
    ),
    card(
      "Response options", summary$choices, 0L, plural(summary$choices, "response option", big_mark = ","), "",
      subtitle = "Links between a variable and an answer choice, with its code: one per variable for each of its choices"
    )
  )
}

# ------ Sending it ----------------------------------------------------------------------------------
#' The one-line description of an extraction's change request, for its title.
#'
#' @param record The extraction's record.
#' @param catalog The live tables the pages pick from.
#' @return A string, e.g. `"Add Adult Health and Personality (AHP) · Wave 3"`.
extraction_service_push_title <- function(record, catalog) {
  paste("Add", extraction_service_title(record, catalog))
}

#' Submit an extraction's change request.
#'
#' It is sent to Change Requests, whoever submits it: its Basic Info (`info`) is filled in there. Then
#' one other staff member approves it, which applies it; an ADMIN can add their own without approval.
#' The questionnaire has to be saved first (the page saves it); what is sent is the saved copy.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The extraction's record.
#' @param me Who is submitting it: `list(staff_id, name, role)`.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param reviewer The staff id of the person asked to review it, or `NULL`.
#' @param now The time to record.
#' @return A list: `applied` (always `FALSE`: it waits on Change Requests), `history_id` (`NULL`) and
#'   `record` (as saved). An error with a sentence if the database refused.
extraction_service_push <- function(connection, record, me, catalog, reviewer = NULL, now = Sys.time()) {
  asked <- if (!is.null(reviewer) && nzchar(as.character(reviewer))) list(id = as.numeric(reviewer))
  sent <- utils::modifyList(
    record,
    list(
      summary = extraction_service_push_title(record, catalog), status = "Awaiting Review",
      submitted = format_time(now), requested_reviewer = asked
    ),
    keep.null = TRUE
  )
  saved <- extraction_model_save(
    connection, sent, c("status", "summary", "submitted", "requested_reviewer"), me$staff_id,
    expected_revision = record$revision, new_revision = FALSE
  )
  list(applied = FALSE, history_id = NULL, record = saved)
}

#' Save an extraction's Basic Info: where it goes in the database.
#'
#' Filled in on Change Requests. It is a change to the request like any other: it makes a new
#' revision, and whoever saves it can't be the one who approves it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The request as the person read it.
#' @param info Where it goes, as `extraction_service_info()` returns it.
#' @param me Who is saving it: `list(staff_id, name, role)`.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @return The record as saved. An error with a sentence if the database refused, e.g. because the
#'   request was changed after the person opened it.
extraction_service_save_info <- function(connection, record, info, me, catalog) {
  placed <- utils::modifyList(record, list(info = info), keep.null = TRUE)
  described <- utils::modifyList(placed, list(summary = extraction_service_push_title(placed, catalog)), keep.null = TRUE)
  extraction_model_save(connection, described, c("summary", "info"), me$staff_id, expected_revision = record$revision)
}
