# ------ Pushing an extraction to the database --------------------------------------------
# A reviewed extraction becomes MATR data: a questionnaire (or just a wave of one that is
# recorded), its blocks as components placed in the questionnaire, its questions and their
# numbers in the wave, and each question's variables with their choices. It is written the way
# an edit is, as a list of items that only add rows (service_edit.R), so the same database
# function applies it, in one transaction, and the Edit history lists every row it added.
#
# A STAFF member's push is a change request that one other staff member approves; an ADMIN's is
# applied at once (service_change_request.R). The rows are worked out when it is applied, from
# the questionnaire as saved and the MATR data as it is then, not when it was sent.
#
# What is reused, and what is new:
#   * An answer choice is kept once for the whole registry, so a choice whose words are already
#     there (exactly) is the one that is there; the others are added.
#   * A set of choices is kept once, so a variable whose choices are those of an existing option
#     set (in any order) uses it; otherwise a set is added, named after its choices.
#   * A new questionnaire adds every block as a new component. Nothing is shared with another
#     questionnaire; that can be done afterwards on Make an edit.
#   * A new wave of a questionnaire that is recorded finds its blocks where it prints them: a
#     block with the same kind and words, in the same place, is that block (so the wave asks the
#     same question, and finds its variables by MATR ID). The blocks that aren't there are
#     added, after the ones that are in the same section.
#   * Nothing already recorded is changed or deleted.

# ------ Before pushing -----------------------------------------------------------------
#' What stops an extraction being pushed.
#'
#' @param record The extraction's record.
#' @param draft The questionnaire as saved; see `draft_service_load()`.
#' @param lookups The pick lists, from `lookup_service_get_all()`.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param records All the extraction records, to find other drafts of the same wave.
#' @return A character vector of sentences, in the order to do them; empty when it can go.
extraction_service_push_blockers <- function(record, draft, lookups, catalog, records = list()) {
  info <- extraction_service_info(record)
  missing <- extraction_service_missing(info)
  problems <- c(
    extraction_service_name_problem(info, catalog),
    extraction_service_abbreviation_problem(info, catalog),
    extraction_service_wave_name_problem(info, catalog, records, record$id),
    extraction_service_date_problem(info$administered_date),
    extraction_service_count_problem(info$respondent_count)
  )
  n_blocks_to_fix <- length(unique(draft_service_check(draft, lookups)$id)) # a blank MATR ID is among them
  as.character(c(
    if (length(missing) > 0) paste0("Fill in the ", word_list(missing), " under Edit Details."),
    if (length(problems) > 0) "Fix the fields outlined in red under Edit Details.",
    if (nrow(draft$components) == 0) "Add the questionnaire's blocks and questions first.",
    if (nrow(draft$components) > 0 && !"Question" %in% draft$components$component_type) "Add at least one question.",
    if (n_blocks_to_fix > 0) paste0("Fix the ", plural(n_blocks_to_fix, "block"), " marked to fix.")
  ))
}

#' How many blocks still haven't been marked as checked.
#'
#' Not a reason to stop: it is said before pushing.
#'
#' @param draft The questionnaire as saved.
#' @return A whole number.
extraction_service_unchecked <- function(draft) {
  sum(!draft$components$checked %in% TRUE)
}

# ------ The rows ----------------------------------------------------------------------------
#' The rows pushing an extraction adds.
#'
#' Reads the questionnaire as it is saved and the MATR data it has to fit, and works out the
#' items (see the top of this file). Nothing is written.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The extraction's record.
#' @return A list: `items` (a data frame in the shape of `edit_item()`, numbered, in the order to
#'   add them) and `summary` (what they come to; see `extraction_service_push_summary()`). An
#'   error with a sentence if it can't be pushed as it is.
extraction_service_push_build <- function(connection, record) {
  # the questions are numbered by their place in the questionnaire, as the review page shows them
  draft <- draft_service_renumber_questions(draft_service_load(connection, record$id))
  info <- extraction_service_info(record)
  ids <- list(
    component_types = lookup_model_get_component_types(connection),
    subjects = lookup_model_get_subjects(connection)
  )
  existing <- if (identical(info$kind, "new_wave")) push_model_existing(connection, info$questionnaire_id)
  extraction_service_build_items(draft, info, ids, existing, push_model_options(connection))
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
#' @param info What staff typed about the questionnaire and wave, from `extraction_service_info()`.
#' @param ids `component_types` and `subjects` as data frames with their ids.
#' @param existing For a new wave, what the questionnaire has printed already, from
#'   `push_model_existing()`; `NULL` for a new questionnaire.
#' @param options The answer choices and option sets there are, from `push_model_options()`.
#' @return A list: `items` and `summary`; see `extraction_service_push_build()`.
extraction_service_build_items <- function(draft, info, ids, existing, options) {
  if (nrow(draft$components) == 0) stop("There is nothing in this questionnaire to add yet.", call. = FALSE)
  push <- push_start(info, ids, existing, options)

  # ---- the questionnaire and its wave
  questionnaire <- if (push$summary$questionnaire) push$token("questionnaire") else as.numeric(info$questionnaire_id)
  if (push$summary$questionnaire) {
    push$insert("questionnaire", list(questionnaire_id = questionnaire), list(
      study_id = as.numeric(info$study_id), respondent_type_id = as.numeric(info$respondent_type_id),
      name = trimws(info$name), abbreviation = trimws(info$abbreviation), description = blank_to_na(info$description)
    ))
  }
  wave <- push$token("wave")
  push$insert("wave", list(wave_id = wave), list(
    questionnaire_id = questionnaire, name = trimws(info$wave_name),
    respondent_count = push_count(info$respondent_count), administered_date = push_date(info$administered_date)
  ))

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
      question_number = block$question_number
    ))
    push_variables(push, draft, block, component_of[[id]], questionnaire)
  }
  list(items = edit_number_items(do.call(rbind, push$items())), summary = push$summary)
}

# The tools a push uses while it works: names for rows that don't exist yet (`token`), a way to add a
# row (`insert`), the ids of kinds of block and subjects, and the count of what it has done so far.
push_start <- function(info, ids, existing, options) {
  items <- list()
  counts <- integer()
  made <- new.env() # answer choices and option sets this push has already added
  push <- new.env()
  push$summary <- list(
    questionnaire = identical(info$kind, "new_questionnaire"), blocks = 0L, blocks_found = 0L, questions = 0L,
    variables = 0L, variables_found = 0L, choices = 0L, answer_options = 0L, option_sets = 0L, option_sets_found = 0L
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
        printed = tolower(as.character(choice$printed)), printed_label = choice$printed_label,
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
    push$summary$option_sets_found <- push$summary$option_sets_found + 1L
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
#' What a push comes to, in words.
#'
#' For example "Adds the questionnaire, the wave, 14 new blocks, 9 questions asked in the wave and 11 new variables."
#'
#' @param summary From `extraction_service_push_build()`.
#' @return A string.
extraction_service_push_summary <- function(summary) {
  parts <- c(
    if (summary$questionnaire) "the questionnaire",
    "the wave",
    if (summary$blocks > 0) plural(summary$blocks, "new block"),
    if (summary$blocks_found > 0) paste0(plural(summary$blocks_found, "block"), " already there"),
    if (summary$questions > 0) paste(plural(summary$questions, "question"), "asked in the wave"),
    if (summary$variables > 0) plural(summary$variables, "new variable"),
    if (summary$variables_found > 0) paste0(plural(summary$variables_found, "variable"), " already there"),
    if (summary$choices > 0) plural(summary$choices, "choice"),
    if (summary$answer_options > 0) plural(summary$answer_options, "new answer choice"),
    if (summary$option_sets > 0) plural(summary$option_sets, "new set of choices", plural_form = "new sets of choices")
  )
  paste0("Adds ", word_list(parts), ".")
}

# ------ Sending it ----------------------------------------------------------------------------------
#' The one-line description of a push, for the request's title.
#'
#' @param record The extraction's record.
#' @param catalog The live tables the pages pick from.
#' @return A string, e.g. `"Add Adult Health and Personality (AHP) · Wave 3"`.
extraction_service_push_title <- function(record, catalog) {
  paste("Add", extraction_service_title(record, catalog))
}

#' Push an extraction to the database.
#'
#' A STAFF member's push is sent to Change Requests, where one other staff member approves it, which
#' applies it. An ADMIN's is applied at once. The questionnaire has to be saved first (the page saves
#' it); what is pushed is the saved copy.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param record The extraction's record.
#' @param me Who is pushing it: `list(staff_id, name, role)`.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param note Why, for the reviewer.
#' @param reviewer The staff id of the person asked to review it, or `NULL`.
#' @param now The time to record.
#' @return A list: `applied` (whether it is in the MATR data now), `history_id` (if so) and `record`
#'   (as saved). An error with a sentence if the database refused.
extraction_service_push <- function(connection, record, me, catalog, note = "", reviewer = NULL, now = Sys.time()) {
  described <- utils::modifyList(
    record,
    list(summary = extraction_service_push_title(record, catalog), note = trimws(note %||% "")),
    keep.null = TRUE
  )
  if (change_request_service_needs_approval(me, extraction_kind)) {
    asked <- if (!is.null(reviewer) && nzchar(as.character(reviewer))) list(id = as.numeric(reviewer))
    sent <- utils::modifyList(
      described,
      list(status = "Awaiting Review", submitted = format_time(now), requested_reviewer = asked),
      keep.null = TRUE
    )
    saved <- extraction_model_save(
      connection, sent, c("status", "summary", "note", "submitted", "requested_reviewer"), me$staff_id,
      expected_revision = record$revision, new_revision = FALSE
    )
    return(list(applied = FALSE, history_id = NULL, record = saved))
  }
  extraction_model_save(connection, described, c("summary", "note"), me$staff_id, new_revision = FALSE)
  history_id <- change_request_model_apply(
    connection, record$id, me$staff_id, extraction_service_push_build(connection, record)$items
  )
  list(applied = TRUE, history_id = history_id, record = extraction_model_get(connection, record$id))
}

# ------ Small things ------------------------------------------------------------------------------------
blank_to_na <- function(text) if (has_text(text)) trimws(text) else NA_character_

# "1,200" as 1200; NA for nothing.
push_count <- function(text) {
  if (!has_text(text)) {
    return(NA)
  }
  as.numeric(gsub(",", "", trimws(text), fixed = TRUE))
}

# "04/15/2022" as "2022-04-15"; NA for nothing.
push_date <- function(text) {
  if (!has_text(text)) {
    return(NA_character_)
  }
  format(as.Date(trimws(text), "%m/%d/%Y"))
}
