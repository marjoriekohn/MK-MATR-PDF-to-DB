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
#     there (as normal_answer() writes them) is the one that is there, and so is a choice whose words were
#     merged into one on Make an edit (answer_option_alias); the others are added. A new choice that looks
#     like one that is there ("yes most of the time" for "yes, most of the time") says so in the preview.
#   * A set of choices is kept once, so a variable whose choices are those of an existing option
#     set (in any order) uses it; otherwise a set is added, named after its choices.
#   * A question linked on the Edit Extraction page with the questions that differ from it only in who they
#     ask about has a stem (question_stem), kept once for each wording: the one there, or a new one. A question
#     that is there already (found, or printed by another questionnaire) keeps the link too: one with no stem
#     gets it (the one change an extraction makes to a row that is there), and one linked under another stem
#     stops the push with a sentence.
#   * The questionnaire's blocks are found where it prints them: a block with the same kind and
#     words, in the same place, is that placement (so the wave asks the same question, and finds its
#     variables by MATR ID). The blocks that aren't there are placed, after the ones that are in the
#     same section.
#   * A component is a wording, kept once for the whole registry: one for each kind and words (exactly,
#     but for runs of spaces), and one Group. Every block the questionnaire doesn't print yet gets a
#     placement of its own, and its component is the one with its kind and words: one the database has
#     (another questionnaire prints it, or this one elsewhere), one this push has come to already (the
#     questionnaire prints it again, as an instruction repeated before each battery), or a new one.
#   * The wave is used as it is; it has to ask no questions yet. A questionnaire or wave that isn't
#     recorded yet is added first, from the Basic Info form's drop-downs.
#   * Nothing already recorded is changed or deleted.
#   * Each variable's choices are its RESPONSE_OPTION rows: one per variable and answer choice, with
#     its code. They are always new, since the variable is, even where the words repeat; the answer
#     choices and the sets of choices they point at are the ones reused.
#   * Who a variable is about is a SUBJECT row: the respondent's ("Respondent") for a blank, the one with the
#     same name (ignoring capitals and spaces at the ends) when there is one, or a new one staff typed.
#   * A choice or a write-in that skips points at the placement it sends respondents to (skip_to_placement_id),
#     so every block is placed before any variable, or says it ends the questionnaire (skip_to_end). A group
#     that skips (on the answers to the questions in it) says so where the questionnaire places it, so every
#     new block's component is added, and its placement named, before any is placed.
#   * The wave records each question's place among the questions (question_number, 1, 2, 3...,
#     sections and groups not counted) and the number printed beside it (printed_number),
#     which can repeat or be missing.

# ------ Before pushing -----------------------------------------------------------------
# What stops an extraction's change request being submitted from the Edit Extraction page: only its
# questionnaire. The Basic Info is filled in afterwards, on Change Requests.
# draft: The questionnaire, as on the Edit Extraction page.
# Returns: A character vector of sentences, in the order to do them; empty when it can be submitted.
extraction_service_draft_blockers <- function(draft, lookups) {
  n_blocks_to_fix <- length(unique(draft_service_check(draft, lookups)$id)) # a blank MATR ID is among them
  as.character(c(
    if (nrow(draft$components) == 0) "Add the questionnaire's blocks and questions first.",
    if (nrow(draft$components) > 0 && !"Question" %in% draft$components$component_type) "Add at least one question.",
    if (n_blocks_to_fix > 0) paste0("Fix the ", plural(n_blocks_to_fix, "block"), " marked to fix.")
  ))
}

# What stops an extraction's change request being approved because of its Basic Info.
# info: Where it goes, as `extraction_service_info()` returns it; the record's by default.
# records: All the extraction records, to find other drafts of the same questionnaire and wave.
# question_counts: How many questions each of the questionnaire's waves asks, for a recorded
#   wave (`questionnaire_service_wave_question_counts()`).
# Returns: A character vector of sentences; empty when the Basic Info is complete and fits.
extraction_service_info_blockers <- function(record, info = extraction_service_info(record), catalog,
                                             records = list(), question_counts = integer()) {
  missing <- extraction_service_missing(info)
  problem <- extraction_service_recorded_wave_problem(info, catalog, question_counts, records, record$id)
  as.character(c(
    if (length(missing) > 0) paste0("Fill in the ", word_list(missing), " under Basic Info."),
    if (!is.null(problem)) "Fix the fields outlined in red under Basic Info."
  ))
}

# What an extraction's questionnaire holds, in a sentence, for submitting it before its Basic Info is
# known.
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
# Not a reason to stop: it is said before the request is submitted.
# draft: The questionnaire as saved.
extraction_service_unchecked <- function(draft) {
  sum(!draft$components$checked %in% TRUE)
}

# ------ The rows ----------------------------------------------------------------------------
# The rows an extraction adds.
# Reads the questionnaire as it is saved (or takes the one given) and the database it has to fit,
# and works out the items (see the top of this file). Nothing is written.
# draft: The questionnaire, if not the saved one (the Edit Extraction page's, before it is saved).
# info: Where it goes, as `extraction_service_info()` returns it; the record's by default.
# Returns: A list: `items` (a data frame in the shape of `edit_item()`, numbered, in the order to
#   add them), `summary` (what they come to; see `extraction_service_push_summary()`) and `rows` (each
#   block, variable, answer choice and set of choices, new or already there; see `push_start()`). An
#   error with a sentence if it can't be added as it is.
extraction_service_push_build <- function(connection, record, draft = NULL, info = extraction_service_info(record)) {
  # the questions are numbered by their place in the questionnaire, as the Edit Extraction page shows them
  draft <- draft_service_renumber_questions(draft %||% draft_service_load(connection, record$id))
  context <- extraction_service_push_context(connection)
  existing <- extraction_service_push_existing(connection, info)
  extraction_service_build_items(draft, info, context$ids, existing, context$options)
}

# What any extraction's rows are worked out against: the kinds of block, the subjects, the
# answer choices and option sets there are, and the components with words.
# Read once for a preview, which is worked out again as its fields change.
# Returns: A list: `ids` (`component_types`, `subjects` and `time_frames`, as data frames with their ids) and
#   `options` (from `push_model_options()`, with `components` from `push_model_components()`, and `placed` and
#   `questionnaires` from `push_model_placed()`).
extraction_service_push_context <- function(connection) {
  list(
    ids = list(
      component_types = lookup_model_get_component_types(connection),
      subjects = lookup_model_get_subjects(connection),
      time_frames = lookup_model_get_time_frames(connection)
    ),
    options = c(push_model_options(connection), list(components = push_model_components(connection)), push_model_placed(connection))
  )
}

# What a recorded questionnaire has printed already, for an extraction that goes into it.
# Returns: From `push_model_existing()`, or `NULL` while none is picked.
extraction_service_push_existing <- function(connection, info) {
  if (grepl("^[0-9]+$", info$questionnaire_id)) {
    push_model_existing(connection, info$questionnaire_id)
  }
}

# The items of pushing an extraction.
# Returns: The items, as a data frame; this is what the database function is given.
extraction_service_push_items <- function(connection, record) {
  extraction_service_push_build(connection, record)$items
}

# The rows to add for a questionnaire as reviewed.
# Pure: everything it needs is given. See the top of this file for what is reused.
# draft: The questionnaire as saved; see `draft_service_from_json()`.
# info: Where it goes, from `extraction_service_info()`.
# ids: `component_types`, `subjects` and `time_frames` as data frames with their ids (without
#   `time_frames`, every time frame is new).
# existing: What the questionnaire has printed already, from `push_model_existing()`.
# options: The answer choices and option sets there are, from `push_model_options()`, and the
#   components with words (`components`, from `push_model_components()`; without it, no block is shared).
# Returns: A list: `items`, `summary` and `rows`; see `extraction_service_push_build()`.
extraction_service_build_items <- function(draft, info, ids, existing, options) {
  if (nrow(draft$components) == 0) stop("There is nothing in this questionnaire to add yet.", call. = FALSE)
  push <- push_start(ids, existing, options)
  questionnaire <- as.numeric(info$questionnaire_id)
  wave <- push_recorded_wave(info, existing)
  push$wave <- wave # the respondents' counts are this wave's

  # ---- the blocks: first each one's component (its words) and placement (its place), so a group or a choice can
  # skip to a block printed after it ...
  blocks <- draft$components
  printed <- draft_service_printed_order(draft)
  found <- if (!is.null(existing)) push_match_blocks(draft, existing, ids) else numeric()
  component_of <- list() # block id -> its component: a number (there already) or a name (new:component:3)
  placement_of <- list() # block id -> its placement: a number (there already) or a name (new:questionnaire_component:3)
  for (id in printed) {
    block <- blocks[blocks$id == id, ]
    if (id %in% names(found)) {
      placement_of[[id]] <- found[[id]]
      component_of[[id]] <- existing$placements$component_id[match(found[[id]], existing$placements$placement_id)]
      push$summary$blocks_found <- push$summary$blocks_found + 1L
      push$note_block(block, "Already in the questionnaire")
      push_link_reused(push, block, component_of[[id]])
      next
    }
    component_of[[id]] <- push_component(push, block)
    placement_of[[id]] <- push$token("questionnaire_component")
  }
  # ---- ... then where the questionnaire places each new one, after what holds it and where it skips to
  push$placement_of <- placement_of
  order_in <- push_new_display_orders(blocks, printed, found, placement_of, existing)
  for (id in push_placing_order(blocks, printed, found)) {
    block <- blocks[blocks$id == id, ]
    placement <- placement_of[[id]]
    if (!id %in% names(found)) {
      parent <- if (is.na(block$parent_id)) NA else placement_of[[block$parent_id]]
      push$insert("questionnaire_component", list(placement_id = placement), c(
        list(
          questionnaire_id = questionnaire, component_id = component_of[[id]], parent_placement_id = parent,
          display_order = order_in[[id]],
          # who answers everything in a section, grid or group; a question's own is on its questionnaire_question
          condition = if (is_container(block$component_type)) block$condition else NA
        ),
        push_group_skip(push, block)
      ))
      if (block$component_type == "Question") {
        follows <- if (is.na(block$parent_question_id)) NA else placement_of[[block$parent_question_id]]
        push$insert("questionnaire_question", list(placement_id = placement), list(
          questionnaire_id = questionnaire, component_id = component_of[[id]], parent_question_placement_id = follows,
          self_contained = if (is.na(block$self_contained)) NA else tolower(as.character(block$self_contained)),
          condition = block$condition
        ))
      }
    }
    if (block$component_type != "Question") next
    push$summary$questions <- push$summary$questions + 1L
    push$insert("wave_question", list(placement_id = placement, wave_id = wave), list(
      questionnaire_id = questionnaire, question_number = block$question_number, printed_number = block$printed_number
    ))
  }
  # ---- then the variables, once every block is there: a choice can skip to a block printed after it
  for (id in printed) {
    block <- blocks[blocks$id == id, ]
    if (block$component_type == "Question") push_variables(push, draft, block, placement_of[[id]], questionnaire)
  }
  list(items = edit_number_items(do.call(rbind, push$items())), summary = push$summary, rows = push$rows())
}

# Where each new block goes among what holds it (or the top): after the blocks there already, in printed order.
# Returns: A named list: block id -> display order, for the blocks that aren't there yet.
push_new_display_orders <- function(blocks, printed, found, placement_of, existing) {
  counted <- list() # what holds them -> how many new blocks are in it so far
  orders <- list()
  for (id in setdiff(printed, names(found))) {
    parent_id <- blocks$parent_id[blocks$id == id]
    parent <- if (is.na(parent_id)) NA else placement_of[[parent_id]]
    holder <- if (is.na(parent)) "(top)" else as.character(parent)
    counted[[holder]] <- (counted[[holder]] %||% 0L) + 1L
    orders[[id]] <- push_taken(existing, parent) + counted[[holder]]
  }
  orders
}

# The order to place the blocks in: printed order, but a group that skips to a block printed after it comes after
# that block (and what holds it), since its row names the place it skips to. What holds a block, and the question a
# question follows, always come first.
push_placing_order <- function(blocks, printed, found) {
  done <- character()
  visiting <- character()
  visit <- function(id) {
    if (is.na(id) || id %in% done || id %in% visiting || !id %in% blocks$id) {
      return(invisible())
    }
    visiting <<- c(visiting, id)
    block <- blocks[blocks$id == id, ]
    visit(block$parent_id)
    visit(block$parent_question_id) # a question's settings name the question it follows
    target <- block$skip_to_id
    if (block$component_type %in% skipping_types && !is.na(target) && target != skip_to_end_id) visit(target)
    done <<- c(done, id)
    invisible()
  }
  for (id in printed) visit(id)
  done
}

# The component a block of the draft prints, for a block the questionnaire doesn't print yet. There is one component
# for each kind and words: the one this push has already come to with them (the questionnaire prints them again),
# the one the database has (another questionnaire prints them, or this one elsewhere), or a new one.
push_component <- function(push, block) {
  key <- paste0("component:", push$type_id(block$component_type), ":", push_words(block$text))
  again <- push$made[[key]]
  if (!is.null(again)) {
    push$summary$blocks_repeated <- push$summary$blocks_repeated + 1L
    push$note_block(block, "Printed again")
    return(again)
  }
  shared <- push_shared_component(push, block)
  if (!is.null(shared)) {
    push$summary$blocks_shared <- push$summary$blocks_shared + 1L
    push$note_block(block, push_shared_status(push$options, shared)) # placed here, not copied
    push_link_reused(push, block, shared)
    push$made[[key]] <- shared
    return(shared)
  }
  component <- push$token("component")
  push$insert("component", list(component_id = component), list(
    component_type_id = push$type_id(block$component_type), text = block$text
  ))
  if (block$component_type == "Question") {
    push$insert("question", list(component_id = component), list(question_stem_id = push_question_stem(push, push_stem_of(block))))
  }
  push$summary$blocks <- push$summary$blocks + 1L
  push$note_block(block, "New", is_new = TRUE)
  push$made[[key]] <- component
  component
}

# The tools a push uses while it works: names for rows that don't exist yet (`token`), a way to add a
# row (`insert`), the ids of kinds of block and subjects, the count of what it has done so far, and each
# block, variable, answer choice and set of choices it has come to (`note`; `rows()` gives them as
# data frames of `text`, `detail`, `status`, `is_new` and `uses`, by kind, in the order they came).
push_start <- function(ids, existing, options) {
  items <- list()
  counts <- integer()
  listed <- list(blocks = list(), variables = list(), answer_options = list(), option_sets = list())
  made <- new.env() # answer choices and option sets this push has already added
  push <- new.env()
  push$summary <- list(
    blocks = 0L, blocks_found = 0L, blocks_shared = 0L, blocks_repeated = 0L, questions = 0L, variables = 0L, variables_found = 0L,
    choices = 0L, answer_options = 0L, answer_options_found = 0L, option_sets = 0L, option_sets_found = 0L, subjects = 0L,
    time_frames = 0L, stems = 0L, linked = 0L
  )
  push$existing <- existing
  push$options <- options
  # the components another questionnaire's block could be, with their words as compared (worked out once)
  push$components <- if (!is.null(options$components)) {
    data.frame(
      component_id = options$components$component_id,
      component_type_id = options$components$component_type_id,
      words = push_words(options$components$text)
    )
  }
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
    # a row that is only its key still has to be said (a question is only its component); one the database
    # numbers (a new option set) is said with no column at all
    if (length(rows) == 0) {
      own <- grepl(paste0("^new:", table, ":"), as.character(key[[1]]))
      rows <- list(if (own) edit_item(table, identifier, "Insert", NULL, NA, NA) else edit_item(table, identifier, "Insert", names(key)[1], NA, key[[1]]))
    }
    items <<- c(items, rows)
  }
  # A change to a row that is there: only a reused question's stem (push_link_reused()).
  push$update <- function(table, key, column, old, new) {
    items <<- c(items, list(edit_item(table, edit_identifier(key), "Update", column, old, new)))
  }
  push$items <- function() items
  # A row of the kind, by `key`: the first time it is come to, what it is; `uses` are added each time.
  push$note <- function(kind, key, text = NULL, detail = NULL, status = NULL, is_new = NULL, uses = 0L) {
    key <- paste0("k", key) # never empty, so it names its entry
    entry <- listed[[kind]][[key]] %||%
      list(text = text, detail = detail, status = status, is_new = is_new, uses = 0L)
    entry$uses <- entry$uses + uses
    listed[[kind]][[key]] <<- entry
  }
  push$note_block <- function(block, status, is_new = FALSE) {
    # a group has no words of its own: nothing to list
    if (block$component_type != "Group") push$note("blocks", block$id, block$text, block$component_type, status, is_new)
  }
  push$rows <- function() {
    words <- function(value) if (length(value) == 0 || is.na(value)) "" else as.character(value)
    lapply(listed, function(entries) {
      data.frame(
        text = vapply(entries, function(entry) words(entry$text), character(1), USE.NAMES = FALSE),
        detail = vapply(entries, function(entry) words(entry$detail), character(1), USE.NAMES = FALSE),
        status = vapply(entries, function(entry) entry$status, character(1), USE.NAMES = FALSE),
        is_new = vapply(entries, function(entry) entry$is_new, logical(1), USE.NAMES = FALSE),
        uses = vapply(entries, function(entry) entry$uses, integer(1), USE.NAMES = FALSE),
        stringsAsFactors = FALSE
      )
    })
  }
  push$type_id <- function(name) ids$component_types$component_type_id[match(name, ids$component_types$name)]
  # Who a variable is about: a blank is the respondent; a name the database doesn't have yet is added once. Names
  # are the same whatever their capitals, and whether their quotes are curly or straight.
  push$subject_id <- function(name) {
    name <- trimws(straight_quotes(if (is.na(name)) respondent_subject else name))
    # the respondent's row, whichever name it has
    same <- if (is.na(blank_respondent(name))) tolower(respondent_names) else tolower(name)
    there <- ids$subjects$subject_id[tolower(trimws(straight_quotes(ids$subjects$name))) %in% same]
    if (length(there) > 0) {
      return(there[1])
    }
    key <- paste0("subject:", tolower(name))
    if (is.null(made[[key]])) {
      made[[key]] <- push$token("subject")
      push$insert("subject", list(subject_id = made[[key]]), list(name = name))
      push$summary$subjects <- push$summary$subjects + 1L
    }
    made[[key]]
  }
  # When a variable is about: none is no time frame; one the database doesn't have yet is added once.
  push$time_frame_id <- function(name) {
    name <- normal_time_frame(name)
    if (is.na(name)) {
      return(NA)
    }
    there <- ids$time_frames$time_frame_id[normal_time_frame(ids$time_frames$name) %in% name]
    if (length(there) > 0) {
      return(there[1])
    }
    key <- paste0("time_frame:", name)
    if (is.null(made[[key]])) {
      made[[key]] <- push$token("time_frame")
      push$insert("time_frame", list(time_frame_id = made[[key]]), list(name = name))
      push$summary$time_frames <- push$summary$time_frames + 1L
    }
    made[[key]]
  }
  push
}

# The variables of one placed question. A variable of a recorded questionnaire that has the MATR ID is that
# variable; the rest are added.
push_variables <- function(push, draft, block, placement, questionnaire) {
  boxes <- draft_service_children(draft, "variables", block$id)
  existing <- push$existing
  here <- if (!is.null(existing) && is.numeric(placement)) {
    existing$variables[existing$variables$placement_id == placement, , drop = FALSE]
  }
  order_so_far <- if (!is.null(here) && nrow(here) > 0) max(here$display_order, na.rm = TRUE) else 0
  for (i in seq_len(nrow(boxes))) {
    box <- boxes[i, ]
    if (!is.null(existing) && box$matr_id %in% existing$variables$matr_id) {
      elsewhere <- existing$variables[existing$variables$matr_id == box$matr_id, , drop = FALSE]
      if (!(is.numeric(placement) && placement %in% elsewhere$placement_id)) {
        stop(sprintf(
          "The MATR ID %s is already used by another question of this questionnaire. Give the variable a MATR ID of its own.",
          box$matr_id
        ), call. = FALSE)
      }
      push$summary$variables_found <- push$summary$variables_found + 1L
      push$note("variables", box$id, box$matr_id, box$label, "Already recorded", FALSE)
      next
    }
    order_so_far <- order_so_far + 1
    variable <- push$token("variable")
    push$insert("variable", list(variable_key = variable), list(
      questionnaire_id = questionnaire, placement_id = placement, matr_id = trimws(box$matr_id),
      display_order = order_so_far, label = box$label, subject_id = push$subject_id(box$subject),
      time_frame_id = push$time_frame_id(box$time_frame %||% NA)
    ))
    push$summary$variables <- push$summary$variables + 1L
    push$note("variables", box$id, box$matr_id, box$label, "New", TRUE)
    if (box$variable_type == "open_ended") {
      push$insert("open_ended", list(variable_key = variable), c(
        list(
          answer_data_type = box$answer_data_type, unit = box$unit %||% NA,
          skip_condition = box$skip_condition, skip_instruction = box$skip_instruction
        ),
        push_skip_to(push, box$skip_to_id)
      ))
      next
    }
    choices <- draft_service_children(draft, "response_options", box$id)
    push$insert("closed_ended", list(variable_key = variable), list(option_set_id = push_option_set(push, choices$answer_option)))
    for (j in seq_len(nrow(choices))) {
      choice <- choices[j, ]
      answer_option <- push_answer_option(push, choice$answer_option, uses = 1L)
      push$insert("response_option", list(variable_key = variable, answer_option_id = answer_option), c(
        list(
          display_order = j, code_value = if (is.na(choice$code_value)) NA else as.character(choice$code_value),
          skip_instruction = choice$skip_instruction
        ),
        push_skip_to(push, choice$skip_to_id)
      ))
      push$summary$choices <- push$summary$choices + 1L
      # how many of the wave's respondents gave this answer, if staff typed it
      if (!is.na(choice$respondent_count) && !is.null(push$wave)) {
        push$insert(
          "response_option_count", list(wave_id = push$wave, variable_key = variable, answer_option_id = answer_option),
          list(respondent_count = choice$respondent_count)
        )
      }
    }
  }
}

# A group's skip, as where the questionnaire places it records it: nothing for a block that isn't a group, or a
# group that doesn't skip, which leave those columns as their defaults. An empty `skip_when` is "every".
push_group_skip <- function(push, block) {
  if (!block$component_type %in% skipping_types || (is.na(block$skip_to_id) && is.na(block$skip_condition))) {
    return(list())
  }
  c(
    list(
      skip_condition = block$skip_condition, skip_when = if (is.na(block$skip_when)) "every" else block$skip_when,
      skip_instruction = block$skip_instruction
    ),
    push_skip_to(push, block$skip_to_id)
  )
}

# Where a choice, write-in or group skips to, as its row records it: the place in the questionnaire
# (skip_to_placement_id), or the end of the questionnaire (skip_to_end). Empty fields aren't sent, which leaves the
# columns as their defaults.
push_skip_to <- function(push, skip_to_id) {
  list(
    skip_to_placement_id = if (!is.na(skip_to_id) && skip_to_id != skip_to_end_id) push$placement_of[[skip_to_id]] %||% NA else NA,
    skip_to_end = if (identical(skip_to_id, skip_to_end_id)) TRUE else NA
  )
}

# The answer choice that is there for these words: the one worded so, the one they were merged into
# (answer_option_alias), or one that normal_answer() writes the same way (worded before it wrote ranges one way,
# say). Always one value: the choice's id, as a number, named with its words as the database keeps them (which
# can differ from `label`: "1 to 2" kept, "1-2" read); or an unnamed NA if there is none.
push_found_answer <- function(options, label) {
  answer_options <- options$answer_options
  found <- function(ids) {
    if (length(ids) == 0) {
      return(NULL)
    }
    stats::setNames(ids[1], answer_options$standardized_label[match(ids[1], answer_options$answer_option_id)])
  }
  found(answer_options$answer_option_id[answer_options$standardized_label %in% label]) %||%
    found(options$aliases$answer_option_id[options$aliases$alias %in% label]) %||%
    found(answer_options$answer_option_id[normal_answer(answer_options$standardized_label) %in% label]) %||%
    NA_real_
}

# The words a choice is kept with: those of the choice that is there for them, or its own, as normal_answer()
# writes them.
push_answer_words <- function(options, labels) {
  labels <- normal_answer(labels)
  vapply(labels, function(label) {
    there <- push_found_answer(options, label)
    if (is.na(there)) label else names(there)
  }, character(1), USE.NAMES = FALSE)
}

# The id (or the name) of an answer choice with these words: the one that is there, or a new one. `uses` is 1 for
# a variable's choice (a response option points at it) and 0 for a member of a set of choices. A new one that looks
# like one that is there says so (normalize_look_alikes()).
push_answer_option <- function(push, label, uses = 0L) {
  label <- normal_answer(label)
  there <- push_found_answer(push$options, label) # one id, named with the words it is kept with; or NA
  if (!is.na(there)) {
    label <- names(there) # the choice's words as kept, so it is listed and counted once whichever wording found it
    push$note("answer_options", label, label, "", "Already in the database", FALSE, uses)
  } else {
    alike <- normalize_look_alikes(label, push$options$answer_options$standardized_label)
    push$note(
      "answer_options", label, label, if (length(alike) > 0) paste0("Looks like “", alike[1], "”") else "",
      "New", TRUE, uses
    )
  }
  if (!is.na(there)) {
    found_key <- paste0("found choice:", label) # counted once, however many variables use it
    if (is.null(push$made[[found_key]])) {
      push$made[[found_key]] <- TRUE
      push$summary$answer_options_found <- push$summary$answer_options_found + 1L
    }
    return(unname(there))
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
# there, or a new one, its choices in the order read (which is its name: public.name(option_set)).
push_option_set <- function(push, labels) {
  labels <- unique(push_answer_words(push$options, labels))
  key <- paste(sort(labels), collapse = "\r")
  options <- push$options
  member_labels <- options$answer_options$standardized_label[match(
    options$members$answer_option_id,
    options$answer_options$answer_option_id
  )]
  keys <- vapply(split(member_labels, options$members$option_set_id), function(m) paste(sort(m), collapse = "\r"), character(1))
  there <- names(keys)[keys == key]
  if (is.null(push$made[[paste0("noted set:", key)]])) { # its name and choices, the first time it is come to
    push$made[[paste0("noted set:", key)]] <- TRUE
    choices <- paste(labels, collapse = " / ")
    name <- if (length(there) > 0) {
      options$option_sets$name[match(there[1], as.character(options$option_sets$option_set_id))]
    } else {
      choices
    }
    push$note(
      "option_sets", key, name, if (identical(tolower(trimws(name)), choices)) "" else choices,
      if (length(there) > 0) "Already in the database" else "New", length(there) == 0
    )
  }
  push$note("option_sets", key, uses = 1L)
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
  push$insert("option_set", list(option_set_id = name))
  for (i in seq_along(labels)) {
    push$insert(
      "answer_option_set", list(answer_option_id = push_answer_option(push, labels[i]), option_set_id = name),
      list(display_order = i)
    )
  }
  push$made[[made_key]] <- name
  push$summary$option_sets <- push$summary$option_sets + 1L
  name
}

# The stem a question shares: the one it was linked with, or its own words when they have [subject] or [time frame]
# in them (a fill-in printed for a person, "(name of twin 1)", is read as [subject]); NA for none.
push_stem_of <- function(block) {
  if (has_text(block$stem)) {
    return(block$stem)
  }
  placeholders <- c(subject_placeholder, time_frame_placeholder)
  if (any(vapply(placeholders, function(p) grepl(p, block$text %||% "", fixed = TRUE), logical(1)))) block$text else NA_character_
}

# The id (or the name) of the stem a question shares (its words, with [subject] for who it asks about): the one
# there with the same words, but for runs of spaces, or a new one; NA for a question that shares none.
push_question_stem <- function(push, stem) {
  words <- push_words(stem)
  if (!nzchar(words)) {
    return(NA)
  }
  stems <- push$options$stems
  there <- stems$question_stem_id[push_words(stems$text) == words]
  if (length(there) > 0) {
    return(there[1])
  }
  key <- paste0("stem:", words)
  if (is.null(push$made[[key]])) {
    push$made[[key]] <- push$token("question_stem")
    push$insert("question_stem", list(question_stem_id = push$made[[key]]), list(text = words))
    push$summary$stems <- push$summary$stems + 1L
  }
  push$made[[key]]
}

# A linked question whose component is there already keeps its link: a component with no stem gets the draft's (an
# Update, which the database allows an extraction only for this), and one with the same stem needs nothing. One
# linked under another stem is a sentence: linking it here would unlink it from the questions it is linked with.
push_link_reused <- function(push, block, component) {
  words <- push_words(push_stem_of(block))
  if (!identical(block$component_type, "Question") || !nzchar(words)) {
    return(invisible())
  }
  linked <- push$options$linked
  current <- linked$question_stem_id[match(component, linked$component_id)]
  if (length(current) == 1 && !is.na(current)) {
    stems <- push$options$stems
    if (identical(push_words(stems$text[match(current, stems$question_stem_id)]), words)) {
      return(invisible())
    }
    called <- draft_service_question_name(list(components = block), block$id)
    stop(sprintf(
      "%s (“%s”) is printed by another questionnaire already, linked with other questions as “%s”. Link it the same way, or unlink it.",
      paste0(toupper(substr(called, 1, 1)), substring(called, 2)), shorten(block$text, 80),
      stems$text[match(current, stems$question_stem_id)]
    ), call. = FALSE)
  }
  stem <- push_question_stem(push, push_stem_of(block)) # first: it may add the stem, which has to come before the update
  push$update("question", list(component_id = component), "question_stem_id", NA, stem)
  push$summary$linked <- push$summary$linked + 1L
  invisible()
}

# For a new wave: which blocks of the draft are blocks the questionnaire already prints. A block is
# one that is there if it has the same kind and words and sits in the same place (inside the same
# block, or at the top). Each block that is there is found once.
#
# Returns: A named numeric vector: the draft's block id -> the placement that is there.
push_match_blocks <- function(draft, existing, ids) {
  blocks <- draft$components
  placements <- existing$placements
  components <- existing$components
  of_component <- match(placements$component_id, components$component_id)
  placed <- data.frame(
    placement_id = placements$placement_id,
    parent = placements$parent_placement_id,
    order = placements$display_order,
    type = ids$component_types$name[match(components$component_type_id[of_component], ids$component_types$component_type_id)],
    text = push_words(components$text[of_component]),
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
      !placed$used & placed$type %in% block$component_type & placed$text == push_words(block$text) &
        (if (is.na(parent)) is.na(placed$parent) else !is.na(placed$parent) & placed$parent == parent)
    )
    if (length(here) == 0) next
    here <- here[which.min(placed$order[here])]
    placed$used[here] <- TRUE
    found[id] <- placed$placement_id[here]
  }
  found
}

# A block's words as they are compared: runs of spaces are one space, and none at the ends.
push_words <- function(text) squish(ifelse(is.na(text), "", text))

# The component the database has with a block's kind and words (but for runs of spaces), to place here instead of a
# copy, or NULL. There is one at most, and one Group (which has no words) shared by every group.
push_shared_component <- function(push, block) {
  components <- push$components
  if (is.null(components)) {
    return(NULL)
  }
  same <- components$component_id[
    components$component_type_id %in% push$type_id(block$component_type) & components$words == push_words(block$text)
  ]
  if (length(same) == 0) NULL else min(same)
}

# Where a block another questionnaire prints is used already: "Used in KQ" (the questionnaire's abbreviation, or its
# name), or "Used by 3 questionnaires".
push_shared_status <- function(options, component_id) {
  questionnaires <- unique(options$placed$questionnaire_id[options$placed$component_id %in% component_id])
  if (length(questionnaires) == 0) {
    return("Printed by another questionnaire")
  }
  if (length(questionnaires) > 1) {
    return(paste("Used by", plural(length(questionnaires), "questionnaire")))
  }
  # its one row: all NAs if it isn't listed, and NULL with no list, which `has_text()` reads as blank either way
  listed <- options$questionnaires
  row <- if (!is.null(listed)) listed[match(questionnaires, listed$questionnaire_id), , drop = FALSE]
  called <- if (has_text(row$abbreviation)) trimws(row$abbreviation) else if (has_text(row$name)) trimws(row$name) else NA_character_
  if (is.na(called)) "Printed by another questionnaire" else paste("Used in", called)
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
  in_section <- if (is.na(parent)) is.na(placements$parent_placement_id) else placements$parent_placement_id %in% parent
  if (!any(in_section)) 0L else as.integer(max(placements$display_order[in_section], na.rm = TRUE))
}

# ------ Said in words ---------------------------------------------------------------------------
# What an extraction's rows come to, in words.
# For example "Adds 14 new blocks, 9 questions asked in the wave, 11 new variables and 20 response
# options."
# summary: From `extraction_service_push_build()`.
extraction_service_push_summary <- function(summary) {
  parts <- c(
    if (summary$blocks > 0) plural(summary$blocks, "new block"),
    if (summary$blocks_found > 0) paste0(plural(summary$blocks_found, "block"), " already there"),
    if (summary$blocks_shared > 0) paste0(plural(summary$blocks_shared, "block"), " other questionnaires print"),
    if (isTRUE(summary$blocks_repeated > 0)) paste0(plural(summary$blocks_repeated, "block"), " printed again"),
    if (summary$questions > 0) paste(plural(summary$questions, "question"), "asked in the wave"),
    if (summary$variables > 0) plural(summary$variables, "new variable"),
    if (summary$variables_found > 0) paste0(plural(summary$variables_found, "variable"), " already there"),
    if (summary$choices > 0) plural(summary$choices, "response option"),
    if (summary$answer_options > 0) plural(summary$answer_options, "new answer choice"),
    if (summary$option_sets > 0) plural(summary$option_sets, "new set of choices", plural_form = "new sets of choices"),
    if (summary$subjects > 0) plural(summary$subjects, "new subject"),
    if (isTRUE(summary$time_frames > 0)) plural(summary$time_frames, "new time frame"),
    if (isTRUE(summary$stems > 0)) plural(summary$stems, "new question stem"),
    if (isTRUE(summary$linked > 0)) paste(plural(summary$linked, "question"), "already there linked")
  )
  paste0("Adds ", word_list(parts), ".")
}

# ------ The preview -------------------------------------------------------------------------------
# What an extraction's change request comes to, to show before it is submitted and while it waits.
# Pure: the rows are worked out from what is given (`context` and `existing` are read by the caller),
# and nothing is written.
# draft: The questionnaire.
# context: From `extraction_service_push_context()`.
# existing: From `extraction_service_push_existing()`.
# Returns: A list for `extraction_request_ui()`: `where` and `what` (lists of cards), `summary` (a
#   sentence), `problem` (why the rows can't be worked out, or `NULL`) and `unchecked`.
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
  preview$what <- extraction_service_what_cards(built$summary, built$rows)
  preview
}

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

# The cards for what an extraction adds: for each kind of row that can be kept once and used again, how many
# are new and how many are already in the database. How often each is used is in its list (`request_rows_ui()`). A wave's
# questions and a variable's response options aren't among them: there is a new one for each question and
# each choice, so they only say how long the questionnaire is (the sentence above the cards counts them).
# summary: From `extraction_service_push_build()`.
# rows: Each row the cards count, from `extraction_service_push_build()`, to list under them; `NULL` for none.
# Returns: A list of cards (see `request_card_ui()`); a card is NEW when its rows are added. Each has `rows` (a
#   data frame of `text`, `detail`, `status`, `is_new` and `uses`, or `NULL`) and `columns`, what its rows'
#   `text`, `detail` and `uses` are called.
extraction_service_what_cards <- function(summary, rows = NULL) {
  count <- function(n) format(n, big.mark = ",")
  card <- function(kind, n_new, n_found, title, found_note, subtitle = "", listed = NULL, columns = character()) {
    list(
      kind = kind, title = title, subtitle = subtitle,
      facts = c(if (n_new > 0) paste(count(n_new), "new"), if (n_found > 0) paste(count(n_found), found_note)),
      is_new = n_new > 0, rows = listed, columns = columns
    )
  }
  blocks <- card(
    "Blocks", summary$blocks, summary$blocks_found,
    plural(summary$blocks + summary$blocks_found + summary$blocks_shared + (summary$blocks_repeated %||% 0L), "block", big_mark = ","),
    "already in the questionnaire",
    listed = rows$blocks, columns = c(text = "Words", detail = "Kind")
  )
  if (summary$blocks_shared > 0) {
    blocks$facts <- c(blocks$facts, paste(count(summary$blocks_shared), "shared with other questionnaires"))
    blocks$is_new <- TRUE # each is placed in the questionnaire
  }
  if (isTRUE(summary$blocks_repeated > 0)) {
    blocks$facts <- c(blocks$facts, paste(count(summary$blocks_repeated), "printed again in this questionnaire"))
    blocks$is_new <- TRUE
  }
  answers <- card(
    "Answer choices", summary$answer_options, summary$answer_options_found,
    plural(summary$answer_options + summary$answer_options_found, "different answer choice", big_mark = ","),
    "already in the database",
    listed = rows$answer_options, columns = c(text = "Words", uses = "Response options")
  )
  sets <- card(
    "Sets of choices", summary$option_sets, summary$option_sets_found,
    plural(summary$option_sets + summary$option_sets_found, "set of choices", plural_form = "sets of choices", big_mark = ","),
    "already in the database",
    listed = rows$option_sets, columns = c(text = "Name", detail = "Choices", uses = "Variables")
  )
  list(
    blocks,
    card(
      "Variables", summary$variables, summary$variables_found,
      plural(summary$variables + summary$variables_found, "variable", big_mark = ","), "already recorded",
      listed = rows$variables, columns = c(text = "MATR ID", detail = "Label")
    ),
    answers,
    sets
  )
}

# ------ Sending it ----------------------------------------------------------------------------------
# The one-line description of an extraction's change request, for its title.
# catalog: The live tables the pages pick from.
# Returns: A string, e.g. `"Add Adult Health and Personality (AHP) · Wave 3"`.
extraction_service_push_title <- function(record, catalog) {
  paste("Add", extraction_service_title(record, catalog))
}

# Submit an extraction's change request.
# It is sent to Change Requests, whoever submits it: its Basic Info (`info`) is filled in there. Then
# one other staff member approves it, which applies it; an ADMIN can add their own without approval.
# The questionnaire has to be saved first (the page saves it); what is sent is the saved copy.
# me: Who is submitting it: `list(staff_id, name, role)`.
# reviewer: The staff id of the person asked to review it, or `NULL`.
# Returns: A list: `applied` (always `FALSE`: it waits on Change Requests), `history_id` (`NULL`) and
#   `record` (as saved, with when it was `submitted`, which the database records). An error with a
#   sentence if the database refused.
extraction_service_push <- function(connection, record, me, catalog, reviewer = NULL) {
  asked <- if (!is.null(reviewer) && nzchar(as.character(reviewer))) list(id = as.numeric(reviewer))
  sent <- utils::modifyList(
    record,
    list(
      summary = extraction_service_push_title(record, catalog), status = "Awaiting Review",
      requested_reviewer = asked
    ),
    keep.null = TRUE
  )
  saved <- extraction_model_save(
    connection, sent, c("status", "summary", "requested_reviewer"), me$staff_id,
    expected_revision = record$revision, new_revision = FALSE
  )
  list(applied = FALSE, history_id = NULL, record = saved)
}

# Save an extraction's Basic Info: where it goes in the database.
# Filled in on Change Requests. It is a change to the request like any other: it makes a new
# revision, and whoever saves it can't be the one who approves it.
# record: The request as the person read it.
# me: Who is saving it: `list(staff_id, name, role)`.
# Returns: The record as saved. An error with a sentence if the database refused, e.g. because the
#   request was changed after the person opened it.
extraction_service_save_info <- function(connection, record, info, me, catalog) {
  placed <- utils::modifyList(record, list(info = info), keep.null = TRUE)
  described <- utils::modifyList(placed, list(summary = extraction_service_push_title(placed, catalog)), keep.null = TRUE)
  extraction_model_save(connection, described, c("summary", "info"), me$staff_id, expected_revision = record$revision)
}
