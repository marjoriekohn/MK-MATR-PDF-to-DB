# ------ Extraction drafts ------------------------------------------------------
# The extraction pipeline returns a questionnaire as nested JSON: sections and groups hold
# blocks, questions hold variables, and variables hold choices (response options). The Edit
# Extraction page edits three flat tables instead, so moving a block only changes its parent_id and
# display_order, the same way QUESTIONNAIRE_COMPONENT stores it:
#
#   components        one row per block, pointing at its section or group (parent_id)
#   variables         one row per variable, pointing at its question
#   response_options  one row per choice, pointing at its variable
#
# Rows get ids when a draft is loaded (c1, v1, o1...), in printed order. Ids aren't saved, but each
# row's `ref` is: the id the row had when the draft was loaded from what the AI returned. It stays
# with the row however the draft is edited, so the reviewed copy can be compared with the AI's answer
# row by row (service_draft_record.R). A row staff added has none.
#
# Two kinds of JSON are loaded. What the reading job returns (the AI's answer, `ai_original`) has no
# `schema_version`: its questions have the number printed beside them (`printed_number`) and name
# their parent by its printed number. The reviewed copy the page saves has `schema_version`, and
# names a question's parent by its place among the questions (`question_number`), which, unlike a
# printed number, never repeats.
#
# A question has two numbers. `question_number` is its place among the questions (1, 2, 3...), worked
# out by the site and kept up to date as blocks move (draft_service_renumber_questions()); it is the
# number the wave records (`wave_question.question_number`). `printed_number` is the number printed
# beside it ("1a", "12"), read off the PDF and corrected by staff; it can repeat (each section can
# start again at 1) or be missing, and the wave keeps it too (`wave_question.printed_number`).
# Sections and groups aren't counted.
#
# Each block also notes the PDF page it was read from (`page`: the first page of the range the reading
# job sent with it), so the page can open the PDF at it. Blocks staff add take the page
# of the block they're added next to.

# The version of the reviewed copy's JSON. A copy without one is what the reading job returned.
draft_schema_version <- 2L

# The kinds of block that hold others:
#   Section  a part of the questionnaire under a printed heading (its words).
#   Grid     questions that share a printed lead-in which is part of each one: the lead-in is its words, and each
#            question's words are the rest ("In the past 4 weeks, how often" + "have you felt calm and peaceful?").
#            It holds only its questions, with their instructions and definitions.
#   Group    blocks that belong together with no words of their own: an instruction or definition with the
#            questions it is about, or questions that belong together. Each question in it is whole on its own.
# A component is a wording kept once, so every group (having none) is the one Group component, placed wherever a
# questionnaire groups blocks; a group's place and order are its placement's.
container_types <- c("Section", "Grid", "Group")

# What each kind of container can't hold: a section is never inside a group or a grid, and a grid holds only its
# questions and what explains them.
not_held_by <- list(Section = character(), Group = "Section", Grid = c("Section", "Group", "Grid"))

# Whether blocks of these kinds can sit in blocks of those.
# outer: The kinds of what would hold them; NA for the top level.
# inner: Their kinds.
can_hold <- function(outer, inner) {
  vapply(seq_along(inner), function(k) {
    holder <- outer[k]
    is.na(holder) || (is_container(holder) && !inner[k] %in% not_held_by[[holder]])
  }, logical(1))
}

# Why a block of kind `inner` can't be in one of kind `outer`, in a sentence.
cant_hold_message <- function(outer) {
  if (identical(outer, "Grid")) {
    "A grid holds only its questions, with their instructions and definitions. Move this out of it."
  } else {
    "A section can't be inside a group. Move it out, or make it a group."
  }
}

# An answer choice as the database keeps it (answer_option.standardized_label): lower case, with straight quotes,
# one space between words, and no question mark, or full stop or colon at the end ("Yes." and "YES" are "yes").
# The printed words aren't kept: a choice is its standard wording, so wordings that only look different are
# written one way: a range of numbers with a hyphen ("1 to 2", "1 - 2", "1–2" and "between 1 and 2" are "1-2"), "4+"
# as "4 or more", a
# dash as a hyphen, abbreviations in full (answer_abbreviations), and a choice printed as "?" as "uncertain" (what
# the forms that print it say it means). What qualifies a choice comes after it in brackets (answer_qualified()), so
# the choices that share an answer are found together. The database checks less (private.normal_answer()), so
# what this gives always passes.
#
# The printed words aren't needed: the dashboard links each questionnaire's PDF, and the database is a guide to
# finding questions, not a copy of the page.
normal_answer <- function(text) {
  text <- straight_quotes(as.character(text))
  text <- gsub("[\u2013\u2014]", "-", text)
  text[grepl("^[[:space:]]*[?]+[[:space:]]*$", text)] <- "uncertain"
  text <- gsub("?", "", text, fixed = TRUE)
  text <- tolower(squish(text))
  text <- gsub("([0-9])\\+", "\\1 or more", text, perl = TRUE)
  text <- gsub("\\bbetween ([0-9][0-9.,]*) and (?=[0-9])", "\\1-", text, perl = TRUE)
  text <- gsub("([0-9]) ?(?:-|to) ?(?=[0-9])", "\\1-", text, perl = TRUE)
  for (short in names(answer_abbreviations)) {
    text <- gsub(paste0("(?<![[:alnum:]/])", short, "(?![[:alnum:]/])"), answer_abbreviations[[short]], text, perl = TRUE)
  }
  whole <- text %in% names(answer_letters)
  text[whole] <- answer_letters[text[whole]]
  text <- answer_qualified(sub("[.:[:space:]]+$", "", text))
  text[!is.na(text) & !nzchar(text)] <- NA
  text
}

# Curly quotes as straight ones ("Respondent’s Siblings" is "Respondent's Siblings"), so words and names typed or
# read either way are the same words.
straight_quotes <- function(text) {
  text <- gsub("[\u2018\u2019]", "'", text)
  gsub("[\u201c\u201d]", '"', text)
}

# A questionnaire's words as the database keeps them (a block's words, a variable's label):
# lower case, so the same words are written one way however the form capitalised them ("NEVER", "Section A:
# Your Health"), and without a figure in brackets that only repeats the number word before it ("more than five
# (5) children" is "more than five children"). Names the database lists (subjects), printed numbers and MATR IDs
# aren't a form's words, and keep their capitals.
normal_text <- function(text) {
  if (is.null(text)) {
    return(NULL)
  }
  text <- tolower(straight_quotes(as.character(text)))
  for (i in seq_along(number_words)) {
    text <- gsub(sprintf("\\b%s \\(%d\\)", number_words[i], i), number_words[i], text, perl = TRUE)
  }
  text
}

number_words <- c(
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty"
)

# Abbreviations written out wherever they are whole words in a choice ("R first" is "respondent first", as TSA
# uses "R"), and letters that are a whole choice ("M" is "male").
answer_abbreviations <- c(
  r = "respondent", dk = "don't know", hh = "household", "n/a" = "not applicable",
  hrs = "hours", hr = "hour", mins = "minutes", wks = "weeks", wk = "week", mos = "months", yrs = "years", yr = "year"
)
answer_letters <- c(m = "male", f = "female")

# Units printed by a write-in's box, written out (with answer_abbreviations' "yrs", "hrs" and the rest).
unit_abbreviations <- c(lbs = "pounds", lb = "pounds", ozs = "ounces", oz = "ounces", ft = "feet", kgs = "kilograms", kg = "kilograms", cm = "centimeters")

# The unit a write-in's number is in, written one way.
# Lower case, abbreviations written out and no full stop at the end ("yrs." is "years", "LBS" is "pounds"), so the
# answers in the same unit are found together across forms, and the forms in other units ("pounds" and
# "kilograms") can be told apart.
# text: Units as printed or typed.
# Returns: The units, with `NA` for a blank.
normal_unit <- function(text) {
  text <- squish(normal_text(as.character(text)))
  text <- sub("[.:]+$", "", text)
  for (short in names(c(answer_abbreviations, unit_abbreviations))) {
    long <- c(answer_abbreviations, unit_abbreviations)[[short]]
    text <- gsub(paste0("(?<![[:alnum:]/])", short, "(?![[:alnum:]/])"), long, text, perl = TRUE)
  }
  text[!is.na(text) & !nzchar(text)] <- NA
  text
}

# A choice as "answer [qualifier]": "yes, all episodes" is "yes [all episodes]", "no - never" is "no [never]",
# "1-2 hours" is "1-2 [hours]" and "biological (blood)" is "biological [blood]". Only a short answer (up to three
# words, not just numbers) is split off at a comma, semicolon or spaced dash, and not before "or" ("never, or almost
# never" is one answer); an amount (a number, range, "once" or "twice", perhaps "or more", perhaps after "less than",
# "about", "at least", ...) is split from what it counts ("less than 21 [days]", "4 or more [times a week]", "more than
# once [a week]"), but not from "and", "to", "of", "out" or "in" ("1 in 10"). Words with brackets already are only tidied, so writing them twice changes nothing.
# `text` is lower case, with one space between words.
answer_qualified <- function(text) {
  # an amount: 21, 2.5, 1,200, once or twice; a range of them (1-2); "or more", "or less" or "or" another (1 or 2)
  number <- "(?:(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?|once|twice)"
  amount <- paste0(number, "(?:-", number, ")*(?: or (?:more|less|fewer|", number, "))?")
  rules <- c(
    "^([^()]*[^ (]) ?\\(([^()]+)\\)$", # a closing remark in brackets
    # a short answer, then more (a comma between digits is a thousands separator: "1,000")
    "^((?=[^,;]*[a-z])(?:[^ ,;]+ ){0,2}?[^ ,;]+)(?: ?(?:;|(?<![0-9]),|,(?![0-9])) ?| - )(?!or\\b)(\\S.*)$",
    # an amount, then what it counts: "1-2 hours", "less than 21 days", "about 1,200 dollars"
    paste0(
      "^((?:(?:less|more|fewer|greater) than |at (?:least|most) |up to |about |approximately |around |nearly |almost |over ",
      "|under |within )?", amount, ") (?!(?:or|and|to|of|out|in)\\b)([a-z].*)$"
    )
  )
  plain <- !is.na(text) & !grepl("[][]", text)
  # the same amount again in other units says nothing more: "21-35 days (3-5 weeks)" is "21-35 days"
  restated <- "^(.*[0-9].*[^ (]) ?\\([0-9][0-9.,/]*(?:-[0-9][0-9.,/]*)? [a-z]+\\)$"
  text[plain] <- sub(restated, "\\1", text[plain], perl = TRUE)
  for (rule in rules) {
    split <- plain & grepl(rule, text, perl = TRUE)
    text[split] <- sub(rule, "\\1 [\\2]", text[split], perl = TRUE)
    plain <- plain & !split
  }
  # typed with brackets: one space before them, none inside, no full stop or colon at the end of the qualifier; and a
  # qualifier that only repeats the answer ("don't know (DK)") is left out
  text <- sub("^(.*[^ ]) ?\\[ ?([^][]*?)[.: ]*\\]$", "\\1 [\\2]", text, perl = TRUE)
  sub("^(.+) \\[\\1\\]$", "\\1", text, perl = TRUE)
}

# A time frame in words, as the forms print it: "in your lifetime", "during the last 12 months", "over the past
# year", "within the past thirty days". In a question's words it starts with "in", "during", "over", "within" or
# "for" (so "on the last day you smoked" isn't one); a field that holds only a time frame can leave that out ("past
# week"). `normal_time_frame()` writes each of them one way.
time_frame_unit <- "(day|week|month|year)s?"
time_frame_lifetime <- "(?:ever\\s+)?(?:in|during|over|throughout)\\s+your\\s+(?:entire\\s+|whole\\s+)?life(?:time)?"
time_frame_past <- paste0(
  "(?:the\\s+)?(?:past|last|previous)\\s+(?:([0-9]+|", paste(number_words, collapse = "|"), ")\\s+)?", time_frame_unit
)
time_frame_preposition <- "(?:in|during|over|within|for)\\s+"
time_frame_pattern <- paste0("(?:", time_frame_lifetime, "|", time_frame_preposition, time_frame_past, ")")

# Time frames written one way.
# The time a variable's answer is about, as the database keeps it (a tag), so the questions about the same time are
# found together whatever words the forms use: "lifetime" ("in your life", "during your whole lifetime"), and
# "past" a number of days, weeks, months or years, the number in words up to nine and in figures from ten
# ("during the last 12 months" and "over the past twelve months" are "past 12 months"; "in the last year" is "past
# year"; "within the last 6 weeks" is "past six weeks"). Any other is kept as typed, in lower case ("since your
# last birthday").
# text: Time frames as printed or typed.
# Returns: The time frames, with `NA` for a blank.
normal_time_frame <- function(text) {
  text <- squish(normal_text(as.character(text)))
  text[!is.na(text) & !nzchar(text)] <- NA
  whole <- paste0(
    "(?i)^(?:", time_frame_lifetime, "|(?:in\\s+)?your\\s+lifetime|lifetime|(?:", time_frame_preposition, ")?",
    time_frame_past, ")[.,:?]*$"
  )
  known <- !is.na(text) & grepl(whole, text, perl = TRUE)
  text[known] <- vapply(text[known], time_frame_words, character(1))
  text
}

# One printed time frame in the database's words.
time_frame_words <- function(found) {
  past <- regmatches(found, regexec(paste0("(?i)(?:past|last|previous)\\s+(?:([0-9]+|[a-z]+)\\s+)?", time_frame_unit), found, perl = TRUE))[[1]]
  if (length(past) == 0) {
    return("lifetime")
  }
  count <- tolower(past[2])
  count <- if (!nzchar(count)) 1L else if (count %in% number_words) match(count, number_words) else as.integer(count)
  unit <- tolower(past[3])
  if (count == 1) paste("past", unit) else paste("past", if (count < 10) number_words[count] else count, paste0(unit, "s"))
}

# Whether blocks of these kinds hold other blocks.
# component_types: Kinds of block.
is_container <- function(component_types) component_types %in% container_types

# The names a subject can have for the respondent: staff and the AI leave a variable's subject blank for them.
respondent_names <- c("respondent", "self")

# Who a variable is about, written one way: the relation to the respondent alone, in lower case ("twin",
# "children", "mother"), so "Respondent's Twin", "respondent's twin" and "twin" are one tag. Who the respondent is
# (a twin, a parent) is the wave's respondent type, so the dashboard can say "Twin's mother".
# subjects: Subjects as typed or read.
# Returns: The subjects, in lower case, with straight quotes and without "respondent's" before the relation; `NA`
#   for a blank.
normal_subject_words <- function(subjects) {
  subjects <- squish(normal_text(as.character(subjects)))
  subjects <- sub("^(the )?respondent'?s ", "", subjects, perl = TRUE)
  subjects[!is.na(subjects) & !nzchar(subjects)] <- NA
  subjects
}

# Who a variable is about, with the respondent as a blank.
# subjects: Subjects as typed or read; "Respondent" or "Self" in any capitals, or a blank, is the respondent.
# Returns: The subjects as `normal_subject_words()` writes them, with `NA` for the respondent.
normal_subject <- function(subjects) {
  subjects <- normal_subject_words(subjects)
  subjects[!is.na(subjects) & subjects %in% respondent_names] <- NA
  subjects
}

# The column that says which row each row sits under. display_order counts
# 1, 2, 3... among the rows that share it.
parent_columns <- c(
  components = "parent_id",
  variables = "component_id",
  response_options = "variable_id"
)

id_prefixes <- c(components = "c", variables = "v", response_options = "o")

# Every column of each table, with what a new row holds until it's filled in.
new_row_defaults <- list(
  components = data.frame(
    id = NA_character_, parent_id = NA_character_, display_order = Inf,
    component_type = NA_character_, text = NA_character_,
    question_number = NA_character_, printed_number = NA_character_,
    parent_question_id = NA_character_, checked = FALSE, ref = NA_character_, page = NA_integer_
  ),
  variables = data.frame(
    id = NA_character_, component_id = NA_character_, display_order = Inf,
    variable_type = "closed_ended", label = NA_character_, subject = NA_character_, time_frame = NA_character_,
    answer_data_type = NA_character_, unit = NA_character_, matr_id = NA_character_, ref = NA_character_
  ),
  response_options = data.frame(
    id = NA_character_, variable_id = NA_character_, display_order = Inf,
    answer_option = NA_character_, code_value = NA_character_, ref = NA_character_
  )
)

# Open an extraction's questionnaire for review.
# It opens where staff left it (their saved copy, `extraction.questionnaire`) or, if they haven't saved one,
# fresh from the reading job (`extraction.ai_original`).
# id: The extraction's id.
# Returns: A draft (see `draft_service_from_json()`). An error if reading hasn't finished.
draft_service_load <- function(connection, id) {
  json <- extraction_model_questionnaire(connection, id)
  json <- json$questionnaire %||% json$original
  if (is.null(json)) stop("This draft hasn't been read yet.", call. = FALSE)
  draft_service_from_json(json)
}

# Save staff's reviewed copy of an extraction's questionnaire.
# id: The extraction's id.
# review_edits: How this copy differs from what the AI returned (see
#   `draft_service_review_edits()`), which replaces what was kept, or `NULL` to leave it as it is.
# expected_revision: The revision the person was looking at, or `NULL` not to check.
# Returns: The extraction's revision after the save, invisibly. An error with a sentence if someone saved
#   it after `expected_revision`.
draft_service_save <- function(connection, id, draft, staff_id, review_edits = NULL, expected_revision = NULL) {
  extraction_model_save_questionnaire(connection, id, draft_service_to_json(draft), staff_id, review_edits, expected_revision)
}

# A draft with nothing in it.
# What a Manual extraction starts as: no blocks, no variables, no choices.
# Returns: A draft: `list(questionnaire, components, variables, response_options)`.
draft_service_empty <- function() {
  draft_service_from_json(list(questionnaire = list(), components = list()))
}

# How far a draft's review has got.
# Returns: A list: `blocks` and how many are `checked`, `boxes` (variables) and how
#   many have a MATR ID (`matr_ids`).
draft_service_counts <- function(draft) {
  list(
    blocks = nrow(draft$components), checked = sum(draft$components$checked),
    boxes = nrow(draft$variables), matr_ids = sum(!is.na(draft$variables$matr_id))
  )
}

# How much of a draft has been checked, as a whole percentage.
# The Edit Extraction page's "N% human-reviewed" and the Extraction Drafts cards both show it.
# counts: From `draft_service_counts()`.
# Returns: A whole number from 0 to 100, rounded down; 0 for a draft with no blocks.
draft_service_percent_checked <- function(counts) {
  if (counts$blocks > 0) as.integer(floor(100 * counts$checked / counts$blocks)) else 0L
}

# ------ JSON to tables ---------------------------------------------------------
# Turn the questionnaire JSON into the three tables the Edit Extraction page edits.
# Rows get ids when a draft is loaded (c1, v1, o1...); ids aren't saved, but `ref` is (see the top of
# this file). Positions are numbered 1, 2, 3... within each parent, the questions by their place among
# the questions, and choices without a code are coded. A variable about the respondent has a blank
# subject, however the JSON says it. Fields the JSON no longer has (skips, conditions, stems, respondent
# counts, from older drafts) are left behind.
# json: The questionnaire as nested lists: `questionnaire` and `components`, and `schema_version`
#   for a reviewed copy (without it, it is what the reading job returned).
# Returns: A draft: `list(questionnaire, components, variables, response_options)`.
draft_service_from_json <- function(json) {
  reviewed <- !is.null(json$schema_version)
  blocks <- flatten_blocks(json$components)
  boxes <- flatten_children(blocks, "variables")
  choices <- Filter(function(choice) !isFALSE(choice[["printed"]]), flatten_children(boxes, "response_options"))
  block_ids <- ids_for("c", seq_along(blocks))
  # The AI's answer names a parent by its printed number; the reviewed copy by its place.
  parent_by <- if (reviewed) pluck(blocks, "question_number") else pluck(blocks, "printed_number")
  # What the AI returned is the starting point that every later copy is compared with: its rows' refs
  # are their ids.
  refs <- function(rows, ids) if (reviewed) pluck(rows, "ref") else ids

  draft <- list(
    questionnaire = json$questionnaire,
    components = data.frame(
      id = block_ids,
      parent_id = ids_for("c", pluck(blocks, ".parent", NA_integer_)),
      display_order = as.numeric(seq_along(blocks)),
      component_type = pluck(blocks, "component_type"),
      text = pluck(blocks, "text"),
      question_number = rep(NA_character_, length(blocks)),
      printed_number = pluck(blocks, "printed_number"),
      parent_question_id = find_parent_questions(parent_by, pluck(blocks, "parent_question"), block_ids),
      checked = pluck(blocks, "checked", FALSE),
      ref = refs(blocks, block_ids),
      page = pluck(blocks, "page", NA_integer_)
    ),
    variables = data.frame(
      id = ids_for("v", seq_along(boxes)),
      component_id = ids_for("c", pluck(boxes, ".parent", NA_integer_)),
      display_order = as.numeric(seq_along(boxes)),
      variable_type = pluck(boxes, "variable_type"),
      label = pluck(boxes, "label"),
      subject = normal_subject(pluck(boxes, "subject")),
      time_frame = normal_time_frame(pluck(boxes, "time_frame")),
      answer_data_type = pluck(boxes, "answer_data_type"),
      matr_id = pluck(boxes, "matr_id"),
      ref = refs(boxes, ids_for("v", seq_along(boxes))),
      unit = normal_unit(pluck(boxes, "unit"))
    ),
    response_options = data.frame(
      id = ids_for("o", seq_along(choices)),
      variable_id = ids_for("v", pluck(choices, ".parent", NA_integer_)),
      display_order = as.numeric(seq_along(choices)),
      answer_option = normal_answer(pluck(choices, "answer_option")),
      code_value = pluck(choices, "code_value"), # text: a code can be a letter (a, b, c)
      ref = refs(choices, ids_for("o", seq_along(choices)))
    )
  )
  # Rows were numbered in printed order; make that 1, 2, 3... within each parent.
  for (table in names(parent_columns)) draft <- renumber(draft, table)
  draft_service_renumber_questions(default_codes(draft))
}

# Every block in the order a respondent meets it.
# blocks: A list of blocks, each possibly holding `children`.
# parent: The position of the section the blocks sit in; `NA` for the top level.
# flat: The blocks found so far (used while it recurses).
# Returns: A list of blocks, each noting the position of its section as `.parent`.
flatten_blocks <- function(blocks, parent = NA_integer_, flat = list()) {
  for (block in blocks) {
    flat[[length(flat) + 1]] <- c(block, list(.parent = parent))
    flat <- flatten_blocks(block$children, parent = length(flat), flat = flat)
  }
  flat
}

# The children listed in `field` of every row, each noting its row's position (.parent).
flatten_children <- function(rows, field) {
  children <- lapply(seq_along(rows), function(i) {
    lapply(rows[[i]][[field]], function(child) c(child, list(.parent = i)))
  })
  unlist(children, recursive = FALSE)
}

# One field from every row.
# rows: A list of rows (lists).
# field: The field's name.
# missing: What fills in for a row without the field; its type is the result's type.
# Returns: A vector with one value for each row.
pluck <- function(rows, field, missing = NA_character_) {
  vapply(
    rows,
    function(row) as.vector(row[[field]] %||% missing, typeof(missing)),
    missing
  )
}

ids_for <- function(prefix, positions) {
  ids <- paste0(prefix, positions, recycle0 = TRUE) # no positions, no ids
  ids[is.na(positions)] <- NA
  ids
}

# The JSON names the question a question depends on by its number (printed, in the AI's answer, which
# can repeat from section to section; its place, in a reviewed copy). The draft keeps that question's
# id instead, so renumbering never breaks the link. A number means the nearest question before it with
# that number, or failing that the first one after it (which the checks then point out).
find_parent_questions <- function(numbers, parent_numbers, ids) {
  vapply(seq_along(parent_numbers), function(i) {
    matches <- setdiff(which(same_number(numbers, parent_numbers[i])), i)
    earlier <- matches[matches < i]
    ids[if (length(earlier)) max(earlier) else matches[1]]
  }, "")
}

same_number <- function(numbers, number) {
  !is.na(numbers) & !is.na(number) &
    tolower(trimws(numbers)) == tolower(trimws(number))
}

# ------ Tables to JSON ---------------------------------------------------------
# The reviewed copy: the same shape the pipeline returns, plus `schema_version`, and what review adds:
# `question_number` (its place) and `checked` on each block, `matr_id` on each variable, `code_value` on
# each choice and `ref` on each row the AI returned. A question names the question it depends on
# (`parent_question`) by that question's place among the questions.
# Fields with nothing in them are left out, as in tests/testthat/fixtures/gna_gold.json.
draft_service_to_json <- function(draft) {
  list(
    schema_version = draft_schema_version,
    questionnaire = draft$questionnaire,
    components = blocks_json(draft, parent_id = NA)
  )
}

blocks_json <- function(draft, parent_id) {
  map_rows(draft_service_children(draft, "components", parent_id), function(block) {
    json <- list(
      component_type = block$component_type,
      display_order = as.integer(block$display_order),
      text = block$text,
      page = block$page
    )
    if (is_container(block$component_type)) {
      json$children <- blocks_json(draft, block$id)
    }
    if (block$component_type == "Question") {
      json$question_number <- block$question_number
      json$printed_number <- block$printed_number
      json$parent_question <- question_number_of(draft, block$parent_question_id)
      json$variables <- boxes_json(draft, block$id)
    }
    json$checked <- block$checked
    json$ref <- block$ref
    without_blanks(json)
  })
}

boxes_json <- function(draft, component_id) {
  map_rows(draft_service_children(draft, "variables", component_id), function(box) {
    json <- list(
      variable_type = box$variable_type,
      display_order = as.integer(box$display_order),
      label = box$label,
      subject = box$subject,
      time_frame = box$time_frame,
      matr_id = box$matr_id
    )
    if (box$variable_type == "closed_ended") {
      json$response_options <- choices_json(draft, box$id)
    } else {
      json$answer_data_type <- box$answer_data_type
      json$unit <- box$unit
    }
    json$ref <- box$ref
    without_blanks(json)
  })
}

choices_json <- function(draft, variable_id) {
  map_rows(draft_service_children(draft, "response_options", variable_id), function(choice) {
    without_blanks(list(
      display_order = as.integer(choice$display_order),
      answer_option = choice$answer_option,
      code_value = choice$code_value,
      ref = choice$ref
    ))
  })
}

# A JSON object without its empty fields.
# A missing field reads back as empty, so nothing is lost.
# fields: A named list.
# Returns: `fields` without the ones that are `NULL` or a single `NA`.
without_blanks <- function(fields) {
  blank <- vapply(fields, function(value) {
    is.null(value) || (is.atomic(value) && length(value) == 1 && is.na(value))
  }, logical(1))
  fields[!blank]
}

# ------ Finding things in a draft ----------------------------------------------
# The rows of one table that sit under a parent, in order.
# parent_id: The parent's id; `NA` for the top level (components only).
# Returns: The rows of `draft[[table]]` under `parent_id`, by `display_order`.
draft_service_children <- function(draft, table, parent_id) {
  rows <- draft[[table]]
  rows <- rows[rows[[parent_columns[[table]]]] %in% parent_id, , drop = FALSE]
  rows[order(rows$display_order), , drop = FALSE]
}

# Block ids in the order a respondent meets them.
# Each block, then what is inside it.
# parent_id: Where to start; `NA` for the whole questionnaire.
# Returns: A character vector of component ids.
draft_service_printed_order <- function(draft, parent_id = NA) {
  ids <- draft_service_children(draft, "components", parent_id)$id
  as.character(unlist(lapply(ids, function(id) {
    c(id, draft_service_printed_order(draft, id))
  })))
}

# The sections a block sits inside.
# id: A component id.
# Returns: The ids of its sections, innermost first; empty at the top level.
draft_service_ancestors <- function(draft, id) {
  parent_id <- draft$components$parent_id[match(id, draft$components$id)]
  if (length(parent_id) == 0 || is.na(parent_id)) {
    return(character())
  }
  c(parent_id, draft_service_ancestors(draft, parent_id))
}

# The id the next new rows of a table get.
# n: How many ids; none gives none.
# Returns: A character vector, e.g. `"c132"` after `"c131"`.
draft_service_next_id <- function(draft, table, n = 1) {
  used <- as.integer(substring(draft[[table]]$id, 2))
  # recycle0: without it, no numbers would still paste one id (the prefix alone)
  paste0(id_prefixes[[table]], max(c(0L, used)) + seq_len(n), recycle0 = TRUE)
}

# A question's place among the questions (1, 2, 3...), which links a question to the one it depends on in the JSON.
# id: Component ids.
# Returns: Their question numbers, `NA` for a block with none.
question_number_of <- function(draft, id) {
  draft$components$question_number[match(id, draft$components$id)]
}

# The number a question is called by: the one printed beside it, or, where none is printed, its
# place among the questions.
# What the outline, the preview and the messages call a question. (The wave records both numbers:
# see the top of this file.)
# blocks: Rows of a draft's `components`.
# Returns: A character vector, one number for each row; `NA` for a block that isn't a question.
draft_service_question_number <- function(blocks) {
  printed <- trimws(blocks$printed_number %||% rep(NA_character_, nrow(blocks)))
  ifelse(!is.na(printed) & nzchar(printed), printed, blocks$question_number)
}

# What goes between a question's number and its words where a list names it: "7 · Have you smoked…".
outline_number_separator <- " · "

# What a question is called in sentences: "question 1a4".
# id: A component id.
# Returns: A string; "an unnumbered question" for a question without a number.
draft_service_question_name <- function(draft, id) {
  number <- draft_service_question_number(draft$components[match(id, draft$components$id), , drop = FALSE])
  if (length(number) == 0 || is.na(number)) "an unnumbered question" else paste("question", number)
}

# Apply a function to each row of a data frame.
# rows: A data frame.
# fn: A function taking a one-row data frame.
# ...: More arguments for `fn`.
# Returns: A list with one result for each row.
map_rows <- function(rows, fn, ...) {
  lapply(seq_len(nrow(rows)), function(i) fn(rows[i, , drop = FALSE], ...))
}
