# ------ Wording things one way --------------------------------------------------------------
# What the Edit Extraction page's Normalize dialog offers, before anything is added to the database:
#
#   Answer choices  a choice whose words aren't in the database but look like a choice's there ("yes most of
#                   the time" for "yes, most of the time"), or were merged into one on Make an edit
#                   (answer_option_alias), can use that choice's words instead. normal_answer() already writes
#                   ranges, dashes and some abbreviations one way, so those never differ.
#   Questions       questions that differ only in who they ask about ("Has Twin A ever repeated a grade?" and
#                   "Has Twin B ever repeated a grade?"), or that are worded the same in parts about different
#                   people, can be linked. Each keeps its own words, number and variables; linking gives them a
#                   stem with [subject] for who they ask about (subject_placeholder), sets their variables'
#                   subjects to who each asks about, and writes a choice that names its variable's subject with
#                   [subject] too ("describes [subject] very well"), so both share one set of choices.
#
# Who a question can ask about is a subject the database has, or one a variable of the draft names, found in its
# words as whole words, whatever their capitals ("Twin A"). The respondent isn't: they are a blank.

#' Answer choices that look like these words: worded the same but for punctuation, capitals, "&" and plurals.
#'
#' @param label An answer choice's words.
#' @param labels The answer choices to look in.
#' @return The ones that look like it, not `label` itself.
normalize_look_alikes <- function(label, labels) {
  labels <- unique(labels[!is.na(labels)])
  if (length(label) != 1 || is.na(label) || length(labels) == 0) {
    return(character())
  }
  labels[normalize_loose_words(labels) == normalize_loose_words(label) & labels != label]
}

# Words as they are compared to find look-alikes: as normal_answer() writes them, "&" as "and", without
# punctuation, and without the "s" at the end of a longer word.
normalize_loose_words <- function(text) {
  text <- gsub("&", " and ", normal_answer(text), fixed = TRUE)
  text <- gsub("[^a-z0-9 \\[\\]]", " ", text, perl = TRUE)
  text <- trimws(gsub("[[:space:]]+", " ", text))
  gsub("\\b([a-z]{3,})s\\b", "\\1", text, perl = TRUE)
}

#' The choices of a draft that could be worded as others are.
#'
#' A choice the database doesn't have whose words were merged into one of its choices, or that look like one of
#' them or like another choice of the draft that more variables offer.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param answer_options The answer choices' words in the database.
#' @param aliases The wordings merged into them: a data frame of `alias` and `label` (the choice's words).
#' @return A data frame: `from` (the draft's words), `uses` (how many of its choices have them), `to` (the words to
#'   use instead), `why` ("merged" or "alike") and `in_database` (whether `to` is a choice the database has).
normalize_service_choice_suggestions <- function(draft, answer_options, aliases = NULL) {
  words <- draft$response_options$answer_option
  words <- words[!is.na(words)]
  uses <- table(words)
  none <- data.frame(from = character(), uses = integer(), to = character(), why = character(), in_database = logical())
  rows <- lapply(names(uses), function(word) {
    if (word %in% answer_options) {
      return(NULL)
    }
    merged <- aliases$label[aliases$alias %in% word]
    if (length(merged) > 0) {
      return(data.frame(from = word, uses = as.integer(uses[[word]]), to = merged[1], why = "merged", in_database = TRUE))
    }
    alike <- normalize_look_alikes(word, answer_options)
    if (length(alike) > 0) {
      return(data.frame(from = word, uses = as.integer(uses[[word]]), to = alike[1], why = "alike", in_database = TRUE))
    }
    # another choice of the draft, offered more often (or as often, and first in the alphabet)
    others <- normalize_look_alikes(word, names(uses))
    others <- others[!others %in% answer_options]
    better <- others[uses[others] > uses[[word]] | (uses[others] == uses[[word]] & others < word)]
    if (length(better) > 0) {
      best <- better[order(-as.integer(uses[better]), better)][1]
      return(data.frame(from = word, uses = as.integer(uses[[word]]), to = best, why = "alike", in_database = FALSE))
    }
    NULL
  })
  rows <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(rows)) none else rows[order(rows$from), , drop = FALSE]
}

#' Use other words for a choice, everywhere the draft has it.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param from The words the draft has.
#' @param to The words to use instead.
#' @return The draft. An error with a sentence if a variable would then offer the same choice twice.
normalize_service_use_choice <- function(draft, from, to) {
  choices <- draft$response_options
  is_from <- choices$answer_option %in% from
  twice <- intersect(choices$variable_id[is_from], choices$variable_id[choices$answer_option %in% to])
  if (length(twice) > 0) {
    matr_id <- draft$variables$matr_id[match(twice[1], draft$variables$id)]
    stop(sprintf(
      "A variable%s offers both “%s” and “%s”, so they can't be worded the same. Change that variable's choices first.",
      if (!is.na(matr_id)) paste0(" (", matr_id, ")") else "", from, to
    ), call. = FALSE)
  }
  draft$response_options$answer_option[is_from] <- to
  draft
}

# ------ Questions about different people ----------------------------------------------------------------------
# Whole-word, any-capitals patterns for subjects' names, the longest first (so "Twin A" is found before "Twin").
# A name of one letter isn't looked for: it would be found as a word in nearly every question ("a").
normalize_subject_patterns <- function(subjects) {
  subjects <- unique(trimws(subjects[!is.na(subjects) & nchar(gsub("[[:space:]]", "", subjects)) > 1]))
  subjects <- subjects[order(-nchar(subjects))]
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", subjects)
  # recycle0: no subjects, no patterns (not one that matches nothing in particular)
  stats::setNames(paste0("(?i)(?<![[:alnum:]])", gsub(" +", "\\\\s+", escaped), "(?![[:alnum:]])", recycle0 = TRUE), subjects)
}

# Which subjects some words name: one character vector for each text.
normalize_named <- function(texts, patterns) {
  lapply(texts, function(text) {
    if (is.na(text)) {
      return(character())
    }
    found <- character()
    for (name in names(patterns)) {
      if (grepl(patterns[[name]], text, perl = TRUE)) {
        found <- c(found, name)
        text <- gsub(patterns[[name]], " ", text, perl = TRUE) # "Twin A" isn't then found again as "Twin"
      }
    }
    found
  })
}

# Words with every mention of a subject as [subject].
normalize_with_placeholder <- function(text, subject, patterns) {
  gsub(patterns[[subject]], subject_placeholder, text, perl = TRUE)
}

#' Words with [subject] filled in.
#'
#' @param text A question stem or an answer choice.
#' @param subject Who it is about: a subject's name; blank for the respondent.
#' @return `text` with [subject] as `subject` (or "the respondent").
normalize_service_fill <- function(text, subject) {
  subject <- if (is.na(subject) || !nzchar(trimws(subject))) "the respondent" else trimws(subject)
  gsub(subject_placeholder, subject, text, fixed = TRUE)
}

#' The questions of a draft that differ only in who they ask about.
#'
#' Questions whose words are the same once each names who it asks about as [subject], each naming someone else;
#' or worded the same, where each is in a part about someone else (its section's heading, or failing that the
#' nearest block before it, names one subject, and they all differ).
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param subjects Who a variable can be about besides the respondent (the database's subjects); those the draft's
#'   variables name are added.
#' @return A list of groups, in printed order, each a list: `stem` (the words they share), `ids` (the questions' ids,
#'   in printed order), `subjects` (who each asks about) and `linked` (whether they share that stem already).
normalize_service_question_groups <- function(draft, subjects) {
  patterns <- normalize_subject_patterns(c(subjects, blank_respondent(draft$variables$subject)))
  if (length(patterns) == 0) {
    return(list())
  }
  printed <- draft_service_printed_order(draft)
  blocks <- draft$components[match(printed, draft$components$id), , drop = FALSE]
  named <- normalize_named(blocks$text, patterns)
  is_question <- blocks$component_type %in% "Question" & !is.na(blocks$text)
  one <- lengths(named) == 1
  stems <- rep(NA_character_, nrow(blocks))
  stems[is_question & one] <- vapply(which(is_question & one), function(i) {
    normalize_with_placeholder(blocks$text[i], named[[i]], patterns)
  }, character(1))
  stems[is_question & lengths(named) == 0] <- blocks$text[is_question & lengths(named) == 0]
  about <- ifelse(one, vapply(named, function(n) n[1] %||% NA_character_, character(1)), NA_character_)
  keys <- tolower(push_words(stems))
  groups <- list()
  for (key in unique(keys[!is.na(stems) & duplicated(keys) & nzchar(keys)])) {
    members <- which(keys == key & !is.na(stems))
    who <- about[members]
    if (all(is.na(who))) who <- vapply(members, function(i) normalize_context_subject(draft, blocks, named, i), character(1))
    if (anyNA(who) || anyDuplicated(tolower(who)) > 0) next
    ids <- blocks$id[members]
    stem <- stems[members[1]]
    groups[[length(groups) + 1]] <- list(
      stem = stem, ids = ids, subjects = who,
      linked = all(push_words(draft$components$stem[match(ids, draft$components$id)]) == push_words(stem))
    )
  }
  groups
}

# Who the part a block is in is about: the subject its innermost section's heading names, or failing that the one
# the nearest block before it names; NA if none names just one. `blocks` are in printed order, and `named` says
# whom each names.
normalize_context_subject <- function(draft, blocks, named, i) {
  for (section in draft_service_ancestors(draft, blocks$id[i])) {
    at <- match(section, blocks$id)
    if (length(named[[at]]) == 1) {
      return(named[[at]])
    }
  }
  before <- rev(seq_len(i - 1))
  hit <- before[lengths(named[before]) == 1][1]
  if (is.na(hit)) NA_character_ else named[[hit]]
}

#' Link questions that differ only in who they ask about.
#'
#' Each gets the stem, each of its variables is about who it asks about, and a choice that names its variable's
#' subject has [subject] there instead, unless the variable's choices name someone else too ("twin a much more"
#' beside "twin b much more" compares them).
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param group One of `normalize_service_question_groups()`.
#' @param subjects As for `normalize_service_question_groups()`.
#' @return The draft.
normalize_service_link <- function(draft, group, subjects) {
  patterns <- normalize_subject_patterns(c(subjects, group$subjects, blank_respondent(draft$variables$subject)))
  for (k in seq_along(group$ids)) {
    id <- group$ids[k]
    subject <- group$subjects[k]
    draft$components$stem[draft$components$id %in% id] <- group$stem
    boxes <- draft$variables$id[draft$variables$component_id %in% id]
    draft$variables$subject[draft$variables$id %in% boxes] <- subject
    for (box in boxes) {
      mine <- which(draft$response_options$variable_id %in% box)
      words <- draft$response_options$answer_option[mine]
      named <- normalize_named(words, patterns)
      if (any(vapply(named, function(n) any(tolower(n) != tolower(subject)), logical(1)))) next
      says <- lengths(named) > 0
      draft$response_options$answer_option[mine[says]] <- normal_answer(vapply(words[says], function(word) {
        normalize_with_placeholder(word, names(patterns)[tolower(names(patterns)) == tolower(subject)][1], patterns)
      }, character(1)))
    }
  }
  draft
}

#' Unlink questions: they no longer share a stem. Their subjects and choices stay as they are.
#'
#' @param draft A draft: `list(questionnaire, components, variables, response_options)`; see the top of `service_draft.R`.
#' @param ids The questions' ids.
#' @return The draft.
normalize_service_unlink <- function(draft, ids) {
  draft$components$stem[draft$components$id %in% ids] <- NA_character_
  draft
}
