# ------ Wording answer choices one way --------------------------------------------------------
# What the Edit Extraction page's Normalize dialog offers, before anything is pushed into the database: a
# choice whose words aren't in the database but look like a choice's there ("yes most of the time" for
# "yes, most of the time"), or like another choice of the draft that more variables offer, can use that
# choice's words instead. normal_answer() already writes ranges, dashes and some abbreviations one way, so
# those never differ.

# Answer choices that look like these words: worded the same but for punctuation, capitals, "&" and plurals.
# label: An answer choice's words.
# labels: The answer choices to look in.
# Returns: The ones that look like it, not `label` itself.
normalize_look_alikes <- function(label, labels) {
  labels <- unique(labels[!is.na(labels)])
  if (length(label) != 1 || is.na(label) || length(labels) == 0) {
    return(character())
  }
  labels[normalize_loose_words(labels) == normalize_loose_words(label) & labels != label]
}

# Words as they are compared to find look-alikes: as normal_answer() writes them, "&" as "and", without
# punctuation (a qualifier's brackets too), and without the "s" at the end of a longer word.
normalize_loose_words <- function(text) {
  text <- gsub("&", " and ", normal_answer(text), fixed = TRUE)
  text <- gsub("[^a-z0-9 ]", " ", text, perl = TRUE)
  text <- squish(text)
  gsub("\\b([a-z]{3,})s\\b", "\\1", text, perl = TRUE)
}

# The choices of a draft that could be worded as others are.
# A choice the database doesn't have that looks like one of its choices, or like another choice of the draft
# that more variables offer.
# answer_options: The answer choices' words in the database.
# Returns: A data frame: `from` (the draft's words), `uses` (how many of its choices have them), `to` (the words to
#   use instead), `why` ("alike") and `in_database` (whether `to` is a choice the database has).
normalize_service_choice_suggestions <- function(draft, answer_options) {
  words <- draft$response_options$answer_option
  words <- words[!is.na(words)]
  uses <- table(words)
  none <- data.frame(from = character(), uses = integer(), to = character(), why = character(), in_database = logical())
  rows <- lapply(names(uses), function(word) {
    if (word %in% answer_options) {
      return(NULL)
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

# Use other words for a choice, everywhere the draft has it.
# from: The words the draft has.
# to: The words to use instead.
# Returns: The draft. An error with a sentence if a variable would then offer the same choice twice.
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
