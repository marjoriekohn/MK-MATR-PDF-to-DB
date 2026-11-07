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
#                   [subject] too ("describes [subject] very well"), so both share one set of choices. So can
#                   questions that differ only in when they ask about ("... in your lifetime?", "... during the
#                   last 12 months?"): their stem has [time frame] (time_frame_placeholder), and each variable is
#                   about its question's time frame, written one way (normal_time_frame()). So can questions that
#                   differ by pronoun ("how old is she?", "how old is he?": [subject]), or in one thing ("... to use
#                   tobacco ...", "... to use alcohol ...": [item]); others, such as one reworded for another
#                   person, are linked by hand with a stem staff type (normalize_service_link_by_hand()).
#
# Who a question can ask about is a subject the database has, or one a variable of the draft names, found in its
# words as whole words, whatever their capitals ("Twin A"). The respondent isn't: they are a blank.

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
# punctuation (a qualifier's brackets too, but not [subject]'s), and without the "s" at the end of a longer word.
normalize_loose_words <- function(text) {
  text <- gsub("&", " and ", normal_answer(text), fixed = TRUE)
  text <- gsub("\\[(?!subject\\])|(?<!\\[subject)\\]", " ", text, perl = TRUE)
  text <- gsub("[^a-z0-9 \\[\\]]", " ", text, perl = TRUE)
  text <- squish(text)
  gsub("\\b([a-z]{3,})s\\b", "\\1", text, perl = TRUE)
}

# The choices of a draft that could be worded as others are.
# A choice the database doesn't have whose words were merged into one of its choices, or that look like one of
# them or like another choice of the draft that more variables offer.
# answer_options: The answer choices' words in the database.
# aliases: The wordings merged into them: a data frame of `alias` and `label` (the choice's words).
# Returns: A data frame: `from` (the draft's words), `uses` (how many of its choices have them), `to` (the words to
#   use instead), `why` ("merged" or "alike") and `in_database` (whether `to` is a choice the database has).
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

# Pronouns for one person, by which they are: a question that uses one set only ("how old is she?") asks about
# someone the same way another asks with the other ("how old is he?").
normalize_pronouns <- list(
  she = c("she", "her", "hers", "herself"),
  he = c("he", "him", "his", "himself")
)

# Which set of pronouns some words use: "she" or "he", or NA for neither or both ("he or she", "s/he").
normalize_pronoun_set <- function(texts) {
  vapply(texts, function(text) {
    words <- tolower(regmatches(text %||% "", gregexpr("[[:alpha:]]+", text %||% ""))[[1]])
    used <- names(normalize_pronouns)[vapply(normalize_pronouns, function(set) any(words %in% set), logical(1))]
    if (length(used) == 1 && !grepl("(?i)\\bs/he\\b", text, perl = TRUE)) used else NA_character_
  }, character(1), USE.NAMES = FALSE)
}

# Words with the pronouns of one set as [subject].
normalize_with_pronoun_placeholder <- function(text, set) {
  gsub(paste0("(?i)\\b(", paste(normalize_pronouns[[set]], collapse = "|"), ")\\b"), subject_placeholder, text, perl = TRUE)
}

# Who a question's variables are about, when they agree on one (NA for the respondent or none).
normalize_variables_subject <- function(draft, id) {
  who <- unique(stats::na.omit(blank_respondent(draft$variables$subject[draft$variables$component_id %in% id])))
  if (length(who) == 1) who else NA_character_
}

# Words with every mention of a subject as [subject].
normalize_with_placeholder <- function(text, subject, patterns) {
  gsub(patterns[[subject]], subject_placeholder, text, perl = TRUE)
}

# ------ Questions about different times --------------------------------------------------------------------------
# The time frames some words ask about, as the database writes them: one character vector for each text.
normalize_time_frames_in <- function(texts) {
  lapply(texts, function(text) {
    if (is.na(text)) {
      return(character())
    }
    found <- regmatches(text, gregexpr(normalize_time_frame_words, text, perl = TRUE))[[1]]
    unique(normal_time_frame(found))
  })
}

# A time frame as whole words, in any capitals.
normalize_time_frame_words <- paste0("(?i)(?<![[:alnum:]])", time_frame_pattern, "(?![[:alnum:]])")

# Words with the time frame they ask about as [time frame].
normalize_with_time_placeholder <- function(text) {
  gsub(normalize_time_frame_words, time_frame_placeholder, text, perl = TRUE)
}

# The questions of a draft that differ only in who or when they ask about.
# Questions whose words are the same once each names who it asks about as [subject] and when as [time frame], each
# asking about someone else, another time, or both ("in your lifetime", "during the last 12 months"); or worded the
# same, where each is in a part about someone else (its section's heading, or failing that the nearest block before
# it, names one subject, and they all differ).
# subjects: Who a variable can be about besides the respondent (the database's subjects); those the draft's
#   variables name are added.
# Returns: A list of groups, in printed order, each a list: `stem` (the words they share), `ids` (the questions' ids,
#   in printed order), `subjects` (who each asks about; `NA` when they all ask about the respondent),
#   `time_frames` (when each asks about; `NA` when the stem has no [time frame]) and `linked` (whether they share
#   that stem already).
normalize_service_question_groups <- function(draft, subjects) {
  patterns <- normalize_subject_patterns(c(subjects, blank_respondent(draft$variables$subject)))
  printed <- draft_service_printed_order(draft)
  blocks <- draft$components[match(printed, draft$components$id), , drop = FALSE]
  named <- normalize_named(blocks$text, patterns)
  times <- normalize_time_frames_in(blocks$text)
  pronouns <- normalize_pronoun_set(blocks$text)
  # a question naming two people compares them, and one naming two times asks about both: they are no one's
  is_question <- blocks$component_type %in% "Question" & !is.na(blocks$text) & lengths(named) <= 1
  stems <- rep(NA_character_, nrow(blocks))
  stems[is_question] <- vapply(which(is_question), function(i) {
    text <- blocks$text[i]
    if (length(named[[i]]) == 1) text <- normalize_with_placeholder(text, named[[i]], patterns)
    if (!is.na(pronouns[i])) text <- normalize_with_pronoun_placeholder(text, pronouns[i])
    if (length(times[[i]]) == 1) text <- normalize_with_time_placeholder(text)
    text
  }, character(1))
  about <- vapply(named, function(n) if (length(n) == 1) n else NA_character_, character(1))
  when <- vapply(times, function(t) if (length(t) == 1) t else NA_character_, character(1))
  keys <- tolower(push_words(stems))
  groups <- list()
  for (key in unique(keys[!is.na(stems) & duplicated(keys) & nzchar(keys)])) {
    members <- which(keys == key & !is.na(stems))
    stem <- stems[members[1]]
    timed <- grepl(time_frame_placeholder, stem, fixed = TRUE)
    who <- about[members]
    if (grepl(subject_placeholder, stem, fixed = TRUE)) {
      # a pronoun names no one: who it is comes from the question's variables, or the part it is in
      for (k in which(is.na(who))) {
        who[k] <- normalize_variables_subject(draft, blocks$id[members[k]])
        if (is.na(who[k])) who[k] <- normalize_context_subject(draft, blocks, named, members[k])
      }
    } else if (!timed) {
      who <- vapply(members, function(i) normalize_context_subject(draft, blocks, named, i), character(1))
      if (anyNA(who)) next
    }
    times_of <- if (timed) when[members] else rep(NA_character_, length(members))
    # each asks about someone else (or, by pronoun alone, another "she" or "he"), another time, or both
    if (anyDuplicated(paste(ifelse(is.na(who), pronouns[members], tolower(who)), times_of)) > 0) next
    # who a pronoun is isn't known for all of them: they are linked by their stem alone
    if (anyNA(who)) who <- rep(NA_character_, length(who))
    ids <- blocks$id[members]
    groups[[length(groups) + 1]] <- list(
      stem = stem, ids = ids, subjects = who, time_frames = times_of,
      linked = all(push_words(draft$components$stem[match(ids, draft$components$id)]) == push_words(stem))
    )
  }
  groups <- c(groups, normalize_item_groups(draft, blocks, stems, unlist(lapply(groups, function(group) group$ids))))
  c(groups, normalize_groups_by_hand(draft, blocks, unlist(lapply(groups, function(group) group$ids))))
}

# ------ Questions about different things ----------------------------------------------------------------------
# Questions worded the same but for one run of up to four words ("tobacco", "alcohol", "marijuana or hashish"),
# when what they share is at least five words and two thirds of each: shorter ones ("do you often feel lonely?",
# "do you often feel tired?") only look alike. A grid's items differ by design, and aren't looked at.
normalize_item_most_words <- 4
normalize_item_least_shared <- 5

# The groups of questions, not in `found`, that differ only in one thing: each with its stem ([item] for the thing),
# its questions' ids in printed order, and the words each has for it (`items`). Their words are compared with who
# and when they ask about as [subject] and [time frame] (`stems`), which have slots of their own.
normalize_item_groups <- function(draft, blocks, stems, found) {
  in_grid <- blocks$component_type[match(blocks$parent_id, blocks$id)] %in% "Grid"
  candidates <- which(blocks$component_type %in% "Question" & !is.na(stems) & !blocks$id %in% found & !in_grid)
  if (length(candidates) < 2) {
    return(list())
  }
  words <- lapply(stems[candidates], normalize_tokens)
  # only questions that start or end alike can be alike: compared in buckets, not every pair
  firsts <- vapply(words, function(w) w[1], character(1))
  lasts <- vapply(words, function(w) w[length(w)], character(1))
  pairs <- unique(do.call(rbind, c(
    lapply(split(seq_along(words), firsts), normalize_pairs_of),
    lapply(split(seq_along(words), lasts), normalize_pairs_of)
  )))
  if (is.null(pairs) || nrow(pairs) == 0) {
    return(list())
  }
  # each pair that is alike: the stem it shares, and each one's words for the thing
  alike <- list()
  for (k in seq_len(nrow(pairs))) {
    found_stem <- normalize_item_stem(words[[pairs[k, 1]]], words[[pairs[k, 2]]])
    if (is.null(found_stem)) next
    for (side in 1:2) {
      alike[[length(alike) + 1]] <- data.frame(stem = found_stem$stem, at = pairs[k, side], item = found_stem$items[side])
    }
  }
  if (length(alike) == 0) {
    return(list())
  }
  alike <- unique(do.call(rbind, alike))
  # the biggest groups first; a question goes in one group only
  sizes <- table(alike$stem)
  taken <- integer()
  groups <- list()
  for (stem in names(sort(sizes, decreasing = TRUE))) {
    members <- alike[alike$stem == stem & !alike$at %in% taken, , drop = FALSE]
    members <- members[order(candidates[members$at]), , drop = FALSE]
    if (nrow(members) < 2 || anyDuplicated(members$item) > 0) next
    taken <- c(taken, members$at)
    ids <- blocks$id[candidates[members$at]]
    # the thing is who they ask about when their variables say so, each someone else: that is [subject]
    who <- vapply(ids, function(id) normalize_variables_subject(draft, id), character(1), USE.NAMES = FALSE)
    people <- !anyNA(who) && anyDuplicated(tolower(who)) == 0
    if (people) stem <- sub(item_placeholder, subject_placeholder, stem, fixed = TRUE)
    groups[[length(groups) + 1]] <- list(
      stem = stem, ids = ids, subjects = if (people) who else rep(NA_character_, length(ids)),
      time_frames = rep(NA_character_, length(ids)), items = members$item,
      linked = all(push_words(draft$components$stem[match(ids, draft$components$id)]) == push_words(stem))
    )
  }
  groups[order(vapply(groups, function(group) match(group$ids[1], blocks$id), numeric(1)))]
}

# Words as compared: each word, each mark of punctuation, and each [placeholder] whole.
normalize_tokens <- function(text) {
  regmatches(tolower(text), gregexpr("\\[[a-z ]+\\]|[[:alnum:]'-]+|[^[:space:][:alnum:]]", tolower(text)))[[1]]
}

# Tokens as words again: a space between each, none before punctuation.
normalize_untokens <- function(tokens) gsub(" ([?.,:;!)])", "\\1", gsub("\\( ", "(", paste(tokens, collapse = " ")))

# Every pair of these, as a two-column matrix (none for fewer than two).
normalize_pairs_of <- function(at) {
  if (length(at) < 2) {
    return(NULL)
  }
  t(utils::combn(at, 2))
}

# The stem two questions' words share, with [item] for the one run of words where they differ, and each one's words
# there; NULL when they aren't alike that way.
normalize_item_stem <- function(a, b) {
  shortest <- min(length(a), length(b))
  before <- 0
  while (before < shortest && a[before + 1] == b[before + 1]) before <- before + 1
  after <- 0
  while (after < shortest - before && a[length(a) - after] == b[length(b) - after]) after <- after + 1
  shared <- before + after
  differ <- c(length(a), length(b)) - shared
  # what they share is counted in words, not punctuation
  is_word <- function(tokens) sum(grepl("[[:alnum:]]", tokens))
  shared_words <- is_word(a[seq_len(before)]) + is_word(if (after > 0) a[(length(a) - after + 1):length(a)])
  if (any(differ < 1) || any(differ > normalize_item_most_words)) {
    return(NULL)
  }
  if (shared_words < normalize_item_least_shared || shared_words < 2 / 3 * max(is_word(a), is_word(b))) {
    return(NULL)
  }
  middle <- function(w) normalize_untokens(w[(before + 1):(length(w) - after)])
  # a thing is named in words: "a" and "b", or "5" and "6", are labels, not things
  if (!all(grepl("[[:alpha:]]{3,}", c(middle(a), middle(b))))) {
    return(NULL)
  }
  list(
    stem = normalize_untokens(c(a[seq_len(before)], item_placeholder, if (after > 0) a[(length(a) - after + 1):length(a)])),
    items = c(middle(a), middle(b))
  )
}

# The questions staff linked by hand (normalize_service_link_by_hand()) that aren't found above: those sharing a
# stem, in printed order, listed as linked so they can be unlinked.
normalize_groups_by_hand <- function(draft, blocks, found) {
  stems <- push_words(blocks$stem)
  shared <- unique(stems[nzchar(stems) & duplicated(stems) & !blocks$id %in% found])
  lapply(shared, function(stem) {
    ids <- blocks$id[stems == stem & !blocks$id %in% found]
    list(
      stem = blocks$stem[match(ids[1], blocks$id)], ids = ids, subjects = rep(NA_character_, length(ids)),
      time_frames = rep(NA_character_, length(ids)), linked = TRUE
    )
  })
}

# Link questions by hand: those reworded for another person ("What is your date of birth?", "What is the date of
# birth of the other biological parent?"), or that differ in any other way Normalize doesn't find.
# Each keeps its own words, number and variables, and gets the stem typed, in lower case (normal_text()).
# numbers: The questions' numbers, as the outline shows them ("B.1", "12").
# stem: The words they share, with [subject] for who and [time frame] for when they ask about.
# Returns: The draft. An error with a sentence if a number is no question's, there are fewer than two, or the stem
#   is blank.
normalize_service_link_by_hand <- function(draft, numbers, stem) {
  numbers <- unique(trimws(numbers[nzchar(trimws(numbers))]))
  if (length(numbers) < 2) stop("Name two or more questions to link, by their numbers.", call. = FALSE)
  stem <- normal_text(trimws(stem %||% ""))
  if (!nzchar(stem)) stop("Type the stem they share, with [subject] for who they ask about.", call. = FALSE)
  questions <- draft$components[draft$components$component_type %in% "Question", , drop = FALSE]
  shown <- draft_service_question_number(questions)
  missing <- setdiff(numbers, shown)
  if (length(missing) > 0) stop("No question is numbered ", paste(missing, collapse = ", "), ".", call. = FALSE)
  draft$components$stem[draft$components$id %in% questions$id[shown %in% numbers]] <- stem
  draft
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

# Link questions that differ only in who or when they ask about.
# Each gets the stem, and each of its variables is about when it asks about. When they ask about different people,
# each variable is about who its question asks about, and a choice that names its variable's subject has [subject]
# there instead, unless the variable's choices name someone else too ("twin a much more" beside "twin b much more"
# compares them).
# group: One of `normalize_service_question_groups()`.
# subjects: As for `normalize_service_question_groups()`.
# Returns: The draft.
normalize_service_link <- function(draft, group, subjects) {
  patterns <- normalize_subject_patterns(c(subjects, group$subjects, blank_respondent(draft$variables$subject)))
  about_people <- !all(is.na(group$subjects))
  for (k in seq_along(group$ids)) {
    id <- group$ids[k]
    subject <- group$subjects[k]
    draft$components$stem[draft$components$id %in% id] <- group$stem
    boxes <- draft$variables$id[draft$variables$component_id %in% id]
    time_frame <- (group$time_frames %||% NA_character_)[k]
    if (!is.na(time_frame)) draft$variables$time_frame[draft$variables$id %in% boxes] <- time_frame
    if (!about_people) next
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

# Unlink questions: they no longer share a stem. Their subjects and choices stay as they are.
# ids: The questions' ids.
# Returns: The draft.
normalize_service_unlink <- function(draft, ids) {
  draft$components$stem[draft$components$id %in% ids] <- NA_character_
  draft
}
