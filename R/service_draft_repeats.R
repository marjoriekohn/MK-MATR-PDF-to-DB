# ------ The same question about each of several people ----------------------------------------------
# Some forms ask the same questions about one person and then again about another: MQ has a section about
# twin A, then the same section about twin B. They are one question each, with a variable for each person.
# The reading words them alike and tags each question with its one person (the prompt's "The same questions
# about each of several people"); `draft_service_join_repeats()` then joins them, when the AI's answer is
# opened (`draft_service_from_json()`). The copy staff save is never joined: it is as they left it.
#
# Two questions are the same when their words are (and their grid's lead-in's, for a grid's items), give or take
# capitals, punctuation and whether a word is written for one person or several (`same_words()`), and each is
# about one person, a different one. The two copies are read in separate requests, and Gemini doesn't always
# inflect or punctuate them alike ("acts like" and "act like", "the twins' life" and "lives", 'butt in' and
# "butt in"); the joined question keeps the first copy's words. Anything else is left as it was read, so nothing
# is joined on a guess: when the AI words two of them differently, they stay two questions and staff join them
# by hand, as before.

# What a question is told by: its words as they are compared (`same_words()`), after its grid's lead-in if it is
# in a grid. A follow-up ("If yes, why?") is told by the question it depends on instead of where it sits, since
# one reading may put it in the grid and another after it.
# Returns: A character vector, one for each row of `blocks`.
repeat_keys <- function(blocks) {
  holder <- match(blocks$parent_id, blocks$id)
  lead_in <- ifelse(blocks$component_type[holder] %in% "Grid", same_words(blocks$text[holder]), "")
  before <- ifelse(is.na(blocks$parent_question_id), lead_in, paste("after", blocks$parent_question_id))
  paste(before, same_words(blocks$text), sep = "\r")
}

# A text as two copies of a question are compared: in lower case, without punctuation, and each word in one form
# whether it was written about one person or several ("acts" and "act", "is" and "are", "tries" and "try",
# "themself" and "themselves", "life" and "lives"). Only for comparing: the forms aren't words to show.
# Returns: A character vector as long as `text`, `NA` where it is.
same_words <- function(text) {
  one_form <- c(
    is = "be", are = "be", was = "be", were = "be", been = "be", has = "have", does = "do",
    lives = "life", themselves = "themself"
  )
  words <- strsplit(gsub("[^a-z0-9 ]+", " ", gsub("'", "", tolower(straight_quotes(text)), fixed = TRUE)), " +")
  compared <- vapply(words, function(word) {
    word <- word[nzchar(word)]
    known <- word %in% names(one_form)
    word[known] <- one_form[word[known]]
    word <- sub("ies$", "y", word)
    paste(sub("(?<=[^s])s$", "", word, perl = TRUE), collapse = " ")
  }, character(1))
  ifelse(is.na(text), NA_character_, compared)
}

# Which question each question repeats.
# A question repeats the first question before it with the same words (`repeat_keys()`) that is about someone
# else, unless another question joined to that one is already about its person.
# Returns: A character vector as long as `draft$components`: the id of the question each row joins, `NA` for a
#   row that stays.
repeats_of <- function(draft) {
  blocks <- draft$components
  joins <- rep(NA_character_, nrow(blocks))
  about_one <- blocks$component_type == "Question" & !is.na(blocks$subject) & !is.na(blocks$text)
  rows <- match(draft_service_printed_order(draft), blocks$id)
  rows <- rows[about_one[rows]]
  for (same in split(rows, repeat_keys(blocks)[rows])) {
    first <- same[1]
    people <- blocks$subject[first]
    for (row in same[-1]) {
      if (blocks$subject[row] %in% people) next
      joins[row] <- blocks$id[first]
      people <- c(people, blocks$subject[row])
    }
  }
  joins
}

# Join the questions a draft asks about one person and then again about another.
# The later question's variables move to the earlier one, after its own, each saying who it is about (and when,
# if the two are about different times); the earlier question has every printed number ("8, 24"); a question that
# depended on the later one depends on the earlier; and a section, grid or group left without a question goes,
# with the instructions it held. Done again until nothing more joins: once two questions are one, the follow-ups
# of each are the same question's follow-ups.
# Returns: The draft; joining it again changes nothing.
draft_service_join_repeats <- function(draft) {
  repeat {
    joins <- repeats_of(draft)
    if (all(is.na(joins))) {
      return(draft)
    }
    draft <- join_repeated(draft, joins)
  }
}

# One round of `draft_service_join_repeats()`.
# joins: For each block, the id of the question it joins, or `NA` (`repeats_of()`).
join_repeated <- function(draft, joins) {
  blocks <- draft$components
  boxes <- draft$variables
  repeated <- !is.na(joins)
  kept <- match(joins[repeated], blocks$id)
  joined <- blocks$id %in% c(blocks$id[repeated], joins[repeated])

  # what each of these questions said of all its variables, each variable now says itself...
  of_question <- match(boxes$component_id, blocks$id)
  for (kind in c("subject", "time_frame")) {
    theirs <- joined[of_question] & is.na(boxes[[kind]])
    boxes[[kind]][theirs] <- blocks[[kind]][of_question[theirs]]
    blocks[[kind]][joined] <- NA
  }
  # ...and the variables of the later question come after the earlier one's
  moving <- repeated[of_question]
  boxes$display_order[moving] <- boxes$display_order[moving] + 1000 * of_question[moving]
  boxes$component_id[moving] <- joins[of_question[moving]]

  for (keeper in unique(kept)) {
    numbers <- c(blocks$printed_number[keeper], blocks$printed_number[repeated][kept == keeper])
    numbers <- unique(numbers[!is.na(numbers)])
    if (length(numbers) > 0) blocks$printed_number[keeper] <- paste(numbers, collapse = ", ")
    # the one question is in the scale either of them was printed with
    scales <- c(blocks$scale[keeper], blocks$scale[repeated][kept == keeper])
    if (any(!is.na(scales))) blocks$scale[keeper] <- scales[!is.na(scales)][1]
  }
  follows <- match(blocks$parent_question_id, blocks$id[repeated])
  blocks$parent_question_id[!is.na(follows)] <- joins[repeated][follows[!is.na(follows)]]

  # the sections, grids and groups the later questions were in, when no question is left in them
  around <- unique(unlist(lapply(blocks$id[repeated], draft_service_ancestors, draft = draft)))
  draft$components <- blocks[!repeated, , drop = FALSE]
  draft$variables <- boxes
  emptied <- Filter(function(id) {
    inside <- draft_service_printed_order(draft, id)
    !any(draft$components$component_type[match(inside, draft$components$id)] == "Question")
  }, around)
  gone <- unique(c(emptied, unlist(lapply(emptied, draft_service_printed_order, draft = draft))))
  draft$components <- draft$components[!draft$components$id %in% gone, , drop = FALSE]

  for (table in names(parent_columns)) draft <- renumber(draft, table)
  draft_service_place_tags(draft_service_renumber_questions(drop_orphans(draft)))
}
