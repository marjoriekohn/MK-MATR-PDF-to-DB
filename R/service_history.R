# ------ Edit History ---------------------------------------------------------------
# What the Edit History page shows: every change made to the MATR metadata database through this site,
# newest first, who made each, and what each did. Only reads; the database writes the history as it
# changes the catalog (model_history.R).

# The kinds of change, as the history records them; "all" is no filter.
history_actions <- c("Pushed", "Emptied", "Added", "Updated", "Deleted")

# How many changes are listed at first, and each time staff ask for more.
history_page_size <- 25

# Every change in the history, newest first.
# most: Only the newest this many; all of them by default.
# Returns: A list of records named by id; see `history_model_get_all()`.
history_service_get_all <- function(connection, most = Inf) {
  history_model_get_all(connection, most)
}

# How many changes there are.
history_service_count <- function(connection) {
  history_model_count(connection)
}

# The changes of one kind that match some words.
# records: The changes, from `history_service_get_all()`.
# action: One of `history_actions`; anything else keeps every kind.
# query: Words to find, all of which must be in the change's sentence or the name of who made it,
#   ignoring capitals.
# Returns: The matching records, in the same order.
history_service_filter <- function(records, action = "all", query = "") {
  words <- strsplit(tolower(trimws(query %||% "")), "[[:space:]]+")[[1]]
  words <- words[nzchar(words)]
  Filter(function(record) {
    if (action %in% history_actions && !identical(record$action, action)) {
      return(FALSE)
    }
    text <- tolower(paste(record$summary, record$staff))
    all(vapply(words, function(word) grepl(word, text, fixed = TRUE), logical(1)))
  }, records)
}

# Who made a change, in words: "by Maggie Kohn", or "by someone no longer listed".
history_service_who <- function(record) {
  paste("by", if (is.na(record$staff %||% NA)) "someone no longer listed" else record$staff)
}

# When a change was made, in words: "Today at 9:15 AM", "Yesterday", "On Sep 29".
history_service_when <- function(record, now = Sys.time()) {
  when <- day_phrase(parse_time(record$occurred), now)
  sub("^(.)", "\\U\\1", when, perl = TRUE)
}

# What one change did.
# id: The change's id.
# Returns: A list: `record` (with its details), and for an add, an update or a delete `groups` (the rows it
#   touched, see `change_service_groups()`), `counts` (see `change_service_counts()`) and `text` (the counts in
#   a sentence); for a push or an emptying `lines`, its counts in words (see `history_service_count_lines()`).
history_service_changes <- function(connection, id) {
  record <- history_model_get(connection, id)
  if (record$action %in% c("Pushed", "Emptied")) {
    return(list(record = record, lines = history_service_count_lines(record$details)))
  }
  groups <- change_service_groups(history_model_items(record$details))
  counts <- change_service_counts(groups)
  list(record = record, groups = groups, counts = counts, text = change_service_counts_text(counts))
}

# A push's or an emptying's counts in words, one line each: "Questions: 287", "New variables: 120",
# "Components reused: 3", "Placements removed: 12". The ids of what was pushed or emptied are left out.
# details: A named list of counts, as the database wrote them.
# Returns: A character vector; empty if there are no counts.
history_service_count_lines <- function(details) {
  if (!is.list(details) || is.null(names(details))) {
    return(character())
  }
  counts <- details[!grepl("_id$", names(details))]
  counts <- counts[order(names(counts) != "questions")] # questions first, the rest as recorded
  # a push counts rows by table ("variable_new"); an emptying counts what went ("variables_removed")
  nouns <- c(component = "components", placement = "placements", wave_question = "questions asked", variable = "variables",
             response_option = "response options", answer_option = "answer options", tag = "tags")
  words <- function(name) gsub("_", " ", if (name %in% names(nouns)) nouns[[name]] else name)
  vapply(names(counts), function(name) {
    label <- if (grepl("_new$", name)) {
      paste("New", words(sub("_new$", "", name)))
    } else if (grepl("_reused$", name)) {
      paste(words(sub("_reused$", "", name)), "reused")
    } else if (grepl("_removed$", name)) {
      paste(words(sub("_removed$", "", name)), "removed")
    } else {
      words(name)
    }
    label <- sub("^(.)", "\\U\\1", label, perl = TRUE)
    paste0(label, ": ", format(as.numeric(counts[[name]]), big.mark = ","))
  }, character(1), USE.NAMES = FALSE)
}
