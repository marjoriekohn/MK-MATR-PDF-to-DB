# ------ Edit History ---------------------------------------------------------------
# What the Edit History page shows: the edits that have been applied, newest first, who
# was behind each, and what each changed. Only reads; edits enter the history when the
# database applies them (service_change_request.R).

# The kinds of edit, as staff pick them on Make an edit; "all" is no filter.
history_kinds <- c("Add", "Update", "Delete")

# How many edits are listed at first, and each time staff ask for more.
history_page_size <- 25

# Every edit in the history, newest first.
# most: Only the newest this many; all of them by default.
# Returns: A list of records named by id; see `history_model_get_all()`.
history_service_get_all <- function(connection, most = Inf) {
  history_model_get_all(connection, most)
}

# The edits of one kind that match some words.
# records: The edits, from `history_service_get_all()`.
# kind: `"Add"`, `"Update"` or `"Delete"`; anything else keeps every kind.
# query: Words to find, all of which must be in the edit's sentence or the names of the
#   people behind it, ignoring capitals.
# Returns: The matching records, in the same order.
history_service_filter <- function(records, kind = "all", query = "") {
  words <- strsplit(tolower(trimws(query %||% "")), "[[:space:]]+")[[1]]
  words <- words[nzchar(words)]
  Filter(function(record) {
    if (kind %in% history_kinds && !identical(record$kind, kind)) {
      return(FALSE)
    }
    text <- tolower(paste(record$summary, record$prepared_by, record$approved_by))
    all(vapply(words, function(word) grepl(word, text, fixed = TRUE), logical(1)))
  }, records)
}

# Who was behind an edit, in words.
# For example "prepared by Maggie Kohn and approved by Alex Rivera", or "made by Maggie Kohn, applied at
# once (it needed no approval)" for an add or an update, or anything an ADMIN did.
history_service_people <- function(record) {
  who <- if (is.na(record$prepared_by)) "someone no longer listed" else record$prepared_by
  if (!isFALSE(record$approval_required) && !is.na(record$approved_by %||% NA)) {
    return(paste0("prepared by ", who, " and approved by ", record$approved_by))
  }
  if (isFALSE(record$approval_required)) {
    return(paste0("made by ", who, ", applied at once (no approval needed)"))
  }
  paste0("prepared by ", who)
}

# When an edit was applied, in words.
# Returns: A string, e.g. "Applied yesterday"; or its status if it hasn't been.
history_service_when <- function(record, now = Sys.time()) {
  if (is.null(record$applied)) {
    return(record$status)
  }
  paste("Applied", day_phrase(parse_time(record$applied), now))
}

# What one edit changed.
# id: The edit's id.
# Returns: A list: `groups` (the rows it touched, see `change_service_groups()`) and `counts`
#   (see `change_service_counts()`).
history_service_changes <- function(connection, id) {
  groups <- change_service_groups(history_model_get_items(connection, id))
  list(groups = groups, counts = change_service_counts(groups))
}

# What an edit changed, in a sentence, over the rows it lists.
# A delete keeps only the row picked (private.apply_draft_items()), so it isn't counted: it says what was
# deleted, "Deleted Kid Questionnaire Wave 1". One deleted before the name was kept says it as its sentence
# does ("Deleted the wave “KQ Wave 1”"), without the count of the rows that went with it.
# counts: Its counts, from `history_service_changes()`.
history_service_changes_text <- function(record, counts) {
  if (identical(record$kind, "Delete")) {
    if (!is.na(record$deleted %||% NA)) {
      return(paste("Deleted", record$deleted))
    }
    return(sub(" and [0-9,]+ connected records?$", "", record$summary %||% ""))
  }
  change_service_counts_text(counts)
}
