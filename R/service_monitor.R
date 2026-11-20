# ------ AI Monitor: how often, and where, staff correct the AI ------------------------------------
# What the AI Monitor page shows, worked out from the pushed AI-Powered extractions (model_monitor.R): each
# correction staff made to what the AI returned (the extraction's review_edits, see the top of
# service_draft_record.R), how often each kind of correction comes up, the ones that come up again and again,
# and how many there were over time. Only reads.
#
# A kind of correction is what was changed, on what, and which field: "Changed a variable's unit", "Removed a
# question", "Moved a choice". A block is called by its kind (a question, a section...) where the correction says
# which. Within a kind the same before and after can come up again ("years old" to "years"): a repeated correction,
# which is what the prompt most needs to hear about.
#
# A correction is the least staff changed, counted once (`monitor_counted_edits()`): what is in a question removed
# goes with it, and a row removed and made again in its place counts only for what it says differently.

# What staff call the fields a correction can be to (review_edit_fields, and the questionnaire's own).
monitor_field_words <- c(
  name = "name", abbreviation = "abbreviation",
  component_type = "kind", text = "wording", printed_number = "printed number",
  parent_question_id = "the question it depends on",
  scale = "measurement scale",
  variable_type = "type", label = "label", subject = "subject", time_frame = "time frame",
  answer_data_type = "answer data type", unit = "unit",
  answer_option = "wording",
  position = "position"
)
monitor_change_words <- c(edited = "Changed", added = "Added", deleted = "Removed", moved = "Moved")

# Every pushed AI-Powered extraction, oldest pushed first (see `monitor_model_get_pushed()`), with the
# corrections that count (`monitor_counted_edits()`).
monitor_service_get_all <- function(connection) {
  lapply(monitor_model_get_pushed(connection), function(record) {
    record$edits <- monitor_counted_edits(record$edits %||% list())
    record
  })
}

# ------ What counts as a correction ---------------------------------------------------------------
# The differences between what the AI read and what was pushed list every row on its own, matched by the row
# of the AI's answer it came from (the top of service_draft_record.R). So a question that shouldn't have been
# there lists its variables and choices as removed too, and a question staff removed and made again (to reword
# it, say) lists everything in it twice, removed and added, though most of it is as the AI read it. Counted as
# they are, the same choices were both removed and added. What counts instead is the least staff changed:
#   - a row removed and a row added of the same sort in the same place (its place in words: "Question 35 >
#     variable 1") are the same row made again; they count as the fields that say something else, if any
#   - a variable or choice removed with the question or variable it is in counts as part of that, and so does
#     one added with the question or variable it is in
# A row is in another when its place starts with the other's ("Question 4 > variable 2" is in "Question 4"). Places
# are names, so two blocks with one name ("A group") are taken for one, and the blocks in a section removed with it
# still count, since a block's place doesn't say what it is in.
# edits: One extraction's differences, as service_draft_record.R keeps them.
# Returns: A list of differences in the same form, in the same order; empty if there are none.
monitor_counted_edits <- function(edits) {
  if (length(edits) == 0) {
    return(list())
  }
  text <- function(name) vapply(edits, function(edit) monitor_text(edit[[name]]), character(1))
  change <- text("change")
  item <- text("item")
  where <- text("where")
  where[is.na(where)] <- ""
  whole <- change %in% c("deleted", "added")
  # each removed row with the first added one of its sort in its place, if there is one
  twin <- rep(NA_integer_, length(edits))
  for (at in which(change == "deleted")) {
    found <- which(change == "added" & item == item[at] & where == where[at] & is.na(twin))
    if (length(found) > 0) {
      twin[at] <- found[1]
      twin[found[1]] <- at
    }
  }
  alone <- whole & is.na(twin)
  inside <- whole & vapply(seq_along(edits), function(at) {
    any(alone & change == change[at] & startsWith(where[at], paste0(where, " > ")))
  }, logical(1))
  # a removed row that goes with its removed question isn't made again: the added one it was paired with counts alone
  twin[!is.na(twin) & change == "added" & inside[twin]] <- NA
  counted <- lapply(seq_along(edits), function(at) {
    if (inside[at] || (!is.na(twin[at]) && change[at] == "added")) {
      list()
    } else if (!is.na(twin[at])) {
      monitor_remade_edits(edits[[at]], edits[[twin[at]]])
    } else {
      list(edits[[at]])
    }
  })
  unlist(counted, recursive = FALSE) %||% list()
}

# A row removed and made again in its place, as the differences it makes: one for each field that says something
# else, in the form service_draft_record.R keeps them.
monitor_remade_edits <- function(removed, added) {
  item <- monitor_text(removed$item)
  before <- monitor_summary_fields(item, monitor_text(removed$before))
  after <- monitor_summary_fields(item, monitor_text(added$after))
  value <- function(values, field) if (field %in% names(values)) values[[field]]
  fields <- intersect(monitor_item_fields(item), union(names(before), names(after)))
  fields <- Filter(function(field) !identical(value(before, field), value(after, field)), fields)
  lapply(fields, function(field) {
    list(change = "edited", item = item, ref = removed$ref, where = added$where, field = field, before = value(before, field), after = value(after, field))
  })
}

# The fields compared for an item ("block", "variable" or "choice"), as service_draft_record.R names them.
monitor_item_fields <- function(item) {
  unlist(review_edit_fields[names(review_edit_items)[review_edit_items == item]], use.names = FALSE)
}

# What a row removed or added said, by field: "component_type: Question; text: how old?" is
# c(component_type = "Question", text = "how old?"). A field left blank isn't said.
# item: "block", "variable" or "choice".
# text: The row as the differences say it (`review_edit_summary()`), or NA.
# Returns: A named character vector; empty for NA.
monitor_summary_fields <- function(item, text) {
  if (length(text) == 0 || is.na(text) || !nzchar(text)) {
    return(character())
  }
  fields <- monitor_item_fields(item)
  pieces <- strsplit(text, sprintf("; (?=(%s): )", paste(fields, collapse = "|")), perl = TRUE)[[1]]
  names <- sub(": .*$", "", pieces)
  stats::setNames(substring(pieces, nchar(names) + 3), names)
}

# A value of a difference as text: its first, or NA for none.
monitor_text <- function(value) if (is.null(value) || length(value) == 0) NA_character_ else as.character(value[[1]])

# A row removed or added as the page says it: a choice by its wording, a block by its words, a variable by its
# type, each with the rest of what it said (the kind of block is in the kind's name).
# item: "block", "variable" or "choice".
# text: The row as the differences say it, or NA.
# Returns: A string; NA for NA.
monitor_said <- function(item, text) {
  if (is.na(text)) {
    return(NA_character_)
  }
  fields <- monitor_summary_fields(item, text)
  first <- switch(item, block = "text", variable = "variable_type", choice = "answer_option", NA_character_)
  lead <- if (first %in% names(fields)) gsub("_", " ", fields[[first]]) else "(no words)"
  rest <- fields[!names(fields) %in% c(first, "component_type")]
  words <- unname(monitor_field_words[names(rest)])
  words[is.na(words)] <- names(rest)[is.na(words)]
  rest <- if (length(rest) > 0) paste0(words, ": ", rest)
  paste(c(lead, rest), collapse = " · ")
}

# Every correction in some extractions, a row each.
# records: From `monitor_service_get_all()`.
# Returns: A data frame with `extraction_id`, `extraction` (its summary, or its PDF's name), `questionnaire` (the
#   abbreviation of the questionnaire it was pushed into, or else the extraction's name), `applied` (POSIXct),
#   `change`, `item`, `field` (NA for an addition or a removal), `where`, `before`, `after` (NA for none),
#   `thing` (what was corrected, in words: "question", "variable"), `kind` (`change`, `thing` and `field` joined
#   by "|") and `label` (the kind in words); no rows if there are none.
monitor_service_corrections <- function(records) {
  rows <- lapply(records, function(record) {
    edits <- record$edits %||% list()
    if (length(edits) == 0) {
      return(NULL)
    }
    value <- function(name) vapply(edits, function(edit) monitor_text(edit[[name]]), character(1))
    data.frame(
      extraction_id = record$id,
      extraction = monitor_extraction_name(record),
      questionnaire = if (has_text(record$questionnaire)) record$questionnaire else monitor_extraction_name(record),
      applied = parse_time(record$applied %||% NA_character_),
      change = value("change"), item = value("item"), field = value("field"), where = value("where"),
      before = value("before"), after = value("after"),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(rows)) {
    rows <- data.frame(
      extraction_id = character(), extraction = character(), questionnaire = character(),
      applied = as.POSIXct(character(), tz = "UTC"), change = character(), item = character(), field = character(),
      where = character(), before = character(), after = character(), stringsAsFactors = FALSE
    )
  }
  rows$thing <- monitor_thing(rows)
  rows$kind <- paste(rows$change, rows$thing, ifelse(is.na(rows$field), "", rows$field), sep = "|")
  rows$label <- monitor_kind_label(rows$change, rows$thing, rows$field)
  rownames(rows) <- NULL
  rows
}

# What an extraction is called on the page: what staff said it was, or the PDF it was read from.
monitor_extraction_name <- function(record) {
  if (has_text(record$summary)) record$summary else if (has_text(record$pdf_name)) record$pdf_name else paste("Extraction", record$id)
}

# What each correction is to, in words: "variable", "choice", "questionnaire", or a block by its kind ("question",
# "section", "interviewer script") where the correction says which, else "block". A block removed or added says
# its kind; a block's place is its name ("Question 4", 'Section "Your health"', "A group", "A note with no
# heading", from `block_name()`). A change to the kind of a block is to a block.
# rows: Rows of `monitor_service_corrections()`, with `change`, `item`, `field`, `where`, `before` and `after`.
monitor_thing <- function(rows) {
  kinds <- names(block_kind_descriptions) # the kinds of block (32_edit_extraction_outline_ui.R)
  said <- ifelse(rows$change == "deleted", rows$before, rows$after)
  vapply(seq_len(nrow(rows)), function(at) {
    if (!identical(rows$item[at], "block")) {
      return(rows$item[at])
    }
    if (identical(rows$field[at], "component_type")) {
      return("block")
    }
    kind <- if (rows$change[at] %in% c("deleted", "added")) monitor_summary_fields("block", said[at])["component_type"]
    if (is.null(kind) || is.na(kind)) {
      where <- if (is.na(rows$where[at])) "" else rows$where[at]
      named <- startsWith(where, paste0(kinds, " ")) | where == kinds | startsWith(tolower(where), paste("a", tolower(kinds)))
      kind <- kinds[named][1]
    }
    if (is.na(kind)) "block" else tolower(kind)
  }, character(1))
}

# A kind of correction in words: "Changed a variable's unit", "Changed the questionnaire's name", "Removed an
# instruction", "Moved a choice", "Changed the question a question depends on".
# change, thing, field: Vectors of the same length, as in `monitor_service_corrections()`.
monitor_kind_label <- function(change, thing, field) {
  if (length(change) == 0) {
    return(character())
  }
  verb <- ifelse(change %in% names(monitor_change_words), monitor_change_words[change], change)
  a_thing <- ifelse(thing == "questionnaire", "the questionnaire", paste(ifelse(grepl("^[aeiou]", thing), "an", "a"), thing))
  field_words <- ifelse(!is.na(field) & field %in% names(monitor_field_words), monitor_field_words[field], field)
  edited <- change == "edited" & !is.na(field)
  unname(ifelse(
    edited & field %in% "component_type", "Changed the kind of block",
    ifelse(
      edited & field %in% "parent_question_id", paste("Changed the question", a_thing, "depends on"),
      ifelse(edited, paste0(verb, " ", a_thing, "'s ", field_words), paste(verb, a_thing))
    )
  ))
}

# The kinds of correction, the commonest first.
# corrections: From `monitor_service_corrections()`.
# extractions: How many extractions they come from (those with none included), for each kind's share.
# Returns: A list, one entry per kind: `kind`, `label`, `change`, `item`, `field`, `count` (how many
#   corrections), `extractions` (how many extractions had one), `share` (of all extractions, 0 to 1) and
#   `examples` (see `monitor_service_examples()`); empty if there are no corrections.
monitor_service_common <- function(corrections, extractions) {
  if (nrow(corrections) == 0) {
    return(list())
  }
  kinds <- split(corrections, corrections$kind)
  common <- lapply(kinds, function(rows) {
    list(
      kind = rows$kind[1],
      label = rows$label[1],
      change = rows$change[1],
      item = rows$item[1],
      field = rows$field[1],
      count = nrow(rows),
      extractions = length(unique(rows$extraction_id)),
      share = if (extractions > 0) length(unique(rows$extraction_id)) / extractions else 0,
      examples = monitor_service_examples(rows)
    )
  })
  counts <- vapply(common, function(kind) kind$count, numeric(1))
  labels <- vapply(common, function(kind) kind$label, character(1))
  unname(common[order(-counts, labels)])
}

# The corrections of one kind, the same before and after counted together, the commonest first.
# rows: Some rows of `monitor_service_corrections()`, all of one kind.
# Returns: A data frame with `before`, `after`, `count`, `questionnaires` (the questionnaires it was made in,
#   e.g. "KQ, MQ") and `where` (the first place it was made).
monitor_service_examples <- function(rows) {
  key <- paste(ifelse(is.na(rows$before), "\r", rows$before), ifelse(is.na(rows$after), "\r", rows$after), sep = "\n")
  groups <- split(seq_len(nrow(rows)), factor(key, levels = unique(key)))
  examples <- do.call(rbind, lapply(groups, function(at) {
    first <- at[1]
    data.frame(
      before = rows$before[first], after = rows$after[first], count = length(at),
      questionnaires = paste(sort(unique(rows$questionnaire[at])), collapse = ", "), where = rows$where[first],
      stringsAsFactors = FALSE
    )
  }))
  examples <- examples[order(-examples$count), , drop = FALSE]
  rownames(examples) <- NULL
  examples
}

# The page's headline numbers.
# records: From `monitor_service_get_all()`.
# Returns: A list: `extractions` (pushed AI-Powered extractions), `corrections`, `per_extraction` (the mean;
#   NA with none) and `untouched` (extractions pushed as the AI read them).
monitor_service_totals <- function(records) {
  counts <- vapply(records, function(record) length(record$edits %||% list()), numeric(1))
  list(
    extractions = length(records),
    corrections = sum(counts),
    per_extraction = if (length(counts) > 0) mean(counts) else NA_real_,
    untouched = sum(counts == 0)
  )
}

# The extractions read by each prompt, the newest prompt first, so a change to the prompt can be seen to help.
# records: From `monitor_service_get_all()`.
# Returns: A data frame with `prompt_version`, `model`, `extractions`, `corrections`, `per_extraction`,
#   `first_applied` and `last_applied` (POSIXct); no rows if there are no records.
monitor_service_by_prompt <- function(records) {
  if (length(records) == 0) {
    return(data.frame(
      prompt_version = character(), model = character(), extractions = integer(), corrections = numeric(),
      per_extraction = numeric(), first_applied = as.POSIXct(character(), tz = "UTC"),
      last_applied = as.POSIXct(character(), tz = "UTC"), stringsAsFactors = FALSE
    ))
  }
  or <- function(value, otherwise) if (length(value) == 0 || is.na(value)) otherwise else value
  version <- vapply(records, function(record) or(record$prompt_version, "(not recorded)"), character(1))
  model <- vapply(records, function(record) or(record$model, ""), character(1))
  counts <- vapply(records, function(record) length(record$edits %||% list()), numeric(1))
  applied <- parse_time(vapply(records, function(record) record$applied %||% NA_character_, character(1)))
  groups <- split(seq_along(records), factor(version, levels = unique(version)))
  rows <- do.call(rbind, lapply(names(groups), function(name) {
    at <- groups[[name]]
    data.frame(
      prompt_version = name,
      model = paste(unique(model[at][nzchar(model[at])]), collapse = ", "),
      extractions = length(at),
      corrections = sum(counts[at]),
      per_extraction = mean(counts[at]),
      first_applied = min(applied[at], na.rm = TRUE),
      last_applied = max(applied[at], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  rows <- rows[order(rows$last_applied, decreasing = TRUE), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# ------ Over time -------------------------------------------------------------------------------
# How long a bar of the chart covers: a day while the extractions span a month or less, a week while they
# span half a year or less, otherwise a month.
monitor_period_unit <- function(dates) {
  if (length(dates) == 0) {
    return("day")
  }
  span <- as.numeric(max(dates) - min(dates))
  if (span <= 31) "day" else if (span <= 7 * 26) "week" else "month"
}

# The first day of the day, week (from Monday) or month each date is in.
monitor_period_start <- function(dates, unit) {
  switch(unit,
    day = dates,
    week = dates - (as.integer(format(dates, "%u")) - 1L),
    month = as.Date(format(dates, "%Y-%m-01"))
  )
}

# A period on the chart's axis: "Nov 7", "Nov 3" (a week, by its Monday) or "Nov 2026".
monitor_period_label <- function(starts, unit) {
  if (unit == "month") {
    return(format(starts, "%b %Y"))
  }
  paste(format(starts, "%b"), as.integer(format(starts, "%d")))
}

# The corrections in each period, for the chart: every period from the first pushed to the last, those with
# none as nought.
# records: From `monitor_service_get_all()`.
# unit: `"day"`, `"week"` or `"month"`; by default, by how long the extractions span (`monitor_period_unit()`).
# tz: The time zone whose days count: the site's.
# Returns: A data frame with `start` (Date), `label`, `extractions` and `corrections`, oldest first, and the
#   unit as its attribute `unit`; no rows if there are no records.
monitor_service_over_time <- function(records, unit = NULL, tz = site_time_zone) {
  applied <- parse_time(vapply(records, function(record) record$applied %||% NA_character_, character(1)))
  counts <- vapply(records, function(record) length(record$edits %||% list()), numeric(1))
  keep <- !is.na(applied)
  dates <- as.Date(applied[keep], tz = tz)
  counts <- counts[keep]
  unit <- unit %||% monitor_period_unit(dates)
  if (length(dates) == 0) {
    return(structure(
      data.frame(start = as.Date(character()), label = character(), extractions = integer(), corrections = numeric()),
      unit = unit
    ))
  }
  starts <- monitor_period_start(dates, unit)
  every <- seq(min(starts), max(starts), by = unit)
  at <- match(starts, every)
  series <- data.frame(
    start = every,
    label = monitor_period_label(every, unit),
    extractions = tabulate(at, nbins = length(every)),
    corrections = vapply(seq_along(every), function(i) sum(counts[at == i]), numeric(1)),
    stringsAsFactors = FALSE
  )
  structure(series, unit = unit)
}

# What a bar of the chart says when the pointer is over it: "Week of Nov 3: 49 corrections in 2 extractions".
monitor_period_text <- function(series) {
  unit <- attr(series, "unit") %||% "day"
  when <- switch(unit, week = paste("Week of", series$label), series$label)
  paste0(
    when, ": ",
    vapply(series$corrections, function(n) plural(n, "correction"), character(1)), " in ",
    vapply(series$extractions, function(n) plural(n, "extraction"), character(1))
  )
}
