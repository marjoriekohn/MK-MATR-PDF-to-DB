# ------ AI Monitor: how often, and where, staff correct the AI ------------------------------------
# What the AI Monitor page shows, worked out from the pushed AI-Powered extractions (model_monitor.R): each
# correction staff made to what the AI returned (the extraction's review_edits, see the top of
# service_draft_record.R), how often each kind of correction comes up, the ones that come up again and again,
# and how many there were over time. Only reads.
#
# A kind of correction is what was changed, on what, and which field: "Changed a variable's unit", "Removed a
# block", "Moved a choice". Within a kind the same before and after can come up again ("years old" to "years"):
# a repeated correction, which is what the prompt most needs to hear about.

# What staff call the fields a correction can be to (review_edit_fields, and the questionnaire's own).
monitor_field_words <- c(
  name = "name", abbreviation = "abbreviation",
  component_type = "kind", text = "text", printed_number = "printed number",
  parent_question_id = "the question it depends on",
  variable_type = "type", label = "label", subject = "subject", time_frame = "time frame",
  answer_data_type = "answer data type", unit = "unit",
  answer_option = "wording",
  position = "position"
)
monitor_item_words <- c(questionnaire = "the questionnaire", block = "a block", variable = "a variable", choice = "a choice")
monitor_change_words <- c(edited = "Changed", added = "Added", deleted = "Removed", moved = "Moved")

# How many of a kind's repeated corrections the page lists.
monitor_examples_shown <- 5

# Every pushed AI-Powered extraction, oldest pushed first; see `monitor_model_get_pushed()`.
monitor_service_get_all <- function(connection) {
  monitor_model_get_pushed(connection)
}

# Every correction in some extractions, a row each.
# records: From `monitor_service_get_all()`.
# Returns: A data frame with `extraction_id`, `extraction` (its summary, or its PDF's name), `applied` (POSIXct),
#   `change`, `item`, `field` (NA for an addition or a removal), `where`, `before`, `after` (NA for none),
#   `kind` (`change`, `item` and `field` joined by "|") and `label` (the kind in words); no rows if there are none.
monitor_service_corrections <- function(records) {
  text_of <- function(value) if (is.null(value) || length(value) == 0) NA_character_ else as.character(value[[1]])
  rows <- lapply(records, function(record) {
    edits <- record$edits %||% list()
    if (length(edits) == 0) {
      return(NULL)
    }
    field_of <- function(edit) text_of(edit$field)
    data.frame(
      extraction_id = record$id,
      extraction = monitor_extraction_name(record),
      applied = parse_time(record$applied %||% NA_character_),
      change = vapply(edits, function(edit) text_of(edit$change), character(1)),
      item = vapply(edits, function(edit) text_of(edit$item), character(1)),
      field = vapply(edits, field_of, character(1)),
      where = vapply(edits, function(edit) text_of(edit$where), character(1)),
      before = vapply(edits, function(edit) text_of(edit$before), character(1)),
      after = vapply(edits, function(edit) text_of(edit$after), character(1)),
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(rows)) {
    rows <- data.frame(
      extraction_id = character(), extraction = character(), applied = as.POSIXct(character(), tz = "UTC"),
      change = character(), item = character(), field = character(), where = character(),
      before = character(), after = character(), stringsAsFactors = FALSE
    )
  }
  rows$kind <- paste(rows$change, rows$item, ifelse(is.na(rows$field), "", rows$field), sep = "|")
  rows$label <- monitor_kind_label(rows$change, rows$item, rows$field)
  rownames(rows) <- NULL
  rows
}

# What an extraction is called on the page: what staff said it was, or the PDF it was read from.
monitor_extraction_name <- function(record) {
  if (has_text(record$summary)) record$summary else if (has_text(record$pdf_name)) record$pdf_name else paste("Extraction", record$id)
}

# A kind of correction in words: "Changed a variable's unit", "Changed the questionnaire's name", "Removed a
# block", "Moved a choice".
# change, item, field: Vectors of the same length, as in `monitor_service_corrections()`.
monitor_kind_label <- function(change, item, field) {
  if (length(change) == 0) {
    return(character())
  }
  verb <- ifelse(change %in% names(monitor_change_words), monitor_change_words[change], change)
  thing <- ifelse(item %in% names(monitor_item_words), monitor_item_words[item], item)
  field_words <- ifelse(!is.na(field) & field %in% names(monitor_field_words), monitor_field_words[field], field)
  edited <- change == "edited" & !is.na(field)
  unname(ifelse(edited, paste0(verb, " ", thing, "'s ", field_words), paste(verb, thing)))
}

# The kinds of correction, the commonest first.
# corrections: From `monitor_service_corrections()`.
# extractions: How many extractions they come from (those with none included), for each kind's share.
# shown: How many of each kind's repeated corrections to keep.
# Returns: A list, one entry per kind: `kind`, `label`, `change`, `item`, `field`, `count` (how many
#   corrections), `extractions` (how many extractions had one), `share` (of all extractions, 0 to 1) and
#   `examples` (see `monitor_service_examples()`); empty if there are no corrections.
monitor_service_common <- function(corrections, extractions, shown = monitor_examples_shown) {
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
      examples = monitor_service_examples(rows, shown)
    )
  })
  counts <- vapply(common, function(kind) kind$count, numeric(1))
  labels <- vapply(common, function(kind) kind$label, character(1))
  unname(common[order(-counts, labels)])
}

# The corrections of one kind, the same before and after counted together, the commonest first.
# rows: Some rows of `monitor_service_corrections()`, all of one kind.
# shown: How many to keep.
# Returns: A data frame with `before`, `after`, `count`, `extractions` (how many extractions had it), `where`
#   and `extraction_id` (the first one), and `extraction` (its name); at most `shown` rows.
monitor_service_examples <- function(rows, shown = monitor_examples_shown) {
  key <- paste(ifelse(is.na(rows$before), "\r", rows$before), ifelse(is.na(rows$after), "\r", rows$after), sep = "\n")
  groups <- split(seq_len(nrow(rows)), factor(key, levels = unique(key)))
  examples <- do.call(rbind, lapply(groups, function(at) {
    first <- at[1]
    data.frame(
      before = rows$before[first], after = rows$after[first], count = length(at),
      extractions = length(unique(rows$extraction_id[at])), where = rows$where[first],
      extraction_id = rows$extraction_id[first], extraction = rows$extraction[first],
      stringsAsFactors = FALSE
    )
  }))
  examples <- examples[order(-examples$count, -examples$extractions), , drop = FALSE]
  rownames(examples) <- NULL
  utils::head(examples, shown)
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
# tz: The time zone whose days count (the site shows times in local time).
# Returns: A data frame with `start` (Date), `label`, `extractions` and `corrections`, oldest first, and the
#   unit as its attribute `unit`; no rows if there are no records.
monitor_service_over_time <- function(records, unit = NULL, tz = "") {
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
