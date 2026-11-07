# Words, times and sizes written the way staff read them. Nothing here knows
# about MATR's data or about Shiny.

# ------ Times, sizes and lists -----------------------------------------------------
# A time as the app keeps it.
# Times are kept as text in UTC, e.g. `"2026-09-29T14:03:22Z"`, and shown in local time.
format_time <- function(time) {
  format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# A time kept as text, as a date-time.
# The database sends times with fractions and an offset (`"2026-09-29T14:03:22.5+00:00"`);
# this reads those as well as the app's own.
# text: A time as text.
# Returns: A `POSIXct` in UTC; `NA` for text it can't read.
parse_time <- function(text) {
  as.POSIXct(sub("Z$", "+0000", sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", text)),
    format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"
  )
}

# How long ago, in words.
# For example "just now", "5 minutes ago", "2 hours ago", "yesterday", "on Sep 29".
time_ago <- function(time, now = Sys.time()) {
  minutes <- as.numeric(difftime(now, time, units = "mins"))
  if (minutes < 1) {
    return("just now")
  }
  if (minutes < 60) {
    return(plural(floor(minutes), "minute", "ago"))
  }
  if (as.Date(time, tz = "") == as.Date(now, tz = "")) {
    return(plural(floor(minutes / 60), "hour", "ago"))
  }
  day_phrase(time, now)
}

# The day of a time, in words.
# For example "today at 9:15 AM", "yesterday", "on Sep 29", or "on Sep 29, 2024" in another year.
day_phrase <- function(time, now = Sys.time()) {
  days <- as.numeric(as.Date(now, tz = "") - as.Date(time, tz = ""))
  if (days == 0) {
    return(paste("today at", sub("^0", "", format(time, "%I:%M %p", tz = ""))))
  }
  if (days == 1) {
    return("yesterday")
  }
  day <- paste("on", format(time, "%b", tz = ""), as.integer(format(time, "%d", tz = "")))
  if (format(time, "%Y", tz = "") != format(now, "%Y", tz = "")) day <- paste0(day, ", ", format(time, "%Y", tz = ""))
  day
}

# A number of seconds in words.
# For example "40 s", "1 min 40 s".
# seconds: A number of seconds.
duration_text <- function(seconds) {
  seconds <- floor(as.numeric(seconds))
  if (seconds < 60) {
    return(paste(seconds, "s"))
  }
  paste(seconds %/% 60, "min", seconds %% 60, "s")
}

# A size in bytes in words.
# For example "2.1 MB", "640 KB".
# bytes: A number of bytes.
file_size <- function(bytes) {
  if (bytes >= 1024^2) sprintf("%.1f MB", bytes / 1024^2) else sprintf("%.0f KB", bytes / 1024)
}

# A count with its noun.
# For example "1 page", "3 pages", "2 hours ago", "1,200 waves".
# noun: The singular noun.
# after: Words to add after the noun, e.g. `"ago"`.
# plural_form: The noun for any other count; defaults to adding an "s".
# big_mark: Put between thousands in the count, e.g. `","`; none by default.
plural <- function(n, noun, after = NULL, plural_form = paste0(noun, "s"), big_mark = "") {
  count <- if (nzchar(big_mark)) format(n, big.mark = big_mark, trim = TRUE) else n
  paste(c(count, if (n == 1) noun else plural_form, after), collapse = " ")
}

# Text with each run of spaces, tabs and new lines made one space, and none at either end. `NA` stays `NA`.
# text: A character vector.
squish <- function(text) trimws(gsub("[[:space:]]+", " ", text))

# A name with its abbreviation: "Adult Health Study (AHS)", or the one there is when the other is blank or `NA`.
# name,abbreviation: Character vectors of the same length.
name_with_abbreviation <- function(name, abbreviation) {
  has_name <- !is.na(name) & nzchar(name)
  has_abbreviation <- !is.na(abbreviation) & nzchar(abbreviation)
  ifelse(has_name & has_abbreviation, paste0(name, " (", abbreviation, ")"), ifelse(has_abbreviation, abbreviation, name))
}

# Words as a list.
# For example "study", "study and wave name", "registry, study and wave name".
# words: A character vector.
word_list <- function(words) {
  as.character(glue::glue_collapse(words, sep = ", ", last = " and "))
}

# Whether text has anything in it besides spaces.
# x: A character vector; `NULL` and `NA` count as nothing.
has_text <- function(x) {
  x <- x %||% ""
  !is.na(x) & nzchar(trimws(x))
}

# An age range in words.
# For example "5-17", "18+", "up to 17", or "any age" when neither age is known: a respondent
# type needs neither.
# minimum,maximum: The youngest and oldest age; each can be `NA`. Vectors are fine.
age_range_text <- function(minimum, maximum) {
  ifelse(
    is.na(minimum) & is.na(maximum), "any age",
    ifelse(is.na(maximum), paste0(minimum, "+"), ifelse(is.na(minimum), paste0("up to ", maximum), paste0(minimum, "-", maximum)))
  )
}
