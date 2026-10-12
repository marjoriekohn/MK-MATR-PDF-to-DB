# Words, times and sizes written the way staff read them. Nothing here knows
# about MATR's data or about Shiny.

# ------ Times, sizes and lists -----------------------------------------------------
#' A time as the app keeps it.
#'
#' Times are kept as text in UTC, e.g. `"2026-09-29T14:03:22Z"`, and shown in local time.
#'
#' @param time A date-time.
#' @return A string.
format_time <- function(time) {
  format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' A time kept as text, as a date-time.
#'
#' The database sends times with fractions and an offset (`"2026-09-29T14:03:22.5+00:00"`);
#' this reads those as well as the app's own.
#'
#' @param text A time as text.
#' @return A `POSIXct` in UTC; `NA` for text it can't read.
parse_time <- function(text) {
  as.POSIXct(sub("Z$", "+0000", sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", text)),
    format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"
  )
}

#' How long ago, in words.
#'
#' For example "just now", "5 minutes ago", "2 hours ago", "yesterday", "on Sep 29".
#'
#' @param time A date-time.
#' @param now What to measure from.
#' @return A string.
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

#' The day of a time, in words.
#'
#' For example "today at 9:15 AM", "yesterday", "on Sep 29".
#'
#' @param time A date-time.
#' @param now What to measure from.
#' @return A string.
day_phrase <- function(time, now = Sys.time()) {
  days <- as.numeric(as.Date(now, tz = "") - as.Date(time, tz = ""))
  if (days == 0) {
    return(paste("today at", sub("^0", "", format(time, "%I:%M %p", tz = ""))))
  }
  if (days == 1) {
    return("yesterday")
  }
  paste("on", format(time, "%b", tz = ""), as.integer(format(time, "%d", tz = "")))
}

#' A number of seconds in words.
#'
#' For example "40 s", "1 min 40 s".
#'
#' @param seconds A number of seconds.
#' @return A string.
duration_text <- function(seconds) {
  seconds <- floor(as.numeric(seconds))
  if (seconds < 60) {
    return(paste(seconds, "s"))
  }
  paste(seconds %/% 60, "min", seconds %% 60, "s")
}

#' A size in bytes in words.
#'
#' For example "2.1 MB", "640 KB".
#'
#' @param bytes A number of bytes.
#' @return A string.
file_size <- function(bytes) {
  if (bytes >= 1024^2) sprintf("%.1f MB", bytes / 1024^2) else sprintf("%.0f KB", bytes / 1024)
}

#' A count with its noun.
#'
#' For example "1 page", "3 pages", "2 hours ago", "1,200 waves".
#'
#' @param n The count.
#' @param noun The singular noun.
#' @param after Words to add after the noun, e.g. `"ago"`.
#' @param plural_form The noun for any other count; defaults to adding an "s".
#' @param big_mark Put between thousands in the count, e.g. `","`; none by default.
#' @return A string.
plural <- function(n, noun, after = NULL, plural_form = paste0(noun, "s"), big_mark = "") {
  count <- if (nzchar(big_mark)) format(n, big.mark = big_mark, trim = TRUE) else n
  paste(c(count, if (n == 1) noun else plural_form, after), collapse = " ")
}

#' Words as a list.
#'
#' For example "study", "study and wave name", "registry, study and wave name".
#'
#' @param words A character vector.
#' @return A string.
word_list <- function(words) {
  as.character(glue::glue_collapse(words, sep = ", ", last = " and "))
}

#' Whether text has anything in it besides spaces.
#'
#' @param x A character vector; `NULL` counts as nothing.
#' @return A logical vector.
has_text <- function(x) {
  nzchar(trimws(x %||% ""))
}

#' An age range in words.
#'
#' For example "5-17", "18+", "up to 17", or "any age" when neither age is known: a respondent
#' type needs neither.
#'
#' @param minimum,maximum The youngest and oldest age; each can be `NA`. Vectors are fine.
#' @return A character vector.
age_range_text <- function(minimum, maximum) {
  ifelse(
    is.na(minimum) & is.na(maximum), "any age",
    ifelse(is.na(maximum), paste0(minimum, "+"), ifelse(is.na(minimum), paste0("up to ", maximum), paste0(minimum, "-", maximum)))
  )
}
