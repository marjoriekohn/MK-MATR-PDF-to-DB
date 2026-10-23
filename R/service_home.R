# ------ The Home page's lists ----------------------------------------------------------------
# What the Home page lists: this person's work in progress, the change requests waiting for them, and the
# newest changes to the MATR database (from the Edit History). Only reads.

# How many change requests and applied changes the Home page lists (its buttons lead to the rest).
home_recent_n <- 4

#' The newest changes applied to the MATR metadata database.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param n How many at most.
#' @return A list: `shown` (the newest `n` applied changes, as `history_service_get_all()` returns
#'   them) and `total` (how many there are).
home_service_recent_history <- function(connection, n = home_recent_n) {
  records <- history_service_get_all(connection) # newest first
  list(shown = utils::head(unname(records), n), total = length(records))
}

#' "See all 12" or "See more", for the button under a list.
#'
#' @param total How many there are in all.
#' @param shown How many are listed.
#' @return A string.
home_service_more_label <- function(total, shown) {
  if (total > shown) sprintf("See all %d", total) else "See more"
}

#' This person's work in progress: the extraction drafts they started and haven't sent for approval yet, and
#' the edits they saved on the Metadata page.
#'
#' Those sent back to them come first (a reviewer is waiting on them), then the most recently saved.
#'
#' @param extractions Extraction records, from `extraction_service_get_all()`.
#' @param edits Edit records, from `edit_service_get_all()`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @return A list of items, each `list(kind, record)`: `kind` is `"extraction"` or `"edit"`.
home_service_my_work <- function(extractions, edits, me) {
  items <- c(
    lapply(unname(extraction_service_open_drafts(extractions, prepared_by_id = me$staff_id)), function(record) {
      list(kind = "extraction", record = record)
    }),
    lapply(unname(edit_service_open(edits, prepared_by_id = me$staff_id, status = c("Draft", "Sent Back"))), function(record) {
      list(kind = "edit", record = record)
    })
  )
  if (length(items) == 0) {
    return(list())
  }
  sent_back <- vapply(items, function(item) identical(item$record$status, "Sent Back"), logical(1))
  saved <- vapply(items, function(item) as.numeric(parse_time(item$record$saved)), numeric(1))
  items[order(!sent_back, -saved)]
}

#' The change requests waiting for this person: those they can approve (the ones they were asked to review
#' first), and their own extractions still waiting for Basic Info, or that an ADMIN adds without approval.
#'
#' The same as the Change Requests page's "For me".
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param n How many at most.
#' @return A list: `shown` (the first `n`) and `total` (how many there are).
home_service_waiting_for_me <- function(connection, me, n = home_recent_n) {
  mine <- change_request_service_filter(change_request_service_waiting(connection), me, "me")
  list(shown = utils::head(unname(mine), n), total = length(mine))
}
