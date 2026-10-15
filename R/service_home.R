# ------ The Home page's lists ----------------------------------------------------------------
# What the Home page shows besides its buttons: the newest changes to the MATR database (from
# the Edit history) and the newest change requests waiting for approval. Only reads.

# How many of the newest change requests and applied changes the Home page lists.
home_recent_n <- 4

#' The newest change requests waiting for approval.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param n How many at most.
#' @return A list: `shown` (the newest `n` requests, most recently sent first, as
#'   `change_request_service_waiting()` returns them) and `total` (how many are waiting).
home_service_recent_requests <- function(connection, n = home_recent_n) {
  waiting <- change_request_service_waiting(connection) # longest waiting first
  list(shown = utils::head(rev(unname(waiting)), n), total = length(waiting))
}

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
