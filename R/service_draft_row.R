# ------ Drafts staff are still working on ------------------------------------------
# What extraction drafts and edit proposals have in common: both are drafts
# (model_draft_row.R) with a status and the staff member who prepared them.

#' The records still open.
#'
#' @param records A list of extraction or edit records.
#' @param statuses The statuses that count as open.
#' @param prepared_by_id Only those prepared by this staff member, if given.
#' @return The matching records.
draft_row_service_open <- function(records, statuses, prepared_by_id = NULL) {
  Filter(function(record) {
    record$status %in% statuses &&
      (is.null(prepared_by_id) || same_staff(record$prepared_by_id, prepared_by_id))
  }, records)
}

#' Whether a draft is this person's: they started it.
#'
#' Staff ids are compared as numbers, however they were read (a whole number from one reply, a
#' double from another), so a draft is never someone else's only because of how its id was stored.
#'
#' @param record An extraction or edit record.
#' @param me Who is using the site: `list(staff_id, name)`.
#' @return `TRUE` or `FALSE`.
draft_row_service_is_mine <- function(record, me) {
  same_staff(record$prepared_by_id, me$staff_id)
}

#' Who started a draft, by name.
#'
#' @param record An extraction or edit record.
#' @return Their name, or "Someone no longer listed" when the staff table has no name for them.
draft_row_service_preparer <- function(record) {
  name <- record$prepared_by
  if (length(name) != 1 || is.na(name) || !has_text(name)) "Someone no longer listed" else trimws(name)
}

# Two staff ids are the same person's when they are the same number.
same_staff <- function(a, b) {
  length(a) == 1 && length(b) == 1 && !is.na(a) && !is.na(b) &&
    isTRUE(suppressWarnings(as.numeric(a) == as.numeric(b)))
}
