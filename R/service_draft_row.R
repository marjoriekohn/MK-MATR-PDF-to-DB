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
  staff_name(record$prepared_by)
}

# ------ One person at a time -----------------------------------------------------
# Whoever has a draft open on the Edit Extraction page has claimed it (draft_row_model_claim()), so
# nobody else can change it there and overwrite their work. Their page renews the claim every
# `draft_claim_renew_seconds` while they are there, and lets it go when they leave the page, close
# it, or have done nothing for `draft_claim_idle_seconds`. A claim not renewed for
# `draft_claim_stale_seconds` has lapsed: its page is gone (a closed laptop, a lost connection), so
# nobody is kept out by it. Anyone may take a draft over from whoever has it.
draft_claim_renew_seconds <- 60
draft_claim_stale_seconds <- 180
draft_claim_idle_seconds <- 15 * 60

#' Who else is working on a draft right now, if anyone.
#'
#' @param record An extraction record, with its `editing` (see `model_extraction.R`).
#' @param me Who is using the site: `list(staff_id, name)`.
#' @param now The time now.
#' @return `list(by_id, by, at)`: who has it (`by` is their name) and when their page last renewed the
#'   claim. `NULL` if nobody does, it is this person, or the claim has lapsed.
draft_row_service_editor <- function(record, me, now = Sys.time()) {
  editing <- record$editing
  if (is.null(editing) || same_staff(editing$by_id, me$staff_id) || draft_row_service_lapsed(editing$at, now)) {
    return(NULL)
  }
  editing$by <- staff_name(editing$by)
  editing
}

#' Whether a claim renewed at `at` has lapsed.
#'
#' @param at When it was last renewed, as text; `NULL` for no claim.
#' @param now The time now.
#' @return `TRUE` or `FALSE`.
draft_row_service_lapsed <- function(at, now = Sys.time()) {
  at <- if (length(at) == 1) parse_time(at)
  length(at) != 1 || is.na(at) || as.numeric(difftime(now, at, units = "secs")) > draft_claim_stale_seconds
}

#' Claim a draft for this person to work on, or renew their claim.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param me Who is claiming it: `list(staff_id, name)`.
#' @param take_over Whether to take it from whoever has it.
#' @return As `draft_row_model_claim()`: `claimed`, who has it (`by_id`, `by`, `at`) and `revision`.
#'   An error with a sentence if the draft is gone or finished with.
draft_row_service_claim <- function(connection, id, me, take_over = FALSE) {
  claim <- draft_row_model_claim(connection, id, me$staff_id, take_over, draft_claim_stale_seconds)
  claim$by <- staff_name(claim$by)
  claim
}

#' Let go of this person's claim on a draft, if they still have it.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param id The draft's id.
#' @param me Who is letting it go: `list(staff_id, name)`.
#' @return Whether they had it.
draft_row_service_release <- function(connection, id, me) {
  draft_row_model_release(connection, id, me$staff_id)
}

# A staff member's name as it is shown, or "Someone no longer listed" when the staff table has none.
staff_name <- function(name) {
  if (length(name) != 1 || is.na(name) || !has_text(name)) "Someone no longer listed" else trimws(name)
}

# Two staff ids are the same person's when they are the same number.
same_staff <- function(a, b) {
  length(a) == 1 && length(b) == 1 && !is.na(a) && !is.na(b) &&
    isTRUE(suppressWarnings(as.numeric(a) == as.numeric(b)))
}
