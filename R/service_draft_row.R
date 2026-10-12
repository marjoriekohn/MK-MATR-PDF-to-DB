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
      (is.null(prepared_by_id) || identical(record$prepared_by_id, prepared_by_id))
  }, records)
}
