# ------ The Home page's counts ----------------------------------------------------------------
# What the Home page's banner counts: this person's work in progress, the change requests waiting for them, and
# the changes added to the MATR metadata database (the Edit History). Only reads.

# This person's work in progress: the extraction drafts they started and haven't sent for approval yet, and the
# edits they saved on the Metadata page, those sent back to them included.
# extractions: Extraction records, from `extraction_service_get_all()`.
# edits: Edit records, from `edit_service_get_all()`.
# Returns: How many there are.
home_service_my_work <- function(extractions, edits, me) {
  length(extraction_service_open_drafts(extractions, prepared_by_id = me$staff_id)) +
    length(edit_service_open(edits, prepared_by_id = me$staff_id, status = c("Draft", "Sent Back")))
}

# The change requests waiting for this person: the Change Requests page's "Waiting for me".
# Returns: How many there are.
home_service_waiting_for_me <- function(connection, me) {
  length(change_request_service_filter(change_request_service_waiting(connection), me, "me"))
}

# The changes added to the MATR metadata database so far.
# Returns: How many there are.
home_service_changes_added <- function(connection) {
  history_model_count(connection)
}
