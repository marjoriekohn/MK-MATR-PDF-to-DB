# ------ The Home page's counts ----------------------------------------------------------------
# What the Home page's banner counts: this person's extraction drafts in progress, and the changes added to
# the MATR metadata database (the Edit History). Only reads.

# This person's work in progress: the extraction drafts they started and haven't pushed yet.
# extractions: Extraction records, from `extraction_service_get_all()`.
# Returns: How many there are.
home_service_my_work <- function(extractions, me) {
  mine <- Filter(function(record) identical(record$status, "Draft") && isTRUE(record$prepared_by_id == me$staff_id), extractions)
  length(mine)
}

# The changes added to the MATR metadata database so far: the rows of the history.
# Returns: How many there are.
home_service_changes_added <- function(connection) {
  history_service_count(connection)
}
