# ------ Staff ---------------------------------------------------------------------
# Who may use the site: the staff table of the MATR database (`connection`, as in model_database.R).
# Each is a STAFF or an ADMIN (`role`, kept for the site's own use: nothing needs an ADMIN). Staff
# are never deleted, only marked inactive (the history names them).

staff_columns <- c("staff_id", "name", "email", "active", "role")

# Find a staff member by email.
# Emails are unique ignoring case. The search also treats `_` as any character, so
# only the exact match is kept.
# email: The email to look for.
# Returns: A list with `staff_id`, `name`, `email`, `active` and `role`, or `NULL` if there is no one.
staff_model_find_by_email <- function(connection, email) {
  rows <- database_model_get(
    connection, "staff", staff_columns,
    order_by = "staff_id", filters = list(email = paste0("ilike.", gsub("([*%\\\\])", "\\\\\\1", email)))
  )
  # ilike also reads "_" as any one character, so keep only the exact match.
  rows <- rows[tolower(rows$email) == tolower(email), , drop = FALSE]
  if (nrow(rows) == 1) as.list(rows)
}
