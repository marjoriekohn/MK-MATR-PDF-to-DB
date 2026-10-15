# Checks what the database lets the site's keys do on the tables the site uses,
# without changing or printing any data.
#
# It reads SUPABASE_URL, SUPABASE_SECRET_KEY and SUPABASE_PUBLISHABLE_KEY
# and prints one line per table:
#
#   secret key  read / update / delete / insert   what the site itself needs
#   publishable read                              what the public dashboard gets
#
# It exits with an error if
# - the secret key can't do all four on a table that exists,
# - the publishable key can read a table in `private_tables` (staff and the drafts, applied or
#   not, must stay hidden from the dashboard), or can't read one of the others, or
# - the database has a table this script doesn't list, so a new table can't go unchecked.

if (file.exists(".env")) readRenviron(".env")
source("R/model_database.R")
source("R/model_table.R") # database_tables: the tables the edit pages read

secret <- list(url = Sys.getenv("SUPABASE_URL"),
               key = Sys.getenv("SUPABASE_SECRET_KEY"))
publishable <- list(url = Sys.getenv("SUPABASE_URL"),
                    key = Sys.getenv("SUPABASE_PUBLISHABLE_KEY"))

# Tables the public dashboard must NOT be able to read:
# RLS on, no policy, no grant.
private_tables <- c(
  "staff", "draft", "extracted_draft", "edit_draft", "applied_item"
)

# Each table with a column to read and filter on (a key: a number or text, so
# "equals 0 and not 0" parses) and a column to aim an update at.
probe_columns <- lapply(database_tables, function(spec) {
  other <- setdiff(names(spec$columns), spec$key)
  list(read = spec$key[1],
       change = if (length(other) > 0) other[1] else spec$key[1])
})
other_tables <- list(
  option_set = list(read = "option_set_id", change = "name"),
  staff = list(read = "staff_id", change = "name"),
  draft = list(read = "draft_id", change = "summary"),
  extracted_draft = list(read = "draft_id", change = "pdf_name"),
  edit_draft = list(read = "draft_id", change = "form"),
  applied_item = list(read = "draft_id", change = "entity_name")
)
tables <- c(probe_columns, other_tables)

# "ok", or why not, in a few words.
outcome <- function(code) {
  tryCatch(
    {
      code
      "ok"
    },
    error = function(problem) {
      message <- conditionMessage(problem)
      if (grepl("won't let the site use this table", message)) {
        return("denied")
      }
      if (grepl("doesn't have that table", message)) {
        return("missing")
      }
      paste("problem:", substr(sub("\n.*", "", message), 1, 60))
    }
  )
}

# A filter no row can pass, so a change or delete never touches anything.
nothing <- function(column) stats::setNames(list("eq.0", "neq.0"), c(column, column))

probe_read <- function(connection, table, column) {
  outcome(database_model_get(connection, table, column, filters = list(), page_size = 1))
}
probe_update <- function(connection, table, column, where) {
  outcome(database_model_update(connection, table, stats::setNames(list(NULL), column), nothing(where), select = where))
}
probe_delete <- function(connection, table, where) {
  outcome(
    database_request(connection, table) |>
      httr2::req_method("DELETE") |>
      database_filter(nothing(where)) |>
      database_perform()
  )
}
probe_insert <- function(connection, table) {
  outcome(
    database_request(connection, table) |>
      httr2::req_method("POST") |>
      httr2::req_body_raw("[]", type = "application/json") |>
      database_perform()
  )
}

cat(sprintf("%-26s %-6s %-8s %-8s %-8s | %s\n", "table", "read", "update", "delete", "insert", "publishable read"))
failed <- character()
exposed <- character()
unreadable <- character()
for (table in names(tables)) {
  columns <- tables[[table]]
  results <- c(
    read = probe_read(secret, table, columns$read),
    update = probe_update(secret, table, columns$change, where = columns$read),
    delete = probe_delete(secret, table, where = columns$read),
    insert = probe_insert(secret, table)
  )
  dashboard <- probe_read(publishable, table, columns$read)
  cat(sprintf(
    "%-26s %-6s %-8s %-8s %-8s | %s\n", table, results[["read"]], results[["update"]],
    results[["delete"]], results[["insert"]], dashboard
  ))
  if (!"missing" %in% results && any(results != "ok")) failed <- c(failed, table)
  if (table %in% private_tables && identical(dashboard, "ok")) exposed <- c(exposed, table)
  if (!table %in% private_tables && !"missing" %in% results && !identical(dashboard, "ok")) {
    unreadable <- c(unreadable, table)
  }
}

# A table the database has and this script doesn't know can't be checked, so say so.
unlisted <- setdiff(names(database_model_describe(secret)$definitions), names(tables))

problems <- c(
  if (length(failed) > 0) paste("The secret key can't do everything on:", paste(failed, collapse = ", ")),
  if (length(exposed) > 0) paste("The publishable key can read, and must not:", paste(exposed, collapse = ", ")),
  if (length(unreadable) > 0) paste("The publishable key can't read, and the dashboard needs to:", paste(unreadable, collapse = ", ")),
  if (length(unlisted) > 0) paste("Tables in the database that this script doesn't check yet:", paste(unlisted, collapse = ", "))
)
if (length(problems) > 0) {
  cat("\n", paste(problems, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}
cat(
  "\nThe secret key can read, update, delete and insert on every table that exists.",
  "\nThe publishable key is denied on ", paste(private_tables, collapse = ", "), " and reads the rest.\n",
  sep = ""
)
