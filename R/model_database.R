# ------ The MATR database ----------------------------------------------------------
# The live tables (REGISTRY, STUDY, QUESTIONNAIRE, WAVE...) and the drafts staff
# are still working on (DRAFT and its detail tables) are in a PostgreSQL database that
# Supabase hosts. The site talks to it through Supabase's REST API:
#   curl "$SUPABASE_URL/rest/v1/registry?select=name,abbreviation" -H "apikey: $SUPABASE_SECRET_KEY"
#
# The secret key gets past Row Level Security, which hides STAFF, WRITE_HISTORY
# and the drafts from the publishable key, so it never goes to a browser or into
# a log.
#
# This file is the only place that makes requests to the database. Every other
# model function names a table and what it wants from it; services and modules
# never build a request. `connection` is list(url, key), made in global.R.
# Errors are sentences staff can act on.

# Supabase sends at most 1,000 rows per request, so longer tables come in pages.
database_page_size <- 1000

# A request gives up after this long, and is tried up to this many times when
# Supabase is busy (429 or 503).
database_timeout_seconds <- 20
database_max_tries <- 3

# ------ Reading ------------------------------------------------------------------
#' Read a table of the MATR database.
#'
#' Every row of `table`, with only `columns`, sorted by `order_by`.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param table The table's name.
#' @param columns The columns to read, as a character vector.
#' @param order_by The columns to sort by; each can end in `.desc`. Defaults to the first column.
#' @param filters Narrow the rows: a named list of PostgREST filters, e.g. `list(kind = database_eq("Edit"))`.
#' @param page_size Rows asked for at a time; Supabase sends at most 1,000.
#' @return A data frame with `columns`; with no rows, one whose columns are logical.
database_model_get <- function(connection, table, columns, order_by = columns[1],
                               filters = list(), page_size = database_page_size) {
  pages <- database_fetch(
    connection, table, columns, order_by, filters, page_size,
    parse = function(body) jsonlite::fromJSON(body)
  )
  if (length(pages) == 0) {
    return(no_rows(columns))
  }
  rows <- do.call(rbind, pages)
  rows[columns]
}

#' Read a table whose rows hold jsonb.
#'
#' Like `database_model_get()`, but each row comes back as a list, and a jsonb value
#' as the lists it was saved as, not turned into data frames.
#'
#' @inheritParams database_model_get
#' @param select PostgREST's `select`: columns, which can embed another table, e.g. `"preparer:staff!prepared_by(name)"`.
#' @param order_by The columns to sort by, or `NULL` for the database's order.
#' @return A list with one list per row; empty if there are none.
database_model_select <- function(connection, table, select, order_by = NULL,
                                  filters = list(), page_size = database_page_size) {
  pages <- database_fetch(
    connection, table, select, order_by, filters, page_size,
    parse = function(body) jsonlite::fromJSON(body, simplifyVector = FALSE)
  )
  as.list(unlist(pages, recursive = FALSE))
}

# The pages of rows, each as `parse` makes them from the reply's body. httr2 asks for one page
# after another, moving `offset` on by a page each time, until the reply says it was the last.
database_fetch <- function(connection, table, select, order_by, filters, page_size, parse) {
  first_page <- database_request(connection, table) |>
    database_filter(filters) |>
    httr2::req_url_query(select = select, order = order_by, .multi = "comma") |>
    httr2::req_url_query(limit = page_size) |>
    httr2::req_headers(Prefer = "count=exact") # so the reply says how many rows there are
  responses <- tryCatch(
    httr2::req_perform_iterative(
      first_page,
      next_req = httr2::iterate_with_offset("offset", start = 0, offset = page_size, resp_complete = database_last_page),
      max_reqs = Inf, progress = FALSE
    ),
    error = function(problem) stop("Couldn't reach the MATR database: ", conditionMessage(problem), call. = FALSE)
  )
  pages <- lapply(responses, function(response) {
    if (httr2::resp_status(response) >= 400) stop(database_problem(response), call. = FALSE)
    parse(httr2::resp_body_string(response))
  })
  Filter(function(page) NROW(page) > 0, pages)
}

# Whether a reply is the last page: its Content-Range ("0-999/1500", or "*/0" for none) reaches
# the end of the table. A reply with no range counts as the last.
database_last_page <- function(resp) {
  range <- httr2::resp_header(resp, "Content-Range") %||% ""
  found <- regmatches(range, regexec("^[0-9]+-([0-9]+)/([0-9]+)$", range))[[1]]
  if (length(found) == 0) {
    return(TRUE)
  }
  as.numeric(found[2]) + 1 >= as.numeric(found[3])
}

#' How many rows a table has.
#'
#' Asks for none of them: only the count the reply carries.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param table The table's name.
#' @param filters Narrow the rows, as for `database_model_get()`.
#' @return A whole number.
database_model_count <- function(connection, table, filters = list()) {
  response <- database_request(connection, table) |>
    database_filter(filters) |>
    httr2::req_url_query(select = "*", limit = 0) |>
    httr2::req_headers(Prefer = "count=exact") |>
    database_perform()
  as.integer(rows_in_table(response))
}

#' The database's own description of its tables and functions.
#'
#' The REST API publishes it as an OpenAPI document at its root: every table with its
#' columns, their types and defaults, which are required to add a row, and the keys that
#' point at one column. It doesn't say anything about check or unique constraints,
#' foreign keys over several columns, indexes, triggers or policies.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return The document as a list: `definitions` (one entry for each table) and `paths`
#'   (the tables, and `/rpc/...` for each function).
database_model_describe <- function(connection) {
  database_rows(database_perform(database_request(connection, "")))
}

# ------ Writing ------------------------------------------------------------------
#' Add one row to a table.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param table The table's name.
#' @param row A named list of columns to values; a jsonb value is a list.
#' @param select The columns to send back. Limit it for a row holding big jsonb.
#' @return The row as the database stored it, as a list, with the columns it filled in.
database_model_insert <- function(connection, table, row, select = "*") {
  response <- database_request(connection, table) |>
    httr2::req_method("POST") |>
    httr2::req_url_query(select = select, .multi = "comma") |>
    httr2::req_headers(Prefer = "return=representation") |>
    database_body(list(row)) |>
    database_perform()
  database_rows(response)[[1]]
}

#' Change the rows of a table that match filters.
#'
#' There must be a filter: this never changes a whole table.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param table The table's name.
#' @param changes A named list of columns to their new values (`NULL` sets null).
#' @param filters Which rows, as for `database_model_get()`.
#' @param select The columns to send back.
#' @return The rows as they are now, as a list of lists. Empty if nothing matched, so
#'   a caller can tell its change didn't apply.
database_model_update <- function(connection, table, changes, filters, select = "*") {
  if (length(filters) == 0) stop("Changing a table needs to say which rows.", call. = FALSE)
  response <- database_request(connection, table) |>
    httr2::req_method("PATCH") |>
    database_filter(filters) |>
    httr2::req_url_query(select = select, .multi = "comma") |>
    httr2::req_headers(Prefer = "return=representation") |>
    database_body(changes) |>
    database_perform()
  database_rows(response)
}

#' Call a function of the database.
#'
#' For what has to change several tables at once: separate requests can't share a transaction,
#' a function does. It is called through `/rpc/<name>` with named arguments.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param name The function's name, e.g. `"apply_pending_edit"`.
#' @param arguments A named list of its arguments.
#' @return What the function returns, as R values. An error with the function's own sentence if
#'   it refused, e.g. an edit that isn't approved.
database_model_call <- function(connection, name, arguments = list()) {
  response <- database_request(connection, paste0("rpc/", name)) |>
    httr2::req_method("POST") |>
    database_body(arguments) |>
    database_perform()
  database_rows(response)
}

# ------ Filters ------------------------------------------------------------------
#' A PostgREST filter for "equals".
#'
#' A filter is a column's name and what to do with it. The value is encoded into the
#' URL by httr2, so what staff type is never pasted in.
#'
#' @param value What the column must equal.
#' @return A string for use in a `filters` list, e.g. `list(kind = database_eq("Edit"))`.
database_eq <- function(value) paste0("eq.", value)

#' A PostgREST filter for "is one of".
#'
#' @param values What the column may equal; none matches no row.
#' @return A string for use in a `filters` list, e.g. `list(draft_id = database_in(c(1, 2)))`.
database_in <- function(values) {
  # The list is part of the filter's own syntax, so only plain values (numbers, ids) may go in it.
  if (!all(grepl("^[A-Za-z0-9_.-]+$", as.character(values)))) stop("A filter can only list plain values.", call. = FALSE)
  paste0("in.(", paste(values, collapse = ","), ")")
}

database_filter <- function(request, filters) {
  if (length(filters) == 0) {
    return(request)
  }
  do.call(httr2::req_url_query, c(list(request), filters))
}

# ------ Requests -----------------------------------------------------------------
# A request to one table of the REST API. Supabase's new keys (sb_secret_...,
# sb_publishable_...) go in the apikey header only: they aren't JWTs, so they
# don't go in Authorization, which is for a signed-in user's token.
database_request <- function(connection, table) {
  if (!nzchar(connection$url %||% "") || !nzchar(connection$key %||% "")) {
    stop(
      "The site doesn't know where the MATR database is. Add SUPABASE_URL and ",
      "SUPABASE_SECRET_KEY to the .env file and restart the site.",
      call. = FALSE
    )
  }
  httr2::request(connection$url) |>
    httr2::req_url_path_append("rest", "v1", table) |>
    httr2::req_headers(apikey = connection$key, .redact = "apikey") |>
    httr2::req_timeout(database_timeout_seconds) |>
    httr2::req_retry(max_tries = database_max_tries) |>
    httr2::req_error(is_error = function(response) FALSE)
}

# JSON with the nulls and blanks the database stores as null, and data frames
# (such as an edit's items) as a list of rows.
database_body <- function(request, data) {
  httr2::req_body_json(request, data, auto_unbox = TRUE, null = "null", na = "null", dataframe = "rows")
}

database_perform <- function(request) {
  response <- tryCatch(httr2::req_perform(request), error = function(problem) {
    stop("Couldn't reach the MATR database: ", conditionMessage(problem), call. = FALSE)
  })
  if (httr2::resp_status(response) >= 400) stop(database_problem(response), call. = FALSE)
  response
}

database_rows <- function(response) {
  jsonlite::fromJSON(httr2::resp_body_string(response), simplifyVector = FALSE)
}

database_problem <- function(response) {
  status <- httr2::resp_status(response)
  body <- tryCatch(httr2::resp_body_json(response), error = function(problem) list())
  message <- body$message %||% ""
  # P0001 is a function of ours refusing with a sentence for staff (supabase/migrations/): show it as written.
  if (identical(body$code, "P0001") && nzchar(message)) {
    return(message)
  }
  # 42501 is PostgreSQL's "permission denied": the key is fine, but the table
  # hasn't been granted to it (supabase/migrations/).
  if (identical(body$code, "42501")) {
    return(paste0(
      "The MATR database won't let the site use this table (", message, "). ",
      "The table needs granting to the service_role; see the migrations in supabase/migrations/."
    ))
  }
  switch(as.character(status),
         "401" = ,
         "403" = "The MATR database turned down the key. Check SUPABASE_SECRET_KEY in the .env file.",
         "404" = paste("The MATR database doesn't have that table:", message),
         "409" = paste("The MATR database wouldn't save that:", message),
         sprintf("The MATR database had a problem (%s): %s", status, message)
  )
}

# The table's row count, from the reply's Content-Range header, e.g.
# "0-999/1500" (or "*/0" for none). Unknown counts as no more rows.
rows_in_table <- function(response) {
  range <- httr2::resp_header(response, "Content-Range") %||% ""
  total <- suppressWarnings(as.numeric(sub(".*/", "", range)))
  if (is.na(total)) 0 else total
}

no_rows <- function(columns) {
  as.data.frame(stats::setNames(rep(list(logical()), length(columns)), columns))
}
