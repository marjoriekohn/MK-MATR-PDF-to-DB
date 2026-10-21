# ------ Files kept in Supabase Storage ---------------------------------------------
# A copy of the site keeps its files on the disk of the server it runs on, and a hosted site loses
# them when it restarts or is deployed again, and doesn't share them between its servers. So the files
# a draft needs later (its PDF) are also kept in a private bucket in the project's Storage, which the
# site's server reads and writes with the secret key (the same `connection` as the database, in the
# apikey header). Nobody else can read the bucket: it isn't public, and no policy lets anyone in.
# The bucket is made by docs/migrations/20261021_pdf_storage.sql.
#
#   curl "$SUPABASE_URL/storage/v1/object/extraction-pdfs/<sha256>.pdf" -H "apikey: $SUPABASE_SECRET_KEY"

# The bucket that keeps the PDFs, each named by its SHA-256.
storage_pdf_bucket <- "extraction-pdfs"

storage_request <- function(connection, bucket, name) {
  if (!nzchar(connection$url %||% "") || !nzchar(connection$key %||% "")) {
    stop("The site doesn't know where the MATR database is, so it can't keep files there.", call. = FALSE)
  }
  httr2::request(connection$url) |>
    httr2::req_url_path_append("storage", "v1", "object", bucket, name) |>
    httr2::req_headers(apikey = connection$key, .redact = "apikey") |>
    httr2::req_timeout(storage_timeout_seconds) |>
    httr2::req_retry(max_tries = database_max_tries) |>
    httr2::req_error(is_error = function(response) FALSE)
}

# A PDF can be tens of megabytes, so it has longer than a database request.
storage_timeout_seconds <- 120

#' Keep a file in a bucket, replacing one of the same name.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param bucket The bucket.
#' @param name The file's name in the bucket.
#' @param path The file on this server.
#' @param type Its media type.
#' @return Called for its effect. An error with a sentence if Storage refused.
storage_model_upload <- function(connection, bucket, name, path, type = "application/pdf") {
  response <- storage_request(connection, bucket, name) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`x-upsert` = "true") |>
    httr2::req_body_file(path, type) |>
    storage_perform()
  if (httr2::resp_status(response) >= 400) stop(storage_problem(response, bucket), call. = FALSE)
  invisible()
}

#' Fetch a file from a bucket.
#'
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param bucket The bucket.
#' @param name The file's name in the bucket.
#' @param path Where to put it on this server.
#' @return `TRUE` if it was there and is now at `path`, `FALSE` if the bucket has no such file. An
#'   error with a sentence if Storage couldn't be asked.
storage_model_download <- function(connection, bucket, name, path) {
  fetching <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(fetching))
  response <- storage_request(connection, bucket, name) |>
    httr2::req_perform(path = fetching) |>
    tryCatch(error = function(problem) {
      stop("Couldn't reach the MATR database's file storage: ", conditionMessage(problem), call. = FALSE)
    })
  if (storage_is_missing(response)) {
    return(FALSE)
  }
  if (httr2::resp_status(response) >= 400) stop(storage_problem(response, bucket), call. = FALSE)
  file.rename(fetching, path) # whole, or not at all: a page asking at the same moment never finds half a file
}

storage_perform <- function(request) {
  tryCatch(httr2::req_perform(request), error = function(problem) {
    stop("Couldn't reach the MATR database's file storage: ", conditionMessage(problem), call. = FALSE)
  })
}

# Storage says a file isn't there with a 400 whose body says 404.
storage_is_missing <- function(response) {
  status <- httr2::resp_status(response)
  if (status == 404) {
    return(TRUE)
  }
  if (status != 400) {
    return(FALSE)
  }
  body <- tryCatch(jsonlite::fromJSON(rawToChar(httr2::resp_body_raw(response))), error = function(problem) list())
  identical(as.character(body$statusCode), "404") && !identical(body$code, "NoSuchBucket")
}

storage_problem <- function(response, bucket) {
  body <- tryCatch(httr2::resp_body_json(response), error = function(problem) list())
  if (identical(body$code, "NoSuchBucket")) {
    return(sprintf(
      "The file storage has no bucket called %s yet, so the PDF can't be kept. Run docs/migrations/20261021_pdf_storage.sql.",
      bucket
    ))
  }
  sprintf("The MATR database's file storage had a problem (%d): %s", httr2::resp_status(response), body$message %||% "no reason given")
}
