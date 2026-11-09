# Checks that the database lets each key do what sql/5_permissions.sql means it to, without changing or
# printing any data. From the project folder:
#   Rscript tests/database/check_access.R
#
# It reads SUPABASE_URL, SUPABASE_SECRET_KEY and SUPABASE_PUBLISHABLE_KEY from .env and prints one line per
# table and function:
#
#   secret key       read / update / delete / insert on every table, and every function   what the site needs
#   publishable key  read on the catalog tables, upvote_questionnaire, and nothing else   what the dashboard gets
#
# It exits with an error if
# - the secret key can't do all four on a table that exists, or can't call a function the site calls;
# - the publishable key can read staff, extraction or history (they must stay hidden from the dashboard), can't
#   read a catalog table, reads a pdf_file row no wave names (an extraction's PDF stays unlisted until pushed),
#   can call push_extraction, apply_change or empty_* (they change the catalog), or can't call
#   upvote_questionnaire;
# - the database has a table this script doesn't list, so a new table can't go unchecked.

if (file.exists(".env")) readRenviron(".env")
source("R/model_database.R")

secret <- list(url = Sys.getenv("SUPABASE_URL"), key = Sys.getenv("SUPABASE_SECRET_KEY"))
publishable <- list(url = Sys.getenv("SUPABASE_URL"), key = Sys.getenv("SUPABASE_PUBLISHABLE_KEY"))

# Every table, with a column to read and filter on (a key, so "equals 0 and not 0" parses) and one to aim an
# update at. The dashboard reads the catalog tables and nothing else.
catalog_tables <- list(
  registry = c("registry_id", "name"), principal_investigator = c("principal_investigator_id", "name"),
  study = c("study_id", "name"), respondent_type = c("respondent_type_id", "name"),
  questionnaire = c("questionnaire_id", "name"), wave = c("wave_id", "name"), pdf_file = c("pdf_sha256", "pdf_pages"),
  component = c("component_id", "text"), placement = c("placement_id", "display_order"),
  wave_question = c("wave_id", "printed_number"), variable = c("variable_key", "label"),
  answer_option = c("answer_option_id", "standardized_label"), response_option = c("variable_key", "code_value"),
  tag = c("tag_id", "name"), placement_tag = c("placement_id", "tag_id"), variable_tag = c("variable_key", "tag_id"),
  response_option_tag = c("variable_key", "tag_id"), measurement_scale = c("scale_id", "name"),
  question_scale = c("scale_id", "item_number")
)
private_tables <- list(staff = c("staff_id", "name"), extraction = c("extraction_id", "summary"), history = c("history_id", "summary"))
tables <- c(catalog_tables, private_tables)

# The functions the site calls, with arguments that can't change anything: no such row, or no staff member.
site_functions <- list(
  push_extraction = list(p_extraction_id = 0, p_staff_id = 0, p_dry_run = TRUE),
  apply_change = list(p_staff_id = 0, p_items = list(), p_summary = "check"),
  empty_wave = list(p_wave_id = 0, p_staff_id = 0), empty_questionnaire = list(p_questionnaire_id = 0, p_staff_id = 0),
  empty_study = list(p_study_id = 0, p_staff_id = 0), empty_registry = list(p_registry_id = 0, p_staff_id = 0)
)
dashboard_function <- list(upvote_questionnaire = list(p_questionnaire_id = 0))

# What a request came to: "ok", "denied", "missing" or "refused" (the function ran and said no, so the key may
# call it), else the problem in a few words.
outcome <- function(request) {
  response <- httr2::req_perform(request)
  status <- httr2::resp_status(response)
  if (status < 300) {
    return("ok")
  }
  message <- tryCatch(httr2::resp_body_json(response)$message, error = function(problem) NULL) %||% ""
  if (status %in% c(401, 403) || grepl("permission denied", message)) {
    return("denied")
  }
  if (status == 404 || grepl("Could not find", message)) {
    return("missing")
  }
  if (grepl("Only an active staff member|There is no questionnaire|no longer in the database", message)) {
    return("refused")
  }
  paste("problem:", substr(sub("\n.*", "", message), 1, 60))
}

# A filter no row can pass, so a change or delete never touches anything.
nothing <- function(column) stats::setNames(list("eq.0", "neq.0"), c(column, column))

probe_read <- function(connection, table, column) {
  outcome(database_request(connection, table) |> httr2::req_url_query(select = column, limit = 1))
}
probe_update <- function(connection, table, column, where) {
  outcome(
    database_request(connection, table) |>
      httr2::req_method("PATCH") |>
      database_filter(nothing(where)) |>
      database_body(stats::setNames(list(NULL), column))
  )
}
probe_delete <- function(connection, table, where) {
  outcome(database_request(connection, table) |> httr2::req_method("DELETE") |> database_filter(nothing(where)))
}
probe_insert <- function(connection, table) {
  outcome(database_request(connection, table) |> httr2::req_method("POST") |> httr2::req_body_raw("[]", type = "application/json"))
}
probe_call <- function(connection, name, arguments) {
  outcome(database_request(connection, paste0("rpc/", name)) |> httr2::req_method("POST") |> database_body(arguments))
}

cat(sprintf("%-24s %-6s %-8s %-8s %-8s | %s\n", "table", "read", "update", "delete", "insert", "publishable read"))
problems <- character()
for (table in names(tables)) {
  columns <- tables[[table]]
  results <- c(
    read = probe_read(secret, table, columns[1]),
    update = probe_update(secret, table, columns[2], where = columns[1]),
    delete = probe_delete(secret, table, where = columns[1]),
    insert = probe_insert(secret, table)
  )
  dashboard <- probe_read(publishable, table, columns[1])
  cat(sprintf("%-24s %-6s %-8s %-8s %-8s | %s\n", table, results[["read"]], results[["update"]], results[["delete"]], results[["insert"]], dashboard))
  if ("missing" %in% results) next
  if (any(results != "ok")) problems <- c(problems, paste("The secret key can't do everything on", table))
  if (table %in% names(private_tables) && identical(dashboard, "ok")) problems <- c(problems, paste("The publishable key can read, and must not:", table))
  if (table %in% names(catalog_tables) && !identical(dashboard, "ok")) problems <- c(problems, paste("The publishable key can't read, and the dashboard needs to:", table))
}

# The dashboard sees only the PDFs waves were extracted from: what it reads from pdf_file is what the waves name.
listed <- function(connection) {
  rows <- database_rows(database_perform(database_request(connection, "pdf_file") |> httr2::req_url_query(select = "pdf_sha256")))
  sort(vapply(rows, function(row) row$pdf_sha256, character(1)))
}
waves <- database_rows(database_perform(database_request(secret, "wave") |> httr2::req_url_query(select = "pdf_sha256,supplement_sha256")))
named <- sort(unique(unlist(lapply(waves, function(row) c(row$pdf_sha256, row$supplement_sha256)))))
if (!identical(listed(publishable), as.character(named))) {
  problems <- c(problems, "The publishable key reads pdf_file rows no wave names, or misses some a wave names")
}

cat("\n", sprintf("%-24s %-8s | %s\n", "function", "secret", "publishable"), sep = "")
for (name in names(site_functions)) {
  site <- probe_call(secret, name, site_functions[[name]])
  dashboard <- probe_call(publishable, name, site_functions[[name]])
  cat(sprintf("%-24s %-8s | %s\n", name, site, dashboard))
  if (!site %in% c("ok", "refused")) problems <- c(problems, paste("The secret key can't call", name))
  if (dashboard != "denied") problems <- c(problems, paste("The publishable key can call, and must not:", name))
}
for (name in names(dashboard_function)) {
  site <- probe_call(secret, name, dashboard_function[[name]])
  dashboard <- probe_call(publishable, name, dashboard_function[[name]])
  cat(sprintf("%-24s %-8s | %s\n", name, site, dashboard))
  if (!dashboard %in% c("ok", "refused")) problems <- c(problems, paste("The publishable key can't call, and the dashboard needs to:", name))
}

# A table the database has and this script doesn't know can't be checked, so say so.
unlisted <- setdiff(names(database_model_describe(secret)$definitions), names(tables))
if (length(unlisted) > 0) problems <- c(problems, paste("Tables in the database that this script doesn't check yet:", paste(unlisted, collapse = ", ")))

if (length(problems) > 0) {
  cat("\n", paste(problems, collapse = "\n"), "\n", sep = "")
  quit(status = 1)
}
cat(
  "\nThe secret key can read, update, delete and insert on every table, and call every function the site calls.",
  "\nThe publishable key reads the catalog and a wave's PDFs, upvotes, and nothing else.\n",
  sep = ""
)
