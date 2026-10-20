# Writes docs/database_tables.md from the live database: every table with its
# columns, types, defaults and keys, and, if it can, the rules the database
# enforces on them. Run it from the project folder after a migration:
#   Rscript tests/database/describe_schema.R
#
# Two sources, and what each can say:
# - The REST API's description of itself: columns, types, defaults,
# which columns are needed to add a row, and single-column keys.
# - The database's catalog, if SUPABASE_ACCESS_TOKEN is set: check and unique
# constraints, foreign keys over several columns, indexes, triggers, functions,
# Row Level Security and its policies. It is read through the Management API,
# which runs SQL; the queries below are all SELECTs. Without the token those
# parts are left out.

if (file.exists(".env")) readRenviron(".env")
source("R/model_database.R", local = TRUE)

output_file <- "docs/database_tables.md"

# The tables in the order a reader meets them, in groups.
groups <- list(
  "Registries and studies" = c("registry", "principal_investigator", "study"),
  "Questionnaires and waves" = c("respondent_type", "questionnaire", "wave"),
  "What is printed" = c(
    "component_type", "component", "question", "questionnaire_component",
    "questionnaire_question", "wave_question"
  ),
  "Answers" = c("variable", "open_ended", "closed_ended", "answer_option", "option_set", "answer_option_set", "response_option", "subject"),
  "Classifying questions" = c("category", "question_category", "measurement_scale", "question_scale"),
  "Staff, drafts and applied changes (not shown to the public dashboard)" = c(
    "staff", "draft", "draft_review_event", "edit_draft", "extracted_draft", "ai_extracted_draft",
    "pdf_file", "applied_item"
  )
)

# ------ Turning the description into text -------------------------------------
# A cell of a Markdown table: one line, with no pipes to break the row.
cell <- function(text) {
  text <- gsub("\\s+", " ", trimws(text %||% ""))
  gsub("|", "\\|", text, fixed = TRUE)
}

# What the description says about a column or "".
description_of <- function(column) {
  text <- column$description
  if (is.character(text) && length(text) == 1) text else ""
}

# The comment a column was given, without PostgREST's note about its keys.
column_comment <- function(column) sub("\\s*Note:.*$", "", description_of(column))

is_key <- function(column) grepl("<pk/>", description_of(column), fixed = TRUE)
points_at <- function(column) grepl("<fk ", description_of(column), fixed = TRUE)

# A table's own number for its rows: its only key column, a bigint, not also
# pointing at another table. The database gives these a random 9-digit number.
is_numbered_by_database <- function(column, columns) {
  keys <- Filter(is_key, columns)
  length(keys) == 1 && is_key(column) && !points_at(column) && identical(column$format, "bigint")
}

column_notes <- function(column, columns) {
  description <- description_of(column)
  notes <- character()
  if (is_key(column)) {
    notes <- c(
      notes,
      if (is_numbered_by_database(column, columns)) "Primary key, numbered by the database." else "Primary key."
    )
  }
  target <- regmatches(description, regexec("<fk table='([^']+)' column='([^']+)'/>", description))[[1]]
  if (length(target) == 3) notes <- c(notes, sprintf("Points at `%s.%s`.", target[2], target[3]))
  comment <- column_comment(column)
  if (nzchar(trimws(comment))) notes <- c(notes, comment)
  paste(notes, collapse = " ")
}

column_default <- function(column) {
  default <- column$default
  if (is.null(default)) {
    return("")
  }
  paste0("`", if (is.logical(default)) tolower(as.character(default)) else as.character(default), "`")
}

table_text <- function(name, definition, catalog = NULL) {
  columns <- definition$properties
  required <- unlist(definition$required)

  # With the catalog, a key the database numbers is known, not guessed, and so is whether a value may be given.
  numbered <- if (is.null(catalog)) NULL else vapply(identity_of(catalog, name), function(row) row$column_name, character(1))
  by_default <- if (is.null(catalog)) {
    character()
  } else {
    vapply(
      Filter(function(row) identical(row$identity_generation, "BY DEFAULT"), identity_of(catalog, name)),
      function(row) row$column_name,
      character(1)
    )
  }
  random <- if (is.null(catalog)) {
    character()
  } else {
    vapply(
      Filter(function(row) identical(row$identity_generation, "RANDOM"), identity_of(catalog, name)),
      function(row) row$column_name,
      character(1)
    )
  }
  numbered_by_database <- function(column, spec) if (is.null(numbered)) is_numbered_by_database(spec, columns) else column %in% numbered
  rows <- vapply(names(columns), function(column) {
    spec <- columns[[column]]
    notes <- column_notes(spec, columns)
    if (column %in% by_default) notes <- sub("numbered by the database\\.", "numbered by the database unless a value is given.", notes)
    if (column %in% random) notes <- sub("numbered by the database\\.", "a random 9-digit number the database picks.", notes)
    sprintf(
      "| `%s` | %s | %s | %s | %s |", column, cell(spec$format %||% spec$type),
      if (column %in% required && !numbered_by_database(column, spec)) "yes" else "",
      cell(column_default(spec)), cell(notes)
    )
  }, character(1))
  c(
    sprintf("### `%s`", name), "",
    cell(definition$description %||% "(no description)"), "",
    "| Column | Type | Required? | Default | Notes |", "| --- | --- | --- | --- | --- |", rows, "",
    if (!is.null(catalog)) rules_text(name, catalog)
  )
}

# The functions the database offers, from the `/rpc/` paths.
function_text <- function(paths) {
  functions <- grep("^/rpc/", names(paths), value = TRUE)
  if (length(functions) == 0) {
    return(character())
  }
  lines <- vapply(functions, function(path) {
    body <- Filter(function(parameter) identical(parameter$name, "args"), paths[[path]]$post$parameters)[[1]]
    arguments <- body$schema$properties
    shown <- if (length(arguments) == 0) {
      "none"
    } else {
      paste(
        vapply(names(arguments), function(a) sprintf("`%s` (%s)", a, arguments[[a]]$format %||% arguments[[a]]$type), character(1)),
        collapse = ", "
      )
    }
    sprintf("| `%s` | %s |", sub("^/rpc/", "", path), shown)
  }, character(1))
  c("## Functions", "", "Called as `POST /rest/v1/rpc/<name>`.", "", "| Function | Arguments |", "| --- | --- |", sort(lines), "")
}

# ------ What only the database's catalog knows -----------------------------------------------
catalog_queries <- list(
  constraints = paste(
    "select c.conrelid::regclass::text as tbl, c.conname, c.contype, pg_get_constraintdef(c.oid) as def",
    "from pg_constraint c where c.connamespace = 'public'::regnamespace order by 1, 3, 2"
  ),
  indexes = "select tablename as tbl, indexname, indexdef from pg_indexes where schemaname = 'public' order by 1, 2",
  triggers = paste(
    "select t.tgrelid::regclass::text as tbl, t.tgname, pg_get_triggerdef(t.oid) as def from pg_trigger t",
    "where not t.tgisinternal and t.tgrelid in (select c.oid from pg_class c where c.relnamespace = 'public'::regnamespace)",
    "order by 1, 2"
  ),
  functions = paste(
    "select p.proname, pg_get_function_arguments(p.oid) as args, pg_get_function_result(p.oid) as result,",
    "p.prosecdef as security_definer from pg_proc p where p.pronamespace = 'public'::regnamespace order by 1"
  ),
  policies = paste(
    "select tablename as tbl, policyname, cmd, roles::text as roles, qual from pg_policies",
    "where schemaname = 'public' order by 1, 2"
  ),
  rls = paste(
    "select c.relname as tbl, c.relrowsecurity as rls_on from pg_class c",
    "where c.relnamespace = 'public'::regnamespace and c.relkind = 'r' order by 1"
  ),
  columns = paste(
    "select table_name as tbl, column_name,",
    "case when is_identity = 'YES' then identity_generation else 'RANDOM' end as identity_generation",
    "from information_schema.columns where table_schema = 'public'",
    "and (is_identity = 'YES' or column_default like '%random_id(%') order by 1, 2"
  ),
  event_triggers = "select evtname as name, evtevent as event, evtfoid::regproc::text as fn from pg_event_trigger order by 1"
)

# The project's reference: the first part of its address.
project_reference <- function(url) sub("^https://([^.]+)\\..*$", "\\1", url)

# Every query's rows, as lists. The token goes in the Authorization header only.
read_catalog <- function(project, token) {
  lapply(catalog_queries, function(sql) {
    response <- httr2::request("https://api.supabase.com/v1/projects") |>
      httr2::req_url_path_append(project, "database", "query") |>
      httr2::req_headers(Authorization = paste("Bearer", token), .redact = "Authorization") |>
      httr2::req_body_json(list(query = sql)) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()
    httr2::resp_body_json(response, simplifyVector = FALSE)
  })
}

# The catalog's rows for one table, or all of them with `name = NULL`.
rows_of <- function(catalog, kind, name = NULL) {
  Filter(function(row) is.null(name) || identical(row$tbl, name), catalog[[kind]])
}

# A definition on one line, in backticks, without the table it is on.
code <- function(text) paste0("`", gsub("`", "'", gsub("\\s+", " ", text)), "`")

# What the catalog says about one table's rules: checks and unique constraints,
# foreign keys over several columns, triggers and indexes. Primary keys and
# one-column foreign keys are in the column table already.
rules_text <- function(name, catalog) {
  constraints <- rows_of(catalog, "constraints", name)
  of_type <- function(type) Filter(function(row) identical(row$contype, type), constraints)
  checks <- c(of_type("c"), of_type("u"))
  several <- Filter(function(row) grepl(",", sub("REFERENCES.*$", "", row$def)), of_type("f"))
  triggers <- rows_of(catalog, "triggers", name)
  own_indexes <- vapply(constraints, function(row) row$conname, character(1))
  indexes <- Filter(function(row) !row$indexname %in% own_indexes, rows_of(catalog, "indexes", name))
  section <- function(title, lines) if (length(lines) > 0) c(sprintf("**%s**", title), "", lines, "")
  c(
    section("Checks and unique constraints", vapply(checks, function(row) sprintf("- `%s`: %s", row$conname, code(row$def)), character(1))),
    section("Foreign keys over several columns", vapply(several, function(row) sprintf("- %s", code(row$def)), character(1))),
    section("Triggers", vapply(triggers, function(row) {
      sprintf(
        "- `%s`: %s",
        row$tgname,
        code(sub(sprintf(" ON public\\.%s ", name), " ", sub("^CREATE (CONSTRAINT )?TRIGGER \\S+ ", "", row$def)))
      )
    }, character(1))),
    section("Other indexes", vapply(indexes, function(row) {
      sprintf(
        "- `%s`: %s%s",
        row$indexname,
        if (grepl("UNIQUE INDEX", row$indexdef)) "unique " else "",
        code(sub("^.* USING \\w+ ", "", row$indexdef))
      )
    }, character(1)))
  )
}

# The tables' columns the database numbers itself, and whether a value may be
# given instead.
identity_of <- function(catalog, name) {
  Filter(function(row) identical(row$tbl, name), catalog$columns)
}

functions_text <- function(catalog) {
  rows <- vapply(catalog$functions, function(row) {
    notes <- c(
      if (row$result %in% c("trigger", "event_trigger")) "Runs from a trigger; not called by the site.",
      if (isTRUE(row$security_definer)) "Runs with its owner's rights."
    )
    sprintf(
      "| `%s` | %s | %s | %s |", row$proname, if (nzchar(row$args)) code(row$args) else "none", code(row$result), paste(
        notes,
        collapse = " "
      )
    )
  }, character(1))
  c("## Functions", "", "| Function | Arguments | Returns | Notes |", "| --- | --- | --- | --- |", rows, "")
}

security_text <- function(catalog) {
  tables <- catalog$rls
  off <- vapply(Filter(function(row) !isTRUE(row$rls_on), tables), function(row) row$tbl, character(1))
  policies <- vapply(catalog$policies, function(row) {
    sprintf("| `%s` | %s | %s | %s | %s |", row$tbl, cell(row$policyname), row$cmd, cell(row$roles), code(row$qual %||% ""))
  }, character(1))
  events <- Filter(function(row) identical(row$fn, "rls_auto_enable"), catalog$event_triggers)
  c(
    "## Row Level Security", "",
    if (length(off) == 0) {
      sprintf("Row Level Security is on for all %d tables.", length(tables))
    } else {
      sprintf("Row Level Security is **off** for: %s.", paste0("`", off, "`", collapse = ", "))
    },
    if (length(events) > 0) {
      sprintf(
        "The event trigger `%s` runs `rls_auto_enable()` after each DDL command and turns it on for every new table in `public`.",
        events[[1]]$name
      )
    },
    "",
    if (length(policies) > 0) {
      c("| Table | Policy | Command | Roles | Using |", "| --- | --- | --- | --- | --- |", policies, "")
    } else {
      "There are no policies."
    }
  )
}

# ------ The document ---------------------------------------------------------
describe <- function(connection, catalog = NULL, source = "the live database's own description of itself") {
  description <- database_model_describe(connection)
  definitions <- description$definitions
  unplaced <- setdiff(names(definitions), unlist(groups))
  missing <- setdiff(unlist(groups), names(definitions))
  if (length(missing) > 0) warning("Not in the database: ", paste(missing, collapse = ", "), call. = FALSE)
  groups <- if (length(unplaced) > 0) c(groups, list("Other tables (not in any group yet)" = unplaced)) else groups

  body <- unlist(lapply(names(groups), function(group) {
    present <- intersect(groups[[group]], names(definitions))
    if (length(present) == 0) {
      return(NULL)
    }
    c(sprintf("## %s", group), "", unlist(lapply(present, function(name) table_text(name, definitions[[name]], catalog))))
  }))
  c(
    "# The MATR database: tables and columns", "",
    sprintf(
      paste0(
        "_Generated by `Rscript tests/database/describe_schema.R` on %s from %s. ",
        "It lists %d tables. Don't edit it by hand: run the script again after a migration._"
      ),
      format(Sys.Date()), source, length(definitions)
    ), "",
    if (is.null(catalog)) {
      paste(
        "This is what the database's REST description says about its tables. It doesn't say anything about check or unique",
        "constraints, foreign keys over more than one column, indexes, triggers or policies, so those aren't here:",
        "run the script with `SUPABASE_ACCESS_TOKEN` set to include them."
      )
    } else {
      "This is what the database says about its tables, from its REST description and its catalog."
    },
    "",
    paste(
      "[database.md](database.md) explains the model. \"Required?\" is \"yes\" for a column with no",
      "default that can't be left empty; a column that isn't marked can be left out when a row is added."
    ), "",
    body,
    if (is.null(catalog)) function_text(description$paths) else c(functions_text(catalog), security_text(catalog))
  )
}

if (sys.nframe() == 0) {
  connection <- list(url = Sys.getenv("SUPABASE_URL"), key = Sys.getenv("SUPABASE_SECRET_KEY"))
  token <- Sys.getenv("SUPABASE_ACCESS_TOKEN")
  catalog <- if (nzchar(token)) read_catalog(project_reference(connection$url), token)
  if (is.null(catalog)) message("SUPABASE_ACCESS_TOKEN isn't set, so constraints, triggers, indexes and policies are left out.")
  writeLines(describe(connection, catalog), output_file, useBytes = FALSE)
  cat("Wrote", output_file, "\n")
}
