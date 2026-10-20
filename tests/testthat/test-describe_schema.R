# tests/database/describe_schema.R: the script that writes docs/database_tables.md from the
# database's description of itself. Here it is given a small description and the
# text it writes is read.

# The script's functions, without running it.
schema_writer <- function() {
  writer <- new.env(parent = globalenv())
  withr::with_dir(app_file(), sys.source("tests/database/describe_schema.R", envir = writer))
  use_test_database() # the script reads .env, which names the real database
  writer
}

small_description <- function() {
  column <- function(format, description = NULL, ...) c(list(format = format, type = "string", description = description), list(...))
  list(
    swagger = "2.0",
    definitions = list(
      registry = list(
        description = "Outermost container.",
        required = list("registry_id", "name"),
        properties = list(
          registry_id = column("bigint", "Note:\nThis is a Primary Key.<pk/>"),
          name = column("text", "What it is called.\n\nNote:\nsomething PostgREST adds"),
          participant_count = column("integer")
        )
      ),
      study = list(
        description = "One research project | run under a registry.",
        required = list("study_id", "registry_id"),
        properties = list(
          study_id = column("bigint", "Note:\nThis is a Primary Key.<pk/>"),
          registry_id = column(
            "bigint",
            "Note:\nThis is a Foreign Key to `registry.registry_id`.<fk table='registry' column='registry_id'/>"
          ),
          status = column("text", default = "Ongoing")
        )
      ),
      question_category = list(
        description = "Links questions to categories.",
        required = list("component_id", "mesh_id"),
        properties = list(
          component_id = column("bigint", "Note:\nThis is a Primary Key.<pk/>"),
          mesh_id = column("text", "Note:\nThis is a Primary Key.<pk/>")
        )
      ),
      new_table = list(description = "A table the groups don't know yet.", properties = list(x = column("text")))
    ),
    paths = list(
      "/registry" = list(),
      "/rpc/upvote_questionnaire" = list(post = list(parameters = list(
        list(`in` = "body", name = "args", schema = list(properties = list(p_questionnaire_id = list(format = "bigint", type = "integer"))))
      ))),
      "/rpc/rls_auto_enable" = list(post = list(parameters = list(list(`in` = "body", name = "args", schema = list(type = "object")))))
    )
  )
}

# Every reply below is made up (httr2::local_mocked_responses()), so no database is asked: the address
# only has to look like one.
mocked_connection <- list(url = "https://abcdefghijklmnopqrst.supabase.co", key = "sb_secret_x")

# The text the script writes for `small_description()`. (It warns about every table in its groups
# that the description lacks, which most of them are here; one test below is about that.)
written_text <- function(env = parent.frame()) {
  writer <- schema_writer()
  httr2::local_mocked_responses(function(req) httr2::response_json(body = small_description()), env = env)
  suppressWarnings(writer$describe(mocked_connection))
}

test_that("each table is a section with its description and a row for every column", {
  text <- written_text()
  expect_true("### `registry`" %in% text)
  expect_true("Outermost container." %in% text)
  expect_true("| Column | Type | Required? | Default | Notes |" %in% text)
  expect_false(any(grepl("Needed to add a row", text, fixed = TRUE)))
  expect_true("| `name` | text | yes |  | What it is called. |" %in% text) # PostgREST's own note is left out
  expect_true("| `participant_count` | integer |  |  |  |" %in% text) # can be left empty
  expect_match(text[2], "")
  expect_match(text[3], "It lists 4 tables")
})

test_that("keys are said to be numbered by the database, or to point at the table they come from", {
  text <- written_text()
  expect_true("| `registry_id` | bigint |  |  | Primary key, numbered by the database. |" %in% text)
  expect_true("| `study_id` | bigint |  |  | Primary key, numbered by the database. |" %in% text)
  expect_true("| `registry_id` | bigint | yes |  | Points at `registry.registry_id`. |" %in% text)
  # a table keyed by two columns has none numbered by the database: both are needed
  expect_true("| `component_id` | bigint | yes |  | Primary key. |" %in% text)
  expect_true("| `mesh_id` | text | yes |  | Primary key. |" %in% text)
})

test_that("defaults are shown, and a pipe in a description doesn't break the table", {
  text <- written_text()
  expect_true("| `status` | text |  | `Ongoing` |  |" %in% text)
  expect_true("One research project \\| run under a registry." %in% text)
})

test_that("tables come in their groups, and one that no group knows is listed at the end", {
  text <- written_text()
  headings <- unname(grep("^## ", text, value = TRUE))
  expect_equal(headings, c(
    "## Registries and studies", "## Classifying questions", "## Other tables (not in any group yet)", "## Functions"
  ))
  expect_true("### `new_table`" %in% text)
})

test_that("the functions the database offers are listed with their arguments", {
  text <- written_text()
  expect_true("| `rls_auto_enable` | none |" %in% text)
  expect_true("| `upvote_questionnaire` | `p_questionnaire_id` (bigint) |" %in% text)
})

test_that("a table the groups expect and the database doesn't have is warned about", {
  writer <- schema_writer()
  httr2::local_mocked_responses(function(req) httr2::response_json(body = small_description()))
  expect_warning(writer$describe(mocked_connection), "Not in the database: principal_investigator, respondent_type")
})

# ------ With the database's catalog (SUPABASE_ACCESS_TOKEN) ----------------------------------
small_catalog <- function() {
  row <- function(...) list(...)
  list(
    constraints = list(
      row(tbl = "registry", conname = "registry_pkey", contype = "p", def = "PRIMARY KEY (registry_id)"),
      row(tbl = "registry", conname = "registry_participant_count_check", contype = "c", def = "CHECK ((participant_count >= 0))"),
      row(tbl = "registry", conname = "registry_name_unique", contype = "u", def = "UNIQUE (name)"),
      row(tbl = "study", conname = "study_registry_fk", contype = "f", def = "FOREIGN KEY (registry_id) REFERENCES registry(registry_id)"),
      row(
        tbl = "question_category", conname = "question_category_fk", contype = "f",
        def = "FOREIGN KEY (component_id, mesh_id) REFERENCES question(component_id, mesh_id)"
      )
    ),
    indexes = list(
      row(
        tbl = "registry",
        indexname = "registry_pkey",
        indexdef = "CREATE UNIQUE INDEX registry_pkey ON public.registry USING btree (registry_id)"
      ),
      row(
        tbl = "registry",
        indexname = "registry_name_unique",
        indexdef = "CREATE UNIQUE INDEX registry_name_unique ON public.registry USING btree (name)"
      ),
      row(
        tbl = "registry",
        indexname = "registry_lower_name",
        indexdef = "CREATE UNIQUE INDEX registry_lower_name ON public.registry USING btree (lower(name))"
      ),
      row(
        tbl = "study",
        indexname = "study_registry_id_idx",
        indexdef = "CREATE INDEX study_registry_id_idx ON public.study USING btree (registry_id)"
      )
    ),
    triggers = list(row(
      tbl = "study", tgname = "study_guard",
      def = "CREATE TRIGGER study_guard BEFORE UPDATE OF registry_id ON public.study FOR EACH ROW EXECUTE FUNCTION study_guard()"
    )),
    functions = list(
      row(proname = "study_guard", args = "", result = "trigger", security_definer = FALSE),
      row(proname = "upvote_questionnaire", args = "p_questionnaire_id bigint", result = "integer", security_definer = TRUE)
    ),
    policies = list(row(
      tbl = "registry",
      policyname = "Dashboard can read",
      cmd = "SELECT",
      roles = "{anon,authenticated}",
      qual = "true"
    )),
    rls = list(row(tbl = "registry", rls_on = TRUE), row(tbl = "study", rls_on = FALSE)),
    columns = list(
      row(tbl = "registry", column_name = "registry_id", identity_generation = "BY DEFAULT"),
      row(tbl = "study", column_name = "study_id", identity_generation = "ALWAYS")
    ),
    event_triggers = list(row(name = "ensure_rls", event = "ddl_command_end", fn = "rls_auto_enable"))
  )
}

written_with_catalog <- function(env = parent.frame()) {
  writer <- schema_writer()
  httr2::local_mocked_responses(function(req) httr2::response_json(body = small_description()), env = env)
  suppressWarnings(writer$describe(mocked_connection, small_catalog()))
}

test_that("with the catalog, a table's checks, unique constraints and other indexes are listed under it", {
  text <- written_with_catalog()
  registry <- text[seq(match("### `registry`", text), match("### `study`", text) - 1)]
  expect_true("**Checks and unique constraints**" %in% registry)
  expect_true("- `registry_participant_count_check`: `CHECK ((participant_count >= 0))`" %in% registry)
  expect_true("- `registry_name_unique`: `UNIQUE (name)`" %in% registry)
  expect_false(any(grepl("registry_pkey", registry))) # the primary key is in the column table
  expect_true("- `registry_lower_name`: unique `(lower(name))`" %in% registry) # an index no constraint made
  expect_false(any(grepl("`registry_name_unique`: unique", registry, fixed = TRUE))) # the unique constraint's own index
})

test_that("with the catalog, foreign keys over several columns and triggers are listed, and one-column ones are not", {
  text <- written_with_catalog()
  expect_true("- `FOREIGN KEY (component_id, mesh_id) REFERENCES question(component_id, mesh_id)`" %in% text)
  expect_false(any(grepl("study_registry_fk", text, fixed = TRUE)))
  expect_false(any(grepl("FOREIGN KEY (registry_id)", text, fixed = TRUE)))
  expect_true(
    "- `study_guard`: `BEFORE UPDATE OF registry_id FOR EACH ROW EXECUTE FUNCTION study_guard()`" %in% text
  )
  expect_true("- `study_registry_id_idx`: `(registry_id)`" %in% text)
})

test_that("with the catalog, the keys the database numbers are known, and whether a value may be given instead", {
  text <- written_with_catalog()
  expect_true("| `registry_id` | bigint |  |  | Primary key, numbered by the database unless a value is given. |" %in% text)
  expect_true("| `study_id` | bigint |  |  | Primary key, numbered by the database. |" %in% text)
})

test_that("with the catalog, the functions and Row Level Security are described from it", {
  text <- written_with_catalog()
  expect_true("| `study_guard` | none | `trigger` | Runs from a trigger; not called by the site. |" %in% text)
  expect_true("| `upvote_questionnaire` | `p_questionnaire_id bigint` | `integer` | Runs with its owner's rights. |" %in% text)
  expect_true("Row Level Security is **off** for: `study`." %in% text)
  expect_match(text[grepl("event trigger", text)], "`ensure_rls` runs `rls_auto_enable\\(\\)`")
  expect_true("| `registry` | Dashboard can read | SELECT | {anon,authenticated} | `true` |" %in% text)
})

test_that("without the catalog the file says what it leaves out and how to include it", {
  text <- written_text()
  expect_match(paste(text, collapse = " "), "run the script with `SUPABASE_ACCESS_TOKEN` set to include them")
  expect_false("## Row Level Security" %in% text)
  expect_false(any(grepl("Checks and unique constraints", text)))
  expect_true("## Row Level Security" %in% written_with_catalog())
  expect_false(any(grepl("run the script with", written_with_catalog(), fixed = TRUE)))
})

test_that("the catalog is read by SELECTs only, with the token in the Authorization header and redacted", {
  writer <- schema_writer()
  sent <- list()
  httr2::local_mocked_responses(function(req) {
    sent[[length(sent) + 1]] <<- req
    httr2::response_json(body = list())
  })
  expect_equal(writer$project_reference("https://abcdefghijklmnopqrst.supabase.co"), "abcdefghijklmnopqrst")
  catalog <- writer$read_catalog("abcdefghijklmnopqrst", "sbp_TEST_TOKEN")
  expect_named(catalog, names(writer$catalog_queries))
  expect_length(sent, length(writer$catalog_queries))
  for (request in sent) {
    expect_equal(request$url, "https://api.supabase.com/v1/projects/abcdefghijklmnopqrst/database/query")
    expect_equal(httr2::req_get_headers(request, "reveal")$Authorization, "Bearer sbp_TEST_TOKEN")
    expect_false(grepl("sbp_TEST_TOKEN", paste(capture.output(print(request)), collapse = " ")))
    expect_match(tolower(request$body$data$query), "^select ")
    expect_false(grepl("\\b(insert|update|delete|drop|alter|create|truncate|grant)\\b", tolower(request$body$data$query)))
  }
})
