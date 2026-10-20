# ------ Sample data for the tests of pure functions ---------------------------------------------------
# The hand-checked GNA extraction, the lookup lists and a small sample of the live tables
# (tests/testthat/fixtures/). The functions that work on tables (the Make an edit forms, what a delete
# reaches, the draft checks) take them as data frames, so their tests need no database; anything
# that is about the database is tested against a real one (helper-00_realdb.R).
#
# The sample tables, in brief:
#   AHP (questionnaire 1, waves 1-2): section A (component 1) holds an
#     instruction, three questions (3, 4, 5) and section B (6) holds 7.
#     Question 5 is a follow-up to question 4.
#   KQ (2, one wave) and KQP (3, one wave) share section 10 and question 9.
#   MQ (4, one wave) shares question 4 ("Do you now smoke cigarettes?") with AHP.
#   Component 15 is printed nowhere.

fixture_path <- function(...) test_path("fixtures", ...)

# Who is using the site, for a test of a pure function that only needs a name and a role to record.
# (A test that touches the database makes a real staff member with local_staff().)
fixture_staff <- list(staff_id = 1L, name = "Maggie Kohn", role = "STAFF")
fixture_other_staff <- list(staff_id = 2L, name = "Alex Rivera", role = "STAFF")

app_file <- function(...) test_path("..", "..", ...)

# A table of the fixtures as the model reads it from the database: ids and numbers as numbers, flags as
# logical, the rest as text, in the order the model asks for (see `table_model_get()`). One that isn't one
# of the edit pages' tables is a plain read of the CSV.
# The files don't change while the tests run, so each is read once and kept (R copies a value a test changes,
# so a test can't change what the next one is given).
fixture_cache <- new.env()
cached_fixture <- function(name, read) {
  if (!exists(name, envir = fixture_cache, inherits = FALSE)) assign(name, read(), envir = fixture_cache)
  get(name, envir = fixture_cache, inherits = FALSE)
}

fixture_table <- function(table) cached_fixture(paste0("table:", table), function() read_fixture_table(table))

read_fixture_table <- function(table) {
  rows <- utils::read.csv(fixture_path("tables", paste0(table, ".csv")), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  spec <- database_tables[[table]]
  if (is.null(spec)) {
    return(rows)
  }
  for (column in names(spec$columns)) {
    rows[[column]] <- switch(spec$columns[[column]],
      id = ,
      number = as.numeric(rows[[column]]),
      flag = as.logical(rows[[column]]),
      as.character(rows[[column]])
    )
  }
  rows <- rows[names(spec$columns)]
  rows[do.call(order, c(unname(as.list(rows[spec$order_by])), list(method = "radix"))), , drop = FALSE]
}

# The sample tables the edit pages read, by name. `tables` can be fewer; `change` is a function that
# edits the list of tables before it's returned, for a test that needs a different database.
test_snapshot <- function(tables = names(database_tables), change = identity) {
  change(stats::setNames(lapply(tables, fixture_table), tables))
}

# The catalog the extraction pages pick from, made of the sample tables, in the orders the models read them.
test_catalog <- function() {
  by_name <- function(rows) rows[order(rows$name, method = "radix"), , drop = FALSE]
  questionnaire_service_catalog(list(
    registries = by_name(fixture_table("registry")[c("registry_id", "name", "abbreviation")]),
    studies = by_name(fixture_table("study")[c("study_id", "registry_id", "principal_investigator_id", "name")]),
    investigators = fixture_table("principal_investigator")[c("principal_investigator_id", "name")],
    questionnaires = fixture_table("questionnaire")[c("questionnaire_id", "study_id", "respondent_type_id", "name", "abbreviation")],
    waves = fixture_table("wave")[c("wave_id", "questionnaire_id", "name", "administered_date", "respondent_count")],
    respondent_types = by_name(fixture_table("respondent_type")[c("respondent_type_id", "name", "minimum_age", "maximum_age")])
  ))
}

# The pick lists, as lookup_service_get_all() returns them, made of the sample tables.
test_lookups <- function() {
  list(
    component_types = fixture_table("component_type")$name,
    subjects = fixture_table("subject")$name,
    answer_options = fixture_table("answer_option")$standardized_label,
    answer_data_types = c("NUMBER", "TEXT", "DATE", "TIME"),
    problems = character()
  )
}

gold_json <- function() {
  cached_fixture("gold_json", function() jsonlite::read_json(fixture_path("gna_gold.json"), simplifyVector = FALSE))
}

gold_draft <- function() {
  cached_fixture("gold_draft", function() draft_service_from_json(gold_json()))
}

# The gold questionnaire's first two parts (blocks c1 to c40, with their variables and choices), for the
# tests of what is drawn: the page draws every block it is given, and drawing all 1,000 rows of the gold
# questionnaire takes seconds each time. Its blocks, variables and choices have the same ids as in the gold
# questionnaire, so a test can use either.
short_gold_json <- function() {
  json <- gold_json()
  json$components <- json$components[1:2]
  json
}

short_gold_draft <- function() {
  cached_fixture("short_gold_draft", function() draft_service_from_json(short_gold_json()))
}

# The gold questionnaire with a MATR ID on every variable (m001, m002...), since a blank one is a
# problem to fix: for tests about everything else the checks look at.
with_matr_ids <- function(draft = gold_draft()) {
  draft$variables$matr_id <- sprintf("m%03d", seq_len(nrow(draft$variables)))
  draft
}

block_numbered <- function(draft, question_number) {
  draft$components[draft$components$question_number %in% question_number, ]
}

numbers_in <- function(draft, section_id) {
  numbers <- draft_service_children(draft, "components", section_id)$question_number
  numbers[!is.na(numbers)]
}

# The keys, as text, of the rows of `table` that match `rows`'s expression.
keys_where <- function(snap, table, condition) {
  rows <- snap[[table]]
  edit_key(rows[eval(substitute(condition), rows, parent.frame()), , drop = FALSE], edit_table_key(table))
}

# A form as the page makes it: for kind `key`, in `mode`, about the row with
# `row_key` (an update) in questionnaire `scope`. `...` are what's typed, by
# field id (kind__field); everything else is as the form starts.
form_for <- function(key, mode, row_key = NULL, scope = NULL, snap = NULL, ...) {
  entity <- edit_entities[[key]]
  snap <- snap %||% test_snapshot(edit_service_tables(entity))
  row <- if (!is.null(row_key)) edit_service_row(entity, snap, row_key)
  fields <- edit_service_fields(entity, snap, mode, row, scope)
  initial <- edit_service_initial(entity, fields, snap, mode, row, scope)
  values <- utils::modifyList(initial, list(...))
  list(entity = entity, snap = snap, row = row, scope = scope, fields = fields, initial = initial, values = values, mode = mode)
}

# The items of a form, as the page would save them.
form_items <- function(form) {
  edit_service_build(form$entity, form$mode, form$snap, form$row, form$scope, form$fields, form$values)
}

# The items as short lines, "Update wave 2 name: Wave 2 -> Round 2", for reading a test.
item_lines <- function(items) {
  paste0(
    items$action, " ", items$entity_name, " ", items$record_identifier, " ", items$attribute_name, ": ",
    ifelse(is.na(items$old_value), "-", items$old_value), " -> ", ifelse(is.na(items$new_value), "-", items$new_value)
  )
}

# A fake upload of a CSV, as a fileInput would hand it over.
fake_csv_upload <- function(lines, name = "matr_ids.csv") {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(lines, path)
  data.frame(name = name, size = file.size(path), type = "text/csv", datapath = path)
}

# A fake upload of a two-page PDF, and a place for the PDF cache that disappears after the test (the
# app keeps uploads in uploads/pdfs/).
fake_upload <- function(name = "AHP_wave3.pdf", text = "%PDF-1.4\n<< /Type /Page >>\n<< /Type /Page >>\n") {
  path <- withr::local_tempfile(fileext = ".pdf", .local_envir = parent.frame())
  writeLines(text, path)
  data.frame(name = name, size = file.size(path), type = "application/pdf", datapath = path)
}

local_pdf_cache <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_options(matr.pdf_dir = dir, .local_envir = env)
  dir
}

# One block of a draft, as a one-row data frame.
component <- function(draft, id) draft$components[draft$components$id == id, ]

# The draft with these blocks marked as checked.
with_checked <- function(draft, ids) {
  draft$components$checked[draft$components$id %in% ids] <- TRUE
  draft
}

# The ids of the blocks that are marked as checked.
checked_blocks <- function(draft) draft$components$id[draft$components$checked %in% TRUE]
