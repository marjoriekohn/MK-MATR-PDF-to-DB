#' # ------ The database the tests use -----------------------------------------------------------------------
#' # Everything that is about the database is tested against a real one: the dev Supabase project
#' # (`dev_project_ref` below says which), or a copy on this computer. There is no stand-in. Only the
#' # things that cost money or start processes (Gemini, the reading jobs) are replaced, with local_stub().
#' #
#' # The tests refuse to run if SUPABASE_URL is anything else, because they add rows and remove them.
#' # Each test makes the rows it needs and removes them again when it ends, however it ends. Every row
#' # has the run's marker in its name ("ZZTEST0712ab ..."), so it can't be mistaken for real data, and a
#' # test never changes or deletes a row it didn't make. test-zz_leftovers.R, the last test to run, checks
#' # that nothing is left behind; tests/tools/clean_test_rows.R removes what a crashed run left.
#' #
#' # One kind of row stays: a draft that was applied is the record of what was done, and the database
#' # refuses to delete it (draft_guard). So a test that applies a change (an edit, a pop-up add, a push)
#' # leaves the applied draft, its applied_item rows and the staff member who prepared it, marked with the
#' # run's marker and the staff member set inactive. The rows the change added to the live tables are removed.
#' #
#' # `test_connection` is the connection. Looking it up checks the address, so a test that doesn't use the
#' # database (the tests of pure functions) runs anywhere.
#'
#' # The project's folder: the first one up from here that has global.R. (Also where tests/tools/ run from.)
#' project_file <- function(...) {
#'   folder <- normalizePath(getwd())
#'   while (!file.exists(file.path(folder, "global.R")) && dirname(folder) != folder) folder <- dirname(folder)
#'   file.path(folder, ...)
#' }
#'
#' # The dev Supabase project's reference: the part of its address before .supabase.co. Production will be
#' # another project, and the tests must never run against it.
#' dev_project_ref <- "nvjyiaehomrysoprfuyl"
#'
#' #' Whether an address is one the tests may write to: the dev project, or a database on this computer.
#' #'
#' #' @param url A Supabase address.
#' #' @return `TRUE` or `FALSE`.
#' is_test_database <- function(url) {
#'   url <- tolower(trimws(url %||% ""))
#'   grepl(sprintf("^https://%s\\.supabase\\.co/?$", dev_project_ref), url) ||
#'     grepl("^https?://(127\\.0\\.0\\.1|localhost)(:[0-9]+)?/?$", url)
#' }
#'
#' test_database <- function() {
#'   url <- Sys.getenv("SUPABASE_URL")
#'   key <- Sys.getenv("SUPABASE_SECRET_KEY")
#'   if (!nzchar(url) || !nzchar(key)) {
#'     stop(
#'       "These tests run against the dev Supabase project. Put its SUPABASE_URL and SUPABASE_SECRET_KEY ",
#'       "in the .env file (docs/testing.md).",
#'       call. = FALSE
#'     )
#'   }
#'   if (!is_test_database(url)) {
#'     stop(
#'       sprintf(
#'         paste0(
#'           "Refusing to run: SUPABASE_URL is %s, which is neither the dev project (https://%s.supabase.co) nor a ",
#'           "database on this computer. The tests add rows to the database and remove them again."
#'         ),
#'         url, dev_project_ref
#'       ),
#'       call. = FALSE
#'     )
#'   }
#'   list(url = url, key = key)
#' }
#' makeActiveBinding("test_connection", test_database, environment())
#'
#' # ------ Marked rows ------------------------------------------------------------------------------------------
#' # One marker for the whole run, and a counter so two rows never share a name.
#' test_marker <- paste0("ZZTEST", format(Sys.time(), "%d%H%M"), paste(sample(letters, 2), collapse = ""))
#' test_counter <- local({
#'   n <- 0L
#'   function() {
#'     n <<- n + 1L
#'     n
#'   }
#' })
#'
#' #' A name for a row a test makes: the marker, then what it is.
#' #'
#' #' @param what Words saying what it is, e.g. `"Study"`.
#' #' @return A string like `"ZZTEST0712ab Study 7"`; different every time.
#' test_name <- function(what = "row") paste(test_marker, what, test_counter())
#'
#' # ------ Removing rows ----------------------------------------------------------------------------------------
#' # The app never deletes rows with a plain request, so the tests have their own.
#' test_delete <- function(table, filters) {
#'   database_request(test_connection, table) |>
#'     httr2::req_method("DELETE") |>
#'     database_filter(filters) |>
#'     database_perform()
#'   invisible()
#' }
#'
#' # Removes a draft a test made, unless it was applied since: an applied draft is the record of what was done,
#' # and the database keeps it (see the top of this file).
#' test_delete_draft <- function(id) {
#'   tryCatch(
#'     test_delete("draft", list(draft_id = database_eq(id))),
#'     error = function(problem) {
#'       if (!grepl("has been applied", conditionMessage(problem))) stop(problem)
#'     }
#'   )
#'   invisible()
#' }
#'
#' # The rows of `table` where some column is one of `ids`, a few hundred to a request.
#' test_delete_in <- function(table, column, ids) {
#'   ids <- unique(ids[!is.na(ids)])
#'   for (chunk in split(ids, ceiling(seq_along(ids) / 200))) {
#'     test_delete(table, stats::setNames(list(database_in(chunk)), column))
#'   }
#' }
#'
#' # The rows with these keys (each a named list of column to value), a hundred to a request.
#' test_delete_keys <- function(table, keys) {
#'   for (chunk in split(keys, ceiling(seq_along(keys) / 100))) {
#'     clauses <- vapply(chunk, function(key) {
#'       paste0("and(", paste0(names(key), ".eq.", unlist(key), collapse = ","), ")")
#'     }, character(1))
#'     test_delete(table, list(or = paste0("(", paste(clauses, collapse = ","), ")")))
#'   }
#' }
#'
#' # The ids a read of `table` found, for a filter written as `list(column = "eq.value")`.
#' test_ids <- function(table, column, filters) {
#'   rows <- database_model_get(test_connection, table, column, order_by = column, filters = filters)
#'   rows[[column]]
#' }
#'
#' #' Remove a staff member and everything that points at them.
#' #'
#' #' Drafts they prepared, saved, were asked to review or sent back, unless applied: an applied draft is the
#' #' record of what was done and the database keeps it, so a staff member one points at stays too, and is
#' #' set inactive instead. Drafts are test data too, whoever else they involve.
#' #'
#' #' @param staff_id The staff member's id.
#' #' @return Called for its effect.
#' test_remove_staff <- function(staff_id) {
#'   who <- as.character(staff_id)
#'   test_delete("draft", list(
#'     status = paste0("neq.", history_applied_status),
#'     or = sprintf("(prepared_by.eq.%s,saved_by.eq.%s,requested_reviewer.eq.%s,sent_back_by.eq.%s)", who, who, who, who)
#'   ))
#'   kept <- tryCatch(
#'     {
#'       test_delete("staff", list(staff_id = database_eq(who)))
#'       FALSE
#'     },
#'     error = function(problem) grepl("foreign key", conditionMessage(problem))
#'   )
#'   if (kept) database_model_update(test_connection, "staff", list(active = FALSE), list(staff_id = database_eq(who)), select = "staff_id")
#' }
#'
#' #' Remove the rows an applied change added to the live tables.
#' #'
#' #' The applied draft lists every row it added, with the numbers the database gave them, so what a test
#' #' added by applying an edit or pushing an extraction is removed from the list: the rows added last first,
#' #' since they point at the ones before. The applied draft itself and its items stay (see the top of this file).
#' #'
#' #' @param history_id The applied draft's id.
#' #' @return Called for its effect.
#' test_remove_applied <- function(history_id) {
#'   items <- history_model_get_items(test_connection, as.character(history_id))
#'   added <- unique(items[items$action == "Insert", c("entity_name", "record_identifier")])
#'   for (table in test_removal_order(unique(added$entity_name))) {
#'     keys <- lapply(added$record_identifier[added$entity_name == table], function(identifier) {
#'       jsonlite::fromJSON(identifier, simplifyVector = FALSE)
#'     })
#'     test_delete_keys(table, keys)
#'   }
#' }
#'
#' # The tables an edit can add to, children before the rows they point at. A variable's subtype row and its choices
#' # are not listed: they go with the variable (on delete cascade), and the database wants a variable and its subtype
#' # removed in one statement.
#' test_table_order <- c(
#'   "variable", "answer_option_set", "option_set", "answer_option",
#'   "wave_question", "questionnaire_question", "question_category", "question_scale",
#'   "questionnaire_component", "question", "component",
#'   "wave", "questionnaire", "study", "registry", "principal_investigator", "respondent_type", "subject",
#'   "category", "measurement_scale"
#' )
#' test_removal_order <- function(tables) {
#'   c(intersect(test_table_order, tables), setdiff(tables, c(test_table_order, "open_ended", "closed_ended", "response_option")))
#' }
#'
#' # ------ Making rows ------------------------------------------------------------------------------------------
#' # Each of these adds a row, removes it when the test ends (rows made later are removed first), and
#' # returns what the database stored.
#' local_row <- function(table, row, key, env = parent.frame()) {
#'   stored <- database_model_insert(test_connection, table, row)
#'   value <- stored[[key]]
#'   withr::defer(test_delete(table, stats::setNames(list(database_eq(value)), key)), envir = env)
#'   stored
#' }
#'
#' #' A staff member for the test.
#' #'
#' #' @param role `"STAFF"` or `"ADMIN"`.
#' #' @param active Whether they are active.
#' #' @param name Their name; marked and numbered if not given.
#' #' @param env The test's environment, where they are removed again.
#' #' @return What the site calls `me`: `list(staff_id, name, role)`, and their `email`.
#' local_staff <- function(role = "STAFF", active = TRUE, name = NULL, env = parent.frame()) {
#'   n <- test_counter()
#'   stored <- database_model_insert(test_connection, "staff", list(
#'     name = name %||% paste(test_marker, role, n),
#'     email = sprintf("%s-%d@example.test", tolower(test_marker), n),
#'     active = active, role = role
#'   ))
#'   withr::defer(test_remove_staff(stored$staff_id), envir = env)
#'   list(staff_id = stored$staff_id, name = stored$name, role = stored$role, email = stored$email)
#' }
#'
#' local_registry <- function(name = test_name("Registry"), env = parent.frame()) {
#'   local_row("registry", list(name = name, abbreviation = "ZZ", participant_count = 100), "registry_id", env)
#' }
#'
#' local_investigator <- function(name = test_name("Investigator"), env = parent.frame()) {
#'   local_row("principal_investigator", list(name = name, email = sprintf("pi%d@example.test", test_counter())), "principal_investigator_id", env)
#' }
#'
#' local_respondent_type <- function(name = test_name("Respondent Type"), minimum_age = 18, maximum_age = 99, env = parent.frame()) {
#'   local_row("respondent_type", list(name = name, minimum_age = minimum_age, maximum_age = maximum_age), "respondent_type_id", env)
#' }
#'
#' local_study <- function(registry, investigator, name = test_name("Study"), status = "Active", env = parent.frame()) {
#'   local_row("study", list(
#'     registry_id = registry$registry_id, principal_investigator_id = investigator$principal_investigator_id,
#'     name = name, status = status
#'   ), "study_id", env)
#' }
#'
#' local_questionnaire <- function(study, respondent_type, name = test_name("Questionnaire"), abbreviation = "ZZQ", env = parent.frame()) {
#'   local_row("questionnaire", list(
#'     study_id = study$study_id, respondent_type_id = respondent_type$respondent_type_id,
#'     name = name, abbreviation = abbreviation
#'   ), "questionnaire_id", env)
#' }
#'
#' local_wave <- function(questionnaire, name = test_name("Wave"), env = parent.frame()) {
#'   local_row("wave", list(questionnaire_id = questionnaire$questionnaire_id, name = name), "wave_id", env)
#' }
#'
#' #' A registry with a study, a questionnaire and a wave: what an extraction is added to.
#' #'
#' #' @param env The test's environment, where it is removed again.
#' #' @return A list of the rows: `registry`, `investigator`, `study`, `respondent_type`, `questionnaire`, `wave`.
#' local_tree <- function(env = parent.frame()) {
#'   registry <- local_registry(env = env)
#'   investigator <- local_investigator(env = env)
#'   respondent_type <- local_respondent_type(env = env)
#'   study <- local_study(registry, investigator, env = env)
#'   questionnaire <- local_questionnaire(study, respondent_type, env = env)
#'   wave <- local_wave(questionnaire, env = env)
#'   list(
#'     registry = registry, investigator = investigator, respondent_type = respondent_type,
#'     study = study, questionnaire = questionnaire, wave = wave
#'   )
#' }
#'
#' # Removes what an applied edit added when the test ends. Call it as soon as the edit is applied.
#' local_applied <- function(history_id, env = parent.frame()) {
#'   withr::defer(test_remove_applied(history_id), envir = env)
#'   invisible(history_id)
#' }
#'
#' # The sentence of the error a function raised, or NULL; for tests of what staff are told.
#' error_text <- function(code) {
#'   tryCatch(
#'     {
#'       code
#'       NULL
#'     },
#'     error = function(problem) conditionMessage(problem)
#'   )
#' }
