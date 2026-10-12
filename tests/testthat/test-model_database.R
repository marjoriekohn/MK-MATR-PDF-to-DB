# # The only file that talks to the database, tested against the real one (helper-00_realdb.R). Rows the
# # tests make are named with the run's marker, and a read that has to be exact filters on it, because the
# # database also holds rows the tests never made.
#
# mine <- function() list(name = paste0("like.", test_marker, "*"))
#
# test_that("a table is read with the columns asked for, in the order asked for, and a null is NA", {
#   local_registry(name = paste(test_marker, "B registry"), env = environment())
#   withr::defer(test_delete("registry", mine()))
#   database_model_insert(test_connection, "registry", list(name = paste(test_marker, "A registry"), abbreviation = "AR", participant_count = 5, description = NULL))
#   registries <- database_model_get(
#     test_connection, "registry", c("registry_id", "name", "description"), order_by = "name", filters = mine()
#   )
#   expect_named(registries, c("registry_id", "name", "description"))
#   expect_equal(registries$name, paste(test_marker, c("A", "B"), "registry"))
#   expect_true(all(is.na(registries$description)))
#   expect_type(registries$registry_id, "integer")
#   expect_s3_class(registry_model_get_all(test_connection), "data.frame")
# })
#
# test_that("a table longer than a page is read a page at a time", {
#   withr::defer(test_delete("registry", mine()))
#   for (i in 1:5) {
#     database_model_insert(test_connection, "registry", list(name = sprintf("%s page %d", test_marker, i), abbreviation = "P", participant_count = 1))
#   }
#   registries <- database_model_get(test_connection, "registry", c("registry_id", "name"), order_by = "name", filters = mine(), page_size = 2)
#   expect_equal(registries$name, sprintf("%s page %d", test_marker, 1:5))
#   expect_equal(anyDuplicated(registries$registry_id), 0)
# })
#
# test_that("a read that matches nothing has its columns and no rows", {
#   none <- database_model_get(
#     test_connection, "study", c("study_id", "name"), filters = list(name = database_eq("ZZTEST there is no such study"))
#   )
#   expect_equal(nrow(none), 0)
#   expect_named(none, c("study_id", "name"))
# })
#
# test_that("rows can be narrowed by filters, and values are encoded, never pasted in", {
#   me <- local_staff(name = paste(test_marker, "a b&c=d, (e)"))
#   found <- database_model_get(test_connection, "staff", c("staff_id", "name"), filters = list(name = database_eq(me$name)))
#   expect_equal(found$staff_id, me$staff_id)
#   expect_equal(nrow(database_model_get(test_connection, "staff", "staff_id", filters = list(staff_id = database_in(c(me$staff_id, 0))))), 1)
#   expect_equal(database_eq("Wave Load"), "eq.Wave Load")
#   expect_error(database_in("1,2) or (a"), "only list plain values")
# })
#
# test_that("counting a table asks for no rows, and can be narrowed", {
#   tree <- local_tree()
#   local_wave(tree$questionnaire)
#   expect_equal(database_model_count(test_connection, "registry"), nrow(registry_model_get_all(test_connection)))
#   expect_equal(
#     database_model_count(test_connection, "wave", filters = list(questionnaire_id = database_eq(tree$questionnaire$questionnaire_id))),
#     2L
#   )
#   expect_type(database_model_count(test_connection, "registry"), "integer")
# })
#
# test_that("a table with jsonb comes back as lists, as it was saved, with the people it names", {
#   me <- local_staff()
#   tree <- local_tree()
#   saved <- local_edit(me, local_respondent_type())
#   rows <- database_model_select(
#     test_connection, "draft", c("draft_id", "status", "preparer:staff!prepared_by(name)"),
#     filters = list(draft_id = database_eq(saved$id))
#   )
#   expect_length(rows, 1)
#   expect_equal(rows[[1]]$preparer$name, me$name)
#   expect_equal(rows[[1]]$status, "Draft")
#   details <- database_model_select(
#     test_connection, "edit_draft", c("draft_id", "form", "items", "reach"), filters = list(draft_id = database_eq(saved$id))
#   )
#   expect_equal(details[[1]]$form$action, "delete")
#   expect_equal(details[[1]]$form$values, list())
#   expect_equal(details[[1]]$items[[1]]$entity_name, "respondent_type")
#   expect_length(
#     database_model_select(test_connection, "draft", "draft_id", filters = list(draft_id = database_eq(0))),
#     0
#   )
# })
#
# test_that("a row is added with its nulls, and what the database stored comes back, defaults included", {
#   row <- list(name = test_name("Inserted"), email = sprintf("%s-ins@example.test", tolower(test_marker)), active = TRUE)
#   created <- database_model_insert(test_connection, "staff", row, select = c("staff_id", "name", "role", "email"))
#   withr::defer(test_delete("staff", list(staff_id = database_eq(created$staff_id))))
#   expect_equal(created$name, row$name)
#   expect_equal(created$role, "STAFF") # a default the database filled in
#   expect_match(as.character(created$staff_id), "^[0-9]+$")
#
#   registry <- database_model_insert(
#     test_connection, "registry", list(name = test_name("Nulls"), abbreviation = "N", participant_count = 3, description = NULL)
#   )
#   withr::defer(test_delete("registry", list(registry_id = database_eq(registry$registry_id))))
#   expect_null(registry$description)
# })
#
# test_that("a change names the rows, says whether anything matched, and never reaches a whole table", {
#   registry <- local_registry()
#   key <- list(registry_id = database_eq(registry$registry_id))
#   changed <- database_model_update(test_connection, "registry", list(abbreviation = "NEW"), key)
#   expect_length(changed, 1)
#   expect_equal(changed[[1]]$abbreviation, "NEW")
#   nothing <- database_model_update(test_connection, "registry", list(abbreviation = "X"), list(registry_id = database_eq(0)))
#   expect_length(nothing, 0)
#   expect_equal(database_model_get(test_connection, "registry", "abbreviation", filters = key)$abbreviation, "NEW")
#   expect_error(database_model_update(test_connection, "registry", list(abbreviation = "X"), list()), "needs to say which rows")
# })
#
# test_that("a function of the database is called by name, and its own sentence is shown as written", {
#   # create_draft refuses a kind it doesn't know with a sentence of its own
#   problem <- error_text(database_model_call(test_connection, "create_draft", list(p_kind = "Nonsense", p_prepared_by = 0, p_parent = list(), p_detail = list())))
#   expect_false(is.null(problem))
#   expect_false(grepl("^The MATR database had a problem", problem))
# })
#
# test_that("problems are sentences staff can act on", {
#   expect_error(
#     registry_model_get_all(list(url = test_connection$url, key = "sb_secret_not_the_key")),
#     "turned down the key. Check SUPABASE_SECRET_KEY"
#   )
#   expect_error(registry_model_get_all(list(url = "", key = "")), "Add SUPABASE_URL and SUPABASE_SECRET_KEY to the .env file")
# })
#
# test_that("a table the key hasn't been granted is not mistaken for a bad key", {
#   skip_if(!nzchar(Sys.getenv("SUPABASE_PUBLISHABLE_KEY")), "SUPABASE_PUBLISHABLE_KEY isn't set")
#   # the publishable key (the public dashboard's) can't read the staff table; supabase/check_access.R checks every table
#   publishable <- list(url = test_connection$url, key = Sys.getenv("SUPABASE_PUBLISHABLE_KEY"))
#   expect_error(
#     database_model_get(publishable, "staff", "email"),
#     "won't let the site use this table \\(permission denied for table staff\\).*supabase/migrations"
#   )
# })
#
# test_that("a row the database refuses is a sentence with its reason", {
#   expect_error(
#     database_model_insert(test_connection, "wave", list(questionnaire_id = 0, name = test_name("Orphan wave"))),
#     "wouldn't save that: insert or update on table \"wave\" violates foreign key"
#   )
# })
#
# test_that("a missing table and an unreachable database are sentences too", {
#   expect_error(
#     database_model_get(test_connection, "zztest_no_such_table", "name"),
#     "doesn't have that table: Could not find the table 'public.zztest_no_such_table'"
#   )
#   # nothing listens on port 1
#   expect_error(
#     registry_model_get_all(list(url = "http://127.0.0.1:1", key = "sb_secret_x")),
#     "Couldn't reach the MATR database"
#   )
# })
#
# test_that("the database's description of itself is read from the API's root", {
#   description <- database_model_describe(test_connection)
#   expect_true(all(c("registry", "staff", "draft", "edit_draft", "extracted_draft", "applied_item") %in% names(description$definitions)))
#   expect_true("/rpc/approve_draft" %in% names(description$paths))
# })
