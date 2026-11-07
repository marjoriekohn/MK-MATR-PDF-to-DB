# ------ Edit proposals for the tests ---------------------------------------------------------------------------
# The tables an edit of a row the test just made is built from, as edit_service_load() reads them, but in one
# request: only the kind's own table is read. Nothing points at the test's own row yet, so the tables that would
# (for a respondent type, its questionnaires and everything they hold) are empty here instead of read, and the
# edit comes out the same. Fine for a kind whose form picks from no other table, as a respondent type's doesn't.
test_edit_snap <- function(entity) {
  tables <- edit_service_tables(entity)
  stats::setNames(lapply(tables, function(table) {
    if (identical(table, entity$table)) table_model_get(test_connection, table) else empty_table(table)
  }), tables)
}

# A table with no rows, its columns of the kinds table_model_get() gives them.
empty_table <- function(table) {
  kinds <- database_tables[[table]]$columns
  as.data.frame(lapply(kinds, function(kind) switch(kind, id = , number = numeric(), flag = logical(), character())))
}

# A proposal as the page makes it before saving: staff `staff` delete the respondent type `respondent_type`.
# A delete is the change a STAFF member can't apply on their own, so it is the one that waits for
# approval. It has no id until edit_model_save() gives it one.
# `snap` is the tables it is built from, if they have been read already.
sample_edit <- function(staff, respondent_type, status = "Draft", now = Sys.time(), previous = NULL, snap = NULL) {
  force(respondent_type) # the row has to exist before the tables are read
  entity <- edit_entities$respondent_type
  snap <- snap %||% test_edit_snap(entity)
  key <- as.character(respondent_type$respondent_type_id)
  built <- edit_service_build(entity, "delete", snap, edit_service_row(entity, snap, key))
  form <- list(action = "delete", entity = "respondent_type", scope = "", target = key, values = list())
  edit_service_record(staff, form, built, status, previous = previous, now = now)
}

# A proposal saved in the database for the test, as `staff` saved it, and removed when the test ends with the
# other drafts of the test's staff (test_clear_drafts()). With no `respondent_type` it proposes to delete the
# file's (shared_respondent_type()); a test that applies the delete gives one of its own.
local_edit <- function(staff, respondent_type = NULL, status = "Draft", ..., env = parent.frame()) {
  shared <- if (is.null(respondent_type)) shared_respondent_type(env)
  respondent_type <- respondent_type %||% shared$row
  edit_model_save(test_connection, sample_edit(staff, respondent_type, status, ..., snap = shared$snap), staff$staff_id)
}

# A respondent type made once for a test file, for the proposals that only propose to delete it, and the
# tables a proposal about it is built from (test_edit_snap()), read once too. Removed when the file ends.
# Returns: A list: `row` (the respondent type, as stored) and `snap` (`NULL` outside a test, to be read then).
shared_respondent_type <- function(env = parent.frame()) {
  if (!is_test_body(env)) {
    return(list(row = local_respondent_type(env = env), snap = NULL))
  }
  file_shared(env, "respondent_type", function(file_env) {
    row <- local_respondent_type(env = file_env)
    list(row = row, snap = test_edit_snap(edit_entities$respondent_type))
  })
}

# An edit applied by `staff`, the way Make an edit does it: the form for kind `key` in mode `verb`, with
# these values typed (by field id) and, for an update or a delete, the row `row_key` (one the test made: see
# test_edit_snap()). Only an ADMIN can apply a delete without approval. Returns the history id; what the edit
# added is removed when the test ends.
local_applied_edit <- function(staff, key, verb, row_key = NULL, ..., env = parent.frame()) {
  entity <- edit_entities[[key]]
  snap <- test_edit_snap(entity)
  row <- if (!is.null(row_key)) edit_service_row(entity, snap, as.character(row_key))
  if (verb == "delete") {
    built <- edit_service_build(entity, "delete", snap, row)
    values <- list()
  } else {
    fields <- edit_service_fields(entity, snap, verb, row)
    initial <- edit_service_initial(entity, fields, snap, verb, row)
    values <- utils::modifyList(initial, list(...))
    built <- edit_service_build(entity, verb, snap, row, fields = fields, values = values)
  }
  form <- list(action = verb, entity = key, scope = "", target = as.character(row_key %||% ""), values = values)
  saved <- edit_service_save(test_connection, edit_service_record(staff, form, built, "Draft"), staff)
  history_id <- edit_service_apply(test_connection, saved, staff)
  local_applied(history_id, env = env)
  history_id
}

# A STAFF member's delete that another staff member approved: it was a change request, and is now in the
# history with its approver. Returns the history id.
local_approved_delete <- function(staff, approver, respondent_type = local_respondent_type(env = env), env = parent.frame()) {
  force(respondent_type)
  request <- local_edit(staff, respondent_type, status = "Awaiting Review", env = env)
  history_id <- change_request_service_approve(test_connection, request, approver)
  local_applied(history_id, env = env)
  history_id
}
