# # ------ Edit proposals for the tests ---------------------------------------------------------------------------
# # A proposal as the page makes it before saving: staff `staff` delete the respondent type `respondent_type`.
# # A delete is the change a STAFF member can't apply on their own, so it is the one that waits for
# # approval. It has no id until edit_model_save() gives it one.
# sample_edit <- function(staff, respondent_type, status = "Draft", now = Sys.time(), previous = NULL) {
#   force(respondent_type) # the row has to exist before the tables are read
#   entity <- edit_entities$respondent_type
#   snap <- edit_service_load(test_connection, entity)
#   key <- as.character(respondent_type$respondent_type_id)
#   built <- edit_service_build(entity, "delete", snap, edit_service_row(entity, snap, key))
#   form <- list(action = "delete", entity = "respondent_type", scope = "", target = key, values = list())
#   edit_service_record(staff, form, built, status, previous = previous, now = now)
# }
#
# # A proposal saved in the database for the test, as `staff` saved it, and removed when the test ends.
# local_edit <- function(staff, respondent_type = local_respondent_type(env = env), status = "Draft", ..., env = parent.frame()) {
#   force(respondent_type)
#   saved <- edit_model_save(test_connection, sample_edit(staff, respondent_type, status, ...), staff$staff_id)
#   withr::defer(test_delete_draft(saved$id), envir = env)
#   saved
# }
#
# # An edit applied by `staff`, the way Make an edit does it: the form for kind `key` in mode `verb`, with
# # these values typed (by field id) and, for an update or a delete, the row `row_key`. Only an ADMIN can apply
# # a delete without approval. Returns the history id; what the edit added is removed when the test ends.
# local_applied_edit <- function(staff, key, verb, row_key = NULL, ..., env = parent.frame()) {
#   entity <- edit_entities[[key]]
#   snap <- edit_service_load(test_connection, entity)
#   row <- if (!is.null(row_key)) edit_service_row(entity, snap, as.character(row_key))
#   if (verb == "delete") {
#     built <- edit_service_build(entity, "delete", snap, row)
#     values <- list()
#   } else {
#     fields <- edit_service_fields(entity, snap, verb, row)
#     initial <- edit_service_initial(entity, fields, snap, verb, row)
#     values <- utils::modifyList(initial, list(...))
#     built <- edit_service_build(entity, verb, snap, row, fields = fields, values = values)
#   }
#   form <- list(action = verb, entity = key, scope = "", target = as.character(row_key %||% ""), values = values)
#   saved <- edit_service_save(test_connection, edit_service_record(staff, form, built, "Draft"), staff)
#   history_id <- edit_service_apply(test_connection, saved, staff)
#   local_applied(history_id, env = env)
#   history_id
# }
#
# # A STAFF member's delete that another staff member approved: it was a change request, and is now in the
# # history with its approver. Returns the history id.
# local_approved_delete <- function(staff, approver, respondent_type = local_respondent_type(env = env), env = parent.frame()) {
#   force(respondent_type)
#   request <- local_edit(staff, respondent_type, status = "Awaiting Review", env = env)
#   history_id <- change_request_service_approve(test_connection, request, approver)
#   local_applied(history_id, env = env)
#   history_id
# }
