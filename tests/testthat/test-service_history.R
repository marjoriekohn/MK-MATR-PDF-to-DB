# # The Edit history service: which edits to show, who was behind each, and what each changed. The edits are
# # made by applying them as Make an edit does, and the history is what the database wrote.
#
# # Three edits by one ADMIN: an add, an update of it and a delete of it, and one a STAFF member's delete
# # approved by a colleague. Returns their records in the order the history lists them (newest first).
# # They are only read, so they are made once for the whole file and removed when it ends.
# history_of_four <- function(env) {
#   admin <- local_staff("ADMIN", name = paste(test_marker, "Alice Admin"), env = env)
#   me <- local_staff(name = paste(test_marker, "Maggie Staff"), env = env)
#   approver <- local_staff(name = paste(test_marker, "Alex Approver"), env = env)
#   name <- test_name("Parents")
#   added <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = name, note = "New kind of respondent", env = env)
#   key <- unname(database_model_get(test_connection, "respondent_type", "respondent_type_id", filters = list(name = database_eq(name)))[[1]])
#   updated <- local_applied_edit(admin, "respondent_type", "update", key, respondent_type__name = paste(name, "renamed"), note = "Typo in the name", env = env)
#   deleted <- local_applied_edit(admin, "respondent_type", "delete", key, note = "Not used any more", env = env)
#   approved <- local_approved_delete(me, approver, note = "Added by mistake", env = env)
#   ids <- as.character(c(approved, deleted, updated, added))
#   records <- history_service_get_all(test_connection)
#   list(records = records[names(records) %in% ids], ids = ids, admin = admin, me = me, approver = approver)
# }
#
# h <- history_of_four(environment()) # at the top of the file: removed when the file ends
#
# test_that("a kind narrows the edits, and 'all' keeps every kind", {
#   expect_equal(names(history_service_filter(h$records, "all")), h$ids)
#   expect_equal(names(history_service_filter(h$records, "Update")), h$ids[3])
#   expect_equal(names(history_service_filter(h$records, "Add")), h$ids[4])
#   expect_equal(names(history_service_filter(h$records, "Delete")), h$ids[1:2])
#   expect_equal(names(history_service_filter(h$records, "something else")), h$ids)
# })
#
# test_that("every word typed has to be in the sentence, the reason or a name, ignoring capitals", {
#   kept <- function(...) names(history_service_filter(h$records, ...))
#   expect_equal(kept("all", "CHANGED"), h$ids[3]) # the sentence
#   expect_equal(kept("all", "typo changed"), h$ids[3]) # the reason and the sentence
#   expect_equal(kept("all", "renamed"), h$ids[2]) # the name the row had when it was deleted
#   expect_equal(kept("all", "mistake"), h$ids[1]) # the reason
#   expect_equal(kept("all", "alex"), h$ids[1]) # the approver
#   expect_equal(kept("all", "alice"), h$ids[2:4]) # who prepared three
#   expect_equal(kept("all", "alice typo"), h$ids[3])
#   expect_length(history_service_filter(h$records, "all", "no such thing"), 0)
#   expect_equal(kept("Update", "  "), h$ids[3])
#   expect_equal(kept("all", NULL), h$ids)
# })
#
# test_that("who was behind an edit reads as a sentence", {
#   expect_equal(history_service_people(h$records[[h$ids[1]]]), paste0("prepared by ", h$me$name, " and approved by ", h$approver$name))
#   expect_equal(history_service_people(h$records[[h$ids[3]]]), paste0("made by ", h$admin$name, ", applied at once (no approval needed)"))
#   expect_equal(history_service_people(list(prepared_by = NA_character_, approved_by = NA_character_, approval_required = TRUE)), "prepared by someone no longer listed")
#   expect_equal(history_service_people(list(prepared_by = "Maggie Kohn", approved_by = NA_character_, approval_required = TRUE)), "prepared by Maggie Kohn")
# })
#
# test_that("when it was applied is in words, and an edit not applied says where it is", {
#   record <- h$records[[h$ids[3]]]
#   withr::local_timezone("UTC")
#   applied <- parse_time(record$applied)
#   expect_match(history_service_when(record, applied + 60), "^Applied today at [0-9]{1,2}:[0-9]{2} [AP]M$")
#   expect_equal(history_service_when(record, applied + 24 * 3600), "Applied yesterday")
#   expect_match(history_service_when(record, applied + 30 * 24 * 3600), "^Applied on [A-Z][a-z]{2} [0-9]{1,2}$")
#   expect_equal(history_service_when(list(applied = NULL, status = "Approved")), "Approved")
# })
#
# test_that("what an edit changed is its rows, grouped, with the counts", {
#   changes <- history_service_changes(test_connection, h$ids[3])
#   expect_length(changes$groups, 1)
#   expect_equal(changes$groups[[1]]$table, "respondent_type")
#   expect_equal(changes$groups[[1]]$fields$column, "name")
#   expect_equal(changes$counts, list(added = 0, changed = 1, removed = 0, tables = 1))
#
#   added <- history_service_changes(test_connection, h$ids[4])
#   expect_equal(added$counts$added, 1)
#   removed <- history_service_changes(test_connection, h$ids[2])
#   expect_equal(removed$counts$removed, 1)
#   expect_match(removed$groups[[1]]$fields$old[1], "Parents")
# })
