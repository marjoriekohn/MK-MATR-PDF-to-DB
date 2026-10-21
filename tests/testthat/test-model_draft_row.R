# What staff are working on before it reaches the live tables is a DRAFT with a row of details for its kind
# (EXTRACTED_DRAFT or EDIT_DRAFT). These are the model's functions for them, and the rules the database keeps
# whoever asks, tested against the real database.

edit_detail <- function() list(edit_kind = "Update", form = list(action = "update", entity = "wave", values = list(wave__name = "R")), items = list())

new_edit <- function(me, parent = list(), detail = edit_detail(), env = parent.frame()) {
  id <- draft_row_model_create(test_connection, "Edit", me$staff_id, parent, detail)
  withr::defer(test_delete("draft", list(draft_id = database_eq(id))), envir = env)
  id
}

test_that("only numbers go into a draft's filter", {
  expect_equal(draft_row_filter("12"), list(draft_id = "eq.12"))
  expect_error(draft_row_filter("1&kind=eq.Edit"), "no draft with that number")
  expect_error(draft_row_filter(NULL), "no draft with that number")
  # expect_error(draft_row_model_get(test_connection, "1 or 1=1"), "no draft with that number")
})

test_that("the database's times are the app's times", {
  expect_equal(database_time("2026-10-03T04:14:50.91677+00:00"), "2026-10-03T04:14:50Z")
  expect_equal(database_time("2026-10-03T00:14:50-04:00"), "2026-10-03T04:14:50Z")
  expect_null(database_time(NULL))
})

test_that("an empty set of columns is sent as an empty object, not an empty list", {
  expect_equal(jsonlite::toJSON(draft_row_object(list()), auto_unbox = TRUE) |> as.character(), "{}")
  expect_equal(draft_row_object(list(a = 1)), list(a = 1))
})

test_that("a draft and its details are added together, and read back with who prepared it", {
  me <- local_staff()
  id <- new_edit(me, list(summary = "Rename a wave", created_at = "2026-10-02T15:30:12Z"))
  expect_match(id, "^[0-9]+$")

  row <- draft_row_model_get(test_connection, id)
  expect_equal(row$kind, "Edit")
  expect_equal(row$status, "Draft") # the database's default
  expect_equal(row$revision, 1)
  expect_equal(row$prepared_by, me$staff_id)
  expect_equal(row$saved_by, me$staff_id)
  expect_equal(row$preparer$name, me$name)
  expect_equal(row$summary, "Rename a wave")
  expect_equal(database_time(row$created_at), "2026-10-02T15:30:12Z") # a time the caller gave is kept
  expect_null(row$requested_reviewer)

  details <- draft_row_model_details(test_connection, edit_draft_table, id, c("edit_kind", "form", "items"))
  expect_named(details, id)
  expect_equal(details[[id]]$edit_kind, "Update")
  expect_equal(details[[id]]$form$values$wave__name, "R")
  expect_equal(details[[id]]$items, list()) # the column's default
  expect_length(draft_row_model_details(test_connection, edit_draft_table, character(), "form"), 0)
})

test_that("a draft that isn't there is NULL, and its details are left out", {
  expect_null(draft_row_model_get(test_connection, "0"))
  expect_length(draft_row_model_details(test_connection, edit_draft_table, "0", "form"), 0)
})

test_that("an extraction's details are its own columns, with defaults for the rest", {
  me <- local_staff()
  id <- draft_row_model_create(test_connection, "Extraction", me$staff_id, list(), list(method = "Manual", info = list(kind = "new_wave")))
  withr::defer(test_delete("draft", list(draft_id = database_eq(id))))
  details <- draft_row_model_details(test_connection, extracted_draft_table, id, c("method", "info", "reading:ai_extracted_draft(reading_status)"))
  expect_equal(details[[id]]$method, "Manual")
  expect_equal(details[[id]]$info$kind, "new_wave")
  expect_null(draft_row_one(details[[id]]$reading)) # a Manual extraction has no reading
  expect_error(
    draft_row_model_create(test_connection, "Extraction", me$staff_id, list(), list(method = "Manual", reading_status = "queued")),
    "A Manual extraction has no reading"
  )
})

test_that("an AI-Powered extraction's reading has its own row, and its PDF is kept once, by its hash", {
  me <- local_staff()
  sha256 <- paste0("zztest", test_marker, test_counter())
  withr::defer(test_delete_pdf(sha256))
  detail <- list(
    method = "AI-Powered", pdf_name = "a.pdf", pdf_sha256 = sha256, pdf_pages = 3L, pdf_bytes = 120L,
    reading_status = "queued", reading_started = "2026-10-02T10:00:00Z"
  )
  first <- draft_row_model_create(test_connection, "Extraction", me$staff_id, list(), detail)
  withr::defer(test_delete("draft", list(draft_id = database_eq(first))))
  # the same file again, under another name, and not knowing its pages: the file keeps what it knows
  second <- draft_row_model_create(test_connection, "Extraction", me$staff_id, list(), utils::modifyList(detail, list(pdf_name = "b.pdf", pdf_pages = NULL), keep.null = TRUE))
  withr::defer(test_delete("draft", list(draft_id = database_eq(second))))

  reading <- draft_row_model_details(test_connection, ai_extracted_draft_table, first, c("reading_status", "review_edits"))[[first]]
  expect_equal(reading$reading_status, "queued")
  expect_equal(reading$review_edits, list()) # the column's default
  files <- database_model_get(test_connection, "pdf_file", c("pdf_sha256", "pdf_pages", "pdf_bytes"), filters = list(pdf_sha256 = database_eq(sha256)))
  expect_equal(nrow(files), 1)
  expect_equal(files$pdf_pages, 3)
  expect_equal(files$pdf_bytes, 120)
  names <- draft_row_model_details(test_connection, extracted_draft_table, c(first, second), "pdf_name")
  expect_equal(c(names[[first]]$pdf_name, names[[second]]$pdf_name), c("a.pdf", "b.pdf"))
  expect_error(
    draft_row_model_create(test_connection, "Extraction", me$staff_id, list(), list(method = "AI-Powered", reading_status = "queued")),
    "extracted_draft_ai_has_pdf"
  )
})

test_that("drafts are listed by kind, newest saved first, narrowed by status, without those finished with", {
  me <- local_staff()
  other <- local_staff()
  first <- new_edit(me)
  second <- new_edit(other)
  extraction <- draft_row_model_create(test_connection, "Extraction", me$staff_id, list(), list(method = "Manual"))
  withr::defer(test_delete("draft", list(draft_id = database_eq(extraction))))
  draft_row_model_save(test_connection, first, me$staff_id, list(status = "Awaiting Review"))

  ours <- c(first, second)
  ids_of <- function(rows) vapply(rows, function(row) as.character(row$draft_id), character(1))
  listed <- ids_of(draft_row_model_list(test_connection, "Edit"))
  expect_equal(listed[listed %in% ours], c(first, second)) # the one saved last comes first
  waiting <- ids_of(draft_row_model_list(test_connection, "Edit", "Awaiting Review"))
  expect_equal(waiting[waiting %in% ours], first)
  expect_false(extraction %in% listed)
  expect_true(extraction %in% ids_of(draft_row_model_list(test_connection, "Extraction")))

  # one finished with (withdrawn, or applied) is left out: the lists only grow with those
  draft_row_model_save(test_connection, second, other$staff_id, parent = list(status = "Withdrawn"), new_revision = FALSE)
  expect_false(second %in% ids_of(draft_row_model_list(test_connection, "Edit")))
  expect_equal(draft_row_model_get(test_connection, second)$status, "Withdrawn") # read on its own, it is there
})

test_that("saving raises the revision, says who saved, and writes only the columns named", {
  me <- local_staff()
  colleague <- local_staff()
  id <- new_edit(me, list(summary = "one", requested_reviewer = colleague$staff_id))

  revision <- draft_row_model_save(test_connection, id, colleague$staff_id, list(summary = "two"), list(form = list(a = "b")))
  expect_equal(revision, 2)
  row <- draft_row_model_get(test_connection, id)
  expect_equal(row$summary, "two")
  expect_equal(row$requested_reviewer, colleague$staff_id) # a column not named keeps its value
  expect_equal(row$revision, 2)
  expect_equal(row$saved_by, colleague$staff_id)
  expect_equal(row$saver$name, colleague$name)
  expect_equal(row$prepared_by, me$staff_id) # who prepared it never changes
  details <- draft_row_model_details(test_connection, edit_draft_table, id, c("edit_kind", "form"))[[id]]
  expect_equal(details$form, list(a = "b"))
  expect_equal(details$edit_kind, "Update")

  # a NULL empties a column
  draft_row_model_save(test_connection, id, me$staff_id, list(requested_reviewer = NULL))
  expect_null(draft_row_model_get(test_connection, id)$requested_reviewer)
  # a change that isn't to the content keeps the revision
  expect_equal(draft_row_model_save(test_connection, id, me$staff_id, list(summary = "three"), new_revision = FALSE), 3)
  expect_equal(draft_row_model_get(test_connection, id)$revision, 3)
})

test_that("a save of a version someone else has since changed is refused", {
  me <- local_staff()
  id <- new_edit(me)
  seen <- draft_row_model_get(test_connection, id)$revision
  draft_row_model_save(test_connection, id, me$staff_id, list(summary = "someone else's"), expected_revision = seen)
  expect_error(
    draft_row_model_save(test_connection, id, me$staff_id, list(summary = "mine"), expected_revision = seen),
    "changed after you opened it"
  )
  expect_equal(draft_row_model_get(test_connection, id)$summary, "someone else's")
  # without a revision to check against, the save goes through
  expect_equal(draft_row_model_save(test_connection, id, me$staff_id, list(summary = "mine")), seen + 2)
})

test_that("only active staff prepare, change or are asked to review a draft", {
  me <- local_staff()
  former <- local_staff(active = FALSE)
  expect_error(draft_row_model_create(test_connection, "Edit", former$staff_id, list(), edit_detail()), "Only active staff can prepare a draft")
  expect_error(draft_row_model_create(test_connection, "Edit", 0, list(), edit_detail()), "Only active staff can prepare a draft")
  expect_error(
    draft_row_model_create(test_connection, "Edit", me$staff_id, list(requested_reviewer = former$staff_id), edit_detail()),
    "Only active staff can be asked to review"
  )
  id <- new_edit(me)
  expect_error(draft_row_model_save(test_connection, id, former$staff_id, list(summary = "x")), "Only active staff can change a draft")
  expect_error(
    draft_row_model_save(test_connection, id, me$staff_id, list(requested_reviewer = former$staff_id)),
    "Only active staff can be asked to review"
  )
  expect_error(draft_row_model_save(test_connection, "0", me$staff_id, list()), "no longer in the database")
})

test_that("a reviewer can be asked, but not the person who prepared it", {
  me <- local_staff()
  colleague <- local_staff()
  id <- new_edit(me, list(requested_reviewer = colleague$staff_id))
  row <- draft_row_model_get(test_connection, id)
  expect_equal(row$requested_reviewer, colleague$staff_id)
  expect_equal(draft_row_person(row$requested_reviewer, row$reviewer), list(id = colleague$staff_id, name = colleague$name))
  expect_error(draft_row_model_save(test_connection, id, me$staff_id, list(requested_reviewer = me$staff_id)), "draft_reviewer_is_someone_else")
})

test_that("what a caller may not set, and what isn't a column, is refused with a sentence", {
  me <- local_staff()
  colleague <- local_staff()
  id <- new_edit(me)
  expect_error(draft_row_model_save(test_connection, id, me$staff_id, list(prepared_by = colleague$staff_id)), "prepared_by can't be set this way")
  expect_error(draft_row_model_save(test_connection, id, me$staff_id, list(revision = 9)), "revision can't be set this way")
  expect_error(draft_row_model_save(test_connection, id, me$staff_id, list(status = "Applied")), "applied by applying it")
  expect_error(draft_row_model_save(test_connection, id, me$staff_id, list(), list(nonsense = 1)), "edit_draft has no column nonsense")
  expect_error(draft_row_model_create(test_connection, "Something", me$staff_id), "An Extraction or an Edit|A draft is an Extraction or an Edit")
  expect_error(draft_row_model_create(test_connection, "Edit", me$staff_id, list(kind = "Extraction"), edit_detail()), "kind can't be set this way")
})

test_that("a draft only moves forward: sent for review, sent back, withdrawn; and never back to a plain draft", {
  me <- local_staff()
  reviewer <- local_staff()
  id <- new_edit(me)
  expect_null(draft_row_submitted(draft_row_model_get(test_connection, id)))
  expect_error(change_request_model_send_back(test_connection, id, 1L, reviewer$staff_id, "no"), "Only a change waiting for review can be sent back")
  draft_row_model_save(test_connection, id, me$staff_id, list(status = "Awaiting Review"))
  submitted <- draft_row_submitted(draft_row_model_get(test_connection, id))
  expect_match(submitted, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T") # the database records when, and who
  expect_error(draft_row_model_save(test_connection, id, me$staff_id, list(status = "Draft")), "can't go back to being a plain draft")
  revision <- draft_row_model_get(test_connection, id)$revision
  change_request_model_send_back(test_connection, id, revision, reviewer$staff_id, "check the age")
  row <- draft_row_model_get(test_connection, id)
  expect_equal(row$status, "Sent Back")
  expect_equal(row$revision, revision) # sending back isn't a change to the content
  sent <- draft_row_sent_back(row)
  expect_equal(sent[c("by", "by_id", "note")], list(by = reviewer$name, by_id = reviewer$staff_id, note = "check the age"))
  expect_match(sent$date, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
  expect_equal(draft_row_submitted(row), submitted) # still the one time it was sent
  expect_null(draft_row_sent_back(list(status = "Draft")))

  draft_row_model_save(test_connection, id, me$staff_id, list(status = "Withdrawn"), new_revision = FALSE)
  expect_error(draft_row_model_save(test_connection, id, me$staff_id, list(status = "Awaiting Review")), "withdrawn, so it can't be changed")
  expect_equal(draft_row_model_get(test_connection, id)$status, "Withdrawn")
})

test_that("a sent-back draft says who sent it back, in the database too", {
  me <- local_staff()
  id <- new_edit(me)
  draft_row_model_save(test_connection, id, me$staff_id, list(status = "Awaiting Review"))
  expect_error(
    draft_row_model_save(test_connection, id, me$staff_id, list(status = "Sent Back")),
    "sent back with send_back_draft"
  )
  expect_error(draft_row_model_save(test_connection, id, me$staff_id, list(submitted_at = "2026-10-02T10:00:00Z")), "submitted_at can't be set this way")
})

test_that("a draft's reviews are read newest first, and only a sent-back draft says who sent it back", {
  row <- list(status = "Sent Back", reviews = list(
    list(event = "Submitted", staff_id = 1, occurred_at = "2026-10-02T09:00:00+00:00"),
    list(event = "Sent Back", staff_id = 2, note = "first", occurred_at = "2026-10-02T10:00:00+00:00", person = list(name = "Ana")),
    list(event = "Submitted", staff_id = 1, occurred_at = "2026-10-02T11:00:00+00:00"),
    list(event = "Sent Back", staff_id = 3, note = "second", occurred_at = "2026-10-02T12:00:00+00:00", person = list(name = "Ben"))
  ))
  expect_equal(draft_row_submitted(row), "2026-10-02T11:00:00Z")
  expect_equal(draft_row_sent_back(row), list(by = "Ben", by_id = 3, date = "2026-10-02T12:00:00Z", note = "second"))
  expect_null(draft_row_latest(row, "Approved"))
  expect_null(draft_row_sent_back(utils::modifyList(row, list(status = "Awaiting Review"))))
  expect_equal(draft_row_one(list(list(a = 1))), list(a = 1))
  expect_equal(draft_row_one(list(a = 1)), list(a = 1))
  expect_null(draft_row_one(list()))
})

test_that("a detail row can be changed where it still matches, without raising the revision", {
  me <- local_staff()
  id <- draft_row_model_create(test_connection, "Extraction", me$staff_id, list(), list(method = "AI-Powered", pdf_name = "a.pdf", pdf_sha256 = "zztest", reading_status = "queued", reading_started = "2026-10-02T10:00:00Z"))
  withr::defer(test_delete("draft", list(draft_id = database_eq(id))))
  revision <- draft_row_model_get(test_connection, id)$revision

  stale <- draft_row_model_update_detail(
    test_connection, ai_extracted_draft_table, id, list(reading_status = "sent"),
    where = list(reading_started = database_eq("2026-10-02T09:00:00Z"))
  )
  expect_null(stale)
  current <- draft_row_model_update_detail(
    test_connection, ai_extracted_draft_table, id, list(reading_status = "sent"),
    where = list(reading_started = database_eq("2026-10-02T10:00:00Z")), select = c("draft_id", "reading_status")
  )
  expect_equal(current$reading_status, "sent")
  expect_equal(draft_row_model_get(test_connection, id)$revision, revision)
})

test_that("removing a draft removes its details, and a draft needs its details", {
  me <- local_staff()
  id <- draft_row_model_create(test_connection, "Edit", me$staff_id, list(), edit_detail())
  test_delete("draft", list(draft_id = database_eq(id)))
  expect_null(draft_row_model_get(test_connection, id))
  expect_length(draft_row_model_details(test_connection, edit_draft_table, id, "form"), 0)
  # a draft added on its own, without details, is refused when the change ends
  expect_error(
    database_model_insert(test_connection, "draft", list(kind = "Edit", prepared_by = me$staff_id, saved_by = me$staff_id)),
    "needs its details"
  )
})

test_that("one person at a time has a draft: whoever claims it first, until they let go, their claim lapses or someone takes over", {
  me <- local_staff()
  colleague <- local_staff()
  id <- new_edit(me)

  mine <- draft_row_model_claim(test_connection, id, me$staff_id)
  expect_true(mine$claimed)
  expect_equal(mine$by_id, me$staff_id)
  expect_equal(mine$by, me$name)
  expect_equal(mine$revision, 1)
  expect_match(mine$at, "Z$") # as the app writes times

  theirs <- draft_row_model_claim(test_connection, id, colleague$staff_id)
  expect_false(theirs$claimed)
  expect_equal(theirs$by, me$name) # told who has it
  expect_true(draft_row_model_claim(test_connection, id, me$staff_id)$claimed) # who has it renews it

  # a claim not renewed for stale_seconds has lapsed, and anyone may take over
  expect_true(draft_row_model_claim(test_connection, id, colleague$staff_id, stale_seconds = 0)$claimed)
  expect_true(draft_row_model_claim(test_connection, id, me$staff_id, take_over = TRUE)$claimed)

  row <- draft_row_model_get(test_connection, id)
  expect_equal(draft_row_editing(row)$by, me$name)
  expect_equal(draft_row_editing(row)$by_id, me$staff_id)
  expect_equal(row$revision, 1) # claiming isn't a change to the content
  expect_equal(row$saved_by, me$staff_id)

  expect_false(draft_row_model_release(test_connection, id, colleague$staff_id)) # only who has it lets it go
  expect_true(draft_row_model_release(test_connection, id, me$staff_id))
  expect_null(draft_row_editing(draft_row_model_get(test_connection, id)))
  expect_error(draft_row_model_claim(test_connection, "1", me$staff_id), "no longer in the database")
})
