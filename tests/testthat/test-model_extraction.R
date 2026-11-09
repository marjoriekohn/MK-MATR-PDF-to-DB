# An extraction: who started it, how it was made, how far reading it has got, where it goes and what staff
# typed about it, kept in the database as one row of `extraction` (its PDF's pages and size in pdf_file).

# The database keeps JSON as jsonb, which orders an object's keys its own way, so two JSON values are the same
# when they hold the same names and values, whatever the order.
sorted_names <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  if (!is.null(names(x))) x <- x[order(names(x))]
  lapply(x, sorted_names)
}
expect_same_json <- function(actual, expected) expect_equal(sorted_names(actual), sorted_names(expected))

test_that("an extraction is saved to the database and read back as a record", {
  local_pdf_cache()
  me <- local_staff()
  saved <- local_extraction(me)

  expect_match(saved$id, "^[0-9]+$")
  expect_equal(saved$kind, "Extraction")
  expect_equal(saved$method, "AI-Powered")
  expect_equal(saved$prepared_by, me$name)
  expect_equal(saved$prepared_by_id, me$staff_id)
  expect_equal(saved$saved_by_id, me$staff_id)
  expect_equal(saved$started, "2026-10-02T10:00:00Z") # the time it was given
  expect_equal(saved$status, "Draft")
  expect_null(saved$pushed)
  expect_equal(saved$revision, 1)
  expect_equal(saved$extraction_status, "queued")
  # the Basic Info picks, as given (the database keeps them as JSON, which doesn't keep the order of the names)
  expect_mapequal(saved$info, list(questionnaire_id = "1", wave_id = "2"))
  expect_equal(saved$pdf$name, "AHP_wave3.pdf")
  expect_equal(saved$pdf$pages, 2)
  expect_equal(saved$pdf$path, extraction_model_pdf_path(saved$pdf$sha256))
  expect_null(saved$error)
  expect_null(saved$reading_pid)
  expect_null(saved$matr_csv)
  expect_null(saved$editing)
  expect_equal(extraction_model_get(test_connection, saved$id), saved)
})

test_that("a draft typed by hand has no reading and keeps what staff said about it", {
  me <- local_staff()
  saved <- local_manual_extraction(me)
  expect_equal(saved$method, "Manual")
  expect_null(saved$extraction_status)
  expect_null(saved$pdf$name)
  expect_null(saved$pdf$path)
  expect_equal(saved$info$wave_id, "2")
})

test_that("records are read newest saved first, named by id, in one request, and a pushed one leaves the list", {
  me <- local_staff()
  # saved a minute apart: ids are random, so two saved at the same moment come in either order
  first <- local_extraction(me, saved = "2026-10-02T10:00:00Z")
  second <- local_extraction(me, saved = "2026-10-02T10:01:00Z")
  before <- test_requests$count
  records <- extraction_model_get_all(test_connection)
  expect_equal(test_requests$count - before, 1L)
  before <- test_requests$count
  expect_equal(extraction_model_get(test_connection, first$id), records[[first$id]])
  expect_equal(test_requests$count - before, 1L)
  ours <- names(records)[names(records) %in% c(first$id, second$id)]
  expect_equal(ours, c(second$id, first$id))
  expect_error(extraction_model_get(test_connection, "0"), "no longer in the database")
  expect_error(extraction_model_get(test_connection, "abc"), "no draft with that number")

  tree <- local_tree()
  pushed <- local_pushed_questionnaire(me, tree)
  expect_false(pushed$id %in% names(extraction_model_get_all(test_connection)))
  expect_true(pushed$id %in% names(extraction_model_get_all(test_connection, status = "Pushed")))
  expect_true(pushed$id %in% names(extraction_model_get_all(test_connection, status = NULL)))
  kept <- extraction_model_get(test_connection, pushed$id)
  expect_equal(kept$status, "Pushed")
  expect_match(kept$pushed, "^[0-9]{4}-")
})

test_that("saving writes only the fields that changed, so what the reading job and staff change don't undo each other", {
  me <- local_staff()
  record <- local_extraction(me)

  # the reading job says how far it has got
  expect_true(extraction_model_save_reading(test_connection, record$id, list(extraction_status = "sent", error = "Slow"), record$reading_started))

  # staff change what they typed, from a copy of the record read before the job's news
  record$info$wave_id <- "4"
  saved <- extraction_model_save(test_connection, record, "info", me$staff_id)
  expect_equal(saved$info$wave_id, "4")
  expect_equal(saved$extraction_status, "sent") # the job's news is still there
  expect_equal(saved$error, "Slow")
  expect_equal(saved$revision, 2) # the database raised it: the content changed
  expect_equal(saved$saved_by_id, me$staff_id)

  # and the job's next news doesn't undo what staff typed
  extraction_model_save_reading(test_connection, record$id, list(extraction_status = "received"), record$reading_started)
  again <- extraction_model_get(test_connection, record$id)
  expect_equal(again$extraction_status, "received")
  expect_equal(again$info$wave_id, "4")
  expect_equal(again$revision, 2) # the job's progress isn't a change to the content

  # a change that isn't to the content (restarting the reading) doesn't raise it either
  restarted <- extraction_model_save(test_connection, utils::modifyList(again, list(extraction_status = "queued")), "extraction_status", me$staff_id)
  expect_equal(restarted$revision, 2)
})

test_that("a save of a version someone has since changed is refused, and a draft that is gone is a sentence", {
  me <- local_staff()
  record <- local_extraction(me)
  elsewhere <- record
  elsewhere$info$wave_id <- "3"
  extraction_model_save(test_connection, elsewhere, "info", me$staff_id) # someone else's save: the revision moves on
  record$info$wave_id <- "5"
  expect_error(extraction_model_save(test_connection, record, "info", me$staff_id, expected_revision = record$revision), "changed after you opened it")
  expect_equal(extraction_model_get(test_connection, record$id)$info$wave_id, "3") # nothing was written
  gone <- utils::modifyList(record, list(id = "0"))
  expect_error(extraction_model_save(test_connection, gone, "info", me$staff_id), "no longer in the database")
})

test_that("the reading job can only save for the reading it was started for, and only the columns it owns", {
  me <- local_staff()
  record <- local_extraction(me)
  expect_false(extraction_model_save_reading(test_connection, record$id, list(extraction_status = "ready"), "2026-10-02T09:00:00Z"))
  expect_equal(extraction_model_get(test_connection, record$id)$extraction_status, "queued")
  expect_true(extraction_model_save_reading(test_connection, record$id, list(extraction_status = "ready", model = "gemini-x", prompt_version = "abc"), "2026-10-02T10:00:00Z"))
  back <- extraction_model_get(test_connection, record$id)
  expect_equal(back$extraction_status, "ready")
  expect_equal(back$model, "gemini-x")
  expect_equal(back$prompt_version, "abc")
  expect_equal(back$revision, 1) # the job's columns don't raise it
  expect_error(extraction_model_save_reading(test_connection, record$id, list(info = list()), "2026-10-02T10:00:00Z"), "can't save info")

  # what it read is kept the same way
  expect_false(extraction_model_save_reading_result(test_connection, record$id, list(components = list()), "2026-10-02T09:00:00Z"))
  expect_null(extraction_model_questionnaire(test_connection, record$id)$original)
  expect_true(extraction_model_save_reading_result(test_connection, record$id, list(components = list()), record$reading_started))
  expect_equal(extraction_model_questionnaire(test_connection, record$id)$original, list(components = list()))
})

test_that("only one site can claim a draft for reading", {
  me <- local_staff()
  record <- local_extraction(me)
  expect_false(extraction_model_claim_reading(test_connection, record$id, "2026-10-02T09:00:00Z")) # a reading since restarted
  expect_true(extraction_model_claim_reading(test_connection, record$id, "2026-10-02T10:00:00Z", now = as.POSIXct("2026-10-02 10:00:05", tz = "UTC")))
  expect_false(extraction_model_claim_reading(test_connection, record$id, "2026-10-02T10:00:00Z"))
  expect_equal(extraction_model_get(test_connection, record$id)$reading_claimed_at, "2026-10-02T10:00:05Z")
})

test_that("the questionnaire staff save is kept apart from what the AI returned, and the save is a new revision", {
  me <- local_staff()
  record <- local_read_extraction(me)
  json <- extraction_model_questionnaire(test_connection, record$id)
  expect_null(json$questionnaire)
  expect_same_json(json$original, short_gold_json())

  reviewed <- list(schema_version = 2L, questionnaire = list(name = "Mine"), components = list())
  revision <- extraction_model_save_questionnaire(test_connection, record$id, reviewed, me$staff_id, review_edits = list(list(change = "edited")), expected_revision = record$revision)
  expect_equal(revision, record$revision + 1)
  json <- extraction_model_questionnaire(test_connection, record$id)
  expect_same_json(json$questionnaire, reviewed)
  expect_same_json(json$original, short_gold_json()) # never touched
  expect_equal(review_edits_of(test_connection, record$id), list(list(change = "edited")))
  expect_equal(extraction_model_get(test_connection, record$id)$saved_by_id, me$staff_id)

  # a stale save is refused; clearing the copy puts it aside with its edits
  expect_error(extraction_model_save_questionnaire(test_connection, record$id, reviewed, me$staff_id, expected_revision = record$revision), "changed after you opened it")
  cleared <- extraction_model_clear_questionnaire(test_connection, record$id, me$staff_id, expected_revision = revision)
  expect_equal(cleared, revision + 1)
  expect_null(extraction_model_questionnaire(test_connection, record$id)$questionnaire)
  expect_equal(review_edits_of(test_connection, record$id), list())
  expect_error(extraction_model_questionnaire(test_connection, "0"), "no longer in the database")
})

test_that("one person at a time has a draft: whoever claims it first, until they let go, their claim lapses or someone takes over", {
  me <- local_staff()
  colleague <- local_staff()
  record <- local_extraction(me)
  mine <- extraction_model_claim(test_connection, record$id, me$staff_id)
  expect_true(mine$claimed)
  expect_equal(mine$by_id, me$staff_id)
  expect_equal(mine$by, me$name)
  expect_equal(mine$revision, record$revision)
  expect_match(mine$at, "^[0-9]{4}-")
  theirs <- extraction_model_claim(test_connection, record$id, colleague$staff_id)
  expect_false(theirs$claimed)
  expect_equal(theirs$by_id, me$staff_id) # and are told who has it
  expect_equal(theirs$by, me$name)
  expect_true(extraction_model_claim(test_connection, record$id, me$staff_id)$claimed) # who has it renews it
  expect_equal(extraction_model_get(test_connection, record$id)$editing$by_id, me$staff_id)
  expect_equal(extraction_model_get(test_connection, record$id)$revision, record$revision) # a claim isn't a save

  # a claim lasts as long as the caller says; a lapsed one can be taken
  expect_false(extraction_model_claim(test_connection, record$id, colleague$staff_id, stale_seconds = 3600)$claimed)
  expect_true(extraction_model_claim(test_connection, record$id, colleague$staff_id, stale_seconds = 0, now = Sys.time() + 5)$claimed)
  expect_false(extraction_model_claim(test_connection, record$id, me$staff_id)$claimed) # and is then the new person's
  expect_true(extraction_model_claim(test_connection, record$id, me$staff_id, take_over = TRUE)$claimed) # anyone can take over

  expect_false(extraction_model_release(test_connection, record$id, colleague$staff_id)) # not theirs to let go
  expect_true(extraction_model_release(test_connection, record$id, me$staff_id))
  expect_null(extraction_model_get(test_connection, record$id)$editing)
  expect_false(extraction_model_release(test_connection, record$id, me$staff_id))
  expect_error(extraction_model_claim(test_connection, "0", me$staff_id), "no longer in the database")
})

test_that("a pushed extraction can't be claimed, changed or discarded; a draft is discarded for good", {
  me <- local_staff()
  tree <- local_tree()
  pushed <- local_pushed_questionnaire(me, tree)
  expect_error(extraction_model_claim(test_connection, pushed$id, me$staff_id), "pushed already")
  expect_error(extraction_model_discard(test_connection, pushed$id), "pushed already")
  frozen <- utils::modifyList(pushed$record, list(info = list(wave_id = "9")))
  expect_error(extraction_model_save(test_connection, frozen, "info", me$staff_id), "pushed extraction")

  draft <- local_extraction(me)
  extraction_model_discard(test_connection, draft$id)
  expect_error(extraction_model_get(test_connection, draft$id), "no longer in the database")
  expect_error(extraction_model_discard(test_connection, draft$id), "no longer in the database")
})

test_that("an upload is kept in Storage and on this server, named by its hash, and fetched by a server without it", {
  dir <- local_pdf_cache()
  upload <- fake_upload()
  sha256 <- test_sha256()
  withr::defer(test_delete_pdf(sha256))
  path <- extraction_model_save_pdf(test_connection, upload$datapath, sha256, pages = 2L)
  expect_equal(path, file.path(dir, paste0(sha256, ".pdf")))
  expect_true(file.exists(path))
  expect_equal(database_model_count(test_connection, "pdf_file", list(pdf_sha256 = database_eq(sha256))), 1) # its row, with its pages and size
  writeLines("changed", path)
  extraction_model_save_pdf(test_connection, upload$datapath, sha256, pages = 2L) # the copy here isn't made twice, nor the row
  expect_equal(readLines(path), "changed")
  expect_equal(database_model_count(test_connection, "pdf_file", list(pdf_sha256 = database_eq(sha256))), 1)
  expect_equal(extraction_model_local_pdf(test_connection, sha256), path) # here already: not fetched

  # a server that hasn't got it (a hosted site that restarted) fetches it from Storage
  unlink(path)
  expect_equal(extraction_model_local_pdf(test_connection, sha256), path)
  expect_identical(readBin(path, "raw", 1e4), readBin(upload$datapath, "raw", 1e4))
  # one Storage never had, and a draft without one
  expect_null(extraction_model_local_pdf(test_connection, test_sha256()))
  expect_null(extraction_model_local_pdf(test_connection, NULL))
  expect_true(extraction_model_is_pdf(upload$datapath))
  expect_equal(extraction_model_page_count(upload$datapath), 2)
  expect_equal(extraction_model_sha256(upload$datapath), digest::digest(file = upload$datapath, algo = "sha256"))
})

test_that("a file that isn't a PDF is recognised, and a CSV is read as text", {
  csv <- fake_csv_upload(c("AH01", "AH02"))
  expect_false(extraction_model_is_pdf(csv$datapath))
  expect_equal(extraction_model_read_csv(csv$datapath), "AH01\nAH02")
  expect_null(extraction_model_page_count(csv$datapath))
})

test_that("a number is sent to the database as digits, never in scientific notation", {
  expect_equal(database_number(120000000), "120000000")
  expect_equal(database_number("100000000"), "100000000")
  expect_equal(database_number(7L), "7")
  expect_error(extraction_model_number("12abc"), "no draft with that number")
})

test_that("Storage's answers are read as found, missing, or a sentence that says what to do", {
  answer <- function(status, body) {
    httr2::response(status_code = status, headers = list(`Content-Type` = "application/json"), body = charToRaw(body))
  }
  missing <- answer(400, '{"statusCode":"404","error":"not_found","message":"Object not found"}')
  no_bucket <- answer(400, '{"statusCode":"404","error":"Bucket not found","message":"Bucket not found","code":"NoSuchBucket"}')
  expect_true(storage_is_missing(missing))
  expect_true(storage_is_missing(answer(404, "{}")))
  expect_false(storage_is_missing(no_bucket)) # the bucket is missing, not the file: that is a problem to report
  expect_match(storage_problem(no_bucket, "extraction-pdfs"), "has no bucket called extraction-pdfs")
  expect_match(storage_problem(answer(403, '{"message":"denied"}'), "extraction-pdfs"), "\\(403\\): denied")
})
