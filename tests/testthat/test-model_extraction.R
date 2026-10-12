# An extraction draft: who started it, how it was made, how far reading it has got and what staff typed about
# it, kept in the database (a draft of kind Extraction with its extracted_draft row).
#
# test_that("an extraction is saved to the database and read back as a record", {
#   local_pdf_cache()
#   me <- local_staff()
#   saved <- local_extraction(me)
#
#   expect_match(saved$id, "^[0-9]+$")
#   expect_equal(saved$kind, "Extraction")
#   expect_equal(saved$method, "AI-Powered")
#   expect_equal(saved$prepared_by, me$name)
#   expect_equal(saved$prepared_by_id, me$staff_id)
#   expect_equal(saved$started, "2026-10-02T10:00:00Z") # the time it was given
#   expect_equal(saved$status, "Draft")
#   expect_equal(saved$extraction_status, "queued")
#   expect_equal(saved$info$wave_name, "Wave 3")
#   expect_equal(saved$pdf$name, "AHP_wave3.pdf")
#   expect_equal(saved$pdf$pages, 2)
#   expect_equal(saved$pdf$path, extraction_model_pdf_path(saved$pdf$sha256))
#   expect_null(saved$error)
#   expect_null(saved$reading_pid)
#   expect_null(saved$matr_csv)
#   expect_equal(extraction_model_get(test_connection, saved$id), saved)
# })
#
# test_that("a draft typed by hand has no reading and keeps what staff said about it", {
#   me <- local_staff()
#   saved <- local_manual_extraction(me)
#   expect_equal(saved$method, "Manual")
#   expect_null(saved$extraction_status)
#   expect_null(saved$pdf$name)
#   expect_null(saved$pdf$path)
#   expect_equal(saved$info$wave_name, "Wave 3")
# })
#
# test_that("records are read newest saved first, named by id, and only extractions", {
#   me <- local_staff()
#   first <- local_extraction(me)
#   second <- local_extraction(me)
#   edit <- local_edit(me)
#   records <- extraction_model_get_all(test_connection)
#   ours <- names(records)[names(records) %in% c(first$id, second$id)]
#   expect_equal(ours, c(second$id, first$id))
#   expect_false(edit$id %in% names(records))
#   expect_error(extraction_model_get(test_connection, edit$id), "no longer in the database")
#   expect_error(extraction_model_get(test_connection, "0"), "no longer in the database")
# })
#
# test_that("saving writes only the fields that changed, so what the reading job and staff change don't undo each other", {
#   me <- local_staff()
#   record <- local_extraction(me)
#
#   # the reading job says how far it has got
#   expect_true(extraction_model_save_reading(test_connection, record$id, list(extraction_status = "sent", error = "Slow"), record$reading_started))
#
#   # staff change what they typed, from a copy of the record read before the job's news
#   record$info$wave_name <- "Wave 4"
#   saved <- extraction_model_save(test_connection, record, "info", me$staff_id)
#   expect_equal(saved$info$wave_name, "Wave 4")
#   expect_equal(saved$extraction_status, "sent") # the job's news is still there
#   expect_equal(saved$error, "Slow")
#   expect_equal(saved$revision, 2)
#   expect_equal(saved$saved_by_id, me$staff_id)
#
#   # and the job's next news doesn't undo what staff typed
#   extraction_model_save_reading(test_connection, record$id, list(extraction_status = "received"), record$reading_started)
#   again <- extraction_model_get(test_connection, record$id)
#   expect_equal(again$extraction_status, "received")
#   expect_equal(again$info$wave_name, "Wave 4")
#   expect_equal(again$revision, 2) # the job's progress isn't a change to the content
# })
#
# test_that("the fields of the shared row and of the details are saved together", {
#   me <- local_staff()
#   colleague <- local_staff()
#   record <- local_extraction(me)
#   record$summary <- "Add the wave"
#   record$note <- "Why"
#   record$requested_reviewer <- list(id = colleague$staff_id)
#   record$info$notes <- "page 6 is scanned"
#   saved <- extraction_model_save(test_connection, record, c("summary", "note", "requested_reviewer", "info"), me$staff_id)
#   expect_equal(saved$summary, "Add the wave")
#   expect_equal(saved$note, "Why")
#   expect_equal(saved$requested_reviewer, list(id = colleague$staff_id, name = colleague$name))
#   expect_equal(saved$info$notes, "page 6 is scanned")
#
#   sent <- utils::modifyList(saved, list(status = "Awaiting Review", submitted = "2026-10-02T12:00:00Z"))
#   sent <- extraction_model_save(test_connection, sent, c("status", "submitted"), me$staff_id, expected_revision = saved$revision, new_revision = FALSE)
#   expect_equal(sent$status, "Awaiting Review")
#   expect_equal(sent$submitted, "2026-10-02T12:00:00Z")
#   expect_equal(sent$revision, saved$revision) # sending isn't a change to the content
#
#   back <- utils::modifyList(sent, list(status = "Sent Back", sent_back = list(by = colleague$name, by_id = colleague$staff_id, date = "2026-10-02T13:00:00Z", note = "Check it")))
#   back <- extraction_model_save(test_connection, back, c("status", "sent_back"), colleague$staff_id, new_revision = FALSE)
#   expect_equal(back$sent_back, list(by = colleague$name, by_id = colleague$staff_id, date = "2026-10-02T13:00:00Z", note = "Check it"))
# })
#
# test_that("a save of a version someone has since changed is refused", {
#   me <- local_staff()
#   record <- local_extraction(me)
#   extraction_model_save(test_connection, record, "info", me$staff_id)
#   record$info$wave_name <- "Mine"
#   expect_error(extraction_model_save(test_connection, record, "info", me$staff_id, expected_revision = record$revision), "changed after you opened it")
# })
#
# test_that("the reading job can only save for the reading it was started for, and only the columns it owns", {
#   me <- local_staff()
#   record <- local_extraction(me)
#   expect_false(extraction_model_save_reading(test_connection, record$id, list(extraction_status = "ready"), "2026-10-02T09:00:00Z"))
#   expect_equal(extraction_model_get(test_connection, record$id)$extraction_status, "queued")
#   expect_true(extraction_model_save_reading(test_connection, record$id, list(extraction_status = "ready", model = "gemini-x", prompt_version = "abc"), "2026-10-02T10:00:00Z"))
#   back <- extraction_model_get(test_connection, record$id)
#   expect_equal(back$extraction_status, "ready")
#   expect_equal(back$model, "gemini-x")
#   expect_equal(back$prompt_version, "abc")
#   expect_error(extraction_model_save_reading(test_connection, record$id, list(info = list()), "2026-10-02T10:00:00Z"), "can't save info")
# })
#
# test_that("only one site can claim a draft for reading", {
#   me <- local_staff()
#   record <- local_extraction(me)
#   expect_false(extraction_model_claim_reading(test_connection, record$id, "2026-10-02T09:00:00Z")) # a reading since restarted
#   expect_true(extraction_model_claim_reading(test_connection, record$id, "2026-10-02T10:00:00Z", now = as.POSIXct("2026-10-02 10:00:05", tz = "UTC")))
#   expect_false(extraction_model_claim_reading(test_connection, record$id, "2026-10-02T10:00:00Z"))
#   expect_equal(extraction_model_get(test_connection, record$id)$reading_claimed_at, "2026-10-02T10:00:05Z")
# })
#
# test_that("a draft's signature changes as it is saved and as it is read", {
#   me <- local_staff()
#   before <- extraction_model_signature(test_connection)
#   record <- local_extraction(me)
#   added <- extraction_model_signature(test_connection)
#   expect_false(added == before)
#   extraction_model_save_reading(test_connection, record$id, list(extraction_status = "sent"), record$reading_started)
#   expect_false(extraction_model_signature(test_connection) == added)
# })

test_that("an upload is kept once, named by its hash, in the PDF cache", {
  dir <- local_pdf_cache()
  upload <- fake_upload()
  path <- extraction_model_save_pdf(upload$datapath, "feed")
  expect_equal(path, file.path(dir, "feed.pdf"))
  expect_true(file.exists(path))
  writeLines("changed", path)
  extraction_model_save_pdf(upload$datapath, "feed") # the same file isn't stored twice
  expect_equal(readLines(path), "changed")
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
