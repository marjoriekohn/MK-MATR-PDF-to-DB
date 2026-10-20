# The questionnaire of an extraction draft: three big jsonb columns of the extracted_draft row, kept apart from the
# rest of the record. The AI's answer is written once by the reading job; staff's copy and the record of their edits
# are written when they save.

test_that("what the AI returned and what staff saved are different columns, and staff's copy wins when there is one", {
  me <- local_staff()
  record <- local_extraction(me)
  expect_null(draft_model_get(test_connection, record$id)) # not read yet

  expect_true(draft_model_save_reading(test_connection, record$id, list(questionnaire = list(name = "AI")), record$reading_started))
  expect_equal(draft_model_get(test_connection, record$id)$questionnaire$name, "AI")

  draft_model_save_reviewed(test_connection, record$id, list(questionnaire = list(name = "Reviewed")), me$staff_id)
  expect_equal(draft_model_get(test_connection, record$id)$questionnaire$name, "Reviewed")
  original <- draft_model_get_record(test_connection, record$id)$original
  expect_equal(original$questionnaire$name, "AI") # never changed by saving
})

test_that("the reading job's answer is only kept for the reading it was started for", {
  me <- local_staff()
  record <- local_extraction(me)
  expect_false(draft_model_save_reading(test_connection, record$id, list(a = 1), "2026-10-02T09:00:00Z"))
  expect_null(draft_model_get(test_connection, record$id))
  expect_true(draft_model_save_reading(test_connection, record$id, list(a = 1), record$reading_started))
  expect_equal(draft_model_get(test_connection, record$id)$a, 1)
})

test_that("saving staff's copy raises the revision, says who saved it, and keeps the edits unless it is given new ones", {
  me <- local_staff()
  record <- local_extraction(me)
  edits <- list(list(at = "2026-10-02T10:00:00Z", by = me$staff_id, change = "edited", item = "block", field = "text", before = "a", after = "b"))
  # (the database keeps a jsonb object's keys in its own order, so the fields are compared by name)
  same_edits <- function(found) {
    expect_length(found, 1)
    expect_equal(found[[1]][names(edits[[1]])], edits[[1]])
  }
  draft_model_save_reviewed(test_connection, record$id, list(x = 1), me$staff_id, edits)
  same_edits(draft_model_get_record(test_connection, record$id)$edits)

  draft_model_save_reviewed(test_connection, record$id, list(x = 2), me$staff_id) # edits left as they are
  same_edits(draft_model_get_record(test_connection, record$id)$edits)
  expect_equal(extraction_model_get(test_connection, record$id)$revision, 3)

  draft_model_save_reviewed(test_connection, record$id, list(x = 3), me$staff_id, list())
  expect_equal(draft_model_get_record(test_connection, record$id)$edits, list())
})

test_that("a manual draft can't hold an AI's answer or a record of edits to it", {
  me <- local_staff()
  record <- local_manual_extraction(me)
  expect_error(
    draft_model_save_reviewed(test_connection, record$id, list(x = 1), me$staff_id, list(list(change = "edited"))),
    "extracted_draft_manual_has_no_ai"
  )
})

test_that("a draft that isn't there is a sentence", {
  expect_error(draft_model_get(test_connection, "0"), "no longer in the database")
  expect_error(draft_model_get_record(test_connection, "0"), "no longer in the database")
})
