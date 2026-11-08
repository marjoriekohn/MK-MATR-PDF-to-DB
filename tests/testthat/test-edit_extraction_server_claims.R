# The Edit Extraction page, continued from test-edit_extraction_server.R (split in two so the halves run at once):
# its header, the PDF beside it, and one person at a time working on a draft. The database is the real one.

test_that("the header is the title and its buttons on one row, with the progress bar across the page under both", {
  html <- htmltools::renderTags(edit_extraction_ui("edit_extraction"))$html
  places <- vapply(c('class="review-title"', 'class="review-actions"', 'id="edit_extraction-save_status"', 'class="review-buttons"', 'id="edit_extraction-undo"', 'id="edit_extraction-request_button"', 'id="edit_extraction-progress"'), function(x) {
    as.integer(regexpr(x, html, fixed = TRUE))
  }, integer(1))
  expect_true(all(places > 0))
  expect_equal(order(places), seq_along(places))
  # the buttons are a group of their own after how saving stands, so on a phone they can share a row evenly
  # (_edit_extraction.scss); the toolbar's buttons and fields are full size, like the header's
  toolbar <- regmatches(html, regexpr('(?s)<div class="outline-toolbar">.*?<div class="outline-split">', html, perl = TRUE))
  expect_length(toolbar, 1)
  expect_false(grepl("btn-sm|form-control-sm|form-select-sm", toolbar))
  # each button is its icon and its words; on a phone only the icon shows (components/_buttons.scss)
  expect_match(html, '<span class="btn-text">Undo</span>')
  expect_false(grepl('<span class="btn-text">MATR IDs</span>', html, fixed = TRUE)) # reading gives them out
  expect_match(as.character(pdf_open_button("http://x/pdf")), '<span class="btn-text">Open PDF</span>')
})

test_that("the PDF opens in its own tab when the file is here, and the page is drawn with one panel", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  local_pdf_cache()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    expect_equal(text_of(output$pdf_link), "") # the test draft's PDF was never uploaded
  })
  html <- htmltools::renderTags(edit_extraction_ui("edit_extraction"))$html
  expect_match(html, 'id="edit_extraction-pdf_link"')
  expect_match(html, "structure-card")
  expect_false(grepl("pdf-card|review-details|pdf-pane|pdf_pane|iframe", html))
  expect_equal(lengths(regmatches(html, gregexpr('class="card ', html))), 1)
  button <- as.character(pdf_open_button("http://x/pdf"))
  expect_match(button, 'href="http://x/pdf"')
  expect_match(button, 'target="_blank"')
  expect_false(grepl("data-pdf-at-picked", button))
  expect_null(pdf_open_button(NULL))
  # on Edit Extraction it opens at the picked block's page (www/js/review.js)
  expect_match(as.character(pdf_open_button("http://x/pdf", at_picked = TRUE)), "data-pdf-at-picked")
})

test_that("Continue shows the PDF that was uploaded with the draft, even on a server that hasn't got it", {
  me <- local_staff()
  seen <- local_browser()
  local_pdf_cache()
  typed <- extraction_service_create_manual(test_connection, me, upload = fake_upload("typed.pdf"))
  withr::defer(test_delete_pdf(typed$pdf$sha256)) # after the draft, which points at it
  withr::defer(test_delete_draft(typed$id))
  unlink(typed$pdf$path) # as a hosted site that restarted, or a session on another of its servers
  # a test's stand-in session can't serve files (it has no registerDataObj), so the address is made up
  local_stub("pdf_address", function(session, name, data) paste0("session/pdf/", data$sha256))
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(extraction_model_get(test_connection, typed$id))
    expect_match(text_of(output$pdf_link), "Open PDF")
    expect_match(output$pdf_link$html, 'target="_blank"')
    expect_true(file.exists(typed$pdf$path)) # fetched from Storage
  })
})

# ------ One person at a time -------------------------------------------------------------------------
# Who has the draft in the database now: their staff id, or NULL.
editing_now <- function(record) extraction_model_get(test_connection, record$id)$editing$by_id

# The page's arguments, with where it records the pages it shows and whether it is the one showing.
claim_page <- function(me, seen, showing = shiny::reactiveVal(TRUE), env = parent.frame()) {
  c(edit_page(me, env = env), list(show_page = function(page) seen$pages <- c(seen$pages, page), active = showing))
}

test_that("opening a draft claims it, leaving the page with nothing unsaved lets it go, and coming back has it again", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  showing <- shiny::reactiveVal(TRUE)
  shiny::testServer(edit_extraction_server, args = claim_page(me, seen, showing), {
    errors <- collect_errors(session)
    expect_true(open(first))
    session$flushReact() # as the site starts: whether the page is showing is known before it changes
    expect_true(has_draft())
    expect_equal(editing_now(first), me$staff_id)
    expect_equal(loaded_revision(), first$revision)

    showing(FALSE)
    session$flushReact()
    expect_null(editing_now(first))
    expect_equal(claim()$why, "left")

    showing(TRUE)
    session$flushReact()
    expect_true(has_draft())
    expect_equal(editing_now(first), me$staff_id)

    # with something unsaved, leaving keeps it: they are still at work on it
    change_draft(reworded(draft()))
    showing(FALSE)
    session$flushReact()
    expect_true(has_draft())
    expect_equal(editing_now(first), me$staff_id)
    expect_length(errors(), 0)
  })
  expect_null(editing_now(first)) # the session ended: let go
})

test_that("a draft someone else is working on asks whether to take it over, and taking it over leaves them only looking", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(colleague, tree)
  draft_row_service_claim(test_connection, first$id, colleague)
  shiny::testServer(edit_extraction_server, args = claim_page(me, seen), {
    expect_false(open(first))
    expect_null(record_id())
    expect_match(seen$modals[[length(seen$modals)]], paste(colleague$name, "is working on this draft"), fixed = TRUE)

    session$setInputs(take_over = 1)
    expect_equal(record_id(), first$id)
    expect_true(has_draft())
    expect_equal(seen$pages, "edit_extraction")
    expect_equal(editing_now(first), me$staff_id)
    expect_false(draft_row_service_claim(test_connection, first$id, colleague)$claimed)
  })
})

test_that("once someone else takes the draft over, the page only shows it: nothing can be changed or saved", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = claim_page(me, seen), {
    open(first)
    expect_true(change_draft(reworded(draft())))
    draft_row_service_claim(test_connection, first$id, colleague, take_over = TRUE)
    session$elapse(draft_claim_renew_seconds * 1000) # the page renews its claim, and finds it gone
    expect_equal(claim()$why, "taken")
    expect_equal(claim()$by, colleague$name)
    expect_false(has_draft())
    expect_match(notice_texts(seen), paste(colleague$name, "took over this draft"), all = FALSE, fixed = TRUE)
    expect_match(text_of(output$banner), paste(colleague$name, "is working on this draft"), fixed = TRUE)
    expect_true(last_update(seen, "save")$disabled)

    before <- draft()
    expect_false(change_draft(reworded(draft(), "Changed anyway?")))
    expect_identical(draft(), before)
    expect_false(save_draft())
    expect_equal(editing_now(first), colleague$staff_id)

    # the banner's Take over asks, then has it again
    session$setInputs(take_over_banner = 1)
    expect_match(seen$modals[[length(seen$modals)]], paste(colleague$name, "is working on this draft"), fixed = TRUE)
    session$setInputs(take_over = 1)
    expect_true(has_draft())
    expect_true(has_unsaved_changes()) # nobody saved meanwhile, so the changes on screen are kept
    expect_equal(editing_now(first), me$staff_id)
  })
})

test_that("doing nothing for a while lets the draft go, and doing something has it again", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = claim_page(me, seen), {
    open(first)
    last_active(Sys.time() - draft_claim_idle_seconds - 1)
    session$elapse(draft_claim_renew_seconds * 1000)
    expect_equal(claim()$why, "idle")
    expect_null(editing_now(first))
    expect_match(text_of(output$banner), "You were away")

    session$setInputs(active = 1) # www/js/review.js: they moved the mouse
    expect_true(has_draft())
    expect_equal(editing_now(first), me$staff_id)
  })
})

test_that("a draft someone else saved within the minute is shown as they saved it, unless changes here would be lost", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  # the colleague takes it over, saves and lets it go again, all before the page renews its claim
  saved_by_colleague <- function(words) {
    draft_row_service_claim(test_connection, first$id, colleague, take_over = TRUE)
    draft_service_save(test_connection, first$id, reworded(numbered_gold(), words), colleague$staff_id)
    draft_row_service_release(test_connection, first$id, colleague)
  }
  shiny::testServer(edit_extraction_server, args = claim_page(me, seen), {
    open(first)
    saved_by_colleague("theirs")
    session$elapse(draft_claim_renew_seconds * 1000) # nobody has it now, so the page has it again
    expect_true(has_draft())
    expect_equal(draft()$components$text[draft()$components$id == "c3"], "theirs") # nothing here was lost
    expect_equal(loaded_revision(), first$revision + 1)
    expect_match(notice_texts(seen), "saved this draft since you opened it", all = FALSE, fixed = TRUE)

    change_draft(reworded(draft(), "Mine"))
    saved_by_colleague("theirs again")
    session$elapse(draft_claim_renew_seconds * 1000)
    expect_true(has_draft())
    expect_true(has_unsaved_changes()) # kept, and asked about
    expect_match(seen$modals[[length(seen$modals)]], "Someone saved this draft after you opened it", fixed = TRUE)
    asked <- function() sum(grepl("Your changes here can't be saved over theirs", unlist(seen$modals), fixed = TRUE))
    expect_equal(asked(), 1)
    session$elapse(draft_claim_renew_seconds * 1000)
    expect_equal(asked(), 1) # not asked again about the same save, over whatever dialog is open now
    saved_by_colleague("theirs once more")
    session$elapse(draft_claim_renew_seconds * 1000)
    expect_equal(asked(), 2) # a later save is asked about
    session$setInputs(reopen = 1)
    expect_false(has_unsaved_changes())
    expect_equal(draft()$components$text[draft()$components$id == "c3"], "theirs once more")
    expect_equal(loaded_revision(), first$revision + 3)
  })
})

test_that("Save is refused if someone saved the draft after the page opened it, and the saved version can be opened instead", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = claim_page(me, seen), {
    open(first)
    change_draft(reworded(draft()))
    # a save that didn't go through this page (Change Requests' Basic Info, say)
    draft_service_save(test_connection, first$id, reworded(numbered_gold(), "theirs"), colleague$staff_id)

    session$setInputs(save = 1)
    expect_true(has_unsaved_changes())
    expect_match(seen$modals[[length(seen$modals)]], "Someone saved this draft after you opened it", fixed = TRUE)

    session$setInputs(reopen = 1)
    expect_false(has_unsaved_changes())
    expect_equal(draft()$components$text[draft()$components$id == "c3"], "theirs")
    expect_equal(loaded_revision(), first$revision + 1)

    expect_true(change_draft(reworded(draft(), "Mine after all")))
    session$setInputs(save = 2)
    expect_false(has_unsaved_changes())
    expect_equal(loaded_revision(), first$revision + 2)
  })
})

test_that("a change is saved by itself a minute after the last one, quietly, and the header says when", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    session$flushReact()
    change_draft(reworded(draft()))
    session$flushReact() # as after each change staff make
    session$elapse(autosave_delay * 1000 - 1000)
    expect_true(has_unsaved_changes()) # still typing, as far as the page knows
    change_draft(reworded(draft(), "do you worry a great deal?"))
    session$flushReact()
    session$elapse(autosave_delay * 1000 - 1000)
    expect_true(has_unsaved_changes()) # the wait starts again with each change
    session$elapse(1500)
    expect_false(has_unsaved_changes())
    expect_equal(component(draft_service_load(test_connection, first$id), "c3")$text, "do you worry a great deal?")
    expect_length(seen$notifications, 0) # no "Saved." notice
    expect_match(text_of(output$save_status), "Saved just now")
  })
})

test_that("staff still at work on the site put off the save by itself until they have done nothing for a minute", {
  me <- local_staff()
  tree <- shared_tree()
  local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    session$flushReact()
    change_draft(reworded(draft()))
    session$flushReact()
    session$elapse(autosave_delay * 1000 - 1000)
    session$setInputs(active = 1) # a key pressed, or the pointer moved, without changing the draft
    session$elapse(autosave_delay * 1000 - 1000)
    expect_true(has_unsaved_changes())
    selected("c3") # a block picked
    session$flushReact()
    session$elapse(autosave_delay * 1000 - 1000)
    expect_true(has_unsaved_changes())
    session$elapse(1500)
    expect_false(has_unsaved_changes())
  })
})

test_that("Re-extract reads the PDF again in place of the questionnaire, lets the draft go and shows the reading", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  local_pdf_cache()
  first <- read_draft(me, tree)
  writeLines("%PDF-1.4", extraction_model_pdf_path(first$pdf$sha256)) # its PDF, on this server
  shown <- NULL
  args <- c(claim_page(me, seen), list(open_reading = function(record) shown <<- record))
  shiny::testServer(edit_extraction_server, args = args, {
    open(first)
    session$flushReact()
    expect_match(output$reextract_button$html, "Re-extract")
    change_draft(reworded(draft()))
    expect_true(save_draft(quietly = TRUE))
    session$setInputs(reextract = 1)
    expect_match(seen$modals[[length(seen$modals)]], "Re-extract this questionnaire?", fixed = TRUE)
    session$setInputs(confirm_reextract = 1)
    expect_null(record_id())
    expect_false(has_unsaved_changes())
    expect_equal(shown$id, first$id)
    expect_equal(shown$extraction_status, "queued")
    expect_null(draft_model_get_both(test_connection, first$id)$questionnaire)
  })
})

test_that("a draft typed by hand has no Re-extract: it has no reading to do again", {
  me <- local_staff()
  local_browser()
  typed <- local_manual_extraction(me)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(typed)
    session$flushReact()
    expect_false(grepl("Re-extract", paste(output$reextract_button$html, collapse = "")))
  })
})

test_that("after a save is refused, only Save tries again", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = claim_page(me, seen), {
    open(first)
    session$flushReact()
    draft_service_save(test_connection, first$id, reworded(numbered_gold(), "theirs"), colleague$staff_id)
    change_draft(reworded(draft()))
    session$flushReact() # as after each change staff make
    session$elapse(autosave_delay * 1000 + 500)
    expect_true(has_unsaved_changes())
    expect_match(seen$modals[[length(seen$modals)]], "Someone saved this draft after you opened it", fixed = TRUE)
    refused <- function() sum(grepl("Saving now would overwrite their changes", unlist(seen$modals), fixed = TRUE))
    expect_equal(refused(), 1)
    change_draft(reworded(draft(), "Mine, again"))
    session$flushReact()
    session$elapse(autosave_delay * 1000 + 500)
    # not tried again by itself (the claim renewed each minute may say on its own that someone else saved it)
    expect_equal(refused(), 1)
    expect_match(text_of(output$save_status), "Save to keep them")
  })
})

test_that("how long ago the draft was saved is said in seconds, then minutes, then the time", {
  at <- as.POSIXct("2026-10-21 14:05:00", tz = "UTC")
  expect_equal(saved_ago(at, at + 3), "Saved just now")
  expect_equal(saved_ago(at, at + 47), "Saved 45 s ago")
  expect_equal(saved_ago(at, at + 4 * 60 + 10), "Saved 4 min ago")
  expect_equal(saved_ago(at, at + 2 * 3600), "Saved at 2:05 PM")
})
