# The Edit Extraction page: one draft's questionnaire, changed in this session until Save writes it to the draft's
# reviewed copy in the database. Inside the page's server, `open()` is how the other pages hand it a draft.
# Names like `draft`, `record` and `selected` are the page's own, so the drafts these tests make are called
# `first`, `second` and so on. The database is the real one: each test makes the registry, study and
# questionnaire its draft is about.

edit_page <- function(me, extractions = test_extractions()) {
  list(extractions = extractions, catalog = test_live_catalog(), connection = test_connection, me = me)
}

# A draft that has been read, as a new wave of the tree's questionnaire. What it read is the first two parts
# of the gold questionnaire (short_gold_json(), as local_read_extraction() saves): the page draws every block
# of the draft it opens, and all of the gold questionnaire's take seconds to draw.
read_draft <- function(me, tree, env = parent.frame()) {
  local_read_extraction(me, info = picked_basic_info(tree), env = env)
}

# A draft with one block's words changed, as staff would change them.
reworded <- function(draft, words = "Do you worry a lot?", id = "c3") {
  draft_service_update(draft, "components", id, "text", words)
}

# The draft read_draft() makes as the page shows it: its questions numbered by their place.
numbered_gold <- function() draft_service_renumber_questions(short_gold_draft())

# Fills in the MATR IDs of the draft on the page, as a CSV would, so nothing is left to fix. Called
# inside testServer(), where `draft` and `change_draft` are the page's.
fill_all_matr_ids <- function(draft, change_draft) {
  change_draft(suppressMessages(draft_service_auto_apply_matr_ids(draft(), sprintf("m%03d", seq_len(nrow(draft()$variables))))), keep_checked = TRUE)
}

test_that("opening a draft loads its questionnaire, numbers its questions by their place, and heads the page", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    errors <- collect_errors(session)
    expect_true(open(first))
    expect_equal(record_id(), first$id)
    expect_identical(draft(), numbered_gold())
    expect_false(has_unsaved_changes()) # the numbering isn't a change of the person's
    questions <- draft()$components[draft()$components$component_type == "Question", ]
    expect_equal(sort(as.integer(questions$question_number)), seq_len(nrow(questions)))
    expect_equal(output$title, paste("Edit Extraction: ZZQ ·", tree$wave$name))
    expect_equal(output$subtitle, tree$questionnaire$name)
    expect_length(undo_stack(), 0)
    expect_null(selected())
    expect_length(errors(), 0)
  })
})

test_that("a change is kept in this session and can be undone, and Save is offered while there is one", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    session$flushReact()
    expect_true(last_update(seen, "save")$disabled)
    expect_true(last_update(seen, "undo")$disabled)
    expect_match(text_of(output$save_status), "All changes saved")

    expect_true(change_draft(reworded(draft())))
    session$flushReact()
    expect_true(has_unsaved_changes())
    expect_length(undo_stack(), 1)
    expect_false(last_update(seen, "save")$disabled)
    expect_false(last_update(seen, "undo")$disabled)
    expect_match(text_of(output$save_status), "Unsaved changes")

    session$setInputs(undo = 1)
    expect_identical(draft(), numbered_gold())
    expect_false(has_unsaved_changes())
    expect_length(undo_stack(), 0)
  })
})

test_that("Undo takes back one change at a time, only the last few are kept, and a selection that is gone goes too", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    for (i in seq_len(undo_depth + 5)) change_draft(reworded(draft(), paste("Words", i)))
    expect_length(undo_stack(), undo_depth)
    session$setInputs(undo = 1)
    expect_equal(component(draft(), "c3")$text, paste("Words", undo_depth + 4))
    session$setInputs(undo = 2)
    expect_equal(component(draft(), "c3")$text, paste("Words", undo_depth + 3))

    added <- draft_service_next_id(draft(), "components")
    change_draft(draft_service_add_block(draft(), "Instruction", added))
    selected(added)
    session$setInputs(undo = 3)
    expect_null(selected())
  })
})

test_that("typing doesn't rebuild the fields, but a pick or a button does; moving a question renumbers", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    before <- layout_version()
    change_draft(reworded(draft(), "Typed words"), typed = TRUE)
    expect_equal(layout_version(), before)
    change_draft(reworded(draft(), "Picked words"))
    expect_equal(layout_version(), before + 1L)
    change_draft(draft()) # nothing changed
    expect_equal(layout_version(), before + 1L)

    # the first question moved to the end takes the last number
    printed <- draft_service_printed_order(draft())
    questions <- printed[draft()$components$component_type[match(printed, draft()$components$id)] == "Question"]
    parents <- draft()$components$parent_id[match(printed, draft()$components$id)]
    moved <- c(setdiff(printed, questions[1]), questions[1])
    change_draft(draft_service_apply_order(draft(), moved, c(parents[printed != questions[1]], NA)))
    expect_equal(question_number_of(draft(), questions[1]), as.character(length(questions)))
    expect_equal(question_number_of(draft(), questions[2]), "1")
  })
})

test_that("a change that fails is explained, the draft stays as it was, and the fields are put back", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    before <- layout_version()
    expect_false(change_draft(draft_service_update(draft(), "components", "gone", "text", "x")))
    expect_equal(notice_texts(seen), "That's no longer in the draft.")
    expect_equal(seen$notifications[[1]]$type, "warning")
    expect_identical(draft(), numbered_gold())
    expect_equal(layout_version(), before + 1L)

    expect_true(change_draft({
      message("Cleared Printed as and Skip to.")
      reworded(draft())
    }))
    expect_equal(notice_texts(seen)[2], "Cleared Printed as and Skip to.")
    expect_equal(seen$notifications[[2]]$type, "message")
    expect_equal(component(draft(), "c3")$text, "Do you worry a lot?")
  })
})

test_that("Save writes the draft to the database (numbers included), and a save that fails is a sentence", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    change_draft(reworded(draft()))
    session$setInputs(save = 1)
    expect_false(has_unsaved_changes())
    stored <- draft_service_load(test_connection, first$id)
    expect_identical(stored, draft())
    expect_equal(component(stored, "c3")$text, "Do you worry a lot?")
    expect_equal(component(stored, "c3")$question_number, component(numbered_gold(), "c3")$question_number)
    expect_equal(notice_texts(seen), "Saved. Nothing is added to the MATR metadata database until its change request is approved.")
    session$flushReact()
    expect_true(last_update(seen, "save")$disabled)

    change_draft(reworded(draft(), "Once more"))
    test_delete_draft(first$id) # the draft has gone from the database
    session$setInputs(save = 2)
    expect_true(has_unsaved_changes())
    expect_match(notice_texts(seen), "Couldn't save: .*no longer in the database", all = FALSE)
  })
})

test_that("the next block with a problem is selected, going round to the first again", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    fill_all_matr_ids(draft, change_draft)
    change_draft(draft_service_update(draft(), "components", "c4", "text", ""))
    change_draft(draft_service_update(draft(), "components", "c9", "text", ""))
    expect_equal(unique(problems()$id), c("c4", "c9"))
    next_one <- function() {
      session$setInputs(action = list(action = "next_problem"))
      selected()
    }
    expect_equal(next_one(), "c4")
    expect_equal(next_one(), "c9")
    expect_equal(next_one(), "c4")
  })
})

test_that("opening another draft while there are unsaved changes asks first: Don't save leaves them, Save keeps them", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  second <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    change_draft(reworded(draft()))
    expect_true(open(second)) # asking is not failing
    expect_equal(record_id(), first$id)
    expect_match(seen$modals[[1]], paste0("Save your changes to ZZQ · ", tree$wave$name, "?"), fixed = TRUE)
    expect_equal(waiting()$id, second$id)

    session$setInputs(discard_and_open = 1)
    expect_equal(record_id(), second$id)
    expect_identical(draft(), numbered_gold())
    expect_false(has_unsaved_changes())
    expect_equal(component(draft_service_load(test_connection, first$id), "c3")$text, component(short_gold_draft(), "c3")$text)

    change_draft(reworded(draft()))
    open(first)
    session$setInputs(save_and_open = 1)
    expect_equal(record_id(), first$id)
    expect_equal(component(draft_service_load(test_connection, second$id), "c3")$text, "Do you worry a lot?")
    expect_equal(seen$closed, 2)

    # the same draft again does nothing, and keeps what was changed
    change_draft(reworded(draft(), "Again"))
    expect_true(open(first))
    expect_length(seen$modals, 2)
    expect_true(has_unsaved_changes())
  })
})

test_that("a save that fails when opening another draft keeps the first one open", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  second <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    change_draft(reworded(draft()))
    open(second)
    test_delete_draft(first$id) # the first has gone
    session$setInputs(save_and_open = 1)
    expect_equal(record_id(), first$id)
    expect_true(has_unsaved_changes())
  })
})

test_that("a draft that can't be opened is a sentence, and what was on screen stays", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  unread <- local_extraction(me, info = picked_basic_info(tree))
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    expect_false(open(unread))
    expect_equal(record_id(), first$id)
    expect_match(notice_texts(seen), "Couldn't open this draft: .*hasn't been read yet")
  })
})

test_that("the MATR IDs dialog explains itself, Auto Apply is off until there is a CSV, and it fills from the top", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  n_boxes <- nrow(short_gold_draft()$variables)
  ids <- sprintf("m%03d", seq_len(n_boxes))
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    errors <- collect_errors(session)
    open(first)
    session$setInputs(action = list(action = "matr_ids"))
    expect_match(seen$modals[[1]], "MATR IDs Upload a CSV that lists the MATR IDs in the order the variables appear")
    expect_false(grepl("earlier waves", seen$modals[[1]])) # the questionnaire prints nothing yet
    expect_match(text_of(output$matr_csv_status), "Drop the CSV of MATR IDs here")
    expect_equal(text_of(output$matr_fill_note), "")

    session$setInputs(matr_csv = fake_csv_upload(c("matr_id", ids), "ahp_ids.csv"))
    expect_equal(matr_ids(), ids)
    expect_equal(extraction_model_get(test_connection, first$id)$matr_csv$name, "ahp_ids.csv")
    expect_match(text_of(output$matr_csv_status), sprintf("ahp_ids.csv %d MATR IDs · m001 to m%03d", n_boxes, n_boxes))
    expect_match(text_of(output$matr_fill_note), sprintf("One MATR ID for each of the %d variables.", n_boxes))
    expect_false(last_update(seen, "auto_apply")$disabled)

    session$setInputs(auto_apply = 1)
    expect_equal(draft()$variables$matr_id[1:2], c("m001", "m002"))
    expect_true(has_unsaved_changes())
    expect_equal(seen$closed, 1)
    expect_length(errors(), 0)
    session$setInputs(undo = 1)
    expect_true(all(is.na(draft()$variables$matr_id) | draft()$variables$matr_id == short_gold_draft()$variables$matr_id))
  })
})

test_that("a CSV with the wrong number of IDs says so, and a CSV that can't be kept is a sentence", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    errors <- collect_errors(session)
    open(first)
    session$setInputs(matr_csv = fake_csv_upload(c("matr_id", "m001", "m002", "m003")))
    expect_length(matr_ids(), 3)
    expect_match(text_of(output$matr_fill_note), "Nothing is lost")
    change_draft(draft_service_update(draft(), "variables", "v1", "matr_id", "typed1"))
    session$flushReact()
    expect_match(text_of(output$matr_fill_note), "1 of the variables have a MATR ID")

    test_delete_draft(first$id)
    session$setInputs(matr_csv = fake_csv_upload(c("matr_id", "m001")))
    expect_match(notice_texts(seen), "Couldn't add the CSV: .*no longer in the database", all = FALSE)
    expect_length(errors(), 0)
  })
})

test_that("a new wave of a recorded questionnaire gets the MATR IDs its questions have, and can ask again", {
  me <- local_staff()
  admin <- local_staff("ADMIN")
  tree <- local_tree()
  seen <- local_browser()
  pushed <- local_pushed_questionnaire(admin, tree)
  # the same questionnaire typed again for its second wave, without MATR IDs
  second_wave <- do.call(local_manual_extraction, c(list(me), picked_basic_info(tree, pushed$questionnaire_id), list(env = environment())))
  draft_service_save(test_connection, second_wave$id, test_questionnaire_draft(matr = c(NA, NA)), me$staff_id)
  second_wave <- extraction_model_get(test_connection, second_wave$id)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    errors <- collect_errors(session)
    expect_true(open(second_wave))
    expect_setequal(draft()$variables$matr_id, pushed$draft$variables$matr_id)
    expect_match(notice_texts(seen), "Filled in the MATR IDs of 2 variables from the questionnaire's earlier waves", all = FALSE)
    expect_true(has_unsaved_changes()) # Save keeps them
    expect_length(undo_stack(), 1)
    # the recorded blocks' words are offered while typing
    expect_true(paste(test_marker, "Part A") %in% suggestions()$Section)

    # the dialog starts on the questionnaire it goes into; nothing is left to fill, and it says so
    session$setInputs(action = list(action = "matr_ids"))
    expect_match(utils::tail(seen$modals, 1)[[1]], "From a questionnaire")
    session$setInputs(matr_questionnaire = pushed$questionnaire_id, import_matr_ids = 1)
    expect_match(utils::tail(notice_texts(seen), 1), "Nothing to fill in")

    # a blank one filled again finds it
    change_draft(draft_service_update(draft(), "variables", "v1", "matr_id", ""))
    session$setInputs(import_matr_ids = 2)
    expect_equal(draft()$variables$matr_id[1], pushed$draft$variables$matr_id[1])
    expect_length(errors(), 0)
  })
})

test_that("a pick list the database couldn't give is a warning when the page starts, not a blank list", {
  me <- local_staff()
  seen <- local_browser()
  local_stub("lookup_model_get_subjects", function(connection) stop("The subject table is missing.", call. = FALSE))
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    expect_match(notice_texts(seen), "The list of subjects couldn't be read")
    expect_equal(seen$notifications[[1]]$type, "warning")
  })
})

test_that("editing a checked block takes the check off, and Undo brings it back", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    errors <- collect_errors(session)
    open(first)
    change_draft(with_checked(draft(), "c3"), keep_checked = TRUE) # its missing MATR IDs keep Verify off
    expect_equal(checked_blocks(draft()), "c3")
    session$flushReact()
    one_checked <- sprintf("%d%% human-reviewed", floor(100 / nrow(draft()$components)))
    expect_match(text_of(output$progress), one_checked)

    session$setInputs(`outline-edit` = list(table = "components", id = "c3", field = "text", value = "Do you worry a lot?", typed = TRUE))
    expect_equal(component(draft(), "c3")$text, "Do you worry a lot?")
    expect_length(checked_blocks(draft()), 0)
    session$flushReact()
    expect_match(text_of(output$progress), "0% human-reviewed")

    session$setInputs(undo = 1)
    expect_equal(checked_blocks(draft()), "c3")
    expect_equal(component(draft(), "c3")$text, component(short_gold_draft(), "c3")$text)
    expect_length(errors(), 0)
  })
})

test_that("a change to a checked block's variable or choices takes its check off; marking and filling a CSV don't", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  ids <- sprintf("m%03d", seq_len(nrow(short_gold_draft()$variables)))
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    change_draft(with_checked(draft(), c("c3", "c4", "c5")), keep_checked = TRUE)
    session$setInputs(`outline-edit` = list(table = "variables", id = "v1", field = "label", value = "Nervous"))
    expect_equal(checked_blocks(draft()), c("c4", "c5"))
    session$setInputs(`outline-action` = list(action = "add_choice", id = "v2"))
    expect_equal(checked_blocks(draft()), "c5")
    suppressMessages(session$setInputs(`outline-action` = list(action = "delete", table = "response_options", id = "o5")))
    expect_length(checked_blocks(draft()), 0)

    session$setInputs(`outline-action` = list(action = "check", id = "c3")) # it has no MATR ID yet: refused
    expect_length(checked_blocks(draft()), 0)

    change_draft(with_checked(draft(), c("c3", "c4")), keep_checked = TRUE)
    session$setInputs(matr_csv = fake_csv_upload(c("matr_id", ids)))
    session$setInputs(auto_apply = 1)
    expect_equal(draft()$variables$matr_id[1], "m001")
    expect_equal(checked_blocks(draft()), c("c3", "c4"))
    session$setInputs(`outline-edit` = list(table = "variables", id = "v1", field = "matr_id", value = "typed"))
    expect_equal(checked_blocks(draft()), "c4")
  })
})

test_that("a block moved into another section loses its check, and one moved in its section keeps it", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
  printed <- draft_service_printed_order(short_gold_draft())
  sent <- function(ids, parents) list(ids = as.list(ids), parents = as.list(ifelse(is.na(parents), "", parents)))
  parents <- short_gold_draft()$components$parent_id[match(printed, short_gold_draft()$components$id)]
  shiny::testServer(edit_extraction_server, args = edit_page(me), {
    open(first)
    change_draft(with_checked(draft(), c("c3", "c4")), keep_checked = TRUE)
    swapped <- printed
    swapped[match(c("c3", "c4"), printed)] <- c("c4", "c3")
    session$setInputs(`outline-order` = sent(swapped, parents[match(swapped, printed)]))
    expect_equal(checked_blocks(draft()), c("c3", "c4"))

    out <- parents
    out[match("c3", printed)] <- NA
    session$setInputs(`outline-order` = sent(printed, out))
    expect_equal(checked_blocks(draft()), "c4")
  })
})

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
  expect_match(html, '<span class="btn-text">MATR IDs</span>')
  expect_match(html, "fa-fingerprint")
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
claim_page <- function(me, seen, showing = shiny::reactiveVal(TRUE)) {
  c(edit_page(me), list(show_page = function(page) seen$pages <- c(seen$pages, page), active = showing))
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
    saved_by_colleague("Theirs")
    session$elapse(draft_claim_renew_seconds * 1000) # nobody has it now, so the page has it again
    expect_true(has_draft())
    expect_equal(draft()$components$text[draft()$components$id == "c3"], "Theirs") # nothing here was lost
    expect_equal(loaded_revision(), first$revision + 1)
    expect_match(notice_texts(seen), "saved this draft since you opened it", all = FALSE, fixed = TRUE)

    change_draft(reworded(draft(), "Mine"))
    saved_by_colleague("Theirs again")
    session$elapse(draft_claim_renew_seconds * 1000)
    expect_true(has_draft())
    expect_true(has_unsaved_changes()) # kept, and asked about
    expect_match(seen$modals[[length(seen$modals)]], "Someone saved this draft after you opened it", fixed = TRUE)
    session$setInputs(reopen = 1)
    expect_false(has_unsaved_changes())
    expect_equal(draft()$components$text[draft()$components$id == "c3"], "Theirs again")
    expect_equal(loaded_revision(), first$revision + 2)
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
    draft_service_save(test_connection, first$id, reworded(numbered_gold(), "Theirs"), colleague$staff_id)

    session$setInputs(save = 1)
    expect_true(has_unsaved_changes())
    expect_match(seen$modals[[length(seen$modals)]], "Someone saved this draft after you opened it", fixed = TRUE)

    session$setInputs(reopen = 1)
    expect_false(has_unsaved_changes())
    expect_equal(draft()$components$text[draft()$components$id == "c3"], "Theirs")
    expect_equal(loaded_revision(), first$revision + 1)

    expect_true(change_draft(reworded(draft(), "Mine after all")))
    session$setInputs(save = 2)
    expect_false(has_unsaved_changes())
    expect_equal(loaded_revision(), first$revision + 2)
  })
})

test_that("a change is saved by itself half a minute after the last one, quietly, and the header says when", {
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
    change_draft(reworded(draft(), "Do you worry a great deal?"))
    session$flushReact()
    session$elapse(autosave_delay * 1000 - 1000)
    expect_true(has_unsaved_changes()) # the wait starts again with each change
    session$elapse(1500)
    expect_false(has_unsaved_changes())
    expect_equal(component(draft_service_load(test_connection, first$id), "c3")$text, "Do you worry a great deal?")
    expect_length(seen$notifications, 0) # no "Saved." notice
    expect_match(text_of(output$save_status), "Saved just now")
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
    draft_service_save(test_connection, first$id, reworded(numbered_gold(), "Theirs"), colleague$staff_id)
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
