# The Edit Extraction page: one draft's questionnaire, changed in this session until Save writes it to the draft's
# reviewed copy in the database. Inside the page's server, `open()` is how the other pages hand it a draft.
# Names like `draft`, `record` and `selected` are the page's own, so the drafts these tests make are called
# `first`, `second` and so on. The database is the real one: each test makes the registry, study and
# questionnaire its draft is about.

# The header, the PDF and one person at a time are in test-edit_extraction_server_claims.R; what both share is in
# helper-edit_extraction_page.R.

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
    for (i in seq_len(undo_depth + 5)) change_draft(reworded(draft(), paste("words", i)))
    expect_length(undo_stack(), undo_depth)
    session$setInputs(undo = 1)
    expect_equal(component(draft(), "c3")$text, paste("words", undo_depth + 4))
    session$setInputs(undo = 2)
    expect_equal(component(draft(), "c3")$text, paste("words", undo_depth + 3))

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
    expect_equal(component(draft(), "c3")$text, "do you worry a lot?")
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
    expect_equal(component(stored, "c3")$text, "do you worry a lot?")
    expect_equal(component(stored, "c3")$question_number, component(numbered_gold(), "c3")$question_number)
    expect_equal(notice_texts(seen), "Saved. Nothing is added to the MATR metadata database until you push it.")
    session$flushReact()
    expect_true(last_update(seen, "save")$disabled)

    change_draft(reworded(draft(), "Once more"))
    test_delete_extraction(first$id) # the draft has gone from the database
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
    expect_equal(component(draft_service_load(test_connection, second$id), "c3")$text, "do you worry a lot?")
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
    test_delete_extraction(first$id) # the first has gone
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

test_that("a new wave of a recorded questionnaire gets the MATR IDs its questions have", {
  me <- local_staff()
  admin <- local_staff("ADMIN")
  tree <- local_tree()
  seen <- local_browser()
  pushed <- local_pushed_questionnaire(admin, tree)
  # the same questionnaire typed again for its second wave, without MATR IDs
  second_wave <- do.call(local_manual_extraction, c(list(me), picked_basic_info(tree, pushed$questionnaire_id), list(env = environment())))
  draft_service_save(test_connection, second_wave$id, test_questionnaire_draft(matr = c(NA, NA)), me$staff_id)
  second_wave <- extraction_model_get(test_connection, second_wave$id)
  shiny::testServer(edit_extraction_server, args = edit_page(me, catalog = test_live_catalog()), {
    errors <- collect_errors(session)
    expect_true(open(second_wave))
    expect_setequal(draft()$variables$matr_id, pushed$draft$variables$matr_id)
    expect_match(notice_texts(seen), "Filled in the MATR IDs of 2 variables from the questionnaire's earlier waves", all = FALSE)
    expect_true(has_unsaved_changes()) # Save keeps them
    expect_length(undo_stack(), 1)
    # the recorded blocks' words are offered while typing
    expect_true(paste(test_marker, "Part A") %in% suggestions()$Section)
    expect_length(errors(), 0)
  })
})

test_that("a pick list the database couldn't give is a warning when the page starts, not a blank list", {
  me <- local_staff()
  seen <- local_browser()
  page <- edit_page(me)
  # after edit_page(), so this stub wins over the file's shared lookups
  local_stub("lookup_model_get_tags", function(connection) stop("The tag table is missing.", call. = FALSE))
  shiny::testServer(edit_extraction_server, args = page, {
    expect_match(notice_texts(seen), "The list of subjects, time frames and units couldn't be read")
    expect_equal(seen$notifications[[1]]$type, "warning")
  })
})

# ------ Previewing and pushing ------------------------------------------------------------------------
test_that("Preview and push saves what is unsaved, and lists what is in the way: blocks to fix, blank Basic Info, a wave that asks questions", {
  me <- local_staff()
  admin <- local_staff("ADMIN")
  tree <- local_tree()
  seen <- local_browser()
  pushed <- local_pushed_questionnaire(admin, tree) # so the tree's wave asks questions already
  first <- read_draft(me, tree)
  shiny::testServer(edit_extraction_server, args = push_page(me, seen, catalog = test_live_catalog()), {
    errors <- collect_errors(session)
    open(first)
    change_draft(reworded(draft()))
    session$setInputs(request = 1)
    expect_false(has_unsaved_changes()) # saved first
    expect_equal(component(draft_service_load(test_connection, first$id), "c3")$text, "do you worry a lot?")
    expect_match(seen$modals[[length(seen$modals)]], "Push to the database")
    expect_match(seen$modals[[length(seen$modals)]], "Basic Info")
    expect_equal(last_update(seen, "wave_id")$selected, as.character(tree$wave$wave_id)) # the form is filled from the draft
    # the form's drop-downs are blank until the browser fills them: the Basic Info is in the way, and the blocks to fix
    session$elapse(600)
    blockers <- text_of(output$push_blockers)
    expect_match(blockers, "Before it can be pushed")
    expect_match(blockers, "Fix the .* blocks marked to fix")
    expect_match(blockers, "Fill in the registry, study, questionnaire and wave under Basic Info")
    expect_match(text_of(output$push_blocker), "^Fix the .* blocks marked to fix")
    expect_true(last_update(seen, "push")$disabled)
    expect_null(dry_run())

    # the MATR IDs filled in, and the tree's wave picked, which asks questions since the push into it
    fill_all_matr_ids(draft, change_draft)
    pick_basic_info(session, picked_basic_info(tree))
    blockers <- text_of(output$push_blockers)
    expect_no_match(blockers, "marked to fix|under Basic Info")
    expect_match(blockers, paste(tree$wave$name, "already asks 2 questions. Empty the wave first (Metadata page)"), fixed = TRUE)
    expect_true(last_update(seen, "push")$disabled)
    expect_null(dry_run())

    session$setInputs(push_cancel = 1)
    expect_false(push_open())
    expect_equal(record_id(), first$id)
    expect_equal(extraction_model_get(test_connection, first$id)$status, "Draft")
    expect_length(errors(), 0)
  })
})

test_that("with nothing in the way the dialog shows what the push adds, and Push adds it, says so and goes to Extraction Drafts", {
  me <- local_staff()
  tree <- local_tree()
  seen <- local_browser()
  typed <- do.call(local_manual_extraction, c(list(me), picked_basic_info(tree), list(env = environment())))
  draft_service_save(test_connection, typed$id, test_questionnaire_draft(), me$staff_id)
  typed <- extraction_model_get(test_connection, typed$id)
  shiny::testServer(edit_extraction_server, args = push_page(me, seen, catalog = test_live_catalog()), {
    errors <- collect_errors(session)
    open(typed)
    reworded_question <- draft()$components$id[draft()$components$printed_number %in% "1"]
    change_draft(draft_service_update(draft(), "components", reworded_question, "text", paste(test_marker, "Do you smoke now?")))
    session$setInputs(request = 1)
    expect_false(has_unsaved_changes())
    pick_basic_info(session, picked_basic_info(tree))
    expect_equal(text_of(output$push_blockers), "")
    # the dry run: what the push would add, without adding it
    expect_false(inherits(dry_run(), "error"))
    expect_equal(dry_run()$questions, 2)
    what <- text_of(output$push_what)
    expect_match(what, "What it adds")
    expect_match(what, "Adds 4 new blocks, 2 questions asked in the wave, 2 new variables")
    expect_match(seen$modals[[length(seen$modals)]], "4 blocks haven't been marked as checked yet")
    expect_length(questionnaire_service_wave_question_counts(test_connection, tree$questionnaire$questionnaire_id), 0)
    expect_false(last_update(seen, "push")$disabled)
    expect_equal(extraction_model_get(test_connection, typed$id)$status, "Draft")

    session$setInputs(push = 1)
    after <- extraction_model_get(test_connection, typed$id)
    expect_equal(after$status, "Pushed")
    expect_null(after$editing) # let go
    expect_equal(questionnaire_service_wave_question_counts(test_connection, tree$questionnaire$questionnaire_id)[[as.character(tree$wave$wave_id)]], 2L)
    printed <- extraction_service_recorded(test_connection, list(info = picked_basic_info(tree)))$texts$text
    expect_true(tolower(paste(test_marker, "Do you smoke now?")) %in% tolower(printed)) # as reworded on the page, saved before the push
    expect_match(notice_texts(seen), "^Pushed\\. Adds 4 new blocks", all = FALSE)
    expect_equal(seen$pages, "drafts")
    expect_null(record_id())
    expect_false(push_open())
    expect_gte(seen$closed, 1)
    expect_length(errors(), 0)
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

    session$setInputs(`outline-edit` = list(table = "components", id = "c3", field = "text", value = "do you worry a lot?", typed = TRUE))
    expect_equal(component(draft(), "c3")$text, "do you worry a lot?")
    expect_length(checked_blocks(draft()), 0)
    session$flushReact()
    expect_match(text_of(output$progress), "0% human-reviewed")

    session$setInputs(undo = 1)
    expect_equal(checked_blocks(draft()), "c3")
    expect_equal(component(draft(), "c3")$text, component(short_gold_draft(), "c3")$text)
    expect_length(errors(), 0)
  })
})

test_that("a change to a checked block's variable or choices takes its check off; marking and filling MATR IDs don't", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  first <- read_draft(me, tree)
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
    fill_all_matr_ids(draft, change_draft) # as the earlier waves' are filled in
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

test_that("how far the checking has got is a progress bar the browser draws, its width not set by the server", {
  html <- htmltools::renderTags(structure_progress_ui(list(checked = 3, blocks = 4)))$html
  expect_match(html, '<progress[^>]*value="75"[^>]*max="100"')
  expect_match(html, "75% human-reviewed")
  expect_no_match(html, "style=")
})
