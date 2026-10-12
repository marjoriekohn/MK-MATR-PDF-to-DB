# # The review page: one draft's questionnaire, changed in this session until Save writes it to the draft's
# # reviewed copy in the database. Inside the page's server, `open()` is how the other pages hand it a draft.
# # Names like `draft`, `record` and `selected` are the page's own, so the drafts these tests make are called
# # `first`, `second` and so on. The database is the real one: each test makes the registry, study and
# # questionnaire its draft is about.
#
# review_page <- function(me, extractions = test_extractions()) {
#   list(extractions = extractions, catalog = test_live_catalog(), connection = test_connection, me = me)
# }
#
# # A draft that has been read (the hand-checked questionnaire), as a new wave of the tree's questionnaire.
# read_draft <- function(me, tree, wave_name = "Wave 3", env = parent.frame()) {
#   local_read_extraction(me, info = typed_new_wave(tree, wave_name = wave_name), env = env)
# }
#
# # A draft with one block's words changed, as staff would change them.
# reworded <- function(draft, words = "Do you worry a lot?", id = "c3") {
#   draft_service_update(draft, "components", id, "text", words)
# }
#
# # The gold questionnaire as the page shows it: its questions numbered by their place.
# numbered_gold <- function() draft_service_renumber_questions(gold_draft())
#
# # Fills in the MATR IDs of the draft on the page, as a CSV would, so nothing is left to fix. Called
# # inside testServer(), where `draft` and `change_draft` are the page's.
# fill_all_matr_ids <- function(draft, change_draft) {
#   change_draft(draft_service_fill_matr_ids(draft(), sprintf("m%03d", seq_len(nrow(draft()$variables)))), keep_checked = TRUE)
# }
#
# test_that("opening a draft loads its questionnaire, numbers its questions by their place, and heads the page", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     errors <- collect_errors(session)
#     expect_true(open(first))
#     expect_equal(record_id(), first$id)
#     expect_identical(draft(), numbered_gold())
#     expect_false(has_unsaved_changes()) # the numbering isn't a change of the person's
#     questions <- draft()$components[draft()$components$component_type == "Question", ]
#     expect_equal(sort(as.integer(questions$question_number)), seq_len(nrow(questions)))
#     expect_equal(output$title, "Review ZZQ · Wave 3")
#     expect_equal(output$subtitle, tree$questionnaire$name)
#     expect_length(undo_stack(), 0)
#     expect_null(selected())
#     expect_length(errors(), 0)
#   })
# })
#
# test_that("a change is kept in this session and can be undone, and Save is offered while there is one", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     session$flushReact()
#     expect_true(last_update(seen, "save")$disabled)
#     expect_true(last_update(seen, "undo")$disabled)
#     expect_match(text_of(output$save_status), "All changes saved")
#
#     expect_true(change_draft(reworded(draft())))
#     session$flushReact()
#     expect_true(has_unsaved_changes())
#     expect_length(undo_stack(), 1)
#     expect_false(last_update(seen, "save")$disabled)
#     expect_false(last_update(seen, "undo")$disabled)
#     expect_match(text_of(output$save_status), "Unsaved changes")
#
#     session$setInputs(undo = 1)
#     expect_identical(draft(), numbered_gold())
#     expect_false(has_unsaved_changes())
#     expect_length(undo_stack(), 0)
#   })
# })
#
# test_that("Undo takes back one change at a time, only the last few are kept, and a selection that is gone goes too", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     for (i in seq_len(undo_depth + 5)) change_draft(reworded(draft(), paste("Words", i)))
#     expect_length(undo_stack(), undo_depth)
#     session$setInputs(undo = 1)
#     expect_equal(component(draft(), "c3")$text, paste("Words", undo_depth + 4))
#     session$setInputs(undo = 2)
#     expect_equal(component(draft(), "c3")$text, paste("Words", undo_depth + 3))
#
#     added <- draft_service_next_id(draft(), "components")
#     change_draft(draft_service_add_block(draft(), "Instruction", added))
#     selected(added)
#     session$setInputs(undo = 3)
#     expect_null(selected())
#   })
# })
#
# test_that("typing doesn't rebuild the fields, but a pick or a button does; moving a question renumbers", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     before <- layout_version()
#     change_draft(reworded(draft(), "Typed words"), typed = TRUE)
#     expect_equal(layout_version(), before)
#     change_draft(reworded(draft(), "Picked words"))
#     expect_equal(layout_version(), before + 1L)
#     change_draft(draft()) # nothing changed
#     expect_equal(layout_version(), before + 1L)
#
#     # the first question moved to the end takes the last number
#     printed <- draft_service_printed_order(draft())
#     questions <- printed[draft()$components$component_type[match(printed, draft()$components$id)] == "Question"]
#     parents <- draft()$components$parent_id[match(printed, draft()$components$id)]
#     moved <- c(setdiff(printed, questions[1]), questions[1])
#     change_draft(draft_service_apply_order(draft(), moved, c(parents[printed != questions[1]], NA)))
#     expect_equal(question_number_of(draft(), questions[1]), as.character(length(questions)))
#     expect_equal(question_number_of(draft(), questions[2]), "1")
#   })
# })
#
# test_that("a change that fails is explained, the draft stays as it was, and the fields are put back", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     before <- layout_version()
#     expect_false(change_draft(draft_service_update(draft(), "components", "gone", "text", "x")))
#     expect_equal(notice_texts(seen), "That's no longer in the draft.")
#     expect_equal(seen$notifications[[1]]$type, "warning")
#     expect_identical(draft(), numbered_gold())
#     expect_equal(layout_version(), before + 1L)
#
#     expect_true(change_draft({
#       message("Cleared Printed as and Skip to.")
#       reworded(draft())
#     }))
#     expect_equal(notice_texts(seen)[2], "Cleared Printed as and Skip to.")
#     expect_equal(seen$notifications[[2]]$type, "message")
#     expect_equal(component(draft(), "c3")$text, "Do you worry a lot?")
#   })
# })
#
# test_that("Save writes the draft to the database (numbers included), and a save that fails is a sentence", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     change_draft(reworded(draft()))
#     session$setInputs(save = 1)
#     expect_false(has_unsaved_changes())
#     stored <- draft_service_load(test_connection, first$id)
#     expect_identical(stored, draft())
#     expect_equal(component(stored, "c3")$text, "Do you worry a lot?")
#     expect_equal(component(stored, "c3")$question_number, component(numbered_gold(), "c3")$question_number)
#     expect_equal(notice_texts(seen), "Saved. Nothing is added to the MATR data until the draft is pushed.")
#     session$flushReact()
#     expect_true(last_update(seen, "save")$disabled)
#
#     change_draft(reworded(draft(), "Once more"))
#     test_delete_draft(first$id) # the draft has gone from the database
#     session$setInputs(save = 2)
#     expect_true(has_unsaved_changes())
#     expect_match(notice_texts(seen), "Couldn't save: .*no longer in the database", all = FALSE)
#   })
# })
#
# test_that("the next block with a problem is selected, going round to the first again", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     fill_all_matr_ids(draft, change_draft)
#     change_draft(draft_service_update(draft(), "components", "c4", "text", ""))
#     change_draft(draft_service_update(draft(), "components", "c9", "text", ""))
#     expect_equal(unique(problems()$id), c("c4", "c9"))
#     next_one <- function() {
#       session$setInputs(action = list(action = "next_problem"))
#       selected()
#     }
#     expect_equal(next_one(), "c4")
#     expect_equal(next_one(), "c9")
#     expect_equal(next_one(), "c4")
#   })
# })
#
# test_that("opening another draft while there are unsaved changes asks first: Don't save leaves them, Save keeps them", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   second <- read_draft(me, tree, wave_name = "Wave 4")
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     change_draft(reworded(draft()))
#     expect_true(open(second)) # asking is not failing
#     expect_equal(record_id(), first$id)
#     expect_match(seen$modals[[1]], "Save your changes to ZZQ · Wave 3\\?")
#     expect_equal(waiting()$id, second$id)
#
#     session$setInputs(discard_and_open = 1)
#     expect_equal(record_id(), second$id)
#     expect_identical(draft(), numbered_gold())
#     expect_false(has_unsaved_changes())
#     expect_equal(component(draft_service_load(test_connection, first$id), "c3")$text, component(gold_draft(), "c3")$text)
#
#     change_draft(reworded(draft()))
#     open(first)
#     session$setInputs(save_and_open = 1)
#     expect_equal(record_id(), first$id)
#     expect_equal(component(draft_service_load(test_connection, second$id), "c3")$text, "Do you worry a lot?")
#     expect_equal(seen$closed, 2)
#
#     # the same draft again does nothing, and keeps what was changed
#     change_draft(reworded(draft(), "Again"))
#     expect_true(open(first))
#     expect_length(seen$modals, 2)
#     expect_true(has_unsaved_changes())
#   })
# })
#
# test_that("a save that fails when opening another draft keeps the first one open", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   second <- read_draft(me, tree, wave_name = "Wave 4")
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     change_draft(reworded(draft()))
#     open(second)
#     test_delete_draft(first$id) # the first has gone
#     session$setInputs(save_and_open = 1)
#     expect_equal(record_id(), first$id)
#     expect_true(has_unsaved_changes())
#   })
# })
#
# test_that("a draft that can't be opened is a sentence, and what was on screen stays", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   unread <- local_extraction(me, info = typed_new_wave(tree, wave_name = "Wave 5"))
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     expect_false(open(unread))
#     expect_equal(record_id(), first$id)
#     expect_match(notice_texts(seen), "Couldn't open this draft: .*hasn't been read yet")
#   })
# })
#
# test_that("the MATR IDs dialog explains itself, Fill is off until there is a CSV, and a CSV fills from the top", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   n_boxes <- nrow(gold_draft()$variables)
#   ids <- sprintf("m%03d", seq_len(n_boxes))
#   shiny::testServer(review_server, args = review_page(me), {
#     errors <- collect_errors(session)
#     open(first)
#     session$setInputs(action = list(action = "matr_ids"))
#     expect_match(seen$modals[[1]], "MATR IDs A CSV lists MATR IDs in the order they appear on the questionnaire")
#     expect_false(grepl("earlier waves", seen$modals[[1]])) # the questionnaire prints nothing yet
#     expect_match(text_of(output$matr_csv_status), "Drop the CSV of MATR IDs here")
#     expect_equal(text_of(output$matr_fill_note), "")
#
#     session$setInputs(matr_csv = fake_csv_upload(c("matr_id", ids), "ahp_ids.csv"))
#     expect_equal(matr_ids(), ids)
#     expect_equal(extraction_model_get(test_connection, first$id)$matr_csv$name, "ahp_ids.csv")
#     expect_match(text_of(output$matr_csv_status), sprintf("ahp_ids.csv %d MATR IDs · m001 to m%03d", n_boxes, n_boxes))
#     expect_match(text_of(output$matr_fill_note), sprintf("One MATR ID for each of the %d variables.", n_boxes))
#     expect_false(last_update(seen, "fill_matr_ids")$disabled)
#
#     session$setInputs(fill_matr_ids = 1)
#     expect_equal(draft()$variables$matr_id[1:2], c("m001", "m002"))
#     expect_true(has_unsaved_changes())
#     expect_equal(seen$closed, 1)
#     expect_length(errors(), 0)
#     session$setInputs(undo = 1)
#     expect_true(all(is.na(draft()$variables$matr_id) | draft()$variables$matr_id == gold_draft()$variables$matr_id))
#   })
# })
#
# test_that("a CSV with the wrong number of IDs says so, and a CSV that can't be kept is a sentence", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     errors <- collect_errors(session)
#     open(first)
#     session$setInputs(matr_csv = fake_csv_upload(c("matr_id", "m001", "m002", "m003")))
#     expect_length(matr_ids(), 3)
#     expect_match(text_of(output$matr_fill_note), "Check the outline against the CSV before filling")
#     change_draft(draft_service_update(draft(), "variables", "v1", "matr_id", "typed1"))
#     session$flushReact()
#     expect_match(text_of(output$matr_fill_note), "This replaces the 1 MATR ID already filled in. Undo brings them back.")
#
#     test_delete_draft(first$id)
#     session$setInputs(matr_csv = fake_csv_upload(c("matr_id", "m001")))
#     expect_match(notice_texts(seen), "Couldn't add the CSV: .*no longer in the database", all = FALSE)
#     expect_length(errors(), 0)
#   })
# })
#
# test_that("a new wave of a recorded questionnaire gets the MATR IDs its questions have, and can ask again", {
#   me <- local_staff()
#   admin <- local_staff("ADMIN")
#   tree <- local_tree()
#   seen <- local_browser()
#   pushed <- local_pushed_questionnaire(admin, tree)
#   # the same questionnaire typed again for its second wave, without MATR IDs
#   second_wave <- do.call(local_manual_extraction, c(list(me), typed_new_wave(tree, pushed$questionnaire_id), list(env = environment())))
#   draft_service_save(test_connection, second_wave$id, test_questionnaire_draft(matr = c(NA, NA)), me$staff_id)
#   second_wave <- extraction_model_get(test_connection, second_wave$id)
#   shiny::testServer(review_server, args = review_page(me), {
#     errors <- collect_errors(session)
#     expect_true(open(second_wave))
#     expect_setequal(draft()$variables$matr_id, pushed$draft$variables$matr_id)
#     expect_match(notice_texts(seen), "Filled in the MATR IDs of 2 variables from the questionnaire's earlier waves", all = FALSE)
#     expect_true(has_unsaved_changes()) # Save keeps them
#     expect_length(undo_stack(), 1)
#     # the recorded blocks' words are offered while typing
#     expect_true(paste(test_marker, "Part A") %in% suggestions()$Section)
#
#     # nothing left to fill: the dialog's button says so
#     session$setInputs(action = list(action = "matr_ids"))
#     expect_match(utils::tail(seen$modals, 1)[[1]], "Fill from the earlier waves")
#     session$setInputs(fill_recorded = 1)
#     expect_match(utils::tail(notice_texts(seen), 1), "Nothing to fill in")
#
#     # a question reworded loses the match; a blank one filled again finds it
#     change_draft(draft_service_update(draft(), "variables", "v1", "matr_id", ""))
#     session$setInputs(fill_recorded = 2)
#     expect_equal(draft()$variables$matr_id[1], pushed$draft$variables$matr_id[1])
#     expect_length(errors(), 0)
#   })
# })
#
# test_that("the About dialog opens on the draft's details, and Save writes them to the draft", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     session$setInputs(edit_info = 1)
#     expect_match(seen$modals[[1]], "About the questionnaire & wave")
#     expect_equal(last_update(seen, "wave_name")$value, "Wave 3") # (the form is a module, so the id is its own)
#
#     session$setInputs(
#       `info-kind` = "new_wave", `info-registry_id` = as.character(tree$registry$registry_id),
#       `info-study_id` = as.character(tree$study$study_id),
#       `info-questionnaire_id` = as.character(tree$questionnaire$questionnaire_id), `info-wave_name` = "Wave 5"
#     )
#     session$setInputs(save_info = 1)
#     expect_equal(extraction_model_get(test_connection, first$id)$info$wave_name, "Wave 5")
#     expect_equal(seen$closed, 1)
#     expect_equal(notice_texts(seen), "Questionnaire and wave details saved.")
#     expect_equal(output$title, "Review ZZQ · Wave 5")
#     expect_false(has_unsaved_changes()) # the details aren't part of the questionnaire
#   })
# })
#
# test_that("a pick list the database couldn't give is a warning when the page starts, not a blank list", {
#   me <- local_staff()
#   seen <- local_browser()
#   local_stub("lookup_model_get_subjects", function(connection) stop("The subject table is missing.", call. = FALSE))
#   shiny::testServer(review_server, args = review_page(me), {
#     expect_match(notice_texts(seen), "The list of subjects couldn't be read")
#     expect_equal(seen$notifications[[1]]$type, "warning")
#   })
# })
#
# test_that("editing a checked block takes the check off, and Undo brings it back", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     errors <- collect_errors(session)
#     open(first)
#     session$setInputs(`outline-action` = list(action = "check", id = "c3"))
#     expect_equal(checked_blocks(draft()), "c3")
#     session$flushReact()
#     one_checked <- sprintf("Structure %d%% complete", floor(100 / nrow(draft()$components)))
#     expect_match(text_of(output$progress), one_checked)
#
#     session$setInputs(`outline-edit` = list(table = "components", id = "c3", field = "text", value = "Do you worry a lot?", typed = TRUE))
#     expect_equal(component(draft(), "c3")$text, "Do you worry a lot?")
#     expect_length(checked_blocks(draft()), 0)
#     session$flushReact()
#     expect_match(text_of(output$progress), "Structure 0% complete")
#
#     session$setInputs(undo = 1)
#     expect_equal(checked_blocks(draft()), "c3")
#     expect_equal(component(draft(), "c3")$text, component(gold_draft(), "c3")$text)
#     expect_length(errors(), 0)
#   })
# })
#
# test_that("a change to a checked block's variable or choices takes its check off; marking and filling a CSV don't", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   ids <- sprintf("m%03d", seq_len(nrow(gold_draft()$variables)))
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     change_draft(with_checked(draft(), c("c3", "c4", "c5")), keep_checked = TRUE)
#     session$setInputs(`outline-edit` = list(table = "variables", id = "v1", field = "label", value = "Nervous"))
#     expect_equal(checked_blocks(draft()), c("c4", "c5"))
#     session$setInputs(`outline-action` = list(action = "add_choice", id = "v2"))
#     expect_equal(checked_blocks(draft()), "c5")
#     suppressMessages(session$setInputs(`outline-action` = list(action = "delete", table = "response_options", id = "o5")))
#     expect_length(checked_blocks(draft()), 0)
#
#     session$setInputs(`outline-action` = list(action = "check", id = "c3"))
#     session$setInputs(`outline-action` = list(action = "check", id = "c3"))
#     expect_length(checked_blocks(draft()), 0)
#
#     change_draft(with_checked(draft(), c("c3", "c4")), keep_checked = TRUE)
#     session$setInputs(matr_csv = fake_csv_upload(c("matr_id", ids)))
#     session$setInputs(fill_matr_ids = 1)
#     expect_equal(draft()$variables$matr_id[1], "m001")
#     expect_equal(checked_blocks(draft()), c("c3", "c4"))
#     session$setInputs(`outline-edit` = list(table = "variables", id = "v1", field = "matr_id", value = "typed"))
#     expect_equal(checked_blocks(draft()), "c4")
#   })
# })
#
# test_that("a block moved into another section loses its check, and one moved in its section keeps it", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   first <- read_draft(me, tree)
#   printed <- draft_service_printed_order(gold_draft())
#   sent <- function(ids, parents) list(ids = as.list(ids), parents = as.list(ifelse(is.na(parents), "", parents)))
#   parents <- gold_draft()$components$parent_id[match(printed, gold_draft()$components$id)]
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     change_draft(with_checked(draft(), c("c3", "c4")), keep_checked = TRUE)
#     swapped <- printed
#     swapped[match(c("c3", "c4"), printed)] <- c("c4", "c3")
#     session$setInputs(`outline-order` = sent(swapped, parents[match(swapped, printed)]))
#     expect_equal(checked_blocks(draft()), c("c3", "c4"))
#
#     out <- parents
#     out[match("c3", printed)] <- NA
#     session$setInputs(`outline-order` = sent(printed, out))
#     expect_equal(checked_blocks(draft()), "c4")
#   })
# })
#
# test_that("the PDF opens in its own window when the file is here, and the page is drawn with one panel", {
#   me <- local_staff()
#   tree <- local_tree()
#   seen <- local_browser()
#   local_pdf_cache()
#   first <- read_draft(me, tree)
#   shiny::testServer(review_server, args = review_page(me), {
#     open(first)
#     expect_equal(text_of(output$pdf_link), "") # the test draft's PDF was never stored here
#   })
#   html <- htmltools::renderTags(review_ui("review"))$html
#   expect_match(html, 'id="review-pdf_link"')
#   expect_match(html, "structure-card")
#   expect_false(grepl("pdf-card|review-details", html))
#   expect_equal(lengths(regmatches(html, gregexpr('class="card ', html))), 1)
#   expect_match(as.character(pdf_open_button("http://x/pdf")), 'href="http://x/pdf"')
#   expect_null(pdf_open_button(NULL))
# })
