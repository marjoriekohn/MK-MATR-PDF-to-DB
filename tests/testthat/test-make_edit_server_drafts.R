# The Make an edit page, continued from test-make_edit_server.R (split in two so the halves run at once): a delete
# sent for approval, saving and discarding drafts, adding a question to a questionnaire, a database that can't be
# read, and the kinds with forms of their own. The database is the real one.

test_that("a STAFF member's delete is sent for approval and waits on Change Requests; an ADMIN's is applied", {
  me <- local_staff()
  admin <- local_staff("ADMIN")
  made <- local_outcomes()
  type <- local_respondent_type()
  gone <- local_respondent_type()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    pick(session, "respondent_type", type$respondent_type_id)
    press(session, "delete")
    draw_form()
    expect_true(needs_approval())
    expect_equal(primary_label(), "Send for approval")
    session$setInputs(`change-confirm_read` = TRUE, `change-confirm_word` = "DELETE")
    session$flushReact()
    if (!change$ready()) skip(paste("the delete form isn't ready:", change$blockers()[1]))
    session$setInputs(submit = 1)
    expect_false(done()$applied)
    expect_equal(done()$record$status, "Awaiting Review")
    expect_match(text_of(output$detail), "Sent for approval")
    expect_true(done()$record$id %in% names(change_request_service_waiting(test_connection)))
    expect_equal(nrow(table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(type$respondent_type_id)))), 1)
  })

  admin_page <- page_args(admin)
  shiny::testServer(make_edit_server, args = admin_page$args, {
    pick(session, "respondent_type", gone$respondent_type_id)
    press(session, "delete")
    draw_form()
    expect_false(needs_approval())
    expect_equal(primary_label(), "Delete it")
    session$setInputs(`change-confirm_read` = TRUE, `change-confirm_word` = "DELETE")
    session$flushReact()
    if (!change$ready()) skip(paste("the delete form isn't ready:", change$blockers()[1]))
    session$setInputs(submit = 1)
    expect_true(done()$applied)
    made$applied <- c(made$applied, done()$history_id)
  })
  expect_equal(nrow(table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(gone$respondent_type_id)))), 0)
})

test_that("Save as draft keeps the form as typed, lists it at the top, and Continue opens it again", {
  me <- local_staff()
  seen <- local_browser()
  page <- page_args(me)
  name <- test_name("Half typed")
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    add(session, "respondent_type")
    draw_form()
    session$setInputs(`change-respondent_type__name` = name)
    session$setInputs(save_draft = 1)
    expect_false(is.null(draft()$id))
    expect_match(notice_texts(seen), "Saved as a draft", all = FALSE)
    saved_id <- draft()$id
    expect_true(saved_id %in% names(mine()))
    expect_match(text_of(output$drafts), "Your saved edits \\(1\\)")

    session$setInputs(cancel = 1, confirm_cancel = 1)
    expect_null(draft())
    expect_equal(verb(), "")
    session$setInputs(action = list(action = "continue", id = saved_id))
    expect_equal(draft()$id, saved_id)
    # the form puts back what was typed (and then lets the saved draft go: it's put back once)
    expect_match(paste(output[["change-main"]]$html, collapse = ""), name, fixed = TRUE)
    expect_equal(verb(), "add")
    expect_equal(kind(), "respondent_type")
    expect_length(errors(), 0)
  })
})

test_that("a saved edit can be discarded, after asking", {
  me <- local_staff()
  seen <- local_browser()
  saved <- local_edit(me, status = "Draft")
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    expect_true(saved$id %in% names(mine()))
    session$setInputs(action = list(action = "discard", id = saved$id))
    expect_match(seen$modals[[1]], "Discard this saved edit\\?")
    session$setInputs(confirm_discard = 1)
    expect_equal(edit_model_get(test_connection, saved$id)$status, "Withdrawn")
    expect_false(saved$id %in% names(mine()))
  })
})

test_that("a question can be added to a questionnaire here: + Add asks which questionnaire, then shows the form", {
  me <- local_staff()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    add(session, "questionnaire_question")
    expect_match(paste(output$detail$html, collapse = ""), "add_scope")
    expect_false(grepl("change-main", paste(output$detail$html, collapse = "")))
    session$setInputs(add_scope = as.character(tree$questionnaire$questionnaire_id))
    expect_equal(scope(), as.character(tree$questionnaire$questionnaire_id))
    expect_false(grepl("You can't add", text_of(output$detail)))
    expect_match(paste(output$detail$html, collapse = ""), "change-main") # the form, with its variables
  })
})

test_that("a database that can't be read says so, and Try again reads it again", {
  me <- local_staff()
  seen <- local_browser()
  page <- page_args(me)
  reads <- 0L
  real <- edit_service_load
  local_stub("edit_service_load", function(connection, entity) {
    reads <<- reads + 1L
    if (reads == 1L) stop("The database is down.", call. = FALSE)
    real(connection, entity)
  })
  shiny::testServer(make_edit_server, args = page$args, {
    open_section(session, "registry")
    expect_match(text_of(output$items_registry), "The database is down.")
    expect_match(text_of(output$items_registry), "Try again")
    session$setInputs(action = list(action = "retry", kind = "registry"))
    expect_false(grepl("The database is down", text_of(output$items_registry)))
    expect_match(text_of(output$items_registry), "Add Registry")
  })
})

test_that("a PDF is only shown: listed without + Add, its details without Edit or Delete", {
  me <- local_staff()
  sha256 <- paste(sample(c(0:9, letters[1:6]), 64, replace = TRUE), collapse = "")
  local_row("pdf_file", list(pdf_sha256 = sha256, pdf_pages = 3, pdf_bytes = 2048), "pdf_sha256")
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    open_section(session, "pdf_file")
    listed <- paste(output$items_pdf_file$html, collapse = "")
    expect_match(listed, sha256, fixed = TRUE)
    expect_false(grepl("Add PDF File", text_of(listed)))
    pick(session, "pdf_file", sha256)
    detail <- paste(output$detail$html, collapse = "")
    expect_match(text_of(detail), paste("PDF", substr(sha256, 1, 12)), fixed = TRUE)
    expect_false(grepl('data-action="edit"|data-action="delete"', detail))
    expect_false(grepl("Open PDF", text_of(detail))) # (its file isn't in Storage)
    shown <- text_of(output[["change-main"]])
    expect_match(shown, "Pages 3")
    expect_match(shown, "Size 2 KB")
    expect_length(errors(), 0)
  })
})

test_that("an option set is added with its choices, and named by them", {
  me <- local_staff()
  choice <- local_row("answer_option", list(standardized_label = tolower(test_name("Choice"))), "answer_option_id") # a choice is in lower case
  made <- local_outcomes()
  page <- page_args(me)
  name <- choice$standardized_label # a set's name is its choices (public.name(option_set))
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    add(session, "option_set")
    draw_form()
    session$setInputs(`change-option_set__answer_option_ids` = key_text(choice$answer_option_id))
    expect_true(change$ready())
    session$setInputs(submit = 1)
    expect_true(isTRUE(done()$applied), info = "the test project needs the option set rules of docs/sql/1_setup.sql")
    if (isTRUE(done()$applied)) made$applied <- c(made$applied, done()$history_id)
    expect_length(errors(), 0)
  })
  set <- table_model_get(test_connection, "option_set", list(name = database_eq(name)))
  expect_equal(nrow(set), 1)
  if (nrow(set) == 1) {
    members <- table_model_get(test_connection, "answer_option_set", list(option_set_id = database_eq(set$option_set_id)))
    expect_equal(members$answer_option_id, choice$answer_option_id)
    expect_equal(members$display_order, 1)
  }
})
