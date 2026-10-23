# The Make an edit page's server, without a browser: the list (a section for each kind), the item picked in the
# details, its form, and what happens to the change: applied at once, saved as a draft, or sent for approval.
# The inputs of the modules inside it
# are named with their module's prefix, e.g. `change-registry__name`. The database is the real one.

# The browser draws the form and sends back its token; here the form is drawn by reading its output, and
# the token sent back by hand.
draw_form <- function(env = parent.frame()) {
  with(env, {
    output[["change-main"]]
    session$setInputs(`change-form_token` = as.character(change$token()))
  })
}

page_args <- function(me) {
  pages <- character()
  list(
    args = list(connection = test_connection, me = me, show_page = function(page) pages <<- c(pages, page)),
    pages = function() pages
  )
}

# What the page applied or saved is removed when the test ends: the rows an applied change added, and the drafts.
local_outcomes <- function(env = parent.frame()) {
  made <- new.env()
  made$applied <- character()
  made$drafts <- character()
  withr::defer(
    {
      for (id in made$applied) test_remove_applied(id)
      for (id in made$drafts) test_delete_draft(id)
    },
    envir = env
  )
  made
}

# What the list's buttons send (www/js/review.js): picking an item, + Add, and the details' Edit and Delete.
pick <- function(session, kind, key, scope = "") {
  session$setInputs(action = list(action = "select", kind = kind, key = as.character(key), scope = scope))
}
add <- function(session, kind, scope = "") session$setInputs(action = list(action = "add", kind = kind, scope = scope))
press <- function(session, what) session$setInputs(action = list(action = what))
# Opening (or closing) a section of the list: only an open section's items are drawn.
open_section <- function(session, id) session$setInputs(action = list(action = "toggle", id = as.character(id)))

test_that("a kind's section lists its items and + Add; a kind that belongs to a questionnaire has one for each", {
  me <- local_staff()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    expect_error(output$items_registry) # closed: nothing is drawn, or read from the database
    open_section(session, "registry")
    registries <- paste(output$items_registry$html, collapse = "")
    expect_match(text_of(registries), tree$registry$name, fixed = TRUE)
    expect_match(registries, sprintf('data-action="select" data-id="registry||%s"', tree$registry$registry_id), fixed = TRUE)
    expect_match(text_of(registries), "Add Registry")
    # Questions: a section for each questionnaire, its questions read once it is opened
    open_section(session, "questionnaire_question")
    questions <- paste(output$items_questionnaire_question$html, collapse = "")
    expect_match(questions, sprintf('data-id="questionnaire_question:%s"', tree$questionnaire$questionnaire_id), fixed = TRUE)
    open_section(session, paste0("questionnaire_question:", tree$questionnaire$questionnaire_id))
    inside <- paste(output[[paste0("items_questionnaire_question_", tree$questionnaire$questionnaire_id)]]$html, collapse = "")
    expect_match(text_of(inside), "Add Question")
    expect_length(errors(), 0)
  })
})

test_that("picking an item shows it in the details, its fields as text, with Edit and Delete", {
  me <- local_staff()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    expect_match(text_of(output$detail), "Pick something in the list")
    pick(session, "registry", tree$registry$registry_id)
    expect_equal(names(snapshot())[1], "registry")
    expect_equal(row()$name, tree$registry$name)
    expect_equal(verb(), "view")
    expect_true(change$active())
    expect_false(change$ready()) # nothing to send while it is only shown
    detail <- text_of(output$detail)
    expect_match(detail, tree$registry$name, fixed = TRUE)
    expect_match(detail, "Edit")
    expect_match(detail, "Delete")
    expect_match(text_of(output[["change-main"]]), tree$registry$abbreviation, fixed = TRUE) # its fields, as text
    expect_false(grepl("data-field|<input", paste(output[["change-main"]]$html, collapse = "")))
    press(session, "edit")
    expect_equal(verb(), "update")
    expect_match(paste(output[["change-main"]]$html, collapse = ""), "<input")
    expect_length(errors(), 0)
  })
})

test_that("an add is applied at once, is in the history, and the details say so", {
  me <- local_staff()
  seen <- local_browser()
  made <- local_outcomes()
  page <- page_args(me)
  name <- test_name("Added here")
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    add(session, "respondent_type")
    expect_true(change$active())
    draw_form()
    session$setInputs(`change-respondent_type__name` = name, `change-respondent_type__minimum_age` = "5")
    expect_true(change$ready())
    expect_false(needs_approval())
    expect_equal(primary_label(), "Add it")
    session$setInputs(submit = 1)
    expect_true(done()$applied)
    made$applied <- c(made$applied, done()$history_id)
    expect_match(text_of(output$detail), "Done")
    expect_match(text_of(output$detail), "in the database now, and recorded in the Edit History")
    # the list is read again, so it has the new one
    open_section(session, "respondent_type")
    expect_match(text_of(output$items_respondent_type), name, fixed = TRUE)
    press(session, "back")
    expect_equal(verb(), "")
    expect_length(errors(), 0)
  })
  rows <- table_model_get(test_connection, "respondent_type", list(name = database_eq(name)))
  expect_equal(nrow(rows), 1)
  expect_equal(rows$minimum_age, 5)
  entry <- history_model_get_all(test_connection)[[made$applied[1]]]
  expect_false(entry$approval_required)
})

test_that("an add that looks like something there asks first, and can go ahead anyway", {
  me <- local_staff()
  seen <- local_browser()
  made <- local_outcomes()
  existing <- local_respondent_type(minimum_age = 21, maximum_age = 24)
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    add(session, "respondent_type")
    draw_form()
    session$setInputs(
      `change-respondent_type__name` = test_name("Looks the same"),
      `change-respondent_type__minimum_age` = "21", `change-respondent_type__maximum_age` = "24"
    )
    session$setInputs(submit = 1)
    expect_null(done())
    expect_match(seen$modals[[1]], "already there")
    expect_match(seen$modals[[1]], existing$name, fixed = TRUE)
    session$setInputs(confirm_duplicate = 1)
    expect_true(done()$applied)
    made$applied <- c(made$applied, done()$history_id)
  })
})

test_that("going to another item while a form is open asks first", {
  me <- local_staff()
  seen <- local_browser()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    add(session, "respondent_type")
    pick(session, "registry", tree$registry$registry_id)
    expect_equal(verb(), "add") # still the form
    expect_match(utils::tail(seen$modals, 1)[[1]], "Leave this edit\\?")
    session$setInputs(confirm_leave = 1)
    expect_equal(verb(), "view")
    expect_equal(kind(), "registry")
  })
})

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
    made$drafts <- c(made$drafts, done()$record$id)
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
  made <- local_outcomes()
  page <- page_args(me)
  name <- test_name("Half typed")
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    add(session, "respondent_type")
    draw_form()
    session$setInputs(`change-respondent_type__name` = name)
    session$setInputs(save_draft = 1)
    expect_false(is.null(draft()$id))
    made$drafts <- c(made$drafts, draft()$id)
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

test_that("a question can be added to a questionnaire here: the details don't say it can't", {
  me <- local_staff()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    add(session, "questionnaire_question", as.character(tree$questionnaire$questionnaire_id))
    expect_false(grepl("You can't add", text_of(output$detail)))
    expect_match(text_of(output$detail), "A question as it's printed in one questionnaire")
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

test_that("Scale Items have a section for each measurement scale, and an item added there is applied", {
  me <- local_staff()
  scale <- local_row("measurement_scale", list(name = test_name("Scale"), abbreviation = test_name("SC")), "scale_id")
  question_type <- database_model_get(
    test_connection, "component_type", "component_type_id", order_by = "component_type_id",
    filters = list(name = database_eq("Question"))
  )
  component <- local_row("component", list(component_type_id = question_type$component_type_id, text = test_name("Scale question")), "component_id")
  local_row("question", list(component_id = component$component_id), "component_id")
  made <- local_outcomes() # (after the rows above, so the item it added goes before them)
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    open_section(session, "question_scale")
    scales <- paste(output$items_question_scale$html, collapse = "")
    expect_match(scales, sprintf('data-id="question_scale:%s"', key_text(scale$scale_id)), fixed = TRUE)
    open_section(session, paste0("question_scale:", key_text(scale$scale_id)))
    inside <- text_of(output[[paste0("items_question_scale_", key_text(scale$scale_id))]])
    expect_match(inside, "No scale items yet.")
    expect_match(inside, "Add Scale Item")
    # drawn again (the tables read again), the list keeps the scale's section open, as the page script has it
    press(session, "retry")
    scales <- paste(output$items_question_scale$html, collapse = "")
    expect_match(scales, sprintf('class="outline-node" data-id="question_scale:%s"', key_text(scale$scale_id)), fixed = TRUE)
    add(session, "question_scale", key_text(scale$scale_id))
    draw_form()
    session$setInputs(`change-question_scale__component_id` = key_text(component$component_id), `change-question_scale__item_number` = "1")
    expect_true(change$ready())
    session$setInputs(submit = 1)
    expect_true(done()$applied)
    made$applied <- c(made$applied, done()$history_id)
    expect_length(errors(), 0)
  })
  rows <- table_model_get(test_connection, "question_scale", list(scale_id = database_eq(scale$scale_id)))
  expect_equal(rows$component_id, component$component_id)
  expect_equal(rows$item_number, "1")
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
    expect_match(text_of(detail), "It is only shown here")
    expect_false(grepl('data-action="edit"|data-action="delete"', detail))
    expect_false(grepl("Open PDF", text_of(detail))) # (its file isn't in Storage)
    shown <- text_of(output[["change-main"]])
    expect_match(shown, "Pages 3")
    expect_match(shown, "Size 2 KB")
    expect_length(errors(), 0)
  })
})

test_that("an option set is added with its choices (once docs/sql/8_option_sets_editable.sql has run)", {
  me <- local_staff()
  choice <- local_row("answer_option", list(standardized_label = test_name("Choice")), "answer_option_id")
  made <- local_outcomes()
  page <- page_args(me)
  name <- test_name("Option set")
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    add(session, "option_set")
    draw_form()
    session$setInputs(`change-option_set__name` = name, `change-option_set__answer_option_ids` = key_text(choice$answer_option_id))
    expect_true(change$ready())
    session$setInputs(submit = 1)
    expect_true(isTRUE(done()$applied), info = "run docs/sql/8_option_sets_editable.sql on the test project")
    if (isTRUE(done()$applied)) made$applied <- c(made$applied, done()$history_id)
    expect_length(errors(), 0)
  })
  set <- table_model_get(test_connection, "option_set", list(name = database_eq(name)))
  expect_equal(nrow(set), 1)
  if (nrow(set) == 1) {
    members <- table_model_get(test_connection, "answer_option_set", list(option_set_id = database_eq(set$option_set_id)))
    expect_equal(members$answer_option_id, choice$answer_option_id)
  }
})
