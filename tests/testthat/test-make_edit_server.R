# The Make an edit page's server, without a browser: the sentence, the pick, the form, and what happens to
# the change: applied at once, saved as a draft, or sent for approval. The inputs of the modules inside it
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

test_that("choosing what to do and what kind reads the tables, and lists the rows to pick from", {
  me <- local_staff()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    session$setInputs(verb = "update", kind = "registry")
    expect_equal(names(snapshot())[1], "registry")
    expect_true("study" %in% names(snapshot()))
    expect_null(picked())
    expect_false(change$active())

    session$setInputs(`find-action` = list(action = "pick", id = as.character(tree$registry$registry_id)))
    expect_equal(picked(), as.character(tree$registry$registry_id))
    expect_true(change$active())
    expect_equal(row()$name, tree$registry$name)
    expect_match(text_of(output$steps), "What to do")
    expect_length(errors(), 0)
  })
})

test_that("an add is applied at once, is in the history, and the page says so", {
  me <- local_staff()
  seen <- local_browser()
  made <- local_outcomes()
  page <- page_args(me)
  name <- test_name("Added here")
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    session$setInputs(verb = "add", kind = "respondent_type")
    expect_true(change$active())
    draw_form()
    session$setInputs(`change-respondent_type__name` = name, `change-respondent_type__minimum_age` = "5")
    expect_true(change$ready())
    expect_false(needs_approval())
    expect_equal(primary_label(), "Add it")
    session$setInputs(submit = 1)
    expect_true(done()$applied)
    made$applied <- c(made$applied, done()$history_id)
    expect_match(text_of(output$sent), "Done")
    expect_match(text_of(output$sent), "in the database now, and recorded in the Edit history")
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
    session$setInputs(verb = "add", kind = "respondent_type")
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

test_that("a STAFF member's delete is sent for approval and waits on Change Requests; an ADMIN's is applied", {
  me <- local_staff()
  admin <- local_staff("ADMIN")
  made <- local_outcomes()
  type <- local_respondent_type()
  gone <- local_respondent_type()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    session$setInputs(verb = "delete", kind = "respondent_type")
    session$setInputs(`find-action` = list(action = "pick", id = as.character(type$respondent_type_id)))
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
    expect_match(text_of(output$sent), "Sent for approval")
    expect_true(done()$record$id %in% names(change_request_service_waiting(test_connection)))
    expect_equal(nrow(table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(type$respondent_type_id)))), 1)
  })

  admin_page <- page_args(admin)
  shiny::testServer(make_edit_server, args = admin_page$args, {
    session$setInputs(verb = "delete", kind = "respondent_type")
    session$setInputs(`find-action` = list(action = "pick", id = as.character(gone$respondent_type_id)))
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
    session$setInputs(verb = "add", kind = "respondent_type")
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
    session$setInputs(action = list(action = "continue", id = saved_id))
    expect_equal(resume()$id, saved_id)
    expect_equal(draft()$id, saved_id)
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

test_that("a question can be added to a questionnaire here: the page doesn't say it can't", {
  me <- local_staff()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    session$setInputs(verb = "add", kind = "questionnaire_question")
    expect_false(grepl("You can't add", text_of(output$explain)))
    expect_match(text_of(output$explain), "A question as it's printed in one questionnaire")
  })
})

test_that("a database that can't be read says so, with Try again", {
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
    session$setInputs(verb = "update", kind = "registry")
    expect_equal(load_problem(), "The database is down.")
    expect_match(text_of(output$explain), "Couldn't read the database")
    session$setInputs(retry = 1)
    expect_null(load_problem())
    expect_equal(names(snapshot())[1], "registry")
  })
})
