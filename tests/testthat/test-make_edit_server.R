# The Make an edit page's server, without a browser: the list (a section for each kind), the item picked in the
# details, its form, and what happens to the change: applied at once, saved as a draft, or sent for approval.
# The inputs of the modules inside it
# are named with their module's prefix, e.g. `change-registry__name`. The database is the real one.

# Sending for approval, saving and discarding drafts, and the kinds with forms of their own are in
# test-make_edit_server_drafts.R; what both share is in helper-make_edit_page.R.

test_that("a kind's section lists its items, by name, and + Add; Questions lists each question once", {
  me <- local_staff()
  tree <- shared_tree()
  shared_pushed_questionnaire() # questions to list, whatever the database has
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    expect_error(output$items_registry) # closed: nothing is drawn, or read from the database
    open_section(session, "registry")
    registries <- paste(output$items_registry$html, collapse = "")
    expect_match(text_of(registries), tree$registry$name, fixed = TRUE)
    expect_match(registries, sprintf('data-action="select" data-id="registry||%s"', tree$registry$registry_id), fixed = TRUE)
    expect_match(text_of(registries), "Add Registry")
    # opening it showed its first item, marked in the list
    expect_equal(c(kind(), verb()), c("registry", "view"))
    expect_match(registries, sprintf('is-selected" tabindex="0" data-action="select" data-id="registry||%s"', picked()), fixed = TRUE)
    # Questions: each question once, with no section for each questionnaire; + Add adds one to a questionnaire
    open_section(session, "question")
    questions <- paste(output$items_question$html, collapse = "")
    expect_false(grepl(sprintf('data-id="question:%s"', tree$questionnaire$questionnaire_id), questions, fixed = TRUE))
    items <- regmatches(questions, gregexpr('class="outline-node" data-id="question\\|\\|[0-9]+"', questions))[[1]]
    expect_gt(length(items), 0)
    expect_false(any(duplicated(items)))
    expect_match(text_of(questions), "Add Question")
    expect_match(questions, 'data-action="add" data-kind="questionnaire_question"', fixed = TRUE)
    # Blocks, a question in one questionnaire and the shared wording of blocks aren't listed
    expect_error(output$items_questionnaire_component)
    expect_error(output$items_component)
    expect_error(output$items_questionnaire_question)
    # Collapse All closes every section
    session$setInputs(action = list(action = "collapse_all"))
    expect_error(output$items_registry)
    expect_error(output$items_question)
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

test_that("going to another page and back with a form open keeps what was typed in it", {
  me <- local_staff()
  showing <- shiny::reactiveVal(TRUE)
  page <- page_args(me)
  name <- test_name("Typed before leaving")
  shiny::testServer(make_edit_server, args = c(page$args, list(active = showing)), {
    add(session, "respondent_type")
    draw_form()
    session$setInputs(`change-respondent_type__name` = name)
    token <- change$token()
    showing(FALSE)
    session$flushReact()
    showing(TRUE)
    session$flushReact()
    output[["change-main"]] # as the browser would draw it again, if it had changed
    expect_equal(change$token(), token) # the form wasn't drawn again
    expect_equal(change$values()[["respondent_type__name"]], name)
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
    expect_match(text_of(output$detail), "Successfully saved to the database")
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

test_that("Scale Items have a section for each measurement scale, and an item added there is applied", {
  me <- local_staff()
  scale <- local_row("measurement_scale", list(name = test_name("Scale"), abbreviation = test_name("SC")), "scale_id")
  # a question for the scale: a component and its question row are only added together, as a push adds them
  pushed <- shared_pushed_questionnaire()
  asked <- database_model_get(
    test_connection, "questionnaire_question", "component_id", order_by = "component_id",
    filters = list(questionnaire_id = database_eq(pushed$questionnaire_id))
  )
  component <- list(component_id = asked$component_id[1])
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
