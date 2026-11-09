# The Metadata page's server (the module make_edit), without a browser: the list (a section for each kind), the item
# picked in the details, its form, and what happens to the change: applied at once. The inputs of the modules inside
# it are named with their module's prefix, e.g. `change-registry__name`. The database is the real one.
# What both halves share is in helper-make_edit_page.R.

test_that("a kind's section lists its items, by name, and + Add; Blocks has a section for each questionnaire", {
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
    # opening it showed its first item, marked in the list
    expect_equal(c(kind(), verb()), c("registry", "view"))
    expect_match(registries, sprintf('is-selected" tabindex="0" data-action="select" data-id="registry||%s"', picked()), fixed = TRUE)
    # Blocks: a section for each questionnaire, its blocks inside, and + Add there
    open_section(session, "placement")
    blocks <- paste(output$items_placement$html, collapse = "")
    expect_match(blocks, sprintf('data-id="placement:%s"', tree$questionnaire$questionnaire_id), fixed = TRUE)
    open_section(session, paste0("placement:", tree$questionnaire$questionnaire_id))
    inside <- text_of(output[[paste0("items_placement_", tree$questionnaire$questionnaire_id)]])
    expect_match(inside, "No blocks yet.")
    expect_match(inside, "Add Block")
    # the kinds of the old schema have no section
    expect_error(output$items_question)
    expect_error(output$items_option_set)
    # Collapse All closes every section
    session$setInputs(action = list(action = "collapse_all"))
    expect_error(output$items_registry)
    expect_error(output$items_placement)
    expect_length(errors(), 0)
  })
})

test_that("picking an item shows it in the details, its fields as text, with Edit, Empty and Delete", {
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
    expect_false(change$ready()) # nothing to apply while it is only shown
    detail <- paste(output$detail$html, collapse = "")
    expect_match(text_of(detail), tree$registry$name, fixed = TRUE)
    expect_match(detail, 'data-action="edit"')
    expect_match(detail, 'data-action="empty"')
    expect_match(detail, 'data-action="delete"')
    expect_match(text_of(output[["change-main"]]), tree$registry$abbreviation, fixed = TRUE) # its fields, as text
    expect_false(grepl("data-field|<input", paste(output[["change-main"]]$html, collapse = "")))
    press(session, "edit")
    expect_equal(verb(), "update")
    expect_match(paste(output[["change-main"]]$html, collapse = ""), "<input")
    # a kind that can't be emptied has no Empty
    pick(session, "investigator", tree$investigator$principal_investigator_id)
    expect_false(grepl('data-action="empty"', paste(output$detail$html, collapse = "")))
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
    expect_match(paste(output$detail$html, collapse = ""), "Add it")
    expect_false(grepl("Save as draft|Send for approval", text_of(output$detail)))
    session$setInputs(submit = 1)
    expect_match(done()$history_id, "^[0-9]+$")
    made$added(done()$new_ids)
    made$history_id <- done()$history_id
    expect_match(text_of(output$detail), "Done")
    expect_match(text_of(output$detail), "Successfully saved to the database")
    expect_match(text_of(output$detail), "See the Edit History")
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
  entry <- database_model_select(test_connection, "history", "action,summary", filters = list(history_id = database_eq(made$history_id)))[[1]]
  expect_equal(entry$action, "Added")
  expect_equal(entry$summary, paste0("Added the respondent type “", name, "”"))
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
    expect_match(done()$history_id, "^[0-9]+$")
    made$added(done()$new_ids)
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

test_that("a delete is applied at once by anyone, after the confirmation, and the item is gone from the list", {
  me <- local_staff()
  gone <- local_respondent_type()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    pick(session, "respondent_type", gone$respondent_type_id)
    press(session, "delete")
    draw_form()
    expect_false(change$ready())
    expect_match(text_of(output$detail), "Delete it")
    session$setInputs(`change-confirm_read` = TRUE)
    expect_true(change$ready()) # nothing points at it: no word to type
    session$setInputs(submit = 1)
    expect_equal(done()$verb, "delete")
    expect_match(text_of(output$detail), "Successfully deleted from the database")
    press(session, "back")
    expect_null(picked())
    expect_length(errors(), 0)
  })
  expect_equal(nrow(table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(gone$respondent_type_id)))), 0)
})

test_that("Empty asks with what the thing holds, then empties it and says so", {
  me <- local_staff()
  seen <- local_browser()
  tree <- local_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    pick(session, "wave", tree$wave$wave_id)
    press(session, "empty")
    expect_match(seen$modals[[1]], "Empty .*\\?")
    expect_match(seen$modals[[1]], "It asks no questions")
    expect_match(seen$modals[[1]], "What stays")
    session$setInputs(confirm_empty = 1)
    expect_match(notice_texts(seen)[length(notice_texts(seen))], "^Emptied. Removed 0 questions, 0 blocks and 0 variables. It is in the Edit History.")
    expect_equal(verb(), "view") # the wave is still there, and still picked
    expect_length(errors(), 0)
  })
  entries <- database_model_select(test_connection, "history", "action,entity,record_id", filters = list(staff_id = database_eq(me$staff_id), action = database_eq("Emptied")))
  expect_true(any(vapply(entries, function(entry) identical(entry$record_id, key_text(tree$wave$wave_id)), logical(1))))
})

test_that("Scale Items have a section for each measurement scale, and an item added there is applied", {
  me <- local_staff()
  scale <- local_row("measurement_scale", list(name = test_name("Scale"), abbreviation = test_abbreviation()), "scale_id")
  question <- local_row("component", list(kind = "Question", text = tolower(test_name("how often do you feel sad"))), "component_id")
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
    session$setInputs(`change-question_scale__component_id` = key_text(question$component_id), `change-question_scale__item_number` = "1")
    expect_true(change$ready())
    session$setInputs(submit = 1)
    expect_match(done()$history_id, "^[0-9]+$")
    expect_length(errors(), 0)
  })
  withr::defer(test_delete("question_scale", list(scale_id = database_eq(scale$scale_id))), priority = "first")
  rows <- table_model_get(test_connection, "question_scale", list(scale_id = database_eq(scale$scale_id)))
  expect_equal(rows$component_id, question$component_id)
  expect_equal(rows$item_number, "1")
})

test_that("+ Add of a block asks which questionnaire, then shows the form, with a question's fields once Question is picked", {
  me <- local_staff()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    add(session, "placement")
    expect_match(paste(output$detail$html, collapse = ""), "add_scope")
    expect_false(grepl("change-main", paste(output$detail$html, collapse = "")))
    session$setInputs(add_scope = as.character(tree$questionnaire$questionnaire_id))
    expect_equal(scope(), as.character(tree$questionnaire$questionnaire_id))
    expect_false(grepl("You can't add", text_of(output$detail)))
    expect_match(paste(output$detail$html, collapse = ""), "change-main")
    draw_form()
    html <- paste(output[["change-main"]]$html, collapse = "")
    expect_match(html, "placement__kind")
    expect_match(html, "placement__variables|variables") # the variable editor, shown for a question
    session$setInputs(`change-placement__kind` = "Section", `change-placement__text` = test_name("A. About you"))
    expect_true(change$ready())
    session$setInputs(`change-placement__kind` = "Question")
    expect_false(change$ready())
    expect_equal(change$blockers()[1], "Fill in the waves that ask it.")
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
    expect_false(grepl('data-action="edit"|data-action="delete"|data-action="empty"', detail))
    expect_false(grepl("Open PDF", text_of(detail))) # (its file isn't in Storage)
    shown <- text_of(output[["change-main"]])
    expect_match(shown, "Pages 3")
    expect_match(shown, "Size 2 KB")
    expect_length(errors(), 0)
  })
})

test_that("a wave's Re-extract is off until it was extracted from a PDF, and the page returns nothing to open", {
  me <- local_staff()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    pick(session, "wave", tree$wave$wave_id)
    detail <- paste(output$detail$html, collapse = "")
    expect_match(detail, 'data-action="reextract_wave" disabled')
    expect_null(session$returned)
  })
})
