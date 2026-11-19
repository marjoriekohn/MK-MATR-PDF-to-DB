# The Metadata page's server (the module make_edit), without a browser: the list (a section for each kind), the item
# picked in the details, its form, and what happens to the change: applied at once. The inputs of the modules inside
# it are named with their module's prefix, e.g. `change-registry__name`. The database is the real one.
# What both halves share is in helper-make_edit_page.R.

test_that("a list holds its items, by name, and + Add; wordings are listed as questions, sections and notes", {
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
    # Questions: every question wording, once, and + Add there adds one to a questionnaire
    open_section(session, "question")
    questions <- paste(output$items_question$html, collapse = "")
    expect_false(grepl('data-kind="placement" data-key', questions, fixed = TRUE)) # wordings, not one questionnaire's blocks
    expect_match(questions, 'data-action="add" data-kind="placement" data-list="question"', fixed = TRUE)
    expect_match(text_of(questions), "Add Question")
    # the database's own kinds have no list: a wording is in one of three by its kind of block, and one
    # questionnaire's blocks and a scale's questions are reached from a wording's and a scale's details
    expect_error(output$items_component)
    expect_error(output$items_placement)
    expect_error(output$items_question_scale)
    expect_error(output$items_option_set)
    # Collapse All closes every list
    session$setInputs(action = list(action = "collapse_all"))
    expect_error(output$items_registry)
    expect_error(output$items_question)
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
    expect_false(grepl('data-action="merge"', detail)) # two registries are never one
    expect_match(text_of(output[["change-main"]]), tree$registry$abbreviation, fixed = TRUE) # its fields, as text
    expect_false(grepl("data-field|<input", paste(output[["change-main"]]$html, collapse = "")))
    press(session, "edit")
    expect_equal(verb(), "update")
    expect_match(paste(output[["change-main"]]$html, collapse = ""), "<input")
    # a kind that can't be emptied has no Empty; one that can be kept twice by mistake has Merge
    pick(session, "investigator", tree$investigator$principal_investigator_id)
    expect_false(grepl('data-action="empty"', paste(output$detail$html, collapse = "")))
    expect_match(paste(output$detail$html, collapse = ""), 'data-action="merge"')
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

test_that("renaming a tag to another tag's name asks before merging them, then shows the one that is left", {
  me <- local_staff()
  seen <- local_browser()
  from <- local_row("tag", list(kind = "unit", name = normal_unit(test_name("old boxes"))), "tag_id")
  into <- local_row("tag", list(kind = "unit", name = normal_unit(test_name("boxes"))), "tag_id")
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    pick(session, "tag", from$tag_id)
    press(session, "edit")
    draw_form()
    session$setInputs(`change-tag__name` = toupper(into$name))
    expect_true(change$ready())
    session$setInputs(submit = 1)
    expect_null(done()) # it asks first
    expect_match(seen$modals[[1]], "Merge these tags")
    expect_match(seen$modals[[1]], into$name, fixed = TRUE)
    session$setInputs(confirm_merge = 1)
    expect_match(done()$summary, "^Merged the tag")
    press(session, "back")
    expect_equal(picked(), key_text(into$tag_id)) # the tag it went into
    expect_length(errors(), 0)
  })
  expect_equal(nrow(table_model_get(test_connection, "tag", list(tag_id = database_eq(from$tag_id)))), 0)
  expect_equal(nrow(table_model_get(test_connection, "tag", list(tag_id = database_eq(into$tag_id)))), 1)
})

test_that("going to another item while a form has something typed in it asks first; one only looked at doesn't", {
  me <- local_staff()
  seen <- local_browser()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    add(session, "respondent_type")
    draw_form()
    session$setInputs(`change-respondent_type__name` = test_name("Half typed"))
    pick(session, "registry", tree$registry$registry_id)
    expect_equal(verb(), "add") # still the form
    expect_match(utils::tail(seen$modals, 1)[[1]], "Leave this edit\\?")
    session$setInputs(confirm_leave = 1)
    expect_equal(verb(), "view")
    expect_equal(kind(), "registry")
    # Edit, then nothing typed: there is nothing to lose, so neither leaving nor Cancel asks
    asked <- length(seen$modals)
    press(session, "edit")
    draw_form()
    session$setInputs(cancel = 1)
    expect_equal(verb(), "view")
    press(session, "edit")
    draw_form()
    pick(session, "investigator", tree$investigator$principal_investigator_id)
    expect_equal(c(kind(), verb()), c("investigator", "view"))
    expect_length(seen$modals, asked)
  })
})

test_that("a tag in the details opens what it names, and Back returns from it", {
  me <- local_staff()
  tree <- shared_tree()
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    pick(session, "questionnaire", tree$questionnaire$questionnaire_id)
    shown <- paste(output[["change-main"]]$html, collapse = "")
    expect_match(shown, sprintf('class="edit-context-pill is-link" data-action="jump" data-kind="study" data-verb="view" data-key="%s"', key_text(tree$study$study_id)), fixed = TRUE)
    expect_false(grepl("Back to", text_of(output$detail))) # picked in the list: nothing to go back to
    session$setInputs(`change-action` = list(action = "jump", kind = "study", verb = "view", key = key_text(tree$study$study_id)))
    expect_equal(c(kind(), picked(), verb()), c("study", key_text(tree$study$study_id), "view"))
    came_from <- shorten(name_with_abbreviation(tree$questionnaire$name, tree$questionnaire$abbreviation), 40) # as the questionnaire is named
    expect_match(text_of(output$detail), paste("Back to", came_from), fixed = TRUE)
    press(session, "trail_back")
    expect_equal(c(kind(), picked(), verb()), c("questionnaire", key_text(tree$questionnaire$questionnaire_id), "view"))
    expect_false(grepl("Back to", text_of(output$detail)))
    expect_length(errors(), 0)
  })
})

test_that("Merge on an item's details merges it into the one picked, once the box is ticked, and shows the one that is left", {
  me <- local_staff()
  from <- local_row("answer_option", list(standardized_label = tolower(test_name("do not know"))), "answer_option_id")
  into <- local_row("answer_option", list(standardized_label = tolower(test_name("dont know"))), "answer_option_id")
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    pick(session, "answer_option", from$answer_option_id)
    expect_match(paste(output$detail$html, collapse = ""), 'data-action="merge"')
    press(session, "merge")
    expect_equal(verb(), "merge")
    expect_match(text_of(output$detail), "Merging an answer choice")
    draw_form()
    expect_false(change$ready())
    session$setInputs(`change-merge_into` = key_text(into$answer_option_id))
    expect_false(change$ready()) # the box isn't ticked
    session$setInputs(`change-confirm_merge_read` = TRUE)
    expect_true(change$ready())
    expect_match(text_of(output$detail), "Merge them")
    session$setInputs(submit = 1)
    expect_match(done()$summary, "^Merged the answer choice")
    press(session, "back")
    expect_equal(c(picked(), verb()), c(key_text(into$answer_option_id), "view")) # the one it went into
    expect_length(errors(), 0)
  })
  expect_equal(nrow(table_model_get(test_connection, "answer_option", list(answer_option_id = database_eq(from$answer_option_id)))), 0)
  expect_equal(nrow(table_model_get(test_connection, "answer_option", list(answer_option_id = database_eq(into$answer_option_id)))), 1)
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

test_that("a scale's questions are added from the scale's details, and Back returns to the scale, which lists them", {
  me <- local_staff()
  scale <- local_row("measurement_scale", list(name = test_name("Scale"), abbreviation = test_abbreviation()), "scale_id")
  question <- local_row("component", list(kind = "Question", text = tolower(test_name("how often do you feel sad"))), "component_id")
  made <- local_outcomes() # (after the rows above, so the item it added goes before them)
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    pick(session, "measurement_scale", scale$scale_id)
    shown <- paste(output[["change-main"]]$html, collapse = "")
    expect_match(text_of(shown), "0 Questions", fixed = TRUE)
    expect_match(shown, sprintf('data-kind="question_scale" data-verb="add" data-scope="%s"', key_text(scale$scale_id)), fixed = TRUE)
    # Add a question, in the scale's details, opens the form for that scale
    session$setInputs(`change-action` = list(action = "jump", kind = "question_scale", verb = "add", scope = key_text(scale$scale_id)))
    expect_equal(c(kind(), scope(), verb()), c("question_scale", key_text(scale$scale_id), "add"))
    draw_form()
    session$setInputs(`change-question_scale__component_id` = key_text(question$component_id))
    expect_true(change$ready())
    session$setInputs(submit = 1)
    expect_match(done()$history_id, "^[0-9]+$")
    # Back returns to the scale, which now lists the question, with Remove
    press(session, "back")
    expect_equal(c(kind(), picked(), verb()), c("measurement_scale", key_text(scale$scale_id), "view"))
    shown <- paste(output[["change-main"]]$html, collapse = "")
    expect_match(text_of(shown), "1 Question ", fixed = TRUE)
    expect_match(shown, sprintf('data-kind="question_scale" data-verb="delete" data-key="%s|%s"', key_text(scale$scale_id), key_text(question$component_id)), fixed = TRUE)
    expect_length(errors(), 0)
  })
  withr::defer(test_delete("question_scale", list(scale_id = database_eq(scale$scale_id))), priority = "first")
  rows <- table_model_get(test_connection, "question_scale", list(scale_id = database_eq(scale$scale_id)))
  expect_equal(rows$component_id, question$component_id)
  expect_named(rows, c("scale_id", "component_id")) # an item is its scale and its question, no number
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
  # + Add in the Questions list is called a question, and its form starts as one
  shiny::testServer(make_edit_server, args = page$args, {
    session$setInputs(action = list(action = "add", kind = "placement", list = "question"))
    expect_match(text_of(output$detail), "New question")
    session$setInputs(add_scope = as.character(tree$questionnaire$questionnaire_id))
    draw_form()
    expect_equal(change$values()[["placement__kind"]], "Question")
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

test_that("PDFs have no list: a wave names its PDF, which its own form changes, and has Open PDF", {
  me <- local_staff()
  tree <- shared_tree()
  sha256 <- paste(sample(c(0:9, letters[1:6]), 64, replace = TRUE), collapse = "")
  local_row("pdf_file", list(pdf_sha256 = sha256, pdf_pages = 3, pdf_bytes = 2048), "pdf_sha256")
  wave <- local_row("wave", list(
    questionnaire_id = tree$questionnaire$questionnaire_id, name = test_name("Wave"), pdf_sha256 = sha256, pdf_name = "first_name.pdf"
  ), "wave_id")
  # a test's stand-in session can't serve files (it has no registerDataObj), so the address is made up
  local_stub("pdf_address", function(session, name, data, ...) paste0("session/pdf/", data$sha256))
  page <- page_args(me)
  shiny::testServer(make_edit_server, args = page$args, {
    errors <- collect_errors(session)
    expect_false("pdf_file" %in% make_edit_listed)
    pick(session, "wave", wave$wave_id)
    expect_match(text_of(output$detail), "Open PDF")
    expect_match(paste(output$detail$html, collapse = ""), paste0('href="session/pdf/', sha256, '"'), fixed = TRUE)
    shown <- text_of(output[["change-main"]])
    expect_match(shown, "PDF file first_name.pdf", fixed = TRUE)
    expect_match(shown, "3 pages · 2 KB", fixed = TRUE)
    # the name is the wave's own: Edit changes it, and the file itself is as it was
    press(session, "edit")
    draw_form()
    session$setInputs(`change-wave__pdf_name` = "better_name.pdf")
    expect_true(change$ready())
    session$setInputs(submit = 1)
    expect_match(done()$history_id, "^[0-9]+$")
    stored <- database_model_select(test_connection, "wave", "pdf_name,pdf_sha256", filters = list(wave_id = database_eq(wave$wave_id)))[[1]]
    expect_equal(stored$pdf_name, "better_name.pdf")
    expect_equal(stored$pdf_sha256, sha256)
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

test_that("a wave that asks questions offers Edit extraction, and one that asks none says why not", {
  html <- function(asks) as.character(wave_edit_extraction_button(asks))
  expect_match(html(TRUE), 'data-action="edit_wave_extraction"', fixed = TRUE)
  expect_false(grepl("disabled", html(TRUE)))
  expect_match(html(FALSE), "disabled")
  expect_match(html(FALSE), "asks no questions yet")
})
