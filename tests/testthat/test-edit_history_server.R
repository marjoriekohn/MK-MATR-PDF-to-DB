# The Edit History page: the changes, newest first, what narrows them, and what "See what changed" shows. The
# records are made up as history_service_get_all() gives them (test-model_history.R reads real ones), except in
# the test that reads what a change did, against the real database.

history_args <- function(active = NULL, connection = test_connection) list(connection = connection, active = active)

# n changes that look like real ones, numbered from 1, for the page that lists a few at a time.
many_records <- function(n) {
  stats::setNames(lapply(rev(seq_len(n)), function(i) {
    list(
      id = as.character(i), action = c("Added", "Updated", "Deleted", "Pushed", "Emptied")[i %% 5 + 1], entity = "respondent_type",
      record_id = as.character(i), summary = paste("Change number", i), staff = "Maggie Kohn", occurred = "2026-10-01T11:01:00+00:00"
    )
  }), rev(seq_len(n)))
}

test_that("the changes are listed newest first, each with its kind, when it was made and who made it", {
  local_stub("history_service_get_all", function(connection) many_records(3))
  shiny::testServer(edit_history_server, args = history_args(connection = NULL), {
    errors <- collect_errors(session)
    session$setInputs(kind = "all", search = "")
    listed <- text_of(output$entries)
    expect_match(listed, "Pushed Change number 3 On Oct 1(, 2026)? · by Maggie Kohn")
    expect_match(listed, "Deleted Change number 2")
    expect_match(listed, "Updated Change number 1")
    expect_lt(regexpr("Change number 3", listed), regexpr("Change number 1", listed)) # newest first
    expect_equal(names(matching()), c("3", "2", "1"))
    expect_length(errors(), 0)
  })
})

test_that("a kind narrows the list, and so do words typed", {
  local_stub("history_service_get_all", function(connection) many_records(10))
  shiny::testServer(edit_history_server, args = history_args(connection = NULL), {
    session$setInputs(kind = "Added", search = "")
    expect_equal(names(matching()), c("10", "5"))
    session$setInputs(kind = "Pushed")
    expect_equal(names(matching()), c("8", "3"))
    session$setInputs(kind = "all", search = "NUMBER 7") # every word, in any case
    expect_equal(names(matching()), "7")
    session$setInputs(search = "nothing like this zzz")
    expect_equal(text_of(output$entries), "No change matches that.")
  })
})

test_that("with nothing changed yet, the page says so", {
  local_stub("history_service_get_all", function(connection) list())
  shiny::testServer(edit_history_server, args = history_args(connection = NULL), {
    session$setInputs(kind = "all", search = "")
    expect_equal(text_of(output$entries), "Nothing has been changed in the MATR metadata database through this site yet.")
  })
})

test_that("See what changed lists each row's fields, old and new, or what a push counted", {
  me <- local_staff()
  seen <- local_browser()
  updated <- local_history(me, "Updated", "Changed the name of the respondent type “Parents”", details = list(
    list(item_number = 1, entity_name = "respondent_type", record_identifier = '{"respondent_type_id":5}', action = "Update", attribute_name = "name", old_value = "Parents", new_value = "Guardians")
  ))
  pushed <- local_history(me, "Pushed", "Pushed the extraction of Kid Questionnaire, Wave 1: 2 questions, 3 new variables.", entity = "wave", details = list(
    questions = 2, variable_new = 3, extraction_id = 99
  ))
  shiny::testServer(edit_history_server, args = history_args(), {
    session$setInputs(kind = "all", search = "")
    session$setInputs(action = list(action = "open", id = as.character(updated$history_id)))
    expect_length(seen$modals, 1)
    shown <- seen$modals[[1]]
    expect_match(shown, "Changed the name of the respondent type “Parents”", fixed = TRUE)
    expect_match(shown, paste0("Today at .* · by ", me$name))
    expect_match(shown, "Changes 1 row, in 1 table.")
    expect_match(shown, "Change respondent type respondent_type_id 5 name Parents → Guardians")

    session$setInputs(action = list(action = "open", id = as.character(pushed$history_id)))
    expect_length(seen$modals, 2)
    expect_match(seen$modals[[2]], "Pushed the extraction of Kid Questionnaire, Wave 1.*Questions: 2 New variables: 3")
  })
})

test_that("a change that isn't there any more is ignored, and a database that can't be read says so", {
  seen <- local_browser()
  local_stub("history_service_get_all", function(connection) many_records(2))
  local_stub("history_service_changes", function(connection, id) stop("The database is down.", call. = FALSE))
  shiny::testServer(edit_history_server, args = history_args(connection = NULL), {
    errors <- collect_errors(session)
    session$setInputs(kind = "all", search = "")
    session$setInputs(action = list(action = "open", id = "99"))
    expect_length(seen$modals, 0)
    expect_length(notice_texts(seen), 0) # nothing to read for a change that isn't there
    session$setInputs(action = list(action = "open", id = "2"))
    expect_length(seen$modals, 0)
    expect_equal(notice_texts(seen), "Couldn't read what changed: The database is down.")
    expect_length(errors(), 0)
  })
})

test_that("only a page of changes is listed at first, and Show more lists the next", {
  local_stub("history_service_get_all", function(connection) many_records(60))
  shiny::testServer(edit_history_server, args = history_args(connection = NULL), {
    session$setInputs(kind = "all", search = "")
    listed <- text_of(output$entries)
    expect_equal(lengths(regmatches(listed, gregexpr("See what changed", listed))), 25)
    expect_match(listed, "Change number 60")
    expect_false(grepl("Change number 35 ", listed))
    expect_match(listed, "Show more \\(35 more\\)")

    session$setInputs(action = list(action = "more"))
    listed <- text_of(output$entries)
    expect_equal(lengths(regmatches(listed, gregexpr("See what changed", listed))), 50)
    expect_match(listed, "Show more \\(10 more\\)")

    # narrowing starts the list again from its top
    session$setInputs(kind = "Added")
    expect_equal(showing(), history_page_size)
  })
})

test_that("coming back to the page reads the history again; a read that fails keeps what was last read", {
  seen <- local_browser()
  reads <- 0L
  local_stub("history_service_get_all", function(connection) {
    reads <<- reads + 1L
    if (reads == 2L) stop("The database is down.", call. = FALSE)
    many_records(2)
  })
  active <- shiny::reactiveVal(FALSE)
  shiny::testServer(edit_history_server, args = history_args(active = active, connection = NULL), {
    session$setInputs(kind = "all", search = "")
    expect_match(text_of(output$entries), "Change number 2")
    active(TRUE)
    session$flushReact()
    expect_match(text_of(output$entries), "Change number 2") # kept
    expect_equal(notice_texts(seen), "The database is down.")
    active(FALSE)
    session$flushReact()
    active(TRUE)
    session$flushReact()
    expect_match(text_of(output$entries), "Change number 1")
    expect_equal(reads, 3L)
  })
})

test_that("each change, and an empty list, is a list card like Extraction's drafts", {
  html <- htmltools::renderTags(history_entry_ui(many_records(1)[[1]]))$html
  for (part in c("list-card", "list-card-body", "list-card-title-row", "list-card-title", "list-card-meta", "list-card-actions")) {
    expect_match(html, paste0('class="', part, '"'), fixed = TRUE, info = part)
  }
  expect_match(html, 'class="row-badge is-updated"', fixed = TRUE)
  expect_match(htmltools::renderTags(no_history(filtered = FALSE))$html, 'class="list-card is-empty"', fixed = TRUE)
  expect_match(htmltools::renderTags(no_drafts("mine"))$html, 'class="list-card is-empty"', fixed = TRUE)
  # the toggle offers every kind of change the history records
  page <- htmltools::renderTags(edit_history_ui("h"))$html
  for (action in history_actions) expect_match(page, sprintf('value="%s"', action), fixed = TRUE, info = action)
  expect_false(grepl("Extraction|Awaiting|approv", page))
})
