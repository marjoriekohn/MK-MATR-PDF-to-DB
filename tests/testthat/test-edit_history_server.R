# The Edit History page: the applied changes, newest first, what narrows them, and what "See what changed"
# shows. The changes are made by applying them, as Make an edit does, against the real database.

history_args <- function(active = NULL) list(connection = test_connection, active = active)

# n applied changes that look like real ones, numbered from 1, for the page that lists a few at a time.
many_records <- function(n) {
  stats::setNames(lapply(rev(seq_len(n)), function(i) {
    list(
      id = as.character(i), kind = c("Add", "Update", "Delete")[i %% 3 + 1], source = "Edit", status = "Applied",
      summary = paste("Edit number", i), prepared_by = "Maggie Kohn", approved_by = "Alex Rivera", approval_required = TRUE,
      prepared = "2026-10-01T10:00:00Z", approved = "2026-10-01T11:00:00Z", applied = "2026-10-01T11:01:00Z"
    )
  }), rev(seq_len(n)))
}

test_that("the changes are listed newest first, each with when it was applied and who was behind it", {
  admin <- local_staff("ADMIN")
  me <- local_staff()
  approver <- local_staff()
  added <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("Listed"))
  approved <- local_approved_delete(me, approver)
  shiny::testServer(edit_history_server, args = history_args(), {
    errors <- collect_errors(session)
    session$setInputs(kind = "all", search = test_marker)
    listed <- text_of(output$entries)
    expect_match(listed, paste0("Delete Deleted the respondent type .* prepared by ", me$name, " and approved by ", approver$name))
    expect_match(listed, paste0("Add Added the respondent type .* made by ", admin$name, ", applied at once"))
    expect_lt(regexpr("Deleted the respondent type", listed), regexpr("Added the respondent type", listed)) # newest first
    expect_equal(names(matching())[1:2], as.character(c(approved, added)))
    expect_length(errors(), 0)
  })
})

test_that("a kind narrows the list, and so do words typed", {
  admin <- local_staff("ADMIN")
  added <- local_applied_edit(admin, "respondent_type", "add", respondent_type__name = test_name("Narrowed"))
  shiny::testServer(edit_history_server, args = history_args(), {
    session$setInputs(kind = "Add", search = test_marker)
    expect_true(as.character(added) %in% names(matching()))
    expect_true(all(vapply(matching(), function(record) record$kind == "Add", logical(1))))
    session$setInputs(kind = "Delete")
    expect_false(as.character(added) %in% names(matching()))
    session$setInputs(kind = "all", search = paste(test_marker, "narrowed")) # every word, in any case
    expect_equal(names(matching()), as.character(added))
    session$setInputs(search = "nothing like this zzz")
    expect_equal(text_of(output$entries), "No edit matches that.")
  })
})

test_that("with nothing applied yet, the page says so", {
  local_stub("history_service_get_all", function(connection) list())
  shiny::testServer(edit_history_server, args = history_args(), {
    session$setInputs(kind = "all", search = "")
    expect_equal(text_of(output$entries), "Nothing has been added to the MATR metadata database through this site yet.")
  })
})

test_that("See what changed lists each row's fields, old and new", {
  admin <- local_staff("ADMIN")
  seen <- local_browser()
  type <- local_respondent_type(minimum_age = 18, maximum_age = 99)
  new_name <- test_name("Renamed")
  changed <- local_applied_edit(admin, "respondent_type", "update", type$respondent_type_id, respondent_type__name = new_name)
  shiny::testServer(edit_history_server, args = history_args(), {
    session$setInputs(kind = "all", search = "")
    session$setInputs(action = list(action = "open", id = as.character(changed)))
    expect_length(seen$modals, 1)
    shown <- seen$modals[[1]]
    expect_match(shown, paste0("Changed the name of the respondent type “", type$name, "”"), fixed = TRUE)
    expect_match(shown, paste0("Applied today at .* · made by ", admin$name))
    expect_match(shown, "Changes 1 row, in 1 table.")
    expect_match(shown, paste0("Change respondent type .*name ", type$name, " → ", new_name))
  })
})

test_that("an edit that isn't there any more is ignored, and a database that can't be read says so", {
  seen <- local_browser()
  local_stub("history_service_get_all", function(connection) many_records(2))
  local_stub("history_service_changes", function(connection, id) stop("The database is down.", call. = FALSE))
  shiny::testServer(edit_history_server, args = history_args(), {
    errors <- collect_errors(session)
    session$setInputs(kind = "all", search = "")
    session$setInputs(action = list(action = "open", id = "99"))
    expect_length(seen$modals, 0)
    expect_length(notice_texts(seen), 0) # nothing to read for an edit that isn't there
    session$setInputs(action = list(action = "open", id = "2"))
    expect_length(seen$modals, 0)
    expect_equal(notice_texts(seen), "Couldn't read what changed: The database is down.")
    expect_length(errors(), 0)
  })
})

test_that("only a page of edits is listed at first, and Show more lists the next", {
  local_stub("history_service_get_all", function(connection) many_records(60))
  shiny::testServer(edit_history_server, args = history_args(), {
    session$setInputs(kind = "all", search = "")
    listed <- text_of(output$entries)
    expect_equal(lengths(regmatches(listed, gregexpr("See what changed", listed))), 25)
    expect_match(listed, "Edit number 60")
    expect_false(grepl("Edit number 35 ", listed))
    expect_match(listed, "Show more \\(35 more\\)")

    session$setInputs(action = list(action = "more"))
    listed <- text_of(output$entries)
    expect_equal(lengths(regmatches(listed, gregexpr("See what changed", listed))), 50)
    expect_match(listed, "Show more \\(10 more\\)")

    # narrowing starts the list again from its top
    session$setInputs(kind = "Add")
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
  shiny::testServer(edit_history_server, args = history_args(active = active), {
    session$setInputs(kind = "all", search = "")
    expect_match(text_of(output$entries), "Edit number 2")
    active(TRUE)
    session$flushReact()
    expect_match(text_of(output$entries), "Edit number 2") # kept
    expect_equal(notice_texts(seen), "The database is down.")
    active(FALSE)
    session$flushReact()
    active(TRUE)
    session$flushReact()
    expect_match(text_of(output$entries), "Edit number 1")
    expect_equal(reads, 3L)
  })
})

test_that("each edit, and an empty list, is a list card like Extraction's drafts and Change Requests'", {
  html <- htmltools::renderTags(history_entry_ui(many_records(1)[[1]]))$html
  for (part in c("list-card", "list-card-body", "list-card-title-row", "list-card-title", "list-card-meta", "list-card-actions")) {
    expect_match(html, paste0('class="', part, '"'), fixed = TRUE, info = part)
  }
  expect_match(htmltools::renderTags(no_history(filtered = FALSE))$html, 'class="list-card is-empty"', fixed = TRUE)
  expect_match(htmltools::renderTags(no_drafts("mine"))$html, 'class="list-card is-empty"', fixed = TRUE)
  expect_match(htmltools::renderTags(no_change_requests("me"))$html, 'class="list-card is-empty"', fixed = TRUE)
})
