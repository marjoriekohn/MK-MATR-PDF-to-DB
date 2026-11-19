# The form for adding or changing a row, or the review before deleting one: what it says beside the form, how a
# pop-up's typed values are put back, and the token that keeps one form's fields from being read as the next
# one's. (The page's own tests, in test-make_edit_server.R, go through it too.)

# The module's reactive arguments, as reactive values so a test can change them.
change_page <- function(kind, verb, row_key = NULL, scope = "", prefill = NULL) {
  entity <- edit_entities[[kind]]
  snap <- test_snapshot(edit_service_tables(entity))
  row <- if (!is.null(row_key)) edit_service_row(entity, snap, row_key)
  list(
    prefill = shiny::reactiveVal(prefill),
    args = list(
      entity = shiny::reactiveVal(entity), verb = shiny::reactiveVal(verb), snapshot = shiny::reactiveVal(snap),
      row = shiny::reactiveVal(row), scope = shiny::reactiveVal(scope),
      prefill = NULL
    )
  )
}
change_args <- function(page) {
  page$args$prefill <- page$prefill
  page$args
}

# The browser draws the form and sends back its token; here the form is drawn by reading
# its output, and the token sent back by hand.
draw <- function(env = parent.frame()) {
  with(env, {
    output$main
    session$setInputs(form_token = as.character(generation()))
  })
}

test_that("what's typed counts only once the browser has sent back the form's token", {
  page <- change_page("registry", "update", "1")
  shiny::testServer(edit_change_server, args = change_args(page), {
    expect_equal(tryCatch(values(), shiny.silent.error = function(problem) "not yet"), "not yet")
    expect_false(ready())
    output$main
    session$setInputs(`registry__name` = "Typed too early", form_token = "stale")
    expect_equal(tryCatch(values(), shiny.silent.error = function(problem) "not yet"), "not yet")

    session$setInputs(form_token = as.character(generation()))
    expect_equal(values()$registry__name, "Typed too early")
  })
})

test_that("a form that is drawn again gets a new token, so the fields before are never read as its own", {
  page <- change_page("registry", "update", "1")
  shiny::testServer(edit_change_server, args = change_args(page), {
    output$main
    first <- generation()
    page$args$row(edit_service_row(edit_entities$registry, page$args$snapshot(), "2"))
    session$flushReact()
    output$main
    expect_equal(generation(), first + 1L)
    expect_equal(tryCatch(values(), shiny.silent.error = function(problem) "not yet"), "not yet")
  })
})

test_that("what a pop-up had typed is put back in the form, and a value no field has is ignored", {
  page <- change_page("registry", "update", "1", prefill = list(registry__name = "Typed before", registry__not_a_field = "ignored"))
  shiny::testServer(edit_change_server, args = change_args(page), {
    html <- paste(output$main$html, collapse = "")
    expect_match(html, 'value="Typed before"', fixed = TRUE) # the field is drawn with what was typed
    expect_false(grepl('value="Mid-Atlantic Twin Registry"', html, fixed = TRUE))
    expect_false(grepl("ignored", html, fixed = TRUE))
  })
})

test_that("a form can ask to go to another kind, with adding as the verb unless it says", {
  page <- change_page("component", "update", "4")
  shiny::testServer(edit_change_server, args = change_args(page), {
    expect_null(jump())
    session$setInputs(action = list(action = "jump", kind = "registry"))
    expect_equal(jump()$kind, "registry")
    expect_equal(jump()$verb, "add")
    session$setInputs(action = list(action = "jump", kind = "placement", verb = "view", scope = "1", key = "104"))
    expect_equal(jump()$kind, "placement")
    expect_equal(jump()$verb, "view")
    expect_equal(c(jump()$scope, jump()$key), c("1", "104"))
  })
})

test_that("changing something several questionnaires share says so, asks to confirm, and offers to open it in just one", {
  page <- change_page("component", "update", "4")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    form <- text_of(output$main)
    expect_match(form, "I understand this changes it in all 2 questionnaires that use it.")
    aside <- text_of(output$impact)
    expect_match(aside, "Shared by 2 questionnaires")
    expect_match(aside, "press View in Questionnaire on its card below")
    expect_match(paste(output$impact$html, collapse = ""), 'data-kind="placement" data-verb="view" data-key="104" data-scope="1"', fixed = TRUE)
    expect_match(aside, "AHP|Adult Health and Personality")
    expect_match(aside, "MQ|Mother Questionnaire")
    session$setInputs(component__text = "do you currently smoke cigarettes?")
    expect_false(ready())
    expect_equal(blockers(), "Tick the box to confirm it changes everywhere it's used.")
    session$setInputs(confirm_shared = TRUE)
    expect_true(ready())
    expect_equal(built()$items$entity_name, "component")
  })
})

test_that("something that is not shared has no such box, and its aside says nothing else uses it", {
  page <- change_page("component", "update", "3")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    expect_false(grepl("I understand this changes it in all", text_of(output$main)))
  })
  page <- change_page("component", "update", "15")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    expect_match(text_of(output$impact), "No questionnaire prints this yet, so changing it affects nothing else.")
  })
})

test_that("adding a study asks only for the study: what it is is said in the info block, above the form", {
  page <- change_page("study", "add")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    main <- paste(as.character(output$main$html), collapse = "")
    expect_match(main, "study__name")
    expect_false(grepl("questionnaire__name|wave__name", main))
    # the form sits in the page's details, under their own title, without a card or heading of its own
    expect_false(grepl("Fill in the details|class=\"card|edit-columns", main))
    expect_equal(blockers()[1], "Fill in the registry and name.")
  })
})

test_that("a row in the details is its fields as text, with where it is used, and nothing to send", {
  page <- change_page("registry", "view", "1")
  shiny::testServer(edit_change_server, args = change_args(page), {
    main <- paste(as.character(output$main$html), collapse = "")
    expect_match(text_of(main), "Mid-Atlantic Twin Registry", fixed = TRUE)
    expect_match(text_of(main), "MATR")
    expect_match(main, 'class="detail-value"')
    expect_false(grepl("<input|<select|<textarea", main))
    expect_false(ready())
    expect_equal(blockers(), "Press Edit to change it.")
    # Edit: the same row's form
    page$args$verb("update")
    draw()
    expect_match(paste(as.character(output$main$html), collapse = ""), "<input")
  })
})

test_that("a questionnaire's details are its study as tags, then what its form changes, then its waves with View", {
  page <- change_page("questionnaire", "view", "1")
  shiny::testServer(edit_change_server, args = change_args(page), {
    main <- paste(as.character(output$main$html), collapse = "")
    shown <- text_of(main)
    expect_match(shown, "^Mid-Atlantic Twin Registry \\(MATR\\) Adult Health Study Name ") # the tags are the names alone
    expect_false(grepl("Status|Dana Reyes|Jo Park", shown)) # the study's own: on the study
    expect_match(main, 'class="edit-context-pill is-link"', fixed = TRUE)
    # the study isn't said again among the lines Edit changes
    lines <- sub("^.*edit-view", "", main)
    expect_false(grepl("Adult Health Study", text_of(lines)))
    expect_match(text_of(lines), "Name Adult Health and Personality Respondent Type Adult (18+)", fixed = TRUE)
    expect_match(shown, "On the dashboard Visible AHP Wave 1 AHP Wave 2$") # its waves: cards, each pressed to open it
    expect_match(main, 'class="edit-use-card" data-action="jump" aria-label="View Wave: AHP Wave 1"', fixed = TRUE)
    expect_false(grepl("2 waves", shown))
    expect_false(grepl("blocks", shown)) # not everything it prints
    # a tag asks the page to open what it names
    session$setInputs(action = list(action = "jump", kind = "study", verb = "view", key = "1"))
    expect_equal(jump()[c("kind", "verb", "key")], list(kind = "study", verb = "view", key = "1"))
  })
})

test_that("a form only looked at has nothing to lose; one typed in does", {
  page <- change_page("registry", "update", "1")
  shiny::testServer(edit_change_server, args = change_args(page), {
    expect_false(dirty()) # not drawn yet
    draw()
    expect_false(dirty())
    session$setInputs(registry__name = "Mid-Atlantic Twin Registry ") # a space isn't a change
    expect_false(dirty())
    session$setInputs(registry__name = "Mid-Atlantic Twins")
    expect_true(dirty())
    session$setInputs(registry__name = "Mid-Atlantic Twin Registry")
    expect_false(dirty()) # put back
  })
  # a new one that starts with something (a question, from the Questions list) isn't typed in yet either
  page <- change_page("placement", "add", scope = "1", prefill = list(placement__kind = "Question"))
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    expect_false(dirty())
    session$setInputs(placement__text = "How tall are you?")
    expect_true(dirty())
  })
  # and what is only shown has no form
  page <- change_page("registry", "view", "1")
  shiny::testServer(edit_change_server, args = change_args(page), expect_false(dirty()))
})

test_that("a merge waits for the one to keep and for the box to be ticked, then is the plan's items", {
  page <- change_page("component", "merge", "11")
  shiny::testServer(edit_change_server, args = change_args(page), {
    expect_true(active())
    expect_false(ready()) # not drawn yet
    draw()
    expect_match(text_of(output$main), "This question is removed did you smoke during your pregnancy?", fixed = TRUE)
    expect_equal(blockers(), "Choose the one to keep.")
    session$setInputs(merge_into = "4")
    expect_match(text_of(output$merge_preview), "This question stays do you now smoke cigarettes?", fixed = TRUE)
    expect_equal(blockers(), "Tick the box to confirm the two become one.")
    expect_false(ready())
    session$setInputs(confirm_merge_read = TRUE)
    expect_true(ready())
    expect_equal(built()$items$action, c("Update", "Delete", "Delete"))
    expect_match(built()$summary, "^Merged the question")
    expect_equal(built()$merge$key, "4") # the one the page shows afterwards
    # something that isn't one of its kind is no plan
    session$setInputs(merge_into = "1")
    expect_false(ready())
    expect_equal(blockers()[1], "Choose the one to keep.")
  })
  # two that can't be merged are never ready, whatever is ticked
  page <- change_page("answer_option", "merge", "14")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    session$setInputs(merge_into = "13", confirm_merge_read = TRUE)
    expect_false(ready())
    expect_equal(blockers(), "These two can't be merged yet: see what's in the way above.")
    expect_match(text_of(output$merge_preview), "3 variables offer both")
  })
})

test_that("new words for a block in one questionnaire say they change that place only", {
  page <- change_page("placement", "update", "404", scope = "4")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    expect_false(grepl("New words change this place only", text_of(output$impact)))
    session$setInputs(placement__text = "Do you currently smoke cigarettes?")
    aside <- text_of(output$impact)
    expect_match(aside, "New words change this place only")
    expect_match(aside, "The old one stays, for the 1 other place that still print it.")
    expect_true(ready())
    expect_equal(built()$items$action, c("Insert", "Insert", "Update"))
  })
})

test_that("a question's fields on a block's add form are read once Question is the kind, and the variables start as one", {
  page <- change_page("placement", "add", scope = "1")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    session$setInputs(placement__kind = "Section", placement__text = "C. Habits")
    expect_true(ready())
    session$setInputs(placement__kind = "Question")
    expect_false(ready())
    expect_equal(blockers()[1], "Fill in the waves that ask it.")
    session$setInputs(placement__asked_in = "1")
    expect_equal(blockers()[1], "Fix the fields outlined in red.")
    expect_equal(text_of(output$note_9), "Variable 1: fill in its MATR ID.") # the variables field is the ninth
    expect_length(edit_variables_parse(values()$placement__variables), 1)
  })
})

test_that("a delete shows what it would take with it, and what to tick and type", {
  page <- change_page("investigator", "delete", "4")
  shiny::testServer(edit_change_server, args = change_args(page), {
    expect_true(active())
    expect_false(stuck())
    draw()
    review <- text_of(output$main)
    expect_match(review, "Jo Park")
    expect_match(review, "1 study will lose their contact Adult Health Study")
    expect_equal(blockers(), c("Tick the box to confirm you've read what will be deleted.", "Type DELETE to be sure."))
    session$setInputs(confirm_read = TRUE, confirm_word = "delete")
    expect_true(ready())
    expect_equal(built()$items$action, c("Update", "Delete", "Delete"))
  })
  # one something stands in the way of is stuck, with no confirmation
  page <- change_page("registry", "delete", "1")
  shiny::testServer(edit_change_server, args = change_args(page), {
    expect_true(stuck())
    expect_false(ready())
    expect_match(text_of(output$main), "2 studies are in the way")
    expect_false(grepl("confirm_read", paste(output$main$html, collapse = "")))
  })
})

test_that("with nothing chosen yet, the form is blank and says to choose first", {
  page <- change_page("registry", "update")
  shiny::testServer(edit_change_server, args = change_args(page), {
    expect_false(active())
    expect_equal(blockers(), "Choose what to change first.")
    expect_false(ready())
    expect_false(stuck())
  })
})
