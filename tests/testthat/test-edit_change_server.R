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
    expect_match(aside, "Open in this questionnaire")
    expect_match(paste(output$impact$html, collapse = ""), 'data-kind="placement" data-verb="view" data-scope="1" data-key="104"', fixed = TRUE)
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
    expect_match(review, "1 study will lose their contact investigator")
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
