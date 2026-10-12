# The form for adding or changing a row, or the review before deleting one: what it says
# beside the form, how a draft being opened again puts back what was typed, and the token
# that keeps one form's fields from being read as the next one's. (The page's own tests,
# in test-make_edit_server.R, go through it too.)

# The module's reactive arguments, as reactive values so a test can change them.
change_page <- function(kind, verb, row_key = NULL, scope = "", resume = NULL) {
  entity <- edit_entities[[kind]]
  snap <- test_snapshot(edit_service_tables(entity))
  row <- if (!is.null(row_key)) edit_service_row(entity, snap, row_key)
  list(
    resume = shiny::reactiveVal(resume),
    args = list(
      entity = shiny::reactiveVal(entity), verb = shiny::reactiveVal(verb), snapshot = shiny::reactiveVal(snap),
      row = shiny::reactiveVal(row), scope = shiny::reactiveVal(scope),
      resume = NULL
    )
  )
}
change_args <- function(page) {
  page$args$resume <- page$resume
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

test_that("a draft opened again puts back what was typed, once, when it is the same form", {
  saved <- list(form = list(
    entity = "registry", action = "update", scope = "", target = "1",
    values = list(registry__name = "Typed before", registry__not_a_field = "ignored")
  ))
  page <- change_page("registry", "update", "1", resume = saved)
  shiny::testServer(edit_change_server, args = change_args(page), {
    html <- paste(output$main$html, collapse = "")
    expect_match(html, 'value="Typed before"', fixed = TRUE) # the field is drawn with what was typed
    expect_false(grepl('value="Mid-Atlantic Twin Registry"', html, fixed = TRUE))
    expect_false(grepl("ignored", html, fixed = TRUE))
    expect_null(page$resume()) # once
  })
})

test_that("a draft of a different form is left to be opened when its form comes up", {
  saved <- list(form = list(
    entity = "registry",
    action = "update",
    scope = "",
    target = "2",
    values = list(registry__name = "Typed before")
  ))
  page <- change_page("registry", "update", "1", resume = saved)
  shiny::testServer(edit_change_server, args = change_args(page), {
    html <- paste(output$main$html, collapse = "")
    expect_false(grepl("Typed before", html, fixed = TRUE))
    expect_match(html, 'value="Mid-Atlantic Twin Registry"', fixed = TRUE)
    expect_equal(page$resume()$form$target, "2") # not this row's form: kept
  })
})

test_that("a form can ask to go to another kind, with adding as the verb unless it says", {
  page <- change_page("question", "update", "4")
  shiny::testServer(edit_change_server, args = change_args(page), {
    expect_null(jump())
    session$setInputs(action = list(action = "jump", kind = "registry"))
    expect_equal(jump()$kind, "registry")
    expect_equal(jump()$verb, "add")
    session$setInputs(action = list(action = "jump", kind = "questionnaire_question", verb = "update"))
    expect_equal(jump()$kind, "questionnaire_question")
    expect_equal(jump()$verb, "update")
  })
})

test_that("changing something several questionnaires share says so, asks to confirm, and offers to change it in just one", {
  page <- change_page("question", "update", "4")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    form <- text_of(output$main)
    expect_match(form, "I understand this changes it in all 2 questionnaires that use it.")
    aside <- text_of(output$impact)
    expect_match(aside, "Shared by 2 questionnaires")
    expect_match(aside, "Change it in one questionnaire only")
    expect_match(aside, "AHP|Adult Health and Personality")
    expect_match(aside, "MQ|Mother Questionnaire")
  })
})

test_that("something that is not shared has no such box, and its aside says nothing else uses it", {
  page <- change_page("question", "update", "3")
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

test_that("a kind that comes with more says what else the database needs, and adds it in the same edit", {
  page <- change_page("study", "add")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    aside <- text_of(output$impact)
    expect_match(aside, "What you're adding")
    expect_match(aside, "It comes with more The database needs a questionnaire and a wave for it, so you'll add them in the same edit.")
    expect_match(text_of(output$main), "Fill in the details")
  })
})

test_that("new words for a block in one questionnaire say the questionnaire gets its own component", {
  page <- change_page("questionnaire_component", "update", "4|4", scope = "4")
  shiny::testServer(edit_change_server, args = change_args(page), {
    draw()
    expect_match(text_of(output$impact), "This changes MQ only")
    expect_false(grepl("New words mean a new component", text_of(output$impact)))

    session$setInputs(questionnaire_component__text = "Do you currently smoke cigarettes?")
    aside <- text_of(output$impact)
    expect_match(aside, "New words mean a new component")
    expect_match(aside, "The old one stays, for the 1 other questionnaire that still print it.")
    expect_true(ready() || length(blockers()) > 0)
  })
})

test_that("a delete shows what it would take with it, and nothing to fill in", {
  page <- change_page("investigator", "delete", "4")
  shiny::testServer(edit_change_server, args = change_args(page), {
    expect_true(active())
    expect_false(stuck())
    draw()
    review <- text_of(output$main)
    expect_match(review, "Jo Park")
    expect_match(blockers()[1], "Tick the box")
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
