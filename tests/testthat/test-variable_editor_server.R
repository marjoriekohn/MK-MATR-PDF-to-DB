# The variable editor: one for Edit Extraction (its outline's tests are test-edit_extraction_outline_server.R) and Make
# an edit, where it edits the variables of the question being added and gives the form them back as JSON.

# The editor's variables as drawn, and the form's JSON of some variables.
cards_html <- function(output) paste(output$cards$html, collapse = "")
variables_json_of <- function(...) as.character(jsonlite::toJSON(list(...), auto_unbox = TRUE))

editor_options <- list(
  subjects = c("Siblings", "Children"), time_frames = "in your lifetime", units = c("pounds", "years"),
  answer_data_types = c("NUMBER", "TEXT", "DATE", "TIME"), answer_options = c("Yes", "No")
)

test_that("Make an edit's editor draws the variables it is given, and gives back what is changed, as JSON", {
  seen <- local_browser()
  shiny::testServer(variable_editor_server, {
    expect_null(session$returned$value()) # nothing until the form gives it the variables
    session$returned$load(edit_variables_default(), editor_options)
    expect_equal(session$returned$value(), edit_variables_default())
    html <- cards_html(output)
    expect_match(html, 'data-field="matr_id"')
    expect_match(html, 'data-action="pick_variable"') # + Variable, under them
    expect_match(html, '<option value="Siblings">') # who a variable can be about, offered as it is typed
    expect_match(html, '<option value="Yes">') # the answer choices
    expect_match(html, '<option value="years">') # the units a write-in can be in

    session$setInputs(edit = list(table = "variables", id = "v1", field = "matr_id", value = "AH6", typed = TRUE))
    session$setInputs(action = list(action = "add_choice", id = "v1"))
    session$setInputs(edit = list(table = "response_options", id = "o2", field = "answer_option", value = "No", typed = TRUE))
    variables <- jsonlite::fromJSON(session$returned$value(), simplifyVector = FALSE)
    expect_equal(variables[[1]]$matr_id, "AH6")
    expect_equal(vapply(variables[[1]]$response_options, function(choice) choice$answer_option, ""), c("", "no"))
    expect_equal(vapply(variables[[1]]$response_options, function(choice) choice$code_value, ""), c("1", "2"))

    # + Variable asks Choices or Write-in; the new one is like the one above (who it's about, its choices)
    session$setInputs(action = list(action = "pick_variable", id = "c1"))
    expect_match(utils::tail(seen$modals, 1)[[1]], "Write-in")
    session$setInputs(action = list(action = "add_box", id = "c1", type = "open_ended"))
    expect_equal(jsonlite::fromJSON(session$returned$value())$variable_type, c("closed_ended", "open_ended"))

    # a delete says nothing: the draft's note offers Undo, which this page hasn't got
    session$setInputs(action = list(action = "delete", table = "response_options", id = "o2"))
    expect_length(jsonlite::fromJSON(session$returned$value(), simplifyVector = FALSE)[[1]]$response_options, 1)
    expect_length(seen$notifications, 0)
  })
})

test_that("on Make an edit a write-in has its answer type and unit, a choice its code and words, and there's no Apply to All?", {
  local_browser()
  shiny::testServer(variable_editor_server, {
    session$returned$load(variables_json_of(list(
      variable_type = "open_ended", matr_id = "AH9", subject = "", label = "", answer_data_type = "NUMBER", unit = "years"
    ), list(
      variable_type = "closed_ended", matr_id = "AH10", subject = "", label = "",
      response_options = list(list(code_value = "1", answer_option = "yes"))
    )), editor_options) # v2's choices are open, as every variable's are until closed
    html <- cards_html(output)
    expect_match(html, 'data-table="variables" data-id="v1" data-field="answer_data_type"')
    expect_match(html, 'value="years"[^>]*data-table="variables" data-id="v1" data-field="unit"')
    expect_match(html, 'data-table="response_options" data-id="o1" data-field="code_value"')
    expect_match(html, 'data-table="response_options" data-id="o1" data-field="answer_option"')
    expect_false(grepl("skip|respondent_count", html)) # routing and counts aren't kept
    expect_false(grepl("choice-apply", html))

    # an optional field added shows, empty, with the cursor in it; taking it away hides it again
    session$setInputs(action = list(action = "show_field", id = "v2", name = "label"))
    added <- 'data-id="v2" data-field="label"[^>]*data-focus-now|data-focus-now[^>]*data-id="v2" data-field="label"'
    expect_match(cards_html(output), added)
    session$setInputs(action = list(action = "hide_field", id = "v2", name = "label"))
    expect_false(grepl('data-id="v2" data-field="label"', cards_html(output)))
  })
})

test_that("a change the draft refuses leaves the variables as they were, and says why", {
  seen <- local_browser()
  shiny::testServer(variable_editor_server, {
    session$returned$load(edit_variables_default(NULL), editor_options)
    before <- session$returned$value()
    session$setInputs(reorder = list(table = "variables", ids = list("v1", "v9"))) # a variable that isn't there
    expect_equal(session$returned$value(), before)
  })
})
