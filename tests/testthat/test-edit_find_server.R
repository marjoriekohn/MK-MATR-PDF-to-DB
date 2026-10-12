# The cards of the rows staff can change or delete: one for each row, narrowed by typing,
# a page at a time, and the one picked shown on its own.

# The module's reactive arguments, as reactive values so a test can change them.
find_page <- function(kind = "registry", verb = "update", scope = "", snapshot = NULL) {
  entity <- shiny::reactiveVal(edit_entities[[kind]])
  verb <- shiny::reactiveVal(verb)
  scope <- shiny::reactiveVal(scope)
  tables <- shiny::reactiveVal(snapshot %||% test_snapshot(edit_service_tables(edit_entities[[kind]])))
  picked <- shiny::reactiveVal(NULL)
  list(
    entity = entity, verb = verb, scope = scope, snapshot = tables, picked = picked,
    args = list(entity = entity, verb = verb, snapshot = tables, scope = scope, picked = picked)
  )
}

# The sample database with `n` registries.
with_registries <- function(n) {
  test_snapshot(edit_service_tables(edit_entities$registry), change = function(snap) {
    registries <- snap$registry[rep(1, n), ]
    registries$registry_id <- seq_len(n)
    registries$name <- sprintf("Registry number %02d", seq_len(n))
    snap$registry <- registries
    snap
  })
}

test_that("every row of the kind is a card, narrowed by the words typed once typing pauses", {
  page <- find_page()
  shiny::testServer(edit_find_server, args = page$args, {
    expect_match(text_of(output$search), "Which registry do you want to update?")
    all <- text_of(output$cards)
    expect_match(all, "Mid-Atlantic Twin Registry")
    expect_match(all, "Example Registry")
    expect_equal(nrow(cards()), 2)

    session$setInputs(query = "twin")
    session$elapse(query_delay_ms / 2)
    expect_equal(nrow(cards()), 2) # typing hasn't paused yet
    session$elapse(query_delay_ms)
    expect_equal(nrow(cards()), 1)
    expect_match(text_of(output$cards), "Mid-Atlantic Twin Registry")
    expect_false(grepl("Example Registry", text_of(output$cards)))
  })
})

test_that("words that match nothing are said not to", {
  page <- find_page()
  shiny::testServer(edit_find_server, args = page$args, {
    session$setInputs(query = "  zzz ")
    session$elapse(query_delay_ms + 50)
    expect_equal(text_of(output$cards), "Nothing matches “zzz”. Try fewer or different words.")
  })
})

test_that("a long list shows a page at a time, and Show more adds the next page", {
  page <- find_page(snapshot = with_registries(60))
  shiny::testServer(edit_find_server, args = page$args, {
    expect_match(text_of(output$cards), "Showing 24 of 60")
    expect_match(text_of(output$cards), "Show 24 more")
    expect_match(text_of(output$cards), "Registry number 24")
    expect_false(grepl("Registry number 25", text_of(output$cards)))

    session$setInputs(action = list(action = "more"))
    expect_match(text_of(output$cards), "Showing 48 of 60")
    expect_match(text_of(output$cards), "Show 12 more")
    session$setInputs(action = list(action = "more"))
    expect_match(text_of(output$cards), "60 registries")
    expect_false(grepl("Show [0-9]+ more", text_of(output$cards)))
  })
})

test_that("narrowing the list, or changing the verb, starts again at the first page", {
  page <- find_page(snapshot = with_registries(60))
  shiny::testServer(edit_find_server, args = page$args, {
    session$setInputs(action = list(action = "more"))
    expect_match(text_of(output$cards), "Showing 48 of 60")
    session$setInputs(query = "registry")
    session$elapse(query_delay_ms + 50)
    expect_match(text_of(output$cards), "Showing 24 of 60") # every registry still matches, but back to one page
    session$setInputs(action = list(action = "more"))
    page$verb("delete")
    session$flushReact()
    expect_match(text_of(output$search), "Which registry do you want to delete?")
    expect_match(text_of(output$cards), "Showing 24 of 60")
  })
})

test_that("picking a card shows just that one, with a way back to the list", {
  page <- find_page()
  shiny::testServer(edit_find_server, args = page$args, {
    session$setInputs(action = list(action = "pick", id = "1"))
    expect_equal(page$picked(), "1")
    picked_card <- text_of(output$cards)
    expect_match(picked_card, "Mid-Atlantic Twin Registry")
    expect_match(picked_card, "Pick a different one")
    expect_false(grepl("Example Registry", picked_card))

    session$setInputs(action = list(action = "unpick"))
    expect_null(page$picked())
    expect_match(text_of(output$cards), "Example Registry")
  })
})

test_that("with nothing to pick from, it says what to do", {
  empty <- test_snapshot(edit_service_tables(edit_entities$registry), change = function(snap) {
    snap$registry <- snap$registry[0, ]
    snap
  })
  page <- find_page(snapshot = empty)
  shiny::testServer(edit_find_server, args = page$args, {
    expect_equal(text_of(output$cards), "There are no registries in the database yet. Choose “add” above to make the first.")
  })
})

test_that("a kind that belongs to a questionnaire asks for one first, and says if it has none", {
  scoped <- edit_entities$questionnaire_component
  expect_equal(edit_find_nothing(scoped, ""), "Choose a questionnaire above to see its questionnaire components.")
  expect_equal(edit_find_nothing(scoped, "1"), "This questionnaire has no questionnaire components yet.")
  expect_match(edit_find_nothing(edit_entities$registry, ""), "There are no registries in the database yet")

  page <- find_page("questionnaire_component", scope = "")
  shiny::testServer(edit_find_server, args = page$args, {
    expect_equal(text_of(output$cards), "Choose a questionnaire above to see its questionnaire components.")
    page$scope("1")
    session$flushReact()
    expect_match(text_of(output$cards), "[0-9]+ questionnaire components?")
  })
})
