# The AI Monitor page: its numbers, chart, prompts and commonest corrections, from the pushed extractions. The
# extractions are made up (test-model_monitor.R reads real ones), so the page is tested without the database.

monitor_page_records <- function() {
  edit <- function(change, item, field = NULL, before = NULL, after = NULL, where = "Question 1") {
    list(change = change, item = item, where = where, field = field, before = before, after = after)
  }
  record <- function(id, applied, edits, prompt_version = "abc123") {
    list(
      id = id, summary = paste("Add Questionnaire", id), pdf_name = "q.pdf", applied = applied,
      model = "gemini-3.8-flash", prompt_version = prompt_version, edits = edits
    )
  }
  list(
    "1" = record("1", "2026-10-01T12:00:00Z", list(
      edit("edited", "variable", "unit", "years old", "years", where = "Question 26 > variable 1"),
      edit("edited", "variable", "unit", "years old", "years", where = "Question 31 > variable 1"),
      edit("edited", "choice", "answer_option", "a few times a week", "a few times [a week]")
    )),
    "2" = record("2", "2026-10-02T12:00:00Z", list(), prompt_version = "def456")
  )
}

monitor_args <- function(active = NULL) list(connection = NULL, active = active)

test_that("the page shows the numbers, a bar for the corrections, each prompt, and the commonest kind first", {
  local_stub("monitor_service_get_all", function(connection) monitor_page_records())
  shiny::testServer(ai_monitor_server, args = monitor_args(), {
    errors <- collect_errors(session)
    session$setInputs(item = "all")
    totals <- text_of(output$totals)
    expect_match(totals, "2 Pushed AI extractions")
    expect_match(totals, "3 Corrections")
    expect_match(totals, "1.5 Corrections per extraction")
    expect_match(totals, "1 Pushed as the AI read them")
    expect_match(output$chart$html, "Oct 1: 3 corrections in 1 extraction", fixed = TRUE)
    prompts <- text_of(output$by_prompt)
    expect_match(prompts, "def456 gemini-3.8-flash · last pushed on Oct 2(, 2026)? 1 0.0")
    expect_lt(regexpr("def456", prompts), regexpr("abc123", prompts)) # the one used last first
    listed <- text_of(output$common)
    expect_match(listed, "Changed a variable's unit 2 corrections in 1 extraction \\(50% of pushed\\)")
    expect_match(listed, "years old → years 2 times in 1 extraction · first at Question 26 > variable 1 in Add Questionnaire 1", fixed = TRUE)
    expect_lt(regexpr("variable's unit", listed), regexpr("choice's wording", listed))
    expect_length(errors(), 0)
  })
})

test_that("picking blocks, variables or choices narrows the corrections, and none of one sort says so", {
  local_stub("monitor_service_get_all", function(connection) monitor_page_records())
  shiny::testServer(ai_monitor_server, args = monitor_args(), {
    session$setInputs(item = "choice")
    expect_equal(vapply(common(), function(kind) kind$label, character(1)), "Changed a choice's wording")
    session$setInputs(item = "block")
    expect_equal(text_of(output$common), "No pushed extraction has a correction of that kind yet.")
    session$setInputs(item = "all")
    expect_length(common(), 2)
  })
})

test_that("with nothing pushed yet, the page says so", {
  local_stub("monitor_service_get_all", function(connection) list())
  shiny::testServer(ai_monitor_server, args = monitor_args(), {
    session$setInputs(item = "all")
    expect_match(text_of(output$totals), "0 Pushed AI extractions")
    expect_match(text_of(output$chart), "Nothing has been pushed yet")
    expect_match(text_of(output$by_prompt), "No prompt has read a pushed extraction yet.")
  })
})

test_that("opening the page reads the extractions again, and a database that can't be read keeps the last ones", {
  reads <- 0
  down <- FALSE
  local_stub("monitor_service_get_all", function(connection) {
    reads <<- reads + 1
    if (down) stop("The database is down.", call. = FALSE)
    monitor_page_records()
  })
  seen <- local_browser()
  on_page <- shiny::reactiveVal(FALSE)
  shiny::testServer(ai_monitor_server, args = monitor_args(active = on_page), {
    session$setInputs(item = "all")
    expect_length(records(), 2)
    before <- reads
    on_page(TRUE)
    session$flushReact()
    records()
    expect_gt(reads, before)
    down <<- TRUE # (in the test's environment, where the stub reads it; a plain `<-` here would make a new one)
    on_page(FALSE)
    session$flushReact()
    on_page(TRUE)
    session$flushReact()
    expect_length(records(), 2) # what was read last
    expect_match(paste(notice_texts(seen), collapse = " "), "The database is down.")
  })
})
