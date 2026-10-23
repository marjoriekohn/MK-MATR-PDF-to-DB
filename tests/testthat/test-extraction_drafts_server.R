# The Extraction Drafts page: a card for each draft still being prepared, whose it is,
# and what its buttons do. The database is the real one.

drafts_page <- function(me, extractions = test_extractions()) {
  did <- new.env()
  did$reviews <- list()
  did$details <- list()
  did$modes <- character()
  did$take_over <- logical()
  list(
    did = did,
    args = list(
      extractions = extractions, catalog = test_live_catalog(), connection = test_connection, me = me,
      open_edit_extraction = function(record, take_over = FALSE) {
        did$reviews[[length(did$reviews) + 1]] <- record
        did$take_over <- c(did$take_over, take_over)
      },
      open_details = function(record) did$details[[length(did$details) + 1]] <- record,
      switch_mode = function(mode) did$modes <- c(did$modes, mode)
    )
  )
}

# Four drafts that are open (two mine, one of them still being read, and a colleague's), one set aside and
# one already sent for approval.
seed_drafts <- function(me, colleague, tree, env = parent.frame()) {
  list(
    mine = local_read_extraction(me, saved = "2026-10-02T12:00:00Z", info = picked_basic_info(tree), env = env),
    theirs = local_read_extraction(colleague, saved = "2026-10-02T11:00:00Z", info = picked_basic_info(tree), env = env),
    reading = local_extraction(me, saved = "2026-10-02T10:00:00Z", info = picked_basic_info(tree), env = env),
    set_aside = local_extraction(me, status = "Withdrawn", info = picked_basic_info(tree), env = env),
    sent = local_extraction(me, status = "Awaiting Review", info = picked_basic_info(tree), env = env)
  )
}

ids_shown <- function(shown) vapply(shown, function(record) record$id, character(1))
# Only the test's own drafts: the dev database has real ones too.
ours <- function(shown, drafts) intersect(ids_shown(shown), vapply(drafts, function(record) record$id, character(1)))
card_title <- function(tree) paste0(tree$questionnaire$name, " (ZZQ) · ", tree$wave$name)
count_on <- function(toggle, label) as.integer(sub(sprintf(".*%s \\(([0-9]+)\\).*", label), "\\1", toggle))

test_that("the open drafts are cards, everyone's by default, with whose they are and how far they have got", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  drafts <- seed_drafts(me, colleague, tree)
  page <- drafts_page(me)
  shiny::testServer(extraction_drafts_server, args = page$args, {
    errors <- collect_errors(session)
    expect_setequal(ours(shown(), drafts), c(drafts$mine$id, drafts$theirs$id, drafts$reading$id))
    cards <- text_of(output$drafts)
    expect_match(cards, card_title(tree), fixed = TRUE) # named from where it goes
    expect_match(cards, paste("Started by", colleague$name))
    expect_match(cards, "Started by you")
    expect_match(cards, "Yours")
    expect_match(cards, "Read by Gemini")
    # expect_false(grepl("Wave 6|Wave 7", cards)) # set aside and sent: not open
    expect_length(errors(), 0)
  })
})

test_that("Mine keeps only this person's drafts, and the buttons count each list", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  drafts <- seed_drafts(me, colleague, tree)
  page <- drafts_page(me)
  shiny::testServer(extraction_drafts_server, args = page$args, {
    toggle <- text_of(output$whose_toggle)
    expect_equal(count_on(toggle, "Mine"), 2L)
    expect_gte(count_on(toggle, "Everyone's"), 3L) # the dev database has other people's drafts too
    session$setInputs(whose = "mine")
    expect_setequal(ids_shown(shown()), c(drafts$mine$id, drafts$reading$id))
    session$setInputs(whose = "everyone")
    expect_length(ours(shown(), drafts), 3)
  })
})

test_that("Continue opens a draft that has been read on the Edit Extraction page, and one still being read in AI-Powered mode", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  drafts <- seed_drafts(me, colleague, tree)
  page <- drafts_page(me)
  shiny::testServer(extraction_drafts_server, args = page$args, {
    session$setInputs(action = list(action = "continue", id = drafts$mine$id))
    expect_equal(page$did$reviews[[1]]$id, drafts$mine$id)
    session$setInputs(action = list(action = "continue", id = drafts$reading$id))
    expect_equal(page$did$details[[1]]$id, drafts$reading$id)
    session$setInputs(action = list(action = "new"))
    expect_equal(page$did$modes, "ai")
  })
})

test_that("a reading that failed offers Try reading again, which queues it", {
  me <- local_staff()
  tree <- shared_tree()
  failed <- local_extraction(me, extraction_status = "failed", error = "Gemini stopped before the end.", info = picked_basic_info(tree))
  page <- drafts_page(me)
  shiny::testServer(extraction_drafts_server, args = page$args, {
    cards <- text_of(output$drafts)
    expect_match(cards, "Reading stopped")
    expect_match(cards, "Try reading again")
    session$setInputs(action = list(action = "retry", id = failed$id))
    expect_equal(extraction_model_get(test_connection, failed$id)$extraction_status, "queued")
  })
})

test_that("Discard asks first, then sets the draft aside", {
  me <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  mine <- local_read_extraction(me, info = picked_basic_info(tree))
  page <- drafts_page(me)
  shiny::testServer(extraction_drafts_server, args = page$args, {
    session$setInputs(action = list(action = "discard", id = mine$id))
    expect_match(seen$modals[[1]], paste("Discard this draft?", card_title(tree)), fixed = TRUE)
    session$setInputs(confirm_discard = 1)
    expect_equal(extraction_model_get(test_connection, mine$id)$status, "Withdrawn")
    expect_equal(notice_texts(seen), "Draft discarded.")
    expect_false(mine$id %in% ids_shown(shown()))
  })
})

test_that("with no drafts the page says so, in either list", {
  me <- local_staff()
  page <- drafts_page(me, fixed_extractions())
  shiny::testServer(extraction_drafts_server, args = page$args, {
    expect_match(text_of(output$drafts), "There are no drafts in progress.")
    session$setInputs(whose = "mine")
    expect_match(text_of(output$drafts), "You have no drafts in progress.")
    expect_match(text_of(output$drafts), "Start a new extraction")
  })
})

# The buttons on the card of one draft, as HTML.
card_buttons <- function(html, id) {
  buttons <- regmatches(html, gregexpr("<button[^>]*>", html))[[1]]
  buttons[grepl(sprintf('data-id="%s"', id), buttons, fixed = TRUE)]
}

test_that("a draft someone else is working on says so, and Continue gives way to Take over, which asks first", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  seen <- local_browser()
  theirs <- local_read_extraction(colleague, info = picked_basic_info(tree))
  mine <- local_read_extraction(me, info = picked_basic_info(tree))
  draft_row_service_claim(test_connection, theirs$id, colleague)
  draft_row_service_claim(test_connection, mine$id, me) # this person's own claim isn't someone else's
  page <- drafts_page(me)
  shiny::testServer(extraction_drafts_server, args = page$args, {
    errors <- collect_errors(session)
    html <- as.character(output$drafts$html)
    expect_match(text_of(output$drafts), paste(colleague$name, "is actively working on this"), fixed = TRUE)
    expect_false(grepl(paste(me$name, "is actively working"), text_of(output$drafts), fixed = TRUE))

    buttons <- card_buttons(html, theirs$id)
    continue <- buttons[grepl('data-action="continue"', buttons)]
    expect_match(continue, "disabled")
    expect_true(any(grepl('data-action="take_over"', buttons)))
    expect_false(any(grepl("disabled", card_buttons(html, mine$id)[grepl('data-action="continue"', card_buttons(html, mine$id))])))

    session$setInputs(action = list(action = "take_over", id = theirs$id))
    expect_match(seen$modals[[length(seen$modals)]], paste0("Take this draft over from ", colleague$name, "?"), fixed = TRUE)
    expect_length(page$did$reviews, 0) # nothing opens until it is confirmed
    session$setInputs(confirm_take_over = 1)
    expect_equal(page$did$reviews[[1]]$id, theirs$id)
    expect_true(page$did$take_over[1])
    expect_length(errors(), 0)
  })
})

test_that("a claim that has lapsed isn't shown, and Take over then just opens the draft", {
  me <- local_staff()
  colleague <- local_staff()
  tree <- shared_tree()
  theirs <- local_read_extraction(colleague, info = picked_basic_info(tree))
  draft_row_service_claim(test_connection, theirs$id, colleague)
  lapsed <- function(records) {
    lapply(records, function(record) {
      if (!is.null(record$editing)) record$editing$at <- format_time(Sys.time() - draft_claim_stale_seconds - 5)
      record
    })
  }
  extractions <- test_extractions()
  page <- drafts_page(me, list(all = shiny::reactive(lapsed(extractions$all())), changed = extractions$changed))
  shiny::testServer(extraction_drafts_server, args = page$args, {
    expect_false(grepl("is actively working", text_of(output$drafts), fixed = TRUE))
    session$setInputs(action = list(action = "take_over", id = theirs$id))
    expect_equal(page$did$reviews[[1]]$id, theirs$id)
    expect_false(page$did$take_over[1])
  })
})
