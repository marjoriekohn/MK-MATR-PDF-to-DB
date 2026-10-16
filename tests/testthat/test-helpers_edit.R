# What the edit pages draw with: a field, what changed, where a row appears, and what a
# delete reaches. The page tests go through most of it; these look at the pieces on
# their own, as words on a screen.

test_that("every field of every kind can be drawn, named for screen readers but with no label above it", {
  drawn <- 0L
  for (entity in edit_entities) {
    snap <- test_snapshot(edit_service_tables(entity))
    scope <- if (!is.null(entity$scope)) "1"
    for (mode in c("add", "update")) {
      if (!is.null(edit_service_can(entity, snap, mode))) next
      row <- if (mode == "update") edit_service_row(entity, snap, edit_service_cards(entity, snap, scope)$key[1])
      fields <- edit_service_fields(entity, snap, mode, row, scope)
      initial <- edit_service_initial(entity, fields, snap, mode, row, scope)
      for (i in seq_along(fields)) {
        field <- fields[[i]]
        html <- paste(as.character(edit_field_ui(shiny::NS("page"), field, initial[[field$id]], paste0("note_", i))), collapse = "")
        info <- paste(entity$key, mode, field$id)
        expect_match(html, paste0('aria-label="', htmltools::htmlEscape(field$label, attribute = TRUE), '"'), fixed = TRUE, info = info)
        expect_false(grepl('class="control-label"|form-label|required-mark', html), info = info)
        drawn <- drawn + 1L
      }
    }
  }
  expect_gt(drawn, 80) # so a kind that stopped being drawn would be noticed
})

test_that("a locked field shows its value and why it can't be changed, and is not an input", {
  form <- form_for("wave", "update", "2")
  locked <- Filter(function(field) !is.null(field$locked), form$fields)
  expect_gt(length(locked), 0)
  html <- paste(as.character(edit_field_ui(shiny::NS("page"), locked[[1]], form$initial[[locked[[1]]$id]], "note_1")), collapse = "")
  expect_match(text_of(html), locked[[1]]$locked, fixed = TRUE)
  expect_match(html, "locked-value", fixed = TRUE)
  expect_false(grepl("<input|<select", html))
})

test_that("a text box says what it holds inside it, and whether it can be left empty", {
  name <- edit_field("name", "Name", required = TRUE, placeholder = "e.g. Adult Health Study")
  expect_equal(edit_field_placeholder(name), "Name (e.g. Adult Health Study)")
  expect_equal(edit_field_placeholder(edit_field("description", "Description", "textarea")), "Description (optional)")
  expect_equal(edit_field_placeholder(edit_field("count", "Respondents", "number", placeholder = "e.g. 1,200")), "Respondents (optional, e.g. 1,200)")
  html <- paste(as.character(edit_field_ui(shiny::NS("page"), name, "", "note_1")), collapse = "")
  expect_match(html, 'placeholder="Name (e.g. Adult Health Study)"', fixed = TRUE)
})

test_that("the info block says what the kind is, what the verb involves and what sending does, once", {
  study <- edit_entities$study
  added <- text_of(edit_info_block(study, "add", approval = FALSE))
  expect_match(added, "Adding a study")
  expect_match(added, study$about, fixed = TRUE)
  expect_match(added, "as soon as you send it")
  deleted <- edit_info_block(study, "delete", approval = TRUE)
  expect_match(text_of(deleted), study$delete_about, fixed = TRUE)
  expect_match(text_of(deleted), "approves it on Change Requests")
  expect_match(paste(as.character(deleted), collapse = ""), "is-danger")
  expect_match(text_of(edit_info_block(study, "update", editing_request = TRUE)), "new version of this request")
  expect_false(grepl("Adding|Changing|Deleting", text_of(edit_info_block(study, ""))))
})

test_that("a drop-down offers its choices by name, with the one that is chosen selected", {
  form <- form_for("study", "update", "1")
  registry <- Filter(function(field) identical(field$id, "study__registry_id"), form$fields)[[1]]
  html <- paste(as.character(edit_field_ui(shiny::NS("page"), registry, form$initial$study__registry_id, "note_1")), collapse = "")
  expect_match(html, "Mid-Atlantic Twin Registry")
  expect_match(html, "Example Registry")
  expect_match(html, '<option value="1" selected>Mid-Atlantic Twin Registry (MATR)</option>', fixed = TRUE)
})

test_that("what changed lists each field, old and new, with words marked where they differ", {
  form <- form_for("registry", "update", "1", registry__name = "Mid-Atlantic Twin Registry (VCU)", registry__participant_count = "51,000")
  changes <- edit_service_changes(form$fields, form$initial, form$values)
  html <- paste(as.character(edit_changes_ui(changes)), collapse = "")
  expect_match(text_of(html), "What you changed")
  expect_match(html, "<ins>", fixed = TRUE) # the words put in
  expect_match(html, "(VCU)", fixed = TRUE)
  expect_match(html, "<del>", fixed = TRUE) # a number is shown as taken out and put in
  expect_match(html, "→", fixed = TRUE)
  expect_match(text_of(edit_changes_ui(changes[0, ])), "Nothing yet. Change a field above.")
})

test_that("where a shared question appears lists each questionnaire, its place and its variables", {
  snap <- test_snapshot(edit_service_tables(edit_entities$question))
  row <- edit_service_row(edit_entities$question, snap, "4")
  usage <- edit_service_usage(edit_entities$question, snap, row)
  where <- text_of(edit_usage_ui(edit_entities$question, "Do you now smoke cigarettes?", usage))
  expect_match(where, "Where this appears")
  expect_match(where, "Everything below changes if you make this edit.")
  expect_match(where, "“Do you now smoke cigarettes?”", fixed = TRUE)
  expect_match(where, "position [0-9]+")
  expect_match(where, "variable")
})

test_that("a component nothing prints, and a row nothing points at, say they affect nothing else", {
  snap <- test_snapshot(edit_service_tables(edit_entities$component))
  unused <- edit_service_row(edit_entities$component, snap, "15")
  expect_match(
    text_of(edit_usage_ui(edit_entities$component, "Unused", edit_service_usage(edit_entities$component, snap, unused))),
    "No questionnaire prints this yet, so changing it affects nothing else."
  )
  snap <- test_snapshot(edit_service_tables(edit_entities$registry))
  alone <- edit_service_row(edit_entities$registry, snap, "2")
  expect_match(
    text_of(edit_usage_ui(edit_entities$registry, "Example Registry", edit_service_usage(edit_entities$registry, snap, alone))),
    "Nothing else points at this yet|Everything below changes"
  )
})

test_that("a row that rows point at lists them, with examples", {
  snap <- test_snapshot(edit_service_tables(edit_entities$registry))
  registry <- edit_service_row(edit_entities$registry, snap, "1")
  usage <- edit_service_usage(edit_entities$registry, snap, registry)
  expect_gt(length(usage$pointing), 0)
  html <- text_of(edit_usage_ui(edit_entities$registry, "MATR", usage))
  expect_match(html, "Adult Health Study")
})

test_that("a delete says what goes with it, what is only changed, and how it falls on each questionnaire", {
  snap <- test_snapshot(edit_service_tables(edit_entities$component))
  entity <- edit_entities$component
  row <- edit_service_row(entity, snap, "3")
  plan <- edit_service_delete_plan(entity, snap, row)
  html <- text_of(edit_delete_ui(entity, row$text, plan))
  expect_match(html, "You're proposing to delete this component What is your date of birth?", fixed = TRUE)
  expect_match(html, "5 other records will be deleted with it")
  expect_match(html, "When this is applied, all of it is removed together.")
  expect_match(html, "Deleted along with it 1 component in a questionnaire AHP · What is your date of birth?", fixed = TRUE)
  expect_match(html, "2 questions in waves AHP Wave 1 · question 1", fixed = TRUE)
  expect_match(html, "How it falls on each questionnaire AHP 1 component in a questionnaire · 1 question in a questionnaire", fixed = TRUE)

  shared <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "9"))
  expect_match(text_of(edit_delete_ui(entity, "Felt sad", shared)), "18 other records will be deleted with it")
  expect_match(text_of(edit_delete_ui(entity, "Felt sad", shared)), "KQ · How often in the past week did you feel sad?", fixed = TRUE)
})

test_that("a delete that something stands in the way of lists what, and offers no confirmation", {
  entity <- edit_entities$investigator
  snap <- test_snapshot(edit_service_tables(entity))
  plan <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "1"))
  expect_gt(length(plan$blocked), 0)
  html <- text_of(edit_delete_ui(entity, "Dana Reyes", plan))
  expect_match(html, "can't be deleted|in the way|lead", ignore.case = TRUE)
})

test_that("the card for a row says what it is, its counts and any warning, and has a button to pick it", {
  snap <- test_snapshot(edit_service_tables(edit_entities$component))
  cards <- edit_service_cards(edit_entities$component, snap, NULL)
  shared <- cards[cards$key == "1", ]
  html <- paste(as.character(edit_card_ui(shared, "update")), collapse = "")
  expect_match(text_of(html), "A. About you", fixed = TRUE)
  expect_match(html, 'data-action="pick"', fixed = TRUE)
  expect_match(text_of(html), "Update this")
  expect_false(grepl(
    'data-action="pick"',
    paste(as.character(edit_card_ui(shared, "update", selected = TRUE)), collapse = ""),
    fixed = TRUE
  ))
  expect_match(text_of(edit_card_ui(shared, "delete")), "Delete this")
})
