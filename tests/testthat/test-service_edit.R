# ------ The sentence and the cards ----------------------------------------------------------
test_that("the kinds are offered in groups, named as they read in the sentence", {
  kinds <- edit_service_kinds()
  expect_equal(names(kinds)[1], "Registries and studies")
  expect_equal(kinds[["Registries and studies"]], c(registry = "registry", investigator = "investigator", study = "study"))
  expect_equal(unname(kinds[["In one questionnaire only"]]), c("questionnaire_component", "questionnaire_question"))
  expect_equal(names(kinds[["In one questionnaire only"]]), c("questionnaire component", "questionnaire question"))
})

test_that("every kind has what the page needs", {
  for (entity in edit_entities) {
    expect_true(nzchar(entity$label), info = entity$key)
    expect_true(entity$article %in% c("a", "an"), info = entity$key)
    expect_true(entity$table %in% names(database_tables), info = entity$key)
    expect_true(is.function(entity$cards) && is.function(entity$fields), info = entity$key)
  }
})

test_that("every kind lists its cards from the sample database", {
  for (entity in edit_entities) {
    snap <- test_snapshot(edit_service_tables(entity))
    scope <- if (!is.null(entity$scope)) "1"
    cards <- edit_service_cards(entity, snap, scope)
    expect_gt(nrow(cards), 0)
    expect_named(cards, c("key", "title", "subtitle", "tag", "warn", "facts", "search"), ignore.order = TRUE, info = entity$key)
    expect_false(anyDuplicated(cards$key) > 0, info = entity$key)
    expect_true(all(nzchar(cards$title)), info = entity$key)
  }
})

test_that("kinds that need a questionnaire have no cards until they have one", {
  entity <- edit_entities$questionnaire_component
  snap <- test_snapshot(edit_service_tables(entity))
  expect_equal(nrow(edit_service_cards(entity, snap, "")), 0)
})

test_that("a registry's card counts its studies", {
  entity <- edit_entities$registry
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  matr <- cards[cards$key == "1", ]
  expect_equal(matr$title, "Mid-Atlantic Twin Registry")
  expect_equal(matr$subtitle, "MATR · 50,000 participants")
  expect_equal(matr$facts[[1]], "2 studies")
  expect_equal(cards$facts[cards$key == "2"][[1]], "1 study")
})

test_that("a component's card says how many questionnaires print it, and warns when it's shared", {
  entity <- edit_entities$question
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  smoke <- cards[cards$title == "Do you now smoke cigarettes?", ]
  expect_equal(smoke$facts[[1]], c("seen in 2 questionnaires: AHP, MQ", "2 variables"))
  expect_equal(smoke$warn, "Shared: changes show in 2 questionnaires")
  expect_equal(smoke$tag, "Question")

  once <- cards[cards$title == "What is your date of birth?", ]
  expect_equal(once$facts[[1]][1], "seen in 1 questionnaire: AHP")
  expect_true(is.na(once$warn))
})

test_that("a component nothing prints says so", {
  entity <- edit_entities$component
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  thanks <- cards[cards$title == "Thank you for taking part.", ]
  expect_equal(thanks$facts[[1]], "not printed in any questionnaire")
  expect_equal(thanks$tag, "Note")
  # questions are their own kind
  expect_false("Do you now smoke cigarettes?" %in% cards$title)
})

test_that("cards are found by any words on them", {
  entity <- edit_entities$question
  snap <- test_snapshot(edit_service_tables(entity))
  expect_equal(nrow(edit_service_cards(entity, snap, query = "")), 7)
  expect_setequal(
    edit_service_cards(entity, snap, query = "SMOKE")$title,
    c("Do you now smoke cigarettes?", "How many cigarettes do you smoke on a typical day?", "Did you smoke during your pregnancy?")
  )
  expect_equal(nrow(edit_service_cards(entity, snap, query = "smoke cigarettes ahp")), 2)
  expect_equal(nrow(edit_service_cards(entity, snap, query = "zzz")), 0)
})

test_that("a questionnaire's blocks are listed in the order they print, with where each sits", {
  entity <- edit_entities$questionnaire_component
  snap <- test_snapshot(edit_service_tables(entity))
  cards <- edit_service_cards(entity, snap, "1")
  expect_equal(cards$title, c(
    "A. About you", "— Please answer every question. Skip any you prefer not to answer.", "— What is your date of birth?",
    "— Do you now smoke cigarettes?", "— How many cigarettes do you smoke on a typical day?",
    "B. Your health", "— How often do you see a doctor?"
  ))
  expect_equal(cards$subtitle[1:2], c("top level · position 1", "in A. About you · position 1"))
  expect_equal(cards$facts[[1]], "4 items inside")
  expect_equal(cards$facts[[4]], "also printed in 1 other questionnaire")
  expect_equal(cards$key[1:2], c("1|1", "1|2"))
})

test_that("a questionnaire's questions say what numbers they had and what they follow up on", {
  entity <- edit_entities$questionnaire_question
  snap <- test_snapshot(edit_service_tables(entity))
  cards <- edit_service_cards(entity, snap, "1")
  expect_equal(nrow(cards), 4)
  expect_equal(cards$subtitle[2], "asked as question 2 in Wave 1, question 2 in Wave 2")
  follow_up <- cards[grepl("How many cigarettes", cards$title), ]
  expect_true("follow-up to “Do you now smoke cigarettes?”" %in% follow_up$facts[[1]])
})

# ------ The form -----------------------------------------------------------------------------------
test_that("a study's add form includes its first questionnaire and wave, linked", {
  form <- form_for("study", "add")
  ids <- vapply(form$fields, function(field) field$id, character(1))
  expect_true(all(c("study__name", "questionnaire__name", "wave__name") %in% ids))
  expect_false("questionnaire__study_id" %in% ids)
  expect_false("wave__questionnaire_id" %in% ids)
  expect_equal(
    unique(vapply(form$fields, function(field) field$part_title %||% NA_character_, character(1)))[1:3],
    c("The study", "Its first questionnaire", "That questionnaire's first wave")
  )
})

test_that("an update form shows the row as it is, with locked fields marked", {
  form <- form_for("wave", "update", "2")
  expect_equal(form$initial[["wave__name"]], "Wave 2")
  expect_equal(form$initial[["wave__administered_date"]], "04/15/2022")
  expect_equal(form$initial[["wave__respondent_count"]], "1210")
  expect_equal(form$initial[["wave__questionnaire_id"]], "1")
  locked <- Filter(function(field) !is.null(field$locked), form$fields)
  expect_equal(vapply(locked, function(field) field$name, character(1)), "questionnaire_id")
  expect_match(locked[[1]]$locked, "can't be moved")
})

test_that("a field is only locked once the row exists", {
  expect_length(Filter(function(field) !is.null(field$locked), form_for("wave", "add")$fields), 0)
  expect_length(Filter(function(field) !is.null(field$locked), form_for("study", "add")$fields), 0)
  expect_length(Filter(function(field) !is.null(field$locked), form_for("study", "update", "1")$fields), 1)
})

test_that("drop-downs hold the rows they point at, by name, saved as IDs", {
  form <- form_for("study", "add")
  registry <- Filter(function(field) field$name == "registry_id", form$fields)[[1]]
  expect_equal(registry$choices, c("Choose a registry" = "", "Example Registry (EXR)" = "2", "Mid-Atlantic Twin Registry (MATR)" = "1"))
  respondent <- Filter(function(field) field$id == "questionnaire__respondent_type_id", form$fields)[[1]]
  expect_equal(unname(respondent$choices), c("", "2", "1", "3"))
  expect_equal(names(respondent$choices)[2], "Child (5-17)")
})

test_that("a new component can be any kind but a question", {
  form <- form_for("component", "add")
  kinds <- Filter(function(field) field$name == "component_type_id", form$fields)[[1]]$choices
  expect_false("Question" %in% names(kinds))
  expect_true("Section" %in% names(kinds))
})

# ------ Checks ----------------------------------------------------------------------------------------------
test_that("required fields left empty are named as the form labels them", {
  form <- form_for("registry", "add")
  check <- edit_service_check(form$fields, form$values)
  expect_equal(check$missing, c("name", "abbreviation", "participants"))

  form <- form_for("registry", "add", registry__name = "Test", registry__abbreviation = "T", registry__participant_count = "12")
  expect_length(edit_service_check(form$fields, form$values)$missing, 0)
})

test_that("a number, a date and an email are checked", {
  form <- form_for("registry", "add", registry__name = "A", registry__abbreviation = "A", registry__participant_count = "lots")
  expect_equal(edit_service_check(form$fields, form$values)$problems$registry__participant_count, "Type a whole number, e.g. 1,200.")

  form <- form_for("wave", "add", wave__questionnaire_id = "1", wave__name = "Wave 9", wave__administered_date = "2022-04-15")
  expect_match(edit_service_check(form$fields, form$values)$problems$wave__administered_date, "MM/DD/YYYY")

  form <- form_for("investigator", "add", investigator__name = "A", investigator__email = "not an email")
  expect_match(edit_service_check(form$fields, form$values)$problems$investigator__email, "full email address")
  form <- form_for("investigator", "add", investigator__name = "A", investigator__email = "a@vcu.edu")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
})

test_that("a wave's name must be new for its questionnaire", {
  form <- form_for("wave", "add", wave__questionnaire_id = "1", wave__name = " wave 2 ")
  expect_equal(edit_service_check(form$fields, form$values)$problems$wave__name, "AHP already has a Wave 2. Pick another name.")
  form <- form_for("wave", "add", wave__questionnaire_id = "2", wave__name = "Wave 2")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)

  # the wave being changed doesn't clash with itself
  form <- form_for("wave", "update", "2")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
})

test_that("the oldest age can't be younger than the youngest", {
  form <- form_for(
    "respondent_type",
    "add",
    respondent_type__name = "Teen",
    respondent_type__minimum_age = "13",
    respondent_type__maximum_age = "12"
  )
  expect_match(edit_service_check(form$fields, form$values)$problems$respondent_type__maximum_age, "can't be younger")
})

test_that("a MeSH ID has the right shape and isn't already listed", {
  form <- form_for("category", "add", category__mesh_id = "smoking", category__term = "Smoking")
  expect_match(edit_service_check(form$fields, form$values)$problems$category__mesh_id, "capital letter")
  form <- form_for("category", "add", category__mesh_id = "D012907", category__term = "Smoking")
  expect_match(edit_service_check(form$fields, form$values)$problems$category__mesh_id, "already in the list")
  form <- form_for("category", "add", category__mesh_id = "D000001", category__term = "X")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
})

test_that("only a section may have no words", {
  section <- edit_type_id(test_snapshot("component_type"), "Section")
  form <- form_for("component", "add", component__component_type_id = "4", component__text = "")
  expect_equal(edit_service_check(form$fields, form$values)$problems$component__text, "Only a section can be left without words.")
  form <- form_for("component", "add", component__component_type_id = as.character(section), component__text = "")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
  form <- form_for("question", "add", question__text = "")
  expect_equal(edit_service_check(form$fields, form$values)$missing, "question wording")
})

test_that("wording that exists already is a warning, not a problem", {
  form <- form_for("question", "add", question__text = "Do you now smoke cigarettes?")
  check <- edit_service_check(form$fields, form$values)
  expect_length(check$problems, 0)
  expect_match(check$warnings$question__text, "already has exactly these words \\(seen in 2 questionnaires: AHP, MQ\\)")
})

test_that("a questionnaire with the same name in its study is a warning", {
  form <- form_for(
    "questionnaire",
    "add",
    questionnaire__study_id = "1",
    questionnaire__name = "Adult Health and Personality",
    questionnaire__abbreviation = "AHP"
  )
  check <- edit_service_check(form$fields, form$values)
  expect_match(check$warnings$questionnaire__name, "already has a questionnaire with the name")
  expect_match(check$warnings$questionnaire__abbreviation, "the abbreviation “AHP”")
})

test_that("a field that isn't shown isn't required or checked", {
  form <- form_for("questionnaire_question", "update", "1|5", scope = "1")
  check <- edit_service_check(form$fields, form$values)
  expect_length(check$missing, 0)
  form <- form_for("questionnaire_question", "update", "1|5", scope = "1", questionnaire_question__parent_question_id = "")
  expect_equal(edit_service_check(form$fields, form$values)$missing, "which question does it follow up on?")
  form <- form_for(
    "questionnaire_question",
    "update",
    "1|5",
    scope = "1",
    questionnaire_question__self_contained = "TRUE",
    questionnaire_question__parent_question_id = ""
  )
  expect_length(edit_service_check(form$fields, form$values)$missing, 0)
})

test_that("a placed block that needs a source needs the one that's chosen", {
  form <- form_for("questionnaire_component", "add", scope = "1")
  expect_equal(form$values[["questionnaire_component__source"]], "existing")
  expect_equal(edit_service_check(form$fields, form$values)$missing, "the component")
  form <- form_for("questionnaire_component", "add", scope = "1", questionnaire_component__source = "new")
  expect_equal(edit_service_check(form$fields, form$values)$missing, c("kind of block"))
})

# ------ Showing what changed -----------------------------------------------------------------------------------
test_that("only fields that differ are changes, shown as staff read them", {
  form <- form_for("wave", "update", "2", wave__name = "Round 2", wave__administered_date = "05/01/2022", wave__respondent_count = "1,210")
  changes <- edit_service_changes(form$fields, form$initial, form$values)
  expect_equal(changes$name, c("name", "administered_date"))
  expect_equal(changes$old, c("Wave 2", "04/15/2022"))
  expect_equal(changes$new, c("Round 2", "05/01/2022"))
  expect_equal(changes$new_value, c("Round 2", "2022-05-01"))
})

test_that("clearing a field is a change to empty", {
  form <- form_for("wave", "update", "2", wave__respondent_count = "")
  changes <- edit_service_changes(form$fields, form$initial, form$values)
  expect_equal(changes$old, "1210")
  expect_equal(changes$new, "(empty)")
  expect_true(is.na(changes$new_value))
})

test_that("a drop-down's change shows the names, not the IDs", {
  form <- form_for("questionnaire", "update", "1", questionnaire__respondent_type_id = "2")
  changes <- edit_service_changes(form$fields, form$initial, form$values)
  expect_equal(changes$old, "Adult (18+)")
  expect_equal(changes$new, "Child (5-17)")
})

test_that("words changed are marked as taken out and put in", {
  diff <- edit_service_word_diff("Do you now smoke cigarettes?", "Do you currently smoke cigarettes?")
  expect_equal(diff$kind, c("same", "del", "ins", "same"))
  expect_equal(diff$text, c("Do you ", "now", "currently", " smoke cigarettes?"))
  expect_equal(edit_service_word_diff("same", "same")$kind, "same")
  expect_equal(edit_service_word_diff("", "new words")$kind, "ins")
  expect_equal(edit_service_word_diff("old words", "")$kind, "del")
  expect_equal(nrow(edit_service_word_diff("", "")), 0)
})

test_that("values are saved tidied", {
  number <- edit_field("n", "N", "number")
  expect_equal(edit_service_normalize(number, " 1,200 "), "1200")
  expect_true(is.na(edit_service_normalize(number, "  ")))
  expect_equal(edit_service_normalize(edit_field("d", "D", "date"), "04/15/2022"), "2022-04-15")
  expect_equal(edit_service_normalize(edit_field("t", "T"), "  hello  "), "hello")
  expect_equal(edit_service_normalize(edit_field("c", "C", "choice"), "FALSE"), "false")
})

test_that("a form that's several rows says which one a repeated label belongs to", {
  form <- form_for("study", "add")
  missing <- edit_service_check(form$fields, form$values)$missing
  expect_equal(
    missing,
    c("registry", "lead investigator", "contact investigator", "study name", "status", "respondent type", "questionnaire name", "abbreviation", "wave name")
  )
})

test_that("every kind builds its forms from the tables it reads, whatever it's asked to do", {
  not_here <- character() # what a kind can't be asked to do here
  built <- 0L
  planned <- 0L
  for (entity in edit_entities) {
    snap <- test_snapshot(edit_service_tables(entity))
    scope <- if (!is.null(entity$scope)) "1"
    for (mode in c("add", "update")) {
      if (!is.null(edit_service_can(entity, snap, mode))) {
        not_here <- c(not_here, paste(entity$key, mode))
        next
      }
      built <- built + 1L
      row <- if (mode == "update") edit_service_row(entity, snap, edit_service_cards(entity, snap, scope)$key[1])
      fields <- edit_service_fields(entity, snap, mode, row, scope)
      initial <- edit_service_initial(entity, fields, snap, mode, row, scope)
      check <- edit_service_check(fields, initial)
      expect_type(check$missing, "character")
      if (mode == "update") {
        # a row opened and left alone changes nothing, and has nothing wrong with it
        expect_equal(nrow(edit_service_changes(fields, initial, initial)), 0, info = entity$key)
        expect_equal(nrow(edit_service_build(entity, mode, snap, row, scope, fields, initial)$items), 0, info = entity$key)
        usage <- edit_service_usage(entity, snap, row)
        expect_named(usage, c("uses", "shared", "pointing"), info = entity$key)
      }
    }
    if (is.null(edit_service_can(entity, snap, "delete"))) {
      row <- edit_service_row(entity, snap, edit_service_cards(entity, snap, scope)$key[1])
      plan <- edit_service_delete_plan(entity, snap, row)
      expect_true(is.logical(plan$far_reaching), info = entity$key)
      planned <- planned + 1L
    }
  }
  # (so a kind that stopped being checked here would be noticed)
  expect_equal(not_here, "questionnaire_question add") # a new question comes only with a New extraction
  expect_equal(built, 2L * length(edit_entities) - 1L)
  expect_equal(planned, length(edit_entities))
})

test_that("a draft's note and values come back as they were saved", {
  form <- form_for("registry", "add", registry__name = "R", registry__abbreviation = "R", registry__participant_count = "5")
  built <- form_items(form)
  record <- edit_service_record(
    fixture_staff,
    list(action = "add", entity = "registry", scope = "", target = "", values = form$values),
    built,
    "because",
    "Draft"
  )
  expect_equal(record$form$values$registry__name, "R")
  expect_equal(record$note, "because")
})

# ------ Empty databases ----------------------------------------------------------------------------
# The live database starts nearly empty, so every kind has to cope with having
# nothing to list, nothing to pick and nothing to count.
empty_tables <- function(snap, keep = character()) {
  for (table in setdiff(names(snap), keep)) snap[[table]] <- snap[[table]][0, , drop = FALSE]
  snap
}

test_that("every kind copes with a database that has nothing in it", {
  without_form <- character() # kinds with no add form to draw here
  for (entity in edit_entities) {
    snap <- empty_tables(test_snapshot(edit_service_tables(entity)))
    scope <- if (!is.null(entity$scope)) ""
    expect_equal(nrow(edit_service_cards(entity, snap, scope)), 0, info = entity$key)
    if (is.null(entity$scope) && is.null(edit_service_can(entity, snap, "add"))) {
      fields <- edit_service_fields(entity, snap, "add")
      initial <- edit_service_initial(entity, fields, snap, "add")
      expect_type(edit_service_check(fields, initial)$missing, "character")
      expect_s3_class(edit_empty_choices_ui(fields) |> htmltools::tagList(), "shiny.tag.list")
    } else {
      without_form <- c(without_form, entity$key)
    }
  }
  # questions come only from New extraction, and the other two belong to a questionnaire
  expect_equal(without_form, c("question", "questionnaire_component", "questionnaire_question"))
})

test_that("a questionnaire with nothing in it yet can be added to", {
  entity <- edit_entities$questionnaire_component
  snap <- empty_tables(test_snapshot(edit_service_tables(entity)), keep = c("questionnaire", "study", "wave", "component_type"))
  expect_equal(nrow(edit_service_cards(entity, snap, "1")), 0)
  fields <- edit_service_fields(entity, snap, "add", NULL, "1")
  source <- Filter(function(field) field$name == "source", fields)[[1]]
  expect_equal(source$default, "new")
  existing <- Filter(function(field) field$name == "component_id", fields)[[1]]
  expect_equal(unname(existing$choices), "")
  # a new block, placed first
  values <- edit_service_initial(entity, fields, snap, "add", NULL, "1")
  values[["questionnaire_component__component_type_id"]] <- "4"
  values[["questionnaire_component__text"]] <- "Welcome."
  items <- edit_service_build(entity, "add", snap, NULL, "1", fields, values)$items
  expect_equal(item_lines(items), c(
    'Insert component {"component_id":"new:component:1"} component_type_id: - -> 4',
    'Insert component {"component_id":"new:component:1"} text: - -> Welcome.',
    'Insert questionnaire_component {"component_id":"new:component:1","questionnaire_id":1} display_order: - -> 1'
  ))
  question <- edit_entities$questionnaire_question
  expect_equal(
    nrow(edit_service_cards(question, empty_tables(test_snapshot(edit_service_tables(question)), keep = c("questionnaire", "study")), "1")),
    0
  )
})

test_that("a form with nothing to pick from says to add it first", {
  snap <- empty_tables(test_snapshot(edit_service_tables(edit_entities$study)))
  fields <- edit_service_fields(edit_entities$study, snap, "add")
  notes <- htmltools::renderTags(htmltools::tagList(edit_empty_choices_ui(fields)))$html
  expect_match(notes, "Nothing to pick for “Registry” yet")
  expect_match(notes, "Add registry first")
  expect_match(notes, "Nothing to pick for “Respondent Type” yet")
})

# ------ What's left to do ----------------------------------------------------------------------
test_that("a form says what's left to do, in the order to do it", {
  expect_length(edit_service_blockers("add", found = list(missing = character(), problems = list())), 0)
  expect_equal(
    edit_service_blockers("add", found = list(missing = c("registry", "name"), problems = list(name = "taken"))),
    c("Fill in the registry and name.", "Fix the fields outlined in red.")
  )
  expect_equal(
    edit_service_blockers("update", found = list(missing = character(), problems = list()), n_changes = 0),
    "Change something to continue."
  )
  expect_length(edit_service_blockers("update", found = list(missing = character(), problems = list()), n_changes = 2), 0)
})

test_that("a change to something several questionnaires share needs confirming", {
  found <- list(missing = character(), problems = list())
  expect_equal(
    edit_service_blockers("update", found, n_changes = 1, shared = 3),
    "Tick the box to confirm it changes everywhere it's used."
  )
  expect_length(edit_service_blockers("update", found, n_changes = 1, shared = 3, confirmed = list(shared = TRUE)), 0)
  expect_length(edit_service_blockers("update", found, n_changes = 1, shared = 1), 0)
  expect_length(edit_service_blockers("add", found, shared = 3), 0) # only an update changes what's shared
})

test_that("a delete needs reading, and, if it reaches far, the word typed", {
  plan <- list(blocked = list(), far_reaching = FALSE)
  expect_equal(
    edit_service_blockers("delete", plan = plan),
    "Tick the box to confirm you've read what will be deleted."
  )
  expect_length(edit_service_blockers("delete", plan = plan, confirmed = list(read = TRUE)), 0)

  far <- list(blocked = list(), far_reaching = TRUE)
  expect_equal(
    edit_service_blockers("delete", plan = far, confirmed = list(read = TRUE)),
    "Type DELETE to be sure."
  )
  expect_equal(
    edit_service_blockers("delete", plan = far),
    c("Tick the box to confirm you've read what will be deleted.", "Type DELETE to be sure.")
  )
  expect_length(edit_service_blockers("delete", plan = far, confirmed = list(read = TRUE, word = " delete ")), 0)
  expect_equal(
    edit_service_blockers("delete", plan = far, confirmed = list(read = TRUE, word = "remove")),
    "Type DELETE to be sure."
  )
})

test_that("a delete something stands in the way of can't be sent, whatever's ticked", {
  blocked <- list(blocked = list(list(table = "study")), far_reaching = TRUE)
  expect_equal(
    edit_service_blockers("delete", plan = blocked, confirmed = list(read = TRUE, word = "DELETE")),
    "This can't be deleted yet: see what's in the way above."
  )
})

test_that("the steps above the page follow the sentence, the pick, the form and the send", {
  step <- function(verb, has_sentence = TRUE, picked = FALSE, ready = FALSE, sent = FALSE) {
    edit_service_steps(verb, has_sentence, picked, ready, sent)$current
  }
  expect_equal(edit_service_steps("add", TRUE, FALSE, FALSE, FALSE)$labels, c("What to do", "Fill it in", "Send for approval"))
  expect_equal(edit_service_steps("update", TRUE, FALSE, FALSE, FALSE)$labels, c("What to do", "Find it", "Change it", "Send for approval"))
  expect_equal(
    edit_service_steps("delete", TRUE, FALSE, FALSE, FALSE)$labels,
    c("What to do", "Find it", "Check what goes", "Send for approval")
  )
  # a change that needs no approval is applied at once
  expect_equal(edit_service_steps("add", TRUE, FALSE, FALSE, FALSE, approval = FALSE)$labels, c("What to do", "Fill it in", "Apply"))
  expect_equal(
    edit_service_steps("delete", TRUE, FALSE, FALSE, FALSE, approval = FALSE)$labels,
    c("What to do", "Find it", "Check what goes", "Apply")
  )

  expect_equal(step("", has_sentence = FALSE), 1)
  expect_equal(step("add"), 2)
  expect_equal(step("add", ready = TRUE), 3)
  expect_equal(step("update"), 2)
  expect_equal(step("update", picked = TRUE), 3)
  expect_equal(step("update", picked = TRUE, ready = TRUE), 4)
  expect_equal(step("delete", picked = TRUE, ready = TRUE), 4)
  expect_equal(step("add", ready = TRUE, sent = TRUE), 4) # one past the last of three
  expect_equal(step("update", picked = TRUE, ready = TRUE, sent = TRUE), 5)
})

test_that("a form with things to fix is saved as an unfinished draft with no items", {
  unfinished <- edit_service_unfinished("update", edit_entities$wave)
  expect_equal(unfinished$summary, "Unfinished: update a wave")
  expect_null(unfinished$items)
})
