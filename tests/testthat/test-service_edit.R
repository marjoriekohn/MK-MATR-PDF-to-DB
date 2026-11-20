# ------ The kinds and their cards ----------------------------------------------------------
test_that("the Metadata page's lists are named for staff, and every kind there is has a name", {
  expect_equal(
    unname(vapply(make_edit_lists, function(list) list$label, character(1))),
    c(
      "Registries", "Studies", "Publications", "Questionnaires", "Waves", "Sections, Grids & Groups", "Questions",
      "Instructions & Notes", "Investigators", "Respondent Types", "Tags", "Answer Choices", "Measurement Scales"
    )
  )
  expect_equal(make_edit_listed, names(make_edit_lists))
  for (list in make_edit_lists) {
    expect_true(nzchar(list$one), info = list$label) # one of them, for + Add
    expect_true(list$kind %in% names(edit_entities), info = list$label)
  }
  # every kind is in a list, but one questionnaire's printing of a wording and a question's place in a scale,
  # which are reached from the wording's and the scale's details, and a PDF, whose name and Open PDF are its wave's
  listed <- unique(vapply(make_edit_lists, function(list) list$kind, character(1)))
  expect_setequal(setdiff(names(edit_entities), listed), c("placement", "question_scale", "pdf_file"))
  expect_setequal(names(make_edit_kinds), names(edit_entities))
  expect_equal(make_edit_kinds[["tag"]], "Tag")
  expect_equal(make_edit_kinds[["investigator"]], "Investigator")
  # the three lists of wordings hold every kind of block between them, each once; + Add there adds one to a questionnaire
  wordings <- Filter(function(list) list$kind == "component", make_edit_lists)
  expect_setequal(unlist(lapply(wordings, function(list) list$blocks)), edit_component_kinds)
  expect_false(anyDuplicated(unlist(lapply(wordings, function(list) list$blocks))) > 0)
  expect_true(all(vapply(wordings, function(list) identical(list$adds, "placement"), logical(1))))
  # the database's own words for them aren't the lists'
  expect_false(any(grepl("Block|Component|Scale Item|Principal|PDF", vapply(make_edit_lists, function(list) list$label, character(1)))))
  # the kinds of the old schema are gone
  expect_false(any(c("component_type", "question", "option_set", "category", "subject", "time_frame", "questionnaire_question") %in% names(edit_entities)))
})

test_that("a list of wordings holds the ones of its kinds, by name, each with the questionnaires that print it", {
  snap <- test_snapshot(edit_service_tables(edit_entities$component))
  questions <- make_edit_cards("question", snap)
  expect_equal(questions$key, c("11", "4", "14", "5", "7", "9", "3")) # alphabetical
  expect_true(all(questions$tag == "Question"))
  expect_equal(questions$subtitle[questions$key == "4"], "AHP, MQ")
  sections <- make_edit_cards("section", snap)
  expect_setequal(sections$tag, c("Section", "Group"))
  expect_equal(sections$title[sections$tag == "Group"], "(group)")
  notes <- make_edit_cards("note", snap)
  expect_setequal(notes$key, c("2", "8", "13", "15"))
  expect_equal(notes$subtitle[notes$key == "15"], "not printed anywhere")
  # what one is called over its details: its kind of block, in a questionnaire or not
  expect_equal(make_edit_called("component", snap, snap$component[snap$component$component_id == 4, ]), "Question")
  placed <- test_snapshot(edit_service_tables(edit_entities$placement))
  expect_equal(make_edit_called("placement", placed, placed$placement[placed$placement$placement_id == 101, ]), "Section")
  expect_equal(make_edit_called("placement"), "Block") # one being added, before its kind is picked
  expect_equal(make_edit_called("study", snap, NULL), "Study")
})

test_that("every kind has what the page needs, and reads only tables the database has", {
  for (entity in edit_entities) {
    expect_true(nzchar(entity$label), info = entity$key)
    expect_true(entity$article %in% c("a", "an"), info = entity$key)
    expect_true(entity$table %in% names(database_tables), info = entity$key)
    expect_true(is.function(entity$cards) && is.function(entity$fields), info = entity$key)
    expect_true(all(edit_service_tables(entity) %in% names(database_tables)), info = entity$key)
  }
  expect_true(all(c("registry", "study", "questionnaire", "wave") %in% edit_emptiable))
  for (what in edit_emptiable) expect_true(nzchar(edit_entities[[what]]$empty_about), info = what)
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

test_that("kinds that need a questionnaire or a scale have no cards until they have one", {
  for (key in c("placement", "question_scale")) {
    entity <- edit_entities[[key]]
    expect_equal(nrow(edit_service_cards(entity, test_snapshot(edit_service_tables(entity)), "")), 0)
  }
})

test_that("a registry's card counts its studies", {
  entity <- edit_entities$registry
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  matr <- cards[cards$key == "1", ]
  expect_equal(matr$title, "Mid-Atlantic Twin Registry")
  expect_equal(matr$subtitle, "MATR · 50,000 participants")
  expect_equal(matr$facts[[1]], "2 studies")
})

test_that("a component's card says its kind, how many questionnaires print it, and warns when it's shared", {
  entity <- edit_entities$component
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  smoke <- cards[cards$key == "4", ]
  expect_equal(smoke$title, "do you now smoke cigarettes?")
  expect_equal(smoke$tag, "Question")
  expect_equal(smoke$facts[[1]], c("seen in 2 questionnaires: AHP, MQ", "2 variables"))
  expect_equal(smoke$subtitle, "AHP, MQ") # what its row in the list says under its words
  expect_equal(smoke$warn, "Shared: changes show in 2 questionnaires")
  section <- cards[cards$key == "1", ]
  expect_equal(section$tag, "Section")
  expect_true(is.na(section$warn))
  group <- cards[cards$key == "16", ]
  expect_equal(group$title, "(group)")
  unused <- cards[cards$key == "15", ]
  expect_equal(unused$facts[[1]], "not printed in any questionnaire")
})

test_that("cards are found by any words on them", {
  entity <- edit_entities$study
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  expect_equal(edit_service_filter(cards, "adult")$title, "Adult Health Study")
  expect_equal(edit_service_filter(cards, "MATR ended")$title, character())
  expect_equal(nrow(edit_service_filter(cards, "")), 3)
  expect_equal(edit_service_filter(cards, "  Example  ")$title, "Example Study")
})

test_that("a questionnaire's blocks are listed in the order they print, with where each sits and what a question depends on", {
  entity <- edit_entities$placement
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)), "1")
  expect_equal(cards$key, c("101", "102", "103", "104", "105", "106", "107"))
  expect_equal(cards$title[1:2], c("a. about you", "— please answer every question. skip any you prefer not to answer."))
  expect_equal(cards$tag[1:3], c("Section", "Instruction", "Question"))
  expect_equal(cards$subtitle[1], "top level · position 1")
  expect_equal(cards$subtitle[2], "in a. about you · position 1")
  expect_equal(cards$subtitle[3], "asked as question 1 in Wave 1, question 1 in Wave 2")
  expect_equal(cards$subtitle[5], "asked as question 2a in Wave 1, question 3 in Wave 2")
  expect_equal(cards$facts[[1]], "4 blocks inside")
  expect_equal(cards$facts[[5]], c("1 variable", "depends on “do you now smoke cigarettes?”"))
  expect_equal(cards$facts[[4]], c("1 variable", "also printed in 1 other questionnaire"))
  # a group is named by its kind, and a block inside it by the group
  mq <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)), "4")
  expect_equal(mq$title[4], "— (group)")
  expect_equal(mq$subtitle[6], "in (group) · position 2")
})

test_that("a tag's card says its kind and how many things carry it", {
  entity <- edit_entities$tag
  cards <- edit_service_cards(entity, test_snapshot(edit_service_tables(entity)))
  respondent <- cards[cards$key == "1", ]
  expect_equal(respondent$tag, "subject")
  expect_equal(respondent$facts[[1]], "on 7 things")
  week <- cards[cards$key == "17", ]
  expect_equal(week$tag, "time frame")
  expect_equal(week$facts[[1]], "on 3 things") # two variables and a question
  expect_equal(cards[cards$key == "2", ]$facts[[1]], "on nothing yet")
})

# ------ Forms --------------------------------------------------------------------------------------
test_that("an update form shows the row as it is, with locked fields marked", {
  form <- form_for("wave", "update", "2")
  expect_equal(form$initial$wave__name, "Wave 2")
  expect_equal(form$initial$wave__administered_date, "04/15/2022")
  expect_equal(form$initial$wave__respondent_count, "1210")
  expect_equal(form$initial$wave__questionnaire_id, "1")
  locked <- Filter(function(field) !is.null(field$locked), form$fields)
  expect_equal(vapply(locked, function(field) field$name, character(1)), "questionnaire_id")
  # the files it was extracted from aren't on the form: they come with a push
  expect_false(any(c("pdf_sha256", "pdf_name") %in% vapply(form$fields, function(field) field$name, character(1))))
})

test_that("a field is only locked once the row exists", {
  add <- form_for("tag", "add")
  expect_null(Filter(function(field) field$name == "kind", add$fields)[[1]]$locked)
  update <- form_for("tag", "update", "1")
  expect_match(Filter(function(field) field$name == "kind", update$fields)[[1]]$locked, "fixed")
})

test_that("drop-downs hold the rows they point at, by name, saved as IDs", {
  form <- form_for("study", "add")
  registry <- Filter(function(field) field$name == "registry_id", form$fields)[[1]]
  expect_equal(registry$choices, c("Choose a registry" = "", "Example Registry (EXR)" = "2", "Mid-Atlantic Twin Registry (MATR)" = "1"))
  study <- Filter(function(field) field$name == "study_id", form_for("questionnaire", "add")$fields)[[1]]
  expect_equal(unname(study$choices["Adult Health Study · MATR"]), "1")
})

test_that("a component and a block are any of the nine kinds; the kind is fixed once it exists", {
  expect_equal(edit_component_kinds, c("Section", "Group", "Grid", "Question", "Instruction", "Introduction", "Definition", "Note", "Interviewer Script"))
  kind <- Filter(function(field) field$name == "kind", form_for("component", "add")$fields)[[1]]
  expect_equal(unname(kind$choices), c("", edit_component_kinds))
  kind <- Filter(function(field) field$name == "kind", form_for("placement", "add", scope = "1")$fields)[[1]]
  expect_equal(unname(kind$choices), c("", edit_component_kinds))
  kind <- Filter(function(field) field$name == "kind", form_for("placement", "update", "101", scope = "1")$fields)[[1]]
  expect_match(kind$locked, "fixed")
})

test_that("required fields left empty are named as the form labels them", {
  form <- form_for("questionnaire", "add")
  found <- edit_service_check(form$fields, form$values)
  expect_equal(found$missing, c("study", "respondent type", "name", "abbreviation"))
  expect_equal(found$missing_ids, c("questionnaire__study_id", "questionnaire__respondent_type_id", "questionnaire__name", "questionnaire__abbreviation"))
  expect_equal(edit_service_check(form_for("tag", "add")$fields, form_for("tag", "add")$values)$missing, c("kind of tag", "name"))
})

test_that("a number, a date and an email are checked", {
  form <- form_for("wave", "update", "2", wave__respondent_count = "lots", wave__administered_date = "2022-04-15")
  problems <- edit_service_check(form$fields, form$values)$problems
  expect_equal(problems$wave__respondent_count, "Type a whole number, e.g. 1,200.")
  expect_equal(problems$wave__administered_date, "Type the date as MM/DD/YYYY, e.g. 04/15/2022.")
  ok <- form_for("wave", "update", "2", wave__respondent_count = "1,300", wave__administered_date = "04/15/2022")
  expect_length(edit_service_check(ok$fields, ok$values)$problems, 0)
  form <- form_for("investigator", "add", investigator__name = "New", investigator__email = "not an email")
  expect_equal(edit_service_check(form$fields, form$values)$problems$investigator__email, "Type a full email address, like name@vcu.edu.")
  expect_length(edit_service_check(form_for("investigator", "add", investigator__name = "New")$fields, form_for("investigator", "add", investigator__name = "New")$values)$problems, 0)
})

test_that("a wave's name must be new for its questionnaire", {
  taken <- form_for("wave", "add", wave__questionnaire_id = "1", wave__name = "wave 1")
  expect_equal(edit_service_check(taken$fields, taken$values)$problems$wave__name, "AHP already has a Wave 1. Pick another name.")
  elsewhere <- form_for("wave", "add", wave__questionnaire_id = "4", wave__name = "Wave 2")
  expect_length(edit_service_check(elsewhere$fields, elsewhere$values)$problems, 0)
  same <- form_for("wave", "update", "2", wave__name = "Wave 2")
  expect_length(edit_service_check(same$fields, same$values)$problems, 0)
})

test_that("the oldest age can't be younger than the youngest", {
  form <- form_for("respondent_type", "add", respondent_type__name = "Teen", respondent_type__minimum_age = "13", respondent_type__maximum_age = "12")
  expect_equal(edit_service_check(form$fields, form$values)$problems$respondent_type__maximum_age, "The oldest age can't be younger than the youngest age.")
})

test_that("only a group has no words, and it has none", {
  form <- form_for("component", "add", component__kind = "Section")
  expect_equal(edit_service_check(form$fields, form$values)$problems$component__text, "Add the printed words. Only a group has none.")
  form <- form_for("component", "add", component__kind = "Group", component__text = "Some words")
  expect_match(edit_service_check(form$fields, form$values)$problems$component__text, "A group has no words of its own")
  form <- form_for("component", "add", component__kind = "Group")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
  # the same at a block in one questionnaire
  form <- form_for("placement", "add", scope = "1", placement__kind = "Group")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
  form <- form_for("placement", "update", "416", scope = "4", placement__text = "words")
  expect_match(edit_service_check(form$fields, form$values)$problems$placement__text, "A group has no words")
})

test_that("a block placed with wording that exists already uses it, with a warning; the wording itself is kept once", {
  form <- form_for("placement", "add", scope = "2", placement__kind = "Question", placement__text = "Do you now smoke cigarettes?")
  found <- edit_service_check(form$fields, form$values)
  expect_null(found$problems$placement__text)
  expect_match(found$warnings$placement__text, "A question with exactly these words is in the database already")
  expect_match(found$warnings$placement__text, "seen in 2 questionnaires: AHP, MQ")

  # a wording added, or changed, to words another of its kind has would be a second copy, which the database refuses
  form <- form_for("component", "add", component__kind = "Question", component__text = "Do you now smoke cigarettes?")
  found <- edit_service_check(form$fields, form$values)
  expect_equal(
    found$problems$component__text,
    "A question with exactly these words is in the database already (seen in 2 questionnaires: AHP, MQ), so there is nothing to add."
  )
  form <- form_for("component", "update", "4", component__text = "How often do you see a doctor?")
  expect_equal(
    edit_service_check(form$fields, form$values)$problems$component__text,
    "A question with exactly these words is in the database already (seen in 1 questionnaire: AHP). Merge this wording into that one instead."
  )
  # another kind with the same words is another component
  form <- form_for("component", "add", component__kind = "Note", component__text = "Do you now smoke cigarettes?")
  found <- edit_service_check(form$fields, form$values)
  expect_length(found$problems, 0)
  expect_length(found$warnings, 0)
})

test_that("a name that is nothing once written the database's way is a problem, not an empty name the database refuses", {
  nothing_left <- "Once written the way the database keeps it, nothing is left of this. Type it in full."
  form <- form_for("tag", "update", "21", tag__name = "per day") # a unit's "per day" is left out
  expect_equal(edit_service_check(form$fields, form$values)$problems$tag__name, nothing_left)
  form <- form_for("answer_option", "update", "19", answer_option__standardized_label = ".")
  expect_equal(edit_service_check(form$fields, form$values)$problems$answer_option__standardized_label, nothing_left)
  form <- form_for("tag", "update", "21", tag__name = "cigarettes per day")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
})

test_that("a questionnaire with the same name or abbreviation in its study can't be added", {
  form <- form_for("questionnaire", "add", questionnaire__study_id = "2", questionnaire__name = "kid questionnaire", questionnaire__abbreviation = "kqp")
  problems <- edit_service_check(form$fields, form$values)$problems
  expect_equal(problems$questionnaire__name, "This study already has a questionnaire with the name “Kid Questionnaire”.")
  expect_equal(problems$questionnaire__abbreviation, "This study already has a questionnaire with the abbreviation “KQP”.")
  other <- form_for("questionnaire", "add", questionnaire__study_id = "1", questionnaire__name = "Kid Questionnaire", questionnaire__abbreviation = "KQ")
  expect_length(edit_service_check(other$fields, other$values)$problems, 0)
})

test_that("a registry's name, and a study's name in its registry, can't be added twice", {
  form <- form_for("registry", "add", registry__name = "mid-atlantic twin registry")
  expect_equal(edit_service_check(form$fields, form$values)$problems$registry__name, "There's already a registry called “Mid-Atlantic Twin Registry”.")
  form <- form_for("study", "add", study__registry_id = "1", study__name = "Adult Health Study")
  expect_equal(edit_service_check(form$fields, form$values)$problems$study__name, "This registry already has a study called “Adult Health Study”.")
  form <- form_for("study", "add", study__registry_id = "2", study__name = "Adult Health Study")
  expect_length(edit_service_check(form$fields, form$values)$problems, 0)
})

test_that("a tag's name is written its kind's way, and is in its kind's list once", {
  expect_equal(edit_tag_name("time frame", "During the last 12 months"), "past year")
  expect_equal(edit_tag_name("subject", "Respondent’s Twin"), "twin")
  expect_equal(edit_tag_name("unit", "Lbs"), normal_unit("Lbs"))
  taken <- form_for("tag", "add", tag__kind = "subject", tag__name = " respondent’s family ")
  expect_equal(edit_service_check(taken$fields, taken$values)$problems$tag__name, "“family” is already in the list of subjects.")
  # the same words are fine as another kind
  other_kind <- form_for("tag", "add", tag__kind = "unit", tag__name = "family")
  expect_length(edit_service_check(other_kind$fields, other_kind$values)$problems, 0)
  # a tag may keep its own name
  same <- form_for("tag", "update", "1", tag__name = "RESPONDENT")
  expect_length(edit_service_check(same$fields, same$values)$problems, 0)
})

test_that("renaming a tag to the name of another of its kind merges it into that one", {
  form <- form_for("tag", "update", "13", snap = tagged_snapshot(), tag__name = "Past Year")
  found <- edit_service_check(form$fields, form$values)
  expect_length(found$problems, 0) # not refused, as a new tag with that name is
  expect_equal(found$warnings$tag__name, paste(
    "“past year” is already in the list of time frames. Saving merges this tag into it:",
    "1 tag on a question and 2 tags on variables move to “past year”, and “past 12 months” is removed."
  ))
  built <- form_items(form)
  # what carries the tag carries the other instead, once (variable 2 has it already), and the tag goes
  expect_equal(item_lines(built$items), c(
    'Delete placement_tag {"placement_id":209,"tag_id":13} NA: - -> -',
    'Insert placement_tag {"placement_id":209,"tag_id":12} NA: - -> -',
    'Delete variable_tag {"tag_id":13,"variable_key":1} NA: - -> -',
    'Insert variable_tag {"tag_id":12,"variable_key":1} NA: - -> -',
    'Delete variable_tag {"tag_id":13,"variable_key":2} NA: - -> -',
    'Delete tag {"tag_id":13} kind: time frame -> -',
    'Delete tag {"tag_id":13} name: past 12 months -> -'
  ))
  expect_equal(built$summary, "Merged the tag “past 12 months” into “past year”, moving 1 tag on a question and 2 tags on variables")
  expect_equal(built$merge[c("from", "into_name", "key")], list(from = "past 12 months", into_name = "past year", key = "12"))

  # a tag nothing carries is only removed
  unused <- form_for("tag", "update", "16", tag__name = "past month")
  expect_match(edit_service_check(unused$fields, unused$values)$warnings$tag__name, "nothing carries “past 30 days”, so it is only removed", fixed = TRUE)
  expect_equal(item_lines(form_items(unused)$items), c('Delete tag {"tag_id":16} kind: time frame -> -', 'Delete tag {"tag_id":16} name: past 30 days -> -'))
  expect_equal(form_items(unused)$summary, "Merged the tag “past 30 days” into “past month”")

  # a name no other tag of its kind has is a rename, as before, and so is the name of a tag of another kind
  renamed <- form_for("tag", "update", "13", tag__name = "past two years")
  expect_null(edit_service_check(renamed$fields, renamed$values)$warnings$tag__name)
  expect_equal(item_lines(form_items(renamed)$items), 'Update tag {"tag_id":13} name: past 12 months -> past two years')
  expect_null(form_items(renamed)$merge)
})

test_that("a tag kept under a name the site now writes another way can be saved in that way, but isn't changed untouched", {
  # "past 30 days" is written "past month" now; with no "past month" in the list, retyping it is a rename
  only_old <- function(snap) {
    snap$tag <- snap$tag[snap$tag$name != "past month", , drop = FALSE]
    snap
  }
  snap <- test_snapshot(edit_service_tables(edit_entities$tag), only_old)
  retyped <- form_for("tag", "update", "16", snap = snap, tag__name = "past month")
  expect_length(edit_service_check(retyped$fields, retyped$values)$problems, 0)
  expect_equal(edit_service_changes(retyped$fields, retyped$initial, retyped$values)[c("old_value", "new_value")], data.frame(old_value = "past 30 days", new_value = "past month"))
  expect_equal(item_lines(form_items(retyped)$items), 'Update tag {"tag_id":16} name: past 30 days -> past month')

  # opened and left alone, it has no change, no problem and no talk of merging, though its name is another tag's
  untouched <- form_for("tag", "update", "13", snap = tagged_snapshot())
  expect_equal(nrow(edit_service_changes(untouched$fields, untouched$initial, untouched$values)), 0)
  found <- edit_service_check(untouched$fields, untouched$values)
  expect_length(c(found$problems, found$warnings), 0)
  expect_equal(NROW(form_items(untouched)$items), 0)
})

test_that("an answer choice is kept in its normal form, once", {
  form <- form_for("answer_option", "add", answer_option__standardized_label = "Yes?")
  expect_equal(edit_service_check(form$fields, form$values)$problems$answer_option__standardized_label, "“yes” is already in the list.")
  fresh <- form_for("answer_option", "add", answer_option__standardized_label = "Not sure.")
  expect_length(edit_service_check(fresh$fields, fresh$values)$problems, 0)
  expect_equal(item_lines(form_items(fresh)$items), "Insert answer_option {\"answer_option_id\":\"new:answer_option:1\"} standardized_label: - -> not sure")
})

test_that("a question's fields on a block's add form show only once Question is the kind", {
  form <- form_for("placement", "add", scope = "1")
  names <- vapply(form$fields, function(field) field$name, character(1))
  expect_true(all(c("kind", "text", "parent_placement_id", "display_order", "depends_on_placement_id", "asked_in", "qn_1", "qn_2", "variables") %in% names))
  for (name in c("depends_on_placement_id", "asked_in", "qn_1", "qn_2", "variables")) {
    field <- form$fields[[match(name, names)]]
    expect_equal(field$show_when, list(field = "kind", value = "Question"), info = name)
  }
  # a section: the question's fields aren't required or checked
  section <- form_for("placement", "add", scope = "1", placement__kind = "Section", placement__text = "C. Habits")
  found <- edit_service_check(section$fields, section$values)
  expect_length(found$missing, 0)
  expect_length(found$problems, 0)
  # a question: it needs a wave that asks it and its variables
  question <- form_for("placement", "add", scope = "1", placement__kind = "Question", placement__text = "How tall are you?")
  found <- edit_service_check(question$fields, question$values)
  expect_equal(found$missing, "waves that ask it")
  expect_equal(found$problems$placement__variables, "Variable 1: fill in its Variable ID.")
})

test_that("a block's update form has a question's fields only for a question, without hiding them", {
  question <- form_for("placement", "update", "105", scope = "1")
  names <- vapply(question$fields, function(field) field$name, character(1))
  expect_true(all(c("depends_on_placement_id", "qn_1", "qn_2") %in% names))
  expect_true(all(vapply(question$fields, function(field) is.null(field$show_when), logical(1))))
  expect_equal(question$initial$placement__depends_on_placement_id, "104")
  expect_equal(question$initial$placement__qn_1, "2a")
  expect_equal(question$initial$placement__text, "how many cigarettes do you smoke on a typical day?")
  section <- form_for("placement", "update", "101", scope = "1")
  expect_false(any(c("depends_on_placement_id", "qn_1", "variables") %in% vapply(section$fields, function(field) field$name, character(1))))
})

test_that("a block goes in a section or group, but a section never goes inside a group, and a grid holds no section", {
  snap <- test_snapshot(edit_service_tables(edit_entities$placement))
  form <- form_for("placement", "add", scope = "4", snap = snap, placement__kind = "Section", placement__text = "D", placement__parent_placement_id = "416")
  expect_equal(edit_service_check(form$fields, form$values)$problems$placement__parent_placement_id, "A section can't be inside a group. Pick a section, or the top level.")
  form <- form_for("placement", "add", scope = "4", snap = snap, placement__kind = "Question", placement__text = "Q", placement__parent_placement_id = "416")
  expect_null(edit_service_check(form$fields, form$values)$problems$placement__parent_placement_id)
  grid <- snap
  grid$component$kind[grid$component$component_id == 6] <- "Grid"
  form <- form_for("placement", "add", scope = "1", snap = grid, placement__kind = "Section", placement__text = "D", placement__parent_placement_id = "106")
  expect_match(edit_service_check(form$fields, form$values)$problems$placement__parent_placement_id, "^A grid holds only its questions")
})

test_that("a position past the end is warned about", {
  form <- form_for("placement", "add", scope = "1", placement__kind = "Note", placement__text = "x", placement__parent_placement_id = "106", placement__display_order = "9")
  expect_equal(edit_service_check(form$fields, form$values)$warnings$placement__display_order, "There are only 2 places there, so it will go last (position 2).")
})

# ------ What changed ---------------------------------------------------------------------------------
test_that("only fields that differ are changes, shown as staff read them", {
  form <- form_for("wave", "update", "2", wave__name = "Round 2", wave__administered_date = "04/15/2022")
  changes <- edit_service_changes(form$fields, form$initial, form$values)
  expect_equal(changes$name, "name")
  expect_equal(changes$old, "Wave 2")
  expect_equal(changes$new, "Round 2")
  expect_equal(changes$label, "Wave name")
})

test_that("clearing a field is a change to empty, and a drop-down's change shows the names, not the IDs", {
  form <- form_for("wave", "update", "2", wave__respondent_count = "")
  changes <- edit_service_changes(form$fields, form$initial, form$values)
  expect_equal(changes$new, "(empty)")
  expect_true(is.na(changes$new_value))
  form <- form_for("study", "update", "1", study__contact_principal_investigator_id = "2")
  changes <- edit_service_changes(form$fields, form$initial, form$values)
  expect_equal(changes$old, "Jo Park")
  expect_equal(changes$new, "Sam Patel")
  expect_equal(changes$new_value, "2")
})

test_that("words changed are marked as taken out and put in", {
  diff <- edit_service_word_diff("do you now smoke cigarettes?", "do you currently smoke cigarettes?")
  expect_equal(diff$kind, c("same", "del", "ins", "same"))
  expect_equal(diff$text[2:3], c("now", "currently"))
  expect_equal(nrow(edit_service_word_diff("", "")), 0)
  expect_equal(edit_service_word_diff("", "new")$kind, "ins")
})

test_that("values are saved tidied", {
  expect_true(is.na(edit_service_normalize(edit_field("x", "X"), "   ")))
  expect_equal(edit_service_normalize(edit_field("x", "X", "number"), "1,200"), "1200")
  expect_equal(edit_service_normalize(edit_field("x", "X", "number"), "many"), "many") # left for the check to refuse
  expect_equal(edit_service_normalize(edit_field("x", "X", "date"), "04/15/2022"), "2022-04-15")
  expect_equal(edit_service_normalize(edit_field("x", "X", "several", choices = c(a = "1", b = "2")), "2, 1"), "1,2")
  expect_equal(edit_service_normalize(edit_field("x", "X", normalize = normal_answer), "Yes?"), "yes")
  expect_equal(edit_service_normalize(edit_field("x", "X", normalize = normal_answer), "F"), "f") # a grade, as much as a sex
  expect_equal(edit_display_value(edit_field("x", "X", "date"), "2022-04-15"), "04/15/2022")
  expect_equal(edit_display_value(edit_field("x", "X", "several", choices = c(Yes = "1", No = "2")), "1,2"), "Yes / No")
  expect_equal(edit_display_value(edit_field("x", "X"), NA), "(empty)")
})

test_that("every kind builds its forms from the tables it reads, whatever it's asked to do", {
  for (entity in edit_entities) {
    snap <- test_snapshot(edit_service_tables(entity))
    scopes <- if (!is.null(entity$scope)) c("1", "4") else list(NULL)
    for (scope in scopes) {
      cards <- edit_service_cards(entity, snap, scope)
      for (mode in c("add", "update")) {
        if (!is.null(edit_service_can(entity, snap, mode))) next
        keys <- if (mode == "update") cards$key else NA
        for (key in keys) {
          row <- if (mode == "update") edit_service_row(entity, snap, key)
          fields <- edit_service_fields(entity, snap, mode, row, scope)
          initial <- edit_service_initial(entity, fields, snap, mode, row, scope)
          expect_no_error(edit_service_check(fields, initial), message = paste(entity$key, mode, key))
          if (mode == "update") {
            # a row as the database has it passes its own form's checks: opened and saved unchanged, nothing is refused
            expect_length(edit_service_check(fields, initial)$problems, 0)
            expect_equal(nrow(form_items(list(entity = entity, snap = snap, row = row, scope = scope, fields = fields, values = initial, mode = mode))$items), 0, info = paste(entity$key, key))
            expect_no_error(edit_service_details(entity, snap, row, fields, initial), message = paste(entity$key, key))
            expect_no_error(edit_service_usage(entity, snap, row), message = paste(entity$key, key))
          }
        }
      }
    }
  }
})
