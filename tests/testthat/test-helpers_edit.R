# What the Metadata page draws with: a field, what changed, where a row appears, what a delete reaches, and what
# emptying asks. The page tests go through most of it; these look at the pieces on their own, as words on a screen.

test_that("every field of every kind can be drawn, with its name beside it and for screen readers", {
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
        if (field$type != "variables") {
          expect_match(html, paste0('aria-label="', htmltools::htmlEscape(field$label, attribute = TRUE), '"'), fixed = TRUE, info = info)
          expect_match(html, '<div class="edit-field-label">', fixed = TRUE, info = info) # the name stays while it is edited
        }
        expect_false(grepl("field-help form-text|form-text field-help|locked-reason", html), info = info) # no small print under a box
        drawn <- drawn + 1L
      }
    }
  }
  expect_gt(drawn, 60) # so a kind that stopped being drawn would be noticed
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

test_that("a field shown only for a question is wrapped so the page shows it once Question is the kind", {
  form <- form_for("placement", "add", scope = "1")
  asked <- Filter(function(field) field$name == "asked_in", form$fields)[[1]]
  html <- paste(as.character(edit_field_ui(shiny::NS("page"), asked, "", "note_1")), collapse = "")
  expect_match(html, "shiny-panel-conditional", fixed = TRUE)
  expect_match(html, "placement__kind", fixed = TRUE)
  expect_match(html, "Question", fixed = TRUE)
})

test_that("a field's name stays beside it, what it is for is in a tooltip, and the box holds only an example", {
  name <- utils::modifyList(edit_field("name", "Name", required = TRUE, placeholder = "e.g. Adult Health Study"), list(id = "study__name"))
  expect_equal(edit_field_placeholder(name), "e.g. Adult Health Study")
  expect_null(edit_field_placeholder(edit_field("description", "Description", "textarea")))
  html <- paste(as.character(edit_field_ui(shiny::NS("page"), name, "", "note_1")), collapse = "")
  expect_match(html, 'placeholder="e.g. Adult Health Study"', fixed = TRUE)
  expect_match(html, '<label for="page-study__name">Name</label>', fixed = TRUE)
  expect_false(grepl("optional|bslib-tooltip", html)) # required, and nothing more to say about it
  # one that can be left empty says so beside its name; a hint is the tooltip of the "i" there, not text under the box
  count <- edit_field("participant_count", "Total participants", "number", hint = "Across all its waves.")
  html <- paste(as.character(edit_field_ui(shiny::NS("page"), utils::modifyList(count, list(id = "q__n")), "", "note_1")), collapse = "")
  expect_match(text_of(html), "Total participants optional", fixed = TRUE)
  expect_match(html, "<bslib-tooltip", fixed = TRUE)
  expect_match(html, 'aria-label="About Total participants"', fixed = TRUE)
  expect_match(html, "Across all its waves.", fixed = TRUE)
})

test_that("the info block says what the verb involves, once, and nothing for a plain add", {
  study <- edit_entities$study
  expect_null(edit_info_block(study, "add"))
  expect_null(edit_info_block(study, ""))
  deleted <- edit_info_block(study, "delete")
  expect_match(text_of(deleted), study$delete_about, fixed = TRUE)
  expect_match(text_of(deleted), "before anything is deleted")
  expect_false(grepl("approv|Change Requests", text_of(deleted)))
  expect_match(paste(as.character(deleted), collapse = ""), "is-danger")
  # a wording is called by its kind of block once the row is known, never a component
  snap <- test_snapshot(edit_service_tables(edit_entities$component))
  question <- edit_service_row(edit_entities$component, snap, "4")
  expect_match(text_of(edit_info_block(edit_entities$component, "update", question)), "Changing a question")
  expect_match(text_of(edit_info_block(edit_entities$component, "update", edit_service_row(edit_entities$component, snap, "2"))), "Changing an instruction")
  expect_false(grepl("component", text_of(edit_info_block(edit_entities$component, "update"))))
  # a form says where to look for what it does
  changed <- text_of(edit_info_block(study, "update"))
  expect_match(changed, "Under the form you'll see what you changed and everywhere it is used. Every change is kept in the History.", fixed = TRUE)
  expect_false(grepl("Nothing is saved until", changed))
  merged <- text_of(edit_info_block(edit_entities$component, "merge", question))
  expect_match(merged, "Merging a question")
  expect_match(merged, "Nothing happens until you press Merge them")
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

test_that("where a shared component appears is a card for each questionnaire: the words as they read there, and its variables", {
  snap <- test_snapshot(edit_service_tables(edit_entities$component))
  row <- edit_service_row(edit_entities$component, snap, "4")
  usage <- edit_service_usage(edit_entities$component, snap, row)
  expect_equal(usage$shared, 2)
  html <- paste(as.character(edit_usage_ui(edit_entities$component, usage)), collapse = "")
  where <- text_of(html)
  # each questionnaire's card: the question, its variables and their choices, and nothing about where it sits
  expect_match(where, "Adult Health and Personality (AHP) do you now smoke cigarettes? View in Questionnaire AH02SM [smokes now] [respondent] 1 = yes", fixed = TRUE)
  expect_match(where, "Mother Questionnaire (MQ) do you now smoke cigarettes? View in Questionnaire MQ02SM", fixed = TRUE)
  expect_false(grepl("Printed in|position|top level|Wave 1|Open in this questionnaire", where))
  expect_equal(lengths(regmatches(html, gregexpr('<div class="edit-use">', html, fixed = TRUE))), 2)
  # the questionnaire is a tag that opens it; View in Questionnaire opens the block as that questionnaire prints it
  expect_match(html, 'class="edit-context-pill is-link" data-action="jump" data-kind="questionnaire" data-verb="view" data-key="1"', fixed = TRUE)
  expect_match(html, 'data-kind="placement" data-verb="view" data-key="104" data-scope="1"', fixed = TRUE)
  # a question asked in no wave says so; a section has no waves to list
  unasked <- snap
  unasked$wave_question <- unasked$wave_question[0, ]
  expect_match(text_of(edit_usage_ui(edit_entities$component, edit_service_usage(edit_entities$component, unasked, row))), "Not asked in any wave yet")
  section <- edit_service_row(edit_entities$component, snap, "1")
  heading <- text_of(edit_usage_ui(edit_entities$component, edit_service_usage(edit_entities$component, snap, section)))
  expect_false(grepl("Not asked", heading))
  # a section, grid or group lists every block it holds there, each with View by its kind
  expect_match(heading, paste(
    "a. about you View in Questionnaire please answer every question. skip any you prefer not to answer. View Instruction",
    "what is your date of birth? View Question do you now smoke cigarettes? View Question"
  ), fixed = TRUE)
  group <- edit_service_row(edit_entities$component, snap, "16")
  held <- paste(as.character(edit_usage_ui(edit_entities$component, edit_service_usage(edit_entities$component, snap, group))), collapse = "")
  expect_match(text_of(held), "(group) View in Questionnaire how many children do you have? View Question", fixed = TRUE)
  expect_match(held, 'aria-label="View Question: how many children do you have?" data-kind="component" data-verb="view" data-key="14"', fixed = TRUE)
  expect_false(grepl("Holds", where)) # a question holds nothing
  # the one group has no words, so its Edit says why it is off; anything else can be changed
  expect_match(edit_entities$component$unchangeable(snap, group), "A group has no words of its own")
  expect_null(edit_entities$component$unchangeable(snap, row))
})

test_that("a questionnaire that prints a question twice has both places in one card, each after its grid's lead-in", {
  entity <- edit_entities$component
  snap <- test_snapshot(edit_service_tables(entity), function(snap) {
    snap$component <- rbind(snap$component, data.frame(component_id = c(90, 91), kind = "Grid", text = c("because of my physical health, i", "as a result of an emotional problem, i")))
    snap$placement <- rbind(snap$placement, data.frame(
      placement_id = c(190, 191, 192, 193), questionnaire_id = 1, component_id = c(90, 7, 91, 7),
      parent_placement_id = c(NA, 190, NA, 192), display_order = c(8, 1, 9, 1), depends_on_placement_id = NA
    ))
    snap
  })
  uses <- edit_service_usage(entity, snap, edit_service_row(entity, snap, "7"))$uses
  twice <- Filter(function(use) use$questionnaire_id == 1, uses)
  words <- vapply(twice, function(use) use$words, character(1))
  expect_true(all(c(
    paste("because of my physical health, i", snap$component$text[snap$component$component_id == 7]),
    paste("as a result of an emotional problem, i", snap$component$text[snap$component$component_id == 7])
  ) %in% words))
  html <- paste(as.character(edit_usage_ui(entity, list(uses = twice, pointing = list()))), collapse = "")
  expect_equal(lengths(regmatches(html, gregexpr('<div class="edit-use">', html, fixed = TRUE))), 1) # one card
  expect_equal(lengths(regmatches(html, gregexpr('<div class="edit-use-place">', html, fixed = TRUE))), length(twice))
})

# The sample tables with two questions inside grids, each after its grid's lead-in: AHP's "how often do you see a
# doctor?" (placement 107, in a grid in section B) and KQ's "how often in the past week did you feel sad?"
# (placement 209; KQP prints the same question, placement 309, outside any grid).
in_grids <- function(entity, change = identity) {
  test_snapshot(edit_service_tables(entity), function(snap) {
    snap$component <- rbind(snap$component, data.frame(component_id = c(90, 91), kind = "Grid", text = c("thinking of the past year,", "these are about your mood.")))
    snap$placement$parent_placement_id[snap$placement$placement_id == 107] <- 190
    snap$placement$parent_placement_id[snap$placement$placement_id == 209] <- 290
    snap$placement <- rbind(snap$placement, data.frame(
      placement_id = c(190, 290), questionnaire_id = c(1, 2), component_id = c(90, 91),
      parent_placement_id = c(106, 210), display_order = c(1, 2), depends_on_placement_id = NA
    ))
    change(snap)
  })
}

test_that("a question is listed in full wherever it is listed: after its grid's lead-in; a wording on its own is its root", {
  snap <- in_grids(edit_entities$component)
  doctor <- "thinking of the past year, how often do you see a doctor?"
  sad <- "these are about your mood. how often in the past week did you feel sad?"
  expect_equal(edit_placement_words(snap, c(107, 104, 190, 209, 309)), c(
    doctor, "do you now smoke cigarettes?", "thinking of the past year,", sad, "how often in the past week did you feel sad?"
  ))
  # only a question follows the lead-in: an instruction inside a grid is its own words
  noted <- snap
  noted$placement$parent_placement_id[noted$placement$placement_id == 102] <- 190
  expect_equal(edit_placement_words(noted, 102), "please answer every question. skip any you prefer not to answer.")
  # a wording printed after one lead-in here and none there is listed with each of the ways it reads, once
  expect_equal(edit_component_words(snap, c(9, 7, 15)), c(
    paste(sad, "/ how often in the past week did you feel sad?"), doctor, "thank you for taking part."
  ))
  # the question's own row in the list, and the title of its details, are the root alone
  cards <- edit_service_cards(edit_entities$component, snap)
  expect_equal(cards$title[cards$key == "7"], "how often do you see a doctor?")
  expect_equal(edit_label(snap, "component", edit_service_row(edit_entities$component, snap, "7")), "how often do you see a doctor?")
  # its card in the questionnaire, and the grid's list of what it holds, say it in full
  expect_equal(edit_service_usage(edit_entities$component, snap, edit_service_row(edit_entities$component, snap, "7"))$uses[[1]]$words, doctor)
  grid <- edit_service_usage(edit_entities$component, snap, edit_service_row(edit_entities$component, snap, "90"))$uses[[1]]
  expect_equal(grid$words, "thinking of the past year,")
  expect_equal(grid$inside$words, doctor)

  # a wave's questions, each of a variable's labels and tags said once however many variables have them
  wave <- edit_entities$wave
  snap <- in_grids(wave, function(snap) {
    snap$variable <- rbind(snap$variable, data.frame(
      variable_key = 99, questionnaire_id = 1, placement_id = 107, matr_id = "AH03DR2", display_order = 2,
      label = "doctor visits", variable_type = "closed_ended", answer_data_type = NA
    ))
    snap
  })
  asked <- Filter(function(line) line$table == "wave_question", edit_service_usage(wave, snap, edit_service_row(wave, snap, "1"))$pointing)[[1]]
  row <- asked$labels[grepl("see a doctor", asked$labels)]
  expect_match(row, paste("·", doctor), fixed = TRUE)
  expect_equal(lengths(regmatches(row, gregexpr("[doctor visits]", row, fixed = TRUE))), 1)

  # what carries a tag, what offers an answer choice, and a scale's questions
  tag <- edit_entities$tag
  snap <- in_grids(tag, function(snap) {
    snap$placement_tag <- rbind(snap$placement_tag, data.frame(placement_id = 209, tag_id = 13))
    snap
  })
  expect_equal(edit_service_usage(tag, snap, edit_service_row(tag, snap, "13"))$pointing[[1]]$labels, sad)
  choice <- edit_entities$answer_option
  snap <- in_grids(choice)
  offered <- edit_service_usage(choice, snap, edit_service_row(choice, snap, "13"))$pointing[[1]]
  expect_true(doctor %in% offered$labels)
  expect_false("how often do you see a doctor?" %in% offered$labels)
  scale <- edit_entities$measurement_scale
  snap <- in_grids(scale)
  expect_equal(edit_service_usage(scale, snap, edit_service_row(scale, snap, "1"))$pointing[[1]]$labels, paste(sad, "/ how often in the past week did you feel sad?"))
  # a block in one questionnaire lists what it holds in full too
  placed <- edit_entities$placement
  snap <- in_grids(placed)
  holds <- Filter(function(line) line$table == "placement", edit_service_usage(placed, snap, edit_service_row(placed, snap, "190"))$pointing)[[1]]
  expect_equal(holds$labels, doctor)
})

test_that("a block in one questionnaire lists what it holds in printed order, or what depends on it", {
  entity <- edit_entities$placement
  snap <- test_snapshot(edit_service_tables(entity))
  lines_of <- function(key) Filter(function(line) line$table == "placement", edit_service_usage(entity, snap, edit_service_row(entity, snap, key))$pointing)
  section <- lines_of("101")[[1]]
  expect_equal(section$text, "Holds 4 blocks")
  expect_equal(section$labels[1:2], c("please answer every question. skip any you prefer not to answer.", "what is your date of birth?"))
  expect_equal(section$pills[1:2], list("Instruction", "Question"))
  expect_equal(section$open[1, ], data.frame(kind = "placement", key = "102", scope = "1", what = "in Questionnaire")) # as this questionnaire prints it
  question <- lines_of("104")[[1]]
  expect_equal(question$text, "1 question depends on it")
  expect_equal(question$labels, "how many cigarettes do you smoke on a typical day?")
})

test_that("a questionnaire lists its waves, not everything it prints, and a wave its questions by number with their tags", {
  entity <- edit_entities$questionnaire
  snap <- test_snapshot(edit_service_tables(entity))
  usage <- edit_service_usage(entity, snap, edit_service_row(entity, snap, "1"))
  expect_equal(vapply(usage$pointing, function(line) line$table, character(1)), "wave") # hundreds of blocks aren't listed here
  waves <- usage$pointing[[1]]
  expect_equal(waves$text, "2 waves")
  expect_equal(waves$labels, c("AHP Wave 1", "AHP Wave 2"))
  expect_equal(waves$open, data.frame(kind = "wave", key = c("1", "2"), scope = "", what = "Wave")) # View opens each
  expect_null(waves$groups) # all in this questionnaire: not said again
  entity <- edit_entities$wave
  snap <- test_snapshot(edit_service_tables(entity))
  usage <- edit_service_usage(entity, snap, edit_service_row(entity, snap, "1"))
  asked <- Filter(function(line) line$table == "wave_question", usage$pointing)[[1]]
  expect_equal(asked$text, "4 Questions")
  expect_equal(asked$labels[2], "2 · do you now smoke cigarettes? [smokes now] [respondent]")
  expect_equal(asked$labels[3], "2a · how many cigarettes do you smoke on a typical day? [cigarettes per day] [cigarettes] [respondent]")
  expect_equal(asked$open$kind, rep("component", 4)) # View opens the question
  expect_equal(asked$open$key[2], "4")
})

test_that("each row that uses something has View, which opens it where it is changed", {
  usage_of <- function(key, row_key) {
    entity <- edit_entities[[key]]
    snap <- test_snapshot(edit_service_tables(entity))
    edit_service_usage(entity, snap, edit_service_row(entity, snap, row_key))$pointing
  }
  studies <- usage_of("registry", "1")[[1]]
  expect_equal(studies$open$kind, c("study", "study"))
  # an investigator's studies say what they are to the study, and a study that names them twice is listed for each
  dana <- usage_of("investigator", "1")
  expect_equal(vapply(dana, function(line) line$text, character(1)), "Leads 1 study")
  sam <- usage_of("investigator", "2")
  expect_equal(vapply(sam, function(line) line$text, character(1)), c("Leads 1 study", "Contact for 1 study"))
  expect_equal(sam[[1]]$open, data.frame(kind = "study", key = "2", scope = "", what = "Study"))
  # studies (and questionnaires, and waves) are a grid of small cards: the card itself is pressed to open it, and
  # under an item's details nothing is over them: no count, and no View beside the name
  html <- paste(as.character(edit_pointing_ui(studies)), collapse = "")
  expect_match(html, paste0(
    '<button type="button" class="edit-use-card" data-action="jump" aria-label="View Study: Adult Health Study" ',
    'title="Open this study" data-kind="study" data-verb="view" data-key="1" data-scope="">'
  ), fixed = TRUE)
  expect_equal(text_of(html), "Adult Health Study Child Development Study")
  expect_equal(lengths(regmatches(html, gregexpr('class="edit-use-card"', html, fixed = TRUE))), 2)
  expect_false(grepl("<details|edit-use-view|2 studies", html))
  # what says more than how many is still said over its cards: the studies an investigator leads, and is the contact for
  expect_false(studies$named)
  expect_true(all(vapply(sam, function(line) line$named, logical(1))))
  led <- paste(as.character(edit_pointing_ui(sam[[1]])), collapse = "")
  expect_match(led, '<p class="edit-use-caption">Leads 1 study</p>', fixed = TRUE)
  expect_match(text_of(paste(as.character(edit_pointing_ui(sam[[2]])), collapse = "")), "^Contact for 1 study ")
  # under a form the cards are in a drop-down, closed until it is clicked, as every line there is
  folded <- paste(as.character(edit_pointing_ui(studies, open = FALSE)), collapse = "")
  expect_match(folded, '<details class="edit-use">', fixed = TRUE)
  expect_match(text_of(folded), "^2 studies Adult Health Study Child Development Study$")
  # a list of rows keeps its count and View: a wave's questions
  wave <- usage_of("wave", "1")
  asked <- paste(as.character(edit_pointing_ui(Filter(function(line) line$table == "wave_question", wave)[[1]])), collapse = "")
  expect_match(asked, '<details class="edit-use" open>', fixed = TRUE)
  expect_match(asked, "edit-use-view", fixed = TRUE)
  # what a block in one questionnaire holds is its own, and opens nothing
  placement <- usage_of("placement", "104")
  expect_null(Filter(function(line) line$table == "variable", placement)[[1]]$open)
  expect_false(grepl("edit-use-view", paste(as.character(edit_pointing_ui(Filter(function(line) line$table == "variable", placement)[[1]])), collapse = "")))
})

test_that("an answer choice lists the variables that offer it by their question, with the MATR ID, its code and the questionnaire", {
  entity <- edit_entities$answer_option
  snap <- test_snapshot(edit_service_tables(entity))
  offered <- edit_service_usage(entity, snap, edit_service_row(entity, snap, "1"))$pointing[[1]]
  expect_equal(offered$text, "Offered by 3 variables")
  expect_equal(offered$labels, c("did you smoke during your pregnancy?", "do you now smoke cigarettes?", "do you now smoke cigarettes?"))
  expect_equal(offered$meta, c("MQ01SP · code 1", "AH02SM · code 1", "MQ02SM · code 1"))
  expect_equal(offered$pills, list("MQ", "AHP", "MQ"))
  expect_equal(offered$open, data.frame(kind = "component", key = c("11", "4", "4"), scope = "", what = "Question"))
  shown <- text_of(edit_pointing_ui(offered))
  expect_match(shown, "do you now smoke cigarettes? AH02SM · code 1 AHP View Question", fixed = TRUE)
  expect_false(grepl("response option", shown)) # the database's word for it
  # "no" is code 0 there
  no <- edit_service_usage(entity, snap, edit_service_row(entity, snap, "2"))$pointing[[1]]
  expect_true(all(grepl("code 0$", no$meta)))
})

test_that("a measurement scale lists its questions with the questionnaires that ask them, View, Remove, and Add a question", {
  entity <- edit_entities$measurement_scale
  snap <- test_snapshot(edit_service_tables(entity))
  cards <- edit_service_cards(entity, snap)
  expect_equal(cards$subtitle, "SMS · 1 question") # its row in the list counts them
  questions <- edit_service_usage(entity, snap, edit_service_row(entity, snap, "1"))$pointing[[1]]
  expect_equal(questions$text, "1 Question")
  expect_equal(questions$labels, "how often in the past week did you feel sad?") # the question alone: the scale's name is the title
  expect_equal(questions$pills, list(c("KQ", "KQP")))
  expect_equal(questions$open, data.frame(kind = "component", key = "9", scope = "", what = "Question"))
  expect_equal(questions$remove[c("kind", "key", "scope")], data.frame(kind = "question_scale", key = "1|9", scope = "1"))
  html <- paste(as.character(edit_pointing_ui(questions)), collapse = "")
  expect_match(text_of(html), "1 Question how often in the past week did you feel sad? KQ KQP View Question Remove Add a question", fixed = TRUE)
  expect_match(html, 'data-kind="question_scale" data-verb="delete" data-key="1|9" data-scope="1"', fixed = TRUE)
  expect_match(html, 'data-kind="question_scale" data-verb="add" data-scope="1"', fixed = TRUE)
  # a scale with no questions yet still has the list, since that is where one is added
  empty <- test_snapshot(edit_service_tables(entity), function(snap) {
    snap$question_scale <- snap$question_scale[0, ]
    snap
  })
  none <- edit_service_usage(entity, empty, edit_service_row(entity, empty, "1"))$pointing
  expect_length(none, 1)
  expect_equal(none[[1]]$text, "0 Questions")
  expect_match(text_of(edit_pointing_ui(none[[1]])), "Add a question")
})

test_that("a tag lists what carries it by its question", {
  entity <- edit_entities$tag
  snap <- tagged_snapshot()
  lines <- edit_service_usage(entity, snap, edit_service_row(entity, snap, "13"))$pointing
  expect_equal(vapply(lines, function(line) line$text, character(1)), c("On 1 question", "On 2 variables"))
  expect_equal(lines[[1]]$labels, "how often in the past week did you feel sad?")
  expect_equal(lines[[1]]$pills, list("KQ"))
  expect_equal(lines[[2]]$meta, c("AH02SM", "AH01DB"))
  expect_equal(lines[[2]]$open$kind, c("component", "component"))
  # one on several questions lists each (a question has no MATR ID to say beside it)
  snap$placement_tag <- rbind(snap$placement_tag, data.frame(placement_id = 104, tag_id = 13))
  on_questions <- edit_service_usage(entity, snap, edit_service_row(entity, snap, "13"))$pointing[[1]]
  expect_equal(on_questions$text, "On 2 questions")
  expect_equal(on_questions$labels, c("do you now smoke cigarettes?", "how often in the past week did you feel sad?"))
})

test_that("what a row belongs to is drawn as tags over its details: an open folder and the name, each opening what it names", {
  lines <- edit_details_lines(
    NA, c("Registry", "Study", "Investigator"), c("Mid-Atlantic Twin Registry (MATR)", "Adult Health Study", NA),
    kind = c("registry", "study", "investigator"), key = c(1, 1, NA)
  )
  html <- paste(as.character(edit_context_ui(lines)), collapse = "")
  expect_equal(text_of(html), "Mid-Atlantic Twin Registry (MATR) Adult Health Study") # the names alone; one that is empty isn't drawn
  expect_match(html, paste0(
    'class="edit-context-pill is-link" data-action="jump" data-kind="study" data-verb="view" data-key="1" ',
    'title="Study: open it, which is where it is changed" aria-label="Study: Adult Health Study. Open it."'
  ), fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("fa-folder-open", html, fixed = TRUE))), 2)
  # one that names nothing to open is only words; what it is is said when it is pointed at, and to screen readers
  plain <- paste(as.character(edit_context_ui(edit_details_lines("kind", "Kind of tag", "subject", locked = TRUE))), collapse = "")
  expect_match(plain, '<span class="edit-context-pill" title="Kind of tag">', fixed = TRUE)
  expect_false(grepl("data-action|fa-folder-open", plain))
  expect_null(edit_context_ui(lines[0, ]))
})

test_that("Merge offers the others of its kind to search, then says which stays, what moves and what to tick", {
  entity <- edit_entities$component
  snap <- test_snapshot(edit_service_tables(entity))
  row <- edit_service_row(entity, snap, "11")
  html <- paste(as.character(edit_merge_ui(shiny::NS("x"), entity, row, "did you smoke during your pregnancy?", edit_merge_choices(entity, snap, row))), collapse = "")
  expect_match(text_of(html), "This question is removed did you smoke during your pregnancy?", fixed = TRUE)
  expect_match(html, 'id="x-merge_into"', fixed = TRUE)
  expect_match(html, "do you now smoke cigarettes? · AHP, MQ", fixed = TRUE)
  expect_match(html, 'id="x-merge_preview"', fixed = TRUE)
  # nothing picked yet: nothing more is said
  expect_null(edit_merge_preview_ui(shiny::NS("x"), entity, row, NULL))
  preview <- text_of(edit_merge_preview_ui(shiny::NS("x"), entity, row, edit_service_merge_plan(entity, snap, row, "4")))
  expect_match(preview, "This question stays do you now smoke cigarettes?", fixed = TRUE)
  expect_match(preview, "What merging does 1 block in a questionnaire moves to “do you now smoke cigarettes?”", fixed = TRUE)
  expect_match(preview, "I understand the two become one. This can't be undone", fixed = TRUE)
  # the only one of its kind has nothing to merge into
  group <- edit_service_row(entity, snap, "16")
  expect_match(text_of(edit_merge_ui(shiny::NS("x"), entity, group, "(group)", edit_merge_choices(entity, snap, group))), "Nothing to merge it into")
  # two that can't be merged say what is in the way, and offer nothing to tick
  answers <- edit_entities$answer_option
  snap <- test_snapshot(edit_service_tables(answers))
  sometimes <- edit_service_row(answers, snap, "14")
  blocked <- text_of(edit_merge_preview_ui(shiny::NS("x"), answers, sometimes, edit_service_merge_plan(answers, snap, sometimes, "13")))
  expect_match(blocked, "These two can't be merged yet")
  expect_match(blocked, "3 variables offer both of these answer choices")
  expect_false(grepl("I understand", blocked))
})

test_that("a component nothing prints, and a row nothing points at, say they affect nothing else", {
  snap <- test_snapshot(edit_service_tables(edit_entities$component))
  unused <- edit_service_row(edit_entities$component, snap, "15")
  expect_match(
    text_of(edit_usage_ui(edit_entities$component, edit_service_usage(edit_entities$component, snap, unused))),
    "No questionnaire prints this yet, so changing it affects nothing else."
  )
  snap <- test_snapshot(edit_service_tables(edit_entities$tag))
  alone <- edit_service_row(edit_entities$tag, snap, "2")
  expect_match(
    text_of(edit_usage_ui(edit_entities$tag, edit_service_usage(edit_entities$tag, snap, alone))),
    "Nothing else uses this yet"
  )
  # and a row something points at lists it
  snap <- test_snapshot(edit_service_tables(edit_entities$registry))
  used <- edit_service_row(edit_entities$registry, snap, "2")
  expect_match(text_of(edit_usage_ui(edit_entities$registry, edit_service_usage(edit_entities$registry, snap, used))), "Example Study")
})

test_that("many rows pointing at a row are listed by questionnaire, the most first, every one of them", {
  snap <- test_snapshot(edit_service_tables(edit_entities$tag))
  respondent <- edit_service_row(edit_entities$tag, snap, "1")
  usage <- edit_service_usage(edit_entities$tag, snap, respondent)
  line <- usage$pointing[[1]]
  expect_equal(line$table, "variable_tag")
  expect_equal(line$n, 7)
  expect_equal(length(line$labels), line$n)
  expect_equal(sum(vapply(line$groups, function(group) length(group$labels), numeric(1))), line$n)
  sizes <- vapply(line$groups, function(group) length(group$labels), numeric(1))
  expect_equal(sizes, sort(sizes, decreasing = TRUE))
  html <- as.character(edit_usage_ui(edit_entities$tag, usage))
  expect_match(html, "<details", fixed = TRUE) # more than a few: folded by questionnaire
  expect_match(text_of(html), paste("in", length(line$groups), "questionnaires"))
})

test_that("a questionnaire's and a wave's details say the registry and study they belong to as tags, and a wave names its files", {
  details_of <- function(key, index = 1) {
    entity <- edit_entities[[key]]
    snap <- test_snapshot(edit_service_tables(entity))
    row <- snap[[entity$table]][index, ]
    fields <- edit_service_fields(entity, snap, "update", row)
    edit_service_details(entity, snap, row, fields, edit_service_initial(entity, fields, snap, "update", row))
  }
  questionnaire <- details_of("questionnaire")
  expect_equal(questionnaire$label, c("Registry", "Study", "Name", "Respondent Type", "Abbreviation", "Total participants", "Description", "On the dashboard"))
  expect_equal(questionnaire$value[questionnaire$label == "On the dashboard"], "Visible")
  expect_false("(empty)" %in% questionnaire$value[1:2])
  # what its own form doesn't change is a tag, which opens where it is changed; the rest are lines of the details.
  # The study's own (its investigators, its status) is on the study
  expect_equal(questionnaire$pill, c(TRUE, TRUE, rep(FALSE, 6)))
  expect_equal(questionnaire$kind[1:2], c("registry", "study"))
  expect_equal(questionnaire$key[1:2], c("1", "1"))
  wave <- details_of("wave")
  expect_equal(
    wave$label,
    c("Registry", "Study", "Questionnaire", "Respondent Type", "Wave name", "Date administered", "Number of respondents", "PDF file", "On the dashboard", "PDF")
  )
  # the name its PDF was uploaded under is the wave's own, changed on its form; the PDF's pages and size are facts
  expect_equal(wave$label[!wave$pill], c("Wave name", "Date administered", "Number of respondents", "PDF file", "On the dashboard"))
  expect_equal(wave$value[wave$label == "PDF file"], "AHP_Redacted.pdf")
  expect_equal(wave$value[wave$label == "PDF"], "12 pages · 2.1 MB")
  expect_true(is.na(wave$kind[wave$label == "PDF"])) # it opens nothing
  expect_false(grepl(" · ", wave$value[wave$label == "Questionnaire"], fixed = TRUE)) # not the drop-down's name
  # a wave that wasn't extracted from a PDF has no file to name, and says so
  typed <- details_of("wave", 2)
  expect_false("PDF file" %in% typed$label)
  expect_equal(typed$value[typed$label == "PDF"], "not extracted from a PDF on this site")
  # a study's registry and investigators are its own form's, so they are lines; a tag's kind is fixed, so it is a tag
  study <- details_of("study")
  expect_false(any(study$pill))
  expect_equal(study$label[1:3], c("Registry", "Investigator", "Contact"))
  tag <- details_of("tag")
  expect_equal(tag$label[tag$pill], "Kind of tag")
  # a wording's words are its title, and the questionnaires that print it are its cards: its tags are its scales
  entity <- edit_entities$component
  snap <- test_snapshot(edit_service_tables(entity))
  row <- edit_service_row(entity, snap, "9")
  fields <- edit_service_fields(entity, snap, "update", row)
  question <- edit_service_details(entity, snap, row, fields, edit_service_initial(entity, fields, snap, "update", row))
  expect_equal(question$label, "Measurement scale")
  expect_equal(question$value, "Short Mood Scale (SMS)")
  expect_equal(question$kind, "measurement_scale")
  expect_true(all(question$pill))
})

test_that("a kind with one field doesn't say it again under the title", {
  entity <- edit_entities$answer_option
  snap <- test_snapshot(edit_service_tables(entity))
  row <- snap$answer_option[1, ]
  fields <- edit_service_fields(entity, snap, "update", row)
  expect_equal(nrow(edit_service_details(entity, snap, row, fields, edit_service_initial(entity, fields, snap, "update", row))), 0)
})

test_that("a delete says what goes with it, what is only changed, what the database forgets, and how it falls on each questionnaire", {
  snap <- test_snapshot(edit_service_tables(edit_entities$placement))
  entity <- edit_entities$placement
  row <- edit_service_row(entity, snap, "104")
  plan <- edit_service_delete_plan(entity, snap, row)
  html <- text_of(edit_delete_ui(entity, "do you now smoke cigarettes?", plan))
  expect_match(html, "You're deleting this block do you now smoke cigarettes?", fixed = TRUE)
  expect_match(html, "6 other records will be deleted with it")
  expect_match(html, "All of it is removed together, at once.")
  expect_match(html, "Deleted along with it 2 questions in waves AHP Wave 1 · question 2 · do you now smoke cigarettes?", fixed = TRUE)
  expect_match(html, "Kept, but changed 1 block in a questionnaire will lose their question they depend on", fixed = TRUE)
  expect_match(html, "How it falls on each questionnaire AHP 1 block in a questionnaire · 2 questions in waves · 1 variable", fixed = TRUE)
  section <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "101"))
  expect_match(text_of(edit_delete_ui(entity, "a. about you", section)), "4 wordings no questionnaire prints any more (forgotten by the database, and taken out of any measurement scale)", fixed = TRUE)
  expect_false(grepl("propos", html))
})

test_that("a delete that something stands in the way of lists what, and offers no confirmation", {
  entity <- edit_entities$registry
  snap <- test_snapshot(edit_service_tables(entity))
  plan <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "1"))
  html <- text_of(edit_delete_ui(entity, "Mid-Atlantic Twin Registry", plan))
  expect_match(html, "This can't be deleted yet")
  expect_match(html, "2 studies are in the way")
  expect_match(html, "What to do: Empty the registry")
  expect_match(html, "Adult Health Study")
  expect_false(grepl("I understand", html))
  # one with nothing in the way asks to confirm, and a far-reaching one to type DELETE
  alone <- edit_service_delete_plan(edit_entities$tag, test_snapshot(edit_service_tables(edit_entities$tag)), edit_service_row(edit_entities$tag, test_snapshot(edit_service_tables(edit_entities$tag)), "2"))
  expect_false(grepl("DELETE", text_of(edit_delete_confirm_ui(shiny::NS("x"), alone))))
  far <- edit_service_delete_plan(entity <- edit_entities$questionnaire, snap <- test_snapshot(edit_service_tables(entity)), edit_service_row(entity, snap, "2"))
  expect_match(as.character(edit_delete_confirm_ui(shiny::NS("x"), far)), "type DELETE")
})

test_that("emptying asks with what would go and what stays", {
  html <- text_of(edit_empty_dialog(shiny::NS("x"), edit_entities$wave, "AHP Wave 1", "It asks 4 questions."))
  expect_match(html, "Empty AHP Wave 1\\?")
  expect_match(html, "It asks 4 questions.")
  expect_match(html, "What stays: the wave itself.")
  expect_match(html, "keeps the PDF it was extracted from")
  expect_match(html, "Answer choices and tags stay too")
  expect_match(html, "Empty it")
})

test_that("each item of a kind is a row of the list, which picks it, and the kind has + Add", {
  snap <- test_snapshot(edit_service_tables(edit_entities$component))
  cards <- edit_service_cards(edit_entities$component, snap, NULL)
  shared <- cards[cards$key == "1", ]
  html <- paste(as.character(make_edit_item("component", shared)), collapse = "")
  expect_match(text_of(html), "a. about you", fixed = TRUE)
  expect_match(html, 'class="outline-row edit-item" tabindex="0" data-action="select"', fixed = TRUE)
  expect_match(html, 'data-kind="component" data-key="1" data-scope=""', fixed = TRUE)
  expect_match(html, "btn-open") # the arrow that opens it
  expect_match(as.character(make_edit_item("component", shared, selected = TRUE)), "is-selected")
  expect_match(html, "type-chip") # its kind of block
  expect_match(text_of(html), "a. about you AHP", fixed = TRUE) # and under its words, the questionnaires that print it
  expect_false(grepl("type-chip", paste(as.character(make_edit_item("component", shared, chip = FALSE)), collapse = "")))
  # + Add in a list of wordings adds one to a questionnaire, and says which list, so the form starts as that kind
  add <- as.character(make_edit_add_button("question"))
  expect_match(add, 'data-action="add" data-kind="placement" data-list="question"', fixed = TRUE)
  expect_match(text_of(add), "Add Question")
  expect_match(as.character(make_edit_add_button("study")), 'data-action="add" data-kind="study" data-list="study"', fixed = TRUE)
  # a kind is a section that opens and closes, closed to begin with
  section <- as.character(make_edit_section("registry", "Registries", NULL))
  expect_match(section, 'class="outline-node is-collapsed"')
  expect_match(section, 'class="outline-row is-container" tabindex="0" data-action="toggle" data-id="registry" aria-expanded="false"')
  # a list is alphabetical; a study's row says its registry and who leads it, not its status
  studies <- make_edit_cards("study", test_snapshot(edit_service_tables(edit_entities$study)))
  expect_equal(studies$title, c("Adult Health Study", "Child Development Study", "Example Study"))
  expect_equal(studies$subtitle[1], "MATR · led by Dana Reyes")
  expect_equal(studies$scope, rep("", 3))
})

test_that("Re-extract is on only for a wave extracted from a PDF, and its dialog says the wave is emptied first", {
  snap <- test_snapshot("wave")
  on <- as.character(wave_reextract_button(snap$wave[1, ]))
  expect_false(grepl("disabled", on))
  off <- as.character(wave_reextract_button(snap$wave[2, ]))
  expect_match(off, "disabled")
  expect_match(off, "nothing to read again")
  dialog <- text_of(reextract_wave_dialog(shiny::NS("x"), "AHP Wave 1"))
  expect_match(dialog, "Re-extract AHP Wave 1\\?")
  expect_match(dialog, "The wave is emptied first")
  expect_match(dialog, "Empty and re-extract")
  expect_false(grepl("Nothing in the database changes|Change Requests", dialog))
})
