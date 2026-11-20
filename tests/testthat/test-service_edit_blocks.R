# Blocks in a questionnaire, against the real database: a question added whole (its wording, place, numbers,
# variables, choices and tags), moved, reworded and deleted, with the database numbering the waves' questions as
# the site says. A test makes its own tree, so what it adds goes with the tree.

variables_json <- function(...) as.character(jsonlite::toJSON(list(...), auto_unbox = TRUE))
choice <- function(code, words) list(code_value = code, answer_option = words)

# Builds and applies a change to a block of the tree's questionnaire, as the page does, from the tables as they are
# now. Returns what was applied, and notes the rows it added for removal when the test ends.
apply_block <- function(me, tree, verb, row_key = NULL, ..., env = parent.frame()) {
  entity <- edit_entities$placement
  snap <- edit_service_load(test_connection, entity)
  scope <- key_text(tree$questionnaire$questionnaire_id)
  row <- if (!is.null(row_key)) edit_service_row(entity, snap, key_text(row_key))
  built <- if (verb == "delete") {
    edit_service_build(entity, "delete", snap, row, scope)
  } else {
    fields <- edit_service_fields(entity, snap, verb, row, scope)
    values <- utils::modifyList(edit_service_initial(entity, fields, snap, verb, row, scope), list(...))
    edit_service_build(entity, verb, snap, row, scope, fields, values)
  }
  applied <- edit_service_apply(test_connection, built, me)
  local_added_rows(applied$new_ids, env = env)
  c(applied, list(summary = built$summary))
}

questionnaire_rows <- function(table, tree) {
  table_model_get(test_connection, table, list(questionnaire_id = database_eq(tree$questionnaire$questionnaire_id)))
}

test_that("a question is added whole, numbered in its wave, and a second one before it moves it along; deleting one numbers the rest again", {
  me <- local_staff()
  tree <- local_tree()
  words <- tolower(test_name("do you smoke"))
  yes <- tolower(test_name("yes")) # a new answer choice, and a new subject and unit, named with the marker
  subject <- test_name("Twin")
  unit <- tolower(test_name("cig"))
  first <- apply_block(
    me, tree, "add",
    placement__kind = "Question", placement__text = words, placement__asked_in = key_text(tree$wave$wave_id),
    placement__variables = variables_json(
      list(variable_type = "closed_ended", matr_id = test_name("SM"), subject = subject, time_frame = "past year", response_options = list(choice("1", yes), choice("0", "no"))),
      list(variable_type = "open_ended", matr_id = test_name("CG"), label = "cigarettes a day", answer_data_type = "NUMBER", unit = unit)
    )
  )
  expect_match(first$history_id, "^[0-9]+$")
  expect_true(all(c("new:component:1", "new:placement:1", "new:variable:1", "new:variable:2") %in% names(first$new_ids)))
  placed <- questionnaire_rows("placement", tree)
  expect_equal(placed$placement_id, first$new_ids[["new:placement:1"]])
  expect_equal(placed$display_order, 1)
  component <- table_model_get(test_connection, "component", list(component_id = database_eq(first$new_ids[["new:component:1"]])))
  expect_equal(component$kind, "Question")
  expect_equal(component$text, words)
  asked <- questionnaire_rows("wave_question", tree)
  expect_equal(asked$question_number, "1")
  expect_true(is.na(asked$printed_number))
  variables <- questionnaire_rows("variable", tree)
  variables <- variables[order(variables$display_order), ] # the table comes back in key order
  expect_equal(variables$variable_type, c("closed_ended", "open_ended"))
  expect_equal(variables$answer_data_type, c(NA, "NUMBER"))
  expect_equal(variables$display_order, c(1, 2))
  choices <- table_model_get(test_connection, "response_option", list(variable_key = database_eq(variables$variable_key[1])))
  choices <- choices[order(choices$display_order), ]
  expect_equal(choices$code_value, c("1", "0"))
  # a new answer choice was added in its normal form, and "no" is the one the list has
  options <- table_model_get(test_connection, "answer_option", list(answer_option_id = database_in(choices$answer_option_id)))
  expect_setequal(options$standardized_label, c(yes, "no"))
  expect_true("new:answer_option:1" %in% names(first$new_ids))
  tagged <- table_model_get(test_connection, "variable_tag", list(variable_key = database_in(variables$variable_key)))
  tags <- table_model_get(test_connection, "tag", list(tag_id = database_in(tagged$tag_id)))
  expect_setequal(paste(tags$kind, tags$name), c(paste("subject", normal_subject(subject)), "time frame past year", paste("unit", normal_unit(unit))))

  # a second question put first: the first moves along, and the wave numbers them again
  second <- apply_block(
    me, tree, "add",
    placement__kind = "Question", placement__text = tolower(test_name("how old are you")), placement__display_order = "1",
    placement__asked_in = key_text(tree$wave$wave_id), placement__variables = variables_json(
      list(variable_type = "open_ended", matr_id = test_name("AGE"), answer_data_type = "NUMBER", unit = "years")
    )
  )
  placed <- questionnaire_rows("placement", tree)
  expect_equal(placed$display_order[match(c(second$new_ids[["new:placement:1"]], first$new_ids[["new:placement:1"]]), placed$placement_id)], c(1, 2))
  asked <- questionnaire_rows("wave_question", tree)
  expect_equal(asked$question_number[match(c(second$new_ids[["new:placement:1"]], first$new_ids[["new:placement:1"]]), asked$placement_id)], c("1", "2"))

  # the second depends on the first once it is moved after it, and its printed number is its own
  moved <- apply_block(me, tree, "update", second$new_ids[["new:placement:1"]],
    placement__display_order = "2", placement__depends_on_placement_id = key_text(first$new_ids[["new:placement:1"]])
  )
  placed <- questionnaire_rows("placement", tree)
  expect_equal(placed$depends_on_placement_id[placed$placement_id == second$new_ids[["new:placement:1"]]], first$new_ids[["new:placement:1"]])
  expect_equal(placed$display_order[placed$placement_id == second$new_ids[["new:placement:1"]]], 2)
  asked <- questionnaire_rows("wave_question", tree)
  expect_equal(asked$question_number[asked$placement_id == first$new_ids[["new:placement:1"]]], "1")

  # deleting the first frees the second and numbers it 1; the first's component is forgotten by the database
  deleted <- apply_block(me, tree, "delete", first$new_ids[["new:placement:1"]])
  expect_match(deleted$summary, "^Deleted the block")
  placed <- questionnaire_rows("placement", tree)
  expect_equal(placed$placement_id, second$new_ids[["new:placement:1"]])
  expect_true(is.na(placed$depends_on_placement_id))
  expect_equal(questionnaire_rows("wave_question", tree)$question_number, "1")
  expect_equal(nrow(table_model_get(test_connection, "variable", list(placement_id = database_eq(first$new_ids[["new:placement:1"]])))), 0)
  expect_equal(nrow(table_model_get(test_connection, "component", list(component_id = database_eq(first$new_ids[["new:component:1"]])))), 0)
  # the answer choice and the tags stay: they are vocabulary (and go with the test's rows)
  expect_equal(nrow(table_model_get(test_connection, "tag", list(tag_id = database_in(tagged$tag_id)))), 3)
})

test_that("a block reworded in one place prints the component with the new words, and a wave emptied asks nothing", {
  me <- local_staff()
  tree <- local_tree()
  section <- apply_block(me, tree, "add", placement__kind = "Section", placement__text = tolower(test_name("a. about you")))
  new_words <- tolower(test_name("a. you"))
  reworded <- apply_block(me, tree, "update", section$new_ids[["new:placement:1"]], placement__text = new_words)
  placed <- questionnaire_rows("placement", tree)
  expect_equal(placed$component_id, reworded$new_ids[["new:component:1"]])
  expect_equal(table_model_get(test_connection, "component", list(component_id = database_eq(reworded$new_ids[["new:component:1"]])))$text, new_words)
  # the old wording is forgotten by the database, since nothing prints it
  expect_equal(nrow(table_model_get(test_connection, "component", list(component_id = database_eq(section$new_ids[["new:component:1"]])))), 0)
  question <- apply_block(
    me, tree, "add",
    placement__kind = "Question", placement__text = tolower(test_name("q")), placement__parent_placement_id = key_text(section$new_ids[["new:placement:1"]]),
    placement__asked_in = key_text(tree$wave$wave_id), placement__variables = variables_json(list(variable_type = "open_ended", matr_id = test_name("V"), answer_data_type = "TEXT"))
  )
  expect_equal(edit_service_empty_preview(test_connection, "wave", tree$wave$wave_id), "It asks 1 question.")
  expect_equal(edit_service_empty(test_connection, "wave", tree$wave$wave_id, me), "Removed 1 question, 2 blocks and 1 variable.")
  expect_equal(nrow(questionnaire_rows("placement", tree)), 0) # the question no wave asks goes, and the section left empty
  expect_equal(nrow(table_model_get(test_connection, "wave", list(wave_id = database_eq(tree$wave$wave_id)))), 1)
})

# A question added to the tree's questionnaire with one write-in variable, asked in the waves given (their ids), inside
# the block `inside` names, if it names one.
add_question <- function(me, tree, words, waves, inside = NULL, env = parent.frame()) {
  apply_block(
    me, tree, "add", env = env,
    placement__kind = "Question", placement__text = tolower(test_name(words)),
    placement__parent_placement_id = if (is.null(inside)) "" else key_text(inside),
    placement__asked_in = paste(key_text(waves), collapse = ","),
    placement__variables = variables_json(list(variable_type = "open_ended", matr_id = test_name("V"), answer_data_type = "TEXT"))
  )
}
placed_id <- function(applied) applied$new_ids[["new:placement:1"]]
variables_at <- function(applied) nrow(table_model_get(test_connection, "variable", list(placement_id = database_eq(placed_id(applied)))))

test_that("deleting a section forgets the wordings of the blocks inside it, as it does its own, when nothing else prints them", {
  me <- local_staff()
  tree <- local_tree()
  section <- apply_block(me, tree, "add", placement__kind = "Section", placement__text = tolower(test_name("a. inside")))
  question <- add_question(me, tree, "inside", tree$wave$wave_id, inside = placed_id(section))
  apply_block(me, tree, "delete", placed_id(section))
  expect_equal(nrow(questionnaire_rows("placement", tree)), 0)
  kept <- function(applied) nrow(table_model_get(test_connection, "component", list(component_id = database_eq(applied$new_ids[["new:component:1"]]))))
  expect_equal(kept(section), 0)
  expect_equal(kept(question), 0) # taken with the section, and printed nowhere else
})

test_that("emptying a wave takes the questions it asked, and leaves a question no wave asks that it never asked", {
  me <- local_staff()
  tree <- local_tree()
  other <- local_wave(tree$questionnaire)
  add_question(me, tree, "asked here", tree$wave$wave_id)
  kept <- add_question(me, tree, "asked there", other$wave_id)
  test_delete("wave_question", list(wave_id = database_eq(other$wave_id))) # as when its own wave is deleted: it stays
  edit_service_empty(test_connection, "wave", tree$wave$wave_id, me)
  expect_equal(questionnaire_rows("placement", tree)$placement_id, placed_id(kept))
  expect_equal(variables_at(kept), 1)
})

test_that("emptying a wave leaves what another wave asks as it was, and emptying a study or registry leaves the others alone", {
  me <- local_staff()
  tree <- local_tree()
  other <- local_wave(tree$questionnaire)
  both <- add_question(me, tree, "both", c(tree$wave$wave_id, other$wave_id))
  add_question(me, tree, "only here", tree$wave$wave_id)
  edit_service_empty(test_connection, "wave", tree$wave$wave_id, me)
  expect_equal(questionnaire_rows("placement", tree)$placement_id, placed_id(both))
  expect_equal(variables_at(both), 1)
  expect_equal(table_model_get(test_connection, "wave_question", list(wave_id = database_eq(other$wave_id)))$question_number, "1")
  expect_equal(nrow(table_model_get(test_connection, "wave_question", list(wave_id = database_eq(tree$wave$wave_id)))), 0)

  elsewhere <- local_tree()
  theirs <- add_question(me, elsewhere, "theirs", elsewhere$wave$wave_id)
  edit_service_empty(test_connection, "study", tree$study$study_id, me)
  expect_equal(nrow(questionnaire_rows("placement", tree)), 0)
  expect_equal(questionnaire_rows("placement", elsewhere)$placement_id, placed_id(theirs))
  add_question(me, tree, "again", tree$wave$wave_id)
  edit_service_empty(test_connection, "registry", tree$registry$registry_id, me)
  expect_equal(nrow(questionnaire_rows("placement", tree)), 0)
  expect_equal(questionnaire_rows("placement", elsewhere)$placement_id, placed_id(theirs))
  expect_equal(variables_at(theirs), 1)
})

test_that("an answer choice merged into another keeps each variable's code and order for it, and its other choices as they were", {
  me <- local_staff()
  # made before the tree, so it is removed after the variable that will offer it
  into <- local_row("answer_option", list(standardized_label = normal_answer(test_name("certainly"))), "answer_option_id")
  tree <- local_tree()
  draft <- test_questionnaire_draft()
  draft$response_options$answer_option <- normal_answer(paste(test_name("merged"), c("yes", "no")))
  local_pushed_questionnaire(me, tree, draft)
  variables <- table_model_get(test_connection, "variable", list(questionnaire_id = database_eq(tree$questionnaire$questionnaire_id)))
  key <- variables$variable_key[variables$variable_type == "closed_ended"]
  offered <- function() {
    rows <- table_model_get(test_connection, "response_option", list(variable_key = database_eq(key)))
    rows[order(rows$display_order), c("answer_option_id", "display_order", "code_value")]
  }
  before <- offered()

  entity <- edit_entities$answer_option
  snap <- edit_service_load(test_connection, entity)
  plan <- edit_service_merge_plan(entity, snap, edit_service_row(entity, snap, key_text(before$answer_option_id[1])), key_text(into$answer_option_id))
  expect_length(plan$blocked, 0)
  edit_service_apply(test_connection, plan, me)

  after <- offered()
  expect_equal(after$answer_option_id, c(into$answer_option_id, before$answer_option_id[2]))
  expect_equal(after$code_value, before$code_value)
  expect_equal(after$display_order, c(1, 2))
  expect_equal(nrow(table_model_get(test_connection, "answer_option", list(answer_option_id = database_eq(before$answer_option_id[1])))), 0)
})
