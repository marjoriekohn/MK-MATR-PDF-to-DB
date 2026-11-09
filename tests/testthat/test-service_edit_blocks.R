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
      list(variable_type = "closed_ended", matr_id = test_name("SM"), subject = subject, time_frame = "past 30 days", response_options = list(choice("1", yes), choice("0", "no"))),
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
  expect_setequal(paste(tags$kind, tags$name), c(paste("subject", normal_subject(subject)), "time frame past 30 days", paste("unit", normal_unit(unit))))

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
