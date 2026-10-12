# What a set of items changes, as a person reads it: the rows, grouped, and how many of each kind.

some_items <- function() {
  data.frame(
    item_number = 1:7,
    entity_name = c("component", "component", "questionnaire_component", "wave", "wave", "answer_option", "response_option"),
    record_identifier = c(
      "{\"component_id\":\"new:component:1\"}", "{\"component_id\":\"new:component:1\"}",
      "{\"component_id\":\"new:component:1\",\"questionnaire_id\":3}", "{\"wave_id\":2}", "{\"wave_id\":2}",
      "{\"answer_option_id\":39}", "{\"answer_option_id\":39,\"variable_key\":4}"
    ),
    action = c("Insert", "Insert", "Insert", "Update", "Update", "Delete", "Delete"),
    attribute_name = c("component_type_id", "text", "display_order", "name", "respondent_count", "standardized_label", NA),
    old_value = c(NA, NA, NA, "Wave 2", "10", "Sometimes", NA),
    new_value = c("4", "Welcome.", "1", "Round 2", NA, NA, NA),
    stringsAsFactors = FALSE
  )
}

test_that("items for the same row and action go together, in the order the rows first appear", {
  groups <- change_service_groups(some_items())
  expect_length(groups, 5)
  expect_equal(
    vapply(groups, function(group) group$table, character(1)),
    c("component", "questionnaire_component", "wave", "answer_option", "response_option")
  )
  expect_equal(vapply(groups, function(group) group$action, character(1)), c("Insert", "Insert", "Update", "Delete", "Delete"))

  component <- groups[[1]]
  expect_equal(component$fields$column, c("component_type_id", "text"))
  expect_equal(component$fields$new, c("4", "Welcome."))
  expect_true(all(is.na(component$fields$old)))

  wave <- groups[[3]]
  expect_equal(wave$fields$column, c("name", "respondent_count"))
  expect_equal(wave$fields$old, c("Wave 2", "10"))
  expect_true(is.na(wave$fields$new[2]))
})

test_that("a row with nothing but its key has no fields", {
  keyed <- change_service_groups(some_items())[[5]]
  expect_equal(keyed$table, "response_option")
  expect_equal(nrow(keyed$fields), 0)
})

test_that("a row is named by its key, and a row the edit adds says so", {
  expect_equal(change_service_row_name("{\"wave_id\":2}"), "wave_id 2")
  expect_equal(change_service_row_name("{\"component_id\":88,\"questionnaire_id\":3}"), "component_id 88, questionnaire_id 3")
  expect_equal(change_service_row_name("{\"component_id\":\"new:component:1\"}"), "component_id (new row 1)")
  expect_equal(change_service_row_name("{\"mesh_id\":\"D012907\"}"), "mesh_id D012907")
  expect_equal(change_service_row_name("not json"), "not json")
})

test_that("it counts the rows added, changed and removed, and the tables they are in", {
  counts <- change_service_counts(change_service_groups(some_items()))
  expect_equal(counts, list(added = 2, changed = 1, removed = 2, tables = 5))
  expect_equal(change_service_counts_text(counts), "Adds 2 rows, changes 1 row and removes 2 rows, in 5 tables.")
  expect_equal(change_service_counts_text(list(added = 0, changed = 1, removed = 0, tables = 1)), "Changes 1 row, in 1 table.")
  expect_equal(
    change_service_counts_text(list(added = 3, changed = 0, removed = 1, tables = 2)),
    "Adds 3 rows and removes 1 row, in 2 tables."
  )
  expect_equal(change_service_counts_text(change_service_counts(list())), "")
})

test_that("no items is no groups", {
  expect_equal(change_service_groups(NULL), list())
  expect_equal(change_service_groups(some_items()[0, ]), list())
})
