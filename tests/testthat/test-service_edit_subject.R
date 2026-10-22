# The Subject kind of Make an edit: staff keep the list of who a variable can be about.

test_that("every subject is a card, by its name", {
  snap <- test_snapshot(edit_service_tables(edit_entities$subject))
  cards <- edit_service_cards(edit_entities$subject, snap, NULL)
  expect_equal(cards$title, c("Respondent", "Respondent's Children", "Respondent's Family", "Respondent's Siblings"))
  expect_equal(cards$key, c("1", "3", "2", "4"))
})

test_that("a subject needs a name, and a name that is in the list already is a problem, ignoring case and spaces", {
  form <- form_for("subject", "add")
  expect_equal(edit_service_check(form$fields, form$values)$missing, "subject")

  taken <- form_for("subject", "add", subject__name = " respondent ")
  expect_equal(edit_service_check(taken$fields, taken$values)$problems$subject__name, "“Respondent” is already in the list.")

  fresh <- form_for("subject", "add", subject__name = "Respondent's Parents")
  expect_length(edit_service_check(fresh$fields, fresh$values)$problems, 0)
  expect_length(edit_service_check(fresh$fields, fresh$values)$missing, 0)
})

test_that("a subject may keep its own name when it is changed, but not take another's", {
  same <- form_for("subject", "update", "1", subject__name = "RESPONDENT")
  expect_length(edit_service_check(same$fields, same$values)$problems, 0)
  other <- form_for("subject", "update", "1", subject__name = "respondent's family")
  expect_equal(edit_service_check(other$fields, other$values)$problems$subject__name, "“Respondent's Family” is already in the list.")
})

test_that("adding a subject is one Insert, on a row with no ID yet", {
  form <- form_for("subject", "add", subject__name = "Respondent's Parents")
  expect_equal(item_lines(form_items(form)$items), "Insert subject {\"subject_id\":\"new:subject:1\"} name: - -> Respondent's Parents")
})

test_that("changing a subject is one Update, and a change to nothing has no items", {
  form <- form_for("subject", "update", "1", subject__name = "Myself")
  expect_equal(item_lines(form_items(form)$items), "Update subject {\"subject_id\":1} name: Respondent -> Myself")
  expect_equal(nrow(form_items(form_for("subject", "update", "1"))$items), 0)
})

test_that("a subject no variable is about can be deleted, and nothing goes with it", {
  entity <- edit_entities$subject
  snap <- test_snapshot(edit_service_tables(entity))
  row <- edit_service_row(entity, snap, "2")
  plan <- edit_service_delete_plan(entity, snap, row)
  expect_length(plan$blocked, 0)
  expect_equal(plan$reach$total, 0)
  expect_false(plan$far_reaching)
  built <- edit_service_build(entity, "delete", snap, row, NULL, plan = plan)
  expect_equal(item_lines(built$items), "Delete subject {\"subject_id\":2} name: Respondent's Family -> -")
  usage <- edit_service_usage(entity, snap, row)
  expect_length(usage$pointing, 0)
  expect_equal(usage$shared, 0)
})

test_that("a subject that variables are about can't be deleted, and the cards say how many", {
  entity <- edit_entities$subject
  snap <- test_snapshot(edit_service_tables(entity))
  expect_true("variable" %in% edit_service_tables(entity))
  plan <- edit_service_delete_plan(entity, snap, edit_service_row(entity, snap, "1"))
  expect_length(plan$blocked, 1)
  expect_match(plan$blocked[[1]]$text, "7 variables are in the way")
  expect_match(plan$blocked[[1]]$fix, "renamed instead")

  cards <- edit_service_cards(entity, snap, NULL)
  facts <- stats::setNames(vapply(cards$facts, paste, character(1), collapse = " "), cards$title)
  expect_equal(facts[["Respondent"]], "about 7 variables")
  expect_equal(facts[["Respondent's Children"]], "about 1 variable")
  expect_equal(facts[["Respondent's Family"]], "about no variables")
})

test_that("a subject is read like the other tables, by its key and name, in the order of its name", {
  # (that the database gives it so is checked in test-model_table.R)
  expect_equal(database_tables$subject$key, "subject_id")
  rows <- fixture_table("subject")
  expect_named(rows, c("subject_id", "name"))
  expect_equal(rows$name[1], "Respondent")
})

test_that("the Subject kind is a section of Make an edit's list", {
  expect_equal(make_edit_kinds$subject, c("Subjects", "Subject"))
})
