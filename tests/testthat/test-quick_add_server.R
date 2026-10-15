# The pop-ups that add a respondent type, a study or an investigator from inside an extraction drop-down. They use the
# Make an edit form for one row, apply it at once, and call back with the new row's key. The database is the real one.
#
# popup_args <- function(me, catalog = test_live_catalog()) {
#   list(connection = test_connection, me = me, catalog = catalog)
# }
#
# # The browser draws the pop-up's form and sends back its token; here the form is drawn by reading its output and
# # the token sent back by hand, then what is typed (by field id).
# fill <- function(session, output, change, ...) {
#   output[["change-main"]]
#   typed <- stats::setNames(list(...), paste0("change-", names(list(...))))
#   do.call(session$setInputs, c(list(`change-form_token` = as.character(change$token())), typed))
# }
#
# test_that("opening a pop-up shows its form, and a table that can't be read is a sentence and no pop-up", {
#   me <- local_staff()
#   seen <- local_browser()
#   shiny::testServer(quick_add_server, args = popup_args(me), {
#     expect_true(open("respondent_type"))
#     expect_length(seen$modals, 1)
#     expect_match(seen$modals[[1]], "Add a respondent type")
#     expect_match(seen$modals[[1]], "Cancel")
#     expect_error(open("registry"), "can't be added from here")
#   })
#   shiny::testServer(quick_add_server, args = list(connection = list(url = test_connection$url, key = "sb_secret_not_the_key"), me = me, catalog = fixed_catalog()), {
#     expect_false(open("respondent_type"))
#     expect_match(notice_texts(seen)[1], "^Couldn't open the form: The MATR database turned down the key")
#     expect_length(seen$modals, 1) # no second pop-up
#   })
# })
#
# test_that("a respondent type typed into the pop-up is added at once, the pop-up closes, and the form gets its key", {
#   me <- local_staff()
#   seen <- local_browser()
#   catalog <- test_live_catalog()
#   name <- test_name("Pop-up type")
#   withr::defer(test_delete("respondent_type", list(name = database_eq(name))))
#   got <- NULL
#   shiny::testServer(quick_add_server, args = popup_args(me, catalog), {
#     open("respondent_type", then = function(key) got <<- key)
#     fill(session, output, change, respondent_type__name = name, respondent_type__minimum_age = "5")
#     expect_true(change$ready())
#     session$setInputs(add = 1)
#
#     expect_match(got, "^[0-9]+$")
#     rows <- table_model_get(test_connection, "respondent_type", list(respondent_type_id = database_eq(got)))
#     expect_equal(rows$name, name)
#     expect_equal(rows$minimum_age, 5)
#     expect_equal(seen$closed, 1L)
#     expect_equal(notice_texts(seen)[1], paste0("Added the respondent type “", name, "”. It is in the Edit history."))
#     # the catalog was read again, so the drop-down can offer the new row
#     expect_true(got %in% as.character(catalog$get()$respondent_types$respondent_type_id))
#   })
#   entry <- history_model_get_all(test_connection)
#   entry <- Filter(function(record) identical(record$prepared_by, me$name), entry)[[1]]
#   expect_equal(entry$kind, "Add")
#   expect_equal(entry$note, "From page 2 of the PDF")
# })
#
# test_that("the button waits for a form that is ready, and says 'Add it anyway' when what is typed looks like something there", {
#   me <- local_staff()
#   seen <- local_browser()
#   existing <- local_respondent_type(minimum_age = 21, maximum_age = 24)
#   shiny::testServer(quick_add_server, args = popup_args(me), {
#     open("respondent_type")
#     fill(session, output, change, respondent_type__name = "")
#     session$flushReact()
#     expect_true(last_update(seen, "add")$disabled)
#     expect_equal(text_of(output$blocker), "Fill in the name.")
#
#     fill(session, output, change, respondent_type__name = test_name("Looks the same"), respondent_type__minimum_age = "21", respondent_type__maximum_age = "24")
#     session$flushReact()
#     expect_false(last_update(seen, "add")$disabled)
#     expect_equal(last_update(seen, "add")$label, "Add it anyway")
#     expect_match(text_of(output$duplicates), "This looks like something that's already there")
#     expect_match(text_of(output$duplicates), "A respondent type with the same age range")
#     expect_match(text_of(output$duplicates), existing$name)
#
#     fill(session, output, change, respondent_type__minimum_age = "22", respondent_type__maximum_age = "23")
#     session$flushReact()
#     expect_equal(last_update(seen, "add")$label, "Add it")
#     expect_equal(text_of(output$duplicates), "")
#   })
# })
#
# test_that("an add the database refuses is a sentence, the pop-up stays open, and nothing is added", {
#   me <- local_staff()
#   seen <- local_browser()
#   name <- test_name("Too old")
#   got <- NULL
#   shiny::testServer(quick_add_server, args = popup_args(me), {
#     open("respondent_type", then = function(key) got <<- key)
#     # the database stops an age over 120, which the form doesn't know about
#     fill(session, output, change, respondent_type__name = name, respondent_type__maximum_age = "150")
#     expect_true(change$ready())
#     session$setInputs(add = 1)
#     expect_null(got)
#     expect_equal(seen$closed, 0L)
#     expect_match(notice_texts(seen)[length(notice_texts(seen))], "^Couldn't add it: .*respondent_type_maximum_age_check")
#     expect_false(adding())
#   })
#   expect_equal(nrow(table_model_get(test_connection, "respondent_type", list(name = database_eq(name)))), 0)
# })
#
# test_that("a study's lead investigator that isn't in the list is added from the study's pop-up, and the study comes back with them picked", {
#   me <- local_staff()
#   seen <- local_browser()
#   registry <- local_registry()
#   name <- test_name("Pop-up investigator")
#   withr::defer(test_delete("principal_investigator", list(name = database_eq(name))))
#   shiny::testServer(quick_add_server, args = popup_args(me), {
#     open("study", values = list(study__registry_id = as.character(registry$registry_id)))
#     # (what the browser sends back for the registry the form was drawn with)
#     fill(session, output, change, study__name = "Half typed study", study__registry_id = as.character(registry$registry_id))
#     session$setInputs(add_investigator = 1)
#     expect_match(seen$modals[[2]], "Add an investigator")
#     expect_match(seen$modals[[2]], "Back") # the button goes back to the study
#
#     fill(session, output, change, investigator__name = name, investigator__email = "popup@example.test")
#     session$setInputs(add = 1)
#     # the study's pop-up is open again, drawn with what was typed, the registry and the new investigator
#     expect_match(seen$modals[[3]], "Add a study")
#     investigator <- database_model_get(test_connection, "principal_investigator", "principal_investigator_id", filters = list(name = database_eq(name)))
#     html <- paste(output[["change-main"]]$html, collapse = "")
#     expect_match(html, 'value="Half typed study"', fixed = TRUE)
#     expect_match(html, sprintf('<option value="%d" selected>', registry$registry_id), fixed = TRUE)
#     expect_match(html, sprintf('<option value="%d" selected>', investigator$principal_investigator_id), fixed = TRUE)
#   })
# })
#
# test_that("Back from the investigator's pop-up returns to the study as it was typed, and Cancel just closes", {
#   me <- local_staff()
#   seen <- local_browser()
#   shiny::testServer(quick_add_server, args = popup_args(me), {
#     open("study", values = list(study__registry_id = "7"))
#     fill(session, output, change, study__name = "Kept", study__registry_id = "7")
#     session$setInputs(add_investigator = 1)
#     session$setInputs(cancel = 1) # Back
#     expect_match(seen$modals[[3]], "Add a study")
#     expect_match(paste(output[["change-main"]]$html, collapse = ""), 'value="Kept"', fixed = TRUE)
#     expect_equal(seen$closed, 1L)
#
#     session$setInputs(cancel = 2) # Cancel on the study itself
#     expect_equal(seen$closed, 2L)
#     expect_length(seen$modals, 3)
#   })
# })
#
# test_that("the form's own 'add one first' button for a missing investigator opens the investigator's pop-up, and other kinds say where to add them", {
#   me <- local_staff()
#   seen <- local_browser()
#   shiny::testServer(quick_add_server, args = popup_args(me), {
#     open("study")
#     fill(session, output, change, study__name = "Typed")
#     change$jump(list(kind = "investigator", verb = "add", at = Sys.time()))
#     session$flushReact()
#     expect_match(seen$modals[[2]], "Add an investigator")
#     change$jump(list(kind = "registry", verb = "add", at = Sys.time()))
#     session$flushReact()
#     expect_equal(notice_texts(seen)[length(notice_texts(seen))], "Add a registry on Make an edit first.")
#   })
# })

test_that("the pop-up draws only the form and its buttons", {
  html <- text_of(quick_add_dialog(edit_service_quick_entity("study"), shiny::NS("q"), back = FALSE))
  expect_match(html, "Add a study")
  expect_match(html, "the questionnaire you are extracting is added to it when its change request is approved")
  expect_match(html, "The lead investigator isn't in the list\\? Add an investigator")
  expect_no_match(html, "Why are you adding it")
  expect_no_match(text_of(quick_add_dialog(edit_service_quick_entity("respondent_type"), shiny::NS("q"))), "lead investigator")
  expect_match(text_of(quick_add_dialog(edit_service_quick_entity("study"), shiny::NS("q"), back = TRUE)), "Back")
  link <- htmltools::renderTags(quick_add_link("go", "Not in the list? Add a thing"))$html
  expect_match(link, 'id="go"')
})
