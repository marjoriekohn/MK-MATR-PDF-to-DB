#' The pop-ups that add a row from inside a Basic Info drop-down.
#'
#' A registry, study, questionnaire or wave that is missing from a drop-down can be added where staff are
#' working. It is the Make an edit form for one row (`edit_change_server()`), and like any add it is
#' applied at once for STAFF and ADMIN alike and written to the Edit history. What was typed is checked
#' for something like it already there as it is typed; that is a warning, not a block. What a form
#' needs that isn't there yet (a study's investigator, a questionnaire's respondent type;
#' `edit_quick_needs`) can be added from its pop-up, which comes back with the new one picked.
#'
#' @param id The module's id.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param catalog The live tables the pages pick from, as `server.R` shares them: `get()` reads them and
#'   `refresh()` reads them again.
#' @return A list with `open`, a function taking a kind (one of `edit_quick_kinds`), `values` (what to
#'   start the form with, by field id), `then` (a function called with the new row's key as text) and
#'   `back` (a function that goes back to what was open before); it returns whether the pop-up opened.
quick_add_server <- function(
  id,
  connection,
  me,
  catalog
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    kind <- reactiveVal(NULL) # the kind of the open pop-up
    opened <- reactiveVal(0L) # ticks each time one opens, so its form is drawn afresh
    snapshot <- reactiveVal(NULL) # the tables the kind needs, as they are now
    prefill <- reactiveVal(NULL) # what the form starts with
    after <- reactiveVal(NULL) # what to do with the new row's key
    before <- reactiveVal(NULL) # how to go back to the pop-up this one was opened from
    adding <- reactiveVal(FALSE) # while the row is being written, so a double click adds it once

    entity <- reactive({
      opened()
      if (!is.null(kind())) edit_service_quick_entity(kind())
    })
    change <- edit_change_server("change", entity, reactive("add"), snapshot, reactive(NULL), reactive(""), prefill)

    found <- reactive({
      tryCatch(
        {
          req(entity(), snapshot())
          edit_service_duplicates(entity(), snapshot(), change$fields(), change$values())
        },
        shiny.silent.error = function(problem) list()
      )
    })

    # ------ Opening -----------------------------------------------------------
    open <- function(key, values = list(), then = NULL, back = NULL) {
      kind_now <- edit_service_quick_entity(key)
      tables <- tryCatch(edit_service_load(connection, kind_now), error = function(problem) {
        show_problem(problem, "Couldn't open the form")
        NULL
      })
      if (is.null(tables)) {
        return(invisible(FALSE))
      }
      kind(key)
      snapshot(tables)
      after(then)
      before(back)
      adding(FALSE)
      prefill(if (length(values) > 0) {
        list(form = list(action = "add", entity = key, scope = "", target = "", values = values))
      })
      opened(opened() + 1L)
      showModal(quick_add_dialog(kind_now, ns, back = is.function(back)))
      invisible(TRUE)
    }

    # ------ Buttons -----------------------------------------------------------
    observe({
      updateActionButton(
        session, "add",
        label = if (length(found()) > 0) "Add it anyway" else "Add it",
        disabled = !isTRUE(change$ready()) || adding()
      )
    })

    output$duplicates <- renderUI({
      matches <- found()
      if (length(matches) > 0) edit_duplicates_inline(matches)
    })

    # What is left to fill in, next to the button.
    output$blocker <- renderUI({
      if (!isTRUE(change$ready())) span(class = "form-text", change$blockers()[1])
    })

    observeEvent(input$cancel, {
      removeModal()
      back <- before()
      if (is.function(back)) back()
    })

    # What the form needs that isn't in its list (a study's investigator, a questionnaire's respondent
    # type): add one, and come back to the form with it picked.
    add_needed <- function(needed) {
      field <- edit_quick_needs[[kind()]][[needed]]
      started <- change$values()
      outer <- kind()
      then <- after()
      back <- before()
      reopen <- function(extra = list()) open(outer, values = utils::modifyList(started, extra), then = then, back = back)
      open(
        needed,
        then = function(key) reopen(stats::setNames(list(key), field)),
        back = function() reopen()
      )
    }
    observeEvent(input$add_needed, add_needed(names(edit_quick_needs[[kind()]])[1]))

    # The form's own "Add one first" buttons: only what the form needs can be added from here.
    observeEvent(change$jump(), {
      wanted <- edit_entities[[change$jump()$kind]]
      if (change$jump()$kind %in% names(edit_quick_needs[[kind()]])) {
        add_needed(change$jump()$kind)
      } else {
        showNotification(
          paste0("Add ", wanted$article, " ", wanted$noun, " on Make an edit first."),
          type = "warning"
        )
      }
    })

    observeEvent(input$add, {
      req(change$ready(), !adding())
      adding(TRUE)
      result <- tryCatch(
        edit_service_quick_add(connection, entity(), snapshot(), change$fields(), change$values(), me),
        error = function(problem) {
          show_problem(problem, "Couldn't add it")
          NULL
        }
      )
      adding(FALSE)
      if (is.null(result)) {
        return()
      }
      removeModal()
      catalog$refresh()
      showNotification(paste0(result$record$summary, ". It is in the Edit history."), type = "message")
      then <- after()
      if (is.function(then)) then(result$key)
    })

    # ------ RETURN ------------------------------------------------------------
    list(open = open)
  })
}
