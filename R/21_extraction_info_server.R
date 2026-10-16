#' The form that says where an extraction goes (its Basic Info): its registry, study, questionnaire and wave.
#'
#' Four drop-downs, each holding what is in the one before it; the wave drop-down offers the
#' questionnaire's waves that ask no questions yet. Each ends with "+ Add new …": picking it opens the
#' pop-up that adds one (`quick_add_server()`), applied at once like any add, and the new row is picked
#' when the pop-up closes. Saving is up to the page. The drop-downs and checks use `catalog`, the live
#' tables as they were when the session started or when something was last added.
#'
#' The form is in the Change Requests dialog, and the pop-up takes the dialog's place, so `reopen` is
#' called to put the dialog back, with what was in the form, when the pop-up closes.
#'
#' @param id The module's id.
#' @param catalog The live tables the pages pick from, as `server.R` shares them: `get()` reads them
#'   (`questionnaire_service_get_catalog()`) and `refresh()` reads them again.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param record_id A reactive: the draft the form is about, so the wave check doesn't count it as a clash.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param reopen A function that shows the dialog the form is in again, or `NULL` if it isn't in one.
#' @return A list: `info` (what the form holds, as `extraction_service_basic_info()` shapes it),
#'   `missing` (the drop-downs still blank), `problems` (what to fix, by field) and `show`, a function
#'   that fills the form from a draft's record (or anything with an `info`), or empties it for `NULL`.
extraction_info_server <- function(
  id,
  catalog,
  extractions,
  record_id,
  me,
  connection,
  reopen = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ INITIALIZE --------------------------------------------------------
    # Read when needed, not once: adding something from a drop-down changes the catalog.
    now <- function() isolate(catalog$get())

    # Each drop-down: what it holds, then "+ Add new …" (once the one before it is picked).
    add_new <- function(noun) stats::setNames(add_new_choice, paste("+ Add new", noun))

    registry_choices <- function() {
      registries <- questionnaire_service_registry_choices(now())
      prompt <- if (length(registries) == 0) "No registries recorded yet" else "Select registry"
      c(stats::setNames("", prompt), registries, add_new("registry"))
    }

    study_choices <- function(registry_id) {
      if (!has_text(registry_id)) {
        return(c("Select a registry first" = ""))
      }
      studies <- questionnaire_service_study_choices(now(), registry_id)
      prompt <- if (length(studies) == 0) "No studies in this registry yet" else "Select study"
      c(stats::setNames("", prompt), studies, add_new("study"))
    }

    questionnaire_choices <- function(study_id) {
      if (!has_text(study_id)) {
        return(c("Select a study first" = ""))
      }
      questionnaires <- questionnaire_service_choices(now(), study_id)
      prompt <- if (length(questionnaires) == 0) "No questionnaires in this study yet" else "Select questionnaire"
      c(stats::setNames("", prompt), questionnaires, add_new("questionnaire"))
    }

    wave_choices <- function(questionnaire_id, counts) {
      if (!has_text(questionnaire_id)) {
        return(c("Select a questionnaire first" = ""))
      }
      waves <- questionnaire_service_empty_wave_choices(now(), questionnaire_id, counts)
      prompt <- if (length(waves) == 0) "No waves without questions yet" else "Select wave"
      c(stats::setNames("", prompt), waves, add_new("wave"))
    }

    quick <- quick_add_server("quick", connection, me, catalog)

    # How many questions each wave of the picked questionnaire asks, read when it is picked: only a wave
    # with none can take an extraction. If it can't be read, every wave is offered and the database
    # checks again when the request is approved.
    read_counts <- function(questionnaire_id) {
      tryCatch(questionnaire_service_wave_question_counts(connection, questionnaire_id), error = function(problem) integer())
    }
    wave_question_counts <- reactive(read_counts(picked(input$questionnaire_id)))

    # ------ REACTIVE ----------------------------------------------------------
    # "+ Add new …" isn't a pick.
    picked <- function(value) if (identical(value, add_new_choice)) "" else value %||% ""

    info <- reactive({
      extraction_service_basic_info(list(
        registry_id = picked(input$registry_id), study_id = picked(input$study_id),
        questionnaire_id = picked(input$questionnaire_id), wave_id = picked(input$wave_id)
      ))
    })

    missing <- reactive(extraction_service_missing(info()))

    problems <- reactive({
      wave <- extraction_service_recorded_wave_problem(
        info(), catalog$get(), wave_question_counts(), extractions$all(), record_id(), me
      )
      Filter(Negate(is.null), list(wave_id = wave))
    })

    # The picks before "+ Add new …" was chosen, to go back to if the pop-up is closed.
    last <- new.env()
    remember <- function(values) for (field in names(values)) assign(field, values[[field]], envir = last)
    before_adding <- function() {
      values <- isolate(info())
      for (field in c("registry_id", "study_id", "questionnaire_id", "wave_id")) {
        if (identical(isolate(input[[field]]), add_new_choice)) values[[field]] <- get0(field, envir = last, ifnotfound = "")
      }
      values
    }

    # The drop-downs get their choices here too, since the dialog is built afresh each time it opens.
    show <- function(record = NULL) {
      info <- extraction_service_place(extraction_service_basic_info(extraction_service_info(record)), now())
      counts <- read_counts(info$questionnaire_id)
      updateSelectInput(session, "registry_id", choices = registry_choices(), selected = info$registry_id)
      updateSelectInput(session, "study_id", choices = study_choices(info$registry_id), selected = info$study_id)
      updateSelectInput(
        session, "questionnaire_id",
        choices = questionnaire_choices(info$study_id), selected = info$questionnaire_id
      )
      updateSelectInput(session, "wave_id", choices = wave_choices(info$questionnaire_id, counts), selected = info$wave_id)
      remember(info[c("registry_id", "study_id", "questionnaire_id", "wave_id")])
    }

    # ------ Picking ---------------------------------------------------------------
    # A pick that isn't in the new list below it is cleared. "+ Add new …" opens its pop-up instead.
    observeEvent(input$registry_id,
      {
        if (identical(input$registry_id, add_new_choice)) {
          return(add_new_row("registry"))
        }
        remember(list(registry_id = input$registry_id))
        choices <- study_choices(input$registry_id)
        updateSelectInput(session, "study_id", choices = choices, selected = still_offered(input$study_id, choices))
      },
      ignoreInit = TRUE
    )

    observeEvent(input$study_id,
      {
        if (identical(input$study_id, add_new_choice)) {
          return(add_new_row("study"))
        }
        remember(list(study_id = input$study_id))
        choices <- questionnaire_choices(input$study_id)
        updateSelectInput(
          session, "questionnaire_id",
          choices = choices, selected = still_offered(input$questionnaire_id, choices)
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$questionnaire_id,
      {
        if (identical(input$questionnaire_id, add_new_choice)) {
          return(add_new_row("questionnaire"))
        }
        remember(list(questionnaire_id = input$questionnaire_id))
      },
      ignoreInit = TRUE
    )

    observeEvent(wave_question_counts(),
      {
        choices <- wave_choices(picked(input$questionnaire_id), wave_question_counts())
        updateSelectInput(session, "wave_id", choices = choices, selected = still_offered(input$wave_id, choices))
      },
      ignoreInit = TRUE
    )

    observeEvent(input$wave_id,
      {
        if (identical(input$wave_id, add_new_choice)) {
          return(add_new_row("wave"))
        }
        remember(list(wave_id = input$wave_id))
      },
      ignoreInit = TRUE
    )

    # ------ Adding what a drop-down lacks ---------------------------------------
    # The pop-up starts with what is picked above it, and refreshes the catalog before it calls back,
    # so the new row is in the choices. It takes the place of the dialog the form is in, so the dialog
    # is put back, with the new row picked (and what is above it, from the row itself); going back from
    # the pop-up puts it back as it was.
    come_back <- function(values) {
      if (is.function(reopen)) reopen()
      show(list(info = values))
    }

    add_new_row <- function(kind) {
      before <- before_adding()
      start <- switch(kind,
        study = list(study__registry_id = before$registry_id),
        questionnaire = list(questionnaire__study_id = before$study_id),
        wave = list(wave__questionnaire_id = before$questionnaire_id),
        list()
      )
      then <- function(key) {
        added <- edit_service_quick_row(now(), kind, key)
        come_back(switch(kind,
          registry = utils::modifyList(before, list(registry_id = key, study_id = "", questionnaire_id = "", wave_id = "")),
          study = utils::modifyList(before, list(
            registry_id = as.character(added$registry_id %||% before$registry_id), study_id = key,
            questionnaire_id = "", wave_id = ""
          )),
          questionnaire = utils::modifyList(before, list(
            registry_id = "", study_id = as.character(added$study_id %||% before$study_id), questionnaire_id = key, wave_id = ""
          )),
          wave = utils::modifyList(before, list(
            questionnaire_id = as.character(added$questionnaire_id %||% before$questionnaire_id), wave_id = key
          ))
        ))
      }
      opened <- quick$open(kind, values = start, then = then, back = function() come_back(before))
      # the pop-up couldn't open: the drop-down goes back to what it had
      if (!isTRUE(opened)) updateSelectInput(session, paste0(kind, "_id"), selected = before[[paste0(kind, "_id")]])
    }

    # ------ OUTPUT ------------------------------------------------------------
    # Under each drop-down: what to fix, if anything, or, for one still blank, a mark that outlines it
    # in red and says "Required" to screen readers.
    lapply(c("registry_id", "study_id", "questionnaire_id", "wave_id"), function(field) {
      output[[paste0(field, "_note")]] <- renderUI({
        problem <- problems()[[field]]
        if (!is.null(problem)) {
          return(span(class = "field-problem", problem))
        }
        if (field %in% extraction_service_missing_fields(info())) span(class = "field-missing visually-hidden", "Required")
      })
    })

    # ------ RETURN ------------------------------------------------------------
    list(info = info, missing = missing, problems = problems, show = show)
  })
}

still_offered <- function(value, choices) {
  if (isTRUE(value %in% choices) && !identical(value, add_new_choice)) value else ""
}
