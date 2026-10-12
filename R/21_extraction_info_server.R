#' The questionnaire and wave form.
#'
#' Saving is up to the page. The drop-downs and checks use `catalog`, the live tables as they were
#' when the session started or when something was last added from a drop-down. A study,
#' a Respondent Type or the investigator a study needs that is missing from a drop-down can be added
#' from the link under it (`quick_add_server()`); it is applied at once and picked in the drop-down.
#'
#' @param id The module's id.
#' @param catalog The live tables the pages pick from, as `server.R` shares them: `get()` reads them
#'   (`questionnaire_service_get_catalog()`) and `refresh()` reads them again.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param record_id A reactive: the draft the form is about, so the wave name check doesn't
#'   count it as a clash.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @return A list: `info` (what the form holds), `missing` (the required fields still blank),
#'   `problems` (what to fix in the others, by field) and `show`, a function that fills the
#'   form from a draft's record, or empties it for `NULL`.
extraction_info_server <- function(
  id,
  catalog,
  extractions,
  record_id,
  me,
  connection
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ INITIALIZE --------------------------------------------------------
    # Read when needed, not once: adding something from a drop-down changes the catalog.
    now <- function() isolate(catalog$get())

    registry_choices <- function() {
      choices_or_none(
        questionnaire_service_registry_choices(now()), "Select registry",
        "No registries recorded yet. Add one on Make an edit first"
      )
    }
    respondent_type_choices <- function() {
      choices_or_none(
        questionnaire_service_respondent_type_choices(now()), "Select Respondent Type",
        "No Respondent Types recorded yet. Add one with the link below"
      )
    }

    # Each list holds what's in the one above it.
    study_choices <- function(registry_id) {
      if (!nzchar(registry_id %||% "")) {
        return(c("Select a registry first" = ""))
      }
      studies <- questionnaire_service_study_choices(now(), registry_id)
      if (length(studies) == 0) {
        return(c("No studies recorded for this registry" = ""))
      }
      c("Select study" = "", studies)
    }

    questionnaire_choices <- function(study_id) {
      if (!nzchar(study_id %||% "")) {
        return(c("Select a study first" = ""))
      }
      questionnaires <- questionnaire_service_choices(now(), study_id)
      if (length(questionnaires) == 0) {
        return(c("No questionnaires recorded for this study" = ""))
      }
      c("Select questionnaire" = "", questionnaires)
    }

    quick <- quick_add_server("quick", connection, me, catalog)

    # ------ REACTIVE ----------------------------------------------------------
    info <- reactive({
      stats::setNames(
        lapply(extraction_info_fields, function(field) input[[field]] %||% ""),
        extraction_info_fields
      )
    })

    missing <- reactive(extraction_service_missing(info()))

    problems <- reactive({
      info <- info()
      live <- catalog$get()
      problems <- list(
        name = extraction_service_name_problem(info, live),
        abbreviation = extraction_service_abbreviation_problem(info, live),
        wave_name = extraction_service_wave_name_problem(info, live, extractions$all(), record_id(), me),
        administered_date = extraction_service_date_problem(info$administered_date),
        respondent_count = extraction_service_count_problem(info$respondent_count)
      )
      Filter(Negate(is.null), problems)
    })

    # The drop-downs get their choices here too, since the review page builds
    # the form afresh each time it opens it.
    show <- function(record = NULL) {
      info <- extraction_service_place(extraction_service_info(record), now())
      updateSelectInput(session, "registry_id", choices = registry_choices(), selected = info$registry_id)
      updateSelectInput(session, "study_id", choices = study_choices(info$registry_id), selected = info$study_id)
      updateRadioButtons(session, "kind", selected = info$kind)
      updateSelectInput(
        session, "questionnaire_id",
        choices = questionnaire_choices(info$study_id), selected = info$questionnaire_id
      )
      updateSelectInput(session, "respondent_type_id", choices = respondent_type_choices(), selected = info$respondent_type_id)
      for (field in c("name", "abbreviation", "wave_name", "administered_date", "respondent_count")) {
        updateTextInput(session, field, value = info[[field]])
      }
      for (field in c("description", "notes")) {
        updateTextAreaInput(session, field, value = info[[field]])
      }
    }

    # ------ Picking a registry and study ----------------------------------------
    # A pick that isn't in the new list is cleared.
    observeEvent(input$registry_id,
      {
        choices <- study_choices(input$registry_id)
        updateSelectInput(session, "study_id", choices = choices, selected = still_offered(input$study_id, choices))
      },
      ignoreInit = TRUE
    )

    observeEvent(input$study_id,
      {
        choices <- questionnaire_choices(input$study_id)
        updateSelectInput(
          session, "questionnaire_id",
          choices = choices, selected = still_offered(input$questionnaire_id, choices)
        )
      },
      ignoreInit = TRUE
    )

    # ------ Adding what a drop-down lacks ---------------------------------------
    # The pop-up refreshes the catalog before it calls back, so the new row is in the choices.
    observeEvent(input$add_respondent_type, {
      quick$open("respondent_type", then = function(key) {
        updateSelectInput(session, "respondent_type_id", choices = respondent_type_choices(), selected = key)
      })
    })

    observeEvent(input$add_study, {
      start <- list(study__registry_id = isolate(input$registry_id) %||% "")
      quick$open("study", values = start, then = function(key) {
        added <- edit_service_quick_row(now(), "study", key)
        registry <- as.character(added$registry_id %||% "")
        updateSelectInput(session, "registry_id", choices = registry_choices(), selected = registry)
        updateSelectInput(session, "study_id", choices = study_choices(registry), selected = key)
        # A study just added has no questionnaires, so this one is its first.
        updateRadioButtons(session, "kind", selected = "new_questionnaire")
      })
    })

    # ------ OUTPUT ------------------------------------------------------------
    # Under each text box: what to fix, if anything.
    lapply(c("name", "abbreviation", "wave_name", "administered_date", "respondent_count"), function(field) {
      output[[paste0(field, "_note")]] <- renderUI({
        problem <- problems()[[field]]
        if (!is.null(problem)) span(class = "field-problem", problem)
      })
    })

    # ------ RETURN ------------------------------------------------------------
    list(info = info, missing = missing, problems = problems, show = show)
  })
}

# A drop-down's choices led by `prompt`; with nothing to choose, one line saying so.
choices_or_none <- function(choices, prompt, none) {
  if (length(choices) == 0) {
    return(stats::setNames("", none))
  }
  c(stats::setNames("", prompt), choices)
}

still_offered <- function(value, choices) {
  if (isTRUE(value %in% choices)) value else ""
}
