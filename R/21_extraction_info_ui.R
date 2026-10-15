#' Where an extraction goes in the MATR metadata database: its registry, study, questionnaire and wave.
#'
#' Picked when its change request is previewed and submitted from the Edit Extraction page.
#' The questionnaire and the wave can each be one that is recorded or a new one;
#' a recorded wave has to have no questions yet. The drop-downs' choices and the notes under the fields
#' come from the server. Required fields have a * after their label.
#'
#' @param id The module's id.
#' @return The page's UI.
extraction_info_ui <- function(id) {
  ns <- NS(id)
  # One step at a time: the study appears once a registry is picked, and the questionnaire and wave once a
  # study is. (A hidden field still holds its value, so a draft opened here keeps what it had.)
  div(
    class = "extraction-info",
    div(
      class = "field-row",
      select_field(ns("registry_id"), "Registry", required = TRUE),
      conditionalPanel(
        "input.registry_id != ''",
        ns = ns,
        select_field(
          ns("study_id"), "Study",
          required = TRUE, add = quick_add_link(ns("add_study"), "Not in the list? Add a study")
        )
      )
    ),
    conditionalPanel(
      "input.registry_id != '' && input.study_id != ''",
      ns = ns,
      questionnaire_fields(ns),
      wave_fields(ns)
    ),
    textAreaInput(
      ns("notes"), "Notes for reviewers",
      rows = 3, width = "100%",
      placeholder = "Anything unusual about this PDF, e.g. page 6 is a scanned insert"
    )
  )
}

# A questionnaire never recorded, or a new wave of a recorded one.
questionnaire_fields <- function(ns) {
  tagList(
    div(
      class = "kind-toggle",
      toggle_input(
        ns("kind"),
        c("New questionnaire" = "new_questionnaire", "New wave" = "new_wave"),
        selected = "new_wave", label = "New or existing questionnaire"
      )
    ),
    conditionalPanel(
      "input.kind == 'new_questionnaire'",
      ns = ns,
      div(
        class = "field-row",
        text_field(ns, "name", "Questionnaire name", "e.g. Adult Health and Personality", required = TRUE),
        text_field(ns, "abbreviation", "Abbreviation", "e.g. AHP", required = TRUE, class = "is-short"),
        select_field(
          ns("respondent_type_id"), "Respondent Type",
          required = TRUE, add = quick_add_link(ns("add_respondent_type"), "Not in the list? Add a Respondent Type")
        )
      ),
      textAreaInput(ns("description"), "Description", rows = 2, width = "100%")
    ),
    conditionalPanel(
      "input.kind == 'new_wave'",
      ns = ns,
      select_field(ns("questionnaire_id"), "Questionnaire", required = TRUE)
    )
  )
}

# Wave name, date and respondents side by side.
wave_fields <- function(ns) {
  div(
    class = "field-row",
    text_field(ns, "wave_name", "Wave name", "e.g. Wave 3", required = TRUE),
    text_field(ns, "administered_date", "Date administered", "MM/DD/YYYY"),
    text_field(ns, "respondent_count", "Number of respondents", "e.g. 1,200")
  )
}

# ------ Fields ------------------------------------------------------------------------
# A text box with a note under it that the server fills with what to fix, if
# anything; then its border turns red.
text_field <- function(ns, input_id, label, placeholder, required = FALSE, class = NULL) {
  note_id <- ns(paste0(input_id, "_note"))
  box <- textInput(ns(input_id), field_label(label, required), placeholder = placeholder, width = "100%")
  div(
    class = c("form-field", class),
    tag_input_attributes(box, "input", `aria-describedby` = note_id, `aria-required` = if (required) "true"),
    uiOutput(note_id, class = "form-text field-note")
  )
}

# A drop-down whose choices the server fills, with `add` (a link that adds what it lacks) under it.
select_field <- function(input_id, label, required = FALSE, add = NULL) {
  menu <- selectInput(input_id, field_label(label, required), choices = NULL, width = "100%", selectize = FALSE)
  div(class = "form-field", tag_input_attributes(menu, "select", `aria-required` = if (required) "true"), add)
}

#' A field's label.
#'
#' @param label The label.
#' @param required Whether to add a * after it.
#' @return A UI element or a string.
field_label <- function(label, required = FALSE) {
  if (!required) {
    return(label)
  }
  tagList(label, span(class = "required-mark", `aria-hidden` = "true", "*"))
}

#' Add attributes to the element inside a Shiny input's markup.
#'
#' @param field A Shiny input.
#' @param element The HTML element to find inside it, e.g. `"select"`.
#' @param ... The attributes.
#' @return The input, changed.
tag_input_attributes <- function(field, element, ...) {
  htmltools::tagQuery(field)$find(element)$addAttrs(...)$allTags()
}
