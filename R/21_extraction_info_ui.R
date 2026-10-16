#' Where an extraction goes in the MATR metadata database (its Basic Info): its registry, study,
#' questionnaire and wave.
#'
#' Filled in on Change Requests, after its change request is submitted from the Edit Extraction page.
#' Four drop-downs in one row, each holding what is in the one before it. The extraction always goes
#' into a recorded questionnaire's wave that asks no questions yet; one that isn't there is added from
#' the drop-down's own "+ Add new …" choice, which opens the pop-up that adds it
#' (`quick_add_server()`). Like every form on the site the drop-downs have no labels: the empty choice
#' says what each holds, and a blank one is outlined in red, since the request can't be approved
#' without it. Their choices and the notes under them come from the server.
#'
#' @param id The module's id.
#' @return The form's UI.
extraction_info_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "extraction-info",
    div(
      class = "field-row basic-info-row",
      select_field(ns, "registry_id", "Registry"),
      select_field(ns, "study_id", "Study"),
      select_field(ns, "questionnaire_id", "Questionnaire"),
      select_field(ns, "wave_id", "Wave")
    )
  )
}

# The value of a drop-down's "+ Add new …" choice.
add_new_choice <- "__add__"

# ------ Fields ------------------------------------------------------------------------
# A drop-down whose choices the server fills; its empty choice says what it holds ("Select study"), and
# `name` is its label for screen readers and when pointed at. Under it is a note the server fills: what
# to fix, if anything, or (said only to screen readers) that it is still blank. Either way its border
# turns red.
select_field <- function(ns, input_id, name) {
  note_id <- ns(paste0(input_id, "_note"))
  menu <- selectInput(ns(input_id), NULL, choices = NULL, width = "100%", selectize = FALSE)
  div(
    class = "form-field",
    tag_input_attributes(
      menu, "select",
      `aria-label` = name, title = name, `aria-describedby` = note_id, `aria-required` = "true"
    ),
    uiOutput(note_id, class = "form-text field-note")
  )
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
