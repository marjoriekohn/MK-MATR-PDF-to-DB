# Where an extraction goes in the MATR metadata database (its Basic Info): its registry, study,
# questionnaire and wave.
# Filled in on the Edit Extraction page, in the dialog that pushes the extraction into the database.
# Four drop-downs in one row, each holding what is in the one before it. The extraction always goes
# into a recorded questionnaire's wave that asks no questions yet; one that isn't there is added from
# the drop-down's own "+ Add new …" choice, which opens the pop-up that adds it
# (`quick_add_server()`). Like every form on the site the drop-downs have no labels: the empty choice
# says what each holds, and a blank one is outlined in red, since the extraction can't be pushed
# without it. Their choices and the notes under them come from the server.
# Under the study, the questionnaire and the wave picked is whether each shows on the dashboard researchers
# search, as a button that hides it or shows it again (`visibility_button()`): everything is shown until staff
# hide it.
# Returns: The form's UI.
extraction_info_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "extraction-info", `data-ns` = ns(""), # its buttons report to this form (www/js/review.js)
    div(
      class = "field-row basic-info-row",
      select_field(ns, "registry_id", "Registry"),
      select_field(ns, "study_id", "Study", visibility = TRUE),
      select_field(ns, "questionnaire_id", "Questionnaire", visibility = TRUE),
      select_field(ns, "wave_id", "Wave", visibility = TRUE)
    )
  )
}

# Whether the study, questionnaire or wave picked shows on the dashboard, as a button that changes it: an open eye
# and "Visible on the dashboard", or a struck eye and "Hidden from the dashboard". Pressing it hides it, or shows
# it again, at once (it is the row's own, not the extraction's, so it doesn't wait for the push).
# kind: `"study"`, `"questionnaire"` or `"wave"`.
# hidden: Whether it is hidden now.
visibility_button <- function(kind, hidden) {
  review_button(
    tagList(app_icon(if (hidden) "hidden" else "visible"), span(if (hidden) "Hidden from the dashboard" else "Visible on the dashboard")),
    "visibility",
    `data-kind` = kind, `aria-pressed` = if (hidden) "true" else "false",
    class = c("btn btn-sm visibility-toggle", if (hidden) "is-hidden"),
    title = sprintf(
      if (hidden) {
        "This %s is kept off the dashboard researchers search. Press to show it there. It changes at once."
      } else {
        "This %s shows on the dashboard researchers search. Press to hide it there. It changes at once."
      },
      kind
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
# visibility: Whether what it picks can be hidden from the dashboard: then the server draws its button under it.
select_field <- function(ns, input_id, name, visibility = FALSE) {
  note_id <- ns(paste0(input_id, "_note"))
  menu <- selectInput(ns(input_id), NULL, choices = NULL, width = "100%", selectize = FALSE)
  div(
    class = "form-field",
    tag_input_attributes(
      menu, "select",
      `aria-label` = name, title = name, `aria-describedby` = note_id, `aria-required` = "true"
    ),
    uiOutput(note_id, class = "form-text field-note"),
    if (visibility) uiOutput(ns(paste0(input_id, "_visibility")), class = "visibility-slot")
  )
}

# Add attributes to the element inside a Shiny input's markup.
# field: A Shiny input.
# element: The HTML element to find inside it, e.g. `"select"`.
# ...: The attributes.
# Returns: The input, changed.
tag_input_attributes <- function(field, element, ...) {
  htmltools::tagQuery(field)$find(element)$addAttrs(...)$allTags()
}
