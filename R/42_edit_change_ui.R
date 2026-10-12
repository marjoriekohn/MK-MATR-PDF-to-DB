#' The form for adding or changing a row, with what you changed and what it reaches.
#'
#' For a delete: what goes with it and the confirmations. The server draws it all once the
#' kind and row are known.
#'
#' @param id The module's id.
#' @return The page's UI.
edit_change_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "edit-change-area",
    `data-ns` = ns(""),
    uiOutput(ns("main"))
  )
}

# The kind that adds the rows a reference points at, so a form can say "add
# one first" when a drop-down has nothing to pick.
edit_reference_kinds <- c(
  registry_id = "registry", principal_investigator_id = "investigator", study_id = "study",
  respondent_type_id = "respondent_type", questionnaire_id = "questionnaire",
  component_type_id = "component_type"
)

#' Notes above the form for drop-downs that need a pick and have nothing to pick from.
#'
#' Each has a link to add the first.
#'
#' @param fields The form's fields, from `edit_service_fields()`.
#' @return A UI element, or `NULL` if none is needed.
edit_empty_choices_ui <- function(fields) {
  notes <- lapply(fields, function(field) {
    real <- unname(field$choices)[nzchar(unname(field$choices))]
    if (field$type != "select" || !isTRUE(field$required) || length(real) > 0 || !is.null(field$locked) || !is.null(field$show_when)) {
      return(NULL)
    }
    kind <- edit_reference_kinds[field$name]
    if (is.na(kind) || kind == "") {
      return(NULL)
    }
    edit_callout(
      "warning",
      title = paste0("Nothing to pick for “", field$label, "” yet"),
      paste0("There are no ", edit_entities[[kind]]$plural, " in the database. "),
      review_button(paste("Add", edit_entities[[kind]]$noun, "first"), "jump", `data-kind` = kind, class = "btn btn-sm btn-outline-dark")
    )
  })
  Filter(Negate(is.null), notes)
}
