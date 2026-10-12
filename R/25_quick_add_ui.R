#' The pop-up that adds a row from inside an extraction drop-down.
#'
#' It holds the Make an edit form for one kind (`edit_change_ui()`), a note that says when what was typed
#' looks like something already there, and the buttons.
#'
#' @param entity The kind, from `edit_service_quick_entity()`.
#' @param ns The module's namespace function.
#' @param back Whether Cancel goes back to another pop-up (the study's, from the investigator's).
#' @return A `modalDialog()`, for `showModal()`.
quick_add_dialog <- function(entity, ns, back = FALSE) {
  modalDialog(
    title = paste("Add", entity$article, entity$noun),
    size = "l",
    class = "quick-add",
    p(class = "form-text", entity$about),
    if (entity$key == "study") {
      p(
        class = "quick-add-more",
        "The lead investigator isn't in the list? ",
        actionLink(ns("add_investigator"), "Add an investigator", icon = icon("plus"))
      )
    },
    edit_change_ui(ns("change")),
    uiOutput(ns("duplicates")),
    textAreaInput(
      ns("note"), "Why are you adding it? (optional)",
      rows = 2, width = "100%",
      placeholder = "Reviewers can see this in the Edit history. Say where the information came from."
    ),
    footer = tagList(
      actionButton(ns("cancel"), if (back) "Back" else "Cancel", class = "btn-outline-dark"),
      uiOutput(ns("blocker"), inline = TRUE, class = "quick-add-blocker"),
      actionButton(ns("add"), "Add it", class = "btn-primary", disabled = TRUE)
    ),
    easyClose = FALSE
  )
}

#' The link under a drop-down that adds what is missing from it.
#'
#' @param input_id The link's input id.
#' @param text What it says, e.g. "Not in the list? Add a respondent type".
#' @return A UI element.
quick_add_link <- function(input_id, text) {
  div(class = "quick-add-link", actionLink(input_id, text, icon = icon("plus")))
}
