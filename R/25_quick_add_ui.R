#' The pop-up that adds a row from inside a Basic Info drop-down.
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
    lapply(names(edit_quick_needs[[entity$key]]), function(needed) {
      what <- edit_entities[[needed]]
      p(
        class = "quick-add-more",
        paste0("The ", what$noun, " isn't in the list? "),
        actionLink(ns("add_needed"), paste("Add", what$article, what$noun), icon = app_icon("add"))
      )
    }),
    edit_change_ui(ns("change")),
    uiOutput(ns("duplicates")),
    footer = tagList(
      actionButton(ns("cancel"), if (back) "Back" else "Cancel", class = "btn-outline-dark"),
      uiOutput(ns("blocker"), inline = TRUE, class = "quick-add-blocker"),
      actionButton(ns("add"), "Add it", class = "btn-primary", disabled = TRUE)
    ),
    easyClose = FALSE
  )
}
