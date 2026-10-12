#' The cards of the rows staff can change or delete.
#'
#' Once one is picked, just that card. Its buttons use the page script's `data-action`.
#'
#' @param id The module's id.
#' @return The page's UI.
edit_find_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "edit-find",
    `data-ns` = ns(""),
    uiOutput(ns("search")),
    uiOutput(ns("cards"))
  )
}
