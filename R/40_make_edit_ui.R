#' The Make an edit page.
#'
#' Finish the sentence "I want to ___ a ___.", find the row if it is already there, change
#' it, and send it for review. Each step appears once the one before it is done. The find
#' and change modules draw the middle; this is the sentence at the top and the bar at the
#' bottom.
#'
#' @param id The module's id.
#' @return The page's UI.
make_edit_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("main"),
    class = "page edit-page",
    `data-ns` = ns(""),
    page_header(
      "Make an edit",
      "Add, update, or delete an item from the database.",
      uiOutput(ns("steps"), inline = TRUE)
    ),
    uiOutput(ns("drafts")),
    card(class = "edit-sentence-card", card_body(
      edit_sentence_ui(ns),
      uiOutput(ns("explain"))
    )),
    uiOutput(ns("where")),
    edit_find_ui(ns("find")),
    edit_change_ui(ns("change")),
    uiOutput(ns("sent")),
    uiOutput(ns("footer"))
  )
}
