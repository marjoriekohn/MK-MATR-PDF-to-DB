#' The Extraction page: three ways to start an extraction of a questionnaire.
#'
#'   Manual          type the questionnaire on a blank Edit Extraction page
#'   AI-Powered      upload a PDF and an AI reads it in the background
#'   Existing Draft  pick up a draft whose change request isn't submitted yet
#'
#' All three end on the same Edit Extraction page, with the same kind of draft.
#'
#' @param id The module's id.
#' @return The page's UI.
extraction_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page extraction-page",
    `data-ns` = ns(""),
    page_header(
      "Extraction",
      "Add a questionnaire to the database.",
      toggle_input(
        ns("mode"), c("Manual" = "manual", "AI-Powered" = "ai", "Existing Draft" = "drafts"),
        selected = "ai", label = "How to start"
      )
    ),
    conditionalPanel("input.mode == 'manual'", ns = ns, extraction_manual_ui(ns("manual"))),
    conditionalPanel("input.mode == 'ai'", ns = ns, extraction_ai_ui(ns("ai"))),
    conditionalPanel("input.mode == 'drafts'", ns = ns, extraction_drafts_ui(ns("drafts")))
  )
}
