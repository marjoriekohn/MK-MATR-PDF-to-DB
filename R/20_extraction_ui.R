# The New Extraction page: two ways to start an extraction of a questionnaire.
#  Manual          type the questionnaire in yourself, on a blank Edit Extraction page
#  AI-Powered      upload a PDF and an AI reads it, in the background
# Both end on the same Edit Extraction page, with the same kind of draft. A draft that isn't pushed yet is
# picked up again on the Extraction Drafts page (`extraction_drafts_page_ui()`).
extraction_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "page extraction-page",
    `data-ns` = ns(""),
    page_header(
      "New Extraction",
      "Upload a PDF and supplemental files.",
      toggle_input(
        ns("mode"), c("Manual" = "manual", "AI-Powered" = "ai"),
        selected = "ai", label = "How to start"
      ),
      eyebrow = "Start here", icon_name = "upload"
    ),
    conditionalPanel("input.mode == 'manual'", ns = ns, extraction_manual_ui(ns("manual"))),
    conditionalPanel("input.mode == 'ai'", ns = ns, extraction_ai_ui(ns("ai")))
  )
}
