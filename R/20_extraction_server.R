#' The New Extraction page's server.
#'
#' Holds the two modes (Manual and AI-Powered). A draft still being read, opened from Extraction Drafts, is
#' shown in AI-Powered mode, and the reading card points back to Extraction Drafts. The page opens on
#' AI-Powered.
#'
#' @param id The module's id.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_edit_extraction A function taking a record and opening it on the Edit Extraction page.
#' @param show_page A function taking a page's name and showing it.
#' @return A list: `show_mode`, a function taking `"manual"` or `"ai"` that shows that mode (or `"drafts"`, the
#'   Extraction Drafts page), and `open_details`, a function taking a draft still being read and showing it.
extraction_server <- function(
  id,
  extractions,
  connection,
  me,
  open_edit_extraction,
  show_page
) {
  moduleServer(id, function(input, output, session) {
    # ------ MODES -------------------------------------------------------------
    show_mode <- function(mode) {
      if (identical(mode, "drafts")) {
        show_page("drafts")
        return()
      }
      updateRadioButtons(session, "mode", selected = mode)
      show_page("extraction")
    }

    extraction_manual_server("manual", extractions, connection, me, open_edit_extraction)
    ai <- extraction_ai_server("ai", extractions, connection, me, open_edit_extraction, show_mode)

    # ------ RETURN ------------------------------------------------------------
    list(
      show_mode = show_mode,
      open_details = function(record) {
        ai$open(record)
        show_mode("ai")
      }
    )
  })
}
