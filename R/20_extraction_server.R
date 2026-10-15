#' The Extraction page's server.
#'
#' Holds the three modes (Manual, AI-Powered and Existing Draft) and moves between them: opening a
#' draft that is still being read shows it in AI-Powered mode, and the reading card points to Existing
#' Draft. The page opens on AI-Powered.
#'
#' @param id The module's id.
#' @param extractions The extraction drafts as `server.R` shares them: `all()` is a reactive list
#'   of records, and `changed()` tells it this session changed one.
#' @param catalog The live tables the pages pick from, as `server.R` shares them: `get()` reads them
#'   (`questionnaire_service_get_catalog()`) and `refresh()` reads them again.
#' @param connection The MATR database, `list(url, key)`; see `model_database.R`.
#' @param me Who is using the site: `list(staff_id, name, role)`.
#' @param open_edit_extraction A function taking a record and opening it on the Edit Extraction page.
#' @return A list with `show_mode`, a function taking `"manual"`, `"ai"` or `"drafts"` that shows that mode.
extraction_server <- function(
  id,
  extractions,
  catalog,
  connection,
  me,
  open_edit_extraction
) {
  moduleServer(id, function(input, output, session) {
    # ------ MODES -------------------------------------------------------------
    show_mode <- function(mode) updateRadioButtons(session, "mode", selected = mode)

    extraction_manual_server("manual", extractions, connection, me, open_edit_extraction)
    ai <- extraction_ai_server("ai", extractions, connection, me, open_edit_extraction, show_mode)
    extraction_drafts_server(
      "drafts", extractions, catalog, connection, me,
      open_edit_extraction,
      open_details = function(record) {
        ai$open(record)
        show_mode("ai")
      },
      show_mode
    )

    # ------ RETURN ------------------------------------------------------------
    list(show_mode = show_mode)
  })
}
