#' The Make an edit page.
#'
#' Everything staff can add, change or delete in the MATR metadata database, one item at a time, laid out as
#' Edit Extraction lays out a questionnaire: an outline on the left and the item picked on the right. Each kind of
#' thing is a section of the outline, closed until it is clicked; open, it lists that kind's items and + Add. A
#' kind that belongs to a questionnaire (its blocks, its questions) has a section for each questionnaire inside
#' it. The details show the item picked, with Edit and Delete; Edit and + Add show its form, with Save. Saved
#' edits that aren't finished are listed above.
#'
#' @param id The module's id.
#' @return The page's UI.
make_edit_ui <- function(id) {
  ns <- NS(id)
  as_fill_carrier(div(
    id = ns("main"),
    class = "page edit-page",
    `data-ns` = ns(""),
    page_header("Make an edit", "Add, change or delete what is in the MATR metadata database, one item at a time."),
    uiOutput(ns("drafts")),
    card(
      class = "structure-card",
      card_body(
        class = "structure-body", padding = 0,
        div(
          class = "outline-panel", `data-ns` = ns(""),
          div(class = "outline-toolbar", outline_search("Search the lists that are open")),
          div(
            class = "outline-split",
            div(
              class = "outline-body",
              div(class = "outline-tree", div(class = "outline-list", lapply(names(make_edit_kinds), function(kind) {
                make_edit_section(kind, make_edit_kinds[[kind]][1], uiOutput(ns(paste0("items_", kind)), class = "outline-list"))
              }))),
              p(class = "outline-filter-empty", "Nothing in the open lists matches.")
            ),
            div(
              class = "detail-panel", role = "region", `aria-label` = "The item picked",
              uiOutput(ns("detail"), class = "detail-body")
            )
          )
        )
      )
    )
  ))
}

# The kinds of thing, in the order the list has them: what the list calls them, and one of them (for + Add).
# Blocks and Questions are as printed in one questionnaire; the shared wording is a block's or question's words
# in every questionnaire that prints them.
make_edit_kinds <- list(
  registry = c("Registries", "Registry"),
  study = c("Studies", "Study"),
  questionnaire = c("Questionnaires", "Questionnaire"),
  wave = c("Waves", "Wave"),
  questionnaire_component = c("Blocks", "Block"),
  questionnaire_question = c("Questions", "Question"),
  investigator = c("Principal Investigators", "Principal Investigator"),
  respondent_type = c("Respondent Types", "Respondent Type"),
  subject = c("Subjects", "Subject"),
  answer_option = c("Answer Choices", "Answer Choice"),
  category = c("Categories", "Category"),
  component_type = c("Component Types", "Component Type"),
  component = c("Shared Block Wording", "Block Wording"),
  question = c("Shared Question Wording", "Question Wording")
)

#' A section of the list: its row, which opens and closes it (the page script does it at once), and what is
#' inside it, drawn only once it is opened (Shiny doesn't draw what is hidden).
#'
#' @param id The section's id: a kind, or a kind and a questionnaire ("questionnaire_question:12").
#' @param label What the row says.
#' @param inside What is in it.
#' @return A UI element.
make_edit_section <- function(id, label, inside) {
  div(
    class = "outline-node is-collapsed", `data-id` = id,
    div(
      class = "outline-row is-container", tabindex = "0", `data-action` = "toggle", `data-id` = id, `aria-expanded` = "false",
      span(class = "outline-text", label)
    ),
    inside
  )
}

#' One item of a kind, in its section: its kind of block (for a block), its name, and the arrow that opens it in
#' the details. Clicking it opens it there too.
#'
#' @param kind The kind, a name of `edit_entities`.
#' @param card One of the kind's cards, from `edit_service_cards()`.
#' @param scope The questionnaire's key, for a kind that belongs to one; otherwise "".
#' @param selected Whether it is the one in the details.
#' @return A UI element.
make_edit_item <- function(kind, card, scope = "", selected = FALSE) {
  id <- paste(kind, scope, card$key, sep = "|")
  picks <- list(`data-id` = id, `data-kind` = kind, `data-key` = card$key, `data-scope` = scope)
  div(
    class = "outline-node", `data-id` = id,
    div(
      class = c("outline-row", "edit-item", if (selected) "is-selected"), tabindex = "0", `data-action` = "select", !!!picks,
      if (!is.na(card$tag)) type_chip(card$tag),
      span(class = "outline-text", card$title, if (nzchar(card$subtitle)) span(class = "edit-item-meta", card$subtitle)),
      do.call(review_button, c(
        list(app_icon("open"), "select", class = "btn btn-sm btn-icon btn-open", title = "Open", `aria-label` = paste("Open", card$title)),
        picks
      ))
    )
  )
}

#' + Add, at the end of a kind's section (or of a questionnaire's, for a kind that belongs to one).
#'
#' @param kind The kind, a name of `edit_entities`.
#' @param scope The questionnaire's key, or "".
#' @return A UI element.
make_edit_add_button <- function(kind, scope = "") {
  review_button(
    icon_label("add", paste("Add", make_edit_kinds[[kind]][2])), "add",
    `data-kind` = kind, `data-scope` = scope, class = "btn btn-outline-dark btn-wide edit-add"
  )
}
