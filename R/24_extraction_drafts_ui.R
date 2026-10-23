#' The Extraction Drafts page: every extraction draft whose change request isn't submitted yet, a card each.
#'
#' @param id The module's id.
#' @return The page's UI.
extraction_drafts_page_ui <- function(id) {
  div(
    class = "page extraction-page drafts-page",
    page_header("Extraction Drafts", "Pick up a draft whose change request isn't submitted yet."),
    extraction_drafts_ui(id)
  )
}

#' The list of drafts on the Extraction Drafts page: whose to list, what the dots mean, and a card each.
#'
#' @param id The module's id.
#' @return A UI element.
extraction_drafts_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "extraction-mode extraction-drafts",
    `data-ns` = ns(""),
    mode_intro(uiOutput(ns("whose_toggle"))),
    progress_key(),
    uiOutput(ns("drafts"))
  )
}

# What the dots on the cards mean, in the theme's colours: green Done, gold In progress, red Needs attention.
progress_key <- function() {
  item <- function(state, label) span(class = "progress-step", progress_dot(state), label)
  div(
    class = "progress-key", role = "group", `aria-label` = "What the dots mean",
    item("done", "Done"),
    item("in_progress", "In progress"),
    item("not_started", "Not started"),
    item("attention", "Needs attention")
  )
}

#' "Mine (1)" and "Everyone's (3)": whose drafts are listed.
#'
#' @param input_id The input's id; its value is `"mine"` or `"everyone"`.
#' @param whose The one picked now.
#' @param n_mine,n_everyone The counts shown on the buttons.
#' @return A UI element.
whose_buttons <- function(input_id, whose, n_mine, n_everyone) {
  choices <- c("mine", "everyone")
  names(choices) <- c(sprintf("Mine (%d)", n_mine), sprintf("Everyone's (%d)", n_everyone))
  toggle_input(input_id, choices, whose, label = "Whose drafts")
}

# ------ One card per draft ---------------------------------------------------------
#' One card for a draft.
#'
#' Whose it is is said the same way on every card, from who started it: "Yours", or their name. While
#' someone else is working on it (`draft_row_service_editor()`), it says so, and Continue gives way to
#' Take over: only one person at a time works on a draft.
#'
#' @param record An extraction record.
#' @param draft The questionnaire it read, or `NULL` until reading has finished.
#' @param catalog The live tables the pages pick from, from `questionnaire_service_get_catalog()`.
#' @param me Who is using the site: `list(staff_id, name)`.
#' @param now The time now, which says whether someone's claim on it has lapsed.
#' @return A UI element.
draft_card <- function(record, draft, catalog, me, now = Sys.time()) {
  mine <- draft_row_service_is_mine(record, me)
  editor <- draft_row_service_editor(record, me, now)
  failed <- identical(record$extraction_status, "failed")
  sent_back <- record$sent_back
  steps <- extraction_service_progress(record, draft)
  div(
    class = "list-card",
    div(class = "draft-thumbnail", `aria-hidden` = "true", lapply(1:5, function(i) span())),
    div(
      class = "list-card-body",
      div(
        class = "list-card-title-row",
        h2(class = "list-card-title", extraction_service_title(record, catalog, draft$questionnaire)),
        row_badge(if (mine) "Yours" else draft_row_service_preparer(record), class = if (mine) "is-mine"),
        if (!is.null(sent_back)) row_badge("Sent back", class = "is-warning"),
        if (identical(record$method, extraction_manual)) row_badge("Typed by hand"),
        if (failed) row_badge("Reading stopped", class = "is-warning"),
        if (!is.null(editor)) row_badge(app_icon("edit"), sprintf(" %s is actively working on this", editor$by), class = "is-editing")
      ),
      p(class = "list-card-meta", draft_meta(record, me)),
      if (!is.null(sent_back)) {
        div(class = "list-card-note", strong(paste0(sent_back$by, ":")), paste0("“", sent_back$note, "”"))
      },
      div(class = "draft-progress", map_rows(steps, function(step) {
        span(class = "progress-step", progress_dot(step$state), step$label)
      }))
    ),
    div(
      class = "list-card-actions",
      if (mine) review_button("Discard", "discard", `data-id` = record$id, class = "btn btn-link"),
      if (failed) {
        review_button("Try reading again", "retry", `data-id` = record$id, class = "btn btn-outline-dark")
      } else if (!is.null(editor)) {
        tagList(
          review_button("Take over", "take_over", `data-id` = record$id, class = "btn btn-outline-dark"),
          review_button(
            "Continue", "continue",
            `data-id` = record$id, class = "btn btn-primary", disabled = TRUE,
            title = sprintf("%s is working on it. Only one person at a time can.", editor$by)
          )
        )
      } else {
        review_button(
          "Continue", "continue",
          `data-id` = record$id, class = "btn btn-primary",
          title = if (!extraction_service_is_read(record)) "Still being read: see its progress"
        )
      }
    )
  )
}

# "Started by you on Sep 29 · last saved 2 hours ago · AHP_wave3.pdf, 14 pages". What there is nothing to
# say about (no PDF, no reading error) is left out, so no piece is empty.
draft_meta <- function(record, me) {
  who <- if (draft_row_service_is_mine(record, me)) "you" else draft_row_service_preparer(record)
  pdf <- record$pdf
  pieces <- c(
    paste("Started by", who, day_phrase(parse_time(record$started))),
    if (!is.null(record$sent_back)) {
      paste("sent back by", record$sent_back$by %||% "a reviewer", day_phrase(parse_time(record$sent_back$date)))
    } else {
      paste("last saved", time_ago(parse_time(record$saved)))
    },
    if (identical(record$extraction_status, "failed")) record$error,
    paste(c(pdf$name, if (!is.null(pdf$pages)) plural(pdf$pages, "page")), collapse = ", ")
  )
  paste(pieces[has_text(pieces)], collapse = " · ")
}

#' What is shown when there are no drafts.
#'
#' @param whose `"mine"` or `"everyone"`.
#' @return A UI element.
no_drafts <- function(whose) {
  div(
    class = "list-card is-empty",
    p(if (whose == "mine") "You have no drafts in progress." else "There are no drafts in progress."),
    review_button("Start a new extraction", "new", class = "btn btn-outline-dark")
  )
}
