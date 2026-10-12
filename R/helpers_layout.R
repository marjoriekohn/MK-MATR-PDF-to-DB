# ------ Page building blocks ----------------------------------------------------
# Pieces every page uses: its title, toggles, and the dialogs and notices that report what happened.

#' A page's title and the sentence under it.
#'
#' @param title The title.
#' @param subtitle The sentence under it.
#' @param ... Anything else (buttons, a save indicator), shown on the right.
#' @return A UI element.
page_header <- function(title, subtitle, ...) {
  div(
    class = "page-header",
    div(h1(title), p(class = "subhead", subtitle)),
    div(class = "page-header-aside", ...)
  )
}

#' A coloured circle that shows how far something has got.
#'
#' @param state `"done"`, `"in_progress"`, `"not_started"` or `"attention"`.
#' @return A UI element.
progress_dot <- function(state) {
  span(class = paste0("progress-dot is-", state), `aria-hidden` = "true")
}

# ------ Toggles -------------------------------------------------------------------
#' Side-by-side buttons that pick one of a few values, the picked one filled in.
#'
#' For example Mine / Everyone's, New / Existing questionnaire, Choices / Write-in. Every
#' such choice on the site is drawn with this, so they all look and work alike.
#'
#' @param name Ties the buttons together, so each toggle on a page needs its own.
#' @param choices Maps each label to the value it stands for.
#' @param selected The value picked now.
#' @param label Names the group for screen readers.
#' @param attributes Put on every button, e.g. the review page's `data-field`.
#' @param small Whether to draw small buttons.
#' @return A UI element.
toggle_buttons <- function(name, choices, selected, label, attributes = list(),
                           small = FALSE) {
  buttons <- Map(function(choice_label, value, button_id) {
    tagList(
      tags$input(
        type = "radio", class = "btn-check", name = name, id = button_id, value = value,
        autocomplete = "off", checked = if (identical(value, selected)) NA, !!!attributes
      ),
      tags$label(class = c("btn", "btn-outline-dark", if (small) "btn-sm"), `for` = button_id, choice_label)
    )
  }, names(choices), unname(choices), paste(name, seq_along(choices), sep = "-"))
  div(class = "btn-group toggle-buttons", role = "group", `aria-label` = label, unname(buttons))
}

#' A toggle that is a Shiny input.
#'
#' `input[[input_id]]` is the picked value, and `updateRadioButtons()` picks another.
#'
#' @param input_id The input's id.
#' @inheritParams toggle_buttons
#' @return A UI element.
toggle_input <- function(input_id, choices, selected, label) {
  toggle_buttons(input_id, choices, selected, label) |>
    tagAppendAttributes(id = input_id, class = "shiny-input-radiogroup")
}

# ------ Telling staff, and asking them -------------------------------------------------
#' Show what went wrong, in the words the service gave.
#'
#' @param problem A condition, from `tryCatch()`.
#' @param doing What was being tried, e.g. `"Couldn't save"`; it comes first, with a colon.
#' @param type The notification's type: `"error"`, `"warning"` or `"message"`.
#' @return Called for its effect.
show_problem <- function(problem, doing = NULL, type = "error") {
  text <- conditionMessage(problem)
  showNotification(if (is.null(doing)) text else paste0(doing, ": ", text), type = type)
}

#' A dialog that asks staff to confirm.
#'
#' @param title The dialog's title.
#' @param ... What goes in the dialog.
#' @param confirm_id The confirm button's input id.
#' @param confirm_label The confirm button's label.
#' @param keep_label The label of the button that just closes the dialog.
#' @param easy_close Whether clicking outside closes it.
#' @return A `modalDialog()`, for `showModal()`.
confirm_dialog <- function(title, ..., confirm_id, confirm_label, keep_label = "Keep it", easy_close = FALSE) {
  modalDialog(
    title = title, ...,
    footer = tagList(modalButton(keep_label), actionButton(confirm_id, confirm_label, class = "btn-primary")),
    easyClose = easy_close
  )
}
