# ------ Page building blocks ----------------------------------------------------
# Pieces every page uses: its title, toggles, and the dialogs and notices that report what happened.

# A page script from `www/`, at an address that changes whenever the file does.
# Browsers keep scripts, so after a new version of the site goes up they could go on running the old
# one against the new server. The file's size and modification time in the address make them fetch it
# again once it has changed.
# path: The script's path under `www/`, e.g. `"js/review.js"`.
# Returns: A `<script>` tag.
page_script <- function(path) {
  info <- file.info(file.path("www", path))
  version <- if (is.na(info$size)) "" else paste0("?v=", info$size, "-", as.integer(info$mtime))
  tags$script(src = paste0(path, version))
}

# A page's title and the sentence under it.
# title: The title.
# subtitle: The sentence under it.
# ...: Anything else (buttons, a toggle), shown on the right, or under it on a phone.
# eyebrow: A few words over the title, in a gold pill (`.eyebrow`), or `NULL`.
# icon_name: The eyebrow's icon, by its name in `app_icons`.
page_header <- function(title, subtitle, ..., eyebrow = NULL, icon_name = NULL) {
  div(
    class = "page-header",
    div(
      class = "page-header-text",
      if (!is.null(eyebrow)) span(class = "eyebrow", if (!is.null(icon_name)) app_icon(icon_name), eyebrow),
      h1(title),
      p(class = "subhead", subtitle)
    ),
    div(class = "page-header-aside", ...)
  )
}

# ------ Details, beside an outline (Edit Extraction, Make an edit) -------------------------------------------
# A heading over some of the details' fields, laid out in a grid.
detail_section <- function(heading, ...) {
  div(class = "detail-section", if (!is.null(heading)) h3(heading), div(class = "detail-grid", ...))
}

# One field of the details, with its label above it; `wide` takes the grid's whole width.
detail_field <- function(label, ..., wide = FALSE) {
  div(class = c("detail-field", if (wide) "is-wide"), span(class = "detail-label", label), ...)
}

# A coloured circle that shows how far something has got.
# state: `"done"`, `"in_progress"`, `"not_started"` or `"attention"`.
progress_dot <- function(state) {
  span(class = paste0("progress-dot is-", state), `aria-hidden` = "true")
}

# ------ Toggles -------------------------------------------------------------------
# Side-by-side buttons that pick one of a few values, the picked one filled in.
# For example Mine / Everyone's, New / Existing questionnaire, Choices / Write-in. Every
# such choice on the site is drawn with this, so they all look and work alike.
# name: Ties the buttons together, so each toggle on a page needs its own.
# choices: Maps each label to the value it stands for.
# selected: The value picked now.
# label: Names the group for screen readers.
# attributes: Put on every button, e.g. the Edit Extraction page's `data-field`.
# small: Whether to draw small buttons.
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

# A toggle that is a Shiny input.
# `input[[input_id]]` is the picked value, and `updateRadioButtons()` picks another.
# input_id: The input's id.
toggle_input <- function(input_id, choices, selected, label) {
  toggle_buttons(input_id, choices, selected, label) |>
    tagAppendAttributes(id = input_id, class = "shiny-input-radiogroup")
}

# ------ Telling staff, and asking them -------------------------------------------------
# Show what went wrong, in the words the service gave.
# problem: A condition, from `tryCatch()`.
# doing: What was being tried, e.g. `"Couldn't save"`; it comes first, with a colon.
# type: The notification's type: `"error"`, `"warning"` or `"message"`.
show_problem <- function(problem, doing = NULL, type = "error") {
  text <- conditionMessage(problem)
  showNotification(if (is.null(doing)) text else paste0(doing, ": ", text), type = type)
}

# Do something, and if it fails, show why (`show_problem()`) instead.
# expr: What to do; it runs here.
# doing,type: As `show_problem()`.
# Returns: What `expr` gave, or `NULL` if it failed.
try_showing <- function(expr, doing = NULL, type = "error") {
  tryCatch(expr, error = function(problem) {
    show_problem(problem, doing, type)
    NULL
  })
}

# A read that keeps what it last read: when the database can't be reached, that stays on screen with a
# message (one at a time: they share an id), and the next read tries again.
# read: A function of no arguments that reads.
# Returns: A function of no arguments: what `read` gives, or what it last gave when it fails (nothing at first).
last_good_read <- function(read) {
  last <- list()
  function() {
    tryCatch(last <<- read(), error = function(problem) {
      showNotification(conditionMessage(problem), type = "error", id = "database_problem")
      last
    })
  }
}

# A change to a draft, as a service function makes it, with what it says shown to staff.
# Service functions explain themselves: their message()s are shown as notes, and when one stop()s, why is
# shown and nothing changes.
# new_draft: The service function's call, e.g. `draft_service_add_choice(draft, "v1")`; it runs here.
# notes: Whether to show its notes. They are written for Edit Extraction ("Deleted. Undo brings it
#   back."), so a page without Undo leaves them out.
# Returns: The changed draft, or `NULL` if it was refused.
try_change <- function(new_draft, notes = TRUE) {
  tryCatch(
    withCallingHandlers(new_draft, message = function(note) {
      if (notes) showNotification(conditionMessage(note), type = "message")
      invokeRestart("muffleMessage")
    }),
    error = function(problem) {
      show_problem(problem, type = "warning")
      NULL
    }
  )
}

# The top of the details panel: the button back to the list, then what is given (a kind's badge, a title).
# ...: What follows the button.
detail_head <- function(...) {
  div(
    class = "detail-head",
    tags$button(type = "button", class = "btn btn-sm btn-link detail-back", `data-detail-back` = NA, app_icon("back"), " List"),
    ...
  )
}

# A dialog that asks staff to confirm.
# title: The dialog's title.
# ...: What goes in the dialog.
# confirm_id: The confirm button's input id.
# confirm_label: The confirm button's label.
# keep_label: The label of the button that just closes the dialog.
# easy_close: Whether clicking outside closes it.
confirm_dialog <- function(title, ..., confirm_id, confirm_label, keep_label = "Keep it", easy_close = FALSE) {
  modalDialog(
    title = title, ...,
    footer = tagList(modalButton(keep_label), actionButton(confirm_id, confirm_label, class = "btn-primary")),
    easyClose = easy_close
  )
}
