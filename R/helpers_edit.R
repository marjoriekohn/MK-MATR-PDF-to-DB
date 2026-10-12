# ------ Make an edit page building blocks ----------------------------------------------------
# What the page's modules draw with: the sentence, the steps, a form's fields,
# a row's card, what changed and what a change reaches. Buttons that act on a
# card or a row use the page script's data-action (see helpers_review.R).

#' A box with something staff should know.
#'
#' @param kind `"info"`, `"warning"` or `"danger"`.
#' @param ... What it says.
#' @param title A bold line above it.
#' @param icon_name An icon; by default one that suits `kind`.
#' @return A UI element.
edit_callout <- function(kind, ..., title = NULL, icon_name = NULL) {
  icon_name <- icon_name %||% switch(kind,
    danger = "triangle-exclamation",
    warning = "circle-exclamation",
    "circle-info"
  )
  div(
    class = paste0("edit-callout is-", kind), role = if (kind != "info") "alert",
    span(class = "edit-callout-icon", `aria-hidden` = "true", icon(icon_name)),
    div(class = "edit-callout-body", if (!is.null(title)) strong(class = "edit-callout-title", title), ...)
  )
}

# ------ The sentence ---------------------------------------------------------------------------
#' "I want to [update] a [question]."
#'
#' Both blanks are drop-downs: `verb` and `kind`.
#'
#' @param ns The module's namespace function.
#' @return A UI element.
edit_sentence_ui <- function(ns) {
  verbs <- c("choose what to do…" = "", "add" = "add", "update" = "update", "delete" = "delete")
  kinds <- c(list("choose a kind of thing…" = ""), edit_service_kinds())
  div(
    class = "edit-sentence",
    span("I want to"),
    selectInput(ns("verb"), NULL, verbs, selectize = FALSE, width = "auto") |>
      tag_input_attributes("select", class = "sentence-select", `aria-label` = "What do you want to do?"),
    uiOutput(ns("article"), inline = TRUE),
    selectInput(ns("kind"), NULL, kinds, selectize = FALSE, width = "auto") |>
      tag_input_attributes("select", class = "sentence-select", `aria-label` = "What kind of thing?"),
    span(".")
  )
}

# ------ Steps -----------------------------------------------------------------------------------
#' Where staff are in the steps.
#'
#' Done steps are ticked, the current one is numbered in black.
#'
#' @param labels The steps' names.
#' @param current The number of the current step.
#' @return A UI element.
edit_steps_ui <- function(labels, current) {
  tags$ol(
    class = "edit-steps", `aria-label` = "Steps",
    lapply(seq_along(labels), function(i) {
      state <- if (i < current) "done" else if (i == current) "current" else "next"
      tags$li(
        class = paste0("edit-step is-", state), `aria-current` = if (state == "current") "step",
        span(class = "edit-step-mark", if (state == "done") icon("check") else i),
        span(class = "edit-step-label", labels[i])
      )
    })
  )
}

# ------ Fields -----------------------------------------------------------------------------------
#' One field of a form.
#'
#' A field that is locked is shown, not typed, with the reason. Fields that only matter
#' while another has a value are wrapped so the page hides them.
#'
#' @param ns The module's namespace function.
#' @param field One of the form's fields, from `edit_service_fields()`.
#' @param value What it starts with.
#' @param slot The output that says what is wrong with it.
#' @return A UI element.
edit_field_ui <- function(ns, field, value, slot) {
  id <- ns(field$id)
  label <- field_label(field$label, isTRUE(field$required))
  if (!is.null(field$tooltip)) label <- tagList(label, field_tooltip(field$tooltip, field$label))
  control <- if (!is.null(field$locked)) {
    div(
      class = "locked-field",
      tags$label(class = "form-label", field$label),
      div(class = "locked-value", edit_display_value(field, edit_service_normalize(field, value))),
      div(class = "form-text locked-reason", icon("lock"), " ", field$locked)
    )
  } else {
    switch(field$type,
      textarea = textAreaInput(id, label, value, rows = field$rows %||% 3, width = "100%", placeholder = field$placeholder),
      select = selectInput(
        id, label, field$choices,
        selected = value, width = "100%",
        selectize = length(field$choices) > 30
      ),
      choice = div(
        role = "group", `aria-label` = field$label,
        tags$label(class = "form-label", label),
        toggle_input(id, field$choices, selected = value, label = field$label)
      ),
      {
        box <- textInput(id, label, value, placeholder = field$placeholder, width = "100%")
        if (!is.null(field$suggest)) {
          list_id <- paste0(id, "-list")
          box <- tagList(tag_input_attributes(box, "input", list = list_id), suggestions(list_id, field$suggest))
        }
        if (field$type == "number") box <- tag_input_attributes(box, "input", inputmode = "numeric")
        box
      }
    )
  }
  field_div <- div(
    class = c("form-field", "edit-field", if (field$type == "textarea") "is-wide"),
    control,
    if (!is.null(field$hint) && is.null(field$locked)) div(class = "form-text field-help", field$hint),
    uiOutput(slot, class = "form-text field-note")
  )
  if (is.null(field$show_when)) {
    return(field_div)
  }
  conditionalPanel(
    sprintf("input['%s__%s'] == '%s'", field$part, field$show_when$field, field$show_when$value),
    ns = ns, field_div
  )
}

#' A heading above the fields of one part of a form that is several rows.
#'
#' @param title The heading.
#' @param why A sentence under it, or `NULL`.
#' @return A UI element.
edit_part_heading <- function(title, why = NULL) {
  div(class = "edit-part-heading", h3(title), if (!is.null(why)) p(class = "form-text", why))
}

# ------ Cards -----------------------------------------------------------------------------------------
#' One row, with what it is and how much depends on it.
#'
#' @param card One row of the cards data frame from `edit_cards()`.
#' @param verb What the button does: `"update"` or `"delete"`.
#' @param selected Whether it is the picked card.
#' @return A UI element.
edit_card_ui <- function(card, verb, selected = FALSE) {
  danger <- verb == "delete"
  div(
    class = c("edit-card", if (selected) "is-selected", if (danger) "is-danger"),
    div(
      class = "edit-card-head",
      if (!is.na(card$tag)) type_chip(card$tag),
      h3(class = "edit-card-title", card$title)
    ),
    if (nzchar(card$subtitle)) p(class = "edit-card-subtitle", card$subtitle),
    if (length(card$facts[[1]]) > 0) {
      div(class = "edit-card-facts", lapply(card$facts[[1]], function(fact) row_badge(fact)))
    },
    if (!is.na(card$warn)) p(class = "edit-card-warn", icon("circle-exclamation"), " ", card$warn),
    if (!selected) {
      review_button(
        paste(tools::toTitleCase(verb), "this"), "pick",
        `data-id` = card$key,
        class = c("btn btn-sm", if (danger) "btn-delete" else "btn-outline-dark")
      )
    }
  )
}

# ------ What changed -------------------------------------------------------------------------------------
#' The fields that differ, old and new.
#'
#' Words are marked where they differ, the way a tracked change reads: taken-out words
#' struck through, put-in words highlighted.
#'
#' @param changes The result of `edit_service_changes()`.
#' @return A UI element.
edit_changes_ui <- function(changes) {
  if (nrow(changes) == 0) {
    return(div(
      class = "edit-changes is-empty",
      span(class = "edit-changes-title", "What you changed"),
      p("Nothing yet. Change a field above.")
    ))
  }
  div(
    class = "edit-changes",
    span(class = "edit-changes-title", "What you changed"),
    lapply(seq_len(nrow(changes)), function(i) {
      div(
        class = "edit-change",
        span(class = "edit-change-label", changes$label[i]),
        if (changes$type[i] %in% c("text", "textarea")) {
          diff <- edit_service_word_diff(
            if (changes$old[i] == "(empty)") "" else changes$old[i], if (changes$new[i] == "(empty)") "" else changes$new[i]
          )
          p(class = "edit-diff", lapply(seq_len(nrow(diff)), function(j) {
            switch(diff$kind[j],
              del = tags$del(diff$text[j]),
              ins = tags$ins(diff$text[j]),
              diff$text[j]
            )
          }))
        } else {
          p(class = "edit-diff", tags$del(changes$old[i]), span(class = "edit-arrow", " → "), tags$ins(changes$new[i]))
        }
      )
    })
  )
}

# ------ Where this appears ---------------------------------------------------------------------------------
#' What changing a row reaches.
#'
#' For a component or question: each questionnaire that prints it. For anything else:
#' the rows that point at it.
#'
#' @param entity The kind of thing; see `edit_entities`.
#' @param title The row's name.
#' @param usage The result of `edit_service_usage()`.
#' @return A UI element.
edit_usage_ui <- function(entity, title, usage) {
  local <- !is.null(entity$scope)
  div(
    class = "edit-usage",
    h2("Where this appears"),
    if (local) {
      p(class = "form-text", "This only changes the one questionnaire you picked.")
    } else if (length(usage$uses) > 0 || length(usage$pointing) > 0) {
      p(class = "form-text", "Everything below changes if you make this edit.")
    },
    if (entity$table == "component") div(class = "edit-usage-title", paste0("“", title, "”")),
    if (length(usage$uses) > 0) {
      div(class = "edit-usage-list", lapply(usage$uses, function(use) {
        div(
          class = "edit-use",
          strong(use$questionnaire),
          p(class = "edit-use-where", edit_use_where(use)),
          if (length(use$waves) > 0) p(class = "edit-use-detail", paste("Asked in", paste(use$waves, collapse = ", "))),
          if (use$variables > 0) p(class = "edit-use-detail", edit_count(use$variables, "variable", "variables"))
        )
      }))
    },
    if (entity$table == "component" && length(usage$uses) == 0) {
      p(class = "edit-usage-none", "No questionnaire prints this yet, so changing it affects nothing else.")
    },
    lapply(usage$pointing, function(line) {
      div(
        class = "edit-use",
        strong(line$text),
        p(class = "edit-use-detail", paste(line$examples, collapse = " · "), if (line$more > 0) paste0(" and ", line$more, " more"))
      )
    }),
    if (entity$table != "component" && length(usage$pointing) == 0 && !local) {
      p(class = "edit-usage-none", "Nothing else points at this yet, so changing it affects nothing else.")
    }
  )
}

# "In section C. Energy · position 3" or "At the top level · position 1".
edit_use_where <- function(use) {
  paste0(
    if (is.na(use$section)) "At the top level" else paste("In", use$section),
    " · position ", use$position
  )
}

# ------ Deleting ------------------------------------------------------------------------------------------------
#' Everything staff should read before something is deleted.
#'
#' What it is, what else goes with it, and what stops it.
#'
#' @param entity The kind of thing; see `edit_entities`.
#' @param title The row's name.
#' @param plan The result of `edit_service_delete_plan()`.
#' @return A UI element.
edit_delete_ui <- function(entity, title, plan) {
  reach <- plan$reach
  blocked <- length(plan$blocked) > 0
  div(
    class = "edit-delete",
    div(
      class = "delete-banner",
      icon("trash-can"),
      div(
        span(class = "delete-banner-label", paste("You're proposing to delete this", entity$noun)),
        strong(class = "delete-banner-title", title)
      )
    ),
    p(class = "delete-about", entity$delete_about),
    if (blocked) edit_blocked_ui(plan$blocked),
    if (!blocked) edit_reach_ui(entity, reach, plan)
  )
}

edit_blocked_ui <- function(blocked) {
  edit_callout(
    "danger",
    title = "This can't be deleted yet",
    p("Something is in the way. Sort it out first, then come back to this."),
    tags$ul(class = "blocked-list", lapply(blocked, function(blocker) {
      tags$li(
        p(blocker$text),
        if (!is.null(blocker$fix)) p(class = "blocked-fix", strong("What to do: "), blocker$fix),
        if (length(blocker$labels) > 1 || (length(blocker$labels) == 1 && !grepl(blocker$labels[1], blocker$text, fixed = TRUE))) {
          tags$ul(
            class = "blocked-rows",
            lapply(utils::head(blocker$labels, 8), tags$li),
            if (length(blocker$labels) > 8) tags$li(paste("and", length(blocker$labels) - 8, "more"))
          )
        }
      )
    }))
  )
}

# What goes with the row: counts by kind, with a few examples, and how it falls
# on each questionnaire.
edit_reach_ui <- function(entity, reach, plan) {
  if (!plan$far_reaching) {
    return(edit_callout(
      "info",
      title = "Nothing else depends on this",
      "Only this one record is deleted. No questionnaire, wave or variable is affected."
    ))
  }
  tagList(
    edit_callout(
      "danger",
      title = if (reach$total > 0) {
        paste(
          edit_count(reach$total, "other record"),
          "will be deleted with it"
        )
      } else {
        "Other records will change"
      },
      "Read the lists below. When this is applied, all of it is removed together."
    ),
    if (length(reach$lines) > 0) {
      div(class = "reach-section", h3("Deleted along with it"), tags$ul(class = "reach-list", lapply(reach$lines, function(line) {
        tags$li(
          strong(line$text),
          if (length(line$examples) > 0) {
            tags$ul(
              class = "reach-examples",
              lapply(line$examples, tags$li),
              if (line$more > 0) tags$li(class = "reach-more", paste0("and ", format(line$more, big.mark = ","), " more"))
            )
          }
        )
      })))
    },
    if (length(reach$changed) > 0) {
      div(class = "reach-section", h3("Kept, but changed"), tags$ul(class = "reach-list", lapply(reach$changed, function(change) {
        tags$li(strong(change$text), p(class = "form-text", paste(change$examples, collapse = " · ")))
      })))
    },
    if (length(reach$by_questionnaire) > 0) {
      div(
        class = "reach-section",
        h3("How it falls on each questionnaire"),
        tags$ul(class = "reach-list", lapply(reach$by_questionnaire, function(q) {
          tags$li(strong(q$questionnaire), p(class = "form-text", paste(q$lines, collapse = " · ")))
        }))
      )
    }
  )
}

#' The confirmations before a delete can be sent.
#'
#' Far-reaching deletes also need the word typed.
#'
#' @param ns The module's namespace function.
#' @param plan The result of `edit_service_delete_plan()`.
#' @return A UI element.
edit_delete_confirm_ui <- function(ns, plan) {
  div(
    class = "delete-confirm",
    checkboxInput(
      ns("confirm_read"),
      if (plan$far_reaching) {
        "I've read the lists above. I understand all of it will be deleted."
      } else {
        "I understand this will be deleted."
      },
      value = FALSE, width = "100%"
    ),
    if (plan$far_reaching) {
      textInput(ns("confirm_word"), "To be sure, type DELETE", width = "20rem")
    }
  )
}

#' A small info mark that explains a field when it is hovered or focused.
#'
#' @param text What it says.
#' @param label The field's label, for screen readers.
#' @return A UI element.
field_tooltip <- function(text, label) {
  bslib::tooltip(
    span(
      class = "field-tooltip", tabindex = "0", role = "img", `aria-label` = paste("About", tolower(label)),
      icon("circle-info")
    ),
    text
  )
}

# ------ Something like it is there already -----------------------------------------------------------------
#' The dialog that says an add looks like something already in the database.
#'
#' It shows what is there and lets staff go back or add it anyway: a warning, not a block.
#'
#' @param found From `edit_service_duplicates()`.
#' @param confirm_id The input id of the "Add it anyway" button.
#' @return A `modalDialog()`, for `showModal()`.
edit_duplicates_dialog <- function(found, confirm_id) {
  modalDialog(
    title = "This looks like something that's already there",
    p("If it is the same thing, adding it again would make a copy. Look before you go on."),
    edit_duplicates_list(found),
    footer = tagList(modalButton("Go back"), actionButton(confirm_id, "Add it anyway", class = "btn-primary")),
    easyClose = TRUE
  )
}

#' The same warning as a note on the form, for a form that is itself in a dialog.
#'
#' A dialog can't open over another, so the pop-ups that add from a drop-down show what is there
#' under the form as it is typed, and their button says "Add it anyway".
#'
#' @param found From `edit_service_duplicates()`.
#' @return A UI element.
edit_duplicates_inline <- function(found) {
  edit_callout(
    "warning",
    title = "This looks like something that's already there",
    p("If it is the same thing, adding it again would make a copy. You can still add it."),
    edit_duplicates_list(found)
  )
}

# What is there, for each way the new row looks like it.
edit_duplicates_list <- function(found) {
  lapply(found, function(match) {
    div(
      class = "edit-duplicate",
      p(strong(paste0(match$why, ":"))),
      lapply(seq_len(nrow(match$cards)), function(i) edit_duplicate_card(match$cards[i, ]))
    )
  })
}

# The row that is there, the way a card shows it, without a button.
edit_duplicate_card <- function(card) {
  div(
    class = "edit-card is-duplicate",
    div(class = "edit-card-head", h3(class = "edit-card-title", card$title)),
    if (nzchar(card$subtitle)) p(class = "edit-card-subtitle", card$subtitle),
    if (length(card$facts[[1]]) > 0) div(class = "edit-card-facts", lapply(card$facts[[1]], function(fact) row_badge(fact)))
  )
}
