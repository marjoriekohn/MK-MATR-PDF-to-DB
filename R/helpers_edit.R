# ------ Make an edit page building blocks ----------------------------------------------------
# What the page's details draw with: what staff should know, a form's fields, what
# changed and what a change reaches (the list is 40_make_edit_ui.R's). Buttons use the
# page script's data-action (see helpers_review.R).

# A box with something staff should know.
# kind: `"info"`, `"warning"` or `"danger"`.
# ...: What it says.
# title: A bold line above it.
# icon_name: An icon, by its name in `app_icons`; by default one that suits `kind`.
edit_callout <- function(kind, ..., title = NULL, icon_name = NULL) {
  icon_name <- icon_name %||% switch(kind,
    danger = "problem",
    warning = "warning",
    "info"
  )
  div(
    class = paste0("edit-callout is-", kind), role = if (kind != "info") "alert",
    span(class = "edit-callout-icon", `aria-hidden` = "true", app_icon(icon_name)),
    div(class = "edit-callout-body", if (!is.null(title)) strong(class = "edit-callout-title", title), ...)
  )
}

# ------ The info block ----------------------------------------------------------------------------
# Everything staff should know about the edit they are making, in one place under the sentence: what the verb
# involves for the kind of thing. Nothing else on the page says it again. `NULL` when there is nothing to say (a
# plain add, applied at once).
# verb: `"add"`, `"update"`, `"delete"`, or `""` while it isn't picked yet.
edit_info_block <- function(entity, verb) {
  doing <- c(add = "Adding", update = "Changing", delete = "Deleting")
  how <- switch(verb,
    update = c(entity$update_about, "Pick the one to change, then change it. You'll see everywhere it's used before you save."),
    delete = c(entity$delete_about, "You'll see everything that goes with it, and confirm, before anything is deleted."),
    NULL
  )
  if (length(how) == 0) {
    return(NULL)
  }
  edit_callout(
    if (identical(verb, "delete")) "danger" else "info",
    title = paste(doing[[verb]], entity$article, entity$noun),
    lapply(how, p)
  )
}

# ------ Fields -----------------------------------------------------------------------------------
# One field of a form.
# Like every form on the site it has no label above it: what it holds is said inside it (a text box's
# placeholder, a drop-down's empty choice) and, for screen readers and when pointed at, in its
# `aria-label` and `title`. A required field that is blank is outlined in red (the note under it marks
# it; see `edit_change_server()`). A field that is locked shows its value, greyed, with the reason.
# Fields that only matter while another has a value are wrapped so the page hides them.
# field: One of the form's fields, from `edit_service_fields()`.
# value: What it starts with.
# slot: The output that says what is wrong with it.
edit_field_ui <- function(ns, field, value, slot) {
  id <- ns(field$id)
  inside <- edit_field_placeholder(field)
  named <- function(tag, element) {
    tag_input_attributes(tag, element, `aria-label` = field$label, title = field$label, `aria-describedby` = slot)
  }
  control <- if (!is.null(field$locked)) {
    div(
      class = "locked-field",
      div(
        class = "locked-value", `aria-label` = field$label, title = field$label,
        edit_display_value(field, edit_service_normalize(field, value))
      ),
      div(class = "form-text locked-reason", app_icon("locked"), " ", field$locked)
    )
  } else {
    switch(field$type,
      textarea = named(
        textAreaInput(id, NULL, value, rows = field$rows %||% 3, width = "100%", placeholder = inside),
        "textarea"
      ),
      select = named(
        selectInput(id, NULL, edit_select_choices(field), selected = value, width = "100%", selectize = length(field$choices) > 30),
        "select"
      ),
      several = named(
        selectizeInput(
          id, NULL, field$choices,
          selected = edit_several_ids(value), multiple = TRUE, width = "100%",
          options = list(placeholder = inside, plugins = list("remove_button"))
        ),
        "select"
      ),
      choice = div(
        role = "group", `aria-label` = field$label, title = field$label,
        toggle_input(id, field$choices, selected = value, label = field$label)
      ),
      variables = variable_editor_ui(ns("variables"), field$label), # its server is edit_change_server()'s
      {
        box <- named(textInput(id, NULL, value, placeholder = inside, width = "100%"), "input")
        if (field$type == "number") box <- tag_input_attributes(box, "input", inputmode = "numeric")
        box
      }
    )
  }
  help <- c(field$hint, field$tooltip)
  field_div <- div(
    class = c("form-field", "edit-field", if (field$type %in% c("textarea", "variables", "several")) "is-wide"),
    control,
    if (length(help) > 0 && is.null(field$locked)) div(class = "form-text field-help", paste(help, collapse = " ")),
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

# A drop-down's choices, its empty choice naming the field ("Select lead investigator"), so two
# drop-downs of the same rows can be told apart without labels.
# field: One of the form's fields, from `edit_service_fields()`, of type `"select"`.
# Returns: The choices, as `selectInput()` takes them.
edit_select_choices <- function(field) {
  choices <- field$choices
  blank <- which(unname(choices) == "")
  if (length(blank) == 0 || is.null(names(choices))) {
    return(choices)
  }
  names(choices)[blank[1]] <- paste0("Select ", field$short, if (!isTRUE(field$required)) " (optional)")
  choices
}

# What a text box says while it is empty: its name, an example or format if it has one, and whether
# it can be left empty.
# field: One of the form's fields, from `edit_service_fields()`.
# Returns: A string, e.g. `"Name (e.g. Adult Health Study)"` or `"Description (optional)"`.
edit_field_placeholder <- function(field) {
  notes <- c(
    if (!isTRUE(field$required)) "optional",
    if (has_text(field$placeholder)) trimws(field$placeholder)
  )
  if (length(notes) == 0) {
    return(field$label)
  }
  paste0(field$label, " (", paste(notes, collapse = ", "), ")")
}

# ------ What changed -------------------------------------------------------------------------------------
# The fields that differ, old and new.
# Words are marked where they differ, the way a tracked change reads: taken-out words
# struck through, put-in words highlighted.
# changes: The result of `edit_service_changes()`.
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
# What changing a row reaches.
# For a component: how many questionnaires print it, and a card for each (see `edit_use_card()`). For
# anything else: the rows that point at it,
# each kind of row a box saying how many there are, where they are, and every one of them. A few are listed as they
# are; more are folded away, one fold for each questionnaire they are in (the most first), or one for them all.
# The row's own name is the details' title, so it isn't said again here.
# usage: The result of `edit_service_usage()`.
edit_usage_ui <- function(entity, usage) {
  local <- !is.null(entity$scope)
  div(
    class = "edit-usage",
    if (length(usage$uses) > 0) {
      n <- length(usage$uses)
      tagList(
        p(
          class = "edit-usage-count",
          strong(edit_count(n, "questionnaire")),
          if (n == 1) " prints this" else " print this"
        ),
        div(class = "edit-usage-list", lapply(usage$uses, edit_use_card, entity = entity))
      )
    },
    if (entity$table == "component" && length(usage$uses) == 0) {
      p(class = "edit-usage-none", "No questionnaire prints this yet, so changing it affects nothing else.")
    },
    lapply(usage$pointing, edit_pointing_ui),
    if (entity$table != "component" && length(usage$pointing) == 0 && !local) {
      p(
        class = "edit-usage-none",
        if (isTRUE(entity$view_only)) "Nothing else points at this." else "Nothing else points at this yet, so changing it affects nothing else."
      )
    }
  )
}

# How many listed as they are before they're folded away.
edit_usage_shown <- 6

# One kind of row that points at the row: "867 variables", "in 4 questionnaires", then the rows.
edit_pointing_ui <- function(line) {
  groups <- line$groups
  where <- if (length(groups) == 1) {
    paste("all in", groups[[1]]$questionnaire)
  } else if (length(groups) > 1) {
    paste("in", edit_count(length(groups), "questionnaire"))
  }
  rows <- function(labels) tags$ul(class = "edit-use-rows", lapply(labels, tags$li))
  div(
    class = "edit-use",
    div(class = "edit-use-head", strong(line$text), if (!is.null(where)) span(class = "edit-use-where", where)),
    if (line$n <= edit_usage_shown) {
      rows(line$labels)
    } else if (length(groups) > 1) {
      div(class = "edit-use-groups", lapply(groups, function(group) {
        edit_use_fold(group$questionnaire, length(group$labels), rows(group$labels))
      }))
    } else {
      edit_use_fold(paste("Show all", format(line$n, big.mark = ",")), NULL, rows(line$labels))
    }
  )
}

# A fold that opens to the rows: what it holds, and how many.
edit_use_fold <- function(title, n, ...) {
  tags$details(
    class = "edit-use-fold",
    tags$summary(span(class = "edit-use-fold-title", title), if (!is.null(n)) span(class = "edit-use-count", format(n, big.mark = ","))),
    ...
  )
}

# A questionnaire that prints a component, as its card in the details shows it.
# Its name, with a button that opens the block as it is in that questionnaire alone; where it sits; for a question
# each wave that asked it, with the number it had, and each variable, with what it records, who it is about, and its
# choices.
# use: One of `edit_service_uses()`.
# entity: The kind of thing: a component.
edit_use_card <- function(use, entity) {
  div(
    class = "edit-use",
    div(
      class = "edit-use-head",
      strong(class = "edit-use-name", use$questionnaire),
      review_button(
        "Open in this questionnaire", "jump",
        `data-kind` = "placement", `data-verb` = "view", `data-scope` = key_text(use$questionnaire_id),
        `data-key` = key_text(use$placement_id),
        class = "btn btn-sm btn-outline-dark"
      )
    ),
    p(class = "edit-use-where", edit_use_where(use)),
    if (length(use$waves) > 0) {
      tags$ul(class = "edit-use-asked", Map(function(wave, number) {
        tags$li(span(class = "edit-use-wave", wave), paste0(" · question ", number))
      }, use$waves, unname(use$numbers)))
    } else if (identical(use$kind, "Question")) {
      p(class = "edit-use-detail", "Not asked in any wave yet.")
    },
    if (length(use$boxes) > 0) {
      div(class = "edit-use-boxes", lapply(use$boxes, function(box) {
        div(
          class = "edit-use-box",
          div(span(class = "edit-use-matr", box$matr_id), if (nzchar(box$tags)) span(class = "edit-use-tags", box$tags)),
          if (length(box$coding) > 0) tags$ul(class = "edit-use-coding", lapply(box$coding, tags$li))
        )
      }))
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
# Everything staff should read before something is deleted.
# What it is, what else goes with it, and what stops it.
# title: The row's name.
# plan: The result of `edit_service_delete_plan()`.
edit_delete_ui <- function(entity, title, plan) {
  reach <- plan$reach
  blocked <- length(plan$blocked) > 0
  div(
    class = "edit-delete",
    div(
      class = "delete-banner",
      app_icon("delete"),
      div(
        span(class = "delete-banner-label", paste("You're deleting this", entity$noun)),
        strong(class = "delete-banner-title", title)
      )
    ),
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
      "Read the lists below. All of it is removed together, at once."
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

# The confirmations before a delete can be sent.
# Far-reaching deletes also need the word typed.
# plan: The result of `edit_service_delete_plan()`.
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
      textInput(ns("confirm_word"), NULL, width = "20rem", placeholder = "To be sure, type DELETE") |>
        tag_input_attributes("input", `aria-label` = "To be sure, type DELETE")
    }
  )
}

# ------ Emptying -----------------------------------------------------------------------------------------------
# What Empty asks first: what goes, what stays, and that it happens at once.
# title: The row's name.
# preview: What would be removed, from `edit_service_empty_preview()`.
edit_empty_dialog <- function(ns, entity, title, preview) {
  confirm_dialog(
    paste0("Empty ", title, "?"),
    p(preview),
    p(strong("What stays: "), "the ", entity$noun, " itself. ", entity$empty_about),
    p("Answer choices and tags stay too: they are vocabulary. This happens at once and is written to the Edit History."),
    confirm_id = ns("confirm_empty"), confirm_label = "Empty it", keep_label = "Cancel"
  )
}

# ------ Something like it is there already -----------------------------------------------------------------
# The dialog that says an add looks like something already in the database.
# It shows what is there and lets staff go back or add it anyway: a warning, not a block.
# found: From `edit_service_duplicates()`.
# confirm_id: The input id of the "Add it anyway" button.
edit_duplicates_dialog <- function(found, confirm_id) {
  confirm_dialog(
    "This looks like something that's already there",
    p("If it is the same thing, adding it again would make a copy. Look before you go on."),
    edit_duplicates_list(found),
    confirm_id = confirm_id, confirm_label = "Add it anyway", keep_label = "Go back", easy_close = TRUE
  )
}

# The same warning as a note on the form, for a form that is itself in a dialog.
# A dialog can't open over another, so the pop-ups that add from a drop-down show what is there
# under the form as it is typed, and their button says "Add it anyway".
# found: From `edit_service_duplicates()`.
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
