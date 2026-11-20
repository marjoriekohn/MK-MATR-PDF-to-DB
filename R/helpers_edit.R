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
# involves for the kind of thing, and that nothing happens until they say so. Nothing else on the page says it
# again. `NULL` when there is nothing to say (a plain add, applied at once).
# verb: `"add"`, `"update"`, `"delete"`, `"merge"`, or `""` while it isn't picked yet.
# row: The row, for a kind whose rows are called by what they are (a wording is a question, a section...).
edit_info_block <- function(entity, verb, row = NULL) {
  doing <- c(add = "Adding", update = "Changing", delete = "Deleting", merge = "Merging")
  how <- switch(verb,
    update = c(entity$update_about, edit_safe_to_try),
    delete = c(entity$delete_about, "You'll see everything that goes with it, and confirm, before anything is deleted."),
    merge = c(
      "Use this when two of them are the same thing kept twice. Everything that uses this one uses the other instead, and this one is removed.",
      "Nothing happens until you press Merge them. You'll see what moves first."
    ),
    NULL
  )
  if (length(how) == 0) {
    return(NULL)
  }
  edit_callout(
    if (identical(verb, "delete")) "danger" else "info",
    title = paste(doing[[verb]], edit_a(edit_noun(entity, row))),
    lapply(how, p)
  )
}

# What a form says before it is filled in: where to look for what it does.
edit_safe_to_try <- "Under the form you'll see what you changed and everywhere it is used. Every change is kept in the History."

# A noun with its article: "a study", "an answer choice", "an instruction".
edit_a <- function(noun) paste(if (grepl("^[aeiou]", tolower(noun))) "an" else "a", noun)

# ------ Fields -----------------------------------------------------------------------------------
# One field of a form: its name on the left and the field beside it, as the details show the same line, so
# pressing Edit changes what is on the right and nothing moves. What the field is for, or why it can't be changed,
# is in a tooltip beside its name (`edit_field_label()`), not in small print under the box. A required field that
# is blank is outlined in red (the note under it marks it; see `edit_change_server()`). A field that is locked
# shows its value, greyed. Fields that only matter while another has a value are wrapped so the page hides them.
# field: One of the form's fields, from `edit_service_fields()`.
# value: What it starts with.
# slot: The output that says what is wrong with it.
edit_field_ui <- function(ns, field, value, slot) {
  id <- ns(field$id)
  inside <- edit_field_placeholder(field)
  named <- function(tag, element) {
    tag_input_attributes(tag, element, `aria-label` = field$label, `aria-describedby` = slot)
  }
  control <- if (!is.null(field$locked)) {
    div(class = "locked-value", `aria-label` = field$label, edit_display_value(field, edit_service_normalize(field, value)))
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
        role = "group", `aria-label` = field$label,
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
  titled <- field$type == "variables" # the variable editor has its name over it, and takes the form's whole width
  field_div <- div(
    class = c("form-field", "edit-field", if (titled) "is-wide"),
    if (!titled) edit_field_label(field, id),
    div(class = "edit-field-control", control, uiOutput(slot, class = "form-text field-note"))
  )
  if (is.null(field$show_when)) {
    return(field_div)
  }
  conditionalPanel(
    sprintf("input['%s__%s'] == '%s'", field$part, field$show_when$field, field$show_when$value),
    ns = ns, class = "edit-field-when", field_div
  )
}

# A field's name beside it, which stays while the form is open. After it: "optional" for a field that can be left
# empty, and an icon whose tooltip says what the field is for (its hint), or, for one that is locked, why it can't
# be changed.
# id: The input's id, so clicking the name puts the cursor in the field.
edit_field_label <- function(field, id) {
  help <- paste(c(field$hint, field$tooltip), collapse = " ")
  locked <- !is.null(field$locked)
  div(
    class = "edit-field-label",
    tags$label(`for` = if (!locked) id, field$label),
    if (!locked && !isTRUE(field$required)) span(class = "edit-field-optional", "optional"),
    if (locked) help_tip(field$label, field$locked, "locked") else if (nzchar(help)) help_tip(field$label, help)
  )
}

# A drop-down's choices, its empty choice naming the field ("Select investigator"), so two
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

# What a text box says while it is empty: an example or the format, for a field that has one. Its name and whether
# it can be left empty are beside it (`edit_field_label()`).
# field: One of the form's fields, from `edit_service_fields()`.
# Returns: A string, e.g. `"e.g. Adult Health Study"`, or `NULL` for none.
edit_field_placeholder <- function(field) {
  if (has_text(field$placeholder)) trimws(field$placeholder)
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
# For a wording (a question, a heading): a card for each questionnaire that prints it (see `edit_use_card()`). For
# anything else: the rows that point at it, each kind of row a drop-down that says how many there are and opens to
# every one of them (see `edit_pointing_ui()`). The row's own name is the details' title, so it isn't said again.
# usage: The result of `edit_service_usage()`.
# open: Whether the drop-downs start open. Under a row's details they do. Under a form, where they are what the
#   change reaches, they open when they are clicked.
edit_usage_ui <- function(entity, usage, open = TRUE) {
  local <- !is.null(entity$scope)
  printed_in <- vapply(usage$uses, function(use) key_text(use$questionnaire_id), character(1))
  div(
    class = "edit-usage",
    if (length(usage$uses) > 0) {
      div(class = "edit-usage-list", lapply(unname(split(usage$uses, factor(printed_in, unique(printed_in)))), edit_use_card))
    },
    if (entity$table == "component" && length(usage$uses) == 0) {
      p(class = "edit-usage-none", "No questionnaire prints this yet, so changing it affects nothing else.")
    },
    lapply(usage$pointing, edit_pointing_ui, open = open),
    if (entity$table != "component" && length(usage$pointing) == 0 && !local) {
      p(
        class = "edit-usage-none",
        if (isTRUE(entity$view_only)) "Nothing else uses this." else "Nothing else uses this yet, so changing it affects nothing else."
      )
    }
  )
}

# How many rows a drop-down lists before Show all.
edit_usage_shown <- 8

# The kinds of row drawn as a grid of small cards, not lines: there are few of each, and each is a thing with
# details of its own. The card is the way to it: the whole card is pressed, and it lifts under the pointer as a
# card on the home page does.
edit_usage_as_cards <- c("study", "questionnaire", "wave")

# View on a listed row or card: "View Study" and the open folder, the same everywhere. It opens that thing in the
# details.
# what: What it opens, as the button says it ("Study", "Question").
# name: The thing's name, for screen readers.
# opens: One row of `edit_opens()`: its `kind`, `key` and `scope`.
edit_view_button <- function(what, name, opens) {
  review_button(
    tagList(paste("View", what), " ", app_icon("view")), "jump",
    class = "btn btn-sm btn-link edit-use-view", `aria-label` = paste0("View ", what, ": ", name),
    `data-kind` = opens$kind, `data-verb` = "view", `data-key` = opens$key, `data-scope` = opens$scope
  )
}

# One kind of row that points at the row, as a drop-down: "7 studies" opens to the rows. Each row is its name, a few
# quiet words about it (a variable's Variable ID and code), tags (the questionnaires that ask it), and View, which
# opens it (and Remove, which takes the row itself away: a question out of a scale). A long list shows its first
# rows, then Show all; rows in more than one questionnaire are folded by questionnaire, the most first.
# Studies, questionnaires and waves are a grid of small cards, each pressed to open it. Under a row's details the
# grid stands alone, with no count over it and nothing to open: the cards are what the row holds. Only when the
# line says what they are to the row, not just how many (`named`: the studies an investigator leads, and the ones
# they are the contact for), is that said over them. Under a form the grid is in its drop-down like any other line.
# line: One of `edit_service_usage()`'s `pointing`.
# open: Whether it starts open.
edit_pointing_ui <- function(line, open = TRUE) {
  groups <- line$groups
  where <- if (length(groups) == 1) {
    paste("all in", groups[[1]]$questionnaire)
  } else if (length(groups) > 1) {
    paste("in", edit_count(length(groups), "questionnaire"))
  }
  view <- function(i) if (!is.null(line$open)) edit_view_button(line$open$what[i], line$labels[i], line$open[i, ])
  row <- function(i) {
    tags$li(
      class = "edit-use-row",
      span(
        class = "edit-use-row-text", line$labels[i],
        if (has_text(line$meta[i])) span(class = "edit-use-meta", line$meta[i]),
        lapply(line$pills[i][[1]], row_badge)
      ),
      view(i),
      if (!is.null(line$remove)) {
        review_button(
          "Remove", "jump",
          class = "btn btn-sm btn-link edit-use-remove", `aria-label` = paste("Remove", line$labels[i]),
          `data-kind` = line$remove$kind[i], `data-verb` = "delete", `data-key` = line$remove$key[i], `data-scope` = line$remove$scope[i]
        )
      }
    )
  }
  rows <- function(index) tags$ul(class = "edit-use-rows", lapply(index, row))
  as_cards <- line$table %in% edit_usage_as_cards && !is.null(line$open) && is.null(line$remove) && line$n > 0
  cards <- function() {
    div(class = "edit-use-cards", lapply(seq_len(line$n), function(i) {
      review_button(
        span(class = "edit-use-card-name", line$labels[i]), "jump",
        class = "edit-use-card", `aria-label` = paste0("View ", line$open$what[i], ": ", line$labels[i]),
        title = paste("Open this", tolower(line$open$what[i])),
        `data-kind` = line$open$kind[i], `data-verb` = "view", `data-key` = line$open$key[i], `data-scope` = line$open$scope[i]
      )
    }))
  }
  if (as_cards && open) {
    return(div(class = "edit-use-grid", if (isTRUE(line$named)) p(class = "edit-use-caption", line$text), cards()))
  }
  tags$details(
    class = "edit-use", open = if (open) NA,
    tags$summary(class = "edit-use-head", strong(line$text), if (!is.null(where)) span(class = "edit-use-where", where)),
    if (line$n == 0) {
      NULL
    } else if (as_cards) {
      cards()
    } else if (line$n <= edit_usage_shown) {
      rows(seq_len(line$n))
    } else if (length(groups) > 1) {
      div(class = "edit-use-groups", lapply(groups, function(group) {
        edit_use_fold(group$questionnaire, length(group$index), rows(group$index))
      }))
    } else {
      tagList(
        rows(seq_len(edit_usage_shown)),
        tags$details(
          class = "edit-use-more",
          tags$summary(
            class = "btn btn-sm btn-outline-dark",
            span(class = "when-closed", paste("Show all", format(line$n, big.mark = ","))), span(class = "when-open", "Show fewer")
          ),
          rows(seq(edit_usage_shown + 1, line$n))
        )
      )
    },
    if (!is.null(line$add)) {
      review_button(
        icon_label("add", line$add$label), "jump",
        `data-kind` = line$add$kind, `data-verb` = "add", `data-scope` = line$add$scope,
        class = "btn btn-sm btn-outline-dark edit-use-add"
      )
    }
  )
}

# ------ What a row belongs to ---------------------------------------------------------------------------------
# The tags over a row's details: what isn't changed on its own form (see `edit_service_details()`). Each is the
# thing's name alone; what it is to the row ("Study") is said when it is pointed at, and to screen readers. One
# that names something opens it, which is where it is changed; one that is empty isn't drawn.
# lines: The details' lines that are tags (`pill`).
edit_context_ui <- function(lines) {
  lines <- lines[lines$value != "(empty)", , drop = FALSE]
  if (nrow(lines) == 0) {
    return(NULL)
  }
  div(class = "edit-context", lapply(seq_len(nrow(lines)), function(i) {
    if (is.na(lines$kind[i]) || is.na(lines$key[i])) {
      return(span(class = "edit-context-pill", title = lines$label[i], span(class = "visually-hidden", paste0(lines$label[i], ": ")), lines$value[i]))
    }
    edit_open_pill(lines$kind[i], lines$key[i], lines$value[i], lines$label[i])
  }))
}

# A tag that opens the thing it names: an open folder and the name.
# kind,key: The kind (a name of `edit_entities`) and the row's key.
# name: The thing's name.
# label: What it is to the row it is shown on ("Study", "Scale").
edit_open_pill <- function(kind, key, name, label) {
  review_button(
    tagList(app_icon("view"), span(class = "edit-context-value", name)), "jump",
    `data-kind` = kind, `data-verb` = "view", `data-key` = key,
    class = "edit-context-pill is-link",
    title = paste0(label, ": open it, which is where it is changed"), `aria-label` = paste0(label, ": ", name, ". Open it.")
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

# A questionnaire that prints a component, as its card in the details shows it: the questionnaire, as a tag that
# opens it, then each place it prints the component there (`edit_use_place()`); a questionnaire that prints it more
# than once has them all in the one card.
# uses: The `edit_service_uses()` of one questionnaire.
edit_use_card <- function(uses) {
  here <- uses[[1]]
  div(
    class = "edit-use",
    div(class = "edit-use-head", edit_open_pill("questionnaire", key_text(here$questionnaire_id), here$questionnaire, "Questionnaire")),
    lapply(uses, edit_use_place)
  )
}

# One place a questionnaire prints a component: the words as they read there (a grid's lead-in, then the question),
# with a button that opens the block as it is in that questionnaire alone; for a section, grid or group every block
# it holds, each with View; for a question each variable, with what it records, who it is about, and its choices.
# use: One of `edit_service_uses()`.
edit_use_place <- function(use) {
  held <- use$inside
  div(
    class = "edit-use-place",
    div(
      class = "edit-use-place-head",
      span(class = "edit-use-words", use$words),
      edit_view_button(
        "in Questionnaire", paste(use$words, "in", use$questionnaire),
        edit_opens("placement", use$placement_id, key_text(use$questionnaire_id))
      )
    ),
    if (is_container(use$kind) && NROW(held) == 0) p(class = "edit-use-detail", "Holds nothing yet."),
    if (NROW(held) > 0) {
      tags$ul(class = "edit-use-rows", lapply(seq_len(nrow(held)), function(i) {
        tags$li(
          class = "edit-use-row",
          span(class = "edit-use-row-text", held$words[i]),
          edit_view_button(held$kind[i], held$words[i], edit_opens("component", held$component_id[i]))
        )
      }))
    },
    if (identical(use$kind, "Question") && length(use$waves) == 0) p(class = "edit-use-detail", "Not asked in any wave yet."),
    if (length(use$boxes) > 0) {
      div(class = "edit-use-boxes", lapply(use$boxes, function(box) {
        div(
          class = "edit-use-box",
          div(span(class = "edit-use-variable", box$matr_id), if (nzchar(box$tags)) span(class = "edit-use-tags", box$tags)),
          if (length(box$coding) > 0) tags$ul(class = "edit-use-coding", lapply(box$coding, tags$li))
        )
      }))
    }
  )
}

# ------ Deleting ------------------------------------------------------------------------------------------------
# Everything staff should read before something is deleted.
# What it is, what else goes with it, and what stops it.
# title: The row's name.
# plan: The result of `edit_service_delete_plan()`.
# noun: What the row is called ("question", for a wording that is one).
edit_delete_ui <- function(entity, title, plan, noun = entity$noun) {
  reach <- plan$reach
  blocked <- length(plan$blocked) > 0
  div(
    class = "edit-delete",
    div(
      class = "delete-banner",
      app_icon("delete"),
      div(
        span(class = "delete-banner-label", paste("You're deleting this", noun)),
        strong(class = "delete-banner-title", title)
      )
    ),
    if (blocked) edit_blocked_ui(plan$blocked),
    if (!blocked) edit_reach_ui(entity, reach, plan)
  )
}

edit_blocked_ui <- function(blocked, title = "This can't be deleted yet") {
  edit_callout(
    "danger",
    title = title,
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

# ------ Merging ------------------------------------------------------------------------------------------------
# Merge on a row's details: the drop-down of what to merge it into, then (drawn apart, as one is picked) what that
# would do. The row in the details is the one that goes; the one picked stays.
# choices: What it can be merged into, from `edit_merge_choices()`.
edit_merge_ui <- function(ns, entity, row, title, choices) {
  noun <- edit_noun(entity, row)
  if (length(choices) < 2) {
    return(edit_callout("warning", title = "Nothing to merge it into", p(paste0("There is no other ", noun, " of its kind in the database."))))
  }
  tagList(
    div(class = "merge-side is-going", span(class = "merge-side-label", paste("This", noun, "is removed")), strong(title)),
    div(
      class = "form-field edit-field is-wide",
      selectizeInput(
        ns("merge_into"), NULL, choices,
        selected = "", width = "100%",
        options = list(placeholder = paste0("Search for the ", noun, " to keep…"))
      ) |> tag_input_attributes("select", `aria-label` = paste("The", noun, "to keep"))
    ),
    uiOutput(ns("merge_preview"))
  )
}

# What merging would do, once the one to keep is picked: the one that stays, what moves to it, and the box to tick;
# or what stands in the way.
# plan: The result of `edit_service_merge_plan()`, or `NULL` while none is picked.
edit_merge_preview_ui <- function(ns, entity, row, plan) {
  if (is.null(plan)) {
    return(NULL)
  }
  noun <- edit_noun(entity, row)
  tagList(
    div(class = "merge-side is-staying", span(class = "merge-side-label", paste("This", noun, "stays")), strong(plan$into_name)),
    if (length(plan$blocked) > 0) {
      edit_blocked_ui(plan$blocked, title = "These two can't be merged yet")
    } else {
      tagList(
        edit_callout("info", title = "What merging does", p(plan$sentence)),
        div(
          class = "delete-confirm",
          checkboxInput(
            ns("confirm_merge_read"),
            sprintf("I understand the two become one. This can't be undone: afterwards nothing says what used “%s”.", plan$from),
            value = FALSE, width = "100%"
          )
        )
      )
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

# Asks before two rows are merged (a tag renamed to another tag's name): it can't be taken back, since afterwards
# nothing says which things carried which.
# merge: What the kind's `merge()` gave: `from` and `into_name` (the two names) and `sentence` (what saving does).
edit_merge_dialog <- function(merge, confirm_id) {
  confirm_dialog(
    "Merge these tags?",
    p(merge$sentence),
    p(sprintf(
      "This can't be undone: once they are one tag, nothing says what carried “%s” and what carried “%s”.",
      merge$from, merge$into_name
    )),
    confirm_id = confirm_id, confirm_label = "Merge them", keep_label = "Go back", easy_close = TRUE
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
