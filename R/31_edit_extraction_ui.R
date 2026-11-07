# The Edit Extraction page: a title on the left and a row of buttons on the right, with how far the checking
# has got across the page under both, over the extracted questionnaire.
# One panel: the whole questionnaire as it was extracted, as a list with the picked block's details
# beside it (`extraction_outline_ui()`). The header's buttons: Undo, Save, Normalize (answer choices worded as
# the database words them, and questions that differ only in who they ask about linked: `normalize_dialog()`),
# Open PDF (if the file is here: it opens in its own tab, at the page of the block picked) and Preview (of the
# change request).
# On a phone the buttons are only their icons, sharing a row of their own evenly under the progress bar.
# (It was called the review page. Change Requests has its own review, of requests waiting for approval.)
edit_extraction_ui <- function(id) {
  ns <- NS(id)
  as_fill_carrier(div(
    class = "edit-extraction-page",
    `data-ns` = ns(""),
    div(
      class = "review-header",
      div(
        class = "review-title",
        h1(textOutput(ns("title"), inline = TRUE)),
        p(class = "subhead", textOutput(ns("subtitle"), inline = TRUE), uiOutput(ns("badges"), inline = TRUE))
      ),
      div(
        class = "review-actions",
        uiOutput(ns("save_status"), inline = TRUE),
        div(
          class = "review-buttons",
          actionButton(ns("undo"), icon_label("undo", "Undo"), class = "btn-outline-dark", disabled = TRUE, title = "Undo"),
          actionButton(ns("save"), icon_label("save", "Save"), class = "btn-outline-dark", disabled = TRUE, title = "Save"),
          actionButton(
            ns("normalize"), icon_label("normalize", "Normalize"), class = "btn-outline-dark",
            title = "Word answer choices as the database does, and link questions that differ only in who they ask about"
          ),
          uiOutput(ns("pdf_link"), inline = TRUE),
          uiOutput(ns("request_button"), inline = TRUE)
        )
      ),
      uiOutput(ns("progress"), class = "review-progress") # how far the checking has got, across the page
    ),
    uiOutput(ns("banner")),
    card(
      class = "structure-card",
      card_body(class = "structure-body", padding = 0, extraction_outline_ui(ns("outline")))
    )
  ))
}

# A button that opens the questionnaire's PDF in a new window, if the file is on this server.
# On Edit Extraction it opens at the page the picked block was read from (`at_picked`, www/js/review.js),
# when that is known.
# url: The address the browser gets the PDF from, or `NULL`.
# at_picked: Whether it opens at the picked block's page.
# Returns: A UI element, or `NULL`.
pdf_open_button <- function(url, at_picked = FALSE) {
  if (is.null(url)) {
    return(NULL)
  }
  tags$a(
    href = url, target = "_blank", rel = "noopener", class = "btn btn-outline-dark",
    `data-pdf-at-picked` = if (at_picked) NA,
    title = if (at_picked) "Open the PDF in a new tab, at the page of the block picked" else "Open the PDF in a new tab",
    icon_label("pdf", "Open PDF")
  )
}

# How far the checking has got: a bar that fills as blocks are marked as checked.
# counts: From `draft_service_counts()`.
# Returns: A UI element saying "N% human-reviewed", with the counts in its tooltip.
structure_progress_ui <- function(counts) {
  percent <- draft_service_percent_checked(counts)
  bslib::tooltip(
    div(
      class = "review-structure",
      div(
        class = "progress review-structure-bar", role = "progressbar", `aria-label` = "Blocks checked",
        `aria-valuenow` = percent, `aria-valuemin` = 0, `aria-valuemax` = 100,
        div(class = "progress-bar", style = paste0("width: ", percent, "%")),
        span(class = "review-structure-text", paste0(percent, "% human-reviewed"))
      )
    ),
    sprintf("%d of %d blocks marked as checked", counts$checked, counts$blocks)
  )
}

# What is said above the panel: a note from a reviewer who sent the draft back, or that it is waiting.
# Returns: A UI element or `NULL`.
edit_extraction_banner <- function(record) {
  if (identical(record$status, "Sent Back") && !is.null(record$sent_back)) {
    return(edit_callout(
      "warning",
      title = paste0(record$sent_back$by, " sent this back"),
      p(record$sent_back$note), p("Change what they ask, save, and submit the change request again.")
    ))
  }
  if (identical(record$status, "Awaiting Review")) {
    edit_callout(
      "info",
      title = "This is waiting for approval on Change Requests",
      "Saving your changes makes a new version of the request, which nobody has approved yet."
    )
  }
}

# What is said above the panel while this page doesn't have the draft to work on: who does, or that it
# was let go while staff were away. The page only shows the draft then (`.claim-lost`).
# claim: Why the page doesn't have it, as `edit_extraction_server()` keeps it: `NULL` while it does.
# unsaved: Whether there are changes on screen that aren't saved.
# Returns: A UI element or `NULL`.
edit_extraction_claim_banner <- function(ns, claim, unsaved = FALSE) {
  if (is.null(claim)) {
    return(NULL)
  }
  if (identical(claim$why, "taken")) {
    return(div(class = "claim-lost", edit_callout(
      "warning",
      title = sprintf("%s is working on this draft", claim$by),
      p(
        "You can look at it, but not change it, so neither of you overwrites the other.",
        if (unsaved) "Your changes that aren't saved can't be saved while they have it."
      ),
      actionButton(ns("take_over_banner"), "Take over", class = "btn-outline-dark")
    )))
  }
  div(class = "claim-lost", edit_callout(
    "info",
    title = "You were away, so others can open this draft",
    p("Nobody else has it yet. Keep working to change it again."),
    actionButton(ns("keep_working"), "Keep working", class = "btn-primary")
  ))
}

# The Preview Change Request dialog: the questionnaire as its change request shows it.
# Nothing is written while it is open. Submit Change Request goes on to sending it. Where it goes
# (its Basic Info) isn't asked here: it is filled in on Change Requests.
preview_request_dialog <- function(ns) {
  modalDialog(
    title = "Preview Change Request",
    size = "xl",
    easyClose = FALSE,
    uiOutput(ns("preview")),
    footer = tagList(
      actionButton(ns("preview_cancel"), "Cancel", class = "btn-outline-dark"),
      uiOutput(ns("preview_blocker"), inline = TRUE, class = "quick-add-blocker"),
      actionButton(ns("submit_request"), "Submit Change Request", class = "btn-primary", disabled = TRUE)
    )
  )
}

# The dialog that submits an extraction's change request.
# Every request goes to Change Requests, where its Basic Info is filled in.
# summary: What the questionnaire holds, in words.
# unchecked: How many blocks haven't been marked as checked.
# approval: Whether it needs another staff member's approval (a STAFF member's request) rather
#   than being added by whoever submits it (an ADMIN).
# reviewers: People who can be asked to review it: a named vector of staff ids, or empty.
submit_request_dialog <- function(ns, summary, unchecked, approval, reviewers) {
  modalDialog(
    title = "Submit Change Request",
    size = "l",
    easyClose = FALSE,
    p(strong(summary)),
    if (unchecked > 0) {
      edit_callout(
        "warning",
        title = sprintf("%s haven't been marked as checked", plural(unchecked, "block")),
        "You can still go on. Reviewers may want them checked first."
      )
    },
    if (approval) {
      edit_callout(
        "info",
        title = "It goes to Change Requests",
        "Fill in its Basic Info there: the registry, study, questionnaire and wave it goes into. Then one other ",
        "staff member approves it. Nothing is added to the MATR metadata database until they do, and then all of it is added at once."
      )
    } else {
      edit_callout(
        "info",
        title = "It goes to Change Requests",
        "Fill in its Basic Info there: the registry, study, questionnaire and wave it goes into. As an ADMIN you can ",
        "then add it yourself, without approval."
      )
    },
    if (approval && length(reviewers) > 0) {
      reviewer_select(ns("request_reviewer"), reviewers)
    },
    footer = tagList(
      actionButton(ns("back_to_preview"), "Back", class = "btn-outline-dark"),
      actionButton(ns("confirm_request"), "Send to Change Requests", class = "btn-primary")
    )
  )
}

# The Normalize dialog: what could be worded as the database words it, and the questions that could be linked,
# each with a button that does it (a change like any other, which Undo takes back). Nothing is written to the
# database: it is all in the draft until its change request is approved.
normalize_dialog <- function(ns) {
  modalDialog(
    title = "Normalize",
    size = "xl",
    easyClose = TRUE,
    uiOutput(ns("normalize_body")),
    footer = modalButton("Close")
  )
}

# What the Normalize dialog lists.
# choices: From `normalize_service_choice_suggestions()`.
# groups: From `normalize_service_question_groups()`.
# draft: The questionnaire on screen.
# Returns: A UI element. Its buttons are data-action buttons of the page (`use_choice`, `link`, `link_all`,
#   `unlink`), so the body carries the page's namespace.
normalize_ui <- function(ns, choices, groups, draft) {
  open <- Filter(function(group) !group$linked, groups)
  div(
    class = "normalize", `data-ns` = ns(""),
    div(
      class = "request-section",
      h3("Answer choices"),
      p(
        class = "form-text",
        "Ranges, dashes and abbreviations are already written one way (“1 to 2” is “1-2”, “hrs” is “hours”). ",
        "These choices are worded a little differently from one the database has, or another of this questionnaire's."
      ),
      if (nrow(choices) == 0) {
        p(class = "text-muted", "Nothing to change.")
      } else {
        tags$table(
          class = "table table-sm normalize-table",
          tags$thead(tags$tr(tags$th("This questionnaire has"), tags$th(class = "is-count", "Choices"), tags$th("Use instead"), tags$th())),
          tags$tbody(lapply(seq_len(nrow(choices)), function(i) {
            row <- choices[i, ]
            tags$tr(
              tags$td(row$from),
              tags$td(class = "is-count", row$uses),
              tags$td(
                row$to, " ",
                span(class = "form-text", switch(row$why,
                  merged = "(merged into it on Make an edit)",
                  if (row$in_database) "(in the database)" else "(more of this questionnaire's choices)"
                ))
              ),
              tags$td(review_button("Use it", "use_choice", `data-from` = row$from, `data-to` = row$to))
            )
          }))
        )
      }
    ),
    div(
      class = "request-section",
      h3("Questions about different people, times or things"),
      p(
        class = "form-text",
        "These questions differ only in who, when or what they ask about. Linking them keeps each one's words, number ",
        "and variables, and gives them one stem, with [subject] for who they ask about, [time frame] for when, and ",
        "[item] for anything else (tobacco, alcohol, cannabis). Each variable is then about who and when its question ",
        "asks about, and a choice that names who says [subject] instead, so they share one set of choices."
      ),
      if (length(groups) == 0) {
        p(class = "text-muted", "None found.")
      } else {
        tagList(
          if (length(open) > 1) {
            div(class = "normalize-all", review_button(icon_label("link", "Link all"), "link_all", class = "btn btn-sm btn-primary"))
          },
          lapply(groups, function(group) normalize_group_ui(group, draft))
        )
      },
      normalize_by_hand_ui(ns)
    )
  )
}

# Linking questions Normalize doesn't find, such as one reworded for another person ("What is your date of birth?",
# "What is the date of birth of the other biological parent?"): their numbers and the stem they share.
normalize_by_hand_ui <- function(ns) {
  div(
    class = "normalize-by-hand",
    h4("Link other questions"),
    p(class = "form-text", "Questions that ask the same thing in other words, such as one reworded for another person."),
    textInput(ns("link_numbers"), "Their numbers", placeholder = "e.g. B.1, B.12"),
    textInput(ns("link_stem"), "The stem they share", placeholder = "e.g. what is the date of birth of [subject]?"),
    actionButton(ns("link_by_hand"), "Link", class = "btn btn-sm btn-primary")
  )
}

# Who, when and what one question of a group asks about: "(about Twin A, in the past year)", "(in your lifetime)",
# "(tobacco)".
normalize_group_about <- function(group, i) {
  parts <- c(
    if (!is.na(group$subjects[i])) paste("about", group$subjects[i]),
    if (!is.na((group$time_frames %||% NA_character_)[i])) group$time_frames[i],
    if (is.na(group$subjects[i]) && !is.null(group$items)) group$items[i]
  )
  if (length(parts) > 0) paste0("(", paste(parts, collapse = ", "), ")") else ""
}

# One group of questions that differ only in who or when they ask about: its stem, each question with whom and
# when it asks about, and Link or Unlink.
normalize_group_ui <- function(group, draft) {
  blocks <- draft$components[match(group$ids, draft$components$id), , drop = FALSE]
  ids <- paste(group$ids, collapse = ",")
  div(
    class = c("edit-card", "normalize-group", if (group$linked) "is-linked"),
    div(
      class = "normalize-group-head",
      h4(class = "edit-card-title", group$stem),
      if (group$linked) {
        tagList(
          row_badge(icon_label("link", "Linked")),
          review_button(icon_label("unlink", "Unlink"), "unlink", `data-ids` = ids)
        )
      } else {
        review_button(icon_label("link", "Link"), "link", `data-ids` = ids, class = "btn btn-sm btn-primary")
      }
    ),
    tags$ul(lapply(seq_len(nrow(blocks)), function(i) {
      number <- draft_service_question_number(blocks[i, ])
      tags$li(
        if (!is.na(number)) strong(number, outline_number_separator),
        blocks$text[i], " ",
        span(class = "form-text", normalize_group_about(group, i))
      )
    }))
  )
}

