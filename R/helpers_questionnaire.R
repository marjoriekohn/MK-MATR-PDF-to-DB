# ------ A questionnaire, laid out to read -----------------------------------------------------
# A draft's questionnaire drawn as a table to read, with nothing to edit, the way a codebook lists it: one
# row for each variable, with its MATR ID (Variable ID), the words of its question with the variable's label in
# square brackets (Question Text), its subjects, time frames and units (Tags), and its codes and choices, or a
# write-in's kind of answer (Variable Coding). Blocks that aren't questions (an instruction, a note) have a row
# each, in printed order. The rows are grouped under the section (and group) they are printed in. It is a
# DataTables table (the DT package). The Push dialog shows it before the extraction is pushed into the database:
# once the push has been tried (`extraction_service_known()`), NEW marks each question wording, option set,
# answer choice and tag the database doesn't have yet. Above it, a toolbar drawn like the site's others: All /
# New / Already in the database (once there are marks), Search, and Download CSV (www/js/preview.js works them).

# A draft's questionnaire, laid out to read as a table.
# known: What the database has already, from `extraction_service_known()`, or `NULL` for no marks.
questionnaire_preview_ui <- function(draft, known = NULL) {
  if (nrow(draft$components) == 0) {
    return(p(class = "form-text", "This questionnaire has no blocks."))
  }
  rows <- questionnaire_preview_rows(draft, known)
  shown <- data.frame(
    Section = htmltools::htmlEscape(rows$section),
    `Variable ID` = ifelse(is.na(rows$matr_id), preview_blank(rows$missing), htmltools::htmlEscape(rows$matr_id)),
    `Question Text` = paste0(
      htmltools::htmlEscape(rows$text),
      ifelse(rows$text_new, paste0(" ", vapply(rows$kind, function(kind) {
        preview_new_mark(if (kind == "Question") "This question's wording" else paste("This", tolower(kind)))
      }, character(1), USE.NAMES = FALSE)), ""),
      ifelse(nzchar(rows$label), paste0(' <span class="qt-label">[', htmltools::htmlEscape(rows$label), "]</span>"), "")
    ),
    Tags = as.character(unlist(Map(preview_tags_html, rows$tag_names, rows$tag_new))),
    `Variable Coding` = as.character(unlist(Map(preview_coding_html, rows$coding, rows$coding_new, rows$set_new))),
    Status = rows$status,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  status_column <- which(names(shown) == "Status") - 1L # DataTables counts from 0
  div(
    class = "questionnaire-preview", `data-status-column` = status_column,
    preview_toolbar(rows),
    DT::datatable(
      shown,
      class = "compact questionnaire-table", rownames = FALSE, escape = FALSE,
      extensions = c("Buttons", "RowGroup"),
      # the toolbar above works the table (www/js/preview.js)
      callback = DT::JS("if (window.matrPreviewTable) window.matrPreviewTable(table);"),
      options = list(
        # the CSV button is DataTables' own, pressed by the toolbar's (its own is hidden: _history.scss)
        dom = "Brtip", ordering = FALSE, pageLength = 50, autoWidth = FALSE,
        # the rows stay in printed order, grouped under their section
        rowGroup = list(dataSrc = 0, emptyDataGroup = "Not in a section"),
        columnDefs = list(
          list(targets = 0, visible = FALSE),
          list(targets = status_column, visible = FALSE, searchable = FALSE),
          list(targets = 1, width = "9rem"), list(targets = 3, width = "14%"), list(targets = 4, width = "28%")
        ),
        buttons = list(list(
          extend = "csv", text = "Download CSV", filename = preview_file_name(draft),
          # every column but the status, the section too, with a choice or a tag on each line, and no markup or NEW
          exportOptions = list(columns = 0:(status_column - 1L), format = list(body = DT::JS(
            "function (data) { return String(data).replace(/<span class=\"new-mark[^\"]*\"[^>]*>[\\s\\S]*?<\\/span>/g, '')",
            ".replace(/<br\\s*\\/?>/g, '\\n').replace(/<[^>]+>/g, '').replace(/ +\\n/g, '\\n').trim()",
            ".replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&quot;/g, '\"').replace(/&#39;/g, \"'\").replace(/&amp;/g, '&'); }"
          )))
        )),
        language = list(info = "_START_ to _END_ of _TOTAL_ rows", infoFiltered = "(of _MAX_)", emptyTable = "No rows", zeroRecords = "No rows match")
      )
    )
  )
}

# The table's toolbar: which rows to show (only once the push has been tried, so there are NEW marks: all of
# them, those with something new, or those with nothing new), a search box and Download CSV. They work the table
# in the browser (www/js/preview.js), so none is a Shiny input.
# rows: From `questionnaire_preview_rows()`.
preview_toolbar <- function(rows) {
  n_new <- sum(rows$status == "new")
  n_found <- sum(rows$status == "found")
  count <- function(label, n) paste0(label, " (", format(n, big.mark = ","), ")")
  div(
    class = "qt-toolbar",
    if (n_new + n_found > 0) {
      toggle_buttons(
        "qt-filter",
        stats::setNames(c("all", "new", "found"), c(
          count("All", nrow(rows)), count("New", n_new), count("Already in the database", n_found)
        )),
        "all", "Which rows to show", attributes = list(`data-qt-filter` = ""), small = TRUE
      )
    },
    div(
      class = "toolbar-field qt-search-field",
      app_icon("search"),
      tags$input(
        type = "search", class = "form-control form-control-sm qt-search",
        placeholder = "Search", `aria-label` = "Search the questionnaire", autocomplete = "off"
      )
    ),
    tags$button(type = "button", class = "btn btn-sm btn-outline-dark qt-download", app_icon("download"), "Download CSV")
  )
}

# The mark beside what the push adds: the certificate, and NEW. Written as one string, with no line breaks in
# it, so the CSV's formatter can take it out whole.
# what: What is new, said when it is pointed at ("This answer choice"), or `NULL` to say nothing.
# label: Its words.
preview_new_mark <- function(what = NULL, label = "NEW") {
  title <- if (!is.null(what)) {
    paste0(' title="', htmltools::htmlEscape(paste(what, "isn't in the database yet: the push adds it"), attribute = TRUE), '"')
  }
  paste0('<span class="new-mark"', title, ">", as.character(app_icon("new")), htmltools::htmlEscape(label), "</span>")
}

# A row's tags, each a pill, a new one marked; nothing for none. The pills are set in a line, but there is a line
# break between each, so the CSV has a tag on each line.
preview_tags_html <- function(names, new) {
  if (length(names) == 0) {
    return("")
  }
  pills <- vapply(seq_along(names), function(i) {
    paste0(
      '<span class="qt-tag', if (isTRUE(new[i])) " is-new", '">', htmltools::htmlEscape(names[i]),
      if (isTRUE(new[i])) paste0(" ", preview_new_mark("This tag")), "</span>"
    )
  }, character(1))
  paste(pills, collapse = "<br>")
}

# A row's coding, a line each: a new answer choice marked, and a new option set (a set of choices no variable in
# the database offers yet) marked over them.
preview_coding_html <- function(lines, new, set_new) {
  lines <- as.character(htmltools::htmlEscape(lines))
  marked <- seq_along(lines) %in% which(new %in% TRUE)
  lines[marked] <- paste0(lines[marked], " ", preview_new_mark("This answer choice"))
  if (isTRUE(set_new)) {
    lines <- c(preview_new_mark("This set of choices", "NEW option set"), lines)
  }
  paste(lines, collapse = "<br>")
}

# "no MATR ID" or "no variables", in italics.
preview_blank <- function(missing) ifelse(nzchar(missing), paste0("<em>", missing, "</em>"), "")

# The CSV's name: the questionnaire's abbreviation or name, or "questionnaire".
preview_file_name <- function(draft) {
  name <- draft$questionnaire$abbreviation %||% NA
  if (!has_text(name)) name <- draft$questionnaire$name %||% NA
  if (!has_text(name)) name <- "questionnaire"
  gsub("[^A-Za-z0-9_-]+", "_", trimws(name))
}

# The rows of the questionnaire's table, in printed order.
# A question has a row for each of its variables (or one, saying it has none); any other block that isn't a
# section or group has one row. Each row names the sections and groups it is in.
# known: What the database has already (`extraction_service_known()`), to say what each row adds, or `NULL`.
# Returns: A data frame: `section` (e.g. "PART B. YOUR HABITS › Group 1", or "" at the top level),
#   `kind`, `matr_id` (NA when there is none), `missing` ("no MATR ID", "no variables" or ""), `text`,
#   `label` (the variable's, or ""), `tags` ("[past 30 days] [siblings]", or ""), `tag_names` (a list of the
#   tags), `coding` (a list of lines: "1 = Yes", or the write-in's kind of answer), and what the push adds:
#   `text_new` (the question's wording, or another block's words), `tag_new` and `coding_new` (a list, a TRUE or
#   FALSE for each tag and each line), `set_new` (the variable's set of choices) and `status`: "new" for a row with
#   any of those, "found" for one with none, and "" when `known` is `NULL` (and for a block that isn't a question
#   when the catalog's other blocks couldn't be read).
questionnaire_preview_rows <- function(draft, known = NULL) {
  rows <- list()
  add <- function(section, kind, matr_id = NA_character_, missing = "", text = "", label = "", tag_names = character(),
                  coding = character(), text_new = FALSE, tag_new = logical(), coding_new = logical(), set_new = FALSE) {
    adds <- text_new || set_new || any(tag_new) || any(coding_new)
    unknown <- is.null(known) || (kind != "Question" && is.null(known$block_found))
    rows[[length(rows) + 1]] <<- list(
      section = section, kind = kind, matr_id = matr_id, missing = missing, text = text, label = label,
      tags = if (length(tag_names) > 0) paste0("[", tag_names, "]", collapse = " ") else "",
      tag_names = list(tag_names), coding = list(coding), text_new = text_new, tag_new = list(tag_new),
      coding_new = list(coding_new), set_new = set_new,
      status = if (unknown) "" else if (adds) "new" else "found"
    )
  }
  walk <- function(parent_id, path) {
    blocks <- draft_service_children(draft, "components", parent_id)
    groups <- 0L
    for (i in seq_len(nrow(blocks))) {
      block <- blocks[i, , drop = FALSE]
      if (is_container(block$component_type)) {
        if (block$component_type == "Group") groups <- groups + 1L
        walk(block$id, c(path, preview_container_name(block, groups)))
        next
      }
      section <- paste(path, collapse = " \u203a ")
      if (block$component_type != "Question") {
        # (words the catalog doesn't have, once its other blocks are read, are new)
        words_new <- !is.null(known$block_found) && !is.na(block$text) &&
          !extraction_service_block_key(block$component_type, block$text) %in% known$block_found
        add(section, block$component_type, text = preview_words(block), text_new = words_new)
        next
      }
      words <- preview_question_words(block)
      text_new <- !is.null(known) && !is.na(block$text) && !squish(block$text) %in% known$question_found
      boxes <- draft_service_children(draft, "variables", block$id)
      if (nrow(boxes) == 0) add(section, "Question", missing = "no variables", text = words, text_new = text_new)
      for (j in seq_len(nrow(boxes))) {
        box <- boxes[j, , drop = FALSE]
        tag_names <- preview_variable_tag_list(box, block)
        coding <- preview_coding(box, draft)
        choices <- if (box$variable_type == "closed_ended") draft_service_children(draft, "response_options", box$id)
        answers <- if (NROW(choices) > 0) choices$answer_option[!is.na(choices$answer_option)] else character()
        add(
          section, "Question",
          matr_id = box$matr_id, missing = if (is.na(box$matr_id)) "no MATR ID" else "",
          text = words, label = if (is.na(box$label)) "" else trimws(box$label), tag_names = tag_names, coding = coding,
          text_new = text_new,
          tag_new = !is.null(known) & extraction_service_tag_key(tag_names) %in% known$tag_new,
          coding_new = if (!is.null(known) && NROW(choices) > 0) choices$answer_option %in% known$answer_new else rep(FALSE, length(coding)),
          set_new = !is.null(known) && length(answers) > 0 && extraction_service_option_set_key(answers) %in% known$option_set_new
        )
      }
    }
  }
  walk(NA, character())
  if (length(rows) == 0) {
    return(data.frame(
      section = character(), kind = character(), matr_id = character(), missing = character(), text = character(),
      label = character(), tags = character(), tag_names = I(list()), coding = I(list()), text_new = logical(),
      tag_new = I(list()), coding_new = I(list()), set_new = logical(), status = character()
    ))
  }
  column <- function(name, type) vapply(rows, function(row) row[[name]], type)
  listed <- function(name) I(lapply(rows, function(row) row[[name]][[1]]))
  data.frame(
    section = column("section", ""),
    kind = column("kind", ""),
    matr_id = vapply(rows, function(row) as.character(row$matr_id), ""),
    missing = column("missing", ""),
    text = column("text", ""),
    label = column("label", ""),
    tags = column("tags", ""),
    tag_names = listed("tag_names"),
    coding = listed("coding"),
    text_new = column("text_new", TRUE),
    tag_new = listed("tag_new"),
    coding_new = listed("coding_new"),
    set_new = column("set_new", TRUE),
    status = column("status", ""),
    stringsAsFactors = FALSE
  )
}

# A section by its heading ("(no heading)" for one without), a group as "Group 2" (the second in what holds
# it). Each group has a name of its own, so where one ends shows as a section's end does: two groups side by
# side are two headings, not one.
preview_container_name <- function(block, group_number = 1L) {
  if (block$component_type == "Group") {
    return(paste("Group", group_number))
  }
  if (is.na(block$text)) "(no heading)" else block$text
}

preview_words <- function(block) {
  if (is.na(block$text)) "(no words)" else block$text
}

# The question's words, without its number at the start if they were read with it ("3. Have you ...").
preview_question_words <- function(block) {
  if (is.na(block$text)) {
    return("(no words)")
  }
  number <- draft_service_question_number(block)
  if (is.na(number) || !nzchar(number)) {
    return(block$text)
  }
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", number)
  trimmed <- sub(paste0("^\\s*(Q(uestion)?\\s*)?", escaped, "\\s*[.):-]\\s+"), "", block$text, ignore.case = TRUE)
  if (nzchar(trimmed)) trimmed else block$text
}

# A variable's tags, the way the database keeps them (a tag is a subject, a time frame or a unit): a write-in's
# unit, then when and who its answer is about (its own tag of each kind, or else its question's), each when there
# is one (a variable about the respondent has no subject).
# block: The variable's question, whose own tags are about all its variables.
# Returns: A character vector, e.g. `c("hours", "past 30 days", "siblings")`; empty for none.
preview_variable_tag_list <- function(box, block = NULL) {
  of_kind <- function(kind) if (is.na(box[[kind]])) block[[kind]] %||% NA_character_ else box[[kind]]
  tags <- c(box$unit, of_kind("time_frame"), of_kind("subject"))
  tags <- trimws(tags[!is.na(tags)])
  tags[nzchar(tags)]
}

# The same, in words: "[hours] [past 30 days] [siblings]", or "".
preview_variable_tags <- function(box, block = NULL) {
  tags <- preview_variable_tag_list(box, block)
  if (length(tags) > 0) paste0("[", tags, "]", collapse = " ") else ""
}

# Each choice with its code ("1 = Yes"), one line each, or a write-in's kind of answer.
preview_coding <- function(box, draft) {
  if (box$variable_type != "closed_ended") {
    return(paste("Write-in:", if (is.na(box$answer_data_type)) "answer type not picked" else box$answer_data_type))
  }
  choices <- draft_service_children(draft, "response_options", box$id)
  if (nrow(choices) == 0) {
    return("no choices")
  }
  paste0(
    ifelse(is.na(choices$code_value), "?", choices$code_value), " = ",
    ifelse(is.na(choices$answer_option), "(blank)", choices$answer_option)
  )
}

# ------ An extraction's push, laid out to read --------------------------------------------------------
# What pushing an extraction into the database comes to, shown to whoever reviewed it before they push it
# (there is no second approval): its Basic Info (the registry, study, questionnaire and wave it goes into), why
# the database would refuse it if it would, and the questionnaire itself, with NEW beside what the database
# doesn't have yet.

# What pushing an extraction comes to, laid out to read.
# preview: A list: `problem` (why it can't be pushed, or `NULL`) and `unchecked` (how many blocks haven't been
#   marked as checked). Either can be left out.
# draft: The questionnaire, or `NULL` if it couldn't be read.
# pdf_url: The address of its PDF, or `NULL`.
# basic_info: The Basic Info form, to show first, or `NULL`.
# table: Where the questionnaire's table goes, in place of drawing it here: a `uiOutput()` the page draws
#   it in once the dialog is open (a DataTables table put in a dialog as it opens stays empty). `NULL` draws it.
# questionnaire: Whether to show the questionnaire (its table, its PDF and how much of it is checked).
extraction_request_ui <- function(preview, draft, pdf_url = NULL, basic_info = NULL, table = NULL, questionnaire = TRUE) {
  div(
    class = "request-preview",
    if (!is.null(basic_info)) {
      div(class = "request-section", h3("Basic Info"), basic_info)
    },
    if (!is.null(preview$problem)) {
      edit_callout("danger", title = "This can't be pushed as it is", preview$problem)
    },
    if (questionnaire && isTRUE(preview$unchecked > 0)) {
      p(class = "form-text", sprintf("%s haven't been marked as checked yet.", plural(preview$unchecked, "block")))
    },
    if (questionnaire) {
      div(
        class = "request-section",
        if (!is.null(pdf_url)) div(class = "change-request-questionnaire-head", pdf_open_button(pdf_url)),
        if (is.null(draft)) {
          p(class = "form-text", "The questionnaire couldn't be read. Edit opens it on the Edit Extraction page.")
        } else {
          div(class = "change-request-questionnaire", table %||% questionnaire_preview_ui(draft))
        }
      )
    }
  )
}
