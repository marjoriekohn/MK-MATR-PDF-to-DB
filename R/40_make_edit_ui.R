# The Metadata page (the module is called make_edit).
# Everything staff can add, change, merge, empty or delete in the MATR metadata database, laid out as Edit
# Extraction lays out a questionnaire: a list on the left and the item picked on the right. Each kind of thing is a
# section of the list (`make_edit_lists`), closed until it is clicked; open, it lists that kind's items, by name,
# and + Add. The details show the item picked: what it belongs to as tags that open it, what its own form changes
# under an Edit button, and what uses it, each with View. Merge, Empty and Delete are along the bottom; Edit and
# + Add show the form there, with Save.
make_edit_ui <- function(id) {
  ns <- NS(id)
  as_fill_carrier(div(
    id = ns("main"),
    class = "page edit-page is-wide",
    `data-ns` = ns(""),
    page_header(
      "Metadata", "Pick a list, then an item, to see it. Nothing changes until you press Edit and then Save.",
      eyebrow = "Database", icon_name = "metadata"
    ),
    card(
      class = "structure-card",
      card_body(
        class = "structure-body", padding = 0,
        div(
          class = "outline-panel", `data-ns` = ns(""),
          div(
            class = "outline-toolbar",
            outline_search("Search the lists that are open"),
            review_button(
              icon_label("collapse", "Collapse All"), "collapse_all",
              class = "btn btn-outline-dark", title = "Close every list", `data-collapse-all` = NA
            )
          ),
          div(
            class = "outline-split",
            div(
              class = "outline-body",
              div(class = "outline-tree", div(class = "outline-list", lapply(make_edit_listed, function(list_id) {
                make_edit_section(list_id, make_edit_lists[[list_id]]$label, uiOutput(ns(paste0("items_", list_id)), class = "outline-list"))
              }))),
              p(class = "outline-filter-empty", "Nothing in the open lists matches.")
            ),
            outline_divider(),
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

# The lists, in the order the page has them: what each is called, one of its items (for + Add), and the kind of
# thing it lists (`kind`, a name of `edit_entities`).
# Three of them list printed wordings (the kind "component": each wording once, however many questionnaires print
# it), told apart by the kinds of block they hold (`blocks`). Staff look for a question or a section, not for a
# "component", and never needed the page's old list of everything one questionnaire prints ("Blocks"): a wording's
# details have a card for each place that prints it, which opens it as it is there. + Add in those three adds one
# to a questionnaire (`adds`: the kind "placement"), since a wording nothing prints is nothing. A measurement
# scale's questions are added and removed in the scale's details, so they have no list either. Nor have PDFs: the
# files a wave was extracted from are named, and opened, in the wave's details.
make_edit_lists <- list(
  registry = list(label = "Registries", one = "Registry", kind = "registry"),
  study = list(label = "Studies", one = "Study", kind = "study"),
  publication = list(label = "Publications", one = "Publication", kind = "publication"),
  questionnaire = list(label = "Questionnaires", one = "Questionnaire", kind = "questionnaire"),
  wave = list(label = "Waves", one = "Wave", kind = "wave"),
  section = list(
    label = "Sections, Grids & Groups", one = "Section, Grid or Group", kind = "component", adds = "placement",
    blocks = c("Section", "Grid", "Group")
  ),
  question = list(label = "Questions", one = "Question", kind = "component", adds = "placement", blocks = "Question"),
  note = list(
    label = "Instructions & Notes", one = "Instruction or Note", kind = "component", adds = "placement",
    blocks = c("Instruction", "Introduction", "Definition", "Note", "Interviewer Script")
  ),
  investigator = list(label = "Investigators", one = "Investigator", kind = "investigator"),
  respondent_type = list(label = "Respondent Types", one = "Respondent Type", kind = "respondent_type"),
  tag = list(label = "Tags", one = "Tag", kind = "tag"),
  answer_option = list(label = "Answer Choices", one = "Answer Choice", kind = "answer_option"),
  measurement_scale = list(label = "Measurement Scales", one = "Measurement Scale", kind = "measurement_scale")
)
make_edit_listed <- names(make_edit_lists)

# What one of a kind is called, for the kinds a list doesn't name: one questionnaire's printing of a wording, and a
# question's place in a scale. (A wording is called by its kind of block: `make_edit_called()`.)
make_edit_kinds <- c(
  unlist(lapply(unname(Filter(function(list) is.null(list$blocks), make_edit_lists)), function(list) stats::setNames(list$one, list$kind))),
  component = "Wording", placement = "Block", question_scale = "Scale Question", pdf_file = "PDF File"
)

# What an item is called over its details: its kind, and for a wording, or one questionnaire's printing of one,
# the kind of block it is ("Question", "Section").
# key: The kind, a name of `edit_entities`.
# row: The item, or `NULL` for one being added.
make_edit_called <- function(key, snap = NULL, row = NULL) {
  block <- if (key == "component") row$kind else if (key == "placement" && !is.null(row)) edit_lookup(snap, "component", "component_id", row$component_id, "kind")
  if (has_text(block)) block else make_edit_kinds[[key]]
}

# A list's items as it shows them, in alphabetical order.
# list_id: The list, a name of `make_edit_lists`.
# Returns: Cards, from `edit_service_cards()`, with `scope` too (none of the kinds listed belongs to a
#   questionnaire or a scale, so it is "").
make_edit_cards <- function(list_id, snap) {
  list <- make_edit_lists[[list_id]]
  cards <- edit_service_cards(edit_entities[[list$kind]], snap)
  if (!is.null(list$blocks)) cards <- cards[cards$tag %in% list$blocks, , drop = FALSE]
  cards$scope <- rep("", nrow(cards))
  cards[order(tolower(cards$title), cards$subtitle, method = "radix"), , drop = FALSE]
}

# A section of the list: its row, which opens and closes it (the page script does it at once), and what is
# inside it, drawn only once it is opened (Shiny doesn't draw what is hidden).
# id: The list, a name of `make_edit_lists`.
# label: What the row says.
# inside: What is in it.
# open: Whether it is drawn open.
make_edit_section <- function(id, label, inside, open = FALSE) {
  div(
    class = c("outline-node", if (!open) "is-collapsed"), `data-id` = id,
    div(
      class = "outline-row is-container", tabindex = "0", `data-action` = "toggle", `data-id` = id,
      `aria-expanded` = if (open) "true" else "false",
      span(class = "outline-text", label)
    ),
    inside
  )
}

# One item of a list: its kind of block (in a list that holds more than one kind), its name with a line about it,
# and the arrow that opens it in the details. Clicking it opens it there too.
# kind: The kind, a name of `edit_entities`.
# card: One of the kind's cards, from `edit_service_cards()`.
# scope: The questionnaire's (or scale's) key, for a kind that belongs to one; otherwise "".
# selected: Whether it is the one in the details.
# chip: Whether to say its kind of block: not where every item is the same kind (Questions).
make_edit_item <- function(kind, card, scope = "", selected = FALSE, chip = TRUE) {
  id <- paste(kind, scope, card$key, sep = "|")
  picks <- list(`data-id` = id, `data-kind` = kind, `data-key` = card$key, `data-scope` = scope)
  div(
    class = "outline-node", `data-id` = id,
    div(
      class = c("outline-row", "edit-item", if (selected) "is-selected"), tabindex = "0", `data-action` = "select", !!!picks,
      if (chip && !is.na(card$tag)) type_chip(card$tag),
      span(class = "outline-text", card$title, if (nzchar(card$subtitle)) span(class = "edit-item-meta", card$subtitle)),
      do.call(review_button, c(
        list(app_icon("open"), "select", class = "btn btn-sm btn-icon btn-open", title = "Open", `aria-label` = paste("Open", card$title)),
        picks
      ))
    )
  )
}

# + Add, at the end of a list. In a list of wordings it adds one to a questionnaire, and says which list it is in,
# so the form starts as that kind of block.
# list_id: The list, a name of `make_edit_lists`.
make_edit_add_button <- function(list_id) {
  list <- make_edit_lists[[list_id]]
  review_button(
    icon_label("add", paste("Add", list$one)), "add",
    `data-kind` = list$adds %||% list$kind, `data-list` = list_id, class = "btn btn-outline-dark btn-wide edit-add"
  )
}

# ------ Re-extract a wave ---------------------------------------------------------------------------------
# Re-extract on a wave's details: the wave is emptied and a new extraction of it is read from the PDF it was
# extracted from, with the latest extraction instructions (`extraction_service_reextract_wave()`). Off, saying
# why, for a wave that wasn't extracted from a PDF on this site.
# row: The wave.
wave_reextract_button <- function(row) {
  has_pdf <- has_text(row$pdf_sha256)
  review_button(
    icon_label("reextract", "Re-extract"), "reextract_wave",
    class = "btn btn-outline-dark", disabled = !has_pdf,
    title = if (has_pdf) {
      "Empty the wave and read its PDF again with the latest extraction instructions, as a new extraction"
    } else {
      "This wave wasn't extracted from a PDF on this site, so there is nothing to read again"
    }
  )
}

# Edit extraction on a wave's details: what the wave asks is opened on the Edit Extraction page as a draft
# (`extraction_service_reopen_wave()`), to be changed there and pushed again in place of what the wave asks now.
# Nothing in the database changes until that push. Off, saying why, for a wave that asks nothing yet.
# asks: Whether the wave asks any question.
wave_edit_extraction_button <- function(asks) {
  review_button(
    icon_label("edit", "Edit extraction"), "edit_wave_extraction",
    class = "btn btn-outline-dark", disabled = !asks,
    title = if (asks) {
      "Open what this wave asks on Edit Extraction, to change it and push it again. Nothing changes until it is pushed"
    } else {
      "This wave asks no questions yet, so there is nothing to edit. Start a New Extraction for it instead"
    }
  )
}

# What Re-extract asks first: the wave is emptied, and where the new extraction goes.
# wave: What the wave is called ("AHP Wave 3").
reextract_wave_dialog <- function(ns, wave) {
  confirm_dialog(
    paste0("Re-extract ", wave, "?"),
    p(strong("The wave is emptied first:"), " its questions go (and, from the questionnaire, any question no other wave asks, with its variables). The wave itself stays, with its PDF."),
    p("Then a new AI-Powered extraction is started from that PDF (and its Variable Names and Coding, if it had them), read with the latest extraction instructions. It goes into this wave when it is pushed, after review on Edit Extraction."),
    confirm_id = ns("confirm_reextract_wave"), confirm_label = "Empty and re-extract", keep_label = "Cancel"
  )
}
