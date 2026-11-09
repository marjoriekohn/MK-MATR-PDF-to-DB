# The Metadata page (the module is called make_edit).
# Everything staff can add, change, empty or delete in the MATR metadata database, laid out as Edit Extraction
# lays out a questionnaire: an outline on the left and the item picked on the right. Each kind of thing is a
# section of the outline, closed until it is clicked; open, it lists that kind's items, by name, and + Add. Blocks
# have a section for each questionnaire, and a scale's items one for each measurement scale. The details show the
# item picked, with Edit and Delete; Edit and + Add show its form, with Save.
make_edit_ui <- function(id) {
  ns <- NS(id)
  as_fill_carrier(div(
    id = ns("main"),
    class = "page edit-page is-wide",
    `data-ns` = ns(""),
    page_header("Metadata", "View, add, change or delete what is in the MATR metadata database.", eyebrow = "Database", icon_name = "metadata"),
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
              class = "btn btn-outline-dark", title = "Close every section", `data-collapse-all` = NA
            )
          ),
          div(
            class = "outline-split",
            div(
              class = "outline-body",
              div(class = "outline-tree", div(class = "outline-list", lapply(make_edit_listed, function(kind) {
                make_edit_section(kind, make_edit_kinds[[kind]][1], uiOutput(ns(paste0("items_", kind)), class = "outline-list"))
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

# The kinds of thing, in the order the list has them: what the list calls them, and one of them (for + Add).
# Blocks are as printed in one questionnaire, a section for each; Components are each wording once, however many
# questionnaires print it (its details have a card for each, which opens it as it is in that questionnaire alone).
# Scale Items are as listed in one measurement scale. PDF Files are only shown.
make_edit_kinds <- list(
  registry = c("Registries", "Registry"),
  study = c("Studies", "Study"),
  questionnaire = c("Questionnaires", "Questionnaire"),
  wave = c("Waves", "Wave"),
  placement = c("Blocks", "Block"),
  component = c("Components", "Component"),
  investigator = c("Principal Investigators", "Principal Investigator"),
  respondent_type = c("Respondent Types", "Respondent Type"),
  tag = c("Tags", "Tag"),
  answer_option = c("Answer Choices", "Answer Choice"),
  measurement_scale = c("Measurement Scales", "Measurement Scale"),
  question_scale = c("Scale Items", "Scale Item"),
  pdf_file = c("PDF Files", "PDF File")
)

# The kinds the list has a section for: all of them.
make_edit_listed <- names(make_edit_kinds)

# A kind's items as the list shows them: in alphabetical order, but blocks in printed order.
# key: The kind, a name of `edit_entities`.
# scope: The questionnaire's (or scale's) key, for a kind that has a section for each; otherwise "".
# Returns: Cards, from `edit_service_cards()`, with `scope` too.
make_edit_cards <- function(key, snap, scope = "") {
  entity <- edit_entities[[key]]
  cards <- edit_service_cards(entity, snap, if (nzchar(scope)) scope)
  cards$scope <- rep(scope, nrow(cards))
  if (key == "placement") {
    return(cards)
  }
  cards[order(tolower(cards$title), cards$subtitle, method = "radix"), , drop = FALSE]
}

# A section of the list: its row, which opens and closes it (the page script does it at once), and what is
# inside it, drawn only once it is opened (Shiny doesn't draw what is hidden).
# id: The section's id: a kind, or a kind and a questionnaire or scale ("placement:12").
# label: What the row says.
# inside: What is in it.
# open: Whether it is drawn open: a section inside another is drawn again with it, as staff left it.
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

# One item of a kind, in its section: its kind of block (for a block), its name, and the arrow that opens it in
# the details. Clicking it opens it there too.
# kind: The kind, a name of `edit_entities`.
# card: One of the kind's cards, from `edit_service_cards()`.
# scope: The questionnaire's (or scale's) key, for a kind that belongs to one; otherwise "".
# selected: Whether it is the one in the details.
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

# + Add, at the end of a kind's section (or of a questionnaire's, for a kind that belongs to one).
# kind: The kind, a name of `edit_entities`.
# scope: The questionnaire's key, or "".
make_edit_add_button <- function(kind, scope = "") {
  review_button(
    icon_label("add", paste("Add", make_edit_kinds[[kind]][2])), "add",
    `data-kind` = kind, `data-scope` = scope, class = "btn btn-outline-dark btn-wide edit-add"
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
