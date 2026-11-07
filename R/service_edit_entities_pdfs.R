# Make an edit's PDFs (group Extractions), which are only shown. The kinds are listed together in edit_entities
# (service_edit_kinds.R).

# ------ PDFs ---------------------------------------------------------------------------------------------
# What PDFs were uploaded as.
# A PDF is kept once by its SHA-256, however many times it is uploaded and under whatever names.
# hashes: PDFs' SHA-256s.
# Returns: One string per PDF: its names joined by " / ", or "PDF" and the start of its hash if no draft or wave
#   names it.
edit_pdf_names <- function(snap, hashes) {
  drafts <- snap$extracted_draft
  waves <- snap$wave_file
  named <- c(
    stats::setNames(drafts$pdf_name, drafts$pdf_sha256),
    if (!is.null(waves)) stats::setNames(c(waves$pdf_name, waves$supplement_name), c(waves$pdf_sha256, waves$supplement_sha256))
  )
  named <- named[!is.na(names(named)) & !is.na(named) & nzchar(named)]
  vapply(hashes, function(hash) {
    names <- unique(unname(named[names(named) %in% hash]))
    if (length(names) == 0) paste("PDF", substr(hash, 1, 12)) else paste(sort(names), collapse = " / ")
  }, character(1), USE.NAMES = FALSE)
}

# "12 pages · 2.1 MB", leaving out what isn't known.
edit_pdf_facts <- function(pages, bytes) {
  vapply(seq_along(pages), function(i) {
    paste(c(if (!is.na(pages[i])) plural(pages[i], "page"), if (!is.na(bytes[i])) file_size(bytes[i])), collapse = " · ")
  }, character(1))
}

pdf_file_entity <- list(
  label = "PDF file", noun = "PDF", plural = "PDFs", article = "a",
  group = "Extractions", table = "pdf_file", view_only = TRUE,
  about = paste0(
    "A questionnaire's PDF, uploaded for an extraction and kept once however many times it's uploaded. It is only ",
    "shown here: it's the record of what extractions were read from, so it isn't changed or deleted, and new ones come ",
    "in on the Extraction page."
  ),
  reads = c("extracted_draft", "draft", "wave_file"),
  can = function(snap, mode) {
    switch(mode,
      add = "PDFs come in when they're uploaded for an extraction, on the Extraction page.",
      update = "A PDF's pages and size are facts about the file, so nothing about it can change.",
      delete = "A PDF is the record of what extractions were read from, so it's kept."
    )
  },
  cards = function(snap, scope) {
    names <- edit_pdf_names(snap, snap$pdf_file$pdf_sha256)
    by_name <- order(tolower(names), method = "radix")
    rows <- snap$pdf_file[by_name, , drop = FALSE]
    used <- count_in(snap$extracted_draft$pdf_sha256, rows$pdf_sha256)
    edit_cards(
      rows$pdf_sha256, names[by_name],
      subtitle = edit_pdf_facts(rows$pdf_pages, rows$pdf_bytes),
      facts = lapply(used, function(n) if (n > 0) paste("read for", edit_count(n, "extraction")) else "read for no extractions")
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("pdf_name", "Uploaded as"),
      edit_field("pdf_pages", "Pages"),
      edit_field("pdf_bytes", "Size"),
      edit_field("pdf_sha256", "SHA-256", hint = "Its fingerprint: the same file always has the same one.")
    )
  },
  initial = function(snap, row, scope) {
    list(
      pdf_name = edit_pdf_names(snap, row$pdf_sha256),
      pdf_pages = if (is.na(row$pdf_pages)) "" else key_text(row$pdf_pages),
      pdf_bytes = if (is.na(row$pdf_bytes)) "" else file_size(row$pdf_bytes)
    )
  }
)
