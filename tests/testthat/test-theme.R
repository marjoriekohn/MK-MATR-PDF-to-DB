# The site's stylesheet (www/sass), compiled as the site compiles it: every button and field is one height on
# every page (defaults.scss), the kinds of button and the list cards are drawn in one place (components/), the
# lines, breakpoints and the page's room from the edge are defaults.scss's, and no page has a copy of its own.

# Compiled here, not taken from sass's cache, so the tests see the files as they are now.
site_css <- function() cached_fixture("site_css", function() as.character(sass::sass(vcu_theme, cache = NULL)))

# Each page's and component's stylesheet, as written, named by its file.
sass_sources <- function() {
  files <- list.files(test_path("..", "..", "www", "sass"), pattern = "\\.scss$", recursive = TRUE, full.names = TRUE)
  files <- files[grepl("/(layout|components)/", files)]
  stats::setNames(vapply(files, function(file) paste(readLines(file, warn = FALSE), collapse = "\n"), ""), basename(files))
}

# The declarations of every rule that names `selector` (on its own, or among others), media queries' too, as one
# string.
css_rule <- function(css, selector) {
  rules <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css))[[1]]
  selectors <- lapply(strsplit(sub("\\{.*", "", rules), ","), trimws)
  paste(sub("^[^{]*\\{", "", rules[vapply(selectors, function(names) selector %in% names, logical(1))]), collapse = "\n")
}

test_that("the stylesheet compiles, and buttons and fields are one height: 2.25rem, or 1.875rem when small", {
  css <- site_css()
  expect_match(css_rule(css, ".btn"), "min-height:\\s*2.25rem")
  expect_match(css_rule(css, ".btn-sm"), "min-height:\\s*1.875rem")
  expect_match(css_rule(css, ".selectize-input"), "min-height:\\s*2.25rem")
  # Bootstrap's own padding comes out at that height (bslib's "shiny" preset would give 2em on each side)
  expect_match(css_rule(css, ".btn"), "--bs-btn-padding-x:\\s*0.875rem")
  expect_match(css_rule(css, ".btn"), "--bs-btn-font-size:\\s*0.875rem")
  expect_false(grepl("--bs-btn-padding-x:\\s*2em", css))
  # one line of words, so neighbours keep one height
  expect_match(css_rule(css, ".btn"), "white-space:\\s*nowrap")
})

test_that("the kinds of button are drawn only in components/_buttons.scss, from Bootstrap's button variables", {
  css <- site_css()
  expect_match(css_rule(css, ".btn-primary"), "--bs-btn-bg:\\s*#FFB300", ignore.case = TRUE)
  expect_match(css_rule(css, ".btn-delete"), "--bs-btn-color:\\s*#B02E01", ignore.case = TRUE)
  # no page stylesheet defines a kind of button, or a height for buttons, of its own (placing one inside
  # something of the page's, e.g. `.edit-variable-row .btn-icon`, is fine)
  pages <- sass_sources()
  pages <- pages[!names(pages) %in% "_buttons.scss"]
  for (page in names(pages)) {
    expect_false(grepl("(^|\\n)\\.btn-(primary|outline-dark|delete|icon|link|wide)\\s*[{,]", pages[[page]], perl = TRUE), info = page)
    expect_false(grepl("same-height-button", pages[[page]], fixed = TRUE), info = page)
  }
})

test_that("a row of buttons that share it evenly is a grid of equal columns, and so are a phone's toggles", {
  css <- site_css()
  for (row in c(".detail-actions", ".review-buttons", ".toggle-buttons")) {
    expect_match(css_rule(css, row), "grid-auto-columns:\\s*minmax\\(0,\\s*1fr\\)", info = row)
  }
})

test_that("lines, breakpoints and the page's room from the screen's edge are the same on every page", {
  for (page in names(sass_sources())) {
    text <- sass_sources()[[page]]
    # the two line colours are defaults.scss's ($line, $line-light), not a page's own
    expect_false(grepl("\\$[a-z-]+-line\\s*:", text, perl = TRUE), info = page)
    # breakpoints are Bootstrap's (media-breakpoint-down(sm|md|lg)), not widths typed in
    expect_false(grepl("@media[^{]*(max|min)-width:\\s*[0-9.]+px", text, perl = TRUE), info = page)
  }
  css <- site_css()
  expect_match(css_rule(css, ".container-fluid"), "padding:\\s*1.5rem")
  # (Bootstrap's other containers come along with .container-fluid; the site uses only that one)
  expect_match(css, "@media \\(max-width: 575.98px\\)\\s*\\{\\s*\\.container-fluid[^{]*\\{\\s*padding:\\s*1rem")
})

test_that("the menu bar has the pages' links (not Home's or Edit Extraction's), folded into a list on a narrow window", {
  css <- site_css()
  expect_match(css, '\\[data-value="?home"?\\]', perl = TRUE)
  expect_match(css, '\\[data-value="?edit_extraction"?\\][^{]*\\{\\s*display:\\s*none', perl = TRUE)
  expect_match(css, "\\.navbar-collapse:is\\(\\.collapsing, \\.show\\) \\.navbar-nav \\{[^}]*flex-direction:\\s*column")
  expect_match(css_rule(css, ".navbar .navbar-brand"), "text-overflow:\\s*ellipsis")
})

test_that("drafts, edits and change requests are one kind of list card, and no page draws its own", {
  css <- site_css()
  expect_match(css_rule(css, ".list-card"), "border:\\s*1px solid")
  for (page in names(sass_sources())) {
    expect_false(grepl("\\.(draft-card|history-entry|change-request-card)\\b", sass_sources()[[page]], perl = TRUE), info = page)
  }
})

test_that("on a phone a bar of buttons puts its note above them, and never makes a button narrower than its words", {
  css <- site_css()
  for (bar in c(".modal-footer", ".list-card-actions", ".extraction-footer")) {
    expect_match(css_rule(css, paste(bar, "> .btn")), "min-width:\\s*max-content", info = bar)
    # (the note's selector has a comma of its own, so it is looked for whole)
    expect_match(css, paste0("\\", bar, " > :not\\(\\.btn, a\\) \\{[^}]*order:\\s*-1"), info = bar)
  }
})

test_that("Edit Extraction's four buttons under a block go two by two when the details beside the list are narrow", {
  expect_match(site_css(), "@container detail \\(max-width: 32rem\\)\\s*\\{\\s*\\.detail-actions\\s*\\{[^}]*grid-template-columns:\\s*repeat\\(2")
})

test_that("a section's title in the list is plain words, not a white box: its rule comes after the fields' own", {
  css <- site_css()
  boxes <- regexpr(".form-control, .form-select, .selectize-input {", css, fixed = TRUE)
  title <- regexpr("\\.outline-text \\{[^}]*background:\\s*transparent", css)
  expect_gt(boxes, 0)
  expect_gt(title, boxes)
})

test_that("a skip sentence that links to its question has the same size of words as one that doesn't", {
  expect_match(css_rule(site_css(), ".edit-extraction-page .skip-link"), "--bs-btn-font-size:\\s*inherit")
})

test_that("the stylesheet carries a fingerprint of every file in www/sass, so a cached copy is never one from before an edit", {
  # sass caches by the files global.R names; the ones they import are part of the stylesheet too
  expect_match(site_css(), "/\\* www/sass [0-9a-f]{32} \\*/")
})
