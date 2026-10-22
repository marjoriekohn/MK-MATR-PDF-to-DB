# The site's stylesheet (www/sass), compiled as the site compiles it: every button and field is one height on
# every page (defaults.scss), the kinds of button are drawn in one place (components/_buttons.scss), and no
# page has a copy of its own.

site_css <- function() cached_fixture("site_css", function() as.character(sass::sass(vcu_theme)))

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
  pages <- list.files(test_path("..", "..", "www", "sass", "layout"), full.names = TRUE)
  for (page in pages) {
    text <- paste(readLines(page, warn = FALSE), collapse = "\n")
    expect_false(grepl("(^|\\n)\\.btn-(primary|outline-dark|delete|icon|link|wide)\\s*[{,]", text, perl = TRUE), info = basename(page))
    expect_false(grepl("same-height-button", text, fixed = TRUE), info = basename(page))
  }
})

test_that("a row of buttons that share it evenly is a grid of equal columns, and so are a phone's toggles", {
  css <- site_css()
  for (row in c(".detail-actions", ".review-buttons", ".toggle-buttons")) {
    expect_match(css_rule(css, row), "grid-auto-columns:\\s*minmax\\(0,\\s*1fr\\)", info = row)
  }
})
