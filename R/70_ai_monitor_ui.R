# The AI Monitor page: how often staff correct what the AI read from a questionnaire's PDF, counted on the
# extractions that were pushed (service_monitor.R). Its headline numbers, a chart of the corrections over time,
# the prompts that read them, and the kinds of correction, the commonest first, each with the corrections that
# come up again and again.
ai_monitor_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("main"),
    class = "page monitor-page",
    `data-ns` = ns(""),
    page_header(
      "AI Monitor",
      "How often staff correct the AI's reading of a questionnaire, counted on the extractions that were pushed.",
      eyebrow = "Extraction quality", icon_name = "ai_monitor"
    ),
    uiOutput(ns("totals"), class = "monitor-totals"),
    div(
      class = "monitor-panels",
      tags$section(
        class = "monitor-panel is-chart",
        h2(class = "monitor-panel-title", "Corrections over time"),
        p(class = "monitor-panel-text", "Each bar adds up the corrections in the extractions pushed then."),
        uiOutput(ns("chart"))
      ),
      tags$section(
        class = "monitor-panel",
        h2(class = "monitor-panel-title", "By prompt"),
        p(class = "monitor-panel-text", "Each version of the extraction prompt, newest first: fewer corrections per extraction means it reads better."),
        uiOutput(ns("by_prompt"))
      )
    ),
    div(
      class = "monitor-section-head",
      div(
        h2(class = "section-title", "Most common corrections"),
        p(class = "section-text", "What staff changed in the AI's reading, the commonest kind first. Under each, the same change made again and again.")
      ),
      toggle_input(
        ns("item"), c("All" = "all", "Blocks" = "block", "Variables" = "variable", "Choices" = "choice"),
        selected = "all", label = "What was corrected"
      )
    ),
    uiOutput(ns("common"))
  )
}

# ------ The headline numbers ------------------------------------------------------------------------
# totals: From `monitor_service_totals()`.
monitor_totals_ui <- function(totals) {
  stat <- function(value, label) div(class = "monitor-stat", span(class = "monitor-stat-value", value), span(class = "monitor-stat-label", label))
  tagList(
    stat(format(totals$extractions, big.mark = ","), "Pushed AI extractions"),
    stat(format(totals$corrections, big.mark = ","), "Corrections"),
    stat(if (is.na(totals$per_extraction)) "–" else format(round(totals$per_extraction, 1), nsmall = 1), "Corrections per extraction"),
    stat(format(totals$untouched, big.mark = ","), "Pushed as the AI read them")
  )
}

# ------ The chart ---------------------------------------------------------------------------------
# A bar for each period, drawn as SVG (its colours are _monitor.scss's), with what each bar counts when the pointer
# is over it.
# series: From `monitor_service_over_time()`.
monitor_chart_ui <- function(series) {
  if (nrow(series) == 0) {
    return(div(class = "monitor-empty", "Nothing has been pushed yet, so there is nothing to chart."))
  }
  width <- 720
  height <- 260
  left <- 44
  right <- 8
  top <- 16
  bottom <- 36
  plot_width <- width - left - right
  plot_height <- height - top - bottom
  ticks <- pretty(c(0, max(series$corrections, 1)), n = 4)
  most <- max(ticks)
  y <- function(value) top + plot_height * (1 - value / most)
  band <- plot_width / nrow(series)
  bar <- min(band * 0.7, 56)
  # every label while they fit, otherwise every second, third... one
  every <- max(1, ceiling(nrow(series) / 12))
  said <- monitor_period_text(series)

  svg_tag <- function(name, ...) htmltools::tag(name, list(...))
  grid <- lapply(ticks, function(tick) {
    tagList(
      svg_tag("line", class = "monitor-grid-line", x1 = left, x2 = width - right, y1 = y(tick), y2 = y(tick)),
      svg_tag("text", class = "monitor-axis-label is-y", x = left - 6, y = y(tick), format(tick, big.mark = ","))
    )
  })
  bars <- lapply(seq_len(nrow(series)), function(i) {
    middle <- left + band * (i - 0.5)
    n <- series$corrections[i]
    tagList(
      svg_tag(
        "rect",
        class = c("monitor-bar", if (n == 0) "is-zero"),
        x = middle - bar / 2, y = y(n), width = bar, height = max(y(0) - y(n), 0), rx = 3,
        svg_tag("title", said[i])
      ),
      if (n > 0 && nrow(series) <= 16) svg_tag("text", class = "monitor-bar-value", x = middle, y = y(n) - 5, format(n, big.mark = ",")),
      if ((i - 1) %% every == 0) svg_tag("text", class = "monitor-axis-label is-x", x = middle, y = height - bottom + 18, series$label[i])
    )
  })
  svg_tag(
    "svg",
    class = "monitor-chart", viewBox = paste(0, 0, width, height), role = "img",
    `aria-label` = paste("Corrections by", attr(series, "unit") %||% "day", "for pushed extractions.", paste(said, collapse = "; ")),
    grid,
    svg_tag("line", class = "monitor-axis", x1 = left, x2 = width - right, y1 = y(0), y2 = y(0)),
    bars
  )
}

# ------ By prompt ---------------------------------------------------------------------------------
# rows: From `monitor_service_by_prompt()`.
monitor_by_prompt_ui <- function(rows) {
  if (nrow(rows) == 0) {
    return(div(class = "monitor-empty", "No prompt has read a pushed extraction yet."))
  }
  tags$table(
    class = "table monitor-table",
    tags$thead(tags$tr(tags$th("Prompt"), tags$th(class = "is-count", "Extractions"), tags$th(class = "is-count", "Per extraction"))),
    tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      note <- c(if (nzchar(row$model)) row$model, paste("last pushed", day_phrase(row$last_applied)))
      tags$tr(
        tags$td(tags$code(row$prompt_version), div(class = "monitor-table-note", paste(note, collapse = " · "))),
        tags$td(class = "is-count", row$extractions),
        tags$td(class = "is-count", format(round(row$per_extraction, 1), nsmall = 1))
      )
    }))
  )
}

# ------ The commonest corrections -----------------------------------------------------------------
# kinds: From `monitor_service_common()`.
# open: How many of the commonest kinds start open.
monitor_common_ui <- function(kinds, open = 3) {
  if (length(kinds) == 0) {
    return(div(class = "list-card is-empty", p("No pushed extraction has a correction of that kind yet.")))
  }
  lapply(seq_along(kinds), function(i) monitor_kind_ui(kinds[[i]], open = i <= open))
}

# One kind of correction: its name and how often it comes up, then its repeated corrections.
monitor_kind_ui <- function(kind, open = FALSE) {
  tags$details(
    class = "monitor-kind", open = if (open) NA,
    tags$summary(
      class = "monitor-kind-head",
      span(class = "monitor-kind-label", kind$label),
      row_badge(plural(kind$count, "correction"), class = paste0("is-", kind$change)),
      span(class = "monitor-kind-reach", sprintf("in %s (%d%% of pushed)", plural(kind$extractions, "extraction"), round(100 * kind$share)))
    ),
    div(class = "monitor-examples", lapply(seq_len(nrow(kind$examples)), function(i) monitor_example_ui(kind, kind$examples[i, ])))
  )
}

# A correction as it was made: what the AI wrote, what staff made it, how often, and where first.
monitor_example_ui <- function(kind, example) {
  value <- function(text, class) {
    if (is.na(text)) span(class = c("monitor-value", class, "is-none"), "(nothing)") else span(class = c("monitor-value", class), text)
  }
  div(
    class = "monitor-example",
    div(
      class = "monitor-example-body",
      div(
        class = "monitor-example-change",
        if (identical(kind$change, "added")) {
          value(example$after, "is-after")
        } else if (identical(kind$change, "deleted")) {
          value(example$before, "is-before")
        } else {
          tagList(value(example$before, "is-before"), span(class = "monitor-arrow", `aria-label` = "changed to", "→"), value(example$after, "is-after"))
        }
      ),
      p(
        class = "list-card-meta",
        paste0(
          if (example$count > 1) paste0(plural(example$count, "time"), " in ", plural(example$extractions, "extraction"), " · first at ") else "At ",
          example$where, " in ", example$extraction
        )
      )
    )
  )
}
