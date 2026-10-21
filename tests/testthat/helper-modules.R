# What the module tests share. A module is tested with shiny::testServer(): no browser,
# so what a module asks the browser to do (a notification, a dialog, new choices for a
# drop-down) is recorded by local_browser() instead of done. The database is the real one
# (helper-00_realdb.R).

# The live tables as server.R shares them: `get()` reads them (here: the real database's) and
# `refresh()` reads them again.
test_live_catalog <- function(connection = test_connection) {
  tables <- shiny::reactiveVal(questionnaire_service_get_catalog(connection))
  list(get = tables, refresh = function() tables(questionnaire_service_get_catalog(connection)))
}

# The same for a catalog that is given (the sample tables), for a page that never touches the database.
fixed_catalog <- function(catalog = test_catalog()) {
  list(get = shiny::reactiveVal(catalog), refresh = function() invisible())
}

# The extraction drafts as server.R shares them, for a page that never touches the database: `records` are given
# (by id), and `changed()` does nothing.
fixed_extractions <- function(records = list()) {
  list(all = shiny::reactive(records), changed = function() invisible())
}

# The extraction drafts as server.R shares them: `all()` reads them from the database
# again each time `changed()` has been called.
test_extractions <- function(connection = test_connection) {
  written <- shiny::reactiveVal(0L)
  list(
    all = shiny::reactive({
      written()
      extraction_service_get_all(connection)
    }),
    changed = function() written(written() + 1L)
  )
}

# The text of a rendered output, a dialog or a notification: tags and extra spaces gone.
text_of <- function(x) {
  html <- if (is.list(x) && !inherits(x, c("shiny.tag", "shiny.tag.list"))) x$html else as.character(x)
  html <- gsub("<[^>]+>", " ", paste(html, collapse = " "))
  html <- gsub("&amp;", "&", gsub("&lt;", "<", gsub("&gt;", ">", gsub("&quot;", "\"", gsub("&#39;|&#x27;", "'", html)))))
  trimws(gsub("\\s+", " ", html))
}

# Records what modules ask the browser to do, for the rest of the test. Returns an
# environment with `notifications` (text and type), `modals` (their text), `closed`
# (how many times a dialog was closed), `pages` (the pages the menu was told to show),
# `updates` (the update*Input and updateActionButton calls, each a list of `fn`,
# `id` and the arguments given), `inserted` (what insertUI() added: `selector`, `where` and
# the `html`) and `removed` (the selectors removeUI() was given).
local_browser <- function(env = parent.frame()) {
  seen <- new.env()
  seen$notifications <- list()
  seen$modals <- list()
  seen$closed <- 0L
  seen$updates <- list()
  seen$pages <- character()
  seen$inserted <- list()
  seen$removed <- character()
  record_update <- function(fn) {
    force(fn)
    function(session, inputId, ...) { # nolint: object_name_linter. (the name shiny's update functions give it)
      seen$updates[[length(seen$updates) + 1]] <- c(list(fn = fn, id = inputId), list(...))
      invisible()
    }
  }
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "default") {
      seen$notifications[[length(seen$notifications) + 1]] <- list(text = text_of(ui), type = type)
      invisible("notification")
    },
    showModal = function(ui, ...) {
      seen$modals[[length(seen$modals) + 1]] <- text_of(ui)
      invisible()
    },
    removeModal = function(...) {
      seen$closed <- seen$closed + 1L
      invisible()
    },
    updateSelectInput = record_update("select"),
    updateRadioButtons = record_update("radio"),
    updateTextInput = record_update("text"),
    updateTextAreaInput = record_update("textarea"),
    updateActionButton = record_update("button"),
    insertUI = function(selector, where = "beforeEnd", ui, ...) {
      seen$inserted[[length(seen$inserted) + 1]] <- list(selector = selector, where = where, html = as.character(ui))
      invisible()
    },
    removeUI = function(selector, ...) {
      seen$removed <- c(seen$removed, selector)
      invisible()
    },
    .package = "shiny",
    .env = env
  )
  testthat::local_mocked_bindings(
    nav_select = function(id, selected = NULL, session = NULL) {
      seen$pages <- c(seen$pages, selected)
      invisible()
    },
    .package = "bslib",
    .env = env
  )
  seen
}

# The last update sent to an input, or NULL if none was.
last_update <- function(seen, id) {
  matching <- Filter(function(update) identical(update$id, id), seen$updates)
  if (length(matching) > 0) matching[[length(matching)]]
}

# The text of the notifications so far, one string each.
notice_texts <- function(seen) vapply(seen$notifications, function(note) note$text, character(1))

# An error in an observer only prints a warning in a test session. This collects them,
# so a test can say there were none (or which there were).
collect_errors <- function(session) {
  errors <- character()
  session$onUnhandledError(function(problem) errors <<- c(errors, conditionMessage(problem)))
  function() errors
}

# What the Edit Extraction page shares with its outline and details modules, kept simple: the
# draft and the selected block are reactive values, and every change goes through
# `change`, which like the page's own keeps the sentences a failed change would show staff
# (in `log$errors`) and says whether the text was typed (`log$typed`).
edit_extraction_state <- function(draft = gold_draft(), lookups = test_lookups()) {
  current <- shiny::reactiveVal(draft)
  selected <- shiny::reactiveVal(NULL)
  log <- new.env()
  log$typed <- logical()
  log$errors <- character()
  layout <- shiny::reactiveVal(0L)
  state <- list(
    draft = shiny::reactive(current()),
    change = function(new_draft, typed = FALSE, redraw = !typed) {
      result <- tryCatch(new_draft, error = function(problem) {
        log$errors <- c(log$errors, conditionMessage(problem))
        NULL
      })
      if (is.null(result)) {
        return(FALSE)
      }
      log$typed <- c(log$typed, typed)
      current(draft_service_uncheck_edited(current(), result))
      if (redraw) layout(layout() + 1L) # as the page does: a pick or a button redraws, typing doesn't
      TRUE
    },
    selected = selected,
    problems = shiny::reactive(draft_service_check(current(), lookups)),
    layout_version = layout
  )
  list(state = state, draft = current, selected = selected, log = log, lookups = lookups)
}

# server.R as a function that talks to `connection`: it is evaluated where R/'s functions
# are (global.R's objects are in the environment above), with `database` replaced.
site_server <- function(connection = test_connection, env = parent.frame()) {
  support <- env
  while (!exists("staff_service_current", envir = support, inherits = FALSE)) support <- parent.env(support)
  scope <- new.env(parent = support)
  scope$database <- connection
  eval(parse(app_file("server.R"), keep.source = FALSE, encoding = "UTF-8"), scope)
}

# Replaces one of the app's functions until the end of the test. The app's functions are in
# an ordinary environment, not a package, so testthat's local_mocked_bindings() can't do it.
# Used for the few that start processes or reach outside, which a test must not do.
local_stub <- function(name, stub, env = parent.frame()) {
  where <- env
  while (!exists(name, envir = where, inherits = FALSE)) where <- parent.env(where)
  original <- get(name, envir = where)
  assign(name, stub, envir = where)
  withr::defer(assign(name, original, envir = where), envir = env)
  invisible(original)
}
