# What the Make an edit page's tests share (test-make_edit_server*.R).

# The browser draws the form and sends back its token; here the form is drawn by reading its output, and
# the token sent back by hand.
draw_form <- function(env = parent.frame()) {
  with(env, {
    output[["change-main"]]
    session$setInputs(`change-form_token` = as.character(change$token()))
  })
}

page_args <- function(me) {
  pages <- character()
  list(
    args = list(connection = test_connection, me = me, show_page = function(page) pages <<- c(pages, page)),
    pages = function() pages
  )
}

# What the page applied is removed when the test ends: the rows the applied changes added, in one request for each
# table. (The drafts it saved go with the drafts of the test's staff.)
local_outcomes <- function(env = parent.frame()) {
  made <- new.env()
  made$applied <- character()
  withr::defer(test_remove_applied(made$applied), envir = env)
  made
}

# What the list's buttons send (www/js/review.js): picking an item, + Add, and the details' Edit and Delete.
pick <- function(session, kind, key, scope = "") {
  session$setInputs(action = list(action = "select", kind = kind, key = as.character(key), scope = scope))
}
add <- function(session, kind, scope = "") session$setInputs(action = list(action = "add", kind = kind, scope = scope))
press <- function(session, what) session$setInputs(action = list(action = what))
# Opening (or closing) a section of the list: only an open section's items are drawn.
open_section <- function(session, id) session$setInputs(action = list(action = "toggle", id = as.character(id)))
