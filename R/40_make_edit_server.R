# State kept outside reactive values: `tables` (each kind's tables as read), `just_failed` (a read that failed as
# its section opened) and `open_sections` (whether each section is open), environments made by new.env() inside the
# module, so each session has its own and they go when it ends. Nothing else reads them. Shiny runs a session's
# code one piece at a time, so nothing changes them while something else is reading them; they aren't made to be
# shared between threads or sessions, and shouldn't be.

# The Metadata page's server (the module is called make_edit).
# The list on the left has a section for each list of things (`make_edit_lists`, 40_make_edit_ui.R); a list's items
# are read from the database the first time it is opened. Picking an item shows it in the details
# (`edit_change_server()`, "view"): Edit is at their top, and Merge, Empty and Delete along the bottom. A tag or a
# View in the details opens the thing it names, and Back returns from it (`trail`). Edit, Delete, Merge and + Add
# show the form, what a delete reaches, or what a merge moves, with the button that does it.
# Every change is applied at once, by the database's apply_change() (`edit_service_apply()`), and written to the
# Edit History. A registry, study, questionnaire or wave has Empty too: it stays, its contents go
# (`edit_service_empty()`). A wave extracted from a PDF has Re-extract: it is emptied and a new extraction of it
# is started from its PDF (`extraction_service_reextract_wave()`), shown with `open_reading`.
# active: A reactive that says whether this page is the one on screen, or `NULL`. Opening the page reads the lists
#   again, since other staff change them.
# open_reading: A function taking an extraction being read that shows how reading it is going (New Extraction's),
#   or `NULL` to go to New Extraction.
# open_draft: A function taking a draft's record that opens it on Edit Extraction, for a wave's Edit extraction
#   (`extraction_service_reopen_wave()`), or `NULL` to go to Extraction Drafts.
make_edit_server <- function(id, connection, me, show_page, active = NULL, open_reading = NULL, open_draft = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    kind <- reactiveVal("") # the kind of the item in the details, a name of edit_entities
    scope <- reactiveVal("") # the questionnaire (or measurement scale), for a kind that belongs to one
    picked <- reactiveVal(NULL) # the key of the item in the details
    verb <- reactiveVal("") # what the details show: "" (nothing), "view", "add", "update", "delete", "merge" or "done"
    entity <- reactive(if (kind() %in% names(edit_entities)) edit_entities[[kind()]])

    done <- reactiveVal(NULL) # what was just done: list(verb, summary, history_id, new_ids)
    duplicates <- reactiveVal(NULL) # the existing rows an add looks like, while staff decide
    leaving_to <- reactiveVal(NULL) # what was picked while a form was open, while staff decide whether to leave it
    add_list <- reactiveVal("") # the list whose + Add is open, a name of make_edit_lists: what the new thing is called
    trail <- reactiveVal(list()) # the items a tag or View was followed from, the last one last: Back returns to it
    entered <- reactiveVal("") # how the item in the details was opened: its verb then ("view", "add", "delete")

    # ------ The tables ---------------------------------------------------------
    # Each kind's tables are read the first time its section is opened (or one of its items is), and kept until a
    # change is made here or the page is opened again, which reads them again for every section that is open.
    tables <- new.env()
    just_failed <- new.env() # a read that failed as its section opened (pick_first()): what its list then shows
    tables_version <- reactiveVal(0L)
    tables_of <- function(key) {
      tables_version()
      if (exists(key, envir = just_failed, inherits = FALSE)) {
        problem <- get(key, envir = just_failed, inherits = FALSE)
        rm(list = key, envir = just_failed)
        return(problem)
      }
      if (!exists(key, envir = tables, inherits = FALSE)) {
        read <- withProgress(message = "Reading the database…", value = 0.5, {
          tryCatch(edit_service_load(connection, edit_entities[[key]]), error = function(problem) problem)
        })
        if (inherits(read, "error")) {
          return(read) # not kept, so opening it again tries again
        }
        assign(key, read, envir = tables)
      }
      get(key, envir = tables, inherits = FALSE)
    }
    read_again <- function() {
      rm(list = ls(tables), envir = tables)
      rm(list = ls(just_failed), envir = just_failed)
      tables_version(tables_version() + 1L)
    }

    snapshot <- reactive({
      req(entity())
      read <- tables_of(kind())
      if (!inherits(read, "error")) read
    })
    load_problem <- reactive({
      req(entity())
      read <- tables_of(kind())
      if (inherits(read, "error")) conditionMessage(read)
    })

    row <- reactive({
      req(entity(), snapshot(), picked())
      edit_service_row(entity(), snapshot(), picked())
    })

    # Opening the page reads the lists again, since other staff change them. Not while a form or a delete is open:
    # that would draw it again as it started, losing what was typed. They are once it is applied or closed.
    if (!is.null(active)) {
      observeEvent(active(),
        {
          if (isTRUE(active()) && !verb() %in% c("add", "update", "delete", "merge")) read_again()
        },
        ignoreInit = TRUE
      )
    }

    # What stops a row being changed, if anything: its kind can't be, or the row itself has nothing to change (the
    # one group, which has no words).
    cant_change <- function(shown) {
      cant("update") %||% if (is.function(entity()$unchangeable) && !is.null(shown)) entity()$unchangeable(snapshot(), shown)
    }

    # What stops a verb for the kind, if anything ("You can't delete a registry yet: …").
    cant <- function(mode) {
      snap <- snapshot()
      if (is.null(snap) || is.null(entity())) {
        return(NULL)
      }
      edit_service_can(entity(), snap, mode)
    }

    # ------ MODULES -----------------------------------------------------------
    # + Add in a list of wordings adds a block to a questionnaire: its form starts as the list's kind of block.
    add_starts <- reactive({
      blocks <- if (nzchar(add_list())) make_edit_lists[[add_list()]]$blocks
      if (identical(kind(), "placement") && identical(verb(), "add") && !is.null(blocks)) list(placement__kind = blocks[1])
    })
    change <- edit_change_server("change", entity, verb, snapshot, row, scope, prefill = add_starts)

    # ------ The list ------------------------------------------------------------
    # A list's items: drawn while it is open, and again when the tables are read again. Only what is open is drawn,
    # so only the kinds staff look at are read from the database. Whether a list is open is kept for each (the page
    # script opens and closes it at once, and says so), so opening one doesn't draw the others again.
    open_sections <- new.env()
    section_open <- function(id) {
      if (!exists(id, envir = open_sections, inherits = FALSE)) assign(id, reactiveVal(FALSE), envir = open_sections)
      get(id, envir = open_sections, inherits = FALSE)
    }

    lapply(make_edit_listed, function(list_id) {
      output[[paste0("items_", list_id)]] <- renderUI({
        req(section_open(list_id)())
        key <- make_edit_lists[[list_id]]$kind
        read <- tables_of(key)
        if (inherits(read, "error")) {
          return(div(
            class = "edit-list-note",
            p(conditionMessage(read)),
            review_button("Try again", "retry", `data-kind` = key, class = "btn btn-sm btn-outline-dark")
          ))
        }
        items_ui(list_id, read)
      })
    })

    items_ui <- function(list_id, snap) {
      list <- make_edit_lists[[list_id]]
      cards <- make_edit_cards(list_id, snap)
      chosen <- isolate(if (identical(kind(), list$kind)) picked())
      tagList(
        if (nrow(cards) == 0) p(class = "edit-list-note", paste0("No ", tolower(list$label), " yet.")),
        lapply(seq_len(nrow(cards)), function(i) {
          # every item of Questions is a question, so its rows don't each say so
          make_edit_item(list$kind, cards[i, , drop = FALSE], "", identical(cards$key[i], chosen), chip = length(list$blocks) != 1)
        }),
        if (!isTRUE(edit_entities[[list$kind]]$view_only)) make_edit_add_button(list_id)
      )
    }

    # Asks before what was typed is thrown away: leaving the edit for another item, or cancelling it.
    unsaved_dialog <- function(title, confirm_id, confirm_label) {
      confirm_dialog(
        title, "What you've typed isn't saved.",
        confirm_id = ns(confirm_id), confirm_label = confirm_label, keep_label = "Keep editing", easy_close = TRUE
      )
    }

    # ------ Picking ------------------------------------------------------------------
    observeEvent(input$action, {
      event <- input$action
      viewing <- identical(verb(), "view")
      switch(event$action,
        toggle = {
          is_open <- section_open(event$id)
          is_open(!is_open())
          if (is_open()) pick_first(event$id)
        },
        collapse_all = for (id in ls(open_sections)) section_open(id)(FALSE), # the page script closes them at once
        select = go_to(list(kind = event$kind, scope = event$scope %||% "", key = event$key, verb = "view")),
        add = go_to(list(kind = event$kind, scope = "", key = NULL, verb = "add", list = event$list %||% "")),
        edit = if (viewing && is.null(cant_change(tryCatch(row(), shiny.silent.error = function(problem) NULL)))) verb("update"),
        merge = if (viewing) verb("merge"),
        delete = if (viewing) verb("delete"),
        empty = ask_empty(),
        reextract_wave = if (identical(kind(), "wave")) showModal(reextract_wave_dialog(ns, edit_label(snapshot(), "wave", row()))),
        edit_wave_extraction = if (identical(kind(), "wave")) edit_wave_extraction(),
        back = after_done(),
        trail_back = go_back(),
        retry = read_again()
      )
    })

    # Opening a list shows its first item in the details, while they show nothing or an item: not over a form or
    # what was just done. Not when they already show one of its items.
    pick_first <- function(list_id) {
      list <- make_edit_lists[[list_id]]
      if (!verb() %in% c("", "view") || is.null(list)) {
        return()
      }
      read <- tables_of(list$kind)
      if (inherits(read, "error")) {
        assign(list$kind, read, envir = just_failed) # its list says so, without reading again at once
        return()
      }
      cards <- make_edit_cards(list_id, read)
      if (nrow(cards) == 0 || (identical(kind(), list$kind) && isTRUE(picked() %in% cards$key))) {
        return()
      }
      show_item(list(kind = list$kind, scope = "", key = cards$key[1], verb = "view"))
    }

    # Going to another item, or to + Add, while a form has something typed in it asks first.
    go_to <- function(where) {
      if (verb() %in% c("add", "update") && change$dirty()) {
        leaving_to(where)
        showModal(unsaved_dialog("Leave this edit?", "confirm_leave", "Leave it"))
        return()
      }
      show_item(where)
    }

    observeEvent(input$confirm_leave, {
      removeModal()
      where <- leaving_to()
      leaving_to(NULL)
      if (!is.null(where)) show_item(where)
    })

    # The item in the details, as Back returns to it: with its name, for the button.
    here <- function() {
      shown <- if (nzchar(kind()) && !is.null(picked())) tryCatch(row(), error = function(problem) NULL)
      if (is.null(shown)) {
        return(NULL)
      }
      list(kind = kind(), scope = scope(), key = picked(), verb = "view", label = shorten(edit_label(snapshot(), entity()$table, shown), 40))
    }

    # Shows an item (or, with no key, the form that adds one). One reached from another (`follows`: a tag or View
    # in its details) keeps the way back to it; one picked in the list starts afresh. The list marks it, if it is
    # drawn there (the page script unmarks the one before).
    show_item <- function(where) {
      if (isTRUE(where$follows)) {
        trail(c(trail(), if (!is.null(here())) list(here())))
      } else if (!isTRUE(where$returns)) {
        trail(list())
      }
      done(NULL)
      add_list(where$list %||% "")
      entered(where$verb)
      kind(where$kind)
      scope(where$scope)
      picked(where$key)
      verb(where$verb)
      if (!is.null(where$key)) session$sendCustomMessage("make-edit-picked", paste(where$kind, where$scope, where$key, sep = "|"))
    }

    # Back, to the item the one in the details was reached from.
    go_back <- function() {
      left <- trail()
      if (length(left) == 0) {
        return()
      }
      trail(left[-length(left)])
      show_item(c(left[[length(left)]], list(returns = TRUE)))
    }

    # The details can ask to go to another item: a tag or a View opens the thing it names, a card under a shared
    # wording opens it as one questionnaire prints it, a scale's question has Remove and the scale Add a question,
    # and a form whose drop-down has nothing to pick adds what it needs first.
    observeEvent(change$jump(), {
      jump <- change$jump()
      go_to(list(
        kind = jump$kind, scope = jump$scope %||% "", key = if (has_text(jump$key)) jump$key, verb = jump$verb %||% "add",
        follows = TRUE
      ))
    })

    # ------ The details ------------------------------------------------------------
    # Drawn again when another item is picked or the details change what they show, never as a form is typed in.
    # What the row holds is the change module's (edit_change_server()), which draws it again when the tables are
    # read again; this draws only around it, and again if the row picked is no longer there once they are
    # (`row_gone`, which changes only when that does, so a form being typed in isn't drawn again).
    row_gone <- reactiveVal(FALSE)
    observe({
      gone <- verb() %in% c("view", "update", "delete", "merge") && is.null(tryCatch(row(), shiny.silent.error = function(problem) NULL))
      row_gone(gone)
    })
    output$detail <- renderUI({
      mode <- verb()
      key <- kind()
      picked()
      scope()
      done()
      row_gone()
      isolate(detail_ui(mode, key))
    })

    detail_ui <- function(mode, key) {
      if (!nzchar(mode) || is.null(entity())) {
        return(div(
          class = "detail-empty",
          p("Pick something in the list to see it here."),
          p("Looking changes nothing: an item is only changed with its Edit button, and then Save.")
        ))
      }
      if (!is.null(load_problem())) {
        return(div(class = "detail-empty", load_problem()))
      }
      if (mode == "done") {
        return(done_ui())
      }
      if (mode == "add" && !is.null(entity()$scope) && !nzchar(scope())) {
        return(scope_picker_ui(key))
      }
      blocked <- if (mode != "view") cant(mode)
      shown <- if (mode != "add") tryCatch(row(), shiny.silent.error = function(problem) NULL)
      if (mode != "add" && is.null(shown)) {
        return(div(class = "detail-empty", "It isn't in the database any more. Pick another."))
      }
      buttons <- detail_buttons(mode, blocked)
      div(
        class = "block-detail edit-detail",
        detail_top(mode, shown),
        div(
          class = "detail-content",
          if (mode == "view") {
            NULL
          } else if (!is.null(blocked)) {
            edit_callout("warning", title = paste("You can't", mode, edit_a(edit_noun(entity(), shown)), "yet"), p(blocked))
          } else {
            edit_info_block(entity(), mode, shown)
          },
          if (is.null(blocked)) edit_change_ui(ns("change")),
          if (mode != "view" && is.null(blocked)) uiOutput(ns("footer_note"), class = "edit-footer-note")
        ),
        if (length(buttons) > 0) div(class = "detail-actions", buttons)
      )
    }

    # The top of the details: Back to the item this one was reached from (a tag or View there), what kind of thing
    # it is, its name, and Edit (off, saying why, for a kind that can't be changed), where it is seen at once.
    detail_top <- function(mode, shown = NULL) {
      left <- trail()
      no_update <- if (mode == "view") cant_change(shown)
      detail_head(
        if (mode == "view" && length(left) > 0) {
          review_button(
            tagList(app_icon("back"), " Back to ", left[[length(left)]]$label), "trail_back",
            class = "btn btn-sm btn-link detail-trail"
          )
        },
        detail_badge(shown),
        h2(class = "detail-title", detail_title(mode)),
        if (mode == "view" && !isTRUE(entity()$view_only)) {
          review_button(
            icon_label("edit", "Edit"), "edit",
            class = "btn btn-primary detail-edit", disabled = !is.null(no_update),
            title = no_update %||% "Change it. Nothing is saved until you press Save"
          )
        }
      )
    }

    # What kind of thing the details show, in a black tag, the same for every kind: a wording by its kind of block;
    # a new one by the list its + Add is in.
    detail_badge <- function(shown) {
      called <- if (is.null(shown) && nzchar(add_list())) make_edit_lists[[add_list()]]$one else make_edit_called(kind(), snapshot(), shown)
      span(class = "detail-kind", called)
    }

    # + Add of a block asks which questionnaire first (and of a scale's question, which scale, though Add a question
    # in a scale's details says); the form comes once one is picked.
    scope_picker_ui <- function(key) {
      table <- entity()$scope$table
      div(
        class = "block-detail edit-detail",
        detail_top("add"),
        div(
          class = "detail-content",
          selectInput(
            ns("add_scope"), NULL, edit_choices(snapshot(), table, blank = entity()$scope$label),
            width = "100%", selectize = nrow(snapshot()[[table]]) > 30
          ) |> tag_input_attributes("select", `aria-label` = entity()$scope$label, title = entity()$scope$label)
        ),
        div(class = "detail-actions", review_button("Cancel", "back", class = "btn btn-outline-dark"))
      )
    }

    observeEvent(input$add_scope, {
      if (identical(verb(), "add") && nzchar(input$add_scope)) scope(input$add_scope)
    })

    detail_title <- function(mode) {
      if (mode == "add") {
        return(paste("New", tolower(if (nzchar(add_list())) make_edit_lists[[add_list()]]$one else make_edit_kinds[[kind()]])))
      }
      title <- edit_label(snapshot(), entity()$table, row())
      switch(mode,
        update = paste("Change", title),
        delete = paste("Delete", title),
        merge = paste("Merge", title),
        title
      )
    }

    # View (Edit is at the top): Merge for a kind that can be merged, Empty for what can be emptied, Delete (off,
    # saying why, when the kind can't be), and for a wave Re-extract, Edit extraction and Open PDF. A kind that is
    # only shown has Open PDF alone. A form: Cancel and Save. A delete or a merge: Cancel and the button that does it, or only Back
    # when something stands in a delete's way.
    detail_buttons <- function(mode, blocked) {
      if (mode == "view" && isTRUE(entity()$view_only)) {
        return(if (kind() == "pdf_file") pdf_open_button(pdf_url(row())))
      }
      if (mode == "view") {
        no_delete <- cant("delete")
        alone <- edit_can_merge(entity()) && NROW(edit_merge_candidates(entity(), snapshot(), row())) == 0
        return(tagList(
          if (edit_can_merge(entity())) {
            review_button(
              icon_label("merge", "Merge into another"), "merge",
              class = "btn btn-outline-dark", disabled = alone,
              title = if (alone) {
                "There is no other of its kind to merge it into"
              } else {
                "For two that are the same thing kept twice: everything that uses this one uses the other instead, and this one is removed"
              }
            )
          },
          if (entity()$table %in% edit_emptiable) {
            review_button(
              icon_label("delete", "Empty"), "empty",
              class = "btn btn-outline-dark", title = paste("Remove everything in this", entity()$noun, "and keep the", entity()$noun, "itself")
            )
          },
          review_button(
            icon_label("delete", "Delete"), "delete",
            class = "btn btn-delete", disabled = !is.null(no_delete), title = no_delete
          ),
          if (kind() == "wave") wave_reextract_button(row()),
          if (kind() == "wave") wave_edit_extraction_button(any(snapshot()$wave_question$wave_id %in% row()$wave_id)),
          if (kind() == "wave" && has_text(row()$pdf_sha256)) pdf_open_button(wave_pdf_url(row()))
        ))
      }
      stuck <- !is.null(blocked) || change$stuck()
      tagList(
        actionButton(ns("cancel"), if (stuck) "Back" else "Cancel", class = "btn-outline-dark"),
        if (!stuck) {
          actionButton(
            ns("submit"), switch(mode, add = "Add it", update = "Save", delete = "Delete it", merge = "Merge them"),
            class = if (mode == "delete") "btn-delete" else "btn-primary", disabled = !change$ready()
          )
        }
      )
    }

    # The address the browser gets a PDF from, for this session only (`pdf_address()`): fetched from Storage if this
    # server hasn't got it. NULL, and Open PDF left out, if it can't be had.
    pdf_url <- function(pdf) {
      path <- tryCatch(extraction_model_local_pdf(connection, pdf$pdf_sha256), error = function(problem) NULL)
      if (is.null(path)) {
        return(NULL)
      }
      name <- strsplit(edit_pdf_names(snapshot(), pdf$pdf_sha256), " / ", fixed = TRUE)[[1]][1]
      pdf_address(session, ns(paste0("pdf-", pdf$pdf_sha256)), list(path = path, name = name, sha256 = pdf$pdf_sha256))
    }

    # The address the browser gets a wave's PDF from, for this session only. The file is fetched from Storage when
    # the address is asked for (Open PDF is pressed), not when the wave is shown: most waves looked at are never
    # opened, and a PDF can be large.
    wave_pdf_url <- function(wave) {
      session$registerDataObj(
        name = ns(paste0("wave-pdf-", wave$pdf_sha256)),
        data = list(sha256 = wave$pdf_sha256, name = if (has_text(wave$pdf_name)) wave$pdf_name else "questionnaire.pdf"),
        filterFunc = function(data, req) {
          path <- tryCatch(extraction_model_local_pdf(connection, data$sha256), error = function(problem) NULL)
          if (is.null(path)) {
            return(shiny::httpResponse(404L, "text/plain; charset=UTF-8", "This PDF isn't in the site's storage any more."))
          }
          pdf_response(c(data, list(path = path)), req)
        }
      )
    }

    # ------ The details' buttons ---------------------------------------------
    observe({
      req(verb() %in% c("add", "update", "delete", "merge"))
      updateActionButton(session, "submit", disabled = !change$ready())
    })

    # What is left to do before it can be applied (that nothing was changed yet is said where the changes are
    # listed, so not again here).
    output$footer_note <- renderUI({
      left <- if (!change$ready()) setdiff(change$blockers(), edit_nothing_changed)
      if (length(left) > 0) left[1]
    })

    # Cancel: a form with something typed in it asks first; it goes back to the item (or to nothing, for an add), or,
    # if the form was opened from another item's details (Add a question, Remove), back to that item.
    observeEvent(input$cancel, {
      if (verb() %in% c("add", "update") && !change$stuck() && change$dirty()) {
        showModal(unsaved_dialog("Cancel this edit?", "confirm_cancel", "Cancel the edit"))
        return()
      }
      back_to_item()
    })

    observeEvent(input$confirm_cancel, {
      removeModal()
      back_to_item()
    })

    back_to_item <- function() {
      if (length(trail()) > 0 && !identical(entered(), "view")) {
        return(go_back())
      }
      verb(if (is.null(picked())) "" else "view")
    }

    # ------ Applying ------------------------------------------------------------
    # The main button. An add is checked for something like it first.
    observeEvent(input$submit, {
      req(change$ready())
      if (identical(verb(), "add")) {
        found <- tryCatch(
          edit_service_duplicates(entity(), snapshot(), change$fields(), change$values()),
          shiny.silent.error = function(problem) list()
        )
        if (length(found) > 0) {
          duplicates(found)
          showModal(edit_duplicates_dialog(found, ns("confirm_duplicate")))
          return()
        }
      }
      # an update that merges the row into another (a tag renamed to another tag's name) asks first too
      merge <- if (identical(verb(), "update")) change$built()$merge
      if (!is.null(merge)) {
        showModal(edit_merge_dialog(merge, ns("confirm_merge")))
        return()
      }
      proceed()
    })

    observeEvent(input$confirm_merge, {
      removeModal()
      proceed()
    })

    observeEvent(input$confirm_duplicate, {
      removeModal()
      duplicates(NULL)
      proceed()
    })

    # Applies the change. The lists are read again, and the details say what happened.
    proceed <- function() {
      built <- change$built()
      applied <- try_showing(edit_service_apply(connection, built, me), "Couldn't apply it")
      if (is.null(applied)) {
        return()
      }
      read_again()
      done(list(
        verb = verb(), summary = built$summary, history_id = applied$history_id, new_ids = applied$new_ids,
        shows = built$merge$key # a row merged into another is gone: the other is shown afterwards
      ))
      verb("done")
    }

    done_ui <- function() {
      outcome <- done()
      div(
        class = "block-detail edit-detail",
        div(
          class = "detail-content edit-sent",
          h2(span(class = "edit-sent-mark", app_icon("done")), "Done"),
          p(strong(outcome$summary)),
          p(if (identical(outcome$verb, "delete")) "Successfully deleted from the database." else "Successfully saved to the database."),
          p(class = "form-text", "It is in the History, with what was there before.")
        ),
        div(
          class = "detail-actions",
          review_button("Back", "back", class = "btn btn-outline-dark"),
          actionButton(ns("to_history"), "See the Edit History", class = "btn-primary")
        )
      )
    }

    # After a change: back to the item changed (the one it was merged into, if it was); after an add or a delete, to
    # the item it was reached from (a scale, after a question was added to it or removed), or to nothing picked.
    after_done <- function() {
      outcome <- done()
      done(NULL)
      if (!is.null(outcome$shows)) picked(outcome$shows)
      if (isTRUE(outcome$verb %in% c("update", "merge")) && !is.null(picked())) {
        return(verb("view"))
      }
      if (length(trail()) > 0) {
        return(go_back())
      }
      picked(NULL)
      verb("")
    }

    # ------ Emptying ---------------------------------------------------------------------------
    # Empty on a registry, study, questionnaire or wave: what it holds goes, it stays. The dialog says what would go,
    # counted from the database as it is now.
    ask_empty <- function() {
      req(entity()$table %in% edit_emptiable)
      preview <- try_showing(edit_service_empty_preview(connection, kind(), picked()), "Couldn't count what it holds")
      if (is.null(preview)) {
        return()
      }
      showModal(edit_empty_dialog(ns, entity(), edit_label(snapshot(), entity()$table, row()), preview))
    }

    observeEvent(input$confirm_empty, {
      removeModal()
      req(identical(verb(), "view"), entity()$table %in% edit_emptiable)
      removed <- try_showing(edit_service_empty(connection, kind(), picked(), me), "Couldn't empty it")
      if (is.null(removed)) {
        return()
      }
      read_again()
      showNotification(paste("Emptied.", removed, "It is in the Edit History."), type = "message")
    })

    # ------ Re-extract a wave ---------------------------------------------------------------
    # The wave in the details is emptied and a new extraction of it started, from the PDF it was extracted from, into
    # the same questionnaire; its reading is shown on New Extraction.
    observeEvent(input$confirm_reextract_wave, {
      removeModal()
      req(identical(kind(), "wave"))
      wave <- row()
      snap <- snapshot()
      study_id <- snap$questionnaire$study_id[match(wave$questionnaire_id, snap$questionnaire$questionnaire_id)]
      id_text <- function(value) if (length(value) == 1 && !is.na(value)) key_text(value) else ""
      place <- list(
        registry_id = id_text(snap$study$registry_id[match(study_id, snap$study$study_id)]),
        study_id = id_text(study_id), questionnaire_id = id_text(wave$questionnaire_id), wave_id = id_text(wave$wave_id)
      )
      record <- try_showing(
        extraction_service_reextract_wave(connection, key_text(wave$wave_id), place, me),
        "Couldn't start the extraction"
      )
      if (is.null(record)) {
        return()
      }
      read_again()
      showNotification("The wave is emptied and its PDF is being read again.", type = "message")
      if (is.function(open_reading)) open_reading(record) else show_page("extraction")
    })

    # ------ Edit a wave's extraction ----------------------------------------------------------
    # What the wave in the details asks is opened on Edit Extraction as a draft (the one already open for it, if
    # there is one); pushing it there replaces what the wave asks. Nothing changes here until then.
    edit_wave_extraction <- function() {
      record <- try_showing(
        extraction_service_reopen_wave(connection, key_text(row()$wave_id), me),
        "Couldn't open the wave's extraction"
      )
      if (is.null(record)) {
        return()
      }
      if (is.function(open_draft)) open_draft(record) else show_page("drafts")
    }

    observeEvent(input$to_history, show_page("edit_history"))

    # ------ RETURN ------------------------------------------------------------
    invisible(NULL)
  })
}
