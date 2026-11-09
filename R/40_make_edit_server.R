# State kept outside reactive values: `tables` (each kind's tables as read), `just_failed` (a read that failed as
# its section opened) and `open_sections` (whether each section is open), environments made by new.env() inside the
# module, so each session has its own and they go when it ends. Nothing else reads them. Shiny runs a session's
# code one piece at a time, so nothing changes them while something else is reading them; they aren't made to be
# shared between threads or sessions, and shouldn't be.

# The Metadata page's server (the module is called make_edit).
# The list on the left has a section for each kind of thing (40_make_edit_ui.R); a section's items are read from
# the database the first time it is opened. Picking an item shows it in the details (`edit_change_server()`,
# "view"), with Edit and Delete; Edit, Delete and + Add show the form or what a delete reaches there, with Save.
# Every change is applied at once, by the database's apply_change() (`edit_service_apply()`), and written to the
# Edit History. A registry, study, questionnaire or wave has Empty too: it stays, its contents go
# (`edit_service_empty()`). A wave extracted from a PDF has Re-extract: it is emptied and a new extraction of it
# is started from its PDF (`extraction_service_reextract_wave()`), shown with `open_reading`.
# active: A reactive that says whether this page is the one on screen, or `NULL`. Opening the page reads the lists
#   again, since other staff change them.
# open_reading: A function taking an extraction being read that shows how reading it is going (New Extraction's),
#   or `NULL` to go to New Extraction.
make_edit_server <- function(id, connection, me, show_page, active = NULL, open_reading = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE ----------------------------------------------------------
    kind <- reactiveVal("") # the kind of the item in the details, a name of edit_entities
    scope <- reactiveVal("") # the questionnaire (or measurement scale), for a kind that belongs to one
    picked <- reactiveVal(NULL) # the key of the item in the details
    verb <- reactiveVal("") # what the details show: "" (nothing), "view", "add", "update", "delete" or "done"
    entity <- reactive(if (kind() %in% names(edit_entities)) edit_entities[[kind()]])

    done <- reactiveVal(NULL) # what was just done: list(verb, summary, history_id, new_ids)
    duplicates <- reactiveVal(NULL) # the existing rows an add looks like, while staff decide
    leaving_to <- reactiveVal(NULL) # what was picked while a form was open, while staff decide whether to leave it
    hint <- reactiveVal(NULL) # what the details say while nothing is picked

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
          if (isTRUE(active()) && !verb() %in% c("add", "update", "delete")) read_again()
        },
        ignoreInit = TRUE
      )
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
    change <- edit_change_server("change", entity, verb, snapshot, row, scope)

    # ------ The list ------------------------------------------------------------
    # A section's items: drawn while it is open, and again when the tables are read again. Only what is open is drawn,
    # so only the kinds staff look at are read from the database. Whether a section is open is kept for each section
    # (the page script opens and closes it at once, and says so), so opening one doesn't draw the others again: a
    # section inside one (a questionnaire's, under Blocks) would be drawn closed.
    open_sections <- new.env()
    section_open <- function(id) {
      if (!exists(id, envir = open_sections, inherits = FALSE)) assign(id, reactiveVal(FALSE), envir = open_sections)
      get(id, envir = open_sections, inherits = FALSE)
    }
    registered <- character()
    items_output <- function(name, section, draw) {
      if (name %in% registered) {
        return()
      }
      registered <<- c(registered, name)
      output[[name]] <- renderUI({
        req(section_open(section)())
        draw()
      })
    }

    lapply(make_edit_listed, function(key) {
      items_output(paste0("items_", key), key, function() {
        read <- tables_of(key)
        if (inherits(read, "error")) {
          return(div(
            class = "edit-list-note",
            p(conditionMessage(read)),
            review_button("Try again", "retry", `data-kind` = key, class = "btn btn-sm btn-outline-dark")
          ))
        }
        entity <- edit_entities[[key]]
        if (!is.null(entity$scope)) {
          return(scope_sections(key, read))
        }
        items_ui(key, read, "")
      })
    })

    # A kind that belongs to a questionnaire (or a measurement scale): a section for each, holding its items.
    scope_sections <- function(key, snap) {
      table <- edit_entities[[key]]$scope$table
      owners <- edit_choices(snap, table)
      owners <- owners[order(tolower(names(owners)), method = "radix")]
      if (length(owners) == 0) {
        return(p(class = "edit-list-note", paste0("There are no ", edit_nouns[[table]][2], " in the database yet.")))
      }
      Map(function(label, q) {
        name <- paste0("items_", key, "_", q)
        items_output(name, paste0(key, ":", q), function() {
          read <- tables_of(key)
          if (!inherits(read, "error")) items_ui(key, read, q)
        })
        id <- paste0(key, ":", q)
        make_edit_section(id, label, uiOutput(ns(name), class = "outline-list"), open = isolate(section_open(id)()))
      }, names(owners), unname(owners))
    }

    items_ui <- function(key, snap, q) {
      entity <- edit_entities[[key]]
      cards <- make_edit_cards(key, snap, q)
      chosen <- isolate(if (identical(kind(), key)) list(scope = scope(), key = picked()))
      tagList(
        if (nrow(cards) == 0) p(class = "edit-list-note", paste0("No ", tolower_words(make_edit_kinds[[key]][1]), " yet.")),
        lapply(seq_len(nrow(cards)), function(i) {
          card <- cards[i, , drop = FALSE]
          make_edit_item(key, card, card$scope, identical(card$scope, chosen$scope) && identical(card$key, chosen$key))
        }),
        if (!isTRUE(entity$view_only)) make_edit_add_button(key, q)
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
      switch(event$action,
        toggle = {
          is_open <- section_open(event$id)
          is_open(!is_open())
          if (is_open()) pick_first(event$id)
        },
        collapse_all = for (id in ls(open_sections)) section_open(id)(FALSE), # the page script closes them at once
        select = go_to(list(kind = event$kind, scope = event$scope %||% "", key = event$key, verb = "view")),
        add = go_to(list(kind = event$kind, scope = event$scope %||% "", key = NULL, verb = "add")),
        edit = verb("update"),
        delete = verb("delete"),
        empty = ask_empty(),
        reextract_wave = if (identical(kind(), "wave")) showModal(reextract_wave_dialog(ns, edit_label(snapshot(), "wave", row()))),
        back = after_done(),
        retry = read_again()
      )
    })

    # Opening a section shows its first item in the details, while they show nothing or an item: not over a form or
    # what was just done. Not when they already show one of its items, or when it only holds sections (Blocks holds a
    # section for each questionnaire).
    pick_first <- function(section) {
      if (!verb() %in% c("", "view")) {
        return()
      }
      parts <- strsplit(section, ":", fixed = TRUE)[[1]]
      key <- parts[1]
      q <- if (length(parts) > 1) parts[2] else ""
      entity <- edit_entities[[key]]
      if (is.null(entity) || (!is.null(entity$scope) && !nzchar(q))) {
        return()
      }
      read <- tables_of(key)
      if (inherits(read, "error")) {
        assign(key, read, envir = just_failed) # its list says so, without reading again at once
        return()
      }
      cards <- make_edit_cards(key, read, q)
      if (nrow(cards) == 0 || (identical(kind(), key) && any(paste(scope(), picked()) %in% paste(cards$scope, cards$key)))) {
        return()
      }
      show_item(list(kind = key, scope = cards$scope[1], key = cards$key[1], verb = "view"))
      # the list marks it as it is drawn; the page script unmarks the item picked before, in a section already open
      session$sendCustomMessage("make-edit-picked", paste(key, cards$scope[1], cards$key[1], sep = "|"))
    }

    # Going to another item, or to + Add, while a form has something typed in it asks first.
    go_to <- function(where) {
      if (verb() %in% c("add", "update")) {
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

    show_item <- function(where) {
      done(NULL)
      hint(NULL)
      kind(where$kind)
      scope(where$scope)
      picked(where$key)
      verb(where$verb)
    }

    # A form can ask to go to another kind: to add the registry it needs, or to open a block as it is in one
    # questionnaire (a card under a shared component has a button for each).
    observeEvent(change$jump(), {
      jump <- change$jump()
      if (identical(jump$verb, "add")) {
        go_to(list(kind = jump$kind, scope = scope(), key = NULL, verb = "add"))
        return()
      }
      if (has_text(jump$key)) {
        go_to(list(kind = jump$kind, scope = jump$scope %||% "", key = jump$key, verb = jump$verb))
        return()
      }
      show_item(list(kind = "", scope = "", key = NULL, verb = ""))
      owner <- edit_nouns[[edit_entities[[jump$kind]]$scope$table %||% "questionnaire"]][1]
      hint(paste0("Open ", make_edit_kinds[[jump$kind]][1], " in the list, then the ", owner, ", and pick it there."))
    })

    # ------ The details ------------------------------------------------------------
    # Drawn again when another item is picked or the details change what they show, never as a form is typed in.
    # What the row holds is the change module's (edit_change_server()), which draws it again when the tables are
    # read again; this draws only around it, and again if the row picked is no longer there once they are
    # (`row_gone`, which changes only when that does, so a form being typed in isn't drawn again).
    row_gone <- reactiveVal(FALSE)
    observe({
      gone <- verb() %in% c("view", "update", "delete") && is.null(tryCatch(row(), shiny.silent.error = function(problem) NULL))
      row_gone(gone)
    })
    output$detail <- renderUI({
      mode <- verb()
      key <- kind()
      picked()
      scope()
      done()
      hint()
      row_gone()
      isolate(detail_ui(mode, key))
    })

    detail_ui <- function(mode, key) {
      if (!nzchar(mode) || is.null(entity())) {
        return(div(class = "detail-empty", hint() %||% "Pick something in the list to see it here, or add one with + Add."))
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
      if (mode != "add" && is.null(tryCatch(row(), shiny.silent.error = function(problem) NULL))) {
        return(div(class = "detail-empty", "It isn't in the database any more. Pick another."))
      }
      div(
        class = "block-detail edit-detail",
        detail_head(span(class = "row-badge", make_edit_kinds[[key]][2]), h2(class = "detail-title", detail_title(mode))),
        div(
          class = "detail-content",
          if (mode == "view") {
            NULL
          } else if (!is.null(blocked)) {
            edit_callout("warning", title = paste("You can't", mode, paste(entity()$article, entity()$noun), "yet"), p(blocked))
          } else {
            edit_info_block(entity(), mode)
          },
          if (is.null(blocked)) edit_change_ui(ns("change")),
          if (mode != "view" && is.null(blocked)) uiOutput(ns("footer_note"), class = "edit-footer-note")
        ),
        div(class = "detail-actions", detail_buttons(mode, blocked))
      )
    }

    # + Add of a kind listed by questionnaire (Blocks) or by scale (Scale Items) asks which first; the form comes once
    # one is picked.
    scope_picker_ui <- function(key) {
      table <- entity()$scope$table
      div(
        class = "block-detail edit-detail",
        detail_head(span(class = "row-badge", make_edit_kinds[[key]][2]), h2(class = "detail-title", detail_title("add"))),
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
        return(paste("New", tolower(make_edit_kinds[[kind()]][2])))
      }
      title <- edit_label(snapshot(), entity()$table, row())
      switch(mode,
        update = paste("Change", title),
        delete = paste("Delete", title),
        title
      )
    }

    # View: Edit and Delete (each off, saying why, when the kind can't be changed that way), Empty for what can be
    # emptied, Re-extract for a wave, or for a PDF, which is only shown, Open PDF. A form: Cancel and Save. A delete:
    # Cancel and Delete, or only Back when something stands in its way.
    detail_buttons <- function(mode, blocked) {
      if (mode == "view" && isTRUE(entity()$view_only)) {
        return(if (kind() == "pdf_file") pdf_open_button(pdf_url(row())))
      }
      if (mode == "view") {
        no_update <- cant("update")
        no_delete <- cant("delete")
        return(tagList(
          review_button(
            icon_label("edit", "Edit"), "edit",
            class = "btn btn-outline-dark", disabled = !is.null(no_update), title = no_update
          ),
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
          if (kind() == "wave") wave_reextract_button(row())
        ))
      }
      stuck <- !is.null(blocked) || change$stuck()
      tagList(
        actionButton(ns("cancel"), if (stuck) "Back" else "Cancel", class = "btn-outline-dark"),
        if (!stuck) {
          actionButton(
            ns("submit"), switch(mode, add = "Add it", update = "Save", delete = "Delete it"),
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

    # ------ The details' buttons ---------------------------------------------
    observe({
      req(verb() %in% c("add", "update", "delete"))
      updateActionButton(session, "submit", disabled = !change$ready())
    })

    # What is left to do before it can be applied.
    output$footer_note <- renderUI({
      if (!change$ready()) change$blockers()[1]
    })

    # Cancel: a form with something typed in it asks first; it goes back to the item (or to nothing, for an add).
    observeEvent(input$cancel, {
      if (verb() %in% c("add", "update") && !change$stuck()) {
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
      done(list(verb = verb(), summary = built$summary, history_id = applied$history_id, new_ids = applied$new_ids))
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
          p(if (identical(outcome$verb, "delete")) "Successfully deleted from the database." else "Successfully saved to the database.")
        ),
        div(
          class = "detail-actions",
          review_button("Back", "back", class = "btn btn-outline-dark"),
          actionButton(ns("to_history"), "See the Edit History", class = "btn-primary")
        )
      )
    }

    # After a change: back to the item changed, or (an add or a delete) to nothing picked.
    after_done <- function() {
      outcome <- done()
      done(NULL)
      changed_still_there <- identical(outcome$verb, "update") && !is.null(picked())
      if (!changed_still_there) picked(NULL)
      verb(if (changed_still_there) "view" else "")
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

    observeEvent(input$to_history, show_page("edit_history"))

    # ------ RETURN ------------------------------------------------------------
    invisible(NULL)
  })
}
