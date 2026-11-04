# Make an edit's kinds in the group Printed content: the kinds of block, and the components and questions every
# questionnaire that prints them shares. The kinds are listed together in edit_entities (service_edit_kinds.R).

# ------ Kinds of block ---------------------------------------------------------------------------------
component_type_entity <- list(
  label = "Component type", noun = "component type", plural = "component types", article = "a",
  group = "Printed content", table = "component_type",
  about = "The kinds of block a questionnaire prints: Section, Question, Instruction and so on.",
  delete_about = "A component type can only be deleted once no component is of that kind.",
  extra_blockers = function(snap, row) {
    if (row$name %in% edit_core_types) {
      list(list(
        text = sprintf(
          paste0(
            "“%s” is built into this site. Questions and sections are handled differently from every other ",
            "kind of block, and the site finds them by this name."
          ),
          row$name
        ),
        fix = "Keep it.", labels = row$name, table = "component_type"
      ))
    }
  },
  cards = function(snap, scope) {
    rows <- snap$component_type
    n_components <- count_in(snap$component$component_type_id, rows$component_type_id)
    edit_cards(
      key_text(rows$component_type_id), rows$name,
      subtitle = ifelse(is.na(rows$description), "", rows$description),
      facts = lapply(n_components, function(n) edit_count(n, "component")),
      warn = ifelse(rows$name %in% edit_core_types, "Built in: the site relies on this name", NA_character_)
    )
  },
  fields = function(snap, mode, row, scope) {
    core <- mode == "update" && !is.null(row) && row$name %in% edit_core_types
    list(
      edit_field("name", "Name",
        required = TRUE, placeholder = "e.g. Instruction",
        locked = if (core) "This kind is built into the site, which finds it by its name.",
        problem = function(value, values) {
          same <- edit_taken(snap, "component_type", "name", value, row)
          if (has_text(value) && nrow(same) > 0) sprintf("There's already a component type called %s.", same$name[1])
        }
      ),
      edit_field("description", "Description", "textarea")
    )
  }
)

# Kinds of block the site treats specially (BR42, BR45, BR53).
edit_core_types <- c("Section", "Question", "Group")

# ------ Printed content: shared by every questionnaire that prints it ------------------------------
#' Whether components are questions.
#'
#' A component is kept once and any number of questionnaires place it. A question is a
#' component of the kind Question that also has a question row.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param component_ids Component ids.
#' @return A logical vector.
edit_is_question <- function(snap, component_ids) {
  component_ids %in% snap$question$component_id
}

#' The id of a kind of block.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param name The kind's name, e.g. `"Question"`.
#' @return The `component_type_id`, or `NA` if there isn't exactly one.
edit_type_id <- function(snap, name) {
  id <- snap$component_type$component_type_id[snap$component_type$name == name]
  if (length(id) == 1) id else NA_real_
}

# "seen in 2 questionnaires: AHP, MQ"; "not printed in any questionnaire".
edit_seen_in <- function(snap, component_id) {
  q_ids <- unique(snap$questionnaire_component$questionnaire_id[snap$questionnaire_component$component_id == component_id])
  if (length(q_ids) == 0) {
    return("not printed in any questionnaire")
  }
  names <- edit_questionnaire_short(snap, q_ids)
  paste0("seen in ", edit_count(length(q_ids), "questionnaire"), if (length(q_ids) <= 4) paste0(": ", paste(names, collapse = ", ")))
}

# The warning on a card, when changing it would reach more than one questionnaire.
edit_shared_warning <- function(snap, component_ids) {
  n <- edit_service_questionnaire_count(snap, component_ids)
  ifelse(n >= 2, paste("Shared: changes show in", n, "questionnaires"), NA_character_)
}

# A component's text field. Every kind has words but a group, which has none of its own (BR42).
edit_text_field <- function(snap, mode, row, label = "Printed words", type_field = TRUE) {
  group <- edit_type_id(snap, "Group")
  edit_field(
    "text", label, "textarea",
    required = !type_field, rows = 3, short = if (type_field) "words" else "wording",
    problem = function(value, values) {
      is_group <- identical(as.numeric(values$component_type_id %||% NA), group)
      if (type_field && !has_text(value) && !is_group) {
        return("Add the printed words. Only a group has none.")
      }
      if (type_field && has_text(value) && is_group) {
        "A group has no words of its own. Put them in an Instruction inside it."
      }
    },
    warn = function(value, values) {
      if (!has_text(value)) {
        return(NULL)
      }
      same <- edit_taken(snap, "component", "text", value, row)
      if (nrow(same) > 0) {
        paste0(
          "Another component already has exactly these words (", edit_seen_in(snap, same$component_id[1]), "). ",
          "Using that one would keep the wording shared instead of making a copy."
        )
      }
    }
  )
}

component_entity <- list(
  label = "Component", noun = "component", plural = "components", article = "a",
  group = "Printed content", table = "component",
  about = paste0(
    "A heading, instruction, definition or any other block that isn't a question. Each is kept ",
    "once, and every questionnaire that prints it uses that one copy."
  ),
  update_about = "A component is shared. Changing its words changes them in every questionnaire that prints it.",
  delete_about = paste(
    "Deleting a component takes it out of every questionnaire that prints it, and everything inside it if it's a",
    "section or group."
  ),
  cards = function(snap, scope) {
    rows <- snap$component[!edit_is_question(snap, snap$component$component_id), , drop = FALSE]
    type <- edit_lookup(snap, "component_type", "component_type_id", rows$component_type_id, "name")
    edit_cards(
      key_text(rows$component_id), edit_component_text(snap, rows$component_id),
      subtitle = "",
      facts = lapply(rows$component_id, function(id) edit_seen_in(snap, id)),
      tag = type, warn = edit_shared_warning(snap, rows$component_id)
    )
  },
  fields = function(snap, mode, row, scope) {
    types <- snap$component_type[snap$component_type$name != "Question", , drop = FALSE]
    list(
      edit_field(
        "component_type_id", "Kind of block", "select",
        required = TRUE,
        choices = if (mode == "add") {
          edit_choices(snap, "component_type", types, blank = "Choose a kind")
        } else {
          edit_choices(snap, "component_type")
        },
        locked = paste0(
          "A component's kind is fixed. To print a different kind of block in just one questionnaire, ",
          "change it there instead (Update, then Questionnaire component)."
        )
      ),
      edit_text_field(snap, mode, row)
    )
  },
  duplicates = function(snap, values) {
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "component", "text", values$text, "A block with exactly these words")
    ))
  }
)

question_entity <- list(
  label = "Question", noun = "question", plural = "questions", article = "a",
  group = "Printed content", table = "component",
  about = paste0(
    "A question's wording, kept once. Every questionnaire that asks it uses that one copy, with its ",
    "own variables and question number."
  ),
  update_about = "A question is shared. Changing its wording changes it in every questionnaire that asks it.",
  delete_about = paste0(
    "Deleting a question takes it out of every questionnaire that asks it. Its variables, their ",
    "choices, its MATR IDs, its question numbers in every wave, and its MeSH tags all go with it."
  ),
  cards = function(snap, scope) {
    rows <- snap$component[edit_is_question(snap, snap$component$component_id), , drop = FALSE]
    boxes <- count_in(snap$variable$component_id, rows$component_id)
    edit_cards(
      key_text(rows$component_id), edit_component_text(snap, rows$component_id),
      subtitle = "",
      facts = Map(
        function(id, n) c(edit_seen_in(snap, id), if (n > 0) edit_count(n, "variable", "variables")),
        rows$component_id,
        boxes
      ),
      tag = "Question", warn = edit_shared_warning(snap, rows$component_id)
    )
  },
  fields = function(snap, mode, row, scope) {
    list(edit_text_field(snap, mode, row, "Question wording", type_field = FALSE))
  },
  duplicates = function(snap, values) {
    questions <- snap$component[edit_is_question(snap, snap$component$component_id), , drop = FALSE]
    only_questions <- snap
    only_questions$component <- questions
    Filter(Negate(is.null), list(
      edit_duplicate_rows(only_questions, "component", "text", values$text, "A question with exactly these words")
    ))
  },
  can = function(snap, mode) {
    if (mode == "add" && !is.null(snap$component_type) && is.na(edit_type_id(snap, "Question"))) {
      "The database has no component type called Question, so a question can't be added yet."
    }
  },
  build = function(mode, snap, row, scope, fields, values) {
    if (mode == "add") {
      text_field <- Filter(function(f) f$name == "text", fields)[[1]]
      edit_new_question_items(snap, edit_service_normalize(text_field, values[["question__text"]]))
    } else {
      edit_update_items(edit_entities[["question"]], snap, row, fields, values)
    }
  }
)
