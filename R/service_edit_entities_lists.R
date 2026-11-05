# Make an edit's other kinds in the group Shared lists: MeSH categories, subjects, option sets, measurement
# scales and their items. The kinds are listed together in edit_entities (service_edit_kinds.R).

# ------ MeSH categories and subjects ------------------------------------------------------------------
category_entity <- list(
  label = "MeSH category", noun = "MeSH category", plural = "MeSH categories", article = "a",
  group = "Shared lists", table = "category",
  about = "A subject heading from the National Library of Medicine's MeSH list, used to tag questions by topic.",
  delete_about = "Deleting a MeSH category removes it from every question it tags. The questions stay.",
  cards = function(snap, scope) {
    rows <- snap$category
    n_tagged <- count_in(snap$question_category$mesh_id, rows$mesh_id)
    edit_cards(
      rows$mesh_id, rows$term,
      subtitle = paste0("MeSH ", rows$mesh_id),
      facts = lapply(n_tagged, function(n) paste("tags", edit_count(n, "question")))
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("mesh_id", "MeSH ID",
        required = TRUE, placeholder = "e.g. D012907",
        locked = "A MeSH ID is the heading's identity at the National Library of Medicine, so it never changes.",
        hint = "A letter followed by 6 to 9 digits.",
        problem = function(value, values) {
          value <- trimws(value)
          if (!nzchar(value)) {
            return(NULL)
          }
          if (!grepl("^[A-Z][0-9]{6,9}$", value)) {
            return("A MeSH ID is a capital letter and then 6 to 9 digits, like D012907.")
          }
          if (nrow(edit_taken(snap, "category", "mesh_id", value, row)) > 0) "That MeSH ID is already in the list."
        }
      ),
      edit_field("term", "Term", required = TRUE, placeholder = "e.g. Smoking")
    )
  }
)

subject_entity <- list(
  label = "Subject", noun = "subject", plural = "subjects", article = "a",
  group = "Shared lists", table = "subject",
  about = paste0(
    "Who a variable can be about, like “Respondent” or “Respondent's Family”. The Edit Extraction page offers these, and the ",
    "reading instructions list them for Gemini."
  ),
  delete_about = paste0(
    "A subject can only be deleted once no variable is about it. A draft that still names it is ",
    "told to pick a subject from the list."
  ),
  cards = function(snap, scope) {
    rows <- snap$subject
    about <- count_in(snap$variable$subject_id, rows$subject_id)
    edit_cards(
      key_text(rows$subject_id), rows$name,
      facts = lapply(about, function(n) if (n > 0) paste("about", edit_count(n, "variable", "variables")) else "about no variables")
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("name", "Subject",
        required = TRUE, placeholder = "e.g. Respondent's Parents",
        hint = if (mode == "update") "A draft that still has the old name is told to pick from the list again.",
        problem = function(value, values) {
          same <- edit_taken(snap, "subject", "name", value, row)
          if (has_text(value) && nrow(same) > 0) sprintf("“%s” is already in the list.", same$name[1])
        }
      )
    )
  }
)

# ------ Sets of choices ---------------------------------------------------------------------------------
#' The answer choices of option sets, as the field of a set's choices holds them.
#'
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param set_ids Option set ids.
#' @return One string per set: its answer choices' ids joined by commas, smallest first.
edit_set_members <- function(snap, set_ids) {
  members <- snap$answer_option_set
  vapply(set_ids, function(id) edit_several_text(members$answer_option_id[members$option_set_id %in% id]), character(1), USE.NAMES = FALSE)
}

# The places in a set's order that choices added to it take: after the ones it keeps, in the order given.
edit_set_places <- function(snap, set_id, keeping, adding) {
  members <- snap$answer_option_set
  kept <- members$display_order[members$option_set_id %in% set_id & members$answer_option_id %in% keeping]
  max(c(0, kept)) + seq_along(adding)
}

# The place a choice has in a set's order.
edit_set_place <- function(snap, set_id, answer_option_id) {
  members <- snap$answer_option_set
  members$display_order[members$option_set_id %in% set_id & members$answer_option_id %in% answer_option_id][1]
}

option_set_entity <- list(
  label = "Option set", noun = "option set", plural = "option sets", article = "an",
  group = "Shared lists", table = "option_set",
  about = paste0(
    "A set of answer choices, kept once for each combination of them, and named by them. A closed-ended variable ",
    "offers exactly one set's choices, in its own order and with its own codes. An extraction uses the set that has ",
    "the same choices, or adds one."
  ),
  update_about = "The choices can only change while no variable offers them. The name is the choices, so it changes with them.",
  delete_about = paste0(
    "An option set can only be deleted once no variable offers it. Scale items that expected its choices stay in ",
    "their scale, expecting none."
  ),
  reads = "answer_option",
  cards = function(snap, scope) {
    rows <- snap$option_set
    offered <- count_in(snap$closed_ended$option_set_id, rows$option_set_id)
    expected <- count_in(snap$question_scale$expected_option_set_id, rows$option_set_id)
    choices <- count_in(snap$answer_option_set$option_set_id, rows$option_set_id)
    edit_cards(
      key_text(rows$option_set_id), ifelse(is.na(rows$name), "(no choices)", rows$name),
      subtitle = vapply(choices, function(n) edit_count(n, "choice"), character(1)),
      facts = Map(function(v, e) {
        c(
          if (v > 0) paste("offered by", edit_count(v, "variable")) else "offered by no variables",
          if (e > 0) paste("expected by", edit_count(e, "scale item"))
        )
      }, offered, expected)
    )
  },
  fields = function(snap, mode, row, scope) {
    offered <- if (!is.null(row)) sum(snap$closed_ended$option_set_id %in% row$option_set_id) else 0
    list(
      edit_field("answer_option_ids", "Answer choices", "several",
        required = TRUE, short = "choices",
        choices = edit_choices(snap, "answer_option"),
        hint = paste(
          "The set is named by them. Their order and codes are each variable's own.",
          "Missing one? Add it under Answer Choices first."
        ),
        locked = if (offered > 0) {
          sprintf(
            "%s %s exactly these choices, so they can't change.",
            edit_count(offered, "variable"), if (offered == 1) "offers" else "offer"
          )
        },
        # (the database's rule too: option_set_choices_unique)
        problem = function(value, values) {
          same <- edit_sets_with(snap, value, row)
          if (nrow(same) > 0) sprintf("“%s” already has exactly these choices: use that set.", same$name[1])
        }
      )
    )
  },
  initial = function(snap, row, scope) list(answer_option_ids = edit_set_members(snap, row$option_set_id)),
  build = function(mode, snap, row, scope, fields, values) edit_option_set_items(mode, snap, row, fields, values),
  duplicates = function(snap, values) {
    same <- edit_sets_with(snap, values$answer_option_ids)
    if (nrow(same) > 0) list(list(rows = same, why = "An option set with these choices")) else list()
  }
)

# The other option sets with exactly these choices (a field's value: ids joined by commas), but `row`.
edit_sets_with <- function(snap, value, row = NULL) {
  sets <- snap$option_set
  if (!is.null(row)) sets <- sets[sets$option_set_id != row$option_set_id, , drop = FALSE]
  wanted <- edit_several_text(edit_several_ids(value))
  if (!nzchar(wanted)) {
    return(sets[0, , drop = FALSE])
  }
  sets[edit_set_members(snap, sets$option_set_id) == wanted, , drop = FALSE]
}

#' The items of adding or changing an option set: its own row, then the answer choices it gains and loses.
#'
#' @param mode `"add"` or `"update"`.
#' @param snap The tables the kind needs, as `edit_service_load()` reads them: a list of data frames named after the tables.
#' @param row The set being changed; `NULL` for an add.
#' @param fields The form's fields, from `edit_service_fields()`.
#' @param values What is in the form: a named list of strings by field id.
#' @return A data frame of items; see `edit_item()`.
edit_option_set_items <- function(mode, snap, row, fields, values) {
  entity <- edit_entities[["option_set"]]
  own <- Filter(function(field) field$name != "answer_option_ids", fields)
  choices <- Filter(function(field) field$name == "answer_option_ids", fields)[[1]]
  if (mode == "add") {
    set <- "new:option_set:1"
    was <- character()
    # the database numbers it; there is nothing else to say about it
    items <- list(edit_item("option_set", edit_identifier(list(option_set_id = set)), "Insert", NULL, NA, NA))
  } else {
    set <- row$option_set_id
    was <- edit_several_ids(edit_set_members(snap, set))
    items <- list(if (length(own) > 0) edit_update_items(entity, snap, row, own, values))
  }
  now <- if (is.null(choices$locked)) edit_several_ids(values[[choices$id]]) else was
  member <- function(id) edit_identifier(list(answer_option_id = as.numeric(id), option_set_id = set))
  adding <- setdiff(now, was)
  places <- edit_set_places(snap, set, intersect(was, now), adding)
  items <- c(
    items,
    lapply(setdiff(was, now), function(id) edit_item("answer_option_set", member(id), "Delete", "display_order", edit_set_place(snap, set, id), NA)),
    Map(function(id, place) edit_item("answer_option_set", member(id), "Insert", "display_order", NA, place), adding, places)
  )
  items <- Filter(Negate(is.null), items)
  if (length(items) == 0) NULL else do.call(rbind, items)
}

# ------ Measurement scales -------------------------------------------------------------------------------
measurement_scale_entity <- list(
  label = "Measurement scale", noun = "measurement scale", plural = "measurement scales", article = "a",
  group = "Shared lists", table = "measurement_scale",
  about = paste0(
    "A published, validated set of questions scored together, like the CES-D. Its items, the questions that make ",
    "it up, are under Scale Items."
  ),
  delete_about = "Deleting a measurement scale takes away its items too. The questions stay.",
  cards = function(snap, scope) {
    rows <- snap$measurement_scale
    items <- count_in(snap$question_scale$scale_id, rows$scale_id)
    edit_cards(
      key_text(rows$scale_id), rows$name,
      subtitle = paste0(rows$abbreviation, ifelse(is.na(rows$version) | !nzchar(rows$version), "", paste0(" · version ", rows$version))),
      facts = lapply(items, function(n) edit_count(n, "item"))
    )
  },
  fields = function(snap, mode, row, scope) {
    taken <- function(column, what) {
      function(value, values) {
        same <- edit_taken(snap, "measurement_scale", column, value, row)
        if (has_text(value) && nrow(same) > 0) sprintf("“%s” already has this %s.", same$name[1], what)
      }
    }
    list(
      edit_field("name", "Name",
        required = TRUE, placeholder = "e.g. Center for Epidemiologic Studies Depression Scale",
        problem = taken("name", "name")
      ),
      edit_field("abbreviation", "Abbreviation", required = TRUE, placeholder = "e.g. CES-D", problem = taken("abbreviation", "abbreviation")),
      edit_field("version", "Version", placeholder = "e.g. 2nd edition"),
      edit_field("description", "Description", "textarea"),
      edit_field("citation", "Citation", "textarea", hint = "Where the scale was published.")
    )
  },
  duplicates = function(snap, values) {
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "measurement_scale", "name", values$name, "A measurement scale with this name")
    ))
  }
)

# A scale item's number, for sorting: 2 before 10, and 12a after 12.
edit_item_order <- function(numbers) {
  order(suppressWarnings(as.numeric(gsub("[^0-9.].*$", "", numbers))), numbers, method = "radix")
}

scale_scope <- list(table = "measurement_scale", label = "In which measurement scale?")

question_scale_entity <- list(
  label = "Scale item", noun = "scale item", plural = "scale items", article = "a",
  group = "Shared lists", table = "question_scale", scope = scale_scope,
  about = paste0(
    "A question that is an item of a measurement scale, with its item number in the published scale and, if the ",
    "scale says, the choices it expects."
  ),
  update_about = "This changes the item in this scale only. The question itself isn't changed.",
  delete_about = "This only takes the question out of this scale. The question stays, in every questionnaire that asks it.",
  reads = c("option_set", "question", "component", "component_type"),
  cards = function(snap, scope) {
    if (!nzchar(scope %||% "")) {
      return(edit_cards(character(), character()))
    }
    rows <- snap$question_scale[snap$question_scale$scale_id %in% as.numeric(scope), , drop = FALSE]
    rows <- rows[edit_item_order(rows$item_number), , drop = FALSE]
    expected <- edit_lookup(snap, "option_set", "option_set_id", rows$expected_option_set_id, "name")
    edit_cards(
      edit_key(rows, c("scale_id", "component_id")), edit_component_text(snap, rows$component_id),
      subtitle = paste("Item", rows$item_number),
      facts = lapply(expected, function(set) if (!is.na(set)) paste0("expects “", set, "”")),
      tag = "Question"
    )
  },
  fields = function(snap, mode, row, scope) {
    scale <- as.numeric(scope %||% row$scale_id)
    questions <- snap$component[edit_is_question(snap, snap$component$component_id), , drop = FALSE]
    in_scale <- snap$question_scale[snap$question_scale$scale_id %in% scale, , drop = FALSE]
    list(
      edit_field("component_id", "Question", "select",
        required = TRUE, choices = edit_choices(snap, "component", questions, blank = "Choose a question"),
        locked = "An item is its question. To use another question, delete this item and add one.",
        problem = function(value, values) {
          there <- in_scale[in_scale$component_id %in% as.numeric(value), , drop = FALSE]
          if (nrow(there) > 0) sprintf("This question is already item %s of this scale.", there$item_number[1])
        }
      ),
      edit_field("item_number", "Item number",
        required = TRUE, placeholder = "e.g. 3 or 12a",
        problem = function(value, values) {
          same <- edit_taken(snap, "question_scale", "item_number", value, row, within = list(scale_id = scale))
          if (has_text(value) && nrow(same) > 0) {
            sprintf("Item %s of this scale is already “%s”.", same$item_number[1], edit_component_text(snap, same$component_id[1], 50))
          }
        }
      ),
      edit_field("expected_option_set_id", "Expected choices", "select",
        choices = edit_choices(snap, "option_set", blank = "No expected choices"), short = "expected choices",
        hint = "The option set the published scale asks for, if it says."
      )
    )
  },
  build = function(mode, snap, row, scope, fields, values) {
    if (mode == "update") {
      return(edit_update_items(edit_entities[["question_scale"]], snap, row, fields, values))
    }
    question <- Filter(function(field) field$name == "component_id", fields)[[1]]
    key <- edit_identifier(list(
      component_id = as.numeric(edit_service_normalize(question, values[[question$id]])), scale_id = as.numeric(scope)
    ))
    rest <- Filter(function(field) field$name != "component_id", edit_service_active(fields, values))
    items <- lapply(rest, function(field) {
      value <- edit_service_normalize(field, values[[field$id]])
      if (!is.na(value)) edit_item("question_scale", key, "Insert", field$name, NA, value)
    })
    do.call(rbind, Filter(Negate(is.null), items))
  },
  summary_add = function(snap, scope, fields, values) {
    scale <- edit_lookup(snap, "measurement_scale", "scale_id", as.numeric(scope), "name")
    question <- edit_component_text(snap, as.numeric(values[["question_scale__component_id"]]), 60)
    paste0("Added “", question, "” to the measurement scale “", scale, "” as item ", trimws(values[["question_scale__item_number"]]))
  }
)
