# Make an edit's other kinds in the group Shared lists: tags, measurement scales and their items. The kinds are
# listed together in edit_entities (service_edit_kinds.R).

# ------ Tags ------------------------------------------------------------------------------------------
# A tag's kinds (tag_kind_check in the database), and how each kind's name is written one way.
edit_tag_kinds <- c("subject", "time frame", "unit")
edit_tag_normalize <- list(subject = normal_subject_words, "time frame" = normal_time_frame, unit = normal_unit)

# A tag's name as it is saved: by its kind's rule, straight quotes for all.
edit_tag_name <- function(kind, name) {
  normalize <- edit_tag_normalize[[kind %||% ""]] %||% straight_quotes
  straight_quotes(normalize(name))
}

tag_entity <- list(
  label = "Tag", noun = "tag", plural = "tags", article = "a",
  group = "Shared lists", table = "tag",
  about = paste0(
    "A word a researcher can filter by, put on a question, a variable or an answer choice: a subject (who the ",
    "answer is about, like “twin”), a time frame (like “past 30 days”) or a unit (like ",
    "“pounds”), each written one way. The Edit Extraction page offers these, and an extraction adds any other it finds."
  ),
  update_about = "A tag is shared. Renaming it renames it everywhere it is used.",
  delete_about = "A tag can only be deleted once nothing carries it. Until then it can be renamed instead.",
  reads = c("variable", "placement", "component", "answer_option"),
  cards = function(snap, scope) {
    rows <- snap$tag
    on <- count_in(snap$variable_tag$tag_id, rows$tag_id) + count_in(snap$placement_tag$tag_id, rows$tag_id) +
      count_in(snap$response_option_tag$tag_id, rows$tag_id)
    edit_cards(
      key_text(rows$tag_id), rows$name,
      facts = lapply(on, function(n) if (n > 0) paste("on", edit_count(n, "thing")) else "on nothing yet"),
      tag = rows$kind
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("kind", "Kind of tag", "select",
        required = TRUE,
        choices = c("Choose a kind" = "", stats::setNames(edit_tag_kinds, edit_tag_kinds)),
        locked = if (mode == "update") "A tag's kind is fixed. To tag things another way, add a tag of that kind."
      ),
      edit_field("name", "Name",
        required = TRUE, placeholder = "e.g. parents, past six months, pounds",
        normalize = function(value) edit_tag_name(if (!is.null(row)) row$kind, value),
        hint = "Written one way: “Respondent’s Twin” is saved as “twin”, “during the last 12 months” as “past 12 months”.",
        problem = function(value, values) {
          kind <- if (has_text(values$kind)) values$kind else row$kind
          if (!has_text(value) || !has_text(kind)) {
            return(NULL)
          }
          same <- edit_taken(snap, "tag", "name", edit_tag_name(kind, value), row, within = list(kind = kind))
          if (nrow(same) > 0) sprintf("“%s” is already in the list of %ss.", same$name[1], kind)
        }
      )
    )
  },
  # a new tag's name is written its kind's way, which the form only knows once the kind is picked
  build = function(mode, snap, row, scope, fields, values) {
    entity <- edit_entities$tag
    if (mode == "update") {
      return(edit_update_items(entity, snap, row, fields, values))
    }
    kind <- values[["tag__kind"]]
    fields <- lapply(fields, function(field) {
      if (field$name == "name") field$normalize <- function(value) edit_tag_name(kind, value)
      field
    })
    edit_add_items(entity, snap, fields, values)
  },
  duplicates = function(snap, values) {
    kind <- if (has_text(values$kind)) list(kind = values$kind)
    name <- if (has_text(values$name)) edit_tag_name(values$kind, values$name)
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "tag", "name", name, "A tag of this kind with this name", within = kind)
    ))
  }
)

# ------ Measurement scales -------------------------------------------------------------------------------
measurement_scale_entity <- list(
  label = "Measurement scale", noun = "measurement scale", plural = "measurement scales", article = "a",
  group = "Shared lists", table = "measurement_scale",
  about = paste0(
    "A published, validated set of questions scored together, like the CES-D. Its items, the questions that make ",
    "it up, are under Scale Items."
  ),
  delete_about = "A measurement scale can only be deleted once it has no items. The questions stay.",
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
  about = "A question that is an item of a measurement scale, with its item number in the published scale.",
  update_about = "This changes the item in this scale only. The question itself isn't changed.",
  delete_about = "This only takes the question out of this scale. The question stays, in every questionnaire that asks it.",
  reads = "component",
  cards = function(snap, scope) {
    if (!nzchar(scope %||% "")) {
      return(edit_cards(character(), character()))
    }
    rows <- snap$question_scale[snap$question_scale$scale_id %in% as.numeric(scope), , drop = FALSE]
    rows <- rows[edit_item_order(rows$item_number), , drop = FALSE]
    edit_cards(
      edit_key(rows, c("scale_id", "component_id")), edit_component_text(snap, rows$component_id),
      subtitle = paste("Item", rows$item_number),
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
    number <- Filter(function(field) field$name == "item_number", fields)[[1]]
    edit_item("question_scale", key, "Insert", "item_number", NA, edit_service_normalize(number, values[[number$id]]))
  },
  summary_add = function(snap, scope, fields, values) {
    scale <- edit_lookup(snap, "measurement_scale", "scale_id", as.numeric(scope), "name")
    question <- edit_component_text(snap, as.numeric(values[["question_scale__component_id"]]), 60)
    paste0("Added “", question, "” to the measurement scale “", scale, "” as item ", trimws(values[["question_scale__item_number"]]))
  }
)
