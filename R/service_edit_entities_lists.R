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
    "answer is about, like “twin”), a time frame (like “past month”) or a unit (like ",
    "“pounds”), each written one way. The Edit Extraction page offers these, and an extraction adds any other it finds."
  ),
  update_about = paste(
    "A tag is shared. Renaming it renames it everywhere it is used. Giving it the name of another tag of its kind",
    "merges it into that one: everything that carries it carries the other instead."
  ),
  delete_about = paste(
    "A tag can only be deleted once nothing carries it. Until then it can be renamed, or merged into another tag",
    "by giving it that tag's name."
  ),
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
        hint = "Written one way: “Respondent’s Twin” is saved as “twin”, “during the last 12 months” as “past year”.",
        # a new tag can't have the name of one of its kind; a tag renamed to one is merged into it
        problem = function(value, values) {
          kind <- if (has_text(values$kind)) values$kind else row$kind
          if (mode == "update" || !has_text(value) || !has_text(kind)) {
            return(NULL)
          }
          same <- edit_taken(snap, "tag", "name", edit_tag_name(kind, value), row, within = list(kind = kind))
          if (nrow(same) > 0) sprintf("“%s” is already in the list of %ss.", same$name[1], kind)
        },
        warn = function(value, values) {
          merge <- if (mode == "update") tag_merge(snap, row, value)
          if (!is.null(merge)) merge$sentence
        }
      )
    )
  },
  # what carries it, by the question it is on (or whose variable or choice it is on), each opening that question
  usage_rows = function(snap, row, table, rows) {
    about <- edit_question_context(snap, rows)
    matr_id <- if ("variable_key" %in% names(rows)) edit_lookup(snap, "variable", "variable_key", rows$variable_key, "matr_id")
    choice <- if (table == "response_option_tag") edit_lookup(snap, "answer_option", "answer_option_id", rows$answer_option_id, "standardized_label")
    meta <- if (!is.null(choice)) paste0(matr_id, " · ", choice) else matr_id
    # (a tag on a question itself has neither to say beside it)
    by_words <- order(tolower(about$words), about$questionnaire, meta %||% rep("", nrow(rows)), method = "radix")
    list(
      rows = rows[by_words, , drop = FALSE], labels = about$words[by_words], meta = meta[by_words],
      pills = as.list(about$questionnaire[by_words]),
      text = paste("On", switch(table,
        placement_tag = edit_count(nrow(rows), "question"),
        variable_tag = edit_count(nrow(rows), "variable"),
        edit_count(nrow(rows), "answer choice")
      ))
    )
  },
  # a tag given the name of another of its kind is merged into it
  merge = function(snap, row, values) tag_merge(snap, row, values[["tag__name"]]),
  # Merge on its details: into another tag of its kind
  alike = function(snap, row) snap$tag[snap$tag$kind %in% row$kind, , drop = FALSE],
  # a new tag's name is written its kind's way, which the form only knows once the kind is picked
  build = function(mode, snap, row, scope, fields, values) {
    entity <- edit_entities$tag
    if (mode == "update") {
      merge <- tag_merge(snap, row, values[["tag__name"]])
      return(if (is.null(merge)) edit_update_items(entity, snap, row, fields, values) else merge$items)
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

# ------ * Merging two tags ------------------------------------------------------------------------------
# What renaming a tag to the name of another tag of its kind comes to: the two are merged, as Merge on a tag's
# details does it (`edit_service_merge_plan()`).
# Left as it is, a tag merges into nothing, even when its name is one the site now writes as another tag's ("past
# 12 months" beside "past year"): an edit changes only what staff changed.
# row: The tag being changed.
# name: What is typed as its name.
# Returns: `NULL` unless the name, written its kind's way, is another tag's of the same kind. Then the merge's plan,
#   its `sentence` saying what saving does (for the form and the dialog).
tag_merge <- function(snap, row, name) {
  if (is.null(row) || !has_text(name) || identical(trimws(name), trimws(row$name))) {
    return(NULL)
  }
  into <- edit_taken(snap, "tag", "name", edit_tag_name(row$kind, name), row, within = list(kind = row$kind))
  if (nrow(into) == 0) {
    return(NULL)
  }
  plan <- edit_service_merge_plan(edit_entities$tag, snap, row, key_text(into$tag_id[1]))
  what_moves <- if (length(plan$moves) > 0) plan$sentence else sprintf("nothing carries “%s”, so it is only removed.", row$name)
  plan$sentence <- sprintf("“%s” is already in the list of %ss. Saving merges this tag into it: %s", plan$into_name, row$kind, what_moves)
  plan
}

# ------ Measurement scales -------------------------------------------------------------------------------
measurement_scale_entity <- list(
  label = "Measurement scale", noun = "measurement scale", plural = "measurement scales", article = "a",
  group = "Shared lists", table = "measurement_scale",
  about = paste0(
    "A published, validated set of questions scored together, like the CES-D. The questions that make it up are ",
    "listed in its details, where they are added and removed."
  ),
  delete_about = "A measurement scale can only be deleted once it has no questions. Remove them first; the questions themselves stay.",
  reads = "placement", # its questions say which questionnaires ask them
  usage_always = "question_scale", # listed even with none: that is where a question is added
  alike = function(snap, row) snap$measurement_scale,
  alike_label = function(snap, rows) name_with_abbreviation(rows$name, rows$abbreviation),
  cards = function(snap, scope) {
    rows <- snap$measurement_scale
    items <- count_in(snap$question_scale$scale_id, rows$scale_id)
    edit_cards(
      key_text(rows$scale_id), rows$name,
      subtitle = paste0(
        rows$abbreviation, ifelse(is.na(rows$version) | !nzchar(rows$version), "", paste0(" · version ", rows$version)),
        " · ", vapply(items, function(n) edit_count(n, "question"), character(1))
      ),
      facts = lapply(items, function(n) edit_count(n, "question"))
    )
  },
  # its questions by their words in full (`edit_component_words()`), each with the questionnaires that ask it; View
  # opens the question, Remove takes it out of the scale, and a question is added under them
  usage_rows = function(snap, row, table, rows) {
    words <- edit_component_words(snap, rows$component_id)
    by_words <- order(tolower(words), method = "radix")
    rows <- rows[by_words, , drop = FALSE]
    scale <- key_text(row$scale_id)
    list(
      rows = rows, labels = words[by_words], text = edit_count(nrow(rows), "Question"),
      pills = lapply(rows$component_id, function(id) {
        edit_questionnaire_short(snap, unique(snap$placement$questionnaire_id[snap$placement$component_id %in% id]))
      }),
      open = edit_opens("component", rows$component_id),
      remove = edit_opens("question_scale", edit_key(rows, edit_table_key("question_scale")), scale),
      add = list(kind = "question_scale", scope = scale, label = "Add a question")
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

scale_scope <- list(table = "measurement_scale", label = "In which measurement scale?")

# A scale's items are its questions, in no order of their own: a question is in a scale or it isn't. So an item
# has nothing to change: it is added and deleted, from the scale's details (Add a question, and Remove beside each;
# the list has no section for them). A push adds one for a question whose extraction names its scale.
question_scale_entity <- list(
  label = "Scale item", noun = "scale item", plural = "scale items", article = "a",
  group = "Shared lists", table = "question_scale", scope = scale_scope,
  about = "A question that is one of a measurement scale's.",
  delete_about = "This only takes the question out of this scale. The question stays, in every questionnaire that asks it.",
  reads = "component",
  can = function(snap, mode) {
    if (mode == "update") "A scale item is its question and nothing more. To use another question, delete this item and add one."
  },
  cards = function(snap, scope) {
    if (!nzchar(scope %||% "")) {
      return(edit_cards(character(), character()))
    }
    rows <- snap$question_scale[snap$question_scale$scale_id %in% as.numeric(scope), , drop = FALSE]
    rows <- rows[order(edit_component_text(snap, rows$component_id), method = "radix"), , drop = FALSE]
    edit_cards(edit_key(rows, c("scale_id", "component_id")), edit_component_text(snap, rows$component_id), tag = "Question")
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
          if (as.numeric(value) %in% in_scale$component_id) "This question is already an item of this scale."
        }
      )
    )
  },
  build = function(mode, snap, row, scope, fields, values) {
    question <- Filter(function(field) field$name == "component_id", fields)[[1]]
    key <- edit_identifier(list(
      component_id = as.numeric(edit_service_normalize(question, values[[question$id]])), scale_id = as.numeric(scope)
    ))
    edit_item("question_scale", key, "Insert", NULL, NA, NA)
  },
  summary_add = function(snap, scope, fields, values) {
    scale <- edit_lookup(snap, "measurement_scale", "scale_id", as.numeric(scope), "name")
    question <- edit_component_text(snap, as.numeric(values[["question_scale__component_id"]]), 60)
    paste0("Added “", question, "” to the measurement scale “", scale, "”")
  }
)
