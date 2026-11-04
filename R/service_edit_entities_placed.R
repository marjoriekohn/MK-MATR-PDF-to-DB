# Make an edit's kinds in the group In one questionnaire only: where a questionnaire prints a block, and its
# settings for a question. The kinds are listed together in edit_entities (service_edit_kinds.R).

# ------ Inside one questionnaire -------------------------------------------------------------------------
questionnaire_scope <- list(table = "questionnaire", label = "In which questionnaire?")

# A placement's card line: where it sits.
edit_where_text <- function(snap, placed, rows) {
  section <- edit_lookup(snap, "component", "component_id", rows$parent_component_id, "text")
  ifelse(
    is.na(rows$parent_component_id), paste0("top level · position ", rows$display_order),
    paste0("in ", ifelse(is.na(section), "an untitled section", section), " · position ", rows$display_order)
  )
}

questionnaire_component_entity <- list(
  label = "Questionnaire component", noun = "questionnaire component", plural = "questionnaire components", article = "a",
  group = "In one questionnaire only", table = "questionnaire_component", scope = questionnaire_scope,
  about = paste0(
    "A block as it's printed in one questionnaire: where it sits in the outline and its place among ",
    "its neighbours. Changes here only affect that questionnaire."
  ),
  update_about = "You're changing this questionnaire only. Other questionnaires that print the same block are not affected.",
  delete_about = paste0(
    "This only removes the block from this questionnaire. The component stays in the database for ",
    "any other questionnaire that prints it."
  ),
  delete_start = function(snap, row) edit_placement_start(snap, row),
  reads = c("question", "component", "component_type", "wave", "wave_question", "variable", "question_category"),
  initial = function(snap, row, scope) edit_placement_initial(snap, row, scope),
  build = function(mode, snap, row, scope, fields, values) edit_placement_items(mode, snap, row, scope, fields, values),
  cards = function(snap, scope) {
    if (!nzchar(scope %||% "")) {
      return(edit_cards(character(), character()))
    }
    placed <- edit_service_outline(edit_service_placements(snap, as.numeric(scope)))
    elsewhere <- edit_service_questionnaire_count(snap, placed$component_id) - 1
    inside <- vapply(placed$component_id, function(id) sum(placed$parent_component_id %in% id), numeric(1))
    edit_cards(
      edit_key(placed, c("questionnaire_id", "component_id")),
      paste0(strrep("— ", placed$depth), edit_component_text(snap, placed$component_id)),
      subtitle = edit_where_text(snap, placed, placed),
      facts = Map(function(n_inside, n_other, type) {
        c(
          if (type == "Section") edit_count(n_inside, "item") |> paste("inside"),
          if (n_other > 0) paste("also printed in", edit_count(n_other, "other questionnaire"))
        )
      }, inside, elsewhere, placed$type),
      tag = placed$type
    )
  },
  fields = function(snap, mode, row, scope) edit_placement_fields(snap, mode, row, scope)
)

questionnaire_question_entity <- list(
  label = "Questionnaire question", noun = "questionnaire question", plural = "questionnaire questions", article = "a",
  group = "In one questionnaire only", table = "questionnaire_question", scope = questionnaire_scope,
  about = paste0(
    "A question as it's printed in one questionnaire: whether it stands on its own, what it's ",
    "conditional on, and the number it was given in each wave. Changes here only affect that ",
    "questionnaire. A new question comes with its variables and their MATR IDs, typed here, so it is ",
    "added whole."
  ),
  update_about = "You're changing this questionnaire only. Other questionnaires that ask the same question are not affected.",
  delete_about = paste0(
    "This only removes the question from this questionnaire: its place in the outline, its numbers ",
    "in each wave, and its variables with their choices and MATR IDs. The question's wording ",
    "stays in the database for any other questionnaire that asks it."
  ),
  delete_start = function(snap, row) edit_placement_start(snap, row),
  reads = c(
    "question", "component", "component_type", "wave", "wave_question", "variable", "question_category",
    # adding a question (service_edit_question_add.R): who variables can be about, and the answer choices
    # (with the wordings merged into them) and sets of choices there are
    "subject", "answer_option", "answer_option_alias", "option_set", "answer_option_set"
  ),
  initial = function(snap, row, scope) edit_question_initial(snap, row, scope),
  build = function(mode, snap, row, scope, fields, values) {
    if (mode == "add") {
      edit_question_add_items_in(snap, scope, fields, values)
    } else {
      edit_question_items(mode, snap, row, scope, fields, values)
    }
  },
  summary_add = function(snap, scope, fields, values) edit_question_add_summary(snap, scope, fields, values),
  can = function(snap, mode) {
    if (mode == "add" && !is.null(snap$component_type) && is.na(edit_type_id(snap, "Question"))) {
      "The database has no component type called Question, so a question can't be added yet."
    }
  },
  cards = function(snap, scope) {
    if (!nzchar(scope %||% "")) {
      return(edit_cards(character(), character()))
    }
    q <- as.numeric(scope)
    placed <- edit_service_outline(edit_service_placements(snap, q))
    questions <- snap$questionnaire_question[snap$questionnaire_question$questionnaire_id == q, , drop = FALSE]
    placed <- placed[placed$component_id %in% questions$component_id, , drop = FALSE]
    questions <- questions[match(placed$component_id, questions$component_id), , drop = FALSE]
    asked <- snap$wave_question[snap$wave_question$questionnaire_id == q, , drop = FALSE]
    boxes <- snap$variable[snap$variable$questionnaire_id == q, , drop = FALSE]
    elsewhere <- edit_service_questionnaire_count(snap, placed$component_id) - 1
    edit_cards(
      edit_key(placed, c("questionnaire_id", "component_id")),
      edit_component_text(snap, placed$component_id),
      subtitle = vapply(placed$component_id, function(id) {
        mine <- asked[asked$component_id == id, , drop = FALSE]
        if (nrow(mine) == 0) {
          return("not asked in any wave")
        }
        paste0("asked as ", paste0(
          "question ", edit_called_number(mine), " in ", edit_lookup(snap, "wave", "wave_id", mine$wave_id, "name"),
          collapse = ", "
        ))
      }, character(1)),
      facts = Map(function(id, n_other, self_contained, parent) {
        c(
          edit_count(sum(boxes$component_id == id), "variable", "variables"),
          if (isFALSE(self_contained)) paste0("follow-up to “", edit_component_text(snap, parent), "”"),
          if (n_other > 0) paste("also asked in", edit_count(n_other, "other questionnaire"))
        )
      }, placed$component_id, elsewhere, questions$self_contained, questions$parent_question_id),
      tag = "Question"
    )
  },
  fields = function(snap, mode, row, scope) {
    if (mode == "add") edit_question_add_fields(snap, scope) else edit_question_fields(snap, row, scope)
  }
)

# What deleting a block from one questionnaire starts from: its placement, and
# for a question its settings too (they are one block).
edit_placement_start <- function(snap, row) {
  key <- edit_key(row, c("questionnaire_id", "component_id"))
  start <- list(questionnaire_component = key)
  if (key %in% edit_key(snap$questionnaire_question, c("questionnaire_id", "component_id"))) {
    start$questionnaire_question <- key
  }
  start
}

# The form for a block in one questionnaire: its words and kind (which, if
# changed, make a new component for this questionnaire only), and where it sits.
edit_placement_fields <- function(snap, mode, row, scope) {
  q <- as.numeric(scope)
  placed <- edit_service_placements(snap, q)
  section <- edit_type_id(snap, "Section")
  non_question <- snap$component_type[snap$component_type$name != "Question", , drop = FALSE]
  updating <- mode == "update"
  is_question <- updating && edit_is_question(snap, row$component_id)
  has_inside <- updating && any(placed$parent_component_id %in% row$component_id)
  parent_choices <- edit_service_parent_choices(snap, placed, if (updating) row$component_id)

  where <- list(
    edit_field("parent_component_id", "Inside which section or group?", "select",
      choices = parent_choices, short = "section or group it's in",
      hint = "Only sections and groups can hold other blocks.",
      problem = function(value, values) {
        # a section is never inside a group (BR53)
        type <- values$component_type_id %||%
          if (updating) edit_lookup(snap, "component", "component_id", row$component_id, "component_type_id") else NA
        into <- placed$type[match(as.numeric(value), placed$component_id)]
        if (identical(as.numeric(type), section) && has_text(value) && identical(into, "Group")) {
          "A section can't be inside a group. Pick a section, or the top level."
        }
      }
    ),
    edit_field(
      "display_order", "Position", "number",
      required = updating, min = 1, short = "position",
      hint = if (updating) {
        "1 is first. The blocks next to it move over to make room."
      } else {
        "1 is first. The blocks next to it move over to make room. Leave empty to put it last."
      },
      warn = function(value, values) {
        position <- edit_whole_number(value)
        parent <- if (nzchar(values$parent_component_id %||% "")) as.numeric(values$parent_component_id) else NA_real_
        room <- edit_service_room(placed, parent) + if (updating && same_parent(row$parent_component_id, parent)) 0 else 1
        if (!is.na(position) && position > room) sprintf("There are only %d places there, so it will go last (position %d).", room, room)
      }
    )
  )
  if (updating) {
    return(c(list(
      edit_text_field(snap, mode, row),
      edit_field(
        "component_type_id", "Kind of block", "select",
        required = TRUE, choices = edit_choices(snap, "component_type"),
        locked = if (is_question) {
          "A question stays a question. Its settings are under Questionnaire question."
        } else if (has_inside) {
          "A section or group with blocks inside can't change kind. Move them out first."
        },
        hint = if (!is_question && !has_inside) {
          "Changing the kind, like changing the words, gives this questionnaire its own new component."
        }
      )
    ), where))
  }
  candidates <- snap$component[
    !edit_is_question(snap, snap$component$component_id) &
      !snap$component$component_id %in% placed$component_id, ,
    drop = FALSE
  ]
  choices <- stats::setNames(
    key_text(candidates$component_id),
    paste0(
      "[", edit_lookup(snap, "component_type", "component_type_id", candidates$component_type_id, "name"), "] ",
      edit_component_text(snap, candidates$component_id),
      recycle0 = TRUE
    )
  )
  c(list(
    edit_field(
      "source", "Which block?", "choice",
      required = TRUE,
      choices = c("One that's already in the database" = "existing", "A new one" = "new"),
      default = if (nrow(candidates) > 0) "existing" else "new",
      hint = "Placing an existing component keeps it shared with the questionnaires already printing it."
    ),
    edit_field("component_id", "The component", "select",
      required = TRUE, show_when = list(field = "source", value = "existing"),
      choices = c("Choose a component" = "", choices),
      hint = "Questions are added as a questionnaire question (Add, then Questionnaire question), so they aren't listed. Blocks already in this questionnaire aren't either."
    ),
    edit_field("component_type_id", "Kind of block", "select",
      required = TRUE, show_when = list(field = "source", value = "new"),
      choices = edit_choices(snap, "component_type", non_question, blank = "Choose a kind")
    ),
    edit_text_field(snap, mode, NULL) |> utils::modifyList(list(show_when = list(field = "source", value = "new")))
  ), where)
}

# The form for a question in one questionnaire.
edit_question_fields <- function(snap, row, scope) {
  q <- as.numeric(scope)
  asked <- edit_placed_rows(snap, "wave_question", q, row$component_id)
  asked <- asked[order(asked$wave_id), , drop = FALSE]
  numbers <- lapply(seq_len(nrow(asked)), function(i) {
    wave <- asked$wave_id[i]
    wave_name <- edit_lookup(snap, "wave", "wave_id", wave, "name")
    edit_field(
      paste0("qn_", key_text(wave)), paste("Number printed in", wave_name),
      placeholder = "None printed", short = paste("number printed in", wave_name),
      hint = if (i == 1) {
        paste(
          "As printed, so it can have letters, and repeat from section to section: 1, 2a, 14. Leave it empty if",
          "none is printed. Its place among the questions is worked out from the outline."
        )
      }
    )
  })
  c(list(
    edit_text_field(snap, "update", row, "Question wording", type_field = FALSE) |>
      utils::modifyList(list(hint = paste0(
        "Changing the wording gives this questionnaire its own new question with the new words. Other ",
        "questionnaires keep the old one."
      ))),
    edit_field(
      "self_contained", "Can a researcher understand it on its own?", "choice",
      choices = c("Yes" = "TRUE", "No, it follows up on another question" = "FALSE"),
      short = "whether it stands on its own"
    ),
    edit_field(
      "parent_question_id", "Which question does it follow up on?", "select",
      required = TRUE,
      short = "the question it follows up on",
      show_when = list(field = "self_contained", value = "FALSE"),
      choices = c("Choose a question" = "", edit_service_parent_question_choices(snap, q, row$component_id)),
      hint = "Only questions printed before this one are listed."
    ),
    edit_field("condition", "Printed condition",
      placeholder = "e.g. If you answered Yes to the previous question",
      hint = "Leave empty if the question is asked of everyone."
    )
  ), numbers)
}
