# Make an edit's kind in the group In one questionnaire only: a block as one questionnaire prints it. The kinds are
# listed together in edit_entities (service_edit_kinds.R).

# ------ Inside one questionnaire -------------------------------------------------------------------------
questionnaire_scope <- list(table = "questionnaire", label = "In which questionnaire?")

# A placement's card line: where it sits.
edit_where_text <- function(snap, rows) {
  holder <- edit_component_text(snap, edit_placement_component(snap, rows$parent_placement_id))
  ifelse(
    is.na(rows$parent_placement_id), paste0("top level · position ", rows$display_order),
    paste0("in ", holder, " · position ", rows$display_order)
  )
}

placement_entity <- list(
  label = "Block", noun = "block", plural = "blocks", article = "a",
  group = "In one questionnaire only", table = "placement", scope = questionnaire_scope,
  about = paste0(
    "A block as it's printed in one questionnaire: where it sits in the outline, and for a question the question ",
    "it depends on, the number it was given in each wave, and its variables with their MATR IDs and choices. ",
    "Changes here only affect that questionnaire."
  ),
  update_about = "You're changing this questionnaire only. Other questionnaires that print the same block are not affected.",
  delete_about = paste0(
    "This only removes the block from this place in the questionnaire, with everything inside it, its numbers in ",
    "each wave and its variables with their choices. The wording stays in the database while another ",
    "questionnaire prints it or a measurement scale lists it; otherwise it is forgotten."
  ),
  reads = c(
    "component", "wave", "wave_question", "variable", "variable_tag", "tag", "answer_option", "response_option",
    "placement_tag", "question_scale"
  ),
  initial = function(snap, row, scope) edit_placement_initial(snap, row, scope),
  build = function(mode, snap, row, scope, fields, values) edit_placement_items(mode, snap, row, scope, fields, values),
  summary_add = function(snap, scope, fields, values) edit_placement_add_summary(snap, scope, fields, values),
  cards = function(snap, scope) {
    if (!nzchar(scope %||% "")) {
      return(edit_cards(character(), character()))
    }
    q <- as.numeric(scope)
    placed <- edit_service_outline(edit_service_placements(snap, q))
    elsewhere <- edit_service_questionnaire_count(snap, placed$component_id) - 1
    inside <- vapply(placed$placement_id, function(id) sum(placed$parent_placement_id %in% id), numeric(1))
    boxes <- count_in(snap$variable$placement_id, placed$placement_id)
    asked <- snap$wave_question[snap$wave_question$questionnaire_id == q, , drop = FALSE]
    edit_cards(
      edit_key(placed, "placement_id"),
      paste0(strrep("— ", placed$depth), edit_component_text(snap, placed$component_id)),
      subtitle = vapply(seq_len(nrow(placed)), function(i) {
        mine <- asked[asked$placement_id == placed$placement_id[i], , drop = FALSE]
        if (nrow(mine) > 0) {
          paste0("asked as ", paste0(
            "question ", edit_called_number(mine), " in ", edit_lookup(snap, "wave", "wave_id", mine$wave_id, "name"),
            collapse = ", "
          ))
        } else if (placed$kind[i] %in% "Question") {
          "not asked in any wave"
        } else {
          edit_where_text(snap, placed[i, , drop = FALSE])
        }
      }, character(1)),
      facts = Map(function(kind, n_inside, n_boxes, n_other, depends_on) {
        c(
          if (is_container(kind)) edit_count(n_inside, "block") |> paste("inside"),
          if (kind %in% "Question") edit_count(n_boxes, "variable", "variables"),
          if (!is.na(depends_on)) paste0("depends on “", edit_component_text(snap, edit_placement_component(snap, depends_on), 40), "”"),
          if (n_other > 0) paste("also printed in", edit_count(n_other, "other questionnaire"))
        )
      }, placed$kind, inside, boxes, elsewhere, placed$depends_on_placement_id),
      tag = placed$kind
    )
  },
  fields = function(snap, mode, row, scope) {
    if (mode == "add") edit_placement_add_fields(snap, scope) else edit_placement_update_fields(snap, row, scope)
  }
)

# The fields that say where a block sits: inside what, and in which position.
edit_placement_where_fields <- function(snap, placed, row = NULL, kind_of = function(values) values$kind) {
  updating <- !is.null(row)
  list(
    edit_field("parent_placement_id", "Inside which section, grid or group?", "select",
      choices = edit_service_parent_choices(snap, placed, if (updating) row$placement_id), short = "section, grid or group it's in",
      hint = "Only sections, grids and groups can hold other blocks.",
      problem = function(value, values) {
        # what each container can hold (placement_rules in the database, can_hold() here)
        kind <- kind_of(values)
        into <- placed$kind[match(as.numeric(value), placed$placement_id)]
        if (has_text(value) && has_text(kind) && !is.na(into) && !can_hold(into, kind)) {
          if (identical(into, "Grid")) {
            "A grid holds only its questions, with their instructions and definitions. Pick somewhere else."
          } else {
            "A section can't be inside a group. Pick a section, or the top level."
          }
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
        parent <- if (has_text(values$parent_placement_id)) as.numeric(values$parent_placement_id) else NA_real_
        room <- edit_service_room(placed, parent) + if (updating && same_parent(row$parent_placement_id, parent)) 0 else 1
        if (!is.na(position) && position > room) sprintf("There are only %d places there, so it will go last (position %d).", room, room)
      }
    )
  )
}

# The form for a block in one questionnaire: its words (which, if changed, point this placement at the component
# with them), where it sits, and for a question what it depends on and its number in each wave that asks it.
edit_placement_update_fields <- function(snap, row, scope) {
  q <- as.numeric(scope)
  placed <- edit_service_placements(snap, q)
  kind <- edit_lookup(snap, "component", "component_id", row$component_id, "kind")
  c(
    list(
      edit_field(
        "kind", "Kind of block", "select",
        required = TRUE, choices = stats::setNames(edit_component_kinds, edit_component_kinds),
        locked = "A block's kind is fixed. To print another kind of block here, delete this one and add one."
      ),
      edit_text_field(snap, NULL, kind_of = function(values) kind) |>
        utils::modifyList(list(hint = paste0(
          "Changing the words changes this place only: it prints the block with the new words (the one the database ",
          "has, or a new one), and other places keep the old one."
        )))
    ),
    edit_placement_where_fields(snap, placed, row, kind_of = function(values) kind),
    if (identical(kind, "Question")) {
      c(
        list(edit_field(
          "depends_on_placement_id", "Which question does it depend on?", "select",
          short = "question it depends on",
          choices = c("None: it stands on its own" = "", edit_service_depends_choices(snap, q, row$placement_id)),
          hint = "Only questions printed before this one are listed."
        )),
        edit_placement_number_fields(snap, edit_placed_rows(snap, row$placement_id)$wave_id, asked_field = FALSE)
      )
    }
  )
}

# The form for adding a block to one questionnaire. The fields a question needs (what it depends on, the waves that
# ask it and the number printed beside it in each, its variables) show once Question is picked as the kind.
edit_placement_add_fields <- function(snap, scope) {
  q <- as.numeric(scope)
  placed <- edit_service_placements(snap, q)
  waves <- snap$wave[snap$wave$questionnaire_id == q, , drop = FALSE]
  waves <- waves[order(waves$wave_id), , drop = FALSE]
  only_questions <- function(field) utils::modifyList(field, list(show_when = list(field = "kind", value = "Question")))
  c(
    list(
      edit_field(
        "kind", "Kind of block", "select",
        required = TRUE, choices = c("Choose a kind" = "", stats::setNames(edit_component_kinds, edit_component_kinds)),
        hint = "A question comes with its variables and the waves that ask it, typed below once Question is picked."
      ),
      edit_text_field(snap, NULL) |>
        utils::modifyList(list(hint = "The block's words only, as printed, without its number. A block with exactly these words shares them."))
    ),
    edit_placement_where_fields(snap, placed),
    list(
      only_questions(edit_field(
        "depends_on_placement_id", "Which question does it depend on?", "select",
        short = "question it depends on",
        choices = c("None: it stands on its own" = "", edit_service_depends_choices(snap, q)),
        hint = "Pick one printed before where this one goes."
      )),
      only_questions(edit_field(
        "asked_in", "Waves that ask it", "several",
        required = TRUE, short = "waves that ask it",
        choices = stats::setNames(key_text(waves$wave_id), waves$name),
        hint = "A question is asked in at least one wave. Its place among the wave's questions is worked out from where it goes in the outline.",
        problem = function(value, values) {
          if (nrow(waves) == 0) "This questionnaire has no waves yet. Add one first (Add, then Wave): a question is asked in a wave."
        }
      ))
    ),
    lapply(edit_placement_number_fields(snap, waves$wave_id, asked_field = TRUE), only_questions),
    list(only_questions(edit_field(
      "variables", "Variables", "variables",
      required = TRUE, default = edit_variables_default(),
      options = edit_variables_options(snap),
      hint = paste(
        "One for each answer a respondent can record, in printed order, each with its MATR ID. A lone check",
        "box has one choice: only choices printed on the form are choices."
      ),
      problem = edit_variables_problem(snap, q)
    )))
  )
}

# One field for each wave: the number printed beside the question there (which can be empty, or repeat from
# section to section).
# wave_ids: The waves.
# asked_field: Whether the form also asks which waves ask it (an add): then the hint says an unpicked wave's
#   number is ignored.
edit_placement_number_fields <- function(snap, wave_ids, asked_field) {
  wave_ids <- sort(wave_ids)
  lapply(seq_along(wave_ids), function(i) {
    wave_name <- edit_lookup(snap, "wave", "wave_id", wave_ids[i], "name")
    edit_field(
      paste0("qn_", key_text(wave_ids[i])), paste("Number printed in", wave_name),
      placeholder = "None printed", short = paste("number printed in", wave_name),
      hint = if (i == 1) {
        paste(
          "As printed, so it can have letters, and repeat from section to section: 1, 2a, 14. Leave it empty if",
          "none is printed.", if (asked_field) "Only the waves picked above count."
        )
      }
    )
  })
}
