# Make an edit's kind in the group Printed content: the components every questionnaire that prints them shares.
# The kinds are listed together in edit_entities (service_edit_kinds.R).

# ------ Kinds of block ---------------------------------------------------------------------------------
# The kinds of block a questionnaire prints (component.kind; component_kind_check in the database). A Section,
# Grid or Group holds other blocks; a Group has no words of its own.
edit_component_kinds <- c("Section", "Group", "Grid", "Question", "Instruction", "Introduction", "Definition", "Note", "Interviewer Script")

# ------ Printed content: shared by every questionnaire that prints it ------------------------------
# Whether components are questions.
# component_ids: Component ids.
edit_is_question <- function(snap, component_ids) {
  edit_lookup(snap, "component", "component_id", component_ids, "kind") %in% "Question"
}

# "seen in 2 questionnaires: AHP, MQ"; "not printed in any questionnaire".
edit_seen_in <- function(snap, component_id) {
  q_ids <- unique(snap$placement$questionnaire_id[snap$placement$component_id == component_id])
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

# A block's words as they are kept: lower case (normal_text()), one space between words.
edit_words <- function(value) {
  words <- normal_text(value)
  if (length(words) == 0 || is.na(words)) words else squish(words)
}

# A block's words. Every kind has words but a Group, which has none of its own. `kind_of(values)` says which kind
# the form's other fields make it, so a Group is checked for having none.
edit_text_field <- function(snap, row, kind_of = function(values) values$kind, label = "Printed words") {
  edit_field(
    "text", label, "textarea",
    rows = 3, short = "words", normalize = edit_words,
    problem = function(value, values) {
      kind <- kind_of(values)
      if (!has_text(value) && !identical(kind, "Group")) {
        return("Add the printed words. Only a group has none.")
      }
      if (has_text(value) && identical(kind, "Group")) {
        "A group has no words of its own. Put them in an Instruction inside it."
      }
    },
    warn = function(value, values) {
      if (!has_text(value)) {
        return(NULL)
      }
      same <- edit_same_component(snap, kind_of(values), value, row)
      if (nrow(same) > 0) {
        paste0(
          "A ", tolower(kind_of(values) %||% "block"), " with exactly these words is in the database already (",
          edit_seen_in(snap, same$component_id[1]), "): it is used, so the wording stays shared."
        )
      }
    }
  )
}

# The component of this kind with exactly these words, but `row`: the database keeps one for each kind and words,
# so a change that types them means that one.
# Returns: Its row of `component`, or none.
edit_same_component <- function(snap, kind, text, row = NULL) {
  same <- edit_taken(snap, "component", "text", edit_words(text), row)
  same[same$kind %in% kind, , drop = FALSE]
}

component_entity <- list(
  label = "Component", noun = "component", plural = "components", article = "a",
  group = "Printed content", table = "component",
  about = paste0(
    "A heading, question, instruction, definition or any other block as it is printed. Each wording is kept ",
    "once, and every questionnaire that prints it uses that one copy."
  ),
  update_about = "A component is shared. Changing its words changes them in every questionnaire that prints it.",
  delete_about = paste(
    "A component can only be deleted once no questionnaire prints it and no measurement scale lists it. One that",
    "nothing prints goes on its own when the next change is made."
  ),
  # its card in each questionnaire lists its variables: who each is about, and its choices
  reads = c("variable", "variable_tag", "tag", "answer_option", "response_option", "wave", "wave_question"),
  cards = function(snap, scope) {
    rows <- snap$component
    boxes <- count_in(edit_placement_component(snap, snap$variable$placement_id), rows$component_id)
    edit_cards(
      key_text(rows$component_id), edit_component_text(snap, rows$component_id),
      subtitle = "",
      facts = Map(
        function(id, n) c(edit_seen_in(snap, id), if (n > 0) edit_count(n, "variable", "variables")),
        rows$component_id,
        boxes
      ),
      tag = rows$kind, warn = edit_shared_warning(snap, rows$component_id)
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field(
        "kind", "Kind of block", "select",
        required = TRUE,
        choices = c("Choose a kind" = "", stats::setNames(edit_component_kinds, edit_component_kinds)),
        locked = if (mode == "update") "A component's kind is fixed. To print a different kind of block, add one."
      ),
      edit_text_field(snap, row, kind_of = function(values) if (has_text(values$kind)) values$kind else if (!is.null(row)) row$kind)
    )
  },
  duplicates = function(snap, values) {
    same <- edit_same_component(snap, values$kind, values$text)
    if (has_text(values$text) && nrow(same) > 0) list(list(rows = same, why = "A block of this kind with exactly these words")) else list()
  }
)
