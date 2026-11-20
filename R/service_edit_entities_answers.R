# Make an edit's answer choices (group Shared lists). The kinds are listed together in edit_entities
# (service_edit_kinds.R).

# ------ Answer choices --------------------------------------------------------------------------------
answer_option_entity <- list(
  label = "Answer choice", noun = "answer choice", plural = "answer choices", article = "an",
  group = "Shared lists", table = "answer_option",
  about = paste0(
    "One answer a question can offer, like “yes” or “sometimes”, kept once for the whole registry in its normal ",
    "form. Variables use it with their own code."
  ),
  update_about = "An answer choice is shared. Rewording it rewords it on every variable that offers it; their codes stay as they are.",
  delete_about = "An answer choice can only be deleted once no variable offers it.",
  reads = c("variable", "response_option_tag", "placement", "component"), # each variable that offers it says its question
  alike = function(snap, row) snap$answer_option,
  cards = function(snap, scope) {
    rows <- snap$answer_option
    on_boxes <- count_in(snap$response_option$answer_option_id, rows$answer_option_id)
    edit_cards(
      key_text(rows$answer_option_id), rows$standardized_label,
      facts = lapply(on_boxes, function(b) if (b > 0) paste("offered on", edit_count(b, "variable", "variables")) else "on no variables")
    )
  },
  # the variables that offer it, by their question: a choice like "k" only makes sense beside what was asked
  # ("what grade is your child in?"). Each says its Variable ID and the code the choice has there, and opens the question.
  usage_rows = function(snap, row, table, rows) {
    if (table != "response_option") {
      return(NULL)
    }
    about <- edit_question_context(snap, rows)
    matr_id <- edit_lookup(snap, "variable", "variable_key", rows$variable_key, "matr_id")
    by_words <- order(tolower(about$words), about$questionnaire, matr_id, method = "radix")
    list(
      rows = rows[by_words, , drop = FALSE], labels = about$words[by_words],
      meta = paste0(matr_id, " · code ", rows$code_value)[by_words], pills = as.list(about$questionnaire[by_words]),
      text = paste("Offered by", edit_count(nrow(rows), "variable"))
    )
  },
  fields = function(snap, mode, row, scope) {
    list(
      edit_field("standardized_label", "Answer choice",
        required = TRUE, placeholder = "e.g. sometimes", normalize = normal_answer,
        hint = paste0(
          "Saved in lower case, without a question mark or a full stop or colon at the end. A range of numbers is ",
          "written “1-2”."
        ),
        problem = function(value, values) {
          if (!has_text(value)) {
            return(NULL)
          }
          same <- edit_taken(snap, "answer_option", "standardized_label", normal_answer(value), row)
          if (nrow(same) > 0) sprintf("“%s” is already in the list.", same$standardized_label[1])
        }
      )
    )
  },
  duplicates = function(snap, values) {
    Filter(Negate(is.null), list(
      edit_duplicate_rows(snap, "answer_option", "standardized_label", normal_answer(values$standardized_label), "An answer choice with these words")
    ))
  }
)
