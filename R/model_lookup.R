# ------ Lookup lists ----------------------------------------------------------------
# The lists the Edit Extraction page's pick lists and the reading instructions use. The words staff
# tag variables with (who a variable is about, when it is about, what unit a number is in) are rows
# of TAG, one list with a kind each, which the push adds to as staff type new ones. The standard
# answer choices are ANSWER_OPTION. `connection` is the MATR database (model_database.R).

# Every tag: the subjects, time frames and units there are.
# Returns: A data frame with `tag_id`, `kind` ("subject", "time frame" or "unit") and `name`.
lookup_model_get_tags <- function(connection) {
  database_model_get(connection, "tag", c("tag_id", "kind", "name"), order_by = c("kind", "name"))
}

# The standard answer choices.
# Returns: A data frame with `answer_option_id` and `standardized_label`.
lookup_model_get_answer_options <- function(connection) {
  database_model_get(
    connection, "answer_option", c("answer_option_id", "standardized_label"),
    order_by = "answer_option_id"
  )
}
