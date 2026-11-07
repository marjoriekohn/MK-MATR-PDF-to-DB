# What the Edit Extraction page's tests share (test-edit_extraction_server*.R): the page's arguments, a draft
# read as a new wave of the tree's questionnaire, and the draft as the page shows it.

# The page's lookup tables are the file's (shared_lookups()): they are the same for every test.
edit_page <- function(me, extractions = test_extractions(), catalog = shared_catalog(env), env = parent.frame()) {
  shared_lookups(env)
  list(extractions = extractions, catalog = catalog, connection = test_connection, me = me)
}

# A draft that has been read, as a new wave of the tree's questionnaire. What it read is the first two parts
# of the gold questionnaire (short_gold_json(), as local_read_extraction() saves): the page draws every block
# of the draft it opens, and all of the gold questionnaire's take seconds to draw.
read_draft <- function(me, tree, env = parent.frame()) {
  local_read_extraction(me, info = picked_basic_info(tree), env = env)
}

# A draft with one block's words changed, as staff would change them.
reworded <- function(draft, words = "do you worry a lot?", id = "c3") {
  draft_service_update(draft, "components", id, "text", words)
}

# The draft read_draft() makes as the page shows it: its questions numbered by their place.
numbered_gold <- function() draft_service_renumber_questions(short_gold_draft())

# Fills in the MATR IDs of the draft on the page, as staff would type them, so nothing is left to fix. Called
# inside testServer(), where `draft` and `change_draft` are the page's.
fill_all_matr_ids <- function(draft, change_draft) {
  filled <- draft()
  filled$variables$matr_id <- sprintf("m%03d", seq_len(nrow(filled$variables)))
  change_draft(filled, keep_checked = TRUE)
}
