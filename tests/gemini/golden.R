# Reads the hand-checked questionnaires with Gemini and compares each reading
# with its known answer. Rerun it whenever prompts/extraction.md changes. It
# calls the Gemini API (a few minutes and a few cents a questionnaire), so it
# isn't one of the unit tests. From the project folder:
#
#   Rscript tests/gemini/golden.R              every questionnaire
#   Rscript tests/gemini/golden.R gna out/     just GNA, saving the reading in out/
#
# A reading saved earlier can be compared again without calling Gemini:
#   Rscript tests/gemini/golden.R gna out/ --saved
#
# The PDFs are in edge-case-pdfs/ (kept out of git). The prompt's lists (kinds of block, subjects, answer
# choices) come from the test project, which gets a new kind such as Grid before live does.

golden <- list(
  gna = list(pdf = "edge-case-pdfs/GNA_Redacted.pdf",
             answer = "tests/testthat/fixtures/gna_gold.json")
)

# ------ Comparing a reading with its answer -------------------------------------------
# Each question is matched to the answer's question with the same number, and
# every field of it, its variables and their choices is compared. Blocks
# that aren't questions are compared in order. Returns one line per difference
# and the share of fields that match.
golden_compare <- function(reading, answer) {
  got <- golden_blocks(reading$components)
  want <- golden_blocks(answer$components)
  differences <- character()
  n_fields <- 0
  n_same <- 0
  same <- function(where, field, got_value, want_value) {
    n_fields <<- n_fields + 1
    if (identical(got_value, want_value)) {
      n_same <<- n_same + 1
    } else {
      differences <<- c(differences, sprintf("%s %s: got %s, want %s", where, field, shown(got_value), shown(want_value)))
    }
  }

  got_numbers <- names(got$questions)
  want_numbers <- names(want$questions)
  for (number in setdiff(want_numbers, got_numbers)) {
    differences <- c(differences, sprintf("Q%s missing: %s", number, want$questions[[number]]$text))
  }
  for (number in setdiff(got_numbers, want_numbers)) {
    differences <- c(differences, sprintf("Q%s extra: %s", number, got$questions[[number]]$text))
  }
  n_fields <- n_fields + length(union(got_numbers, want_numbers))
  n_same <- n_same + length(intersect(got_numbers, want_numbers))

  for (number in intersect(want_numbers, got_numbers)) {
    g <- got$questions[[number]]
    w <- want$questions[[number]]
    where <- paste0("Q", number)
    for (field in c("text", "parent_question", "section", "grid")) {
      same(where, field, g[[field]], w[[field]])
    }
    same(where, "variables", length(g$variables), length(w$variables))
    for (i in seq_len(min(length(g$variables), length(w$variables)))) {
      gv <- g$variables[[i]]
      wv <- w$variables[[i]]
      box <- sprintf("%s box %d", where, i)
      for (field in c("variable_type", "label", "subject", "time_frame", "answer_data_type", "unit")) {
        same(box, field, gv[[field]], wv[[field]])
      }
      same(box, "choices", choice_field(gv, "answer_option"), choice_field(wv, "answer_option"))
    }
  }

  for (block in setdiff(want$others, got$others)) differences <- c(differences, paste("missing", block))
  for (block in setdiff(got$others, want$others)) differences <- c(differences, paste("extra", block))
  n_fields <- n_fields + length(union(got$others, want$others))
  n_same <- n_same + length(intersect(got$others, want$others))
  same("Questionnaire", "name", reading$questionnaire$name, answer$questionnaire$name)
  same("Questionnaire", "abbreviation", reading$questionnaire$abbreviation, answer$questionnaire$abbreviation)
  list(differences = differences, score = n_same / n_fields, n_fields = n_fields)
}

# Questions by printed number, each noting the section it's in (named by its first
# child) and the lead-in of the grid it's in; every other block as "Type: text". A group is
# looked inside, and isn't compared itself (it has no words).
golden_blocks <- function(blocks, section = "top level", questions = list(), others = character(), grid = NULL) {
  for (block in blocks) {
    if (block$component_type == "Question") {
      block$section <- section
      block$grid <- grid
      questions[[block$printed_number %||% paste0("(unnumbered) ", block$text)]] <- block
    } else if (block$component_type %in% c("Group", "Grid")) {
      if (block$component_type == "Grid") others <- c(others, paste0("Grid: ", block$text))
      inner <- golden_blocks(block$children, section, questions, others, if (block$component_type == "Grid") block$text else grid)
      questions <- inner$questions
      others <- inner$others
    } else if (block$component_type == "Section") {
      first <- if (length(block$children)) block$children[[1]] else list(text = "nothing (empty)")
      label <- paste0(
        "section starting ",
        if (is.null(first$printed_number)) substr(first$text %||% "", 1, 40) else paste0("Q", first$printed_number)
      )
      others <- c(others, paste0(label, ": ", block$text %||% "(no heading)"))
      inner <- golden_blocks(block$children, label, questions, others)
      questions <- inner$questions
      others <- inner$others
    } else {
      others <- c(others, paste0(block$component_type, ": ", block$text))
    }
  }
  list(questions = questions, others = others)
}

choice_field <- function(box, field) {
  vapply(box$response_options, function(choice) paste(choice[[field]] %||% "-", collapse = ""), character(1))
}

shown <- function(value) {
  if (is.null(value)) {
    return("null")
  }
  if (length(value) > 1) {
    return(paste0("[", paste(value, collapse = " | "), "]"))
  }
  if (is.character(value)) {
    return(paste0('"', value, '"'))
  }
  as.character(value)
}

# ------ Running it --------------------------------------------------------------------
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  names_wanted <- setdiff(args[!grepl("/$|^--", args)], "")
  out_dir <- c(args[grepl("/$", args)], tempdir())[1]
  use_saved <- "--saved" %in% args
  for (path in list.files("R", "\\.[Rr]$", full.names = TRUE)) source(path)
  if (file.exists(".env")) readRenviron(".env")
  lookups <- lookup_service_get_all(list(url = Sys.getenv("TEST_SUPABASE_URL"), key = Sys.getenv("TEST_SUPABASE_SECRET_KEY")))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  for (name in if (length(names_wanted)) names_wanted else names(golden)) {
    case <- golden[[name]]
    out_path <- file.path(out_dir, paste0(name, ".json"))
    if (use_saved) {
      json <- jsonlite::read_json(out_path, simplifyVector = FALSE)
    } else {
      started <- Sys.time()
      reading <- reading_service_read(case$pdf, lookups, progress = function(status) {
        cat(format(Sys.time(), "%H:%M:%S"), status, "\n")
      })
      json <- reading$json
      jsonlite::write_json(json, out_path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
      cat(sprintf(
        "\n%s read in %.0f s by %s, prompt %s; %d problems left\ntokens: %s in, %s out, %s thinking; most in one answer %s of 65536\n",
        name, difftime(Sys.time(), started, units = "secs"), reading$model, reading$prompt_version,
        length(reading$problems), reading$tokens$input, reading$tokens$output, reading$tokens$thinking,
        reading$tokens$most_in_one_answer
      ))
      if (length(reading$problems)) cat(paste("  problem:", reading$problems), sep = "\n")
      json <- jsonlite::read_json(out_path, simplifyVector = FALSE) # as the Edit Extraction page would load it
    }
    result <- golden_compare(json, jsonlite::read_json(case$answer, simplifyVector = FALSE))
    cat(sprintf("%s: %.1f%% of %d fields match. Reading saved to %s\n", name, 100 * result$score, result$n_fields, out_path))
    cat(paste(" ", result$differences), sep = "\n")
  }
}
