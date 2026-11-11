
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MATR-PDF-to-DB Admin Website

<!-- badges: start -->

[![tests](https://github.com/vcu-cara/MATR-PDF-to-DB/actions/workflows/tests.yaml/badge.svg)](https://github.com/vcu-cara/MATR-PDF-to-DB/actions/workflows/tests.yaml)
[![coverage](https://github.com/vcu-cara/MATR-PDF-to-DB/blob/badges/coverage.svg?raw=true)](https://github.com/vcu-cara/MATR-PDF-to-DB/actions/workflows/tests.yaml)

<!-- badges: end -->

A R Shiny website for staff of MATR (the Mid-Atlantic Twin Registry at
Virginia Commonwealth University) to bring questionnaire metadata into
the database and to maintain what is already there.

- **Home page**: How many extraction drafts you have in progress and
  how many changes have been made, and a card for each page.
- **New Extraction**: Staff upload a questionnaire PDF. Gemini reads it
  into blocks, questions, variables and choices. Staff review and
  correct the whole extraction on the Edit Extraction page (the AI reads
  the number printed beside each question, and the site numbers the
  questions in order), give the variables their MATR IDs from a CSV (in
  order, or one by one) or from a recorded questionnaire that asks the
  same questions, say which registry, study, questionnaire and wave it
  goes into, and push it. The push is one database function, so the
  whole questionnaire goes in or none of it does.
- **Drafts**: The extractions not pushed yet, yours and your colleagues’.
- **Metadata**: Staff use plain language to build a change by filling in
  the sentence: “I want to add / update / delete a registry, study,
  questionnaire, wave, question…”. This helps them find the row, change
  it, see everywhere it reaches, and apply it at once. A wave,
  questionnaire, study or registry can also be emptied, and a wave
  extracted again from its PDF.
- **History**: Every change made through this site, newest first: what it
  was, who made it and when, and each field it changed, old and new (or
  what a push or an emptying counted).
- **AI Monitor**: How often staff correct what the AI read, by prompt
  version and over time, and the corrections that come up again and
  again.

## Setting up the database

The database is a Supabase project built by the eight scripts in
[sql/](sql), run in order, each whole, in the project's SQL Editor:
`1_tables.sql` (starts over: it removes every table and row first, and
refuses to while a questionnaire has content unless its
`set matr.start_over = 'yes'` line is uncommented), `2_references.sql`,
`3_indexes.sql`, `4_functions.sql`, `5_permissions.sql`,
`6_descriptions.sql`, `7_seed.sql` (the starting rows: registries,
studies, questionnaires, waves, lists and staff) and `8_search.sql` (the
dashboard's search by meaning; run it again whenever `5_permissions.sql`
is run). Before `8_search.sql`, deploy the Edge Functions in
[supabase/functions](supabase/functions)
(`supabase functions deploy --project-ref <ref>`) and tell the database
where the project is, once:
`select vault.create_secret('https://<ref>.supabase.co', 'project_url');`.
[docs/database_design.md](docs/database_design.md) says what the schema
is and why. The site reads the project's address and keys from `.env`
(`SUPABASE_URL`, `SUPABASE_SECRET_KEY`, `SUPABASE_PUBLISHABLE_KEY`);
`Rscript tests/database/check_access.R` checks that each key can do what
it should and nothing more.

On the **test project** only, also run
[tests/database/test_functions.sql](tests/database/test_functions.sql):
the functions the tests use to make and remove their rows. It refuses to
run anywhere else. `Rscript tests/database/seed_test_data.R` then pushes
two sample questionnaires into the seed's waves through the site's own
path (`--dry-run` checks them without writing).

## Tests

`shiny::runTests()` from the project folder runs the tests
(tests/testthat.R says how they run, and how to run a few of them). The
tests of the database run against the test project named in `.env` as
`TEST_SUPABASE_URL`, `TEST_SUPABASE_SECRET_KEY` and
`TEST_SUPABASE_PUBLISHABLE_KEY`, never the real one; without those they
are skipped. Each test makes the rows it needs, named with the run's
marker (`ZZTEST...`), and removes them when it ends; the last test
checks that nothing was left. After a crashed run,
`Rscript tests/database/clean_test_rows.R` removes what was left, and
[tests/database/reset_test_database.sql](tests/database/reset_test_database.sql)
(SQL Editor, test project only) does the same from inside the database.
