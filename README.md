
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MATR-PDF-to-DB Admin Website

<!-- badges: start -->

[![tests](https://github.com/vcu-cara/MATR-PDF-to-DB/actions/workflows/tests.yaml/badge.svg)](https://github.com/vcu-cara/MATR-PDF-to-DB/actions/workflows/tests.yaml)
[![coverage](https://github.com/vcu-cara/MATR-PDF-to-DB/blob/badges/coverage.svg?raw=true)](https://github.com/vcu-cara/MATR-PDF-to-DB/actions/workflows/tests.yaml)

<!-- badges: end -->

A R Shiny website for staff of MATR (the Mid-Atlantic Twin Registry at
Virginia Commonwealth University) to bring questionnaire metadata into
the database and to maintain what is already there.

- **Home page**: The newest change requests and the newest applied
  changes, and a button to each working page.
- **Extraction**: Users can upload a questionnaire PDF. Gemini reads it
  into blocks, questions, variables and choices. Staff review and
  correct the whole extraction on the Edit Extraction page (the AI reads
  the number printed beside each question, and the site numbers the
  questions in order), give the variables their MATR IDs from a CSV (in
  order, or one by one) or from a recorded questionnaire that asks the
  same questions, and preview and submit it as a change request, saying
  which registry, study, questionnaire and wave it goes into.
- **Edit**: Staff use plain language to build a SQL query by filling in
  the sentence. “I want to add / update / delete a registry, study,
  questionnaire, wave, question…”. This helps them find the row, change
  it, see everywhere it reaches, and then send it for review.
- **Change Request**: A questionnaire is added to the database after one
  other person has reviewed it. Approving and applying are database
  functions, so an edit is applied all at once or not at all.
- **Edit history**: Every edit that was applied, newest first: what it
  was, who prepared and approved it, and each field it changed, old and
  new.
