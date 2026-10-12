
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MATR-PDF-to-DB Admin Website

<!-- badges: start -->

[![R-CMD-check](https://github.com/vcu-cara/MATR-PDF-to-DB/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vcu-cara/MATR-PDF-to-DB/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

A R Shiny website for staff of MATR (the Mid-Atlantic Twin Registry at
Virginia Commonwealth University) to bring questionnaires into the MATR
database and to maintain what is already there.

- **Home page**: The newest change requests and the newest applied
  changes, and a button to each working page.
- **Extraction**: Users can upload a questionnaire PDF. Gemini reads it
  into blocks, questions, variables and choices. Staff check and correct
  the result, fill in MATR IDs from a CSV, and save it as a draft.
- **Edit**: Staff use plain language to build a SQL query by filling in
  the sentence. “I want to add / update / delete a registry, study,
  questionnaire, wave, question…”. This helps them find the row, change
  it, see everywhere it reaches, provides a location for them to explain
  why the change is necessary, and then send it for review.
- **Change Request**: A questionnaire is added to the database after one
  other person has reviewed it. Approving and applying are database
  functions, so an edit is applied all at once or not at all.
- **Edit history**: Every edit that was applied, newest first: what it
  was, why, who prepared and approved it, and each field it changed, old
  and new.
