# The MATR metadata database: design

_The schema the scripts in [sql/](../sql) build, why it looks the way it does, and the contract the site's code
follows. Rewritten 2026-11-08 to simplify the earlier design (33 tables, 8 of them draft bookkeeping)._

## What the database is for

A normalized catalog of every question the registry has asked in 50 years, so researchers can find what we
have. Staff extract each questionnaire PDF on this site (an AI reads it, staff review it, then push it); a
separate public site, "The Dashboard", reads the catalog and lets researchers search questions, see the
PDFs, and collect the MATR IDs (variables) they want. The catalog has to stay clean: one wording per
component, one row per answer choice, no duplicate questions.

## The model (22 tables)

**Catalog, read by the dashboard**

| Table | One row is | Notes |
| --- | --- | --- |
| `registry` | a registry (MATR, S4S) | |
| `principal_investigator` | a researcher | |
| `study` | a project under a registry | lead and contact investigator |
| `respondent_type` | who a questionnaire is for | age range |
| `questionnaire` | a form | name and abbreviation unique within its study |
| `wave` | one administration of a form | also keeps the PDF and supplement it was extracted from (`pdf_sha256`, `supplement_*`); kept when emptied so Re-extract can read them again |
| `pdf_file` | a PDF in Storage, by SHA-256 | pages, bytes |
| `component` | one printed wording, kept once | `kind` (Section, Group, Grid, Question, Instruction, Introduction, Definition, Note, Interviewer Script), `text`, and for a Question `normal_text` (generated) |
| `placement` | one place a questionnaire prints a component | `parent_placement_id` (the Section/Grid/Group it is in), `display_order`, `depends_on_placement_id` (the earlier question it only makes sense after) |
| `wave_question` | that a wave asked a question | `question_number` (its place, 1, 2, 3...), `printed_number` (as printed) |
| `variable` | one MATR ID | `variable_type` (`open_ended`/`closed_ended`), `answer_data_type` (NUMBER/TEXT/DATE/TIME, open-ended only), `label` |
| `answer_option` | one answer choice, kept once | `standardized_label` in its normal form |
| `response_option` | one choice as one variable printed it | `display_order`, `code_value` |
| `tag` | a subject, time frame or unit | `kind` + `name`, unique per kind |
| `placement_tag`, `variable_tag`, `response_option_tag` | a tag on a question, a variable, a choice | |
| `measurement_scale`, `question_scale` | validated scales and their items | |

**Private to the site**

| Table | One row is |
| --- | --- |
| `staff` | a staff member (never deleted; `active`) |
| `extraction` | one extraction, from first save to pushed and after: method, status (`Draft`/`Pushed`), `info` (where it goes), `questionnaire` (the working JSON), `ai_original`, `review_edits`, PDF and supplement, the reading job's columns, `revision` |
| `history` | one change to the catalog: `action` (Pushed, Emptied, Added, Updated, Deleted), `entity`, `record_id`, `summary`, `details` |

Every ID is a random 9-digit number picked by the database (`private.random_id`).

## Decisions, and the questions they answer

**Why were there so many draft tables?** The old design split one draft over `draft`, `edit_draft`,
`extracted_draft` and `ai_extracted_draft` (subtype tables with kind columns), and recorded its review in
`draft_review_event` and what it did in `applied_item`. All of that served a two-person approval workflow
that is gone. Now an extraction is one row of `extraction`, and an edit on Make an edit is applied at once
by `apply_change()` and recorded as one row of `history`.

**What was `applied_item` for?** The field-by-field record of an applied edit, for Edit History. It is now
`history.details` (a JSON list of the same fields) on the one history row, and for an extraction the counts.

**Approval.** Nothing needs a second person. Anyone active in `staff` can push an extraction, apply an
edit, empty or delete. `staff.role` stays for the site's own use.

**Skips.** The old schema carried skip conditions, skip answers, skip instructions and skip targets on
groups, open-ended variables and response options (nine columns, three tables). Researchers only need to
know that a question hangs off another one. That is now one column: `placement.depends_on_placement_id`.
A question with it empty stands on its own. Routing text printed on the page ("go to question 10") is not
kept.

**Self-contained, conditions.** `self_contained` said the same thing as having a parent, and `condition`
("For females only") is printed text that belongs in an Instruction block. Both are gone.

**Tags.** `subject` and `time_frame` were tables, and `unit` a text column of `open_ended`. They are one
list, `tag`, with a `kind`, and three link tables so a tag can sit on a question (`placement_tag`), a
variable (`variable_tag`) or a single response option (`response_option_tag`). The push writes a variable's
`subject`, `time_frame` and `unit`; the other two levels are there for the editor.

**Open-ended variables and `answer_data_type`.** A question with two blanks ("___ feet ___ inches") is two
MATR IDs, so two variables, each with its own data type and unit tag; that is one row per MATR ID, not a
duplicate. The data type is a check constraint on `variable`, not a Postgres enum type: a check is easier
to change and shows up in the table editor; an enum type buys nothing here.

**No `option_set`.** A set of choices was derivable from `response_option` and only served the scale's
"expected option set" (never used). Gone, with `answer_option_set`, `answer_option_alias`,
`response_option_count` (never filled), `category`/`question_category` (never filled; add back when MeSH
tagging is built), `component_type` (nine fixed kinds are a check constraint), `question`/`question_stem`
(see normalization) and `wave_file` (folded into `wave`).

**Normalization of question text.** The printed words stay verbatim (lower case) in `component.text`,
because researchers want to see how a question was printed. `component.normal_text` is a generated column
from `private.normal_question()`: it strips the words that only ask for the answer ("what is your", "enter
your", "write your", a trailing "now", directions in brackets) so "what is your height?", "enter your
height" and "height" share `height`. The dashboard groups by it. It is computed by the database, never
typed, so it can't drift. Answer choices are normalized the same way (`private.normal_answer`, mirrored by
the site's `normal_answer()`), and time frames and units by the site (`normal_time_frame()`,
`normal_unit()`) before they become tags.

**Emptying.** `empty_wave`, `empty_questionnaire`, `empty_study`, `empty_registry` remove what the thing
holds, keep the thing itself, and remove what that leaves owned by nothing: questions no wave asks any
more, sections left empty, the components they printed that nothing prints any more (unless a scale names
them). Answer options and tags stay: they are vocabulary. Each writes a history row. Re-extract on a wave = empty it, then start an
AI-Powered extraction from the wave's PDF.

**Multiple waves of one questionnaire.** A push into a second wave reuses the placements the questionnaire
already has (same component under the same parent) and the variables with the same MATR ID on that
question, and only adds `wave_question` rows for the new wave.

## The functions the site calls (through `/rpc/`, as `service_role`)

| Function | Does |
| --- | --- |
| `push_extraction(p_extraction_id, p_staff_id, p_dry_run default false)` | writes an extraction's JSON into the catalog; the wave must ask nothing yet; marks the extraction Pushed; writes history. Returns counts. With `p_dry_run`, does it and undoes it, returning the counts (the preview). |
| `apply_change(p_staff_id, p_items, p_summary)` | applies a Make-an-edit item list (Insert/Update/Delete with `new:<table>:<n>` tokens, old-value checks) in one transaction; writes one history row. Returns `{history_id, new_ids}`. |
| `empty_wave(p_wave_id, p_staff_id)`, `empty_questionnaire(...)`, `empty_study(...)`, `empty_registry(...)` | see Emptying. Return counts. |
| `upvote_questionnaire(p_questionnaire_id)` | the dashboard's upvote (anon). |

Everything else is plain REST on the tables: the site inserts, patches and deletes `extraction` rows
directly (a PATCH filtered on `revision=eq.<n>` is the optimistic lock; the `extraction_touch` trigger bumps
`revision` and `saved_at` on each staff save), and reads the catalog tables as before.

## The extraction JSON (`extraction.questionnaire`, also what the prompt asks for)

```
{"questionnaire": {"name", "abbreviation"}, "components": [block, ...]}
block:    {"component_type", "text", "printed_number", "question_number", "parent_question",
           "variables": [variable, ...], "children": [block, ...], "checked", "ref", "page"}
variable: {"variable_type", "matr_id", "label", "subject", "time_frame", "unit", "answer_data_type",
           "response_options": [choice, ...], "ref"}
choice:   {"answer_option", "code_value", "ref"}   (may also carry "unit"/"time_frame"/"subject")
```

`parent_question` is the `question_number` of the question this one depends on. Gone from the JSON:
`self_contained`, `condition`, `skip_condition`, `skip_when`, `skip_instruction`, `skip_to`, `skip_to_end`,
`parent_from_skip`, `stem`, `respondent_count`. The push ignores keys it doesn't know, so an old draft still
pushes.

## Rules the database keeps

- A placement's parent is a Section, Grid or Group; a Grid holds no containers; a dependency links Question
  to Question; no cycles (`placement_rules`, `placement_no_cycle`).
- Only a Question is asked in a wave or has variables. A wave and its questions share one questionnaire
  (composite foreign keys).
- A closed-ended variable has at least one response option, an open-ended one none (checked at commit).
- One component per kind and words; one Group. One tag per kind and name. One answer option per normal label.
- Staff are never deleted. The history is never changed. A pushed extraction's content is never changed.
