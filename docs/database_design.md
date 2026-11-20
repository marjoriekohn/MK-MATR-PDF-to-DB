# The MATR metadata database: design

_The schema the scripts in [sql/](../sql) build, why it looks the way it does, and the contract the site's code
follows. Rewritten 2026-11-08 to simplify the earlier design (33 tables, 8 of them draft bookkeeping)._

## What the database is for

A normalized catalog of every question the registry has asked in 50 years, so researchers can find what we
have. Staff extract each questionnaire PDF on this site (an AI reads it, staff review it, then push it); a
separate public site, "The Dashboard", reads the catalog and lets researchers search questions, see the
PDFs, and collect the MATR IDs (variables) they want. The catalog has to stay clean: one wording per
component, one row per answer choice, no duplicate questions.

## The model (24 tables)

**Catalog, read by the dashboard**

| Table | One row is | Notes |
| --- | --- | --- |
| `registry` | a registry (MATR, S4S) | |
| `principal_investigator` | a researcher | |
| `study` | a project under a registry | lead and contact investigator; `hidden` keeps it off the dashboard (see Hidden from the dashboard) |
| `respondent_type` | who a questionnaire is for | age range |
| `questionnaire` | a form | name and abbreviation unique within its study; `hidden` |
| `wave` | one administration of a form | also keeps the PDF and supplement it was extracted from (`pdf_sha256`, `supplement_*`); kept when emptied so Re-extract can read them again. The files' names (`pdf_name`, `supplement_name`) are the wave's, and staff can correct them on the Metadata page; `hidden` |
| `pdf_file` | a PDF in Storage, by SHA-256 | pages, bytes |
| `component` | one printed wording, kept once | `kind` (Section, Group, Grid, Question, Instruction, Introduction, Definition, Note, Interviewer Script), `text` |
| `placement` | one place a questionnaire prints a component | `parent_placement_id` (the Section/Grid/Group it is in), `display_order`, `depends_on_placement_id` (the earlier question it only makes sense after) |
| `wave_question` | that a wave asked a question | `question_number` (its place, 1, 2, 3...), `printed_number` (as printed) |
| `variable` | one MATR ID | `variable_type` (`open_ended`/`closed_ended`), `answer_data_type` (NUMBER/TEXT/DATE/TIME, open-ended only), `label` |
| `answer_option` | one answer choice, kept once | `standardized_label` in its normal form |
| `response_option` | one choice as one variable printed it | `display_order`, `code_value` |
| `tag` | a subject, time frame or unit | `kind` + `name`, unique per kind |
| `placement_tag`, `variable_tag`, `response_option_tag` | a tag on a question, a variable, a choice | |
| `publication`, `study_publication` | papers that used a study's data, and which studies each is about | `title`, `authors`, `journal`, `published_date`, `doi` (the DOI alone, lower case; the link is https://doi.org/ + it), `url` (a plain link, for one with no DOI) |
| `measurement_scale`, `question_scale` | validated scales and their items | a scale's `name` and `abbreviation`; an item is a question's wording (`component_id`) in a scale, in no order |

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
variable (`variable_tag`) or a single response option (`response_option_tag`). A tag is said once: who and
when go on the **question** when they are about all of it ("have you smoked?" [lifetime]) and on each
**variable** when the question's variables differ (the same question's answers for [twin a] and [twin b], or
for [lifetime] and [past year]); a kind of tag is never on a question and on its variables at once, and
a unit is always a variable's. So whoever reads a variable's tags reads its question's too (`variable_tag`,
then `placement_tag` for the kinds the variable hasn't got). The site keeps this when a draft is opened and
saved (`draft_service_place_tags()`), and drops a variable's label that only repeats one of its tags. The
push writes a question's `subject` and `time_frame` and a variable's `subject`, `time_frame` and `unit`.

**Publications.** A study lists the publications that used its data, and one publication can be about several
studies, so a publication is a row of its own and `study_publication` says which studies it is about; a link
goes with its study or its publication. Staff add a publication under Metadata, picking its studies with it;
the dashboard reads both tables. A DOI is kept once, as the DOI alone in lower case, whatever was pasted; a
publication with no DOI (an older paper, a report) can have a plain link instead (`url`). The link to show is
the DOI's when there is one, else the plain link.

**Measurement scales.** A questionnaire can say which published scale a question comes from, usually as the
scale's abbreviation in brackets right after the question's words ("Are you a worrier? (EPQ-R)"). It is read
from wherever it is said: the questionnaire's PDF (the reading takes it out of the words) or the file of
Variable Names and Coding, where it follows the question's wording (the step that gives the variables their
MATR IDs gives their questions their scale too; one the PDF printed stands). Either way it is the question's
`scale` (a question in more than one scale has them all, with commas between: "FTND, CDS-12"); staff see and
change it on Edit Extraction, beside the question's tags. The push finds the scale by its abbreviation or name, ignoring case
(`private.scale_id()`; one the database hasn't got is added, named by its abbreviation until staff give it its
full name) and puts the question's wording in each (`question_scale`). The wording is the scale's item wherever
it is printed, so a push only ever adds an item; staff take one out on Make an edit, and an item goes with its
question once no questionnaire prints it any more (a wave emptied, a block deleted). An item has no number:
the order of a scale's questions isn't kept. A wave reopened for editing shows each question's scale again.

**Open-ended variables and `answer_data_type`.** A question with two blanks ("___ feet ___ inches") is two
MATR IDs, so two variables, each with its own data type and unit tag; that is one row per MATR ID, not a
duplicate. The data type is a check constraint on `variable`, not a Postgres enum type: a check is easier
to change and shows up in the table editor; an enum type buys nothing here.

**No `option_set`.** A set of choices was derivable from `response_option` and only served the scale's
"expected option set" (never used). Gone, with `answer_option_set`, `answer_option_alias`,
`response_option_count` (never filled), `category`/`question_category` (never filled; add back when MeSH
tagging is built), `component_type` (nine fixed kinds are a check constraint), `question`/`question_stem`
(see normalization) and `wave_file` (folded into `wave`).

**Normalization of question text.** `component.text` says what was asked in the registry's plain words, not
how the page printed it (the dashboard links each questionnaire's PDF for that): one complete sentence
("Height?" is "what is your height?"), without the time or the one-of-several people it asks about (those
are tags), without padding ("in general,", "if any,") and without directions on how to mark an answer. A
grid's lead-in and each of its items read as one sentence. So the same question in two questionnaires is
the same component, told apart by its tags. The extraction prompt (`prompts/extraction.md`) writes it that
way; text that only says how to answer, or only announces what comes next, isn't extracted at all. A form
that asks the same questions about one person and then another (a section about twin A, then twin B) is one
question each, with a variable per person, joined when the AI's answer is opened
(`draft_service_join_repeats()`); such a question has every printed number ("8, 24"). Each wording is one
component, so the dashboard groups a question by its component, and finds questions that ask the same thing
in other words by meaning: an
embedding of each question in `question_embedding`, kept up to date by the database (`sql/8_search.sql`).
Answer choices are normalized the same way (`private.normal_answer`, mirrored by
the site's `normal_answer()`), and tags by the site before they become rows of `tag`: a subject is the
relation to the respondent alone, in lower case (`normal_subject()`: "Respondent's Twin" is "twin"; who the
respondent is comes from the wave's respondent type, so the dashboard can say "Twin's mother"), a time frame is
"lifetime" or "past N units" with N in words under ten (`normal_time_frame()`: "past 30 days", "past six
months"), and a unit is written out (`normal_unit()`: "pounds"). The database matches a tag's name ignoring
case, so a spelling that slips past the site still finds the one tag.

**Emptying.** `empty_wave`, `empty_questionnaire`, `empty_study`, `empty_registry` remove what the thing
holds, keep the thing itself, and remove what that leaves owned by nothing: questions no wave asks any
more, sections left empty, the components they printed that nothing prints any more, with the scale
items that named them. Answer options and tags stay: they are vocabulary. Each writes a history row. Re-extract on a wave = empty it, then start an
AI-Powered extraction from the wave's PDF.

**Editing a pushed wave again.** `reopen_wave` makes a new Draft extraction holding what the wave asks as the
catalog has it now (so edits made on Make an edit since the push are in it), going into the same wave; its
`info.updates_wave_id` names the wave. Nothing changes until it is pushed: then `push_extraction` replaces
what the wave asks in one transaction (the wave's questions go, the placements no wave asks any more go, the
draft is pushed, and components nothing prints any more are forgotten afterwards, so a wording the update
still prints keeps its component). Each block of such a draft names the component it came from
(`component_id`). A block reworded when nothing else prints its component renames the component. When other
places print it too (another questionnaire, or another block of this one), staff choose in the Push dialog:
**only here** (the default) gives this place a new component and leaves the old one to the others;
**everywhere** (`"reword": "everywhere"`) renames the component itself, or, if the new words are a component
already, points every place at that one. The extraction the wave was first pushed from is kept as it was.
When the extraction the wave was last pushed from was read by the AI, the new draft is AI-Powered too and
carries that reading (`ai_original`, with its model and prompt version; `reading_status` is `ready`, and it is
not read again from the draft: Re-extract is the wave's own). Its `info.reopened_from` names that extraction.
The catalog doesn't keep which of the AI's rows each row was, so the site matches the draft's rows to the
reading through the copy that was pushed (same kind and words for a block, same place for a variable or a
choice) when the draft is opened, and the draft keeps the matches once it is saved (the site saves it before
it is pushed, changed or not, so its `review_edits` are worked out). So the Edit Extraction page still shows
what differs from the AI's reading, and a block can be put back to it. The AI Monitor counts a reading once,
under the extraction it was first pushed from (its date, model and prompt), with the `review_edits` of its
latest push: a correction put back as the AI read it no longer counts. Corrections to the questionnaire's name
and abbreviation stay the first push's, since a reopened draft takes those from the catalog. `review_edits` list
every row on its own; the AI Monitor counts the least staff changed (`monitor_counted_edits()` in
R/service_monitor.R): what is in a question or variable removed or added goes with it, and a row removed and one
added in the same place (a row its match was lost for, or one staff deleted and made again) count only as the
fields that differ.

**Hidden from the dashboard.** `study.hidden`, `questionnaire.hidden` and `wave.hidden` (boolean, false
unless staff say otherwise) say that the row is kept off the dashboard researchers search. This site only
records it: staff set it on the Metadata page (the row's own form), through `apply_change` like any edit. The
row stays in the database and on this site either way. The database's policies keep a hidden row, and
everything that is there only because of it (a hidden study's questionnaires, a question only hidden waves
asked), from the dashboard's keys (`sql/5_permissions.sql`); the site's secret key skips them.
Variables of a placement another wave still asks are not rewritten by an update (they are that wave's too).

**Merging.** Two rows that are the same thing kept twice (a question worded slightly differently in two
questionnaires, two headings, two answer choices, two tags, two scales, two investigators or respondent types)
are made one on the Metadata page: Merge on the one that should go, then the one to keep. It needs nothing new in
the database: the site works out what points at the row from the same list of links a delete uses
(`R/service_edit_merge.R`, `edit_relations`) and sends `apply_change()` the items that point each at the other
row instead, then delete the first. A place that prints a merged question or heading keeps its own position,
numbers, variables and contents; only the wording it names changes. A row the merged one is part of the key of (a
tag on a variable, a question in a scale, a choice a variable offers) is taken out and put back under the other,
or only taken out if the other is there already. Two answer choices that one variable offers both of can't be
merged until one is taken off that variable, since each has a code of its own there. The history records a merge
as one change (its action is Deleted, as the merged row goes; the summary says "Merged ... into ...").

**Multiple waves of one questionnaire.** A push into a second wave reuses the placements the questionnaire
already has (same component under the same parent) and the variables with the same MATR ID on that
question, and only adds `wave_question` rows for the new wave.

## The functions the site calls (through `/rpc/`, as `service_role`)

| Function | Does |
| --- | --- |
| `push_extraction(p_extraction_id, p_staff_id, p_dry_run default false)` | writes an extraction's JSON into the catalog; the wave must ask nothing yet, unless the extraction was reopened from it (then it replaces what the wave asks); marks the extraction Pushed; writes history. Returns counts, each row counted once however often the extraction uses it (`tag_reused` is how many different tags were already there), with the `names` of the tags, answer choices and option sets it adds and finds and of the questions the database had already. An option set is the choices one variable offers, whatever their order or codes; there is no table of them, so the push works them out from `response_option`. With `p_dry_run`, does it and undoes it, returning the same (the preview). |
| `apply_change(p_staff_id, p_items, p_summary)` | applies a Make-an-edit item list (Insert/Update/Delete with `new:<table>:<n>` tokens, old-value checks) in one transaction; writes one history row. Returns `{history_id, new_ids}`. |
| `empty_wave(p_wave_id, p_staff_id)`, `empty_questionnaire(...)`, `empty_study(...)`, `empty_registry(...)` | see Emptying. Return counts. |
| `reopen_wave(p_wave_id, p_staff_id)` | see Editing a pushed wave again. Returns the draft's `extraction_id` (the same one while that draft is open). |
| `upvote_questionnaire(p_questionnaire_id)` | the dashboard's upvote (anon). |
| `unstar_questionnaire(p_questionnaire_id)` | one upvote fewer, when a dashboard visitor takes back their star (anon); never below zero. |

Everything else is plain REST on the tables: the site inserts, patches and deletes `extraction` rows
directly (a PATCH filtered on `revision=eq.<n>` is the optimistic lock; the `extraction_touch` trigger bumps
`revision` and `saved_at` on each staff save), and reads the catalog tables as before.

## The extraction JSON (`extraction.questionnaire`, also what the prompt asks for)

```
{"questionnaire": {"name", "abbreviation"}, "components": [block, ...]}
block:    {"component_type", "text", "printed_number", "question_number", "parent_question",
           "subject", "time_frame",                      (a question's own tags)
           "scale",                                      (a question's measurement scales, by abbreviation: "FTND, CDS-12")
           "variables": [variable, ...], "children": [block, ...], "checked", "ref", "page",
           "component_id", "reword"}                     (a reopened wave's blocks only)
variable: {"variable_type", "matr_id", "label", "subject", "time_frame", "unit", "answer_data_type",
           "response_options": [choice, ...], "ref"}
choice:   {"answer_option", "code_value", "ref"}   (may also carry "unit"/"time_frame"/"subject")
```

`parent_question` is the `question_number` of the question this one depends on. `printed_number` is free text
and may list several ("8, 24": one question joined from two). Gone from the JSON:
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
