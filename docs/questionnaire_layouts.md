# Questionnaire layouts: what the registry's forms print, and how the metadata should hold it

From a page-by-page read of the 16 questionnaires in `edge-case-pdfs/` (about 390 pages, November 2026). The full
notes, with the file and page of every example, are in [questionnaire_layouts_catalogue.md](questionnaire_layouts_catalogue.md).
Examples below are cited as `FILE pPAGE Q`.

The goal they are judged by: **normalize as much as possible and reuse repeated wording, while still being able to
rebuild each question with its whole context.** The database is a guide for finding questions across registries,
not a copy of the page (the dashboard links each PDF).

## 1. Glossary: the axes of a printed table

Every table, grid or chart in the forms is two or three of these axes crossed:

| Axis | What it is | Examples |
| --- | --- | --- |
| **Item** | a thing asked about; read with the lead-in it is a question | "Are you a worrier?", "Hay fever", "Smoked cigarettes", "used LSD (acid)?" |
| **Choice** | an answer that can be marked | Yes / No, Never … Often, "1. Less than 1 time" |
| **Subject** | who the answer is about | Yourself, Your mother, Twin A, Your spouse, A brother (count) |
| **Instance** | which one of several of the same kind | Pregnancy 1–12, First child … eighth, household member B–I, episode R.23b–l |
| **Measure** | a separately headed question asked of every row or instance | Date of birth, Sex, Birth weight, "Did a doctor ever tell you …?" |
| **Time / aspect** | a frame or domain the same question is asked in | in your lifetime / last 12 months / last 30 days; Drinking / Drug use; Mon … Sun |

## 2. The kinds of layout found

| # | Layout (rows × columns) | How often | Examples |
| --- | --- | --- | --- |
| L1 | Item × Choice | very common | GNA p1 Q1, KQ p2 Q15, Pink p13 Q74, MQ p3 Q20 |
| L2 | Item × Subject (lone boxes or counts) | common | LCQ p1 Q1, Pink p5 Q27, VAQ2 Health Q3, VAQ3 Health Q5, AHP p5 Q49 |
| L3 | Item × Subject × Choice | several | KQ p3 Q16 (Mother / Father scales), MQ p10 Q55, VAQ3 Repro p8 Q32 |
| L4 | Item × Time-or-aspect × Choice | several | KQ p5 Q28–34, MQ p11 Q63, KQ p6 Q37 (Drinking / Drug use) |
| L5 | Item × Measure | several | GNA p4 Q15 (Step 1 / Step 2), VAQ2 Health Q6 (check + age), VAQ3 Health p13 Q46, AAATS Parent p20 D.13a |
| L6 | Instance × Measure (a roster or chart) | common | GNA p2 Q3, AHP p7 Q56, LCQ p2 Q3–4, VAQ3 Repro Q3/Q5/Q8, Pink p2 Q13, AAATS Twin p2 A.4 |
| L7 | Choice × Instance (instances span the whole form) | one form, every page | VAQ2 Reproductive ("Pregnancy number 1–10" columns on every question) |
| L8 | Subject × Choice (one question, several people) | common | VAQ2 Health Q2b, Pink p3 Q17–25 (a code legend answered by writing the code under each person), silhouettes |
| L9 | Subject × Event × Choice (rows are people) | a few | Pink p10 Q63 (Died / Serious illness / Personal crisis) |
| L10 | Choice × Subject, check-all | rare | 25b grandparents' ancestry (screenshot) |
| L11 | Item × Time, write-ins | rare | Pink p8 Q44 (Beer / Wine / Liquor × Mon … Sun) |
| L12 | Form × Measure | rare | VAQ3 Health p10 Q33 (cigarettes / pipe / cigar / chew / snuff × how long / age / amount) |

The same tables also come **unrolled** into separately numbered questions: a battery printed as numbered questions
(AAATS Twin p36 D.1–D.18, p107 T.1–T.35, VAQ3 Health p9 Q17–28), a time-frame grid as numbered fragments
("H.3 …in your lifetime?" AAATS Twin p46), a roster as numbered questions ("A.18d Second pregnancy." AAATS Twin p7),
or a lead-in shared across numbered questions ("For questions H3-H16, ask: Have you . . ." TSA p13).

## 3. What this means for the block kinds

### Grid (new): a lead-in and the questions it is part of

**The rule:** a Grid's `text` is the lead-in that is part of each question's meaning, printed once and stored once.
It always has one: items that are whole questions on their own, with only a direction above them ("Please answer each
question by marking Yes or No.", GNA Q1), are a Group with that Instruction, even when they are printed as a table.
A Grid's children are **Questions along the axis that says what is asked**; every other axis becomes **variables** of
those questions:

- a Subject axis → one variable per subject (`variable.subject`);
- a Time / aspect, Instance, Measure or Form axis → one variable per column, labelled with it (`variable.label`);
- a Choice axis → the choices of each variable.

So L1–L5 have rows as questions; L6 (rosters) has the *columns* as questions and a variable per instance. The full
question is always rebuilt the same way: **the Grid's text + a space + the question's text** (+ the variable's
label), for example "In the past 4 weeks, how often have you felt calm and peaceful?" or "On how many occasions
(if any) have you drunk any amount of alcohol … in your lifetime?". The Grid's text keeps the punctuation it joins
with ("In the last 12 months," / "How old were you when" / "…each of the following?").

A Grid is not tied to lettering: its questions can each have their own printed number (TSA H3–H16, AAATS H.3–H.5,
KQ Q28–34 under "On how many occasions have you done each of the following?").

**L8–L10 are not Grids.** When the rows are answer choices and the columns are people (25b, silhouettes, income for
Yourself / Your spouse, Pink's code legends), it is **one Question** asking about several subjects: one variable per
subject. Pink p10 Q63 (people × events) is a Grid turned sideways: each event column is a Question, a variable per person.

**Why rows should be questions** (the convention change from the AHP/KQ review, where a battery was one question with
a variable per item): the same items are printed as the rows of a table in one form and as separate questions in
another.

| Item set | As rows of a table | As separate questions |
| --- | --- | --- |
| EPQ personality items ("Are you a worrier?") | GNA p1 Q1, AHP p3 Q39 | VAQ3 Health p9 Q17–28, Pink p12 Q73 (numbered list), MQ p11 Q65 (as fragments of "Are you the type of person…") |
| Drug-risk items ("Smoke one or more packs of cigarettes per day") | KQ p7 Q40 | AAATS Twin p59 J.1–J.6 |
| Times used (lifetime / 12 months / 30 days) | KQ p5 Q28–34, MQ p9 Q49 | AAATS Twin p46 H.3–H.5, I.3–I.5, L, M, Ma, N |
| Problem checklist (SCL type) | Pink p13 Q74, VAQ3 Health p15 Q58 | — (the same instrument in two forms) |
| Disease lists | VAQ2 Health Q3, VAQ3 Health Q5, VAQ3 Supplemental Q5, VAQ2/VAQ3 Repro (children) | — |

As questions, an item is the same kind of thing everywhere: a component shared by its words across questionnaires,
found by a search for questions, and linkable to a measurement scale. As variable labels it is neither.

### Group: stays, smaller, still with no words

A Group holds an **Instruction or Definition with the questions it applies to**, or questions that **route together**
(AHP p2 "If you answered No to items 22a-d, go to Question 28"; KQ p5). The difference from a Grid: a Grid's lead-in
is part of each question ("In the past 4 weeks, how often" + "have you felt calm and peaceful?"); what a Group holds
besides its questions only says how to answer them or what a word means, and each question is whole without it. A
table of whole questions under a direction (GNA Q1) is a Group. The forms have many of these scopes:

- a glossary for a range of questions: "For questions #56 through #63, please refer to the following list" (MQ p10);
- a time frame for the next N questions: "PLEASE NOTE THAT THE NEXT SIX QUESTIONS CONCERN THE PAST 12 MONTHS" (Pink p10);
- a direction for a range: "For questions 50 through 55, please check Yes or No as appropriate." (Pink p9).

A question with its follow-up no longer needs a Group: `parent_question` already links them (Pink p1 Q5/6, AHP p1
Q14, VAQ3 Repro p3 Q9's chain of six). That removes the most common reason Groups are made.

### No new types for these (each is a column or a rule, not a kind of block)

| Seen | Proposal |
| --- | --- |
| "Lead-in" | The Grid's text (above). A separate block would apply to questions only by sitting before them, which breaks when two lead-ins share a group. |
| Rosters (L6, L7) | A Grid sideways; the instance is the variable's label ("Pregnancy 3"). |
| Interviewer checkpoints and interviewer-recorded items: "Has R answered 'Yes' to question D30 or D31 or D32?" (TSA p8), "**CHECKPOINT: COUNTING SYMPTOMS …" (AAATS Twin p101), "IWER: Are all examples exclusively describing feelings of guilt?" (AAATS Parent p88), "Time interview began" (AAATS p1), "Parent interviewed: (CHECK ONE)" (AAATS Parent p1) | A Question with one new fact: **who records it** (respondent / interviewer), e.g. `questionnaire_question.recorded_by`. They have numbers, choices and routing like questions, may be columns in the data, but nobody is asked them, so the dashboard can leave them out of question search. |
| Pictures as a scale (silhouettes, Pink p6, Blue p7) | An Instruction describing the picture; choices "0" … "8". |
| Interviewer-only text (TSA's bold, AAATS's [CAPITALS], PROBE, FIELDCODE, "Read if necessary") | Instruction blocks; never part of a question's text. Possibly a flag on Instruction later; not needed for search. |

## 4. Rebuilding the full question

`full question = (each ancestor Grid's text) + question text (+ variable label)`, Instructions and Definitions left
out; the Section heading shown beside it as context, not joined into the sentence. Section headings sometimes carry
meaning ("Sleep Patterns (During the past 12 months)" › "ON WEEKDAYS:" Pink p7), which is why they are shown.

Things that break the join unless the item is written whole:

- **Elided items**: "H4. or DMT?" after "H3. used LSD (acid)?" (TSA p13), "H.2 Not even once?" (AAATS p46),
  "G.2 Did any of your brothers or sisters (do this)?" (AAATS p44), "O3. or alcohol use?" (TSA p28), "Sisters? Sons?"
  (VAQ3 Health p1 Q4).
- **Fill-ins**: "(name of twin 1)" (AAATS Parent p12), "(SUBTRACT 12 MONTHS FROM CURRENT DATE)" (AAATS Twin p16),
  "(SYMPTOM)", "(condition)", "(number in B.2a)", "(you)"; alternatives "(are/were)", "(do/did)", "(this/these)",
  "(smoke / use)", "s/he/they", "For boys: … For girls: …" (AAATS Twin p26).

**Decided (D-1):** elided items are written whole ("used DMT?"), so the join reads as a question and they can be
found; the PDF has the exact words.

**Decided (D-2):** a fill-in that names a person ("(name of twin 1)", "(you)") becomes `[subject]`, like the linked
stems, and the question shares the stem of its words; other fill-ins stay as printed.

## 5. Normalizing questions that repeat

| Repeats | Examples | Covered today? |
| --- | --- | --- |
| Same question, another person **named** | MQ Twin A / Twin B sections (p2–6), TSA A.15/A.16/A.18 (mother / father / siblings) | Yes: Normalize links them under a `[subject]` stem |
| Same question, another person by **pronoun** | "How old is she?" / "How old is he?" (Pink p1 Q5/6) | Yes: the pronoun is `[subject]` |
| Same question **reworded** for another person | "What is your date of birth?" / "What is the date of birth of the other biological parent?" (AAATS Parent B.1 / B.12) | Linked by hand in Normalize, with a stem staff type |
| Same question, another **substance, condition or reason** | TSA C / E / G co-twin tobacco / alcohol / cannabis; TSA J per drug class; VAQ2 Repro ~15 "during any pregnancy" questions; AAATS R symptom triplets; AAATS U race / gender / anything else | Yes: `[item]` (D-3) |
| Same follow-up wording many times in **one** form | "What was its effect on you?" ×30 (AAATS Twin C), "Is he/she NOW following the advice?" | Each becomes a new component with the same words (a questionnaire places a component once) |
| Same instrument across **forms** | EPQ, SCL, disease lists, Pink / Blue / Yellow (one survey for twin, parent and spouse with different people listed) | Shared only when the words match exactly; rows-as-questions makes far more of them match |

**Decided (D-3):** a stem has a third slot, `[item]`, for the one thing questions otherwise worded the same ask
about ("when you were growing up, how often were you encouraged to use [item] by your twin?" for tobacco, alcohol and
cannabis). Normalize finds them: the same words but for one run of up to four, sharing at least five words and two
thirds of each, not a grid's items. Where their variables name who each is about, the slot is `[subject]` instead.
Questions reworded for another person, or by pronoun ("how old is she?" / "how old is he?"), are linked too: by
pronoun automatically, reworded by hand with a stem staff type.

## 6. Answer choices: what the new normalization still misses

| Seen | Today | Proposal |
| --- | --- | --- |
| A choice printed as **"?"**: "Yes / ? / No", "If you are uncertain, please circle ?" (Pink p14 Q75) | `normal_answer()` strips "?", leaving a **blank choice** that can't be saved | **Bug.** Write it as "uncertain" (what the form says it means). |
| "Between 3 and 5 times", "Between 6 and 10 times" (AAATS Parent p85) | left as words | `3-5 [times]`, like "3 to 5 times" |
| Restated units: "21-35 days (3-5 weeks)" (VAQ3 Repro p7 Q22) | `21-35 days [3-5 weeks]` | Decide: keep, or drop a bracket that only restates the range in other units (`21-35 [days]`) |
| Printed codes: "1. Yes", "(1) Don't know", "99. Don't know", "0. Never", "a. Excellent" (AAATS, Pink, VAQ) | the prompt drops them | Keep dropping them from the words; they are the `code_value` where printed |
| Volunteered and special answers: "(If vol.) only one time", "89: 'All my life'", "Possibly*" (AAATS Parent p61–64, Twin R) | — | The words without the marker; the marker is interviewer instruction |
| Abbreviations: "R first", "Cotwin first" (TSA p6), "Spouse in HH" (AAATS Parent p4), "DK" | — | Written out: "respondent first", "spouse in household", "don't know" |
| Comparisons naming two people: "Twin A much more … Twin B much more" (MQ p8 Q42), "Me / My Twin / No difference" (VAQ3 p13), "My twin's health is much better" (VAQ3 p16) | left as printed (linking skips them) | Fine as is; the variable is about both twins (D-4 below) |
| Dollar ranges: "$5,000-9,999", "Less than $1,000 (including loss)" | `less than $1,000 [including loss]`; ranges kept | Fine |
| Qualifiers: "Yes, it got worse.", "Married - living apart now", "Overweight, but trying to lose weight." | `yes [it got worse]`, `married [living apart now]`, `overweight [but trying to lose weight]` | Done |

## 7. Variables

- **Split answers**: ft + in, lbs + oz, hrs + mins, days / weeks / months / years as alternatives (TSA B10, D8: up
  to five), a number + a period code ("__ Days" + "1. Per week 2. Per month", AAATS Parent D.8), a date as month /
  day / year (Yellow's handwritten TODMO / TODDA / TODYY). One question, one variable per part, labelled by unit:
  already supported.
- **Valid ranges printed by a box**: "lbs (1-12)", "ozs (00-15)", "weeks (27-44)", "(00-25)". Not kept today;
  could be a variable note later.
- **Cells left out**: cancer sites with no male or female columns (VAQ3 Health p4), rows with only a Twin box (VAQ3
  Supplemental). Simply no variable there.
- **Who a comparison is about (D-4, decided)**: a variable comparing the twins is about neither alone. Its subject is
  "Twins (compared)" rather than blank (which means the respondent).

## 8. Conditions and routing

The forms route on much more than one answer: value branches on a write-in ("If 0 … If 1 or more …" TSA p1; three
branches AAATS Parent p86), comparisons of two answers ("If same age as first use …" TSA p7), counts of answers
("1. 0-1 Yes responses in the C.12 series" AAATS Parent p66), whole sections ("COMPLETE THIS SECTION IF: ITEM H.4
… IS BOX 3 OR GREATER …" AAATS Twin p78), sections by sex or twin status ("For Females Only (Males continue with
Question 56.)" AHP p6; "FOR TWINS (IF YOU ARE NOT A TWIN GO TO QUESTION 65.)" VAQ3 Health p15).

**Decided (D-5, revised 2026-11-08):** the database is a guide, not an engine that runs the form, so it keeps one
thing about routing: which earlier question a question depends on (`parent_question` in the extraction,
`placement.depends_on_placement_id` in the database). Skip conditions, skip instructions and skip targets are not
kept, and neither is a `condition` column: "For Females Only" and "COMPLETE THIS SECTION IF …" are printed text,
kept as an Instruction block where they are printed.

## 9. Proposed fixture pages (one per kind, for tests and for checking prompt changes)

| Kind | Page |
| --- | --- |
| L1 Item × Choice, whole-question items under a direction (a Group, not a Grid) | GNA p1 Q1 |
| L1 with a lead-in the items finish | AHP p1 Q16, AHP p2 Q20 |
| L2 / L3 Item × Subject (× Choice) | LCQ p1 Q1, KQ p3 Q16 |
| L4 Item × Time × Choice, nested under a shared lead-in | KQ p5–6 Q28–34 |
| L5 Item × Measure with a (condition) fill-in | AAATS Parent p20 D.13a |
| L6 Roster | AHP p7 Q56, LCQ p2 Q3–Q4 |
| L7 Choice × Instance | VAQ2 Reproductive p1 Q3 |
| L8 Code legend × Subject | Pink p3 Q17 |
| L9 Subject rows | Pink p10 Q63 |
| Unrolled with elided items | TSA p13 H3–H16 |
| Checkpoint | TSA p8 after D32, AAATS Twin p101 R.21 |
| Choice "?" | Pink p14 Q75 |
| Section with a condition and routing | AHP p6, VAQ3 Health p15 |

## 10. Decisions, in the order they unblock work

1. **Grid** as defined in section 3: always with a lead-in, with rows (or roster columns) as Questions. Reverses
   the AHP/KQ battery convention (questions count goes up; MATR IDs unchanged).
2. **Group** only for instruction / definition scope and shared routing; follow-ups via `parent_question` alone. Done.
3. **`recorded_by`** (respondent / interviewer) on questions instead of a Checkpoint type.
4. D-1 elided items written whole; D-2 person fill-ins as `[subject]`. Done.
5. D-5 routing kept as one dependency per question; no `condition` column, no skip fields. Done (section 8).
6. Choice fixes: "?" (bug), "between X and Y", restated units, abbreviations.
7. D-3 general stem slot ([item], found by Normalize); D-4 subject for comparisons ("Twins (compared)"). Done.
