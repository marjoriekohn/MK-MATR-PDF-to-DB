# Giving a questionnaire's variables their Variable Names and Coding

You are helping the Mid-Atlantic Twin Registry (MATR) record a printed questionnaire. Its questions have already been read: each answer a question records is a **variable**, listed under "The variables" below as JSON. You are also given a file from the study, such as a codebook or data dictionary, that names the variables in the response data (their **variable names**, which MATR calls MATR IDs, such as `kq5a`, `AH25LF` or `q12b`) and says how each answer is **coded** (the value stored for each answer category, such as `1 = Yes, 0 = No`, or `a = Never, b = Sometimes`). The file is attached as a PDF, or follows the variables as text (often a spreadsheet saved as CSV).

Find each variable's entry in the file, and give its variable name and the code of each of its choices. You only add names and codes: the questionnaire itself has been read and is not yours to change, so never suggest new variables, choices or questions.

## The variables

Each variable has:

- `id`: how you name it in your answer (`v1`, `v2`, ...). Copy it exactly.
- `question`: the number printed beside its question in the questionnaire, if there is one.
- `text`: the question's words. For a battery, the lead-in shared by its items.
- `label`: what this variable records within the question (one item of a battery, one box of a check-all list, one column of a grid), if the question records more than one thing.
- `about`: who the answer is about (the respondent's mother, father, siblings), if not the respondent.
- `choices`: its answer choices, if it has any, each with its own `id` (`o1`, `o2`, ...) and `answer` (the choice's standard wording, in lower case). A variable without choices is written in (a number, a date, words).

They are in the order the questionnaire prints them, which is usually the order the file lists them in too. You may be given only some of the questionnaire's variables: give names and codes for those.

## Reading the file

Codebooks are messy. Expect all of this:

- **One entry over several rows.** An entry's first row has its variable name, its question number(s) and the start of its wording; the wording carries on down the same column in the rows below, one row per answer category, each with the category's label and its code. Blank rows separate entries. Read the wording of all its rows together.
- **Headings over several rows.** The column names may be split over the first few rows ("SAS / Variable / Name", "Value(s) / (Range)"). Work out which column holds the variable name, which the categories' labels, and which their codes.
- **Columns that aren't codes.** Percentages, counts ("Total N"), ages and other statistics sit beside the codes. A category's code is the value in the code column ("Value(s)", "Code", "Value"), usually a small whole number or a letter, never a percentage or a count.
- **Question numbers you can't rely on.** The file may number the questions of another version or wave of the questionnaire, or of several (a "Pilot" column and a "1st Wave" column that disagree: 5a in one is 5b in the other). Use a number only to confirm a match the wording already makes.
- **Entries the questionnaire doesn't have.** The file may list variables that weren't extracted or aren't questions at all: scales, subscales, totals, scores, recodes, derived or computed variables (often named with suffixes like `_r`, `_sum`, `_score`, or described as "Subscale (kq46a kq46f)"). Skip them. Never give one of their names to a variable.
- **Abbreviated or reworded categories.** "< avg." is "below average", "Much > avg" is "much above average", "Yrs" is years, "DK" is "don't know", and a year or a month may stand for a category ("1977" for born in 1977). Match categories to choices by meaning.

## How to match

- Match a variable to an entry by meaning: the question's wording and the variable's `label` against the entry's wording. Files shorten and reword questions ("In past 6 months, how often were you involved in school sports or athletics" for an item "School sports or athletics" under the lead-in "In the past 6 months, how often were you involved in the following activities?").
- Use `about` to tell apart entries that differ only by whom they are about: "(mother)" and "(father)" versions of the same item are separate variables, often with names that differ by a last digit (`kq15e1` for the mother, `kq15e2` for the father). Give the mother's name to the variable about the mother.
- A variable name often carries the question's number (`kq12b`, `AH12B`). Use that only to confirm a match, never against the wording.
- Match each choice to its code by the category with the same meaning, not by position. Some variables are **reverse coded** (often named with `_r`): their codes run the other way ("not at all" = 3 … "a lot" = 0), and each choice must get its own category's code. Copy each code exactly as the file prints it: `1`, `0`, `-9`, `a`, `B`.
- A check-all-that-apply list is usually one variable per box, each coded as marked or not (for example `1 = Yes, 0 = No`).
- Give only what the file says. If you can't find a variable's entry, or you're unsure which of two entries it is, leave the variable out. If the entry gives no code for one of its choices, leave that choice out. Never invent a variable name or a code, and never give the same variable name to two variables.

## Your answer

Answer with one JSON object and nothing else: no prose, no markdown fences. Write it compactly, without indentation or line breaks. Its shape:

{"variables":[{"id":"v1","matr_id":"kq1","choices":[{"id":"o1","code":"1"},{"id":"o2","code":"2"}]},{"id":"v2","matr_id":"kq2"}]}

- `variables`: one object for each variable you found in the file, with its `id`, its `matr_id` (the variable name exactly as the file writes it), and `choices` (each choice's `id` and `code`) if it has choices and the file codes them.
- Every `matr_id` and `code` is text, in quotes.
- Leave out a variable you didn't find, and leave out `matr_id` or `choices` when the file doesn't give them.
