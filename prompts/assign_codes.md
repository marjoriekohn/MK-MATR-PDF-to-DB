# Giving a questionnaire's variables their Variable Names and Coding

You are helping the Mid-Atlantic Twin Registry (MATR) record a printed questionnaire. Its questions have already been read: each answer a question records is a **variable**, listed under "The variables" below as JSON. You are also given a file from the study, such as a codebook or data dictionary, that names the variables in the response data (their **variable names**, also called Variable IDs, such as `kq5a`, `AH25LF` or `q12b`) and says how each answer is **coded** (the value stored for each answer category, such as `1 = Yes, 0 = No`, or `a = Never, b = Sometimes`). The file is attached as a PDF, or follows the variables as text (often a spreadsheet saved as CSV). It may also be the questionnaire PDF itself, when the study gave no separate file: see "When the file is the questionnaire" below.

Find each variable's entry in the file, and give its variable name, the code of each of its choices, the codes of any categories its entry codes that the questionnaire doesn't print, and, when the entry names one, the measurement scale its question comes from. The questionnaire itself has been read and is not yours to change: never suggest new variables or questions, and add a choice only as "Categories the questionnaire doesn't print" below says.

## The variables

Each variable has:

- `id`: how you name it in your answer (`v1`, `v2`, ...). Copy it exactly.
- `question`: the number printed beside its question in the questionnaire, if there is one.
- `text`: the question's words. For a battery, the lead-in shared by its items.
- `label`: what this variable records within the question (one item of a battery, one box of a check-all list, one column of a grid), if the question records more than one thing.
- `about`: who the answer is about (the respondent's mother, father, siblings), if not the respondent.
- `when`: the time the answer is about ("lifetime", "past year"), if the question names one. The question's `text` leaves the time out.
- `choices`: its answer choices, if it has any, each with its own `id` (`o1`, `o2`, ...) and `answer` (the choice's standard wording, in lower case). A variable without choices is written in (a number, a date, words).

They are in the order the questionnaire prints them, which is usually the order the file lists them in too. You may be given only some of the questionnaire's variables: give names and codes for those.

## Reading the file

Codebooks are messy. Expect all of this:

- **One entry over several rows.** An entry's first row has its variable name, its question number(s) and the start of its wording; the wording carries on down the same column in the rows below, one row per answer category, each with the category's label and its code. Blank rows separate entries. Read the wording of all its rows together.
- **Headings over several rows.** The column names may be split over the first few rows ("SAS / Variable / Name", "Value(s) / (Range)"). Work out which column holds the variable name, which the categories' labels, and which their codes.
- **Columns that aren't codes.** Percentages, counts ("Total N"), ages and other statistics sit beside the codes. A category's code is the value in the code column ("Value(s)", "Code", "Value"), usually a small whole number or a letter, never a percentage or a count.
- **Question numbers you can't rely on.** The file may number the questions of another version or wave of the questionnaire, or of several (a "Pilot" column and a "1st Wave" column that disagree: 5a in one is 5b in the other). Use a number only to confirm a match the wording already makes.
- **Entries the questionnaire doesn't have.** The file may list variables that weren't extracted or aren't questions at all: scales, subscales, totals, scores, recodes, derived or computed variables (often named with suffixes like `_r`, `_sum`, `_score`, or described as "Subscale (kq46a kq46f)"). Skip them. Never give one of their names to a variable.
- **A measurement scale named with the question.** Some files say which published scale a question comes from, usually as the scale's abbreviation in brackets right after the question's words: "Would you call yourself a nervous person? (EPQ-R)", "Do you often feel ""fed-up""? (EPQ-R)". That is the variable's `scale` (see "Scales" below); it is not part of the question's wording, so leave it aside when you match the wording.
- **Abbreviated or reworded categories.** "< avg." is "below average", "Much > avg" is "much above average", "Yrs" is years, "DK" is "don't know", and a year or a month may stand for a category ("1977" for born in 1977). Match categories to choices by meaning.
- **Codes listed once for the whole file.** A table of codes for missing data printed once for every variable ("Codes for Missing Data": NI = No information, NASK = Not asked, …) belongs to no entry. Never give those codes to a variable.

## When the file is the questionnaire

When "The file" below says the PDF is the questionnaire itself, look for variable names and codes printed beside its questions. A REDCap data dictionary ("Data Dictionary Codebook") prints each field's variable name in square brackets (`[domicile]`: its name is `domicile`, without the brackets) and each category's code beside its label (`1  Apartment`, `999  Prefer not to answer`). Some printed questionnaires print a small code beside each box (`1 ☐ Yes  2 ☐ No`): those are the choices' codes. Only the pages the variables are printed on may be attached; the variables are on them.

- A question's printed number ("12", "41", "Q5") is not a variable name, nor is the number of a row in a data dictionary. Give a `matr_id` only when the PDF prints a variable's name.
- A questionnaire that prints no variable names and no codes has nothing to give: answer `{"variables":[]}`.

## How to match

- Match a variable to an entry by meaning: the question's wording and the variable's `label` against the entry's wording. Files shorten and reword questions ("In past 6 months, how often were you involved in school sports or athletics" for an item "School sports or athletics" under the lead-in "In the past 6 months, how often were you involved in the following activities?").
- Use `when` to tell apart entries that differ only by the time they are about: "in your lifetime", "in the last 12 months" and "in the last 30 days" versions of the same question are separate variables, often with names that differ by a last letter (`mq50a`, `mq50b`, `mq50c`).
- Use `about` to tell apart entries that differ only by whom they are about: "(mother)" and "(father)" versions of the same item are separate variables, often with names that differ by a last digit (`kq15e1` for the mother, `kq15e2` for the father). Give the mother's name to the variable about the mother.
- A variable name often carries the question's number (`kq12b`, `AH12B`). Use that only to confirm a match, never against the wording.
- Match each choice to its code by the category with the same meaning, not by position. Some variables are **reverse coded** (often named with `_r`): their codes run the other way ("not at all" = 3 … "a lot" = 0), and each choice must get its own category's code. Copy each code exactly as the file prints it: `1`, `0`, `-9`, `a`, `B`.
- A check-all-that-apply list is usually one variable per box, each coded as marked or not (for example `1 = Yes, 0 = No`). Some files code it as one variable instead: see "A check-all list coded as one variable" below.
- A written-in variable labelled "If" and a choice ("If other, describe", "If yes, specify") holds what respondents wrote for that choice. Its entry is the one for that write-in: in a REDCap data dictionary, the field printed right after its question that only asks to describe or specify, shown only for that choice (`[diagn_o]  Please describe:` with "Show the field ONLY if: [diagn] = '4'"); in other files, an entry such as "Other, specify" or a name ending in `_o`, `_oth` or `_spec`. It has no codes.
- A question's words may show an earlier answer in square brackets where a REDCap file prints that field's name ("Did you live [alone / with others]?" is the file's "Did you live [livewith] before the pandemic?"). Match them by the rest of the wording.
- Give only what the file says. If you can't find a variable's entry, or you're unsure which of two entries it is, leave the variable out. If the entry gives no code for one of its choices, leave that choice out. Never invent a variable name or a code, and never give the same variable name to two variables (but for the boxes below).

## Categories the questionnaire doesn't print

An entry often codes more categories than the questionnaire prints for its variable: a "No" for a lone box left unmarked (the box prints only "Yes", the entry codes `1 = Yes; 2 = No`), or a code for an answer that is missing (`99 = No answer`, `-9 = Refused`, `999 = Prefer not to answer` when the questionnaire doesn't offer it). Give each such category in the variable's `more_choices`, with its `code` exactly as the file prints it and its `answer` in the registry's standard wording: all in lower case, abbreviations written out ("DK" is "don't know"), and, when one of these registry wordings says the same thing, that one exactly as listed:
{{answer_options}}

- Only categories this variable's own entry lists, with their codes. Never one you work out, one that "usually" goes with such a question, or a code listed once for the whole file.
- Only for a variable whose choices are all in the file: give every printed choice its code in `choices` too.
- Not for a category that matches one of the variable's choices: that choice gets the code in `choices`.
- Not for a variable without choices (one written in), and not for the boxes of a check-all list coded as one variable.

## A check-all list coded as one variable

Some files code a check-all-that-apply list as one variable with a code for each box, not one variable per box. A REDCap "checkbox" field does: the field `income` lists its boxes as `1  income___1  My own paycheck`, `2  income___2  My own unemployment benefit`, `999  income___999  Prefer not to answer`. The questionnaire has been read with one variable per box (each labelled with the box's words, or, for a list of people, with the person as `about`).

- Give each box's variable the field's name as its `matr_id`, the same for every box: `income`, never `income___1`, which only names the box for the form's skip instructions.
- Give each box's variable the box's code as its `box_code`: `"1"` for "My own paycheck", `"999"` for "Prefer not to answer". Match boxes to codes by their words, not their order.
- Leave out the box variables' `choices` and `more_choices`: each box becomes one choice of the one variable, coded with its `box_code`.
- Use `box_code` only for a list the file codes this way. A list whose boxes each have their own variable name (`kq12a`, `kq12b`, each coded `1 = Yes, 0 = No`) is coded as usual, with `choices`.

## Scales

When a variable's entry names the published measurement scale its question comes from, give it as the variable's `scale`.

- Write the scale's abbreviation. When it is one of the registry's scales, use the abbreviation exactly as listed here, whether the file gives the abbreviation or the full name:
{{scales}}
  For any other scale, write its abbreviation as the file does, without the brackets.
- An entry that names more than one ("How soon after you woke up did you smoke your first cigarette (FTND) (CDS-12)") has them all, in the file's order, separated by a comma and a space: "FTND, CDS-12".
- Give it only when the entry names one. Never work a scale out from what the question asks, and don't take another bracket for one: "(mother)", "(check one)", "(Range 0-9)", "(kq46a kq46f)".
- Most files name none: then leave `scale` out of every variable.

## Your answer

Answer with one JSON object and nothing else: no prose, no markdown fences. Write it compactly, without indentation or line breaks. Its shape:

{"variables":[{"id":"v1","matr_id":"kq1","scale":"EPQ-R","choices":[{"id":"o1","code":"1"},{"id":"o2","code":"2"}],"more_choices":[{"answer":"no answer","code":"99"}]},{"id":"v2","matr_id":"kq2"},{"id":"v3","matr_id":"income","box_code":"1"},{"id":"v4","matr_id":"income","box_code":"2"}]}

- `variables`: one object for each variable you found in the file, with its `id`, its `matr_id` (the variable name exactly as the file writes it), its `scale` if its entry names one, `choices` (each choice's `id` and `code`) if it has choices and the file codes them, `more_choices` (each category's `answer` and `code`) for categories its entry codes that the questionnaire doesn't print, and `box_code` for a box of a check-all list coded as one variable.
- Every `matr_id`, `scale`, `box_code`, `answer` and `code` is text, in quotes.
- Leave out a variable you didn't find, and leave out `matr_id`, `scale`, `choices`, `more_choices` or `box_code` when the file doesn't give them.
