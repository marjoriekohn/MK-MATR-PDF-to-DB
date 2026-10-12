# Reading a questionnaire for the MATR metadata database

You are reading a printed questionnaire from the Mid-Atlantic Twin Registry (MATR). Record everything respondents were shown and asked, in printed order, as JSON in the shape described below. Researchers use the record to find questions and to see exactly how each was printed, so record what is on the page: never invent or reword questions, choices or instructions (the one exception, the unprinted "No" of a lone check box, is described below). Copy every `text` letter for letter, keeping its capitals and punctuation even where they look odd: don't correct, add or drop a comma, full stop or capital.

## The JSON

Answer with one JSON object and nothing else: no prose, no markdown fences, no explanation. Write it compactly, without indentation or line breaks. These are the only fields, each object's in the order listed:

- the questionnaire: `questionnaire` (an object with `name` and `abbreviation`), then `components` (a list of blocks)
- a block: `component_type`, `continued`, `text`, `question_number`, `self_contained`, `condition`, `parent_question`, `variables` (a list of variables), `children` (a list of blocks)
- a variable: `variable_type`, `label`, `subject`, `answer_data_type`, `response_options` (a list of choices)
- a choice: `answer_option`, `printed`, `printed_label`, `skip_instruction`

`continued`, `self_contained` and `printed` are true or false; every other field holds text. Leave out any field that would be empty: never write null, "" or [] for a field, and never fill one in to be helpful when nothing is printed for it.

## Leave out

- Anything covered by a redaction (a solid black box), even partly. If a question, a grid column or an answer box is partly hidden, leave all of it out rather than guess. Don't mention the redaction.
- Blanks that collect who someone is rather than asking about them: names, street addresses, telephone numbers and dates of birth, including the name, address and birthdate columns of a grid about the respondent's children or brothers and sisters. Keep that grid's other columns, such as sex, relationship or whether they live with the respondent, even when they are all that's left of it (one Question per column, as in "Layouts" below), and keep every number a question asks for, such as an age or a count.
- Marks made on the form after it was printed: handwriting, ticks, crosses, circled or filled-in answers, staff notes and codes in the margins. Record the blank form.
- Page headers and footers, page numbers, form codes, office-use-only boxes, ID or barcode fields, and closing courtesies such as "Thank you for your time."

## Blocks

The questionnaire is a tree of blocks in printed order: a Section's `children` are the blocks printed under it, nested inside it. Never list a Section and then the blocks that belong to it after it. Each block's `component_type` is one of:

- **Section**: holds other blocks (its `children`), and is the only type that can. `text` is the heading printed at its head, including any number or letter ("Part B. Your health"); leave it out when the group has no printed heading.
- **Question**: asks for one or more answers, and has the `variables` that record them.
- **Introduction**: opening text that tells the respondent what the questionnaire or study is about.
- **Instruction**: tells the respondent how to answer, or is the lead-in shared by the items after it.
- **Definition**: says what a term means, such as "A drink is one beer, one glass of wine or one shot of liquor".
- **Interviewer Script**: words an interviewer reads aloud, or directions for the interviewer.
- **Note**: any other printed text that belongs to the questionnaire.

Allowed types: {{component_types}}.

Only Questions have `question_number`, `self_contained`, `condition`, `parent_question` and `variables`, and only Sections have `children` (and `continued`, described under "Page ranges"). Every block except a Section without a heading has `text`.

## Layouts

Questionnaires mix layouts. Build each one from blocks like this.

1. **A numbered question with its own answer boxes** is one Question, placed where it is printed.
2. **A numbered item with lettered sub-items** (a battery under a shared lead-in, such as "5. In the past month: a. … b. …") is a Section without `text`. Its first child is the lead-in, as an Instruction, without its number, unless the lead-in is half a sentence (pattern 8). Each sub-item follows as a Question numbered with the item's number and the sub-item's letter run together: "5a", "5b", … "5aa".
3. **A grid whose rows are items** (conditions, symptoms, foods) and whose columns are answer boxes is laid out the same way: the lead-in and any instructions for filling in the grid make one Instruction (again, unless the lead-in is half a sentence), then each row is a Question with one variable per column, in column order, each labelled by its column heading.
4. **A grid whose rows are blank slots for people or things** (first child, second child, …) and whose columns are separately headed questions about each one: the lead-in is an Instruction, then each column is a Question whose `text` is the column heading (without any column number) and which has one variable per row, labelled "First child", "Second child", "Third child" and so on, in row order. If the columns carry their own printed numbers ("#1", "#2", …), number each column's question with the item number, "#" and the column number: column #3 of item 4 is "4#3". If they don't, a single column takes the item number, and several take the item number plus a, b, c in column order.
5. **A definition printed inside a question**, such as "(a drink is …)", comes out of the question's text. Write it as a Definition block in its own words, without the brackets, starting with a capital and adding no full stop, and put the Definition and the Question together in a Section without `text`, Definition first.
6. **A numbered question followed by an unnumbered part** that asks something more ("If yes, list them below.") is a Section without `text` holding both parts as Questions, numbered with letters added: "12a" and "12b".
7. **A printed heading over several items** is a Section with that heading, holding everything printed under it up to the next heading of the same rank. Sections sit inside sections: a battery (pattern 2), a definition (pattern 5) or a follow-up pair (pattern 6) under a printed heading is a Section inside the heading's Section. Text printed above a group that tells respondents how to answer all of it is an Instruction at the start of the group's Section.
8. **A lead-in that is half a sentence**, which each sub-item or grid row finishes, is part of every one of those questions, not an Instruction: "My health now limits me in . . ." over "a. climbing several flights of stairs." is the Question "My health now limits me in climbing several flights of stairs." (how to join them is under "Questions"). Such a lead-in stops mid-sentence, often on an ellipsis, a dash or a word like "in", "you" or "how often", and each sub-item carries on that sentence instead of starting one of its own. Anything printed before the unfinished sentence, such as "Circle one answer on each line.", is still an Instruction. A lead-in that is complete in itself ("Have you ever had any of the following?") or that sets the scene for sub-items that are whole questions ("On a typical day:" over "How many cups of coffee do you drink?") stays an Instruction.

## Questions

- `text`: the question exactly as printed (spelling, capitals, punctuation), without its number or letter, its answer choices, routing such as "(Go to Question 9)", or the unit printed beside an answer box. Join lines broken by the layout with single spaces, and rejoin words hyphenated across a line break. When the lead-in is half a sentence (pattern 8), `text` is the lead-in followed by the sub-item, joined with a single space: drop the lead-in's number and the ellipsis, dots, dash or colon it trails off with, and keep everything else in both parts as printed, including the sub-item's capitals and closing punctuation. Otherwise keep each sub-item's text as printed, including whether it starts with a capital.
- `question_number`: the printed number as text, without its trailing period or bracket: "7", "12b", "4.3". Every question has a number, and no two share one.
- `self_contained`: false when a researcher who reads only this question, with the instruction at the head of its section, can't tell what it asks because it leans on an earlier question: "If yes, how old were you?", "Please explain.", or items under "During that time..." where "that time" is set by an earlier question. Otherwise true, even if only some respondents are asked it.
- `parent_question`: the `question_number` of the earlier question this one hangs off, if any. A question hangs off an earlier one when it isn't self contained (it hangs off the question it leans on), or when respondents only reach it because of how they answered an earlier question: every question that a skip instruction sends some respondents past hangs off the question with that skip instruction. Questions everyone answers have no parent, including the question a skip instruction sends respondents on to.
- `condition`: the printed words that say who should answer, such as "If yes" or "Women only": just the condition, which also stays in `text`. Leave it out when none is printed. Routing printed on an earlier question is not a condition.

## Variables

A question has one variable for each separate answer a respondent can record, that is, each column the answers would fill in a spreadsheet, in printed order. A question with a blank for weeks, a blank for months and a box for "All of my life" has three variables. A box or several ruled lines for writing one answer in words (a list, an explanation) is one variable, however many lines it has.

- `variable_type`: "closed_ended" when the respondent marks one of the printed choices (boxes, bubbles, ovals, numbers to circle); "open_ended" when they write the answer (a line, a blank, boxes for digits).
- `label`: what this box records, when the question has several boxes or the box has a printed caption or unit; leave it out for a question's only box when it needs no explanation. Use the column heading, caption or unit printed with the box, in sentence case (a capital first letter, the rest as ordinary prose, no capitals used for emphasis), with abbreviations written out: a column headed "If yes, age started" with "yrs." beside the box is "If yes, age started (years)", and a blank followed by "times per week" is "Times per week". People rows in a grid are "First child", "Second child", "First sibling", and so on.
- `subject`: who the recorded answer is about. "Self" when it describes the respondent. When the box records something about other people, the subject naming them: the respondent's children, brothers and sisters, or family (parents, brothers, sisters and children together, as in "has any family member had …"). A question asking whether the respondent has more than five children is about the children. Allowed: {{subjects}}.
- `answer_data_type`, open-ended boxes only: "NUMBER" for counts, ages, years, amounts and other numbers; "TEXT" for words; "DATE" for a date or part of one; "TIME" for a time of day. Allowed: {{answer_data_types}}.
- `response_options`, closed-ended boxes only: the choices, in printed order. When the choices are laid out in several columns, list them in the order of the scale they form, which is normally down the first column, then down the next. When a battery or grid prints its choices once as column headings, every question repeats them.

## Choices (`response_options`)

- `answer_option`: the choice in standard wording: ordinary sentence capitals (no words capitalised for emphasis), abbreviations written out ("M" is "Male", "DK" is "Don't know"), and no routing. When one of these registry wordings means the same thing, use it exactly:
{{answer_options}}
- `printed`: true when the choice is printed on the form.
- `printed_label`: the exact printed words when they differ from `answer_option` in any way, even only in capitals ("NEVER", "M"). Leave it out when they don't, and for a choice that isn't printed.
- `skip_instruction`: the routing printed beside the choice, without brackets, such as "Go to Question 10".
- **A lone check box.** When a question or a grid column offers a single box to mark (a "Yes" bubble on its own, or a box beside "All of my life"), its variable still has two choices: "Yes", printed, then "No", not printed (`printed: false`), meaning the box was left empty. If the box's printed words aren't "Yes", they are the Yes choice's `printed_label` and the variable's `label`. A check-all-that-apply list works the same way, with one variable per box.

## The questionnaire

`questionnaire.name` and `questionnaire.abbreviation` are the title and abbreviation printed on the form. Leave them out when they aren't printed (or are covered).

## Page ranges

A questionnaire is read a few pages at a time, so the end of this prompt names the pages to record ("This PDF has 6 pages. Record pages 3-4."). The whole PDF is there so you can see what comes before and after, but record only the blocks that start on those pages: a question that starts on your last page and runs onto the next is yours, in full, and one that started before your first page isn't. Number questions as printed, and name parents by their printed numbers even when they're on other pages.

If your first page carries on a section that began on an earlier page (more items of the same battery, grid or headed section), start with that Section again, with `continued: true` and its printed heading as `text`, holding only the blocks on your pages; don't repeat its lead-in as an Instruction, though a half-sentence lead-in still begins each question's `text`. Do the same for a section inside it that also carries on. Leave `continued` out everywhere else.

## Example

Printed:

```
PART B. YOUR HABITS
3. Do you drink coffee?   ( ) Yes   ( ) No (Go to Question 5)
4. On a typical day:
   a. How many cups of coffee do you drink?  [__] cups
   b. Do you take it with sugar?  ( ) Yes  ( ) No
5. Mark each place you have lived.   ( ) CITY   ( ) Farm
6. In the past week I have . . .
   a. walked to work.   ( ) Yes   ( ) No
   b. cooked dinner at home.   ( ) Yes   ( ) No
```

Recorded as `components` (written out here with line breaks so it's easy to read):

```json
[{"component_type": "Section", "text": "PART B. YOUR HABITS", "children": [
  {"component_type": "Question", "text": "Do you drink coffee?", "question_number": "3", "self_contained": true,
   "variables": [{"variable_type": "closed_ended", "subject": "Self", "response_options": [
     {"answer_option": "Yes", "printed": true},
     {"answer_option": "No", "printed": true, "skip_instruction": "Go to Question 5"}]}]},
  {"component_type": "Section", "children": [
    {"component_type": "Instruction", "text": "On a typical day:"},
    {"component_type": "Question", "text": "How many cups of coffee do you drink?", "question_number": "4a", "self_contained": true, "parent_question": "3",
     "variables": [{"variable_type": "open_ended", "label": "Cups", "subject": "Self", "answer_data_type": "NUMBER"}]},
    {"component_type": "Question", "text": "Do you take it with sugar?", "question_number": "4b", "self_contained": false, "parent_question": "3",
     "variables": [{"variable_type": "closed_ended", "subject": "Self", "response_options": [
       {"answer_option": "Yes", "printed": true},
       {"answer_option": "No", "printed": true}]}]}]},
  {"component_type": "Question", "text": "Mark each place you have lived.", "question_number": "5", "self_contained": true,
   "variables": [
     {"variable_type": "closed_ended", "label": "City", "subject": "Self", "response_options": [
       {"answer_option": "Yes", "printed": true, "printed_label": "CITY"},
       {"answer_option": "No", "printed": false}]},
     {"variable_type": "closed_ended", "label": "Farm", "subject": "Self", "response_options": [
       {"answer_option": "Yes", "printed": true, "printed_label": "Farm"},
       {"answer_option": "No", "printed": false}]}]},
  {"component_type": "Section", "children": [
    {"component_type": "Question", "text": "In the past week I have walked to work.", "question_number": "6a", "self_contained": true,
     "variables": [{"variable_type": "closed_ended", "subject": "Self", "response_options": [
       {"answer_option": "Yes", "printed": true},
       {"answer_option": "No", "printed": true}]}]},
    {"component_type": "Question", "text": "In the past week I have cooked dinner at home.", "question_number": "6b", "self_contained": true,
     "variables": [{"variable_type": "closed_ended", "subject": "Self", "response_options": [
       {"answer_option": "Yes", "printed": true},
       {"answer_option": "No", "printed": true}]}]}]}]}]
```

Everything under the heading is inside its Section, and each battery is a Section inside that. 4b isn't self contained ("it" is the coffee from question 3). 4a and 4b hang off 3 because "No" skips them. Questions 5 and 6 are everyone's again, so they have no parent. The lead-in of 4 sets the scene for two whole questions, so it is an Instruction; the lead-in of 6 is half a sentence that each sub-item finishes, so it begins both questions' `text` and there is no Instruction.

## Before you answer

Check that you went through every page to the end, that every Section holds the blocks printed under it, that no Instruction is half a sentence that the questions after it finish, that every question has at least one variable, that every closed-ended variable has at least two choices, that no two questions share a number, that every question that isn't self contained names a parent, and that every parent is a question printed earlier.
