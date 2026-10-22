# Reading a questionnaire for the MATR metadata database

You are reading a printed questionnaire from the Mid-Atlantic Twin Registry (MATR). Record everything respondents were shown and asked, in printed order, as JSON in the shape described below. Researchers use the record to find questions and to see exactly how each was printed, so record what is on the page: never invent or reword questions, choices or instructions, and never add a choice that isn't printed. Copy every `text` letter for letter, keeping its capitals and punctuation even where they look odd: don't correct, add or drop a comma, full stop or capital. The one exception is dots that only lead the eye to the answers (see "Questions").

## The JSON

Answer with one JSON object and nothing else: no prose, no markdown fences, no explanation. Write it compactly, without indentation or line breaks. These are the only fields, each object's in the order listed:

- the questionnaire: `questionnaire` (an object with `name` and `abbreviation`), then `components` (a list of blocks)
- a block: `component_type`, `continued`, `text`, `printed_number`, `self_contained`, `condition`, `parent_question`, `variables` (a list of variables), `children` (a list of blocks)
- a variable: `variable_type`, `label`, `subject`, `answer_data_type`, `skip_condition`, `skip_instruction`, `response_options` (a list of choices)
- a choice: `answer_option`, `printed_label`, `skip_instruction`

`continued` and `self_contained` are true or false; every other field holds text. Leave out any field that would be empty: never write null, "" or [] for a field, and never fill one in to be helpful when nothing is printed for it.

## Leave out

- Anything covered by a redaction (a solid black box), even partly. If a question, a grid column or an answer box is partly hidden, leave all of it out rather than guess. Don't mention the redaction.
- Blanks that collect who someone is rather than asking about them: names, street addresses, telephone numbers and dates of birth, including the name, address and birthdate columns of a grid about the respondent's children or brothers and sisters. Keep that grid's other columns, such as sex, relationship or whether they live with the respondent, even when they are all that's left of it (one Question per column, as in "Layouts" below), and keep every number a question asks for, such as an age or a count.
- Marks made on the form after it was printed: handwriting, ticks, crosses, circled or filled-in answers, staff notes and codes in the margins. Record the blank form.
- Page headers and footers, page numbers, form codes, office-use-only boxes, ID or barcode fields, and closing courtesies such as "Thank you for your time."
- The date the form is filled in ("Today's date", "Please provide today's date"), with any heading that holds nothing else.

## Blocks

The questionnaire is a tree of blocks in printed order: a Section's or Question Group's `children` are the blocks printed under it, nested inside it. Never list a Section or Question Group and then the blocks that belong to it after it. Each block's `component_type` is one of:

- **Section**: a part of the questionnaire under a printed heading, holding the blocks printed under it (its `children`). `text` is the heading, including any number or letter ("Part B. Your health"). A group with no printed heading is a Question Group, not a Section.
- **Question Group**: sets apart some of a Section's blocks that belong together and have no heading of their own (its `children`): questions under a lead-in, instruction or definition that only they share, or a question with its follow-up (see "Layouts" and "When to use a Question Group"). It has no `text`: it is only a container. A Question Group can sit inside a Section or another Question Group; a Section never sits inside a Question Group.
- **Question**: asks for one or more answers, and has the `variables` that record them.
- **Introduction**: opening text that tells the respondent what the questionnaire or study is about.
- **Instruction**: tells the respondent how to answer, or is the lead-in shared by the items after it. Routing ("Go to Question 9", "If you are not a twin, go to Question 52") is never an Instruction: see "Routing".
- **Definition**: says what a term means, such as "A drink is one beer, one glass of wine or one shot of liquor".
- **Interviewer Script**: words an interviewer reads aloud, or directions for the interviewer.
- **Note**: any other printed text that belongs to the questionnaire.

Allowed types: {{component_types}}.

Only Questions have `printed_number`, `self_contained`, `condition`, `parent_question` and `variables`, and only Sections and Question Groups have `children` (and `continued`, described under "Page ranges"). Every block except a Question Group has `text`.

## Layouts

Questionnaires mix layouts. Build each one from blocks like this.

1. **A numbered question with its own answer boxes** is one Question, placed where it is printed.
2. **A numbered item with lettered sub-items** (a battery under a shared lead-in, such as "5. In the past month: a. … b. …"): the lead-in comes first, as an Instruction, without its number, unless the lead-in is half a sentence (pattern 8). Each sub-item follows as a Question whose `printed_number` is the item's number and the sub-item's letter run together: "5a", "5b", … "5aa". Whether they go in a Question Group is under "When to use a Question Group".
3. **A grid whose rows are items** (conditions, symptoms, foods) and whose columns are answer boxes is laid out the same way: the lead-in and any instructions for filling in the grid make one Instruction (again, unless the lead-in is half a sentence), then each row is a Question with one variable per column, in column order, each labelled by its column heading. A row that only names a group for the rows indented under it ("Diabetes" over "Onset before age 18" and "Onset after age 18", "Cancer" over the kinds of cancer) isn't a block of its own, and makes no Instruction or Question Group: each row under it is a Question whose `text` is the group's name, a space, a hyphen, a space, then the row, as printed: "Diabetes - Onset before age 18".
4. **A grid whose rows are blank slots for people or things** (first child, second child, …) and whose columns are separately headed questions about each one: the lead-in is an Instruction, then each column is a Question whose `text` is the column heading (without any column number) and which has one variable per row, labelled "First child", "Second child", "Third child" and so on, in row order. If the columns carry their own printed numbers ("#1", "#2", …), each column's `printed_number` is the item number, "#" and the column number: column #3 of item 4 is "4#3". If they don't, every column's `printed_number` is the item number. The lead-in and the columns go together in a Question Group, unless they are all their Section holds.
5. **A definition printed inside a question**, such as "(a drink is …)", comes out of the question's text. Write it as a Definition block in its own words, without the brackets, starting with a capital and ending with a full stop, and put the Definition and the Question together in a Question Group, Definition first, unless they are all their Section holds.
6. **A numbered question followed by an unnumbered part** that asks something more ("If yes, list them below.") is two Questions, together in a Question Group unless they are all their Section holds. The first has the printed number ("12"); the second has none, unless a number or letter is printed beside it.
7. **A printed heading over several items** is a Section with that heading, holding everything printed under it up to the next heading of the same rank. Sections sit inside sections: a battery (pattern 2), a definition (pattern 5) or a follow-up pair (pattern 6) under a printed heading goes in a Question Group inside the heading's Section when the Section holds other blocks too; when it is everything the Section holds, its blocks go straight in the Section. Text printed above a group that tells respondents how to answer all of it is an Instruction at the start of the group's Section or Question Group.
8. **A lead-in that is half a sentence**, which each sub-item or grid row finishes, is part of every one of those questions, not an Instruction: "My health now limits me in . . ." over "a. climbing several flights of stairs." is the Question "My health now limits me in climbing several flights of stairs." (how to join them is under "Questions"). Such a lead-in stops mid-sentence, often on an ellipsis, a dash or a word like "in", "you" or "how often", and each sub-item carries on that sentence instead of starting one of its own. Anything printed before the unfinished sentence, such as "Circle one answer on each line.", is still an Instruction. A lead-in that is complete in itself ("Have you ever had any of the following?") or that sets the scene for sub-items that are whole questions ("On a typical day:" over "How many cups of coffee do you drink?") stays an Instruction. Questions that each carry a half-sentence lead-in share nothing else, so they need no Question Group.

### When to use a Question Group

A Question Group sets some blocks apart from the other blocks of their Section (or of the top level): the questions that follow a lead-in, instruction or definition that the Section's other questions don't, or a question with its follow-up. Don't make one:

- just because questions share a printed number (5a, 5b, 5c), or are a numbered item's sub-items: with nothing that only they share, they are Questions straight in the Section;
- when it would hold everything in its Section: the Section already holds them, so they go straight in it (the lead-in Instruction first);
- around a single block.

## Questions

- `text`: the question exactly as printed (spelling, capitals, punctuation), without its number or letter, its answer choices, routing such as "(Go to Question 9)", words saying who answers it ("For females only:", which is its `condition`), or the unit printed beside an answer box. Dots that only lead the eye from the words to the answer boxes (". . .", "....", "…") aren't words: leave them out, and end the text with a colon when the answers finish its sentence ("When it was at its worst, it lasted:"), otherwise with a full stop. The same goes for an Instruction that trails off in dots ("Think about the time when these feelings were at their worst."). Join lines broken by the layout with single spaces, and rejoin words hyphenated across a line break. When the lead-in is half a sentence (pattern 8), `text` is the lead-in followed by the sub-item, joined with a single space: drop the lead-in's number and the ellipsis, dots, dash or colon it trails off with, and keep everything else in both parts as printed, including the sub-item's capitals and closing punctuation. Otherwise keep each sub-item's text as printed, including whether it starts with a capital.
- `printed_number`: the number or label printed beside the question, as text, exactly as printed but without its trailing period or bracket: "7", "12b", "4.3", "Q7". A sub-item's printed letter is run together with its item's number ("5a"). Leave it out when no number or letter is printed for the question: never make one up. Printed numbers can repeat (each section can start again at 1): never change one to make it unique.
- `self_contained`: false when a researcher who reads only this question, with the instruction at the head of its section, can't tell what it asks because it leans on an earlier question: "If yes, how old were you?", "Please explain.", or items under "During that time..." where "that time" is set by an earlier question. Otherwise true, even if only some respondents are asked it.
- `parent_question`: the `printed_number` of the earlier question this one hangs off, if any (when several earlier questions are printed with that number, the nearest one before it is meant). A question hangs off an earlier one when it isn't self contained (it hangs off the question it leans on), or when respondents only reach it because of how they answered an earlier question: every question that a skip instruction sends some respondents past hangs off the question with that skip instruction. Questions everyone answers have no parent, including the question a skip instruction sends respondents on to.
- `condition`: who should answer. For an answer to an earlier question, the printed words, such as "If yes": just the condition, which also stays in `text`. For who the respondent is, the group's name in a word or two, with a capital first letter: "Female" or "Male" for a sex (printed "For females only" or "Women only" is "Female"), otherwise the group, such as "Twins" or "Parents"; the questions that routing about who the respondent is sends some respondents past get one too (see "Routing"). A question has one condition: one that hangs off an earlier question (`parent_question`) has that question's answer as its condition, or none, never who the respondent is as well. Leave it out otherwise. Routing printed on an earlier question's answers is not a condition: it is that answer's skip.

## Variables

A question has one variable for each separate answer a respondent can record, that is, each column the answers would fill in a spreadsheet, in printed order. A question with a blank for weeks, a blank for months and a box for "All of my life" has three variables. A box or several ruled lines for writing one answer in words (a list, an explanation) is one variable, however many lines it has.

- `variable_type`: "closed_ended" when the respondent marks one of the printed choices (boxes, bubbles, ovals, numbers to circle); "open_ended" when they write the answer (a line, a blank, boxes for digits).
- `label`: what this box records, when the question has several boxes or the box has a printed caption or unit; leave it out for a question's only box when it needs no explanation, and for a check box whose one choice already says what it records (a box printed beside "All of my life": see "A lone check box"). Use the column heading, caption or unit printed with the box, in sentence case (a capital first letter, the rest as ordinary prose, no capitals used for emphasis), with abbreviations written out: a column headed "If yes, age started" with "yrs." beside the box is "If yes, age started (years)", and a blank followed by "times per week" is "Times per week". People rows in a grid are "First child", "Second child", "First sibling", and so on.
- `subject`: who the recorded answer is about, when it is about other people than the respondent. Leave it out when it describes the respondent. When the box records something about other people, the subject naming them: the respondent's children, brothers and sisters, or family (parents, brothers, sisters and children together, as in "has any family member had …"). A question asking whether the respondent has more than five children is about the children. Allowed: {{subjects}}.
- `answer_data_type`, open-ended boxes only: "NUMBER" for counts, ages, years, amounts and other numbers; "TEXT" for words; "DATE" for a date or part of one; "TIME" for a time of day. Allowed: {{answer_data_types}}.
- `skip_condition` and `skip_instruction`, open-ended boxes only: routing printed for one answer the respondent might write, such as "If none, go to Question 57" after "How many times have you been pregnant?". `skip_condition` is that answer in a word or two, with a capital first letter ("None", "0", "Never"); `skip_instruction` is the routing, without brackets or the condition ("Go to Question 57"). Leave both out when no routing is printed for the box.
- `response_options`, closed-ended boxes only: the choices printed on the form, in printed order. A respondent can only mark a choice that is printed, so never add one that isn't (such as a "No" for a box left empty). When the choices are laid out in several columns, list them in the order of the scale they form, which is normally down the first column, then down the next. When a battery or grid prints its choices once as column headings, every question repeats them.

## Choices (`response_options`)

- `answer_option`: the choice in standard wording: ordinary sentence capitals (no words capitalised for emphasis), abbreviations written out ("M" is "Male", "DK" is "Don't know"), and no routing. When one of these registry wordings means the same thing, use it exactly:
{{answer_options}}
- `printed_label`: the exact printed words when they differ from `answer_option`, even only in capitals ("NEVER", "M"). Leave it out when they don't, or when the only difference is a full stop at the end or how a fraction is written ("2 ½" and "2 1/2").
- `skip_instruction`: the routing printed beside the choice, without brackets, such as "Go to Question 10".
- **A lone check box.** When a question or a grid column offers a single box to mark, its variable has one choice. Don't add a "No" for the box left empty: it isn't printed.
  - A box printed with words of its own beside it ("All of my life", "These symptoms haven't stopped"): the choice is those words, as its `answer_option` (never "Yes"), and the variable has no `label`, since the choice says what it records.
  - A box with no words of its own (a bubble under a column heading such as "Yourself", or in a grid of children): the choice is "Yes", and the column heading is the variable's `label`.
  - A check-all-that-apply list works the same way, with one variable per box, each with its one choice: the words printed beside the box.

## Routing

Routing tells some respondents to jump ahead: "(Go to Question 9)", "If no, skip to Section C", "If none, go to Question 57", "If you are not a twin, go to Question 52." Record it where it belongs, never as a block of its own (not an Instruction, Note or Interviewer Script), and copy its words as printed, without brackets.

1. **Beside a choice**, or after a question for one of its printed choices ("If no, go to Question 9"): that choice's `skip_instruction` ("Go to Question 9"), without the "If no".
2. **For an answer written in a box** ("If none, go to Question 57"): the variable's `skip_condition` ("None") and `skip_instruction` ("Go to Question 57"), as under "Variables".
3. **About who the respondent is**, not about an answer ("(If you are not a twin, go to Question 52.)", "Men go to Section D"): every question it sends respondents past (from the next question up to, but not including, the one it sends them to) gets a `condition` naming who answers them, turned around into a word or two: "Twins", "Female", "Parents". Nothing else records it, except that a question that hangs off an earlier one keeps that question's answer as its condition instead (a question has one condition).
4. **To the end**: routing that ends the questionnaire ("You have finished", "Stop here", or "Go to Section 5" where Section 5 is only the closing words) is recorded the same way, as printed; it needs no question of its own.

Every question that routing in rules 1 and 2 sends some respondents past hangs off the question with the routing (`parent_question`), as under "Questions".

## The questionnaire

`questionnaire.name` and `questionnaire.abbreviation` are the title and abbreviation printed on the form. Leave them out when they aren't printed (or are covered).

## Page ranges

A questionnaire is read a few pages at a time, so the end of this prompt names the pages to record ("This PDF has 6 pages. Record pages 3-4."). The whole PDF is there so you can see what comes before and after, but record only the blocks that start on those pages: a question that starts on your last page and runs onto the next is yours, in full, and one that started before your first page isn't. Give questions their printed numbers, and name parents by their printed numbers even when they're on other pages.

If your first page carries on a Section or Question Group that began on an earlier page (more items of the same battery, grid or headed section), start with it again, with `continued: true` (and, for a Section, its printed heading as `text`), holding only the blocks on your pages; don't repeat its lead-in as an Instruction, though a half-sentence lead-in still begins each question's `text`. Do the same for a Section or Question Group inside it that also carries on. Leave `continued` out everywhere else.

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
  {"component_type": "Question", "text": "Do you drink coffee?", "printed_number": "3", "self_contained": true,
   "variables": [{"variable_type": "closed_ended", "response_options": [
     {"answer_option": "Yes"},
     {"answer_option": "No", "skip_instruction": "Go to Question 5"}]}]},
  {"component_type": "Question Group", "children": [
    {"component_type": "Instruction", "text": "On a typical day:"},
    {"component_type": "Question", "text": "How many cups of coffee do you drink?", "printed_number": "4a", "self_contained": true, "parent_question": "3",
     "variables": [{"variable_type": "open_ended", "label": "Cups", "answer_data_type": "NUMBER"}]},
    {"component_type": "Question", "text": "Do you take it with sugar?", "printed_number": "4b", "self_contained": false, "parent_question": "3",
     "variables": [{"variable_type": "closed_ended", "response_options": [
       {"answer_option": "Yes"},
       {"answer_option": "No"}]}]}]},
  {"component_type": "Question", "text": "Mark each place you have lived.", "printed_number": "5", "self_contained": true,
   "variables": [
     {"variable_type": "closed_ended", "response_options": [
       {"answer_option": "City", "printed_label": "CITY"}]},
     {"variable_type": "closed_ended", "response_options": [
       {"answer_option": "Farm"}]}]},
  {"component_type": "Question", "text": "In the past week I have walked to work.", "printed_number": "6a", "self_contained": true,
   "variables": [{"variable_type": "closed_ended", "response_options": [
     {"answer_option": "Yes"},
     {"answer_option": "No"}]}]},
  {"component_type": "Question", "text": "In the past week I have cooked dinner at home.", "printed_number": "6b", "self_contained": true,
   "variables": [{"variable_type": "closed_ended", "response_options": [
     {"answer_option": "Yes"},
     {"answer_option": "No"}]}]}]}]
```

Everything under the heading is inside its Section. 4a and 4b follow an Instruction the Section's other questions don't, so they and it are a Question Group; 6a and 6b share nothing but the half sentence each one's `text` begins with, so they sit straight in the Section, like 3 and 5. Every variable is about the respondent, so none has a `subject`. 4b isn't self contained ("it" is the coffee from question 3). 4a and 4b hang off 3 because "No" skips them. Questions 5 and 6 are everyone's again, so they have no parent. Each box of 5 is a lone check box with its own words, so its variable has the one choice those words make ("City", printed "CITY"), no "No", and no label. The lead-in of 4 sets the scene for two whole questions, so it is an Instruction; the lead-in of 6 is half a sentence that each sub-item finishes, so it begins both questions' `text` and there is no Instruction.

## Before you answer

Check that you went through every page to the end, that every Section has its printed heading and holds the blocks printed under it, that every group without a heading is a Question Group with no `text`, that every Question Group sets its blocks apart from others in the same Section (none holds its whole Section, and none is there only because its questions share a number), that no Instruction is half a sentence that the questions after it finish, that every question has at least one variable, that every closed-ended variable has at least one choice and no choice that isn't printed, that no printed number was made up, that every question that isn't self contained names a parent, that every parent is a question printed earlier, that no routing was recorded as an Instruction or other block, that no lone check box with words of its own was recorded as "Yes", and that no `text` ends in leader dots.
