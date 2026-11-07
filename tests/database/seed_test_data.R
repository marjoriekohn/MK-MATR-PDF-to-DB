# Fills the test project with sample MATR data that looks like the real thing.
# From the project folder:
#   Rscript tests/database/seed_test_data.R            (replaces the sample data)
#   Rscript tests/database/seed_test_data.R --dry-run  (builds and checks it, writes nothing)
#
# The data is made up, but shaped like the questionnaires staff have extracted: sections with
# printed headings, introductions and instructions, questions printed with numbers like 15a,
# batteries that are one question with a variable per item, follow-up questions reached by a skip,
# check boxes about the respondent's relatives, MATR IDs in the registry's style, and the answer
# choices real questionnaires print. One questionnaire was asked in two waves (the second adds a
# question), and one wave of another is recorded but asks nothing yet, ready for an extraction.
#
# Each questionnaire goes in the way an extraction does: a Manual extraction prepared by a sample
# ADMIN (Dana Whitfield, dana.whitfield@example.test, left inactive), submitted and added, so its
# rows are the ones the site itself would write and the Edit History lists them. The MeSH
# categories and measurement scales, which no extraction adds, are written directly.
#
# Running it removes the sample data there is first: every registry, study, questionnaire,
# component, answer choice, MeSH category and measurement scale whose name isn't a test's marked one
# ("ZZTEST..."), with what hangs off them. Kinds of block, subjects, staff and drafts stay. The
# applied drafts of an earlier run stay too (the database keeps applied drafts), in the Edit History.
# Don't run it while the tests are running. It only works on the test project or a local database.

dry_run <- "--dry-run" %in% commandArgs(trailingOnly = TRUE)

log_info <- function(...) {
  cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))
  flush.console()
}

# ------ Loading -----------------------------------------------------------------------------------------------
if (file.exists(".env")) readRenviron(".env")
suppressMessages({
  library(shiny)
  library(testthat)
})
for (path in sort(list.files("R", "\\.[Rr]$", full.names = TRUE))) sys.source(path, envir = globalenv())
source("tests/testthat/helper-00_realdb.R")
connection <- test_database()
log_info(if (dry_run) "Dry run: nothing is written. " else "", "Test database: ", connection$url)

# ------ Writing questionnaires --------------------------------------------------------------------------------
# A questionnaire is written as blocks, the way it is printed. A question's `key` names it for the
# questions that follow it and the choices that skip to it. Questions are numbered as printed: the
# next number, or with `item`, a letter after it ("a" starts the next number: 15a, 15b...).

# (A block given as NULL is left out: a question one wave didn't print.)
section <- function(text, ...) list(component_type = "Section", text = text, children = Filter(Negate(is.null), list(...)))
group <- function(...) list(component_type = "Group", children = list(...))
introduction <- function(text) list(component_type = "Introduction", text = text)
instruction <- function(text) list(component_type = "Instruction", text = text)
definition <- function(text) list(component_type = "Definition", text = text)
note <- function(text) list(component_type = "Note", text = text)

question <- function(key, text, ..., item = NULL, follows = NULL, condition = NULL, self_contained = TRUE) {
  list(
    component_type = "Question", key = key, text = text, item = item, variables = list(...),
    follows = follows, condition = condition, self_contained = self_contained
  )
}

# A variable chosen from `choices` (code = words). `skips` says where a choice sends respondents:
# list(No = "key"), or "end"; the printed words are made from the question's number.
# `printed` are words printed differently from the choice's own (code = words).
choose <- function(matr_id, choices, label = NULL, about = NULL, skips = list(), printed = NULL) {
  list(
    variable_type = "closed_ended", matr_id = matr_id, label = label, subject = about,
    choices = choices, skips = skips, printed = printed
  )
}

# A variable the respondent writes. `skip` is list(when = "0", to = "key").
write_in <- function(matr_id, label, type = "NUMBER", about = NULL, skip = NULL) {
  list(variable_type = "open_ended", matr_id = matr_id, label = label, answer_data_type = type, subject = about, skip = skip)
}

# The answer choices printed most often, with the codes the questionnaires give them.
yes_no <- c("1" = "Yes", "2" = "No")
yes_no_01 <- c("1" = "Yes", "0" = "No")
check_box <- c("1" = "Yes")
never_often <- c("0" = "Never", "1" = "Rarely", "2" = "Sometimes", "3" = "Often")
never_frequently <- c("0" = "Never", "1" = "Seldom", "2" = "Occasionally", "3" = "Frequently")
always_never <- c("3" = "Always", "2" = "Usually", "1" = "Sometimes", "0" = "Never")
two_weeks <- c("0" = "Not at all", "1" = "Several days", "2" = "More than half the days", "3" = "Nearly every day")
not_true <- c("0" = "Not true", "1" = "Sometimes true", "2" = "True")
child_true <- c("0" = "Not true", "1" = "Somewhat or sometimes true", "2" = "Very true or often true")
excellent_poor <- c("1" = "Excellent", "2" = "Very good", "3" = "Good", "4" = "Fair", "5" = "Poor")
none_to_four <- c("0" = "None", "1" = "1", "2" = "2", "3" = "3", "4" = "4 or more")
risk <- c("0" = "No risk", "1" = "Slight risk", "2" = "Moderate risk", "3" = "Great risk", "4" = "Don't know")
twin_type <- c("1" = "Identical", "2" = "Fraternal", "3" = "Not sure")

# The reviewed questionnaire JSON (schema 2, as the Edit Extraction page saves it) for blocks written
# with the functions above.
# blocks: The top-level blocks.
# Returns: The JSON as nested lists, for `draft_service_from_json()`.
questionnaire_json <- function(blocks) {
  # every block in printed order, to number them first
  flat <- list()
  visit <- function(blocks) {
    for (block in blocks) {
      flat[[length(flat) + 1]] <<- block
      visit(block$children)
    }
  }
  visit(blocks)
  place <- list() # key -> place in printed order (what a skip names)
  number <- list() # key -> its question_number, 1, 2, 3...
  printed <- list() # key -> the number printed beside it
  n_questions <- 0L
  last_printed <- 0L
  for (i in seq_along(flat)) {
    block <- flat[[i]]
    if (block$component_type != "Question") next
    n_questions <- n_questions + 1L
    if (is.null(block$item) || block$item == "a") last_printed <- last_printed + 1L
    place[[block$key]] <- i
    number[[block$key]] <- as.character(n_questions)
    printed[[block$key]] <- paste0(last_printed, block$item %||% "")
  }
  go_to <- function(key) if (identical(key, "end")) "Go to the end of the questionnaire" else paste("Go to Question", printed[[key]])
  skip_fields <- function(key) {
    if (identical(key, "end")) list(skip_to_end = TRUE) else list(skip_to = place[[key]])
  }
  compact <- function(row) Filter(Negate(is.null), row)

  variable_json <- function(box, order) {
    row <- list(
      matr_id = box$matr_id, label = box$label, display_order = order, variable_type = box$variable_type,
      answer_data_type = box$answer_data_type, subject = box$subject
    )
    if (box$variable_type == "open_ended" && !is.null(box$skip)) {
      row <- c(row, list(skip_condition = box$skip$when, skip_instruction = go_to(box$skip$to)), skip_fields(box$skip$to))
    }
    if (box$variable_type == "closed_ended") {
      row$response_options <- lapply(seq_along(box$choices), function(j) {
        code <- names(box$choices)[j]
        words <- unname(box$choices[j])
        skip <- box$skips[[words]]
        compact(c(
          list(display_order = j, code_value = code, answer_option = words),
          if (!is.null(skip)) c(list(skip_instruction = go_to(skip)), skip_fields(skip))
        ))
      })
    }
    compact(row)
  }
  page <- 1L
  block_json <- function(block, order) {
    row <- list(component_type = block$component_type, text = block$text, display_order = order, checked = TRUE)
    if (block$component_type == "Question") {
      if (as.integer(number[[block$key]]) %% 7L == 0L) page <<- page + 1L # about seven questions to a page
      row <- c(row, list(
        printed_number = printed[[block$key]], question_number = number[[block$key]],
        self_contained = block$self_contained, condition = block$condition,
        parent_question = if (!is.null(block$follows)) number[[block$follows]],
        parent_from_skip = if (!is.null(block$follows)) TRUE,
        variables = lapply(seq_along(block$variables), function(j) variable_json(block$variables[[j]], j))
      ))
    }
    row$page <- page
    if (!is.null(block$children)) row$children <- lapply(seq_along(block$children), function(j) block_json(block$children[[j]], j))
    compact(row)
  }
  list(
    schema_version = 2L, questionnaire = list(),
    components = lapply(seq_along(blocks), function(j) block_json(blocks[[j]], j))
  )
}

# ------ The questionnaires ------------------------------------------------------------------------------------

# The Adult Health Questionnaire, asked of adult twins. `wave` 2 added a question about vaping.
adult_health <- function(wave = 1) {
  list(
    introduction(paste(
      "This questionnaire asks about your health and your health habits. Please answer every question as",
      "well as you can. If you are not sure of an answer, give your best guess. Your answers will be kept",
      "confidential and will only be used for research."
    )),
    section(
      "About Your Health in General",
      question("dob", "What is your date of birth?", write_in("HQ1", "Date of birth", "DATE")),
      question("height", "How tall are you without shoes?", write_in("HQ2FT", "Feet"), write_in("HQ2IN", "Inches")),
      question("weight", "How much do you weigh now, wearing indoor clothing?", write_in("HQ3", "Pounds")),
      question("health", "In general, would you say your health is:", choose("HQ4", excellent_poor)),
      question(
        "health_year", "Compared to one year ago, how would you rate your health in general now?",
        choose("HQ5", c(
          "1" = "Much better now than one year ago", "2" = "Somewhat better now than one year ago",
          "3" = "About the same as one year ago", "4" = "Somewhat worse now than one year ago",
          "5" = "Much worse now than one year ago"
        ))
      ),
      question(
        "limits", "Do you have a long-term illness, health problem, or disability that limits your daily activities?",
        choose("HQ6A", yes_no, printed = c("1" = "Yes, please describe:")),
        write_in("HQ6B", "Please describe", "TEXT")
      )
    ),
    section(
      "About Your Health Habits",
      question(
        "exercise",
        "During a typical week, on how many days do you do at least 30 minutes of moderate physical activity (for example, brisk walking, gardening, or cycling)?",
        write_in("HQ7", "Days")
      ),
      question(
        "smoked100", "Have you smoked at least 100 cigarettes in your entire life?",
        choose("HQ8", yes_no, skips = list(No = "drink_ever"))
      ),
      question(
        "smoke_now", "Do you now smoke cigarettes every day, some days, or not at all?",
        choose("HQ9", c("1" = "Every day", "2" = "Some days", "3" = "Not at all")),
        follows = "smoked100"
      ),
      question(
        "drink_ever", "Have you ever had a drink of alcohol (beer, wine, wine coolers, or liquor)?",
        choose("HQ10", yes_no, skips = list(No = "sleep"))
      ),
      question(
        "drink_days", "During the past 30 days, on how many days did you have at least one drink of alcohol?",
        write_in("HQ11", "Days"),
        follows = "drink_ever"
      ),
      question(
        "drink_amount", "On the days that you drank, about how many drinks did you usually have?",
        choose("HQ12", c("1" = "1 drink", "2" = "2 drinks", "3" = "3 or 4 drinks", "4" = "5 or 6 drinks", "5" = "7 or more drinks")),
        follows = "drink_ever"
      ),
      question("sleep", "On a typical weeknight, how many hours of sleep do you get?", write_in("HQ13", "Hours")),
      if (wave >= 2) {
        question(
          "vape", "In the past 30 days, have you used an e-cigarette or other vaping product, even once?",
          choose("HQ51", yes_no)
        )
      }
    ),
    section(
      "About Your Mood",
      instruction("Over the last 2 weeks, how often have you been bothered by any of the following problems? Please mark one answer for each."),
      question("phq1", "Little interest or pleasure in doing things", choose("HQ14A", two_weeks), item = "a"),
      question("phq2", "Feeling down, depressed, or hopeless", choose("HQ14B", two_weeks), item = "b"),
      question("gad1", "Feeling nervous, anxious, or on edge", choose("HQ14C", two_weeks), item = "c"),
      question("gad2", "Not being able to stop or control worrying", choose("HQ14D", two_weeks), item = "d"),
      question(
        "treated", "In the past 12 months, have you talked with a doctor, counselor, or other professional about your mood or nerves?",
        choose("HQ15", yes_no)
      )
    ),
    section(
      "About Health Conditions in Your Family",
      instruction(paste(
        "For each condition below, mark whether you have ever had it and whether any of your blood relatives",
        "have had it. If any of your relatives have had it, write how many."
      )),
      definition("Blood relatives are your parents, brothers, sisters, children, grandparents, aunts, and uncles who are related to you by birth, not by marriage or adoption."),
      family_condition("hbp", "a", "High blood pressure", "HQ16A"),
      family_condition("diabetes", "b", "Diabetes (sugar)", "HQ16B"),
      family_condition("heart", "c", "Heart attack", "HQ16C"),
      family_condition("stroke", "d", "Stroke", "HQ16D"),
      family_condition("asthma", "e", "Asthma", "HQ16E"),
      family_condition("depression", "f", "Depression", "HQ16F"),
      family_condition("alcohol", "g", "Alcohol or drug problems", "HQ16G")
    ),
    section(
      "For Females Only",
      question("menarche", "How old were you when you had your first menstrual period?", write_in("HQ17", "Age"), condition = "Female"),
      question(
        "pregnant", "How many times have you been pregnant?",
        write_in("HQ18", "Times", skip = list(when = "0", to = "twin_contact")),
        condition = "Female"
      ),
      question(
        "births", "How many of those pregnancies resulted in a live birth?",
        write_in("HQ19", "Live births"),
        condition = "Female", follows = "pregnant"
      )
    ),
    section(
      "About You and Your Twin",
      question(
        "twin_contact", "How often do you and your twin see each other, talk on the phone, or send messages?",
        choose("HQ20", c(
          "1" = "Every day", "2" = "A few times a week", "3" = "About once a week", "4" = "A few times a month",
          "5" = "About once a month", "6" = "A few times a year", "7" = "Less than once a year"
        )),
        condition = "Twins"
      ),
      question(
        "mistaken", "When you were growing up, how often were you and your twin mistaken for each other by:",
        choose("HQ21A", never_often, "your parents?"),
        choose("HQ21B", never_often, "other family members?"),
        choose("HQ21C", never_often, "teachers?"),
        choose("HQ21D", never_often, "friends?"),
        condition = "Twins"
      ),
      question("zygosity", "Do you think you and your twin are identical or fraternal?", choose("HQ22", twin_type), condition = "Twins")
    ),
    note("Thank you for completing this questionnaire. Please return it in the postage-paid envelope provided.")
  )
}

# A condition of the respondent's and their relatives', as the Adult Health Questionnaire prints them.
family_condition <- function(key, item, text, matr_id) {
  question(
    key, text,
    choose(paste0(matr_id, "S"), check_box, "Yourself"),
    choose(paste0(matr_id, "R"), check_box, "Your relatives", about = "Respondent's Family"),
    write_in(paste0(matr_id, "N"), "Number of relatives affected", about = "Respondent's Family"),
    item = item
  )
}

# The Teen Questionnaire, answered by adolescent twins.
teen <- function() {
  list(
    introduction(paste(
      "This survey asks about you, your friends, and your family. There are no right or wrong answers.",
      "Please answer each question by yourself, without talking to your twin about it."
    )),
    section(
      "About Yourself",
      question("sex", "What is your sex?", choose("tq1", c("1" = "Male", "2" = "Female"))),
      question("age", "How old are you?", write_in("tq2", "Years")),
      question(
        "grade", "What grade are you in?",
        choose("tq3", c(
          "6" = "6th grade", "7" = "7th grade", "8" = "8th grade", "9" = "9th grade", "10" = "10th grade",
          "11" = "11th grade", "12" = "12th grade", "0" = "Not in school"
        ))
      )
    ),
    section(
      "About Your Activities",
      group(
        instruction("Please give an answer for \"a\" - \"d\"."),
        question(
          "activities", "In the past 6 months, how often did you take part in:",
          choose("tq4a", never_frequently, "school sports or teams?"),
          choose("tq4b", never_frequently, "music, art, drama, or dance lessons?"),
          choose("tq4c", never_frequently, "clubs or youth groups?"),
          choose("tq4d", never_frequently, "volunteer work?")
        )
      ),
      question(
        "screens", "On a school night, about how many hours do you spend on a phone, computer, TV, or video games for fun?",
        choose("tq5", c("0" = "Less than 1 hour", "1" = "1-2 hours", "2" = "3-4 hours", "3" = "5 or more hours"))
      )
    ),
    section(
      "About Your Friends and Family",
      question("friends", "Not counting your twin, how many close friends do you have?", choose("tq6", none_to_four)),
      group(
        note("Remember to give an answer for \"a\" and \"b\"."),
        question(
          "siblings", "Not counting your twin, how many brothers and sisters do you have (include step and half brothers and sisters)?",
          choose("tq7a", none_to_four, "Brothers", about = "Respondent's Siblings"),
          choose("tq7b", none_to_four, "Sisters", about = "Respondent's Siblings")
        )
      ),
      question(
        "get_along", "How well do you get along with your twin?",
        choose("tq8", c("1" = "Very well", "2" = "Fairly well", "3" = "Not very well", "4" = "Not at all well"))
      ),
      question(
        "close", "In the past 12 months, how often:",
        choose("tq9a", always_never, "did you feel close to your mother?"),
        choose("tq9b", always_never, "did you feel close to your father?"),
        choose("tq9c", always_never, "could you talk to a parent about a problem?")
      )
    ),
    section(
      "About Smoking and Drinking",
      instruction("Remember, your answers are private. No one in your family or at your school will see them."),
      question(
        "cig_ever", "Have you ever tried a cigarette, even one or two puffs?",
        choose("tq10", yes_no_01, skips = list(No = "alc_ever"))
      ),
      question("cig_age", "How old were you when you first tried a cigarette?", write_in("tq11", "Age"), follows = "cig_ever"),
      question(
        "alc_ever", "Have you ever had a drink of alcohol, more than just a sip?",
        choose("tq12", yes_no_01, skips = list(No = "risk"))
      ),
      question(
        "alc_days", "During the past 30 days, on how many days did you have at least one drink of alcohol?",
        choose("tq13", c("0" = "0 days", "1" = "1 or 2 days", "2" = "3 to 5 days", "3" = "6 to 9 days", "4" = "10 or more days")),
        follows = "alc_ever"
      ),
      question(
        "risk", "How much do you think people risk harming themselves if they:",
        choose("tq14a", risk, "smoke one or more packs of cigarettes a day?"),
        choose("tq14b", risk, "have five or more drinks once or twice a week?"),
        choose("tq14c", risk, "use marijuana once or twice a week?")
      )
    ),
    section(
      "About How You Have Been Feeling",
      instruction("For each sentence, mark how true it has been for you in the past 3 months."),
      question("sad", "I felt sad or down.", choose("tq15a", not_true), item = "a"),
      question("interest", "I lost interest in things I usually enjoy.", choose("tq15b", not_true), item = "b"),
      question("sleep", "I had trouble falling asleep or staying asleep.", choose("tq15c", not_true), item = "c"),
      question("tired", "I felt tired or had very little energy.", choose("tq15d", not_true), item = "d"),
      question("worried", "I felt worried or nervous.", choose("tq15e", not_true), item = "e")
    )
  )
}

# The Parent Report on Twins, answered by a parent about both twins.
parent_report <- function() {
  twins <- "Respondent's Children"
  per_twin <- function(matr_id, choices) {
    list(
      choose(paste0(matr_id, "A"), choices, "Twin A", about = twins),
      choose(paste0(matr_id, "B"), choices, "Twin B", about = twins)
    )
  }
  behavior <- function(key, item, text, matr_id) do.call(question, c(list(key, text), per_twin(matr_id, child_true), list(item = item)))
  list(
    introduction(paste(
      "This questionnaire asks about your twins. If you have more than one set of twins, please answer about",
      "the twins named in the cover letter."
    )),
    section(
      "About You",
      question(
        "relation", "What is your relationship to the twins?",
        choose("PR1", c(
          "1" = "Mother", "2" = "Father", "3" = "Stepmother", "4" = "Stepfather", "5" = "Grandparent",
          "6" = "Other relative", "7" = "Other"
        ))
      ),
      question("parent_dob", "What is your date of birth?", write_in("PR2", "Date of birth", "DATE")),
      question(
        "education", "What is the highest level of school you have completed?",
        choose("PR3", c(
          "1" = "Less than high school", "2" = "High school diploma or GED", "3" = "Some college or technical school",
          "4" = "College degree (4 years)", "5" = "Graduate or professional degree"
        ))
      )
    ),
    section(
      "About the Twins' Birth",
      definition("Twin A is the twin who was born first. Twin B is the twin who was born second."),
      question(
        "birth_weight", "What was each twin's weight at birth?",
        write_in("PR4ALB", "Twin A (pounds)", about = twins), write_in("PR4AOZ", "Twin A (ounces)", about = twins),
        write_in("PR4BLB", "Twin B (pounds)", about = twins), write_in("PR4BOZ", "Twin B (ounces)", about = twins)
      ),
      question("gestation", "How many weeks pregnant was the twins' mother when they were born? (40 weeks is full term)", write_in("PR5", "Weeks", about = twins)),
      do.call(question, c(list("nicu", "Did either twin stay in a neonatal intensive care unit (NICU) after birth?"), per_twin("PR6", yes_no))),
      question("zygosity", "Are the twins identical or fraternal?", choose("PR7", twin_type, about = twins)),
      question(
        "mistaken", "When the twins were young children, how often were they mistaken for each other by:",
        choose("PR8A", never_often, "you?", about = twins),
        choose("PR8B", never_often, "other family members?", about = twins),
        choose("PR8C", never_often, "teachers or babysitters?", about = twins)
      )
    ),
    section(
      "About the Twins' Behavior",
      instruction("Below is a list of items that describe children. For each item, mark how true it is of each twin now or within the past 6 months."),
      behavior("still", "a", "Has trouble sitting still", "PR9A"),
      behavior("worries", "b", "Worries a lot", "PR9B"),
      behavior("fights", "c", "Gets in fights with other children", "PR9C"),
      behavior("shy", "d", "Is shy or timid", "PR9D"),
      behavior("sleeping", "e", "Has trouble sleeping", "PR9E")
    ),
    section(
      "About the Twins' Health",
      do.call(question, c(list("asthma", "Has a doctor ever told you that either twin has asthma?"), per_twin("PR10", yes_no))),
      do.call(question, c(list("adhd", "Has a doctor ever told you that either twin has attention-deficit/hyperactivity disorder (ADHD)?"), per_twin("PR11", yes_no))),
      question(
        "visits", "In the past 12 months, how many times has each twin seen a doctor for an illness or injury?",
        write_in("PR12A", "Twin A", about = twins), write_in("PR12B", "Twin B", about = twins)
      ),
      question("smoker", "Does anyone who lives in the twins' home smoke cigarettes?", choose("PR13", yes_no, about = "Respondent's Family")),
      question("comments", "Please write any other comments you would like to share about your twins.", write_in("PR14", "Comments", "TEXT"))
    ),
    note("Thank you! Please return this questionnaire in the envelope provided.")
  )
}

# ------ Basic Info --------------------------------------------------------------------------------------------
registry <- list(
  name = "Mid-Atlantic Twin Registry", abbreviation = "MATR", participant_count = 50000,
  description = paste(
    "The MATR is a population-based twin registry housed in Virginia Commonwealth University (VCU)'s Office of",
    "the Vice President for Research & Innovation. With more than 50,000 individuals of varying ages and",
    "demographic backgrounds, the MATR is the largest twin registry in the United States. Most MATR registrants",
    "live in Virginia, North Carolina, and South Carolina, but twins from all over the US are part of the registry."
  )
)
investigators <- list(
  whitcomb = list(name = "Elaine Whitcomb", email = "ewhitcomb@example.edu"),
  delaney = list(name = "Marcus Delaney", email = "mdelaney@example.edu"),
  raman = list(name = "Priya Raman", email = "praman@example.edu"),
  kessler = list(name = "Thomas Kessler", email = "tkessler@example.edu")
)
respondent_types <- list(
  adult = list(name = "Adult Twin", minimum_age = 18, maximum_age = 110, description = "An adult twin answering about themself"),
  teen = list(name = "Adolescent Twin", minimum_age = 11, maximum_age = 17, description = "A twin aged 11 to 17 answering about themself"),
  parent = list(name = "Parent of Twins", minimum_age = 18, maximum_age = 99, description = "A parent or guardian answering about their twins")
)
studies <- list(
  health = list(
    name = "Adult Twin Health and Lifestyle Study", status = "Active", pi = "whitcomb", contact = "delaney",
    description = "How genes and environment shape the physical health, mood, and health habits of adult twins.",
    methods = paste(
      "Paper questionnaires mailed to adult twin pairs enrolled in the MATR, with an online version offered.",
      "Non-responders received a reminder postcard at 3 weeks and a second questionnaire at 6 weeks."
    ),
    acknowledgements = "Funded by the National Institutes of Health. We thank the twins who take part in the MATR."
  ),
  teens = list(
    name = "Twin Adolescent Development Study", status = "Ended", pi = "raman",
    description = "Friendships, family relationships, and early substance use in adolescent twins and their parents.",
    methods = "Each twin and one parent completed separate questionnaires at home, returned in sealed envelopes."
  ),
  aging = list(
    name = "Aging Twins Memory and Health Study", status = "Active", pi = "kessler", contact = "kessler",
    description = "Changes in memory, daily functioning, and health among twins aged 60 and older."
  )
)
# Each questionnaire, its waves, and the content each wave asked (NULL: recorded, not extracted yet).
questionnaires <- list(
  list(
    study = "health", respondent_type = "adult", name = "Adult Health Questionnaire", abbreviation = "AHQ",
    description = "Health, health habits, mood, and family history of adult twins.",
    waves = list(
      list(name = "Wave 1", administered_date = "2014-03-10", respondent_count = 4812, blocks = adult_health(1)),
      list(name = "Wave 2", administered_date = "2018-09-24", respondent_count = 3967, blocks = adult_health(2))
    )
  ),
  list(
    study = "teens", respondent_type = "teen", name = "Teen Questionnaire", abbreviation = "TQ",
    description = "Activities, friends, family, substance use, and mood of adolescent twins.",
    waves = list(list(name = "Wave 1", administered_date = "2009-10-05", respondent_count = 1186, blocks = teen()))
  ),
  list(
    study = "teens", respondent_type = "parent", name = "Parent Report on Twins", abbreviation = "PRT",
    description = "A parent's report of each twin's birth, behavior, and health.",
    waves = list(list(name = "Wave 1", administered_date = "2009-10-05", respondent_count = 902, blocks = parent_report()))
  ),
  list(
    study = "aging", respondent_type = "adult", name = "Memory and Daily Living Questionnaire", abbreviation = "MDL",
    description = "Memory, daily activities, and health of older twins.",
    waves = list(list(name = "Wave 1", administered_date = "2025-04-01", respondent_count = NA, blocks = NULL))
  )
)

# MeSH headings (their NLM unique IDs) and the questions about them, by a MATR ID of each question.
categories <- list(
  D001827 = list(term = "Body Height", note = "The distance from the sole to the crown of the head.", matr = "HQ2FT"),
  D001835 = list(term = "Body Weight", note = "The mass or quantity of heaviness of an individual.", matr = "HQ3"),
  D006304 = list(term = "Health Status", note = "The level of health of the individual.", matr = c("HQ4", "HQ5", "HQ6A")),
  D015444 = list(term = "Exercise", note = "Physical activity which is usually regular and done with the intention of improving health.", matr = c("HQ7", "tq4a")),
  D012907 = list(term = "Smoking", note = "Inhaling and exhaling the smoke of burning tobacco.", matr = c("HQ8", "HQ9", "tq10", "tq11", "PR13")),
  D000428 = list(term = "Alcohol Drinking", note = "Behaviors associated with the ingesting of alcoholic beverages.", matr = c("HQ10", "HQ11", "HQ12", "tq12", "tq13")),
  D012890 = list(term = "Sleep", note = "A readily reversible suspension of sensorimotor interaction with the environment.", matr = c("HQ13", "tq15c")),
  D003863 = list(term = "Depression", note = "Depressive states, usually of moderate intensity.", matr = c("HQ14A", "HQ14B", "HQ16FS", "tq15a", "tq15b")),
  D001007 = list(term = "Anxiety", note = "Feelings or emotions of dread, apprehension, and impending disaster.", matr = c("HQ14C", "HQ14D", "tq15e")),
  D006973 = list(term = "Hypertension", note = "Persistently high systemic arterial blood pressure.", matr = "HQ16AS"),
  D003920 = list(term = "Diabetes Mellitus", note = "A heterogeneous group of disorders characterized by hyperglycemia.", matr = "HQ16BS"),
  D001249 = list(term = "Asthma", note = "A form of bronchial disorder with episodic narrowing of the airways.", matr = c("HQ16ES", "PR10A")),
  D001289 = list(term = "Attention Deficit Disorder with Hyperactivity", note = "A behavior disorder of inattention, impulsivity, and hyperactivity.", matr = c("PR11A", "PR9AA")),
  D001724 = list(term = "Birth Weight", note = "The mass or quantity of heaviness of an individual at birth.", matr = "PR4ALB"),
  D011247 = list(term = "Pregnancy", note = "The status during which female mammals carry their developing young.", matr = c("HQ18", "HQ19")),
  D014427 = list(term = "Twins", note = "Two individuals derived from two fetuses that were fertilized at or about the same time.", matr = c("HQ20", "HQ21A", "HQ22", "tq8", "PR7", "PR8A"))
)
# Published scales some questions belong to, with the MATR ID of each item in order.
scales <- list(
  list(
    name = "Patient Health Questionnaire-2", abbreviation = "PHQ-2", version = "1.0",
    description = "Two-item screener for depression: the first two items of the PHQ-9.",
    citation = "Kroenke K, Spitzer RL, Williams JBW. The Patient Health Questionnaire-2: validity of a two-item depression screener. Med Care. 2003;41(11):1284-1292.",
    items = c("HQ14A", "HQ14B")
  ),
  list(
    name = "Generalized Anxiety Disorder-2", abbreviation = "GAD-2", version = "1.0",
    description = "Two-item screener for generalized anxiety: the first two items of the GAD-7.",
    citation = "Kroenke K, Spitzer RL, Williams JBW, Monahan PO, Lowe B. Anxiety disorders in primary care: prevalence, impairment, comorbidity, and detection. Ann Intern Med. 2007;146(5):317-325.",
    items = c("HQ14C", "HQ14D")
  )
)

# ------ Checking the questionnaires ----------------------------------------------------------------------------
# Each wave's questionnaire is checked as the Edit Extraction page checks one before it is sent, and its rows are
# worked out as they would be pushed into an empty questionnaire, so a mistake here stops before anything changes.
lookups <- lookup_service_get_all(connection)
context <- extraction_service_push_context(connection)
empty <- list(
  placements = data.frame(placement_id = numeric(), component_id = numeric(), parent_placement_id = numeric(), display_order = numeric()),
  components = data.frame(component_id = numeric(), component_type_id = numeric(), text = character()),
  questions = data.frame(placement_id = numeric()),
  variables = data.frame(variable_key = numeric(), placement_id = numeric(), matr_id = character(), display_order = numeric()),
  waves = data.frame(wave_id = 1, name = "Wave 1"),
  wave_questions = data.frame(wave_id = numeric(), placement_id = numeric())
)
drafts <- list()
for (q in questionnaires) {
  for (w in q$waves) {
    if (is.null(w$blocks)) next
    label <- paste(q$abbreviation, w$name)
    draft <- draft_service_from_json(questionnaire_json(w$blocks))
    problems <- draft_service_check(draft, lookups)
    if (nrow(problems) > 0) {
      print(problems)
      stop(label, " has problems to fix (above).", call. = FALSE)
    }
    built <- extraction_service_build_items(draft, list(questionnaire_id = "1", wave_id = "1"), context$ids, empty, context$options)
    summary <- built$summary
    log_info(sprintf(
      "%s: %d blocks, %d questions, %d variables, %d choices (%d new answer choices, %d new sets of choices)",
      label, summary$blocks + summary$blocks_found + summary$blocks_shared, summary$questions, summary$variables, summary$choices, summary$answer_options, summary$option_sets
    ))
    drafts[[label]] <- draft
  }
}
if (dry_run) {
  log_info("Dry run finished: the questionnaires are fine. Nothing was written.")
  quit(save = "no")
}

# ------ Removing the sample data there is -------------------------------------------------------------------------
log_info("Removing the sample data there is (rows not marked ZZTEST)")
unmarked <- function(column) stats::setNames(list("not.like.ZZTEST*"), column)
ids_where <- function(table, key, filters = list()) {
  database_model_get(connection, table, key, order_by = key, filters = filters)[[key]]
}

old_scales <- ids_where("measurement_scale", "scale_id", unmarked("name"))
old_categories <- ids_where("category", "mesh_id", unmarked("term"))
test_delete_in("question_scale", "scale_id", old_scales)
test_delete_in("question_category", "mesh_id", old_categories)

old_questionnaires <- ids_where("questionnaire", "questionnaire_id", unmarked("name"))
for (table in c("variable", "wave_question", "questionnaire_question", "questionnaire_component", "wave")) {
  test_delete_in(table, "questionnaire_id", old_questionnaires)
}
# the blocks no questionnaire prints any more
placed <- ids_where("questionnaire_component", "component_id")
old_components <- setdiff(ids_where("component", "component_id", list(or = "(text.is.null,text.not.like.ZZTEST*)")), placed)
for (table in c("question_category", "question_scale", "question", "component")) {
  test_delete_in(table, "component_id", old_components)
}
test_delete_in("questionnaire", "questionnaire_id", old_questionnaires)
log_info(sprintf("  %d questionnaires and %d blocks", length(old_questionnaires), length(old_components)))

for (spec in list(
  c("study", "study_id", "name"), c("principal_investigator", "principal_investigator_id", "name"),
  c("respondent_type", "respondent_type_id", "name"), c("registry", "registry_id", "name")
)) {
  ids <- ids_where(spec[1], spec[2], unmarked(spec[3]))
  test_delete_in(spec[1], spec[2], ids)
  log_info(sprintf("  %d rows of %s", length(ids), spec[1]))
}

# answer choices and sets of choices nothing uses any more
used_sets <- unique(c(ids_where("closed_ended", "option_set_id"), ids_where("question_scale", "expected_option_set_id")))
old_sets <- setdiff(ids_where("option_set", "option_set_id", unmarked("name")), used_sets)
test_delete_in("answer_option_set", "option_set_id", old_sets)
test_delete_in("option_set", "option_set_id", old_sets)
used_options <- unique(c(ids_where("response_option", "answer_option_id"), ids_where("answer_option_set", "answer_option_id")))
old_options <- setdiff(ids_where("answer_option", "answer_option_id", unmarked("standardized_label")), used_options)
test_delete_in("answer_option", "answer_option_id", old_options)
test_delete_in("category", "mesh_id", old_categories)
test_delete_in("measurement_scale", "scale_id", old_scales)
log_info(sprintf(
  "  %d sets of choices, %d answer choices, %d MeSH categories, %d measurement scales",
  length(old_sets), length(old_options), length(old_categories), length(old_scales)
))

# ------ Adding the Basic Info ----------------------------------------------------------------------------------
log_info("Adding the registry, investigators, respondent types, studies, questionnaires and waves")
insert <- function(table, row) database_model_insert(connection, table, Filter(function(value) !is.null(value) && !is.na(value), row))
registry_row <- insert("registry", registry)
pi_rows <- lapply(investigators, function(row) insert("principal_investigator", row))
type_rows <- lapply(respondent_types, function(row) insert("respondent_type", row))
study_rows <- lapply(studies, function(study) {
  insert("study", list(
    registry_id = registry_row$registry_id,
    principal_investigator_id = pi_rows[[study$pi]]$principal_investigator_id,
    contact_principal_investigator_id = if (!is.null(study$contact)) pi_rows[[study$contact]]$principal_investigator_id,
    name = study$name, status = study$status, description = study$description,
    methods = study$methods, acknowledgements = study$acknowledgements
  ))
})
for (i in seq_along(questionnaires)) {
  q <- questionnaires[[i]]
  questionnaires[[i]]$row <- insert("questionnaire", list(
    study_id = study_rows[[q$study]]$study_id, respondent_type_id = type_rows[[q$respondent_type]]$respondent_type_id,
    name = q$name, abbreviation = q$abbreviation, description = q$description
  ))
  for (j in seq_along(q$waves)) {
    w <- q$waves[[j]]
    questionnaires[[i]]$waves[[j]]$row <- insert("wave", list(
      questionnaire_id = questionnaires[[i]]$row$questionnaire_id, name = w$name,
      administered_date = w$administered_date, respondent_count = w$respondent_count
    ))
  }
}

# ------ Adding the questionnaires, as extractions ------------------------------------------------------------------
# The sample ADMIN who prepares and adds them: found by email, active while they work.
admin_email <- "dana.whitfield@example.test"
admin <- database_model_get(connection, "staff", c("staff_id", "name", "role"), filters = list(email = database_eq(admin_email)))
if (nrow(admin) == 0) {
  admin <- insert("staff", list(name = "Dana Whitfield", email = admin_email, role = "ADMIN", active = TRUE))
} else {
  database_model_update(connection, "staff", list(active = TRUE, role = "ADMIN"), list(email = database_eq(admin_email)), select = "staff_id")
}
me <- list(staff_id = admin$staff_id[1], name = admin$name[1], role = "ADMIN")

tryCatch(
  for (q in questionnaires) {
    for (w in q$waves) {
      if (is.null(w$blocks)) next
      label <- paste(q$abbreviation, w$name)
      info <- extraction_service_info(list(info = list(
        registry_id = registry_row$registry_id, study_id = study_rows[[q$study]]$study_id,
        questionnaire_id = q$row$questionnaire_id, wave_id = w$row$wave_id
      )))
      record <- extraction_service_create_manual(connection, me, info)
      draft_service_save(connection, record$id, drafts[[label]], me$staff_id)
      record <- extraction_model_get(connection, record$id)
      sent <- extraction_service_push(connection, record, me, questionnaire_service_get_catalog(connection))$record
      history_id <- change_request_service_add(connection, sent, me)
      log_info(sprintf("  %s added (draft %s, history %s)", label, record$id, history_id))
    }
  },
  finally = database_model_update(connection, "staff", list(active = FALSE), list(email = database_eq(admin_email)), select = "staff_id")
)

# ------ MeSH categories and measurement scales ----------------------------------------------------------------------
log_info("Adding MeSH categories and measurement scales")
seeded <- vapply(questionnaires, function(q) format(q$row$questionnaire_id, scientific = FALSE), character(1))
variables <- database_model_get(
  connection, "variable", c("variable_key", "matr_id", "placement_id"),
  order_by = "variable_key", filters = list(questionnaire_id = database_in(seeded))
)
placed <- push_model_by_ids(connection, "questionnaire_component", c("placement_id", "component_id"), "placement_id", variables$placement_id)
variables$component_id <- placed$component_id[match(variables$placement_id, placed$placement_id)]
component_of <- function(matr_ids) {
  found <- variables$component_id[match(matr_ids, variables$matr_id)]
  if (anyNA(found)) stop("No variable has the MATR ID ", paste(matr_ids[is.na(found)], collapse = ", "), call. = FALSE)
  found
}
for (mesh_id in names(categories)) {
  category <- categories[[mesh_id]]
  insert("category", list(mesh_id = mesh_id, term = category$term, scope_note = category$note))
  for (component_id in unique(component_of(category$matr))) {
    insert("question_category", list(component_id = component_id, mesh_id = mesh_id))
  }
}
closed <- database_model_get(connection, "closed_ended", c("variable_key", "option_set_id"), order_by = "variable_key")
for (scale in scales) {
  stored <- insert("measurement_scale", scale[c("name", "abbreviation", "version", "description", "citation")])
  for (i in seq_along(scale$items)) {
    key <- variables$variable_key[match(scale$items[i], variables$matr_id)]
    insert("question_scale", list(
      scale_id = stored$scale_id, component_id = component_of(scale$items[i]),
      expected_option_set_id = closed$option_set_id[match(key, closed$variable_key)], item_number = as.character(i)
    ))
  }
}
log_info("Done.")
