# What the reading returns for a form that asks about twin A, then the same about twin B (MQ's sections two and
# three), worded alike as the prompt says, each question tagged with its one twin.
yes_no <- function(...) c(list(variable_type = "closed_ended", response_options = list(list(answer_option = "no"), list(answer_option = "yes"))), list(...))
asked <- function(text, number, ..., variables = list(yes_no())) {
  c(list(component_type = "Question", text = text, printed_number = number, variables = variables), list(...))
}
about_twin <- function(twin, first) {
  n <- function(k) as.character(first + k)
  list(component_type = "Section", text = paste("Section: about twin", twin, "only"), children = list(
    list(component_type = "Instruction", text = paste0("If twin ", twin, " is away at school, answer for the last time they lived at home.")),
    asked("What is the sex of the twins?", n(0), subject = paste("twin", twin), variables = list(
      list(variable_type = "closed_ended", response_options = list(list(answer_option = "male"), list(answer_option = "female")))
    )),
    list(component_type = "Grid", text = "Have the twins ever", children = list(
      asked("repeated a grade?", paste0(n(1), "a"), subject = paste("twin", twin)),
      asked("been to a neurologist?", paste0(n(1), "b"), subject = paste("twin", twin))
    )),
    asked("If yes, why?", NULL, subject = paste("twin", twin), parent_question = paste0(n(1), "b"), variables = list(
      list(variable_type = "open_ended", answer_data_type = "TEXT")
    ))
  ))
}
both_twins <- function(...) {
  list(questionnaire = list(), components = c(
    list(list(component_type = "Section", text = "Section one: about the twins", children = list(
      asked("How similar are the twins?", "1", subject = "twins")
    ))),
    list(about_twin("a", 8), about_twin("b", 24)),
    list(...)
  ))
}
words_of <- function(draft) draft$components$text[match(draft_service_printed_order(draft), draft$components$id)]

test_that("the same questions asked about one twin and then the other are one question each, with a variable for each twin", {
  draft <- draft_service_from_json(both_twins())
  blocks <- draft$components
  # twin B's section is gone, with the instruction it held; twin A's questions are there once
  expect_equal(words_of(draft), c(
    "Section one: about the twins", "How similar are the twins?",
    "Section: about twin a only", "If twin a is away at school, answer for the last time they lived at home.",
    "What is the sex of the twins?", "Have the twins ever", "repeated a grade?", "been to a neurologist?", "If yes, why?"
  ))
  # each has both printed numbers, and a variable for each twin, in that order, with its choices
  sex <- blocks[blocks$text %in% "What is the sex of the twins?", ]
  expect_equal(sex$printed_number, "8, 24")
  expect_equal(sex$subject, NA_character_) # who it is about is each variable's now
  boxes <- draft_service_children(draft, "variables", sex$id)
  expect_equal(boxes$subject, c("twin a", "twin b"))
  expect_equal(boxes$display_order, c(1, 2))
  expect_equal(draft_service_children(draft, "response_options", boxes$id[2])$answer_option, c("male", "female"))
  grade <- blocks[blocks$text %in% "repeated a grade?", ]
  expect_equal(grade$printed_number, "9a, 25a")
  expect_equal(draft_service_children(draft, "variables", grade$id)$subject, c("twin a", "twin b"))
  # the follow-up is one question too, hanging off the one question it follows
  why <- blocks[blocks$text %in% "If yes, why?", ]
  expect_equal(nrow(why), 1)
  expect_equal(why$parent_question_id, blocks$id[blocks$text %in% "been to a neurologist?"])
  expect_equal(draft_service_children(draft, "variables", why$id)$subject, c("twin a", "twin b"))
  # the questions are numbered by their place, and the draft has nothing to fix but its Variable IDs
  expect_equal(blocks$question_number[blocks$component_type == "Question"], as.character(1:5))
  expect_equal(nrow(draft_service_check(draft, test_lookups(), matr_ids = FALSE)), 0)
  # joining again changes nothing
  expect_identical(draft_service_join_repeats(draft), draft)
})

test_that("only questions worded alike and about different people are joined, and a reviewed copy is left as staff saved it", {
  # the same words about the same person, or about nobody in particular, are two questions
  same_person <- draft_service_from_json(list(questionnaire = list(), components = list(
    asked("How old is your child?", "1", subject = "children"), asked("How old is your child?", "2", subject = "children"),
    asked("Do you smoke?", "3"), asked("Do you smoke?", "4")
  )))
  expect_equal(sum(same_person$components$component_type == "Question"), 4)
  # other words, or the same words under another grid's lead-in, are other questions
  other_words <- draft_service_from_json(list(questionnaire = list(), components = list(
    asked("What is the sex of the twins?", "8", subject = "twin a"), asked("What is the age of the twins?", "24", subject = "twin b"),
    list(component_type = "Grid", text = "Have the twins ever", children = list(asked("smoked?", "9a", subject = "twin a"))),
    list(component_type = "Grid", text = "Have the twins often", children = list(asked("smoked?", "25a", subject = "twin b")))
  )))
  expect_equal(sum(other_words$components$component_type == "Question"), 4)
  # capitals, spaces and the last full stop don't make other words; a time the two share stays the question's
  alike <- draft_service_from_json(list(questionnaire = list(), components = list(
    asked("My child smiles often.", "20a", subject = "twin a", time_frame = "past year"),
    asked("my child  smiles often", "36a", subject = "twin b", time_frame = "past year")
  )))
  expect_equal(alike$components$printed_number, "20a, 36a")
  expect_equal(alike$components$time_frame, "past year")
  expect_equal(alike$variables$subject, c("twin a", "twin b"))
  expect_equal(alike$variables$time_frame, c(NA_character_, NA_character_))
  # a section that keeps a question of its own stays, with it
  kept <- draft_service_from_json(both_twins())
  extra <- both_twins()
  extra$components[[3]]$children <- c(extra$components[[3]]$children, list(asked("Is twin B left-handed?", "40")))
  extra <- draft_service_from_json(extra)
  expect_equal(setdiff(words_of(extra), words_of(kept)), c("Section: about twin b only", "If twin b is away at school, answer for the last time they lived at home.", "Is twin B left-handed?"))
  # the copy staff saved opens as they saved it
  saved <- draft_service_to_json(same_person)
  saved$components[[2]]$subject <- "siblings"
  expect_equal(sum(draft_service_from_json(saved)$components$component_type == "Question"), 4)
})

test_that("a follow-up is told by the question it follows, so it is joined wherever each reading put it", {
  follow_up <- function(twin, parent) asked("If yes, why?", NULL, subject = paste("twin", twin), parent_question = parent, variables = list(
    list(variable_type = "open_ended", answer_data_type = "TEXT")
  ))
  # twin A's follow-up was read inside the grid, twin B's after it, in the section
  draft <- draft_service_from_json(list(questionnaire = list(), components = list(
    list(component_type = "Section", text = "About the twins", children = list(
      list(component_type = "Grid", text = "Have the twins ever", children = list(asked("been to a neurologist?", "13j", subject = "twin a"), follow_up("a", "13j")))
    )),
    list(component_type = "Section", text = "About the twins", children = list(
      list(component_type = "Grid", text = "Have the twins ever", children = list(asked("been to a neurologist?", "29j", subject = "twin b"))),
      follow_up("b", "29j")
    ))
  )))
  expect_equal(words_of(draft), c("About the twins", "Have the twins ever", "been to a neurologist?", "If yes, why?"))
  why <- draft$components[draft$components$text %in% "If yes, why?", ]
  expect_equal(draft_service_children(draft, "variables", why$id)$subject, c("twin a", "twin b"))
  expect_identical(draft_service_join_repeats(draft), draft)
  # the same follow-up after two different questions is two questions
  apart <- draft_service_from_json(list(questionnaire = list(), components = list(
    asked("Have the twins been to a neurologist?", "1", subject = "twin a"), follow_up("a", "1"),
    asked("Have the twins been to a dentist?", "2", subject = "twin b"), follow_up("b", "2")
  )))
  expect_equal(sum(apart$components$component_type == "Question"), 4)
})

test_that("a question printed with several numbers is found by any of them", {
  expect_equal(same_number(c("8, 24", "9", NA), "24"), c(TRUE, FALSE, FALSE))
  expect_equal(same_number(c("8, 24", "9a,25A"), "25a"), c(FALSE, TRUE))
  expect_equal(same_number("8, 24", NA), FALSE)
})

test_that("two copies join though Gemini inflected or punctuated one differently, and different questions never do", {
  twin <- function(who, ...) list(component_type = "Section", text = paste("About twin", who), children = list(...))
  one <- function(text, number, who) asked(text, number, variables = list(yes_no(subject = paste("twin", who))))
  grid <- function(lead_in, ...) list(component_type = "Grid", text = lead_in, children = list(...))
  draft <- draft_service_from_json(list(questionnaire = list(), components = list(
    twin(
      "a",
      one("Plays or works well by themselves.", "12c", "a"),
      grid(
        "How frequently do the twins",
        one("be touchy, easily annoyed, or act like many things get on their nerves?", "21c", "a"),
        one("interrupt or \"butt in\" on the activities of others?", "21h", "a")
      ),
      grid("During the first five years of the twins' life, how frequently were you worried about", one("their physical development?", "19a", "a")),
      one("Do the twins like school?", "22", "a")
    ),
    twin(
      "b",
      one("Plays or works well by themself.", "28c", "b"),
      grid(
        "How frequently do the twins",
        one("is touchy, easily annoyed or acts like many things get on their nerves?", "37c", "b"),
        one("interrupt or 'butt in' on the activities of others?", "37h", "b")
      ),
      grid("During the first five years of the twins' lives, how frequently were you worried about", one("their physical development?", "35a", "b")),
      one("Do the twins like their school?", "38", "b") # another question: not joined
    )
  )))
  questions <- draft$components[draft$components$component_type == "Question", ]
  expect_equal(questions$printed_number, c("12c, 28c", "21c, 37c", "21h, 37h", "19a, 35a", "22", "38"))
  expect_equal(questions$text[1:2], c("Plays or works well by themselves.", "be touchy, easily annoyed, or act like many things get on their nerves?")) # the first copy's words
  expect_equal(draft_service_children(draft, "variables", questions$id[2])$subject, c("twin a", "twin b"))
})
