#' Get the Choice Text based on the Choice from a Question
#'
#' Input a question and a choice, and this function will
#' do its best to give you back the choice text.
choice_text_from_question <- function(question, choice) {
  is_99 <- choice == "-99"

  # if the question is a multiple answer question,
  # meaning some form of "check all that apply",
  # then the answers are boolean valued -- either they
  # checked it or they didn't. Return TRUE, FALSE, or
  # "Seen, but Unanswered" depending.
  if (is_multiple_choice(question)) {
    if (choice %in% c(1, "1")) {
      choice <- "TRUE"
    } else {
      choice <- "FALSE"
    }

    # if the question is a single answer multiple choice
    # question, then it either has recode values, or
    # the choice given is directly correspondent with
    # the index of the choice in the $Payload$Choices
    # list. if the choice given doesn't match any
    # of the recode values, try getting it directly from
    # the choices.
  } else if (is_mc_single_answer(question)) {
    if ("RecodeValues" %in% names(question$Payload)) {
      recoded_value <- which(question$Payload$RecodeValues == x)
      if (length(recoded_value) != 0) choice <- recoded_value
      choice <- question$Payload$Choices[[choice]][[1]]
    } else {
      choice <- question$Payload$Choices[[choice]][[1]]
    }


    # if the question is a single answer matrix question,
    # the question will either have recode values, or not.
    # if the question has recode values, attempt to use the
    # $Payload$RecodeValues list to retrieve the recoded_value.
    # If that doesn't work, just use the original choice given.
  } else if (is_matrix_single_answer(question)) {
    if ("RecodeValues" %in% names(question$Payload)) {
      recoded_value <- which(question$Payload$RecodeValues == choice)
      if (length(recoded_value) != 0) {
        choice <- names(question$Payload$RecodeValues[recoded_value])[[1]]
      }
      choice <- question$Payload$Choices[[choice]][[1]]
    } else {
      choice <- question$Payload$Choices[[choice]][[1]]
    }
  }

  if (is_99) choice <- "Seen, but Unanswered"
  choice <- clean_html(choice)
  return(choice)
}



#' A Shiny app to format Qualtrics survey data and generate reports
#'
#' This function launches the Shiny interface for the Qualtrics
#' package from the files in the install or 'inst' directory.
#'
#' @usage qualtrics::app()
app <- function() {
  shiny::runApp(system.file('shiny', package = 'qualtrics'))
}

#' Setup the Global Environment for a Survey
#'
#' This function sets up the .GlobalEnv to include survey, responses, questions
#' (without trash questions, with clean question text,
#' with a human readable question type, with responses, and with results if
#' automatically generateable), and blocks (without trash blocks, with questions
#' inserted in place of the BlockElements representing them).
#'
#' @param headerrows An optional parameter for specifying the number of
#' headerrows in the response csv.
get_setup <- function(headerrows) {
  if (missing(headerrows)) {
    headerrows <- 3
  }
  try(survey <<- ask_for_qsf())
  try(responses <<- ask_for_csv(headerrows = headerrows))
  try(blocks <<- blocks_from_survey(survey))
  try(questions <<- questions_from_survey(survey))
  try(questions <<- remove_trash_questions(questions, blocks))
  try(questions <<- clean_question_text(questions))
  try(questions <<- human_readable_qtype(questions))
  try(blocks <<- remove_trash_blocks(blocks))
  try(questions <<- link_responses_to_questions(questions, responses))
  try(questions <<- generate_results(questions))
  try(blocks <<- questions_into_blocks(questions, blocks))

  if ( exists("survey", 1) &&
       exists("responses", 1) &&
       exists("questions", 1) &&
       exists("blocks", 1)
  ) {
    cat("survey, responses, questions, and blocks have all been made
        globally available in your R session.")
  }
}


#' Find Question from DataExportTag
#'
#' This function takes a list of questions and an export tag and
#' looks for the matching question. It will try to select
#' the question uniquely.
find_question <- function(questions, exporttag) {
  matched_question_index <- which(sapply(questions, function(x) x$Payload$DataExportTag == exporttag))
  return(questions[[matched_question_index]])
}


#' Find Question Index from DataExportTag
#'
#' This function takes a list of questions and an export tag and
#' looks for the matching question. It returns the index(es) of
#' the questions with that Question Data Export Tag.
find_question_index <- function(questions, exporttag) {
  matched_question_index <- which(sapply(questions, function(x) x$Payload$DataExportTag == exporttag))
  return(matched_question_index)
}
