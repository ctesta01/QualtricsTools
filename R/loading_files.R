#' Ask the user for the Qualtrics Survey file
#'
#' @return The survey file the user uploads, as a list
ask_user_for_qsf <- function(surveyfile) {
  if (missing(surveyfile)) {
    print("Select Qualtrics Survey File:")
    surveyfile = file.choose()
  }
  survey = fromJSON(file=surveyfile)

  return(survey)
}

#' Ask the user for the Qualtrics Response Set
#'
#' This function automatically turns the entries in the
#' first row into a "text" attribute of the column. Additionally,
#' it removes the first row after turning its entries into attributes.
#'
#' @return The csv file the user uploads, as a data frame
ask_user_for_csv <- function(responsesfile) {
  if (missing(responsesfile)) {
    print("Select CSV Response File:")
    responsesfile = file.choose()
  }
  responses = read.csv(responsesfile)


  for (i in 1:length(colnames(responses))) {
    column <- colnames(responses)[i]
    attr(responses[[column]], "text") <- responses[1, i]
  }

  responses <- responses[-1, ]
  return(responses)
}

#' Generate a List of Survey Blocks
#'
#' This function takes the survey blocks out of the survey
#' and turns them into their own list for later use.
#' Additionally, the block element can be further reduced to include
#' only the payload of the survey blocks.
#'
#' @param survey This should be a Qualtrics survey in the form of a list
#' imported from the JSON-formatted QSF file.
#'
#' @return The blocks element returned is a list of blocks containing for
#' each a type, description, ID, and BlockElements (which contains the
#' list of questions included in a given block).
blocks_from_survey <- function(survey) {
  blocks <- Filter(function(x) x$Element == "BL", survey$SurveyElements)
  blocks <- blocks[[1]]$Payload
  return(blocks)
}

#' Generate a List of Questions
#'
#' This function takes the questions out of the survey and
#' turns them into their own list for later use.
#' Each question is an element of the returned list, and
#' each element has its own list as its content. Most questions
#' include things like "SurveyID", "Element", "PrimaryAttribute",
#' "SecondaryAttribute", and a "Payload" which contains
#' most of the information about the question.
#'
#' @inheritParam blocks_from_survey
#' @return A list of questions from the uploaded QSF file
questions_from_survey <- function(survey) {
  questions <- survey$SurveyElements
  for (i in length(questions):1) {
    if (questions[[i]]$Element != "SQ") {
      questions[[i]] <- NULL
    }
  }
  return(questions)
}

#' Remove Questions from the Trash Block
#'
#' This function removes any questions from a list of questions
#' that are listed in the Trash block.
#'
#' @param questions A list of questions from a Qualtrics survey
#' @param blocks A list of blocks from a Qualtrics survey
#' @return The list of questions returned is the list of questions
#' provided except without any questions listed in the Trash
#' block of the blocks list provided.
remove_trash_questions <- function(questions, blocks) {
  trash <- Filter(function(x) x$Type == "Trash", blocks)
  trash_questions <- list()
  for (i in trash[[1]]$BlockElements) {
    trash_questions <- c(i$QuestionID, trash_questions)
  }

  delete_if_in_trash <- function(x) {
    if (x$Payload$QuestionID %in% trash_questions) {
      return(NULL)
    } else {
      return(x)
    }
  }
  questions <- lapply(questions, delete_if_in_trash)
  questions <- Filter(Negate(function(x) is.null(unlist(x))), questions)
  return(questions)
}

#' Remove the Trash Block from the list of Blocks
#'
#' @inheritParams remove_trash_questions
#' @return The list of blocks is returned without any Trash blocks
remove_trash_blocks <- function(blocks) {
  blocks[which(sapply(blocks, function(x) x$Type == "Trash"))] = NULL
  return(blocks)
}
