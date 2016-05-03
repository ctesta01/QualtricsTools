#' Determine if a question is a multiple choice and multiple answer question
#'
#' Each of the is-functions defined in the qualtrics package are used
#' for determining which response parsing function should be used. A
#' function is considered multiple choice if it is listed in
#' the qsf file as having $Payload$QuestionType == "MC"
#' (standing for Multiple Choice), and the `$Payload$Selector` is set to one of the
#' following:
#' "Multiple Answer Vertical",
#' "Multiple Answer Horizontal",
#' "Multiple Choice Select Box",
#' "Multiple Answer Column",
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_mc_multiple_answer <- function(question) {

    is_Multiple_Choice = (question$Payload$QuestionType == "MC")
    has_MultipleAnswer_selector = (question$Payload$Selector == "MAVR" ||
                                   question$Payload$Selector == "MAHR" ||
                                   question$Payload$Selector == "MSB" ||
                                   question$Payload$Selector == "MACOL")

    is_Multiple_Answer = (is_Multiple_Choice && has_MultipleAnswer_selector)

    return(is_Multiple_Choice && has_MultipleAnswer_selector)
}


#' Determine if a question is a matrix and multiple answer question
#'
#' A question is considered a matrix multiple answer question if it
#' is a `$Payload$QuestionType == "Matrix"` question with
#' `$Payload$SubSelector == "MultipleAnswer"`
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_matrix_multiple_answer <- function(question) {
  is_Matrix = (question$Payload$QuestionType == "Matrix")
  has_Matrix_MA_selector = (question$Payload$SubSelector == "MultipleAnswer")

  return(is_Matrix && has_Matrix_MA_selector)
}


#' Determine if a question is a single answer question
#'
#' Each of the is-functions defined in the qualtrics package are used
#' for determining which response parsing function should be used. A
#' function is considered multiple choice if it is listed in
#' the qsf file as having $Payload$QuestionType == "MC"
#' (standing for Multiple Choice), and the `$Payload$Selector` is set to one of the
#' following:
#' "Single Answer Vertical",
#' "Single Answer Horizontal",
#' "Single Answer Column",
#' "Dropdown List",
#' "Select Box",
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_mc_single_answer <- function(question) {
    is_Multiple_Choice = (question$Payload$QuestionType == "MC")
    has_SingleAnswer_selector = (question$Payload$Selector == "SAVR" ||
                                 question$Payload$Selector == "SAHR" ||
                                 question$Payload$Selector == "SACOL" ||
                                 question$Payload$Selector == "DL" ||
                                 question$Payload$Selector == "SB")

    return(is_Multiple_Choice && has_SingleAnswer_selector)
}

#' Determine if a question is a matrix and multiple answer question
#'
#' A question is considered a matrix multiple answer question if it
#' is a `$Payload$QuestionType == "Matrix"` question with
#' `$Payload$SubSelector == "SingleAnswer"` or `$Payload$SubSelector == "DL"`
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_matrix_single_answer <- function(question) {
  is_Matrix = question$Payload$QuestionType == "Matrix"

  has_Matrix_SA_selector = (question$Payload$SubSelector == "DL" ||
                              question$Payload$SubSelector == "SingleAnswer")

  return(is_Matrix && has_Matrix_SA_selector)
}


#' Determine if a question is a matrix and multiple answer question
#'
#' A question is considered a matrix multiple answer question if it
#' is a `$Payload$QuestionType == "Matrix"` question with
#' `$Payload$SubSelector == "SingleAnswer"` or `$Payload$SubSelector == "DL"`
#'
#' @param question The question parameter is a single question from a qualtrics survey.
#'
#' @return The return value of this is a boolean, true if it is one of these kinds of
#' questions and false otherwise.
is_matrix_bipolar <- function(question) {
  is_Matrix = question$Payload$QuestionType == "Matrix"

  has_Matrix_SA_selector = question$Payload$Selector == "Bipolar"

  return(is_Matrix && has_Matrix_SA_selector)
}
