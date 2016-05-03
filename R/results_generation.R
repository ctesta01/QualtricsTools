#' Format Percents
#'
#' percent0 is a function for formatting a decimal number into a string representing a percentage with
#' a specific number of decimal places and with a "%" appended.
#' The function uses formatC from the R base package and
#' defaults to formatting the number with 1 digit and with the "f" format
#' in the formatC argument.
percent0 <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


#' Create the Results Table for a Multiple Choice Single Answer Question
#'
#' The mc_single_answer_results function uses the definition of the choices in the QSF file
#' and their potentially recoded values to determine how to table the results paired to that question.
#'
#' @param question The question must have a paired response column placed into the question
#' under `$Responses`
#' @return a table with an N, Percent, and choice column, detailing the number of responses for each
#' choice.
mc_single_answer_results <- function(question) {
    # the factors are the variable codes that users could choose between
    # if a question has been recoded, the variable names are in $Payload$RecodeValues
    # and if not, they are in $Payload$Choices
    if ("RecodeValues" %in% names(question$Payload)) {
        factors <- unlist(question$Payload$RecodeValues)
    } else {
        factors <- names(question$Payload$Choices)
    }

    # the responses to single answer questions are only one column, so they are always the first column.
    # take the responses and sort them by the factors to get responses_tabled.
    # the second column of the responses_tabled are the numbers of responses for each factor, our Ns.
    # total number of responses to a question is counted by all the answers that aren't -99
    # use Ns and respondent_count to calculate the percents for each factor.
    responses <- question$Responses[[1]]
    responses_tabled <- as.data.frame(table(factor(responses, levels=factors)))
    N <- responses_tabled[,2]
    respondent_count <- length(question$Responses[[1]] != -99)
    Percent <- percent0(N / respondent_count)

    # if the choice variables have been recoded, first the factors are retrieved from the responses_tabled,
    # then they are turned into the list of corresponding indexes in the RecodeValues list,
    # which are then used to recover the original choice text from the Choices list,
    # and then the choices are flattened to a cleaner list.
    # if the choice variables are not recoded, then they can be retrieved directly from the responses_table
    if ("RecodeValues" %in% names(question$Payload)) {
        choices_recoded <- responses_tabled[,1]
        choices_uncoded <- sapply(choices_recoded, function(x) which(question$Payload$RecodeValues == x))
        choices <- sapply(choices_uncoded, function(x) question$Payload$Choices[[x]])
        choices <- unlist(choices, use.names = FALSE)
    } else {
        choices <- responses_tabled[,1]
    }

    # construct the results table with a column for N, Percent, and choices,
    # but make sure that the choices column doesn't have a header when it prints.
    results_table <- data.frame(N, Percent, choices)
    colnames(results_table)[3] <- ""
    results_table
}


#' Create the Results Table for a Multiple Choice Single Answer Question
#'
#' The mc_multiple_answer_results function uses the definition of the choices in the QSF file
#' and their potentially recoded values to determine how to table the results paired to that question.
#'
#' @inheritParams mc_single_answer_results
#' @return a table with an N, Percent, and choice column, detailing the number of responses for each
#' choice.
mc_multiple_answer_results <- function(question) {
  # take each response column and sum all the 1s in it together to get the
  # number of responses for a given choice.
  # since if a respondent did not respond to a question, their response
  # appears as -99 in all columns of the responses, we can use any column (in this case
  # we will use the first) to determine the total number of valid responses.
  # to get the respondent_count, we determine how many responses were not -99 in the
  # first response column.
  # the response column names are simply the data export tag appended with an underscore
  # and then the choice number as it is recorded in $Payload$Choices.
  # from the choice numbers in the column, we construct a list of the corresponding choice
  # texts, and then flatten it.
  # lastly, flatten the Ns list and calculate the Percents.
  N <- sapply(question$Responses, function(x) sum(x == 1))
  respondents_count <- length(question$Responses[[1]] != -99)
  data_export_tag <- question$Payload$DataExportTag
  names(N) <- gsub(paste0(data_export_tag, "_"), "", names(question$Responses))
  choices <- sapply(names(N), function(x) question$Payload$Choices[[x]])
  choices <- unlist(choices, use.names = FALSE)
  N <- unlist(N, use.names = FALSE)
  Percent <- percent0(N / respondents_count)

  # construct the results table with a column for N, Percent, and choices,
  # but make sure that the choices column doesn't have a header when it prints.
  results_table <- data.frame(N, Percent, choices)
  colnames(results_table)[3] <- ""
  results_table
}


#' Create the Results Table for a Multiple Choice Single Answer Question
#'
#' The matrix_single_answer_results function uses the definition of the choices and answers in the
#' QSF file and their potentially recoded values to determine how to table the results paired
#' to that question. If you look at the source code, keep in mind that a matrix question's sub-questions
#' are called "Choices" and that the choices for each sub-question are called "Answers"
#'
#' @inheritParams mc_single_answer_results
#' @return a table with the matrix-sub-questions listed in the first column, the percentages for each
#' choice for each sub-question listed in a table, and then another column with the total respondents
#' for each subquestion.
matrix_single_answer_results <- function(question) {
  # the factors are the variable codes that users could choose between
  # if a question has been recoded, the variable names are in $Payload$RecodeValues
  # and if not, they are in $Payload$Choices
  if ("RecodeValues" %in% names(question$Payload)) {
    factors <- unlist(question$Payload$RecodeValues)
  } else {
    factors <- unlist(question$Payload$AnswerOrder)
  }

  # create the responses table, a table detailing the
  # number of times each choice was chosen for each sub-question.
  # N is a list with as many elements as there are questions, with the number
  # of respondents for each matrix sub-question in each entry.
  # the responses table is iterated over and turned into percents according to the
  # original values in the responses table and the respondents counts in the N variable.
  responses <- sapply(question$Responses, function(x) table(factor(x, factors)))
  N <- sapply(question$Responses, function(x) strtoi(length(which(x != -99))))
  for (i in 1:nrow(responses)) {
    for (j in 1:ncol(responses)) {
      responses[i,j] <- percent0(strtoi(responses[i,j]) /
                              N[1])
    }
  }

  # flip the responses such that the sub-questions are the rows, and the choices
  # appear on top as columns.
  # if the choices have been recoded, go through the $Payload$RecodeValues to get the original
  # choice texts.
  # if the choices haven't been recoded, use the $Payload$Answers to retrieve the original
  # choice texts.
  # replace the column names with the choice text.
  # the sub-question tags are formed as the $Payload$DataExportTag with an underscore, then an integer
  # starting from 1. Strip the DataExportTag and underscore from the sub-question tag, and then
  # use the $Payload$Choices list to retrieve the original sub-question text.
  responses <- t(responses)
  if ("RecodeValues" %in% names(question$Payload)) {
    answers_uncoded <- sapply(colnames(responses), function(x) which(question$Payload$RecodeValue == x))
    answers <- sapply(answers_uncoded, function(x) question$Payload$Answers[[x]])
    answers <- unlist(answers, use.names = FALSE)
  } else {
    answers <- sapply(colnames(responses), function(x) question$Payload$Answers[[x]])
  }
  colnames(responses) <- answers
  export_tag_with_underscore <- paste0(question$Payload$DataExportTag, "_")
  choices <- sapply(rownames(responses), function(x) question$Payload$Choices[[gsub(export_tag_with_underscore, "", x)]])
  choices <- unlist(choices, use.names = FALSE)

  # form a data frame with the first column listing the sub-question text, then
  # include the table of percents for each answer choice for each sub-question,
  # then include as the last column the number of respondents to each sub-question.
  responses <- data.frame(choices, responses, N, check.names=F)
  colnames(responses)[1] <- ""
  rownames(responses) <- NULL
  return(responses)
}

