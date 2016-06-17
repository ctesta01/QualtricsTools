#' Format Percents
#'
#' percent0 is a function for formatting a decimal number into a string representing a percentage with
#' a specific number of decimal places and with a "%" appended.
#' The function uses formatC from the R base package and
#' defaults to formatting the number with 1 digit and with the "f" format
#' in the formatC argument.
#'
#' This function is mostly a function for aiding in formatting results tables.
percent0 <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


#' Create the Results Table for a Multiple Choice Single Answer Question
#'
#' The mc_single_answer_results function uses the definition of the choices in the QSF file
#' and their potentially recoded values to determine how to table the results paired to that question.
#'
#' @param question The question must have a paired response column placed into the question
#' under [['Responses']]
#' @return a table with an N, Percent, and choice column, detailing the number of responses for each
#' choice.
mc_single_answer_results <- function(question) {
    # If a question's response options were given recoded values
    # during the survey's creation, the variables for the recoded values
    # are used as the factors a response could match. If there was no
    # recoding of values, then the choices are directly use as the factors
    # a response can match.
    if ("RecodeValues" %in% names(question[['Payload']])) {
        factors <- unlist(question[['Payload']][['RecodeValues']])
    } else {
        factors <- names(question[['Payload']][['Choices']])
    }

    # the responses to single answer questions are only one column.
    # first, take the responses and sort them by the factors to get responses_tabled.
    # the second column of the responses_tabled are the numbers of responses for each factor, our Ns.
    # total number of responses to a question is counted by all the answers that aren't -99
    # use Ns and respondent_count to calculate the percents for each factor.
    responses <- question[['Responses']][[question[['Payload']][['DataExportTag']]]]
    responses_tabled <- as.data.frame(table(factor(responses, levels=factors)))
    N <- responses_tabled[,2]
    exporttag <- question[['Payload']][['DataExportTag']]
    respondent_count <- length(which((question[['Responses']][[exporttag]] != -99) & (question[['Responses']][[exporttag]] != "")))
    Percent <- percent0(N / respondent_count)

    # if the choice variables have been recoded, first the factors are retrieved from the responses_tabled,
    # then they are turned into the list of corresponding indexes in the RecodeValues list,
    # which are then used to recover the original choice text from the Choices list,
    # and then the choices are flattened to a cleaner list.
    # if the choice variables are not recoded, then they can be retrieved directly from the responses_table
    if ("RecodeValues" %in% names(question[['Payload']])) {
        choices_recoded <- responses_tabled[,1]
        choices_uncoded <- sapply(choices_recoded, function(x) which(question[['Payload']][['RecodeValues']] == x))
        choices <- sapply(choices_uncoded, function(x) question[['Payload']][['Choices']][[x]][[1]])
    } else {
        choices_uncoded <- responses_tabled[,1]
        choices <- sapply(choices_uncoded, function(x) question[['Payload']][['Choices']][[x]][[1]])
    }
    choices <- unlist(choices, use.names = FALSE)
    choices <- sapply(choices, clean_html)


    # construct the results table with a column for N, Percent, and choices,
    # but make sure that the choices column doesn't have a header when it prints.
    results_table <- data.frame(N, Percent, choices, row.names = NULL)
    colnames(results_table)[3] <- ""
    results_table
}


#' Create the Results Table for a Multiple Choice Single Answer Question
#'
#' The mc_multiple_answer_results function uses the definition of the choices in the QSF file
#' and their (if present) recoded values to determine how to table the results paired to that question.
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
  # and then the choice number as it is recorded in [['Payload']][['Choices']].
  # from the choice numbers in the column, we construct a list of the corresponding choice
  # texts, and then flatten it.
  # lastly, flatten the Ns list and calculate the Percents.
  orig_responses <- question[['Responses']]
  not_text_columns <- which(sapply(colnames(orig_responses), function(x) !(grepl("TEXT", x))))
  orig_responses <- orig_responses[, not_text_columns]

  N <- sapply(orig_responses, function(x) sum(x == 1))
  respondents_count <- length(which(apply(
    orig_responses, 1, function(row) !(all(row == -99) | all(row == "")))))
  data_export_tag <- question[['Payload']][['DataExportTag']]
  names(N) <- gsub(paste0(data_export_tag, "_"), "", names(orig_responses))

  # Qualtrics Insights' platform uses the recoded values for the column names
  # in the response data. However, Qualtrics only used the choice values
  # before their insights platform. The condition used for recoding the columns
  # is that "RecodeValues" appears in the question's payload, and that
  # all the column names (after having their data export tag removed) are
  # contained in the RecodeValues list.
  if ("RecodeValues" %in% names(question[['Payload']]) && all(names(N) %in% question[['Payload']][['RecodeValues']])) {
    names(N) <- sapply(names(N), function(x) which(question[['Payload']][['RecodeValues']] == x))
  }

  # After recoding the choices, grab their choice text.
  # Clean the choice text of HTML, and format the data as percents.
  choices <- sapply(names(N), function(x) question[['Payload']][['Choices']][[x]][[1]])
  choices <- unlist(choices, use.names = FALSE)
  choices <- sapply(choices, clean_html)
  N <- unlist(N, use.names = FALSE)
  Percent <- percent0(N / respondents_count)

  # construct the results table with a column for N, Percent, and choices,
  # but make sure that the choices column doesn't have a header when it prints.
  results_table <- data.frame(N, Percent, choices, row.names = NULL)
  colnames(results_table)[3] <- ""
  results_table
}


#' Create the Results Table for a Matrix Single Answer Question
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
  # if a question has been recoded, the variable names are in [['Payload']][['RecodeValues']]
  # and if not, they are in [['Payload']][['Choices']]
  if ("RecodeValues" %in% names(question[['Payload']])) {
    factors <- unlist(question[['Payload']][['RecodeValues']])
  } else {
    factors <- unlist(question[['Payload']][['AnswerOrder']])
  }

  # create the responses table, a table detailing the
  # number of times each answer was chosen for each sub-question (choice).
  # N is a list with as many elements as there are questions, with the number
  # of respondents for each matrix sub-question in each entry.
  # the responses table is iterated over and turned into percents according to the
  # original values in the responses table and the respondents counts in the N variable.
  orig_responses <- question[['Responses']]
  not_text_columns <- which(sapply(colnames(orig_responses), function(x) !(grepl("TEXT", x))))
  orig_responses <- orig_responses[, not_text_columns]
  responses <- sapply(orig_responses, function(x) table(factor(x, factors)))
  N <- sapply(orig_responses, function(x) strtoi(length(which(x != -99 & x != ""))))
  for (i in 1:nrow(responses)) {
    for (j in 1:ncol(responses)) {
      responses[i,j] <- percent0(strtoi(responses[i,j]) /
                              N[j])
    }
  }

  # flip the responses such that the sub-questions are the rows, and the choices
  # appear on top as columns.
  # if the choices have been recoded, go through the [['Payload']][['RecodeValues']] and [['Payload']][['ChoiceDataExportTags']]
  # to get the original
  # choice texts.
  # if the choices haven't been recoded, use the [['Payload']][['Answers']] and [['Payload']][['Choices']] to retrieve the original
  # choice texts.
  # replace the column names with the choice text.
  responses <- t(responses)
  if ("RecodeValues" %in% names(question[['Payload']])) {
    answers_uncoded <- sapply(colnames(responses), function(x) names(question[['Payload']][['RecodeValues']][which(question[['Payload']][['RecodeValues']] == x)])[[1]])
    answers <- sapply(answers_uncoded, function(x) question[['Payload']][['Answers']][[x]][[1]])
    answers <- unlist(answers, use.names = FALSE)
  } else {
    answers <- sapply(colnames(responses), function(x) question[['Payload']][['Answers']][[x]][[1]])
  }
  answers <- sapply(answers, clean_html)
  colnames(responses) <- answers

  # in Qualtrics Insights, Qualtrics has improved the naming of the response columns.
  # prior to insights, they did not include the data export tag in the response header.
  # After insights, the data export tag has been included in the response columns.
  # The choice export tags also have "-" replaced with "_" whenever they're used as response
  # headers. We check if the response columns are exactly the choice data export tags,
  # or if they are the choice export tags prepended with the data export tags.
  choice_export_tags_with_underscores <- sapply(question[['Payload']][['ChoiceDataExportTags']], function(x) gsub("-", "_", x))
  response_names_without_export_tag <- gsub(paste0(question[['Payload']][['DataExportTag']], "_"), "", names(orig_responses))

  # if the response columns are the choice data export tags, then
  # we use the choice data export tags numbering to retrieve the
  # corresponding choice text.
  if (all(names(orig_responses) %in% choice_export_tags_with_underscores)) {
    choices_uncoded <- sapply(rownames(responses), function(x) which(choice_export_tags_with_underscores == x))
    choices <- sapply(choices_uncoded, function(x) question[['Payload']][['Choices']][[x]][[1]])

    # if the response columns are prepended with the data export tag, we
    # use the response column names without the data export tag to retrieve
    # the choice text by going through the choice data export tags indexes.
  } else if (all(response_names_without_export_tag %in% question[['Payload']][['ChoiceDataExportTags']])){
    choices <- sapply(response_names_without_export_tag, function(x) which(question[['Payload']][['ChoiceDataExportTags']] == x))
    choices <- sapply(choices, function(x) question[['Payload']][['Choices']][[x]][[1]])

    # if neither of the above are true, we attempt a last ditch
    # effort and strip the data export tag if it's present from the
    # response column name, and try to retrieve it directly from the
    # list of choices using the rest of the response column name.
  } else {
    export_tag_with_underscore <- paste0(question[['Payload']][['DataExportTag']], "_")
    choices <- sapply(rownames(responses), function(x)
      question[['Payload']][['Choices']][[gsub(export_tag_with_underscore, "", x)]][[1]])
  }

  # The choices can contain information like DisplayLogic and
  # Text Entry components, so sometimes unlisting choices will
  # give us more elements than we need. If the choices are longer
  # than we need, we
  # try to lower the dimension,
  # and we try to select only the display parts of the
  # choices.
  # Then we run the HTML cleaner on the choices
  # so that the choice text appears nicely.
  if (length(choices) == length(N)) {
    choices <- unlist(choices, use.names = FALSE)
  } else {
    if (length(dim(choices)) == 2) {
      choices <- choices[1,]
    }
    choices <- lapply(choices, function(x) x[['Display']])
  }
  choices <- sapply(choices, clean_html)

  # form a data frame with the first column listing the sub-question text, then
  # include the table of percents for each answer choice for each sub-question,
  # then include as the last column the number of respondents to each sub-question.
  responses <- data.frame(choices, N, responses, check.names=FALSE, row.names = NULL)
  colnames(responses)[1] <- ""
  rownames(responses) <- NULL
  return(responses)
}


#' Create the Results Table for a Matrix Multiple Answer Question
#'
#' @inheritParams mc_single_answer_results
#' @return a table with the matrix-sub-questions listed in the first column,
#' a column with the total number of respondents for each subquestion, and
#' the percentages for each choice for each sub-question.
matrix_multiple_answer_results <- function(question) {
  # this function takes a long vector with the rownames of the response
  # columns, and turns it into a matrix of the results.
  # the response column names usually look something like "Q5_1_2",
  # ie, Question 5, Choice 1, Answer 2.
  vector_to_dataframe <- function(vector) {
    df <- data.frame(names = names(vector), value = as.numeric(vector))
    `%>%` <- magrittr::`%>%`

    # the regular expression used in this line is a
    # positive lookbehind for a "_", followed by
    # a positive lookahead for a string of
    # one or more integers, followed by the end of the string.
    # this separate command splits the column header into the
    # first part, and the last part, where the last part is always
    # the answer variable.
    df <- tidyr::separate(df, names, into=c("Choices", "Answers"),
                          sep = "(?<=_) ?(?=([0-9]+$))") %>% tidyr::spread(Answers, value)

    # when the separated results are spread, they list the
    # first part of the separation in the first column.
    # we fix that by replacing the rownames with the first column,
    # and deleting the first column.
    rownames(df) <- gsub("_$", "", df[,1])
    df <- df[,-1]
    return(df)
  }

  # select only the non-text responses, and save the question's export_tag
  orig_responses <- question[['Responses']]
  not_text_columns <- which(sapply(colnames(orig_responses), function(x) !(grepl("TEXT", x))))
  orig_responses <- orig_responses[, not_text_columns]
  export_tag <- question[['Payload']][['DataExportTag']]

  # Qualtrics Insights prepends every response column with the question's export_tag,
  # Qualtrics before Insights did not always do so.
  if (all(gdata::startsWith(names(orig_responses), export_tag))) {
    names(orig_responses) <- gsub(paste0(export_tag, "_"), "", names(orig_responses))
  }

  # create the percentage table of results, choices spanning the rows,
  # answers spanning the columns.
  ma_matrix_nums <- sapply(orig_responses, function (y) sum(y == 1))
  respondents_count <- length(which(apply(orig_responses, 1, function(x) any(x == 1))))
  ma_matrix_nums <- vector_to_dataframe(ma_matrix_nums)
  for (i in 1:nrow(ma_matrix_nums)) {
    for (j in 1:ncol(ma_matrix_nums)) {
      ma_matrix_nums[i, j] <- percent0(strtoi(ma_matrix_nums[i, j]) / (respondents_count))
    }
  }

  # if "RecodeValues" or "ChoiceDataExportTags" appear
  # in the QSF entry for a question, use those lists
  # to find and replace the given coded variable with the
  # variable internal to the QSF -- the actual index of the
  # answer or choice in the [['Payload']][['Answers']] and [['Payload']][['Choices']]
  # lists
  if ("RecodeValues" %in% names(question[['Payload']]) && colnames(ma_matrix_nums) %in% question[['Payload']][['RecodeValues']]) {
    Answers <- sapply(colnames(ma_matrix_nums), function(x) names(question[['Payload']][['RecodeValues']])[which(question[['Payload']][['RecodeValues']] == x)] )
  } else {
    Answers <- colnames(ma_matrix_nums)
  }
  if ("ChoiceDataExportTags" %in% names(question[['Payload']]) && rownames(ma_matrix_nums) %in% question[['Payload']][['ChoiceDataExportTags']]) {
    Choices <- sapply(rownames(ma_matrix_nums), function(x) names(question[['Payload']][['ChoiceDataExportTags']])[which(question[['Payload']][['ChoiceDataExportTags']] == x)] )
  } else {
    Choices <- rownames(ma_matrix_nums)
  }

  # get the choices and answers from their index,
  # and clean them of any html strings
  Answers <- sapply(Answers, function(x) question[['Payload']][['Answers']][[x]][[1]])
  Choices <- sapply(Choices, function(x) question[['Payload']][['Choices']][[x]][[1]])
  colnames(ma_matrix_nums) <- clean_html(Answers)
  Choices <- clean_html(Choices)

  # create the data frame
  df <- cbind(Choices, N=rep(respondents_count, nrow(ma_matrix_nums)), ma_matrix_nums)
  return(df)
}



#' Create Results Tables and Pair Them to Questions
#'
#' The generate_results function takes a list of questions which have
#' their responses paired to them, determines their question type,
#' uses the results generation functions to create their results table,
#' and saves the table to the question's [['Table']] element. The function
#' returns the list of questions with their paired results tables.
#'
#' @param questions A list of questions with the relevant response columns
#' stored as a data frame under the questions[[i]][['Responses']] element. Create
#' such a list of questions by using link_responses_to_questions.
#'
#' @return A list of questions with their results tables paired to them
#' under the questions[[i]][['Table']]
generate_results <- function(questions) {

  # loop through all the questions that have responses,
  # and for each question that has responses, determine
  # it's question type (among the ones which have question
  # results generating functions), then generate the results for
  # that question and save them to that question.
  for (i in 1:length(questions)) {
    if (is.null(questions[[i]][['Responses']])) {
      has_responses <- FALSE
    } else {
      has_responses <- ncol(questions[[i]][['Responses']]) != 0
    }

    if (has_responses) {
      questions[[i]][['Table']] <- NULL

      if (is_mc_multiple_answer(questions[[i]])) {
        try(questions[[i]][['Table']] <-
              mc_multiple_answer_results(questions[[i]]), silent = TRUE)

      } else if (is_mc_single_answer(questions[[i]])) {
        try(questions[[i]][['Table']] <-
              mc_single_answer_results(questions[[i]]), silent = TRUE)

      } else if (is_matrix_multiple_answer(questions[[i]])) {
        try(questions[[i]][['Table']] <-
              matrix_multiple_answer_results(questions[[i]]), silent = TRUE)

      } else if (is_matrix_single_answer(questions[[i]])) {
        try(questions[[i]][['Table']] <-
              matrix_single_answer_results(questions[[i]]), silent = TRUE)
      }
    }
  }

  return(questions)
}
