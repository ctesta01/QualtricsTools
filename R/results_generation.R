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
  # always rounding 5 up
  # taken from the following stackoverflow post
  # http://stackoverflow.com/questions/12688717/round-up-from-5-in-r
  round2 = function(x, n) {
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5
    z = trunc(z)
    z = z/10^n
    z*posneg
  }
  paste0(formatC(round2(100 * x, 1), format = format, digits = digits, ...), "%")
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
    if ("RecodeValues" %in% names(question[['Payload']]) && length(question[['Payload']][['RecodeValues']]) > 0) {
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
    if ("RecodeValues" %in% names(question[['Payload']]) && length(question[['Payload']][['RecodeValues']]) > 0) {
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
    names(N) <- sapply(names(N), function(x) names(question[['Payload']][['RecodeValues']])[which(question[['Payload']][['RecodeValues']] == x)])
  }

  # After recoding the choices, grab their choice text.
  # Clean the choice text of HTML, and format the data as percents.
  choices <- lapply(names(N), function(x) question[['Payload']][['Choices']][[x]][[1]])
  choices <- lapply(choices, clean_html)
  choices <- unlist(choices, use.names = FALSE)
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
  if ("RecodeValues" %in% names(question[['Payload']]) && length(question[['Payload']][['RecodeValues']]) > 0) {
    factors <- unlist(question[['Payload']][['RecodeValues']])
  } else if ("AnswerOrder" %in% names(question[['Payload']]) && length(question[['Payload']][['AnswerOrder']]) > 0){
    factors <- unlist(question[['Payload']][['AnswerOrder']])
  } else {
    factors <- names(question[['Payload']][['Answers']])
  }

  orig_responses <- question[['Responses']]
  not_text_columns <- which(sapply(colnames(orig_responses), function(x) !(grepl("TEXT", x))))
  orig_responses <- orig_responses[, not_text_columns]
  responses <- sapply(orig_responses, function(x) table(factor(x, factors)))
  N <- sapply(orig_responses, function(x) suppressWarnings(strtoi(length(which(as.integer(as.character(x)) > 0)))))

  # flip the responses such that the sub-questions are the rows, and the choices
  # appear on top as columns.
  # if the choices have been recoded, go through the [['Payload']][['RecodeValues']] and [['Payload']][['ChoiceDataExportTags']]
  # to get the original
  # choice texts.
  # if the choices haven't been recoded, use the [['Payload']][['Answers']] and [['Payload']][['Choices']] to retrieve the original
  # choice texts.
  # replace the column names with the choice text.
  responses <- t(responses)

  # in Qualtrics Insights, Qualtrics has improved the naming of the response columns.
  # prior to insights, they did not include the data export tag in the response header.
  # After insights, the data export tag has been included in the response columns.
  # The choice export tags also have "-" replaced with "_" whenever they're used as response
  # headers. We check if the response columns are exactly the choice data export tags,
  # or if they are the choice export tags prepended with the data export tags.
  choice_export_tags_with_underscores <- sapply(question[['Payload']][['ChoiceDataExportTags']], function(x) gsub("-", "_", x))
  response_names_without_export_tag <- gsub(paste0(question[['Payload']][['DataExportTag']], "_"), "", names(orig_responses))



  if ("RecodeValues" %in% names(question[['Payload']]) && length(question[['Payload']][['RecodeValues']]) > 0) {
    if (any(question[['Payload']][['RecodeValues']] < 0)) {

      # if there are RecodeValues that are less than 0,
      # consider them as "No Opinion" / NA / "Prefer not to Answer"
      # kinds of responses, and table them separately so that
      # the rest of the table isn't skewed by the non-respondents.
      na_choices <- question[['Payload']][['RecodeValues']][which(question[['Payload']][['RecodeValues']] < 0)]
      na_columns <- responses[, unlist(na_choices), drop=FALSE]
      responses <- responses[,!(colnames(responses) %in% na_choices)]
      total_N <- sapply(orig_responses, function(x) strtoi(length(which(x != -99 & x != ""))))

      # calculate the percentages of non-applicable respondents from the
      # total respondents.
      for (i in 1:ncol(na_columns)) {
        for (j in 1:nrow(na_columns)) {
          na_columns[j,i] <- percent0(strtoi(na_columns[j,i]) /
                                        total_N[i])
        }
      }

      # get the choice names for the non-applicable choices
      colnames(na_columns) <- sapply(colnames(na_columns), function(x)
        names(question[['Payload']][['RecodeValues']][which(question[['Payload']][['RecodeValues']] == x)])[[1]])
      colnames(na_columns) <- sapply(colnames(na_columns), function(x) question[['Payload']][['Answers']][[x]][[1]])
    }

    # get the answer names for the response table's vertical "answer" components
    answers_uncoded <- sapply(colnames(responses), function(x)
      names(question[['Payload']][['RecodeValues']][which(question[['Payload']][['RecodeValues']] == x)])[[1]])
    answers <- lapply(answers_uncoded, function(x) question[['Payload']][['Answers']][[x]][[1]])
    answers <- unlist(answers, use.names = FALSE)
  } else {
    answers <- sapply(colnames(responses), function(x) question[['Payload']][['Answers']][[x]][[1]])
  }
  answers <- lapply(answers, clean_html)
  colnames(responses) <- answers

  # create the responses table, a table detailing the
  # percentage of times each answer was chosen for each sub-question (choice).
  # N is a list that says for each choice how many respondents there were that chose it.
  # percent0 is used to convert the number of respondents divided
  # by the total valid respondents to a single decimal point percent.
  for (i in 1:ncol(responses)) {
    for (j in 1:nrow(responses)) {
      responses[j,i] <- percent0(strtoi(responses[j,i]) /
                                   N[j])
    }
  }

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
  if (exists('na_columns') && exists('total_N')) {
    responses <- data.frame(choices, N, responses, total_N, na_columns, check.names=FALSE, row.names = NULL)
  } else {
    responses <- data.frame(choices, N, responses, check.names=FALSE, row.names = NULL)
  }
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
  # remove the text entry columns from the responses
  orig_responses <- question[['Responses']]
  not_text_columns <- which(sapply(colnames(orig_responses), function(x) isTRUE(!(grepl("TEXT", x)))))
  orig_responses <- orig_responses[, not_text_columns]

  # remove the data export tag from the column headers of the response data
  export_tag <- question[['Payload']][['DataExportTag']]
  names(orig_responses) <- gsub(paste0(export_tag, "_"), "", names(orig_responses))

  # responses_by_answers is for separating the responses to
  # each answer row.
  # the response columns are split by their names excluding
  # the last "_[-0-9]+$" part, (an underscore and an integer before the end of the
  # string).
  responses_by_answers <- list()
  answer_codes <- sapply(names(orig_responses), function(x) gsub("_[-0-9]+$", "", x, perl=TRUE))
  for (ans in unique(answer_codes)) {

    # for each answer, save response columns that start with that answer.
    responses_by_answers[[ans]] <- list()
    responses_by_answers[[ans]][['responses']] <- list()
    responses_by_answers[[ans]][['responses']] <- orig_responses[,
                                                                 which(gdata::startsWith(names(orig_responses), ans))
                                                                 ]

    # remove the answer from the column name
    colnames(responses_by_answers[[ans]][['responses']]) <-
      sapply(colnames(responses_by_answers[[ans]][['responses']]),
             function(x) gsub(paste0(ans, "_"), "", x))

    # calculate the total number of valid respondents, N
    responses_by_answers[[ans]][['N']] <- length(which(apply(
      responses_by_answers[[ans]][['responses']],
      1,
      function(x) any(x==1))))

    # if there are RecodeValues indicating NA like options,
    # separate them from the valid respondents, recalculate N,
    # and calculate total_N
    if ("RecodeValues" %in% names(question[['Payload']]) &&
        any(question[['Payload']][['RecodeValues']] < 0)) {

      # if the response columns have use recode values
      # to represent the choices
      if (all(names(responses_by_answers[[ans]][['responses']]) %in%
              question[['Payload']][['RecodeValues']])) {

        # separate na_columns from the valid responses
        responses_by_answers[[ans]][['na_columns']] <- list()
        responses_by_answers[[ans]][['na_columns']] <- responses_by_answers[[ans]][['responses']][
          which(names(responses_by_answers[[ans]][['responses']]) < 0)
          ]
        responses_by_answers[[ans]][['responses']] <- responses_by_answers[[ans]][['responses']][
          which(names(responses_by_answers[[ans]][['responses']]) >= 0)
          ]


        # otherwise, if the response columns use the
        # Choices directly to represent the choices
      } else if (all(names(responses_by_answers[[ans]][['responses']]) %in%
                     names(question[['Payload']][['RecodeValues']]))) {

        # separate na_columns from the valid responses
        na_cols <- which(sapply(
          names(responses_by_answers[[ans]][['responses']]),
          function(x) question[['Payload']][['RecodeValues']][[x]] < 0))
        responses_by_answers[[ans]][['na_columns']] <- list()
        responses_by_answers[[ans]][['na_columns']] <- responses_by_answers[[ans]][['responses']][na_cols]
        responses_by_answers[[ans]][['responses']] <- responses_by_answers[[ans]][['responses']][
          which(!names(responses_by_answers[[ans]][['responses']]) %in% na_cols)
          ]


      }

      # calculate the total_N and recalculate N to be the number of
      # valid respondents only
      responses_by_answers[[ans]][['total_N']] <-
        responses_by_answers[[ans]][['N']]
      responses_by_answers[[ans]][['N']] <- length(which(apply(
        responses_by_answers[[ans]][['responses']],
        1,
        function(x) any(x==1))))


    }

    # turn the responses for this answer into a list of percentages
    # for each choice
    responses_by_answers[[ans]][['responses']] <- apply(
      responses_by_answers[[ans]][['responses']],
      2,
      function(x) percent0(sum(x==1) / responses_by_answers[[ans]][['N']]))

    if (all(c('na_columns', 'total_N') %in% names(responses_by_answers[[ans]]))) {
      # turn the not-applicable columns for this answer into a
      # list of percentages for each non-applicable choice
      responses_by_answers[[ans]][['na_columns']] <- apply(
        responses_by_answers[[ans]][['na_columns']],
        2,
        function(x) percent0(sum(x==1) / responses_by_answers[[ans]][['total_N']]))
    }
  }

  # get the list of Ns for every answer, get the percents for each choices
  # for each answer, and table them.
  N <- lapply(responses_by_answers, '[[', 'N')
  choices <- t(as.data.frame(lapply(responses_by_answers, '[[', 'responses'),
                             optional=TRUE))

  # this is a helper function for renaming the choices based on the
  # recodevalues, if appropriate. Insights uses the recodevalues reliably,
  # but legacy didn't. So the test is whether or not all the choices are
  # in the recodevalues.
  rename_choices <- function(choice_names, question) {
    if ("RecodeValues" %in% names(question[['Payload']]) &&
        all(choice_names %in% question[['Payload']][['RecodeValues']])) {

      sapply(choice_names, function(x) {
        code <- names(question[['Payload']][['RecodeValues']])[
          which(question[['Payload']][['RecodeValues']] == x)
          ]
        question[['Payload']][['Answers']][[code]][[1]]
      })
    } else {
      sapply(choice_names, function(x) {
        question[['Payload']][['Answers']][[x]][[1]]
      })
    }
  }

  # relabel the names of the valid choices
  colnames(choices) <- rename_choices(colnames(choices), question)


  # if there were non-applicable options, table them, re-label the choices,
  # then table the N, choices, total_N, and na_choices together.
  # if there weren't non-applicable options, just table the
  # valid N and the choices
  if (all(c('total_N', 'na_columns') %in% sapply(responses_by_answers, names))) {
    na_choices <- t(as.data.frame(lapply(responses_by_answers, '[[', 'na_columns'),
                                  optional=TRUE))
    total_N <- lapply(responses_by_answers, '[[', 'total_N')
    colnames(na_choices) <- rename_choices(colnames(na_choices), question)
    responses_tabled <- cbind(N, choices, total_N, na_choices)

  } else {
    responses_tabled <- cbind(N, choices)
  }

  # if the rownames are labeled by the Choice Data Export Tags, use them to convert
  # their labels to the question parts' names.
  # if not, just try to use the labels directly to retrieve the part's labeling.
  if (all(rownames(responses_tabled) %in% question[['Payload']][['ChoiceDataExportTags']])) {
    rownames(responses_tabled) <- sapply(rownames(responses_tabled), function(x) {
      code <- names(question[['Payload']][['ChoiceDataExportTags']])[
        which(question[['Payload']][['ChoiceDataExportTags']] == x)
        ]
      question[['Payload']][['Choices']][[code]][[1]]
    })
  } else {
    rownames(responses_tabled) <- sapply(rownames(responses_tabled),
                                         function(x) question[['Payload']][['Choices']][[x]][[1]])
  }

  # include the rownames as the first row
  responses_tabled <- cbind(rownames(responses_tabled), responses_tabled)
  colnames(responses_tabled)[1] <- " "
  return(responses_tabled)
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
