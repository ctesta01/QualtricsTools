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
mc_single_answer_results <- function(question, original_first_rows) {
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

  # get the data export tag and questionID
  exporttag <- question[['Payload']][['DataExportTag']]
  questionID <- question[['Payload']][['QuestionID']]

  # if the original_first_rows are available, we use them to pick the right
  # response column, and if not, we select the column by using the DataExportTag
  if (!missing(original_first_rows)) {
    selected_column <- which(original_first_rows[2,] == questionID)
    selected_column <- colnames(original_first_rows)[selected_column]
    responses <- question[['Responses']][[selected_column]]
  } else {
    responses <- question[['Responses']][[exporttag]]
  }

  # first, take the responses and sort them by the factors to get responses_tabled.
  # the second column of the responses_tabled are the numbers of responses for each factor, our Ns.
  # total number of responses to a question is counted by all the answers that aren't -99
  # use Ns and respondent_count to calculate the percents for each factor.
  responses_tabled <- as.data.frame(table(factor(responses, levels=factors)))
  N <- responses_tabled[,2]
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

  # append the results table to the question
  question[['Table']] <- results_table

  # add a denominator note to the question
  if (!'qtNotes' %in% names(question)) question[['qtNotes']] <- list()
  question[['qtNotes']] <- c(question[['qtNotes']], paste0('Denominator Used: ', toString(respondent_count)))
  return(question)
}


#' Create the Results Table for a Multiple Choice Single Answer Question
#'
#' The mc_multiple_answer_results function uses the definition of the choices in the QSF file
#' and their (if present) recoded values to determine how to table the results paired to that question.
#'
#' @inheritParams mc_single_answer_results
#' @return a table with an N, Percent, and choice column, detailing the number of responses for each
#' choice.
mc_multiple_answer_results <- function(question, original_first_rows) {
  # save the original responses
  orig_responses <- question[['Responses']]

  # determine if we should use the original_first_rows
  if (!missing(original_first_rows) &&
      all(colnames(orig_responses) %in% colnames(original_first_rows)) &&
      dim(original_first_rows)[[1]] >= 2
  ) {
    should_use_ofr <- TRUE
  } else should_use_ofr <- FALSE

  # search either the import IDs or response column names
  # for the string "TEXT", and include all but those including
  # "TEXT" from the relevant_responses
  if (should_use_ofr) {
    relevant_responses <- orig_responses[
      which(unlist(lapply(colnames(orig_responses), function(x) !grepl("TEXT", original_first_rows[2,x]))))
      ]
  } else {
    relevant_responses <- orig_responses[
      which(unlist(lapply(colnames(orig_responses), function(x) !grepl("TEXT", x))))
      ]
  }

  # create a list of possible variations of the data export tag
  data_export_tag <- question[['Payload']][['DataExportTag']]
  data_export_tags <- c(data_export_tag, gsub("#", "_", data_export_tag), gsub("-", "_", data_export_tag))
  data_export_tags <- unique(c(paste0(data_export_tags, "_"), data_export_tags))
  data_export_tags <- paste0(data_export_tags, collapse="|")

  # rename the columns to be the choice indices they represent
  colnames(relevant_responses) <- lapply(colnames(relevant_responses), function(x) {
    if (should_use_ofr) {
      choice_index <- original_first_rows[2,x]
      choice_index <- gsub("QID[0-9]*-", "", choice_index)
    } else {
      x <- gsub(data_export_tags, "", x)
      if ("RecodeValues" %in% names(question[['Payload']]) && x %in% question[['Payload']][['RecodeValues']]) {
        x <- names(question[['Payload']][['RecodeValues']])[which(
          question[['Payload']][['RecodeValues']] == x
        )]
      }
      return(x)
    }
  })

  # get the number of respondents for each choice
  N <- lapply(relevant_responses, function(x) sum(x != 0 & x != -99 & x != ""))

  # determine if the question has any NA-type choices
  if ('RecodeValues' %in% names(question[['Payload']])) {
    has_na <- any(question[['Payload']][['RecodeValues']] < 0)
  } else has_na <- FALSE

  # if the question has NA choices, calculate a valid_denominator
  if (has_na) {
    non_negative_columns <- which(unlist(lapply(colnames(relevant_responses), function(x) {
      question[['Payload']][['RecodeValues']][[x]] >= 0
    })))
    non_negative_responses <- relevant_responses[non_negative_columns]
    valid_denominator <- length(which(apply(non_negative_responses, 1, function(x) {
      !(all(x == -99) | all(x == "") | all(x == 0))
    })))
  }

  # calculate the total denominator
  total_denominator <- length(which(apply(relevant_responses, 1, function(x) {
    !(all(x %in% c(-99,"",0)))})))


  # calculate the percent for each column:
  # if it's an NA-column use the total denominator,
  # if it's not an NA-column, but the question has NA options, use the valid denominator
  # if the question has no NA choices, use the total_denominator
  Percent <- lapply(1:length(N), function(x) {
    if (has_na &&
        !names(N)[[x]] %in% colnames(non_negative_responses)) {
      percent0(N[[x]] / total_denominator)
    } else if (has_na &&
               names(N)[[x]] %in% colnames(non_negative_responses)) {
      percent0(N[[x]] / valid_denominator)
    } else {
      percent0(N[[x]] / total_denominator)
    }
  })

  # get the choice text for each column, and then clean the HTML out of it
  choices <- lapply(names(N), function(x) question[['Payload']][['Choices']][[x]][[1]])
  choices <- clean_html(choices)

  # make sure that these are flat lists
  choices <- unlist(choices, use.names=FALSE)
  N <- unlist(N, use.names=FALSE)
  Percent <- unlist(Percent, use.names=FALSE)

  # construct and return the output data frame
  results_table <- data.frame(N, Percent, choices, row.names=NULL)
  colnames(results_table)[3] <- ""

  # append the results table
  question[['Table']] <- results_table

  # add a note for the denominators used in the question
  if ('qtNotes' %in% names(question)) question[['qtNotes']] <- list()
  if (exists('valid_denominator')) {
    question[['qtNotes']] <- c(question[['qtNotes']], paste0('Valid Denominator Used: ',
                                    toString(valid_denominator)))
    question[['qtNotes']] <- c(question[['qtNotes']], paste0('Total Denominator Used: ',
                                    toString(total_denominator)))
  } else {
    question[['qtNotes']] <- c(question[['qtNotes']], paste0('Denominator Used: ',
                                    toString(total_denominator)))
  }

  return(question)
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
matrix_single_answer_results <- function(question, original_first_rows) {
  # save the original responses
  orig_responses <- question[['Responses']]

  # determine if we should use the original_first_rows
  if (!missing(original_first_rows) &&
      all(names(orig_responses) %in% names(original_first_rows)) &&
      nrow(original_first_rows) >= 2) {
    should_use_ofr <- TRUE
  } else should_use_ofr <- FALSE

  # search either the import IDs or response column names
  # for the string "TEXT", and include all but those including
  # "TEXT" from the relevant_responses
  if (should_use_ofr) {
    relevant_responses <- orig_responses[
      which(unlist(lapply(colnames(orig_responses), function(x) !grepl("TEXT", original_first_rows[2,x]))))
      ]
  } else {
    relevant_responses <- orig_responses[
      which(unlist(lapply(colnames(orig_responses), function(x) !grepl("TEXT", x))))
      ]
  }

  # replace the column names with the answer indices
  if (should_use_ofr) {
    # if we should use the original_first_rows, then replace the column names with the import tags,
    # then remove the question-ID from those import tags, leaving only the choice indices
    names(relevant_responses) <- lapply(names(relevant_responses), function(x) original_first_rows[2, x])
    question_id <- question[['Payload']][['QuestionID']]
    colnames(relevant_responses) <- lapply(names(relevant_responses), function(x) gsub(paste0(question_id, "-"), "", x))
  } else {
    # create a list of possible variations of the data export tag
    data_export_tag <- paste0("^", question[['Payload']][['DataExportTag']])
    data_export_tags <- c(data_export_tag, gsub("#", "_", data_export_tag), gsub("-", "_", data_export_tag))
    data_export_tags <- unique(c(paste0(data_export_tags, "_"), data_export_tags))
    data_export_tags <- paste0(data_export_tags, collapse="|")

    # remove the data export tag from the response column names
    colnames(relevant_responses) <- lapply(colnames(relevant_responses), function(x) gsub(data_export_tags, "", x))

    # if the question is taken from a side-by-side question, then it has an AnswerDataExportTag that
    # needs removed from the end of the column names
    if ("AnswerDataExportTag" %in% names(question[['Payload']])) {
      colnames(relevant_responses) <- lapply(colnames(relevant_responses), function(x) gsub(
        paste0("_", question[['Payload']][['AnswerDataExportTag']], "$"), "", x))
    }

    # if there's ChoiceDataExportTags being used, translate the column names from recode values
    # to answer indices
    if ("ChoiceDataExportTags" %in% names(question[['Payload']]) &&
        typeof(question[['Payload']][['ChoiceDataExportTags']]) != "logical"  &&
        all(colnames(relevant_responses) %in% question[['Payload']][['ChoiceDataExportTags']])) {
      colnames(relevant_responses) <- lapply(colnames(relevant_responses), function(x) {
        names(question[['Payload']][['ChoiceDataExportTags']])[
          which(question[['Payload']][['ChoiceDataExportTags']] == x)
          ]})
    }
  }

  # determine if the question has any NA-type choices
  if ('RecodeValues' %in% names(question[['Payload']])) {
    has_na <- any(question[['Payload']][['RecodeValues']] < 0)
  } else has_na <- FALSE

  # calculate the valid denominator for each answer
  valid_denominator <- apply(relevant_responses, 2, function(x) sum(x >= 0))

  # calculate the total denominator for each answer
  total_denominator <- apply(relevant_responses, 2, function(x) sum(x != -99 & x != ""))

  # get the na responses for the question, if it has NA responses
  if (has_na) {
    na_factors <- question[['Payload']][['RecodeValues']][
      which(question[['Payload']][['RecodeValues']] < 0)
      ]
  }

  # get the valid responses for the question
  if ("RecodeValues" %in% names(question[['Payload']]) &&
      length(question[['Payload']][['RecodeValues']]) > 0) {
    valid_factors <- question[['Payload']][['RecodeValues']][
      which(question[['Payload']][['RecodeValues']] >= 0)
      ]
  } else {
    valid_factors <- names(question[['Payload']][['Answers']])
  }

  # table the responses
  valid_responses <- sapply(relevant_responses, function(x) table(factor(x, valid_factors)))
  if (! is.data.frame(valid_responses)) {
    valid_responses <- as.data.frame(valid_responses)
    valid_responses <- t(valid_responses)
    colnames(valid_responses) <- valid_factors
    rownames(valid_responses) <- colnames(relevant_responses)
  } else valid_responses <- t(valid_responses)
  if (has_na) {
    na_responses <- sapply(relevant_responses, function(x) table(factor(x, na_factors)))
    if (! is.data.frame(na_responses)) {
      na_responses <- as.data.frame(na_responses)
      na_responses <- t(na_responses)
      colnames(na_responses) <- na_factors
      rownames(na_responses) <- colnames(relevant_responses)
    } else na_responses <- t(na_responses)
  }
  valid_responses <- as.data.frame(valid_responses)

  # convert the number of respondents for each answer (row) by choice (column) combination
  # to a percentage
  for (i in 1:nrow(valid_responses)) {
    for (j in 1:ncol(valid_responses)) {
      if (valid_responses[i, j] == 0 | valid_denominator[[i]] == 0) {
        valid_responses[i, j] <- percent0(0)
      } else {
        valid_responses[i, j] <- percent0(as.integer(valid_responses[i, j]) / valid_denominator[[i]])
      }
    }
  }

  # if there's a set of na_responses
  # convert the number of respondents for each answer (row) by choice (column) combination
  # to a percentage
  if (has_na) {
    for (i in 1:nrow(na_responses)) {
      for (j in 1:ncol(na_responses)) {
        if (na_responses[i, j] == 0 | total_denominator[[i]] == 0) {
          na_responses[i, j] <- percent0(0)
        } else {
          na_responses[i, j] <- percent0(as.integer(na_responses[i, j]) / total_denominator[[i]])
        }
      }
    }
  }

  # translate the recode values to choice indices
  if ("RecodeValues" %in% names(question[['Payload']]) &&
      colnames(valid_responses) %in% question[['Payload']][['RecodeValues']]) {
    colnames(valid_responses) <- lapply(colnames(valid_responses), function(x) {
      names(question[['Payload']][['RecodeValues']])[which(question[['Payload']][['RecodeValues']] == x)]
    })
    if (has_na) {
      colnames(na_responses) <- lapply(colnames(na_responses), function(x) {
        names(question[['Payload']][['RecodeValues']])[which(question[['Payload']][['RecodeValues']] == x)]
      })
    }
  }

  # Reorder using the AnswerOrder
  if ("AnswerOrder" %in% names(question[['Payload']]) && should_use_ofr) {
    if (has_na) {
      answers <- sapply(unlist(question[['Payload']][['AnswerOrder']]), function(x) question[['Payload']][['RecodeValues']][[toString(x)]])
      valid_answers <- which(answers >= 0)
      valid_responses <- valid_responses[, valid_answers]
    } else {
      valid_responses <- valid_responses[, unlist(question$Payload$AnswerOrder)]
    }
  }

  # translate the choice indices to choice text
  colnames(valid_responses) <- lapply(colnames(valid_responses), function(x) question[['Payload']][['Answers']][[x]][[1]])
  if (has_na) colnames(na_responses) <- lapply(colnames(na_responses), function(x) question[['Payload']][['Answers']][[x]][[1]])
  colnames(valid_responses) <- lapply(colnames(valid_responses), clean_html)
  if (has_na) colnames(na_responses) <- lapply(colnames(na_responses), clean_html)

  # get the answer text as a list
  choices <- rownames(valid_responses)
  if ('ChoiceDataExportTags' %in% names(question[['Payload']]) &&
      typeof(question[['Payload']][['ChoiceDataExportTags']]) != 'logical' &&
      rownames(valid_responses) %in% question[['Payload']][['ChoiceDataExportTags']]) {
    choices <- lapply(choices, function(x) names(question[['Payload']][['ChoiceDataExportTags']])[
      which(question[['Payload']][['ChoiceDataExportTags']] == x)
      ])
  }
  choices <- lapply(choices, function(x) question[['Payload']][['Choices']][[x]][[1]])
  choices <- lapply(choices, clean_html)
  choices <- unlist(choices, use.names=FALSE)

  # construct the data frame
  if (has_na) {
    results_table <- data.frame(choices, N=valid_denominator, valid_responses, total_N=total_denominator, na_responses, check.names=FALSE, row.names=NULL)
  } else {
    results_table <- data.frame(choices, N=valid_denominator, valid_responses, check.names=FALSE, row.names=NULL)
  }

  # clean up the colnames and rownames
  colnames(results_table)[1] <- ""
  rownames(results_table) <- NULL

  # append the results table
  question[['Table']] <- results_table
  return(question)
}

#' Create the Results Table for a Matrix Multiple Answer Question
#'
#' @inheritParams mc_single_answer_results
#' @return a table with the matrix-sub-questions listed in the first column,
#' a column with the total number of respondents for each subquestion, and
#' the percentages for each choice for each sub-question.
matrix_multiple_answer_results <- function(question, original_first_rows) {
  # save the original responses
  orig_responses <- question[['Responses']]

  # determine if we should use the original_first_rows
  if (!missing(original_first_rows) &&
      all(colnames(orig_responses) %in% colnames(original_first_rows)) &&
      dim(original_first_rows)[[1]] >= 2
  ) {
    should_use_ofr <- TRUE
  } else should_use_ofr <- FALSE

  # search either the import IDs or response column names
  # for the string "TEXT", and include all but those including
  # "TEXT" from the relevant_responses
  if (should_use_ofr) {
    relevant_responses <- orig_responses[
      which(unlist(lapply(colnames(orig_responses), function(x) !grepl("TEXT", original_first_rows[2,x]))))
      ]
  } else {
    relevant_responses <- orig_responses[
      which(unlist(lapply(colnames(orig_responses), function(x) !grepl("TEXT", x))))
      ]
  }

  # create a list of possible variations of the data export tag
  data_export_tag <- question[['Payload']][['DataExportTag']]
  data_export_tags <- c(data_export_tag, gsub("#", "_", data_export_tag), gsub("-", "_", data_export_tag))
  data_export_tags <- unique(c(paste0(data_export_tags, "_"), data_export_tags))
  data_export_tags <- paste0(data_export_tags, collapse="|")

  # remove the QID or Question Data Export Tags from the columns
  # now columns look like "1_4" and "avocados_10"
  colnames(relevant_responses) <- lapply(colnames(relevant_responses), function(x) {
    if (should_use_ofr) {
      choice_index <- original_first_rows[2,x]
      choice_index <- gsub("^QID[# 0-9]*-", "", choice_index)
    } else {
      x <- gsub(data_export_tags, "", x)
    }
  })

  # if the question is reduced from a side-by-side question,
  # then we need to remove the AnswerDataExportTag from the end of the column names
  if ("AnswerDataExportTag" %in% names(question[['Payload']])) {
    names(relevant_responses) <- gsub(paste0("_", question[['Payload']][['AnswerDataExportTag']]), "", names(relevant_responses))
  }

  # separate the response columns into a list for each
  # answer row
  responses_by_answers <- list()
  answer_codes <- sapply(names(relevant_responses), function(x) gsub("[_ -][-0-9]+$", "", x, perl=TRUE))
  for (ans in unique(answer_codes)) {

    # for each answer, save response columns that start with that answer.
    responses_by_answers[[ans]] <- list()
    responses_by_answers[[ans]][['responses']] <- list()
    responses_by_answers[[ans]][['responses']] <-
      relevant_responses[which(grepl(paste0("^", ans, "[_ -]"), names(relevant_responses)))]

    # remove the answer from the column name
    colnames(responses_by_answers[[ans]][['responses']]) <-
      sapply(colnames(responses_by_answers[[ans]][['responses']]),
             function(x) gsub(paste0(ans, "[_ -]"), "", x))

    # calculate the total number of valid respondents, N
    responses_by_answers[[ans]][['N']] <- length(which(apply(
      responses_by_answers[[ans]][['responses']],
      1,
      function(x) any(x!=0 & x!= "" & x!=-99))))

    # if there are RecodeValues indicating NA like options,
    # separate them from the valid respondents, recalculate N,
    # and calculate total_N
    if ("RecodeValues" %in% names(question[['Payload']]) &&
        suppressWarnings(any(question[['Payload']][['RecodeValues']] < 0))) {

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
          function(x) suppressWarnings(question[['Payload']][['RecodeValues']][[x]] < 0)))
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
      function(x) {
        if (sum(x!=-99 & x!=0 & x!="") != 0 && responses_by_answers[[ans]][['N']] != 0) {
          percent0(sum(x!=-99 & x!=0 & x!="") / responses_by_answers[[ans]][['N']])
        } else percent0(0)
      }
    )

    if (all(c('na_columns', 'total_N') %in% names(responses_by_answers[[ans]]))) {
      # turn the not-applicable columns for this answer into a
      # list of percentages for each non-applicable choice
      responses_by_answers[[ans]][['na_columns']] <- apply(
        responses_by_answers[[ans]][['na_columns']],
        2,
        function(x) {
          if (sum(x!=-99 & x!=0 & x!="") != 0 && responses_by_answers[[ans]][['total_N']] != 0) {
            percent0(sum(x!=-99 & x!=0 & x!="") / responses_by_answers[[ans]][['total_N']])
          } else percent0(0)
        }
      )
    }
  }

  # if there's only one column to the question, then we will use the number of
  # respondents who answered any part of the question as the denominator, instead
  # of the number of respondents who answered that specific question part.
  if (all(lapply(responses_by_answers, function(x) length(x[['responses']])) == 1)) {
    all_question_respondents <- length(which(apply(
      relevant_responses,
      1,
      function(x) any(x!=0 & x!= -99 & x!=""))))
    for (ans in answer_codes) {
      if (responses_by_answers[[ans]][['N']] != 0 &
          all_question_respondents != 0) {
        responses_by_answers[[ans]][['responses']] <- percent0(
          responses_by_answers[[ans]][['N']] /
            all_question_respondents
        )
      } else percent0(0)
    }
  }

  # get the list of Ns for every answer, get the percents for each choices
  # for each answer, and table them.
  N <- lapply(responses_by_answers, '[[', 'N')
  choices <- t(as.data.frame(lapply(responses_by_answers, '[[', 'responses'),
                             optional=TRUE))

  # if there's only one option, then the choice will need manual naming because
  # R has trouble differentiating between single column data frames and lists
  only_one_option <- ncol(choices) == 1 && length(unique(gsub("[0-9 _ -]*-", "", colnames(relevant_responses)))) == 1
  if (only_one_option) colnames(choices) <- gsub("[0-9 _ -]*-", "", colnames(relevant_responses)[[1]])

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
  if ('ChoiceDataExportTags' %in% names(question[['Payload']]) &&
      all(rownames(responses_tabled) %in% question[['Payload']][['ChoiceDataExportTags']])) {
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

  # clean html out of the colnames and rownames
  rownames(responses_tabled) <- sapply(rownames(responses_tabled), clean_html)
  colnames(responses_tabled) <- sapply(colnames(responses_tabled), clean_html)

  # include the rownames as the first row
  responses_tabled <- cbind(rownames(responses_tabled), responses_tabled)
  colnames(responses_tabled)[1] <- " "
  question[['Table']] <- responses_tabled
  return(question)
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
generate_results <- function(questions, original_first_rows) {

  # loop through all the questions that have responses,
  # and for each question that has responses, determine
  # it's question type (among the ones which have question
  # results generating functions), then generate the results for
  # that question and save them to that question.
  for (i in 1:length(questions)) {
    questions[[i]] <- process_question_results(questions[[i]], original_first_rows)
  }
  return(questions)
}

process_question_results <- function(question, original_first_rows) {
  # get original_first_rows from global scope if not passed directly
  if (missing(original_first_rows)) original_first_rows <- get("original_first_rows", envir=globalenv())

  # we should only use original_first_rows if they're greater than 2 rows
  if (!missing(original_first_rows) &&
      nrow(original_first_rows) >= 2) {
    should_use_ofr <- TRUE
  } else should_use_ofr <- FALSE

  # Only process questions which have results
  if (is.null(question[['Responses']])) {
    has_responses <- FALSE
  } else {
    has_responses <- ncol(question[['Responses']]) != 0
  }

  if (has_responses) {
    question[['Table']] <- NULL

    try({
      # multiple choice multiple answer
      if (is_mc_multiple_answer(question)) {
        if (should_use_ofr) {
          question <- mc_multiple_answer_results(question, original_first_rows)
        } else {
          question <- mc_multiple_answer_results(question)
        }

        # multiple choice single answer
      } else if (is_mc_single_answer(question)) {
        if(should_use_ofr) {
          question <- mc_single_answer_results(question, original_first_rows)
        } else {
          question <- mc_single_answer_results(question)
        }

        # matrix multiple answer
      } else if (is_matrix_multiple_answer(question)) {
        if (should_use_ofr) {
          question <- matrix_multiple_answer_results(question, original_first_rows)
        } else {
          question <- matrix_multiple_answer_results(question)
        }

        # matrix single answer
      } else if (is_matrix_single_answer(question)) {
        if (should_use_ofr) {
          question <- matrix_single_answer_results(question, original_first_rows)
        } else {
          question <- matrix_single_answer_results(question)
        }
      }
    }, silent=TRUE)
  }
  return(question)
}
