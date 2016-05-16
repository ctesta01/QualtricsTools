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
#' under $Responses
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
    responses <- question$Responses[[question$Payload$DataExportTag]]
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
        choices <- sapply(choices_uncoded, function(x) question$Payload$Choices[[x]][[1]])
    } else {
        choices_uncoded <- responses_tabled[,1]
        choices <- sapply(choices_uncoded, function(x) question$Payload$Choices[[x]][[1]])
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
  respondents_count <- length(which(apply(
    question$Responses, 1, function(row) !(all(row == -99) | all(row == "")))))
  data_export_tag <- question$Payload$DataExportTag
  names(N) <- gsub(paste0(data_export_tag, "_"), "", names(question$Responses))
  if ("RecodeValues" %in% names(question$Payload) && names(N) %in% question$Payload$RecodeValues) {
    names(N) <- sapply(names(N), function(x) which(question$Payload$RecodeValues == x))
  }
  choices <- sapply(names(N), function(x) question$Payload$Choices[[x]][[1]])
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
  N <- sapply(question$Responses, function(x) strtoi(length(which(x != -99 & x != ""))))
  for (i in 1:nrow(responses)) {
    for (j in 1:ncol(responses)) {
      responses[i,j] <- percent0(strtoi(responses[i,j]) /
                              N[j])
    }
  }

  # flip the responses such that the sub-questions are the rows, and the choices
  # appear on top as columns.
  # if the choices have been recoded, go through the $Payload$RecodeValues and $Payload$ChoiceDataExportTags
  # to get the original
  # choice texts.
  # if the choices haven't been recoded, use the $Payload$Answers and $Payload$Choices to retrieve the original
  # choice texts.
  # replace the column names with the choice text.
  # the sub-question tags are formed as the $Payload$DataExportTag with an underscore, then an integer
  # starting from 1. Strip the DataExportTag and underscore from the sub-question tag, and then
  # use the $Payload$Choices list to retrieve the original sub-question text.
  responses <- t(responses)
  if ("RecodeValues" %in% names(question$Payload)) {
    answers_uncoded <- sapply(colnames(responses), function(x) which(question$Payload$RecodeValue == x))
    answers <- sapply(answers_uncoded, function(x) question$Payload$Answers[[x]][[1]])
    answers <- unlist(answers, use.names = FALSE)
  } else {
    answers <- sapply(colnames(responses), function(x) question$Payload$Answers[[x]][[1]])
  }
  answers <- sapply(answers, clean_html)
  colnames(responses) <- answers
  choice_export_tags_with_underscores <- sapply(question$Payload$ChoiceDataExportTags, function(x) gsub("-", "_", x))
  response_names_without_export_tag <- gsub(paste0(question$Payload$DataExportTag, "_"), "", names(question$Responses))
  if (all(names(question$Responses) %in% choice_export_tags_with_underscores)) {
    choices_uncoded <- sapply(rownames(responses), function(x) which(choice_export_tags == x))
    choices <- sapply(choices_uncoded, function(x) question$Payload$Choices[[x]][[1]])
  } else if (all(response_names_without_export_tag %in% question$Payload$ChoiceDataExportTags)){
    choices <- sapply(response_names_without_export_tag, function(x) which(question$Payload$ChoiceDataExportTags == x))
    choices <- sapply(choices, function(x) question$Payload$Choices[[x]][[1]])
  } else {
    export_tag_with_underscore <- paste0(question$Payload$DataExportTag, "_")
    choices <- sapply(rownames(responses), function(x)
      question$Payload$Choices[[gsub(export_tag_with_underscore, "", x)]][[1]])
  }
  if (length(choices) == length(N)) {
    choices <- unlist(choices, use.names = FALSE)
  } else {
    if (length(dim(choices)) == 2) {
      choices <- choices[1,]
    }
    choices <- lapply(choices, function(x) x$Display)
  }
  choices <- sapply(choices, clean_html)

  # form a data frame with the first column listing the sub-question text, then
  # include the table of percents for each answer choice for each sub-question,
  # then include as the last column the number of respondents to each sub-question.
  responses <- data.frame(choices, responses, N, check.names=FALSE, row.names = NULL)
  colnames(responses)[1] <- ""
  rownames(responses) <- NULL
  return(responses)
}


#' Create the Results Table for a Matrix Multiple Answer Question
#'
#' The matrix_multiple_answer_results function is the naive solution to creating the
#' results table for multiple answer matrix questions. It creates the results by
#' taking the columns of the question, splitting them into sets with as many
#' choices as there are per subquestion in each set of columns, and then
#' laying the calculated results of each set on top of one another. Unfortunately, this
#' will not take into account ordering. This assumes that the columns are in the default
#' ordering from Qualtrics and have not been moved. Hopefully later versions of the program
#' will adjust for this.
#'
#' @inheritParams mc_single_answer_results
#' @return a table with the matrix-sub-questions listed in the first column, the percentages for each
#' choice for each sub-question listed in a table, and then another column with the total respondents
#' for each subquestion.
matrix_multiple_answer_results <- function(question) {
  respondents_count <- sapply(question$Responses, function(y) strtoi(length(which(y != -99))))
  headernames <- sapply(question$Payload$Answers, function(y) y$Display)
  headernames <- sapply(headernames, clean_html)
  rownames <- sapply(question$Payload$Choices, function(y) y$Display)
  rownames <- sapply(rownames, clean_html)
  ma_matrix_sums <- sapply(question$Responses, function (y) sum(y == 1))
  chunk2 <- function(y,n) split(y, cut(seq_along(y), n, labels = FALSE))
  df <- t(as.data.frame((chunk2(ma_matrix_sums, length(headernames)))))
  rownames(df) <- rownames
  colnames(df) <- headernames
  respondents_count <- respondents_count[seq(1, length(question$Responses), length(headernames))]
  for (i in 1:nrow(df)) {
    for (j in 1:ncol(df)) {
      df[i,j] <- percent0(strtoi(df[i,j]) / respondents_count[i])
    }
  }
  choices <- sapply(question$Payload$Choices, function(y) y$Display)
  choices <- sapply(choices, clean_html)

  df <- cbind(df, N=respondents_count)
  df <- cbind("Choices"=choices, df)
  return(df)
}


#' Create Results Tables and Pair Them to Questions
#'
#' The generate_results function takes a list of questions which have
#' their responses paired to them, determines their question type,
#' uses the results generation functions to create their results table,
#' and saves the table to the question's $Table element. The function
#' returns the list of questions with their paired results tables.
#'
#' @param questions A list of questions with the relevant response columns
#' stored as a data frame under the questions[[i]]$Responses element. Create
#' such a list of questions by using link_responses_to_questions.
#'
#' @return A list of questions with their results tables paired to them
#' under the questions[[i]]$Table
generate_results <- function(questions) {

  for (i in 1:length(questions)) {
    if (is.null(questions[[i]]$Responses)) {
      has_responses <- FALSE
    } else {
      has_responses <- ncol(questions[[i]]$Responses != 0)
    }

    if (has_responses) {
      questions[[i]]$Table <- NULL
      if (is_mc_multiple_answer(questions[[i]])) {
        try(questions[[i]]$Table <- mc_multiple_answer_results(questions[[i]]), silent = TRUE)
      } else if (is_mc_single_answer(questions[[i]])) {
        try(questions[[i]]$Table <- mc_single_answer_results(questions[[i]]), silent = TRUE)
      } else if (is_matrix_multiple_answer(questions[[i]])) {
        try(questions[[i]]$Table <- matrix_multiple_answer_results(questions[[i]]), silent = TRUE)
      } else if (is_matrix_single_answer(questions[[i]])) {
        try(questions[[i]]$Table <- matrix_single_answer_results(questions[[i]]), silent = TRUE)
      }
    }
  }

  return(questions)
}


#' Create a List of HTML Versions of the Results Tables
#'
#' @param questions A list of questions with the relevant results tables
#' stored as data frames under the questions[[i]]$Table element. Create
#' such a list of questions by using generate_results function
#'
#' @return A list of HTML results tables for each question
html_tabelize <- function(blocks) {
  tables <- list()
  for (i in 1:length(blocks)) {
    if (length(blocks[[i]]$BlockElements) != 0) {
      for (j in 1:length(blocks[[i]]$BlockElements)) {
        # tables = c(print_tables, blocks[[i]]$BlockElements[[j]]$Payload$DataExportTag)
        if (is.null(blocks[[i]]$BlockElements[[j]]$Table) == FALSE) {
          tables = c(tables, capture.output(print(xtable::xtable(blocks[[i]]$BlockElements[[j]]$Table,
                                                caption=paste0("Question ",
                                                blocks[[i]]$BlockElements[[j]]$Payload$DataExportTag,
                                                ": ", blocks[[i]]$BlockElements[[j]]$Payload$QuestionTextClean)),
                                 type="html",
                                 html.table.attributes='class="data table table-bordered table-condensed"',
                                 caption.placement="top",
                                 include.rownames=FALSE)))
          tables = c(tables, "<br>")
        }
      }
    }
  }
  return(unlist(lapply(tables, paste)))
}


#' Create a Message Stating Which Questions Weren't Automatically Tabled
#'
#' This is function is used in the Shiny app to tell users which questions weren't
#' automatically coded. This may be changed later to be more informative, or
#' to include this information elsewhere.
#'
#' @inheritParams html_tabelize
#' @return A message stating for which questions could not have
#' results automatically generated.
uncodeable_questions_message <- function(questions) {
  uncodeable_questions <- which(sapply(questions, function(x) !("Table" %in% names(x))))
  uncodeable_questions <- sapply(uncodeable_questions, function(x)
    questions[[x]]$Payload$DataExportTag)
  uncodeable_message <- ""
  if (length(uncodeable_questions) > 0) {
    uncodeable_questions <- paste(uncodeable_questions, collapse=", ")
    uncodeable_message <- sprintf("The following questions could not be automatically coded: %s",
                                  uncodeable_questions)
  }
  return(uncodeable_message)
}
