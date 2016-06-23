# take a list of rows, all with the same length, and
# turn them into a data frame.
list_of_rows_to_df <- function(data) {
  nCol <- max(vapply(data, length, 0))
  data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
  data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
  data.frame(data)
}


#' Get the Index of the Question Corresponding to a Response Column
#'
#' Use this function to get the indexes identifying a question
#' in the blocks list. Give it a response column name, and it will
#' try to find the question it corresponds to. Otherwise, it will
#' respond NULL.
question_from_response_column <- function(blocks, response_name) {
  # construct a list, with keys as the response column names, and
  # values as pairs of block and blockelement indexes.
  responses_to_indexes <- list()
  for (i in 1:number_of_blocks(blocks)) {
    if ('BlockElements' %in% names(blocks[[i]])) {
      for (j in 1:length(blocks[[i]][['BlockElements']])) {
        if ("Responses" %in% names(blocks[[i]][['BlockElements']][[j]])) {
          for (k in names(blocks[[i]][['BlockElements']][[j]][['Responses']])) {
          responses_to_indexes[[k]] <- c(i, j)
          }
        }
      }
    }
  }
  return(responses_to_indexes[[response_name]])
}


#' Get the Choice Text based on the Choice from a Question
#'
#' Input a question and a choice, and this function will
#' do its best to give you back the choice text.
choice_text_from_question <- function(question, choice) {
  original <- choice
  choice <- as.character(choice)

  # if the question is a multiple answer question,
  # meaning some form of "check all that apply",
  # then the answers are boolean valued -- either they
  # checked it or they didn't. Return TRUE, FALSE, or
  # "Seen, but Unanswered" depending.
  if (is_multiple_choice(question)) {
    if (choice %in% c(1, "1")) {
      choice <- "Selected"
    } else {
      choice <- "Not Selected"
    }

    # if the question is a single answer multiple choice
    # question, then it either has recode values, or
    # the choice given is directly correspondent with
    # the index of the choice in the [['Payload']][['Choices']]
    # list. if the choice given doesn't match any
    # of the recode values, try getting it directly from
    # the choices.
  } else if (is_mc_single_answer(question)) {
    if ("RecodeValues" %in% names(question[['Payload']]) && length(question[['Payload']][['RecodeValues']]) > 0) {
      recoded_value <- which(question[['Payload']][['RecodeValues']] == choice)
      if (length(recoded_value) != 0)
        choice <- recoded_value
      if (choice %in% names(question[['Payload']][['Choices']]))
        choice <- question[['Payload']][['Choices']][[choice]][[1]]
    } else {
      if (choice %in% names(question[['Payload']][['Choices']]))
        choice <- question[['Payload']][['Choices']][[choice]][[1]]
    }


    # if the question is a single answer matrix question,
    # the question will either have recode values, or not.
    # if the question has recode values, attempt to use the
    # [['Payload']][['RecodeValues']] list to retrieve the recoded_value.
    # If that doesn't work, just use the original choice given.
  } else if (is_matrix_single_answer(question)) {
    if ("RecodeValues" %in% names(question[['Payload']]) && length(question[['Payload']][['RecodeValues']]) > 0) {
      recoded_value <- which(question[['Payload']][['RecodeValues']] == choice)
      if (length(recoded_value) != 0) {
        choice <- names(question[['Payload']][['RecodeValues']][recoded_value])[[1]]
      }
      if (choice %in% names(question[['Payload']][['Answers']]))
        choice <- question[['Payload']][['Answers']][[choice]][[1]]
    } else {
      if (choice %in% names(question[['Payload']][['Answers']]))
        choice <- question[['Payload']][['Answers']][[choice]][[1]]
    }
  }

  if (original %in% c(-99, "-99")) choice <- "Seen, but Unanswered"
  if (is.na(choice) || identical(choice, original)) choice <- ""
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
  shiny::runApp(system.file('shiny', package = 'QualtricsTools'))
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
get_setup <- function(headerrows, already_loaded) {
  if (missing(headerrows)) {
    headerrows <- 2
  }

  if (missing(already_loaded)) {
    already_loaded <- FALSE
  }

  if (already_loaded == FALSE) {
    try(survey <<- ask_for_qsf())
    try(responses <<- ask_for_csv(headerrows = headerrows))
  }

  if (already_loaded == TRUE) {
    if (!exists("survey", where = -1)) {
      survey <- sample_survey
    } else {
      survey <- get("survey", envir=-1)
    }

    if (!exists("responses", where = -1)) {
      responses <- sample_responses
    } else {
      responses <- get("responses", envir=-1)
    }
  }

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
  matched_question_index <- which(sapply(questions, function(x) x[['Payload']][['DataExportTag']] == exporttag))
  return(questions[[matched_question_index]])
}


#' Find Question Index from DataExportTag
#'
#' This function takes a list of questions and an export tag and
#' looks for the matching question. It returns the index(es) of
#' the questions with that Question Data Export Tag.
find_question_index <- function(questions, exporttag) {
  matched_question_index <- which(sapply(questions, function(x) x[['Payload']][['DataExportTag']] == exporttag))
  return(matched_question_index)
}


#' Return a list of a Question's Display Logic Components
#'
#' For each question, if they appear, go through the
#' Question's DisplayLogic, each Choice's DisplayLogic, and
#' each Answer's DisplayLogic. For each of them, use clean_html
#' to format them, and then add them to the list. If a question
#' has any of these display logic components, insert before
#' adding any display logic a line detailing what part of the
#' question the following display logic corresponds to.
#'
#' @param question A qualtrics survey question
#' @return an ordered list of display logic messages
display_logic_from_question <- function(question) {

  # display_logic is a list for storing display logic messages,
  # e will be the index we use to increment as we add to display_logic.
  display_logic <- list()
  e <- 1

  # if there is "DisplayLogic" in the question's payload,
  # add a message saying "Question Display Logic:", and then increment once.
  # Next, since DisplayLogic has many components, not all of which we are looking
  # to examine, we select the elements that are numeric.
  # DisplayLogic looks something like this:
  # question[['Payload']][['DisplayLogic']]$`0`$`1`[['Description']]
  # Determining the first and second indices within the DisplayLogic is the goal
  # of the operations used to define the dl_indices_1 and dl_indices_2.
  if ("DisplayLogic" %in% names(question[['Payload']])) {
    display_logic[[e]] <- "Question Display Logic:"
    e <- e+1
    dl_indices_1 <- suppressWarnings(which(!is.na(as.numeric(names(question[['Payload']][['DisplayLogic']])))))
    for (i in dl_indices_1) {
      dl_indices_2 <- suppressWarnings(which(!is.na(as.numeric(names(question[['Payload']][['DisplayLogic']][[i]])))))
      for (j in dl_indices_2) {
        if ("Description" %in% names(question[['Payload']][['DisplayLogic']][[i]][[j]])) {
          display_logic[[e]] <- clean_html(question[['Payload']][['DisplayLogic']][[i]][[j]][['Description']])
          e <- e+1
        }
      }
    }
  }

  # we do the same process for the
  # choices, but including a message before each display logic describing which
  # choice it corresponds to.
  if ("Choices" %in% names(question[['Payload']])) {
    choices_with_logic <- sapply(question[['Payload']][['Choices']], function(x) "DisplayLogic" %in% names(x))
    has_choice_logic <- any(choices_with_logic)
    choices_with_logic <- which(choices_with_logic)
    if (has_choice_logic) {
      display_logic[[e]] <- "Choice Display Logic:"
      e <- e+1
      for (i in choices_with_logic) {
        display_logic[[e]] <- paste0("Display Logic for ", question[['Payload']][['Choices']][[i]][['Display']], ":")
        e <- e+1
        dl_indices_1 <- suppressWarnings(which(!is.na(as.numeric(names(question[['Payload']][['Choices']][[i]][['DisplayLogic']])))))
        for (j in dl_indices_1) {
          dl_indices_2 <- suppressWarnings(which(!is.na(as.numeric(names(question[['Payload']][['Choices']][[i]][['DisplayLogic']][[j]])))))
          for (k in dl_indices_2) {
            if ("Description" %in% names(question[['Payload']][['Choices']][[i]][['DisplayLogic']][[j]][[k]])) {
              display_logic[[e]] <- clean_html(question[['Payload']][['Choices']][[i]][['DisplayLogic']][[j]][[k]][['Description']])
              e <- e+1
            }
          }
        }
      }
    }
  }

  # for the answers, we do the exact same as the choices.
  if ("Answers" %in% names(question[['Payload']])) {
    answers_with_logic <- sapply(question[['Payload']][['Answers']], function(x) "DisplayLogic" %in% names(x))
    has_answer_logic <- any(answers_with_logic)
    answers_with_logic <- which(answers_with_logic)
    if (has_answer_logic) {
      display_logic[[e]] <- "Answer Display Logic:"
      e <- e+1
      for (i in answers_with_logic) {
        display_logic[[e]] <- paste0("Display Logic for ", question[['Payload']][['Answers']][[i]][['Display']], ":")
        e <- e+1
        dl_indices_1 <- suppressWarnings(which(!is.na(as.numeric(names(question[['Payload']][['Answers']][[i]][['DisplayLogic']])))))
        for (j in dl_indices_1) {
          dl_indices_2 <- suppressWarnings(which(!is.na(as.numeric(names(question[['Payload']][['Answers']][[i]][['DisplayLogic']][[j]])))))
          for (k in dl_indices_2) {
            if ("Description" %in% names(question[['Payload']][['Answers']][[i]][['DisplayLogic']][[j]][[k]])) {
              display_logic[[e]] <- clean_html(question[['Payload']][['Answers']][[i]][['DisplayLogic']][[j]][[k]][['Description']])
              e <- e+1
            }
          }
        }
      }
    }
  }

  return(display_logic)
}


#' Get the Choice Text from the First Row of the Responses
#'
#' This function uses the first row of the response data from Qualtrics
#' to determine the choice text a response column corresponds to.
#'
#' @param response_column The name of a response column from the response set
#' @param original_first_row The first row of the original response set, with names
#' @param blocks A list of the survey blocks, with the questions included in them
#' @return The choice text corresponding to a response column
choice_text_from_response_column <- function(response_column, original_first_row, blocks) {

  # get the question's place in the blocks from the response column,
  # save the indices needed to refer to the question in the blocks list,
  # save the raw question text,
  # and clean it of HTML tags
  question_indices <- question_from_response_column(blocks, response_column)
  i <- question_indices[[1]]
  j <- question_indices[[2]]
  question_text <- blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionText']]
  question_text <- clean_html(question_text)

  # get the first-row-entry from the responses for the given response column,
  # count the number of dashes in the cleaned question text,
  # and count the number of dashes in the first-row-entry.
  # NOTE: counting the dashes in the question text is limited to the first 99
  # characters, since the question is cut off in the first row after 99
  # characters.
  first_row_entry <- as.character(original_first_row[response_column][[1]])
  stem_dashes <- gregexpr("-", substr(question_text, 1, 99))[[1]]
  stem_dash_n <- length(which(stem_dashes > 0))
  first_row_dashes <- gregexpr("-", first_row_entry)[[1]]
  first_row_dash_n <- length(which(first_row_dashes > 0))

  # if the number of dashes in the first-row-entry is the same as
  # the number of dashes in the question stem, then the choice text
  # for the response column can be set to blank.
  # if the number of dashes in the first-row-entry is greater
  # than the number of dashes in the question stem, then
  # the choice text for that response column should be set to
  # the text of the first-row-entry after the appropriate
  # number of dashes
  if (first_row_dash_n > stem_dash_n) {
    choice_text <- substr(first_row_entry, first_row_dashes[[stem_dash_n + 1]] + 1, nchar(first_row_entry))
  } else {
    choice_text <- ""
  }

  return(choice_text)
}


#' Split Respondents by a Response Column
#'
#' This function splits the respondents into separate respondent groups
#' based on the values in the specified response column. Then, for each
#' respondent group, the blocks with questions are duplicated. Each set of
#' blocks has a different responent group inserted into its questions, and
#' then each set of blocks is processed. The output is a list of blocks with
#' the results processed and inserted into each BlockElement.
#'
#'  @param response_column The response column that will be used to split the respondents
#'  @param headerrows the number of rows in the response csv before the response data starts
#'  @param already_loaded This can be set to TRUE to indicate that the survey and responses
#'  should be sourced from the global scope; in other words that the survey and its responses
#'  have are "already loaded."
#'
#'  @return A list of a list of blocks. The same question, but with different respondent groups,
#'  might look something like split_blocks[[1]][[1]][['BlockElements']][[1]] and
#'  split_blocks[[2]][[1]][['BlockElements']][[1]]. These refer to the first and second respondent
#'  groups, the first block, and the first block element.
split_respondents <- function(response_column, headerrows, already_loaded) {
  if (missing(headerrows)) {
    headerrows <- 3
  }
  if (missing(already_loaded)) {
    already_loaded <- FALSE
  }

  if (already_loaded != TRUE) {
    try(survey <<- ask_for_qsf())
    try(responses <<- ask_for_csv(headerrows = headerrows))
  }

  if (already_loaded == TRUE) {
    if (!exists("survey", where = -1)) {
      survey <- sample_survey
    } else {
      survey <- get("survey", envir=-1)
    }

    if (!exists("responses", where = -1)) {
      responses <- sample_responses
    } else {
      responses <- get("responses", envir=-1)
    }
  }

  # split the respondents by their responses to in the response_column
  split_responses <- split(responses, responses[response_column], drop=TRUE)

  # print the respondent levels to console and wait for input
  preview <- c(nrow(responses),
               table(factor(responses[[response_column]])))
  names(preview)[1] <- "Total"
  print(preview,
        row.names = FALSE)

  cat ("\nPress [enter] to continue")
  line <- readline()


  # process the blocks and questions as per usual
  blocks <- blocks_from_survey(survey)
  questions <- questions_from_survey(survey)
  questions <- remove_trash_questions(questions, blocks)
  questions <- clean_question_text(questions)
  questions <- human_readable_qtype(questions)
  blocks <- remove_trash_blocks(blocks)

  # insert the header into the blocks
  blocks[['header']] <- c(paste0("Survey Name: ",
                                 survey[['SurveyEntry']][['SurveyName']]),
                          paste0("Total Number of Original Respondents: ",
                                 nrow(responses)))

  # duplicate the blocks and questions once for every respondent group
  split_blocks <- rep(list(blocks), times = length(split_responses))
  split_questions <- rep(list(questions), times = length(split_responses))

  # for each of the respondent groups, insert the responses into the
  # questions for that respondent group, generate the question's results,
  # and then insert the questions into the blocks for that respondent group.
  for (i in 1:length(split_responses)) {
    split_questions[[i]] <- link_responses_to_questions(split_questions[[i]], split_responses[[i]])
    split_questions[[i]] <- generate_results(split_questions[[i]])
    split_blocks[[i]] <- questions_into_blocks(split_questions[[i]], split_blocks[[i]])
    split_blocks[[i]][['header']] <- c(split_blocks[[i]][['header']],
                                       paste0("Survey Respondents who had ",
                                              responses[[response_column]][[1]],
                                              " in the ",
                                              response_column,
                                              " column"),
                                       paste0("Number of Respondents in Respondent Group: ",
                                              nrow(split_responses[[i]])))
  }



  return(split_blocks)
}


blocks_header_to_html <- function(blocks) {
  return(c("<h4>",
           paste(blocks[['header']], collapse="<br>"),
           "</h4><br>"))
}

number_of_blocks <- function(blocks) {
  if (is.null(names(blocks))) {
    return(length(blocks))
  } else {
    as_ints <- sapply(names(blocks), function(x) {
      (!is.na(suppressWarnings(as.integer(x)))) || (x == "")
      })
    block_length <- length(which(as_ints))
    return(block_length)
  }
}
