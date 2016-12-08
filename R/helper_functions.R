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
    if ("RecodeValues" %in% names(question[['Payload']]) && choice %in% question[['Payload']][['RecodeValues']]) {
      recoded_value <- which(question[['Payload']][['RecodeValues']] == choice)
      recoded_value <- names(question[['Payload']][['RecodeValues']])[[as.integer(recoded_value)]]
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
        choice <- names(question[['Payload']][['RecodeValues']])[[recoded_value]]
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
#' @param already_loaded can be set to TRUE to indicate that get_setup should
#' try to get the survey, responses, and original_first_rows from the global scope
#' instead of asking the user for them. If they aren't there, then the function
#' will use the sample data included in the package.
get_setup <- function(
  headerrows,
  already_loaded,
  qsf_path,
  csv_path
  ) {
  # default to headerrows = 3
  if (missing(headerrows)) {
    headerrows <- 3
  }

  # default to already_loaded = FALSE
  if (missing(already_loaded)) {
    already_loaded <- FALSE
  }

  # ask the user for the CSV and the QSF if the
  if (already_loaded == FALSE) {
    if (missing(qsf_path)) {
      survey <- ask_for_qsf()
    } else {
      survey <- ask_for_qsf(qsf_path)
    }
    if (missing(csv_path)) {
      responses <- ask_for_csv(headerrows=headerrows)
    } else {
      responses <- ask_for_csv(csv_path, headerrows=headerrows)
    }
    original_first_rows <- as.data.frame(responses[[2]])
    responses <- as.data.frame(responses[[1]])
  }

  if (already_loaded == TRUE) {
    if (!exists("survey", where = -1)) {
      survey <- sample_survey
    } else {
      survey <- get("survey", envir=-1)
    }

    if (!exists("responses", where = -1) || !exists("original_first_rows", where = -1)) {
      responses <- sample_responses
      original_first_rows <<- sample_original_first_rows
    } else {
      responses <- get("responses", envir=-1)
      original_first_rows <- get("original_first_rows", envir=1)
    }
  }

  blocks <- blocks_from_survey(survey)
  questions <- questions_from_survey(survey)
  questions <- remove_trash_questions(questions, blocks)
  blocks <- remove_trash_blocks(blocks)
  questions_and_blocks <- split_side_by_sides(questions, blocks)
  questions <- questions_and_blocks[[1]]
  blocks <- questions_and_blocks[[2]]
  questions <- clean_question_text(questions)
  questions <- human_readable_qtype(questions)
  questions <- link_responses_to_questions(questions, responses, original_first_rows)
  questions <- generate_results(questions, original_first_rows)
  blocks <- questions_into_blocks(questions, blocks)

  # insert a header into the blocks
  blocks[['header']] <- c(paste0("Survey Name: ",
                                 survey[['SurveyEntry']][['SurveyName']]),
                          paste0("Number of Respondents: ",
                                 nrow(responses)))

  survey <<- survey
  responses <<- responses
  questions <<- questions
  blocks <<- blocks
  original_first_rows <<- original_first_rows

  if ( exists("survey", 1) &&
       exists("responses", 1) &&
       exists("questions", 1) &&
       exists("blocks", 1) &&
       exists("original_first_rows")
  ) {
    cat("The survey, responses, the response set's original_first_rows, questions, and blocks variables have all been made globally available in your R session.")
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


#' Get the Choice Text from the First Row of the Responses
#'
#' This function uses the first row of the response data from Qualtrics
#' to determine the choice text a response column corresponds to.
#'
#' @param response_column The name of a response column from the response set
#' @param original_first_row The first row of the original response set. If you have
#' the original_first_rows, you can use original_first_rows[1,].
#' @param blocks A list of the survey blocks, with the questions included in them
#' @return The choice text corresponding to a response column
choice_text_from_response_column <- function(response_column, original_first_row, blocks) {

  # get the question's place in the blocks from the response column,
  # save the indices needed to refer to the question in the blocks list,
  # save the raw question text,
  # and clean it of HTML tags
  question_indices <- question_from_response_column(blocks, response_column)
  if (is.null(question_indices)) return("")
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
  if (!response_column %in% colnames(original_first_row)) return("")
  first_row_entry <- enc2native(as.character(original_first_row[response_column][1,]))
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
    choice_text <- clean_html(choice_text)

  } else {
    choice_text <- ""
  }

  return(choice_text)
}


#' Block's Header to HTML
#'
#' Get an HTML Header for a list of survey blocks. The header
#' is created by either get_coded_questions_and_blocks or
#' by split_respondents.
#'
#' @param blocks A list of blocks with a 'header' inserted.
#' @return an HTML header for the survey
blocks_header_to_html <- function(blocks) {
  header <- c("<h4>",
           paste(blocks[['header']][1:2], collapse="<br>"))
  if (length(blocks[['header']]) > 2) {
    header <- c(header,
                "<br><br>",
                paste(blocks[['header']][3:length(blocks[['header']])], collapse="<br>"),
                "</h4><br>"
    )
  }
  header <- c(header,
              "</h4></br>")
  return(header)
}


#' Count the Number of Blocks
#'
#' Since the blocks list is used to transport some additional information
#' beyond the contents of the survey questions, this function is here to
#' help in counting how many valid question blocks there are.
#' Any real question blocks will be enumerated (aka numbered) in R, as opposed to the
#' content that's been added which will be named (with a string). This means that when
#' looking at the names of the blocks list, the integer values or the values which
#' have no name are the question blocks, and the values which have names are the
#' information added by the QualtricsTools functions. This function counts up the former.
#'
#' @param blocks A list of blocks
#' @return the number of question blocks
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

#' Generate a List of Questions from Those Contained in the Blocks
#'
#' This function iterates through the blocks and anything that has a DataExportTag
#' is added to a list of questions, and it returns that list of questions from
#' the blocks.
questions_from_blocks <- function(blocks) {
  questions <- list()
  e <- 1
  for (i in 1:length(blocks)) {
    if ('BlockElements' %in% names(blocks[[i]])) {
      for (j in 1:length(blocks[[i]][['BlockElements']])) {
        if ('Payload' %in% names(blocks[[i]][['BlockElements']][[j]]) &&
            'DataExportTag' %in% names(blocks[[i]][['BlockElements']][[j]][['Payload']])) {
          questions[[e]] <- blocks[[i]][['BlockElements']][[j]]
          e <- e + 1
        }
      }
    }
  }
  return(questions)
}

#' Get the Flow out of the Survey
#'
#' The 'Flow' is a list of Block IDs in the order that they are presented
#' in the survey as it is taken by a respondent. The flow list that is returned
#' from this function is used by functions like text_appendices_table and
#' tabelize_blocks to get the ordering of the survey in the preview correct.
flow_from_survey <- function(survey) {
  flow <- which(sapply(survey[['SurveyElements']], function(x) x[['Element']] == "FL"))
  flow <- sapply(survey[['SurveyElements']][[flow]][['Payload']][['Flow']], function(x)
    if ('ID' %in% names(x)) {
      x[['ID']]
    } else if ('Flow' %in% names(x)) {
      sapply(x[['Flow']], function(y) if ('ID' %in% names(y)) y[['ID']])
    })
  flow <- unlist(flow)
  return(flow)
}
