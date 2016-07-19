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
    headerrows <- 3
  }

  if (missing(already_loaded)) {
    already_loaded <- FALSE
  }

  if (already_loaded == FALSE) {
    try(survey <- ask_for_qsf())
    try(responses <- ask_for_csv(headerrows=headerrows))
    original_first_rows <- as.data.frame(responses[[2]])
    responses <- as.data.frame(responses[[1]])
    cat(length(responses))
    cat('\n')
  }

  if (already_loaded == TRUE) {
    if (!exists("survey", where = -1)) {
      survey <- sample_survey
    } else {
      survey <- get("survey", envir=-1)
    }

    if (!exists("responses", where = -1)) {
      responses <- sample_responses
      original_first_rows <<- sample_original_first_rows
    } else {
      responses <- get("responses", envir=-1)
      original_first_rows <- get("original_first_rows", envir=1)
    }
  }

  try(blocks <- blocks_from_survey(survey))
  try(questions <- questions_from_survey(survey))
  try(questions <- remove_trash_questions(questions, blocks))
  try(questions <- clean_question_text(questions))
  try(questions <- human_readable_qtype(questions))
  try(blocks <- remove_trash_blocks(blocks))
  try(questions_and_blocks <- split_side_by_sides(questions, blocks))
  questions <- questions_and_blocks[[1]]
  blocks <- questions_and_blocks[[2]]
  cat(length(responses))
  cat('\n')
  questions <- link_responses_to_questions(questions, responses, original_first_rows)
  cat(length(responses))
  cat('\n')

  try(questions <- generate_results(questions))
  try(blocks <- questions_into_blocks(questions, blocks))

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
#' @param original_first_row The first row of the original response set, with names
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
  return(c("<h4>",
           paste(blocks[['header']], collapse="<br>"),
           "</h4><br>"))
}


#' Count the Number of Blocks
#'
#' Since the blocks list is used to transport some additional information
#' beyond the contents of the survey questions, this function is here to
#' help in counting how many valid question blocks there are.
#' Any real question blocks will be enumerated in R, as opposed to the
#' content that's been added which will be named. This means that when
#' looking at the names of the blocks list, the integer values or the values which
#' have no name are the question blocks, and the values which have names are the
#' added information. This function counts up the former.
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
