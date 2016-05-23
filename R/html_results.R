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
tabelize_blocks <- function(blocks) {
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


#' Create HTML Tables for the Text Entry Questions
#'
#' This function creates an HTML string with tables
#' for each of the text entry questions and their responses.
#' The appendix_lettering function inside this function
#' is to create lettering for the tables in the style
#' of "A, B, ..., Z, AA, ..., ZZ" and so forth. The html
#' tables are created by looping through the blocks
#' and their contained question block elements. For each
#' question, if the question is a Text Entry question or
#' any of the response columns contain "TEXT" then a table
#' is created for those responses.
#'
#' @param blocks A list of blocks with block elements replaced
#' by the question with its paired responses.
#'
#' @return an html string containing a title,
#' question text, and the text responses for each
#' text appendix.
text_appendices_table <- function(blocks) {
  appendix_lettering <- function(number) {
    if (number %in% 1:26) {
      return(LETTERS[[number]])
    } else if (number %in% 27:702) {
      first_digit <- (floor((number - 1) / 26))
      second_digit <- ((number - 1) %% 26) + 1
      first_letter <- LETTERS[[first_digit]]
      second_letter <- LETTERS[[second_digit]]
      return(paste0(first_letter, second_letter))
    }
  }

  tables <- list()
  e <- 0
  for (i in 1:length(blocks)) {
    if (length(blocks[[i]]$BlockElements) != 0) {
      for (j in 1:length(blocks[[i]]$BlockElements)) {
        if (!(is.null(blocks[[i]]$BlockElements[[j]]$Responses))) {

          text_columns <- which(sapply(colnames(blocks[[i]]$BlockElements[[j]]$Responses),
                                       function(x) grepl("TEXT", x)))

          if (blocks[[i]]$BlockElements[[j]]$Payload$QuestionType == "TE") {
            responses <- blocks[[i]]$BlockElements[[j]]$Responses
            responses <- as.data.frame(responses[!apply(responses, 1, function(x) any(x=="")),])
            responses <- as.data.frame(responses[!apply(responses, 1, function(x) any(x==-99)),])
            colnames(responses) <- colnames(blocks[[i]]$BlockElements[[j]]$Responses)
            if (length(as.list(responses)) > 0) {
              e <- e+1
              tables <- c(tables, paste0("Appendix ", appendix_lettering(e), ": <br>"))
              tables <- c(tables, capture.output(print(xtable::xtable(
                responses,
                caption=paste0(
                  blocks[[i]]$BlockElements[[j]]$Payload$QuestionTextClean,
                  "<br># of Respondents: ",
                  nrow(responses))
              ),
              type="html",
              html.table.attributes='class="data table table-bordered table-condensed"',
              caption.placement="top",
              include.rownames=FALSE)))

              tables <- c(tables, "<br>")
            }

          } else if (length(text_columns) > 0) {
            for (k in 1:length(text_columns)) {
              responses <- blocks[[i]]$BlockElements[[j]]$Responses[text_columns[[k]]]
              responses <- as.data.frame(responses[!apply(responses, 1, function(x) any(x=="")),])
              responses <- as.data.frame(responses[!apply(responses, 1, function(x) any(x==-99)),])
              colnames(responses) <- colnames(blocks[[i]]$BlockElements[[j]]$Responses[text_columns[[k]]])
              if (length(as.list(responses)) > 0) {
                e <- e+1

                tables <- c(tables, paste0("Appendix ", appendix_lettering(e), ": <br>"))
                tables <- c(tables, capture.output(print(xtable::xtable(
                  responses,
                  caption=paste0(
                    blocks[[i]]$BlockElements[[j]]$Payload$QuestionTextClean,
                    "<br># of Respondents: ",
                    nrow(responses))
                ),
                type="html",
                html.table.attributes='class="data table table-bordered table-condensed"',
                caption.placement="top",
                include.rownames=FALSE))
                )

                tables <- c(tables, "<br>")
              }

            }
          }
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
#' @inheritParams tabelize_blocks
#' @return A message stating for which questions could not have
#' results automatically generated.
uncodeable_questions_message <- function(questions) {
  uncodeable_questions <- which(sapply(questions, function(x)
    !("Table" %in% names(x)) &&
      (x$Payload$QuestionType != "TE") &&
      (x$Payload$QuestionType != "DB")))
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


#' Create a Question Dictionary
#'
#' @param blocks The blocks provided to this function must include questions inserted into
#' the BlockElements. Create the list of blocks from a survey with blocks_from_survey(),
#' and with questions on hand, insert them into the blocks with questions_into_blocks().
#' @return A data frame with a row for each question describing the question's details.
create_question_dictionary <- function(blocks) {

  list_of_rows_to_df <- function(data) {
    nCol <- max(vapply(data, length, 0))
    data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
    data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
    data.frame(data)
  }

  # create_entry creates the row for any individual
  # response with the following elements in it:
  # - The data export tag,
  # - "QuestionTextClean", the question text stripped of any HTML strings/entities,
  # - "QuestionTypeHuman", the human readable question type,
  # - "QuestionType", the qualtrics supplied question type,
  # - "Selector", the qualtrics defined question selector
  create_entry <- function(i, j) {
    return(c(
      # data export tag
      blocks[[i]]$BlockElements[[j]]$Payload$DataExportTag,
      # question text
      blocks[[i]]$BlockElements[[j]]$Payload$QuestionTextClean,
      # human readable question type
      blocks[[i]]$BlockElements[[j]]$Payload$QuestionTypeHuman,
      # qualtrics question type
      blocks[[i]]$BlockElements[[j]]$Payload$QuestionType,
      # qualtrics question selector
      blocks[[i]]$BlockElements[[j]]$Payload$Selector
    ))
  }

  ### loop through each block, then each question,
  # then of the columns of the responses,
  # then each of the entries in each of the response columns,
  # and create an entry using "create_entry"
  entries <- list()
  e <- 0
  for (i in 1:length(blocks)) {
    if (length(blocks[[i]]$BlockElements) != 0) {
      for (j in 1:length(blocks[[i]]$BlockElements)) {
        e <- e + 1
        if (is.null(blocks[[i]]$BlockElements[[j]]$Payload$SubSelector)) {
          blocks[[i]]$BlockElements[[j]]$Payload$SubSelector <- ""
        }
        entries[[e]] <- create_entry(i, j)
      }
    }
  }

  # entries are turned into a data frame with the specified headers
  question_dictionary <- list_of_rows_to_df(entries)
  colnames(question_dictionary) <- c("DataExportTag",
                                     "QuestionText", "QuestionType", "QuestionType2",
                                     "QuestionType3")
  return(question_dictionary)
}
