#' Create a List of HTML Versions of the Results Tables
#'
#' @param questions A list of questions with the relevant results tables
#' stored as data frames under the questions[[i]][['Table']] element. Create
#' such a list of questions by using generate_results function
#'
#' @return A list of HTML results tables for each question
tabelize_blocks <- function(blocks, flow, include_block_headers) {
  # include_block_headers lets you choose whether or not each block
  # will get its own header in the output
  if (missing(include_block_headers)) include_block_headers <- TRUE

  # all the html tables will be saved into the tables list.
  tables <- list()
  tables[[1]] <- "<br>"
  options(stringsAsFactors = FALSE)

  # determine the order of the block indices that we will use to
  # go through the blocks
  if (!missing(flow)) {
    # if the survey flow was provided, then use it to figure out
    # the block_ordering
    block_ordering <- list()
    for (h in flow) {
      matched_block <- sapply(blocks, function(x) {
        if ('ID' %in% names(x)) {
          return(identical(x[['ID']], h))
        } else return(FALSE)
      })
      if (table(matched_block)['TRUE'] == 1) {
        block_ordering <- c(block_ordering, which(matched_block))
      }
    }
  } else {
    # if no flow was provided, then just go in order through all the blocks
    block_ordering <- 1:length(blocks)
  }

  for (i in block_ordering) {
    if ('BlockElements' %in% names(blocks[[i]])) {
      if (include_block_headers) tables <- c(tables, paste0("<h5>", blocks[[i]][['Description']], "</h5><br>"))
      if (length(blocks[[i]][['BlockElements']]) != 0) {
        for (j in 1:length(blocks[[i]][['BlockElements']])) {
          # don't get any questions that are supposed to be skipped
          if (!'qtSkip' %in% names(blocks[[i]][['BlockElements']][[j]]) ||
              blocks[[i]][['BlockElements']][[j]][['qtSkip']] != TRUE) {
            #if a question isn't a descriptive block, insert the question description for it
            if (blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionType']] != "DB") {
              tables <- c(tables, question_description(blocks[[i]][['BlockElements']][[j]]))
            }
          }
        }
      }
    }
  }
  return(unlist(lapply(tables, paste)))
}


#' Create Question Description and Results Table Entry
#'
#' @param question A qualtrics survey question
#'
#' @return A list of HTML with a description table for the question,
#' and either the generated results, or a note saying why there are
#' no results.
question_description <- function(question) {
  tables <- list()

  # get display logic for the question
  display_logic <- display_logic_from_question(question)

  # if the display logic is too long, write a note saying
  # that there is complex display logic, and to refer to the
  # Display Logic output.
  if (length(display_logic) > 3) {
    display_logic <- list("This question contains complex display logic. Please refer to the Display Logic panel.")
  }

  # the question header is the data export tag, the question text (stripped of html),
  # and then display logic.
  description <- c(question[['Payload']][['DataExportTag']],
                   question[['Payload']][['QuestionTextClean']],
                   display_logic)

  # if there's no results table, append a message to the question
  # description accordingly.
  # - text entry -> "Question XX is a text entry question. See Appendix."
  # - otherwise -> "The results table for Question XX could not be
  #                 automatically processed."
  if ("Payload" %in% names(question)) {
    if (!"Table" %in% names(question) && question[['Payload']][['QuestionType']] == "TE") {
      description <- c(description, paste0(
        "Question ",
        question[['Payload']][['DataExportTag']],
        " is a text entry question. See Appendix."))
    } else if (!"Table" %in% names(question) &&
               !all(grepl("TEXT", names(question[['Responses']])))) {
      description <- c(description, paste0(
        "The results table for Question ",
        question[['Payload']][['DataExportTag']],
        " could not be automatically processed."))
    }
  }

  # if the question isn't a text entry question, but has
  # columns in which "TEXT" appears, print one of the following
  # messages, depending on how many text entry columns are in the
  # question:
  #  1 - "This question has a text entry component. See Appendix."
  # >1 - "This question has multiple text entry components. See Appendices."
  if ("Payload" %in% names(question)) {
    if (question[['Payload']][['QuestionType']] != "TE") {
      if (length(grep("TEXT", names(question[['Responses']]))) == 1) {
        description <- c(description, paste0(
          "This question has a text entry component. See Appendix."))
      } else if (length(grep("TEXT", names(question[['Responses']]))) > 1) {
        description <- c(description, paste0(
          "This question has multiple text entry components. See Appendices."))
      }
    }
  }

  # Check for notes, and include them where necessary
  if ('qtNotes' %in% names(question)) {
    description <- c(description, question[['qtNotes']])
  }

  # reshape the data into a data frame
  question_header <- do.call(rbind.data.frame,
                             t(description))

  # append the question description printed as an html table
  tables = c(tables, capture.output(
    print(xtable::xtable(question_header),
          include.colnames=FALSE,
          type="html",
          caption.placement="top",
          html.table.attributes='class="question_description data table table-bordered table-condensed"',
          include.rownames=FALSE)))
  tables <- c(tables, "&nbsp;")

  # if the question has a results table, append it as an html table.
  if ("Table" %in% names(question)) {
    tables = c(tables, capture.output(
      print(xtable::xtable(question[['Table']]),
            type="html",
            html.table.attributes='class="data table table-bordered table-condensed"',
            include.rownames=FALSE)))
  }

  tables = c(tables, "<br><br>")
  return(tables)
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
#' @param original_first_row This is the original first row of the
#' response set. If you have the original_first_rows, you can
#' pass original_first_rows[1,] to
#'
#' @return an html string containing a title,
#' question text, and the text responses for each
#' text appendix.
text_appendices_table <- function(blocks, original_first_row, flow) {
  options(stringsAsFactors = FALSE)

  # determine the order of the block indices that we will use to
  # go through the blocks
  if (!missing(flow)) {
    # if the survey flow was provided, then use it to figure out
    # the block_ordering
    block_ordering <- list()
    for (h in flow) {
      matched_block <- sapply(blocks, function(x) {
        if ('ID' %in% names(x)) {
          return(x[['ID']] == h)
        } else return(FALSE)
      })
      if (table(matched_block)['TRUE'] == 1) {
        block_ordering <- c(block_ordering, which(matched_block))
      }
    }
  } else {
    # if no flow was provided, then just go in order through all the blocks
    block_ordering <- 1:length(blocks)
  }

  # start with an empty list and an index at 0
  # the index e is for creating the appendix names,
  # by use of the above appendix_lettering function.
  # tables is for storing the HTML for all of the
  # text appendices tables.
  tables <- list()
  e <- 1

  # loop through every response column that is EITHER
  # 1) the only response column to a TextEntry question, or
  # 2) a response column containing the string "TEXT".
  for (i in block_ordering) {
    if ('BlockElements' %in% names(blocks[[i]])) {
      tables <- c(tables, paste0("<h5>", blocks[[i]][['Description']], "</h5><br>"))
      for (j in 1:length(blocks[[i]][['BlockElements']])) {
        if (!"qtSkip" %in% names(blocks[[i]][['BlockElements']][[j]]) ||
            blocks[[i]][['BlockElements']][[j]][['qtSkip']] != TRUE) {

          # Table Coded Comments
          if ('CodedComments' %in% names(blocks[[i]][['BlockElements']][[j]])) {
            for (k in 1:length(blocks[[i]][['BlockElements']][[j]][['CodedComments']])) {
              tables <- c(tables,
                          table_coded_comments(blocks[[i]][['BlockElements']][[j]],
                                               k,
                                               e,
                                               blocks,
                                               original_first_row))
            }
          }

          # Skip verbatim comments for flagged questions
          if (!"verbatimSkip" %in% names(blocks[[i]][['BlockElements']][[j]]) ||
              blocks[[i]][['BlockElements']][[j]][['verbatimSkip']] != TRUE) {
            # Make sure the question has responses
            if ('Responses' %in% names(blocks[[i]][['BlockElements']][[j]]) &&
                ncol(blocks[[i]][['BlockElements']][[j]][['Responses']]) > 0) {

              # which columns are text entry data?
              text_columns <- which(sapply(colnames(blocks[[i]][['BlockElements']][[j]][['Responses']]),
                                           function(x) grepl("TEXT", x)))

              # If the question is a TextEntry question
              if (blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionType']] == "TE") {
                # remove all the empty and -99 responses
                responses <- blocks[[i]][['BlockElements']][[j]][['Responses']]
                responses <- as.data.frame(responses[!apply(responses, 1, function(x) all(x=="")),])
                responses <- as.data.frame(responses[!apply(responses, 1, function(x) all(x==-99)),])
                colnames(responses) <- colnames(blocks[[i]][['BlockElements']][[j]][['Responses']])

                # Table No Respondents
                if (nrow(responses) == 0) {
                  tables <- c(tables, table_no_respondents(blocks[[i]][['BlockElements']][[j]], e))
                  e <- e+1
                  next
                }

                # Table Text Entry Appendices
                if (length(as.list(responses)) > 0) {
                  tables <- c(tables,
                              table_text_entry(blocks[[i]][['BlockElements']][[j]],
                                               responses,
                                               e,
                                               blocks,
                                               original_first_row))
                  e <- e+1
                }

              } else if (length(text_columns) > 0) {
                for (k in 1:length(text_columns)) {

                  # Write the choice text
                  if (!missing(original_first_row)) {
                    # if the original_first_row is available, use it to construct the question text
                    # with the corresponding choice text appended.
                    # otherwise, just use the question text.
                    response_column <- names(blocks[[i]][['BlockElements']][[j]][['Responses']])[text_columns[[k]]]
                    choice_text <- choice_text_from_response_column(response_column, original_first_row, blocks)
                    if (choice_text != "") {
                      question_text <- paste0(blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionTextClean']],
                                              "-",
                                              choice_text)
                    } else {
                      question_text <- blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionTextClean']]
                    }
                  }

                  # remove the empty and -99 responses
                  responses <- blocks[[i]][['BlockElements']][[j]][['Responses']][text_columns[[k]]]
                  responses <- as.data.frame(responses[!apply(responses, 1, function(x) all(x=="")),])
                  responses <- as.data.frame(responses[!apply(responses, 1, function(x) all(x==-99)),])
                  colnames(responses) <- colnames(blocks[[i]][['BlockElements']][[j]][['Responses']][text_columns[[k]]])

                  # Table No Respondents Questions
                  if (nrow(responses) == 0) {
                    tables <- c(tables, table_no_respondents(blocks[[i]][['BlockElements']][[j]], e))
                    e <- e+1
                    next
                  }

                  # Skip Single Answer questions with too many text entry components to table correctly
                  if (is_mc_single_answer(blocks[[i]][['BlockElements']][[j]]) &&
                      length(which(sapply(blocks[[i]][['BlockElements']][[j]][['Payload']][['Choices']], function(x) 'TextEntry' %in% names(x) && x[['TextEntry']] == "true"))) > 1) {
                    question_message <- list()
                    question_message <- c(blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionTextClean']],
                                        "",
                                        "This question could not be processed automatically because the CSV data does not separate the responses for each text entry component of this question.")
                    question_message <- as.data.frame(question_message)
                    colnames(question_message)[1] <- blocks[[i]][['BlockElements']][[j]][['Payload']][['DataExportTag']]
                    tables <- list()
                    tables <- c(tables, capture.output(print(xtable::xtable(question_message),
                                                             type="html",
                                                             html.table.attributes='class="text_appendices data table table-bordered table-condensed"',
                                                             include.rownames=FALSE)))
                    tables <- c(tables, "<br>")
                    next
                  }

                  # Table Non-Text Entry Questions
                  tables <- c(tables, table_non_text_entry(
                    blocks[[i]][['BlockElements']][[j]],
                    responses,
                    e))
                  e <- e+1
                }
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
  # get all the questions that don't have tables, and
  # aren't a text entry question or a descriptive box.
  uncodeable_questions <- which(sapply(questions, function(x)
    !("Table" %in% names(x)) &&
      (x[['Payload']][['QuestionType']] != "TE") &&
      (x[['Payload']][['QuestionType']] != "DB") &&
      (x[['Payload']][['Selector']] != "TE")))

  # get the data export tags of the uncodeable questions
  uncodeable_questions <- sapply(uncodeable_questions, function(x)
    questions[[x]][['Payload']][['DataExportTag']])

  # write the message (ex. "The following questions could not be
  # automatically coded: Q1, Q2, ...")
  uncodeable_message <- ""
  if (length(uncodeable_questions) > 0) {
    uncodeable_questions <- paste(uncodeable_questions, collapse=", ")
    uncodeable_message <- sprintf("The following questions could not be automatically processed: %s",
                                  uncodeable_questions)
  } else {
    uncodeable_message <- "All questions were successfully processed!"
  }
  uncodeable_message <- paste0("<b>", uncodeable_message, "</b>")
  return(uncodeable_message)
}

#' Generate Tables for Each Question with Display Logic
#'
#' This function loops through every block, and checks if
#' each block element has display logic (and is not a descriptive box).
#' The descriptive boxes aren't included.
#' If it has display logic, it's inserted into the tables returned by
#' this function.
#'
#' @param blocks Survey blocks with questions substituted for the blockelements
#'
#' @return a list of html tables detailing the display logic for each question
#' containing display logic.
tabelize_display_logic <- function(blocks) {
  # all the html tables will be saved into the tables list.
  tables <- list()
  options(stringsAsFactors = FALSE)
  for (i in 1:number_of_blocks(blocks)) {
    if ('BlockElements' %in% names(blocks[[i]])) {
      for (j in 1:length(blocks[[i]][['BlockElements']])) {


        # if the display logic isn't trivial, include it.
        # each table should have the structure:
        #   - Data Export Tag
        #   - Question Text (stripped of HTML)
        #   - Display logic ...
        display_logic <- display_logic_from_question(blocks[[i]][['BlockElements']][[j]])
        if ("SkipLogic" %in% names(blocks[[i]][['BlockElements']][[j]][['Payload']])) {
          display_logic <- c(display_logic, "Skip Logic:")
          for (k in 1:length(blocks[[i]][['BlockElements']][[j]][['Payload']][['SkipLogic']])) {
            display_logic <-
              c(display_logic,
                clean_html(blocks[[i]][['BlockElements']][[j]][['Payload']][['SkipLogic']][[k]][['Description']]))
          }
        }
        if (length(display_logic) > 1) {
          display_logic <- do.call(rbind.data.frame, t(c(
            blocks[[i]][['BlockElements']][[j]][['Payload']][['DataExportTag']],
            blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionTextClean']],
            display_logic
          )))
          tables = c(tables, capture.output(print(xtable::xtable(display_logic),
                                                  include.colnames=FALSE,
                                                  type="html",
                                                  html.table.attributes='class="survey_logic data table table-bordered table-condensed"',
                                                  include.rownames=FALSE
          )))

          tables <- c(tables, "<br>")
        }
      }
    }
  }
  return(unlist(lapply(tables, paste)))
}

# appendix_lettering takes a number
# and returns the corresponding lettered index.
# examples:
# 1 -> A
# 2 -> B
# 27 -> AA
# 29 -> AC
# ... and so forth.
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


# CODED COMMENTS:
table_coded_comments <- function(question, cc_index, appendix_e, blocks, original_first_row) {
  response_column <- question[['CodedComments']][[cc_index]][[1]]
  choice_text <- choice_text_from_response_column(response_column, original_first_row, blocks)
  if (choice_text != "") {
    question_text <- paste0(question[['Payload']][['QuestionTextClean']],
                            "-",
                            choice_text)
  } else {
    question_text <- question[['Payload']][['QuestionTextClean']]
  }
  text_appendix_header <- c(paste0("Appendix ", appendix_lettering(appendix_e)),
                            question_text,
                            "Coded Comments",
                            "")

  text_appendix_header <- as.data.frame(text_appendix_header)
  text_appendix_header <- do.call("cbind", replicate(2, text_appendix_header, simplify=FALSE))
  coded_comment_names <- c(paste0(question[['Payload']][['DataExportTag']]),
                           paste0(question[['Payload']][['DataExportTag']], " "))
  colnames(text_appendix_header) <- coded_comment_names
  colnames(question[['CodedComments']][[cc_index]][[2]]) <- coded_comment_names
  response_n <- t(as.data.frame(c("Responses", "N")))
  colnames(response_n) <- coded_comment_names
  coded_comments_table <- rbind(response_n, question[['CodedComments']][[cc_index]][[2]])
  text_appendix <- rbind(text_appendix_header, coded_comments_table)
  tables <- list()
  tables <- c(tables, capture.output(print(xtable::xtable(text_appendix),
                                           type="html",
                                           html.table.attributes='class="text_appendices data table table-bordered table-condensed"',
                                           include.rownames=FALSE)))

  tables <- c(tables, "<br>")
  return(tables)
}


# NO RESPONDENTS:
table_no_respondents <- function(question, appendix_e) {

  No_Respondents <- c(paste0("Appendix ", appendix_lettering(appendix_e)),
                      question[['Payload']][['QuestionTextClean']],
                      "Verbatim responses -- these have not been edited in any way.",
                      "",
                      "No respondents answered this question")
  No_Respondents <- as.data.frame(No_Respondents)
  colnames(No_Respondents)[1] <- question[['Payload']][['DataExportTag']]
  tables <- list()
  tables <- c(tables, capture.output(print(xtable::xtable(No_Respondents),
                                           type="html",
                                           html.table.attributes='class="text_appendices data table table-bordered table-condensed"',
                                           include.rownames=FALSE)))
  tables <- c(tables, "<br>")
  return(tables)
}


# TEXT ENTRY QUESTIONS:
table_text_entry <- function(question, responses, appendix_e, blocks, original_first_row) {
  response_n <- paste0("Responses: (", nrow(responses), ")")

  # generate the headers for each column
  text_appendix_header <- list()
  for (l in 1:ncol(responses)) {
    choice_text <- choice_text_from_response_column(colnames(responses)[[l]], original_first_row, blocks)
    if (choice_text != "") {
      question_text <- paste0(question[['Payload']][['QuestionTextClean']],
                              "-",
                              choice_text)
    } else {
      question_text <- question[['Payload']][['QuestionTextClean']]
    }
    text_appendix_header[[l]] <- c(paste0("Appendix ", appendix_lettering(appendix_e)),
                                   question_text,
                                   "Verbatim responses -- these have not been edited in any way.",
                                   "",
                                   response_n)
  }
  text_appendix_header <- do.call(cbind.data.frame, text_appendix_header)

  # set colnames as response column names
  colnames(text_appendix_header) <- colnames(responses)

  # bind the header and responses together to make the text appendix
  text_appendix <- rbind(text_appendix_header,responses)

  tables <- list()
  # turn the text appendix into an html table, and add it to the tables list
  tables <- c(tables, capture.output(print(xtable::xtable(text_appendix),
                                           type="html",
                                           html.table.attributes='class="text_appendices data table table-bordered table-condensed"',
                                           include.rownames=FALSE)))
  tables <- c(tables, "<br>")
  return(tables)
}

# NON TEXT-ENTRY TABLES:
table_non_text_entry <- function(question, responses, appendix_e){
  response_n <- paste0("Responses: (", nrow(responses), ")")

  # generate the header for the text appendix
  text_appendix_header <- c(paste0("Appendix ", appendix_lettering(appendix_e)),
                            question[['Payload']][['QuestionTextClean']],
                            "Verbatim responses -- these have not been edited in any way.",
                            "",
                            response_n)

  text_appendix_header <- as.data.frame(text_appendix_header)

  # repeat the header for each response column, and
  # use the responses' column names
  if (ncol(responses) > 1) for (l in 1:(ncol(responses)-1)) text_appendix_header <- cbind(text_appendix_header, text_appendix_header[,1])
  colnames(text_appendix_header) <- colnames(responses)

  # bind the header and responses together to make the text appendix
  text_appendix <- rbind(text_appendix_header, responses)

  tables <- list()
  # turn the text appendix into an html table, and add it to the tables list
  tables <- c(tables, capture.output(print(xtable::xtable(text_appendix),
                                           type="html",
                                           html.table.attributes='class="text_appendices data table table-bordered table-condensed"',
                                           include.rownames=FALSE)))
  tables <- c(tables, "<br>")
  return(tables)
}
