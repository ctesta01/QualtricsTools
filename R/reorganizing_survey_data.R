#' Get Questions (with Responses) and Blocks (with Questions)
#'
#' This function returns a list with two elements, where
#' the first element is questions with their responses
#' and results-tables listed in them, and secondly
#' the blocks of the survey, with questions listed in
#' each block as BlockElements.
#'
#' @param survey A qualtrics survey, uploaded from a qsf/json file
#' @param responses Qualtrics responses to a survey, uploaded from csv as a data frame
#'
#' @return a list with two elements, the first being the survey questions,
#' and the second being the survey blocks
get_coded_questions_and_blocks <- function(survey, responses) {
  # select the block elements from the survey
  blocks <- blocks_from_survey(survey)

  # select the questions from the survey
  questions <- questions_from_survey(survey)

  # remove the questions that were found in the trash block
  questions <- remove_trash_questions(questions, blocks)

  # clean the question text of HTML and CSS tags
  questions <- clean_question_text(questions)

  # categorize each question's Response Type
  # (Single Answer, Multiple Answer,
  #  Text Entry, Rank Order)
  questions <- human_readable_qtype(questions)

  # remove the trash block from the blocks
  blocks <- remove_trash_blocks(blocks)

  # split side by side questions into their component questions
  questions_and_blocks <- split_side_by_sides(questions, blocks)
  questions <- questions_and_blocks[[1]]
  blocks <- questions_and_blocks[[2]]

  # insert the response columns into their corresponding
  # question under question[['Responses']]
  questions <- link_responses_to_questions(questions, responses)

  # generate each question's results table and insert it
  # in question[['Table']]
  questions <- generate_results(questions)

  # insert the questions into the blocks
  blocks <- questions_into_blocks(questions, blocks)

  # insert the header into the blocks
  blocks[['header']] <- c(paste0("Survey Name: ",
                                 survey[['SurveyEntry']][['SurveyName']]),
                          paste0("Number of Respondents: ",
                                 nrow(responses)))

  # return questions and blocks as a list of 2 elements
  questions_and_blocks <- list()
  questions_and_blocks[['questions']] <- questions
  questions_and_blocks[['blocks']] <- blocks
  return(questions_and_blocks)
}


#' Generate a List of Survey Blocks
#'
#' This function takes the survey blocks out of the survey
#' and turns them into their own list for later use.
#' Additionally, the block element can be further reduced to include
#' only the payload of the survey blocks.
#'
#' @param survey This should be a Qualtrics survey in the form of a list
#' imported from the JSON-formatted QSF file.
#'
#' @return The blocks element returned is a list of blocks containing for
#' each a type, description, ID, and BlockElements (which contains the
#' list of questions included in a given block).
blocks_from_survey <- function(survey) {
    blocks <- Filter(function(x) x[['Element']] == "BL", survey[['SurveyElements']])
    blocks <- blocks[[1]][['Payload']]
    return(blocks)
}

#' Generate a List of Questions
#'
#' This function takes the questions out of the survey and
#' turns them into their own list for later use.
#' Each question is an element of the returned list, and
#' each element has its own list as its content. Most questions
#' include things like "SurveyID", "Element", "PrimaryAttribute",
#' "SecondaryAttribute", and a "Payload" which contains
#' most of the information about the question.
#'
#' @inheritParams blocks_from_survey
#' @return A list of questions from the uploaded QSF file
questions_from_survey <- function(survey) {
    questions <- survey[['SurveyElements']]
    for (i in length(questions):1) {
        if (questions[[i]][['Element']] != "SQ") {
            questions[[i]] <- NULL
        }
    }
    return(questions)
}

#' Remove Questions from the Trash Block
#'
#' This function removes any questions from a list of questions
#' that are listed in the Trash block.
#'
#' @param questions A list of questions from a Qualtrics survey
#' @param blocks A list of blocks from a Qualtrics survey
#' @return The list of questions returned is the list of questions
#' provided except without any questions listed in the Trash
#' block of the blocks list provided.
remove_trash_questions <- function(questions, blocks) {
    # select the trash block
    trash <- Filter(function(x) x[['Type']] == "Trash", blocks)

    # retrieve the trash questions
    trash_questions <- list()
    for (i in trash[[1]][['BlockElements']]) {
        trash_questions <- c(i[['QuestionID']], trash_questions)
    }

    # remove the questions that were found among the
    # trash questions
    delete_if_in_trash <- function(x) {
        if (x[['Payload']][['QuestionID']] %in% trash_questions) {
            return(NULL)
        } else {
            return(x)
        }
    }
    questions <- lapply(questions, delete_if_in_trash)
    questions <- Filter(Negate(function(x) is.null(unlist(x))), questions)
    return(questions)
}

#' Remove the Trash Block from the list of Blocks
#'
#' This function finds among the trash blocks which has its [['Type']] value
#' set to "Trash" and then removes it from the blocks list. The iteration
#' in this function is backwards because it assigns NULL to any list
#' items which need to be removed. Therefore, if it assigns NULL
#' to a value and then moves up, it will not only skip questions as the
#' higher up questions move downward as questions are deleted, but
#' the total length of the list will be changing as it gets closer to the
#' end of the list.
#'
#' @inheritParams remove_trash_questions
#' @return The list of blocks is returned without any Trash blocks
remove_trash_blocks <- function(blocks) {
  for (i in number_of_blocks(blocks):1) {
    if ('Type' %in% names(blocks[[i]])) {
      if (blocks[[i]][['Type']] == "Trash") {
        blocks[[i]] <- NULL
        break
      }
    }
    if ('BlockElements' %in% names(blocks[[i]])) {
      if (length(blocks[[i]][['BlockElements']]) != 0) {
        for (j in length(blocks[[i]][['BlockElements']]):1) {
          if(blocks[[i]][['BlockElements']][[j]][['Type']] != "Question") {
            blocks[[i]][['BlockElements']][[j]] <- NULL
          }
        }
      }
    }
  }
  return(blocks)
}


#' Link Responses to Questions
#'
#' The columns of the response data must be matched up to their corresponding
#' questions and question-parts in order to analyze them. To do so,
#' each question is looped through, determining the DataExportTag of the question,
#' and the ChoiceDataExportTags of the question. The DataExportTag might look
#' like a variable name that has been set by the user in Qualtrics, or an
#' automatically generated name like "QID1" or "QID1.1". The columns
#' with names automatically generated by Qualtrics usually are of a form
#' starting with the DataExportTag, followed by an underscore, and then more details.
#' Sometimes they are exactly the DataExportTag without any modification. Lastly,
#' if the user sets their own variable naming in Qualtrics, then the question
#' contains in its [['Payload']] the [['ChoiceDataExportTags']] list, which contains
#' these user defined variables. This function goes through each question,
#' determines the DataExportTag and ChoiceDataExportTags, and uses each to select
#' the matching columns of the responses. Those are then inserted into that specific
#' question under [['Responses']], and the whole questions list is returned. One small
#' caveat is that if the column name starts with an integer, then R
#' will prepend it with an "X", so there is a helper function included here to include
#' those columns with prepended "X"s as well.
#'
#' @param questions A list of questions selected from a Qualtrics survey
#' @param responses A data frame of responses from a Qualtrics survey
#'
#' @return The updated list of questions, each including its relevant response columns
#' as a data frame stored in [['Responses']].
link_responses_to_questions <- function (questions, responses) {
    for (i in 1:length(questions)) {
        # create a string with the data export tag and an underscore
        # create a string with the data export tag and a period
        export_tag_with_underscore <- paste0( questions[[i]][['Payload']][['DataExportTag']], "_" )
        export_tag_with_period <- paste0( questions[[i]][['Payload']][['DataExportTag']], "." )
        export_tag_with_hashtag <- paste0( questions[[i]][['Payload']][['DataExportTag']], "#" )

        # there's also the possibility that a response column starts
        # with a choice data export tag from a question.
        # this unlists the choice data export tags and creates
        # a list of response columns that start with a choice data export tag.
        starts_with_choice_export_tags <- vector('integer')
        if ("ChoiceDataExportTags" %in% names(questions[[i]][['Payload']])) {
          choice_export_tags <- unlist(questions[[i]][['Payload']][['ChoiceDataExportTags']])
          choice_export_tags <- sapply(choice_export_tags, function(x) gsub("-", "_", x))
          for (j in choice_export_tags) {
            starts_with_choice_export_tags <- c(starts_with_choice_export_tags,
                                                which(gdata::startsWith(names(responses), j)))
          }
        }

        # the response columns that match a question are the ones that:
        # - start with a data export tag followed by a underscore,
        # - start with a data export tag followed by a period,
        # - match the data export tag exactly,
        # - start with a choice data export tag.
        # take those matching response columns and join them to the question under [['Responses']]
        matching_responses <- c(which(gdata::startsWith(names(responses), export_tag_with_underscore)),
                                which(gdata::startsWith(names(responses), export_tag_with_period)),
                                which(gdata::startsWith(names(responses), export_tag_with_hashtag)),
                                which(names(responses) == questions[[i]][['Payload']][['DataExportTag']]),
                                starts_with_choice_export_tags)
        questions[[i]][['Responses']] <- as.data.frame(responses[unique(matching_responses)])
    }
    questions
}


#' Organize the Questions into their Survey Blocks
#'
#' This function takes a blocks list (create this with blocks_from_survey and
#' then remove_trash_blocks), inserts the questions from a survey
#' appropriately into the blocks element, and then returns the
#' blocks list including the questions as elements in the blocks[[i]][['BlockElements']].
#'
#' @inheritParams remove_trash_questions
#' @return a list of blocks including questions under blocks[[i]][['BlockElements']]
#' for each index i.
questions_into_blocks <- function(questions, blocks) {
  for (i in 1:number_of_blocks(blocks)) {
    # loop through each block, and in each block, loop through the BlockElements
    if (length(blocks[[i]][['BlockElements']]) != 0) {
      for (j in 1:length(blocks[[i]][['BlockElements']])) {

        # create matching_question as a list of indices of questions which
        # have the corresponding QuestionID
        matching_question <- which(sapply(questions,
                                    function(x) isTRUE(x[['Payload']][['QuestionID']] ==
                                      blocks[[i]][['BlockElements']][[j]][['QuestionID']])))

        # if matching_question is a list of length 1 then we've matched the
        # question uniquely and can replace the BlockElement with the actual question
        if (length(matching_question) == 1) {
          blocks[[i]][['BlockElements']][[j]] <- questions[[matching_question]]
        }
      }
    }
  }
  return(blocks)
}


#' Create Cleaned Question Text
#'
#' This function loops through every question and applies the clean_html function to
#' the QuestionText and then saves the cleaned output to QuestionTextClean.
#'
#' @param questions A list of questions extracted from a Qualtrics QSF file. Use
#' questions_from_survey() to get them from an imported survey.
#'
#' @return A list of questions which now include in their Payload a QuestionTextClean
#' element, a copy of the QuestionText but cleaned of any HTML tags and HTML entities.
clean_question_text <- function(questions) {
  remove_css_style <- function(x) gsub("<style.*style>", "", x)

  for (i in 1:length(questions)) {
    questions[[i]][['Payload']][['QuestionTextClean']] <-
      clean_html(remove_css_style(questions[[i]][['Payload']][['QuestionText']]))
  }

  return(questions)
}


#' Clean HTML and whitespace from a string
#'
#' This function uses regex extensively to clean HTML out of a given text block.
#' "(&[a-z]*;|<.*?>)" is the first regular expression used.
#' It matches a substring that starts with & and ends with ; with
#' lower case letters between them, or a substring with < and > on each side, with
#' any characters between. Each matched substring is replaced with a space character.
#' The next regex is "\\s+". It matches multiple characters of whitespace, and
#' reduces them to a single space character.
#' The last regex used is "^\\s+|\\s+$". It matches whitespace at the beginning
#' or end of the text and removes it.
#'
#' @param text any text string that might contain HTML or whitespace that needs stripped.
#' @return text without any html or extraneous whitespace.
clean_html <- function(text) {
  clean_html_tags <- function(x) gsub("(&[a-z]*;|<.*?>)", " ", x)
  clean_extra_whitespace <- function(x) gsub("\\s+", " ", x)
  clean_leading_whitespace <- function (x) gsub("^\\s+|\\s+$", "", x)
  return(clean_leading_whitespace(clean_extra_whitespace(clean_html_tags(text))))
}


#' Create Human Readable Question Types
#'
#' This function saves, admittedly reductionist, more friendly question type
#' descriptions. It doesn't have human human readable versions for every question type,
#' but for some it is useful to reduce the question type to something more simple.
#' This has a nested function (create_qtype) that determines a question's human question type,
#' and then it loops through the questions and saves to each the QuestionTypeHuman
#' field with the output of create_qtype.
#'
#' @inheritParams clean_question_text
#' @return A list of questions which include in their Payload a QuestionTypeHuman field.
human_readable_qtype <- function(questions) {
  create_qtype <- function(q) {
    qtype <- which(c(is_multiple_choice(q),
                     is_single_answer(q),
                     is_rank_order(q),
                     is_text_entry(q)))
    if (length(qtype) == 0) qtype <- 0
    human_qtype <- switch(qtype,
                          "Multiple Answer",
                          "Single Answer",
                          "Rank Order",
                          "Text Entry")
    if (is.null(human_qtype)) human_qtype <- ""
    return(human_qtype)
  }

  for (i in 1:length(questions)) {
    questions[[i]][['Payload']][['QuestionTypeHuman']] <- create_qtype(questions[[i]])
  }

  return(questions)
}


#' Create a Question Dictionary
#'
#' @param blocks The blocks provided to this function must include questions inserted into
#' the BlockElements. Create the list of blocks from a survey with blocks_from_survey(),
#' and with questions on hand, insert them into the blocks with questions_into_blocks().
#' @return A data frame with a row for each question describing the question's details.
create_question_dictionary <- function(blocks) {
  # create_entry creates the row for any individual
  # response with the following elements in it:
  # - The data export tag,
  # - "QuestionTextClean", the question text stripped of any HTML strings/entities,
  # - "QuestionTypeHuman", the human readable question type,
  # - "QuestionType", the qualtrics supplied question type,
  # - "Selector", the qualtrics defined question selector
  create_entry <- function(i, j) {
    if (!"SubSelector" %in% names(blocks[[i]][['BlockElements']][[j]][['Payload']])) {
      blocks[[i]][['BlockElements']][[j]][['Payload']][['SubSelector']] <- ""
    }
    return(c(
      blocks[[i]][['BlockElements']][[j]][['Payload']][['DataExportTag']],
      blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionTextClean']],
      blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionType']],
      blocks[[i]][['BlockElements']][[j]][['Payload']][['Selector']],
      blocks[[i]][['BlockElements']][[j]][['Payload']][['SubSelector']],
      blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionTypeHuman']]
    ))
  }

  ### loop through each block, then each question,
  # then of the columns of the responses,
  # then each of the entries in each of the response columns,
  # and create an entry using "create_entry"
  entries <- list()
  e <- 0
  for (i in 1:number_of_blocks(blocks)) {
    if (length(blocks[[i]][['BlockElements']]) != 0) {
      for (j in 1:length(blocks[[i]][['BlockElements']])) {
        e <- e + 1
        entries[[e]] <- create_entry(i, j)
      }
    }
  }

  if (length(entries) > 0) {
    question_dictionary <- list_of_rows_to_df(entries)
    colnames(question_dictionary) <-
      c("Question Export Tag",
        "Question Text",
        "Question Type 1",
        "Question Type 2",
        "Question Type 3",
        "Response Type")
  } else {
    question_dictionary <- NULL
  }
  return(question_dictionary)
}


#' Create Uncodeable Question Dictionary
#'
#' The "uncodeable" questions are the questions that
#' do not have results tables inserted into them. This
#' function is meant to run on a list of blocks that have
#' had questions with their results tables inserted into them.
#' For any that do not have results tables, this function
#' assumes they were not successfully processed, adds them to the
#' list of uncodeable questions, and then returns a
#' question dictionary detailing them.
#'
#' @param blocks A list of blocks with questions that have been
#' processed with generate_results(questions). The questions can be inserted
#' into the blocks from a survey by using questions_into_blocks(questions, blocks).
#' @return A data frame providing the details of the questions that were not
#' successfully processed by generate_results(questions).
uncodeable_question_dictionary <- function(blocks) {

  # loop through each question,
  # and then remove everything that's not a survey question,
  # any questions that have a results table, and any questions
  # that are text entry or descriptive box questions.

  # make sure we run backwards so that we don't
  # move the next question to our current iterator, and then
  # skip it.
  for (i in 1:number_of_blocks(blocks)) {
    if (length(blocks[[i]][['BlockElements']]) != 0) {
      for (j in length(blocks[[i]][['BlockElements']]):1) {
        if (!("Element" %in% names(blocks[[i]][['BlockElements']][[j]]))) {
          blocks[[i]][['BlockElements']][[j]] <- NULL
        }
        else if ("Element" %in% names(blocks[[i]][['BlockElements']][[j]])) {
          if ("Table" %in% names(blocks[[i]][['BlockElements']][[j]]) ||
              blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionType']] == "TE" ||
              blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionType']] == "DB"
              ) {
            blocks[[i]][['BlockElements']][[j]] <- NULL
          }
        }
      }
    }
  }

  # we've cut out everything that isn't something that didn't get coded,
  # so now we just create a question dictionary with the remaining
  # results-tables-less questions.
  return(create_question_dictionary(blocks))
}


#' Create Long and Lean Response Dictionary
#'
#' lean_responses() creates a data frame where each row corresponds to
#' an individual response to the survey. Each response contains
#' the respondents' id, the question data export tag, the response column's name,
#' the question text, the question type(s), the variable response, and the coded
#' response, and then that individual's responses to any questions chosen for inclusion
#' as panel data.
#' @param panel_columns A list of names of response columns to include in the
#' output formatted as panel data.
#' @param question_blocks A list of blocks, with questions inserted in place of the
#' BlockElements representing them.
#' @param survey_responses The responses to the survey, as imported by ask_for_csv()
#' @return a data frame with each row detailing an individual survey response.
lean_responses <- function(panel_columns, question_blocks, survey_responses) {
  # get the blocks and responses from the global environment
  # TODO: these should also be optionally passed as direct parameters
  if (missing(question_blocks)) {
    blocks <- get("blocks", envir=1)
  } else {
    blocks <- question_blocks
  }
  if (missing(survey_responses)) {
    responses <- get("responses", envir=1)
  } else {
    responses <- survey_responses
  }

  # this create_entry function returns an entry (a row)
  # to be used in the lean_responses output.
  create_entry <- function(question, response_column, response_row) {
    responses <- get("responses", envir=1)

    # make sure that the subselector either exists or is set to "", so that
    # including it in an entry doesn't error
    if (!("SubSelector" %in% names(question[['Payload']]))) {
      question[['Payload']][['SubSelector']] <- ""
    }

    return(c(
      # Respondent ID:
      as.character(responses[,1][[response_row]]),
      # Question Data Export Tag:
      question[['Payload']][['DataExportTag']],
      # Question Response Column:
      names(question[['Responses']])[[response_column]],
      # Question Text:
      question[['Payload']][['QuestionTextClean']],
      # Question Type 1:
      question[['Payload']][['QuestionType']],
      # Question Type 2:
      question[['Payload']][['Selector']],
      # Question Type 3:
      question[['Payload']][['SubSelector']],
      # Response Type:
      question[['Payload']][['QuestionTypeHuman']],
      # Raw Response:
      toString(question[['Responses']][[response_column]][[response_row]]),
      # Coded Response:
      choice_text_from_question(question, question[['Responses']][[response_column]][[response_row]])
    ))
  }

  # create a dictionary as a list to store row-entries in.
  # for each block element, try to create an entry and add it
  # to the dictionary.
  # TODO: does this fail well?
  dictionary <- list()
  e <- 0
  for (b in 1:number_of_blocks(blocks)) {
    if ('BlockElements' %in% names(blocks[[b]])) {
      for (be in 1:length(blocks[[b]][['BlockElements']])) {
        if ("Responses" %in% names(blocks[[b]][['BlockElements']][[be]])) {
          coln <- ncol(blocks[[b]][['BlockElements']][[be]][['Responses']])
          rown <- nrow(blocks[[b]][['BlockElements']][[be]][['Responses']])
          if (coln > 0) {
            for (c in 1:coln) {
              if (rown > 0) {
                for (r in 1:rown) {

                  # if a block element has responses,
                  # for each response increment the dictionary index e once,
                  # and try to add to the dictionary the entry for that
                  # question. If creating the entry fails, return to the
                  # console a message saying
                  e <- e+1
                  dictionary[[e]] <-
                    tryCatch(create_entry(blocks[[b]][['BlockElements']][[be]], c, r),
                             error = function(e) {
                               cat(paste0("\nCreating an entry for the following question failed. \nDataExportTag: "
                                          , blocks[[b]][['BlockElements']][[be]][['Payload']][['DataExportTag']]
                                          , "\nResponse Column: "
                                          , c
                                          , "\nResponse Row: "
                                          , r
                               ))
                               return(NULL)
                             })
                }
              }
            }
          }
        }
      }
    }
  }

  # list_of_rows_to_df turns the rows into a data frame
  dictionary <- list_of_rows_to_df(dictionary)
  names(dictionary) <- c(
    "Respondent ID",
    "Question Data Export Tag",
    "Question Response Column",
    "Question Text",
    "Question Type 1",
    "Question Type 2",
    "Question Type 3",
    "Response Type",
    "Raw Response",
    "Coded Response"
  )

  # if there is a list of panel data included, for each
  # use answers_from_response_column to turn that column name
  # into panel data, then merge all the panel data together,
  # then merge it all into the dictionary.
  if (!missing(panel_columns)) {
    panel_data <- list()
    for (i in 1:length(panel_columns)) {
      panel_data[[i]] <- answers_from_response_column(panel_columns[[i]], responses, dictionary)
    }
    if (length(panel_columns) > 1) {
      panel_data <- reshape::merge_recurse(panel_data)
      dictionary <- merge(x = dictionary, y = panel_data, by = "Respondent ID", all = TRUE)
    } else if (length(panel_columns) == 1) {
      dictionary <- merge(x = dictionary, y = panel_data[[1]], by = "Respondent ID", all = TRUE)
    }
  }
  return(dictionary)
}


#' Get the Survey Respondents Answers from a Specific Response Column
#'
#' This function is to help in selecting the response data to a specific response
#' column. It selects that data from the lean_responses data (if it's available),
#' or the responses data frame. If it selects the response data from the lean_responses
#' data frame, the returned data frame includes a "Raw Response" and a "Coded Response"
#' column. If not, it includes exactly the response column as it appears in the responses.
#'
#' @param response_column The name of a response column that appears in the response set.
#' @param responses the data frame of responses
#' @param lean_responses responses reshaped with the lean_responses() function
#' @return a data frame with 2-3 columns, the first being "Respondent ID", the next 1-2 being the
#' response data for each respondent.
answers_from_response_column <- function(response_column, responses, lean_responses) {
  # if the lean_responses are included as an argument, and the response_column given as an
  # argument appears in the "Question Response Column" -- use the lean_responses
  if (!missing(lean_responses) && response_column %in% lean_responses[[3]]) {
    selected_df <- lean_responses[lean_responses[[3]] == response_column,
                                  c(1, 9, 10)]
    names(selected_df) <- c("Respondent ID",
                            paste0("Raw Response: ",
                                   lean_responses[lean_responses[[3]] == response_column, 4][[1]]
                                   ),
                            paste0("Coded Response: ",
                                   lean_responses[lean_responses[[3]] == response_column, 4][[1]]
                                   )
                            )
  # otherwise, use the responses
  } else {
    selected_df <- responses[c(1,which(names(responses) == response_column))]
    names(selected_df) <- c("Respondent ID", response_column)
  }

  return(selected_df)
}



split_side_by_sides <- function(questions, blocks) {
  for (i in length(questions):1) {
    if (questions[[i]][['Payload']][['QuestionType']] == 'SBS') {
      split_questions <- list()
      for (j in 1:questions[[i]][['Payload']][['NumberOfQuestions']]) {
        split_questions[[j]] <- list()
        split_questions[[j]][['Payload']] <- questions[[i]][['Payload']][['AdditionalQuestions']][[as.character(j)]]
        split_questions[[j]][['Payload']][['QuestionTextClean']] <- paste0(
          questions[[i]][['Payload']][['QuestionText']],
          "-",
          clean_html(questions[[i]][['Payload']][['AdditionalQuestions']][[as.character(j)]][['QuestionText']])
        )
      }

      orig_question_id <- questions[[i]][['Payload']][['QuestionID']]
      split_question_ids <- lapply(split_questions, function(x) x[['Payload']][['QuestionID']])
      split_block_elements <- lapply(split_question_ids, function(x) list("Type"="Question", "QuestionID"=x))
      for (k in 1:length(blocks)) {
        for (j in 1:length(blocks[[k]][['BlockElements']])) {
          block_elmt_question_id <- blocks[[k]][['BlockElements']][[j]][['QuestionID']]
          if (block_elmt_question_id == orig_question_id) {
            blocks[[k]][['BlockElements']][[j]] <- NULL
            blocks[[k]][['BlockElements']] <- append(blocks[[k]][['BlockElements']], split_block_elements, after=(j-1))
            break
          }
        }
      }

      questions[[i]] <- NULL
      questions <- append(questions, value=split_questions, after=(i-1))
    }
  }

  return(list(questions, blocks))
}
