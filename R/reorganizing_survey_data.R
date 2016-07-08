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
        next
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
              blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionType']] == "DB" ||
              all(grepl("TEXT", colnames(blocks[[i]][['BlockElements']][[j]][['Responses']])))
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
#' the respondents' id, the response column's name,
#' the variable response, and the coded
#' response.
#' @param question_blocks A list of blocks, with questions inserted in place of the
#' BlockElements representing them.
#' @param survey_responses The responses to the survey, as imported by ask_for_csv()
#' @return a data frame with each row detailing an individual survey response.
lean_responses <- function(question_blocks, survey_responses) {
  # get the blocks, responses, and original_first_row from the global environment
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
  create_entry <- function(question, responses, response_column, response_row) {
    return(c(
      # Respondent ID:
      as.character(responses[,1][[response_row]]),
      # Question Response Column:
      names(question[['Responses']])[[response_column]],
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
                    tryCatch(create_entry(question=blocks[[b]][['BlockElements']][[be]],
                                          responses=responses,
                                          response_column=c,
                                          response_row=r),
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
  dictionary <- do.call(rbind.data.frame, dictionary)
  names(dictionary) <- c(
    "Respondent ID",
    "Question Response Column",
    "Raw Response",
    "Coded Response"
  )
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
#' @param question_dict a data frame with each question response column, created by
#' the create_response_column_dictionary() function
#' @return a data frame with 2-3 columns, the first being "Respondent ID", the next 1-2 being the
#' response data for each respondent.
answers_from_response_column <- function(response_column, responses, lean_responses, question_dict) {
  # if the lean_responses are included as an argument, and the response_column appears
  # in the "Question Response Column" -- use the lean_responses
  if (!missing(question_dict) &&
      !missing(lean_responses) &&
      response_column %in% lean_responses[[2]]) {

    # select the respondent ID, raw response, and coded response
    # from the lean_responses data frame
    selected_df <- lean_responses[lean_responses[[2]] == response_column,
                                  c(1, 3, 4)]

    # paste the question stem and question choice together from the
    # question dictionary to create the column names
    names(selected_df) <- c("Respondent ID",
                            paste0("Raw Response: ",
                                   question_dict[question_dict[[2]] == response_column, 3][[1]],
                                   question_dict[question_dict[[2]] == response_column, 4][[1]]
                            ),
                            paste0("Coded Response: ",
                                   question_dict[question_dict[[2]] == response_column, 3][[1]],
                                   question_dict[question_dict[[2]] == response_column, 4][[1]])
    )

    # otherwise, use the responses data frame directly
  } else {
    selected_df <- responses[c(1,which(names(responses) == response_column))]
    names(selected_df) <- c("Respondent ID", response_column)
  }

  return(selected_df)
}


#' Split Side-by-Side Questions into Multiple Questions
#'
#' This function updates both the list of questions and list of blocks from a survey
#' to reflect a side-by-side question as multiple individual questions.
#'
#' @param questions A list of questions from a survey
#' @param blocks A list of blocks from a survey
#' @return A list of questions and a list of blocks with their SBS questions split
#' into multiple questions
split_side_by_sides <- function(questions, blocks) {
  # loop through every question
  for (i in length(questions):1) {
    # if a question is a side-by-side question,
    # use the 'NumberOfQuestions' element from its payload
    # to determine how many questions to turn it into, and then
    # fill those questions with the payload of the 'AdditionalQuestions'
    # from the SBS question.
    if (questions[[i]][['Payload']][['QuestionType']] == 'SBS') {
      split_questions <- list()
      for (j in 1:questions[[i]][['Payload']][['NumberOfQuestions']]) {
        split_questions[[j]] <- list()
        split_questions[[j]][['Payload']] <- questions[[i]][['Payload']][['AdditionalQuestions']][[as.character(j)]]

        # question text will include the SBS question's original question text and the
        # specific question component's question text.
        split_questions[[j]][['Payload']][['QuestionTextClean']] <- paste0(
          questions[[i]][['Payload']][['QuestionText']],
          "-",
          clean_html(questions[[i]][['Payload']][['AdditionalQuestions']][[as.character(j)]][['QuestionText']])
        )
      }

      # use the SBS question's QuestionID to look up the question in the blocks
      # and replace the original with the split question's QuestionIDs
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
                                              names(split_responses)[[1]],
                                              " in the ",
                                              response_column,
                                              " column"),
                                       paste0("Number of Respondents in Respondent Group: ",
                                              nrow(split_responses[[i]])))
  }

  return(split_blocks)
}


#' Create a Response Column Dictionary
#' 
#' By providing the questions structured by their blocks and the original first
#' row of the response CSV from Qualtrics, this function is able to create a 
#' dictionary where each row is an entry for a response column. The response 
#' columns are listed along with the data export tag of the question they correspond 
#' to, their question stem and question choice, the question types (1, 2, and 3 levels 
#' of question types!), and the response type. 
#' 
#' @param question_blocks use questions_into_blocks() to create a list of blocks with 
#' the survey's questions inserted appropriately into them.
#' @param orig_first_row this is the first row of the response data CSV, it is 
#' automatically provided to you when you use get_setup() or in the shiny application.
#' @return a dataframe detailing in each row the response columns and their description.
create_response_column_dictionary <- function(question_blocks, orig_first_row) {
  # get the blocks, responses, and original_first_row from the global environment
  if (missing(question_blocks)) {
    blocks <- get("blocks", envir=1)
  } else {
    blocks <- question_blocks
  }
  if (missing(orig_first_row)) {
    original_first_row <- get("original_first_row", envir=1)
  } else {
    original_first_row <- orig_first_row
  }

  # this create_entry function returns an entry (a row)
  # to be used in the lean_responses output.
  create_entry <- function(question, response_column, original_first_row) {

    # make sure that the subselector either exists or is set to "", so that
    # including it in an entry doesn't error
    if (!("SubSelector" %in% names(question[['Payload']]))) {
      question[['Payload']][['SubSelector']] <- ""
    }

    # get the choice text and append it to the question text based on the
    # response column and the original_first_row entry in that column
    rcol <- names(question[['Responses']])[[response_column]]
    choice_text <- choice_text_from_response_column(rcol, original_first_row, blocks)

    return(c(
      # Question Data Export Tag:
      question[['Payload']][['DataExportTag']],
      # Question Response Column:
      names(question[['Responses']])[[response_column]],
      # Question Stem:
      question[['Payload']][['QuestionTextClean']],
      # Question Choice:
      choice_text,
      # Question Type 1:
      question[['Payload']][['QuestionType']],
      # Question Type 2:
      question[['Payload']][['Selector']],
      # Question Type 3:
      question[['Payload']][['SubSelector']],
      # Response Type:
      question[['Payload']][['QuestionTypeHuman']]
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
                  # if a block element has responses,
                  # for each response column increment the dictionary index e once,
                  # and try to add to the dictionary the entry for that
                  # response column. If creating the entry fails, return to the
                  # console a message saying so. 
                  e <- e+1
                  dictionary[[e]] <-
                    tryCatch(create_entry(question=blocks[[b]][['BlockElements']][[be]],
                                          response_column=c,
                                          original_first_row=original_first_row),
                             error = function(e) {
                               cat(paste0("\nCreating an entry for the following question failed. \nDataExportTag: "
                                          , blocks[[b]][['BlockElements']][[be]][['Payload']][['DataExportTag']]
                                          , "\nResponse Column: "
                                          , c
                               ))
                               return(NULL)
                             })
            }
          }
        }
      }
    }
  }

  # list_of_rows_to_df turns the rows into a data frame
  dictionary <- do.call(rbind.data.frame, dictionary)

  # rename the dictionary with the appropriate column names
  names(dictionary) <- c(
    "Question Data Export Tag",
    "Question Response Column",
    "Question Stem",
    "Question Choice",
    "Question Type 1",
    "Question Type 2",
    "Question Type 3",
    "Response Type"
  )

  return(dictionary)
}


#' Create Panel Data for Reshaped Data
#' 
#' The user provides this function a character vector of names of response columns (
#' as the panel_columns var), along with the response set data frame, and the returned
#' data is a data frame with respondent IDs and column(s) for each column specified. 
#' If the optional parameters, lean_responses and question_dict are provided, then 
#' the function will use them to include a "Raw Response" and a "Coded Response" 
#' column for any columns which are question response columns. 
#'
#' @param panel_columns a list of names of columns desired for inclusion
#' @param survey_responses a response set data frame imported from the Qualtrics CSV responses
#' @param lean_responses an optionally included data frame generated by the lean_responses() function
#' @param question_dict an optionally included data frame generated by the create_response_column_dictionary() function
create_panel_data <- function(panel_columns, survey_responses, lean_responses, question_dict) {
  # if the user doesn't provide a response set, try to grab 'responses' from the global scope
  if (missing(survey_responses)) { 
    responses <- get("responses", envir=1)
  } else {
    responses <- survey_responses
  }
  
  # if the user didn't provide panel_columns,
  # let's not waste anybody's time.
  if (missing(panel_columns)) return(NULL)
  if (length(panel_columns)==0) return(NULL)

  # initialize a list for us to store data frames in, panel_df.
  panel_df <- list()

  # for each panel column specified by the user, use answers_from_response_column() to generate
  # a data frame with the respondent IDs and response column(s) for that panel column.
  for (i in 1:length(panel_columns)) {
    if (!missing(question_dict) && 
        !missing(lean_responses)) {

      # if the user included a question_dict and a lean_responses dataframe, use them 
      # to get a "Raw Response" and "Coded Response" column in the output of answers_from_response_column()
      panel_df[[i]] <- answers_from_response_column(panel_columns[[i]], responses, lean_responses, question_dict)
    } else {
      # otherwise, just use the responses data frame
      panel_df[[i]] <- answers_from_response_column(panel_columns[[i]], responses)
    }
  }

    # from our list of data panel_column based data frames, 
    # either use merge_recurse to get one data frame, 
    # or select the only data frame there. 
  if (length(panel_columns) > 1) {
    panel_df <- reshape::merge_recurse(panel_df)
  } else if (length(panel_columns) == 1) {
    panel_df <- panel_df[[1]]
  }
  
  return(panel_df)
}

