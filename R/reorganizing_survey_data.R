requireNamespace('gdata')

#' Get Restructured Questions (with inserted Responses)
#' and Blocks (with inserted Questions) from data
#' provided by Qualtrics.
#'
#' This function returns a list with two elements, where
#' the first element is questions with their responses
#' and results-tables listed in them, and secondly
#' the blocks of the survey, with questions listed in
#' each block as BlockElements.
#'
#' @param survey A qualtrics survey list object,
#' uploaded from a Qualtrics Survey File (QSF). Use
#' ask_for_qsf() to create such a survey list object from a QSF file.
#' @param responses A dataframe of Qualtrics responses to a survey.
#' Use ask_for_csv() to create such a dataframe from a CSV file.
#' @param original_first_rows A dataframe contianing the header information
#' for each column of response data. This dataframe includes a row for the DataExportTag based
#' response column names, another for the Question Text stem and choice text (although
#' truncated), and a row with QID based column names.
#' @return A list with two elements: the questions,
#' and the survey blocks. All questions in the trash are
#' excluded, HTML and CSS are removed from question text,
#' responses, notes, and a more easily human readable question
#' type are inserted into each question. The blocks then have
#' their BlockElements replaced by corresponding questions
#' where applicable. The BlockElements of the list of Blocks
#' from a survey originally only refer to questions
#' by their DataExportTag. However, this is inconvenient
#' because it adds an additional lookup step, and so they are
#' replaced by the real objects here.
get_coded_questions_and_blocks <-
  function(survey, responses, original_first_rows) {
    # select the block elements from the survey
    blocks <- blocks_from_survey(survey)

    # select the questions from the survey
    questions <- questions_from_survey(survey)

    # remove the questions that were found in the trash block
    questions <- remove_trash_questions(questions, blocks)

    # remove the trash block from the blocks
    blocks <- remove_trash_blocks(blocks)

    # split side by side questions into their component questions
    questions_and_blocks <- split_side_by_sides(questions, blocks)
    questions <- questions_and_blocks[[1]]
    blocks <- questions_and_blocks[[2]]

    # clean the question text of HTML and CSS tags
    questions <- clean_question_text(questions)

    # categorize each question's Response Type
    # (Single Answer, Multiple Answer,
    #  Text Entry, Rank Order)
    questions <- human_readable_qtype(questions)

    # insert the response columns into their corresponding
    # question under question[['Responses']]
    questions <-
      link_responses_to_questions(questions, responses, original_first_rows)

    # generate each question's results table and insert it
    # in question[['Table']]
    questions <- generate_results(questions, original_first_rows)

    # insert notes into their corresponding questions
    notes <- notes_from_survey(survey)
    questions <- insert_notes_into_questions(questions, notes)

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
#' @inheritParams get_coded_questions_and_blocks
#'
#' @return The blocks element returned is a list of blocks containing for
#' each a type, description, ID, and BlockElements (which contains the
#' list of questions included in a given block).
blocks_from_survey <- function(survey) {
  blocks <-
    Filter(function(x)
      x[['Element']] == "BL", survey[['SurveyElements']])
  blocks <- blocks[[1]][['Payload']]
  return(blocks)
}

#' Generate a List of Notes Blocks
#'
#' @inheritParams get_coded_questions_and_blocks
#'
#' @return This returns a list of blocks
notes_from_survey <- function(survey) {
  blocks <-
    Filter(function(x)
      x[['Element']] == "NT", survey[['SurveyElements']])
  return(blocks)
}

#' Insert the Notes for a question into its qtNotes
insert_notes_into_questions <- function(questions, notes) {
  for (note in notes) {
    # Find and set up the corresponding question to insert
    # the notes contents into the qtNotes list element of that question
    qid = note[['Payload']][['ParentID']]
    qid_index = find_question_index_by_qid(questions, qid)
    if (length(qid_index) > 0) {
      if (!"qtNotes" %in% names(questions[[qid_index]])) {
        questions[[qid_index]][['qtNotes']] <- list()
      }

      # Don't include the 'Removed' notes
      # And for the notes which aren't removed, prepend them with 'User Note: '
      notes_list <-
        sapply(note[['Payload']][['Notes']], function(x) {
          if (x[['Removed']] != 'TRUE')
            return(paste0('User Note: ', x[['Message']]))
        })
      # get only the non-NULL notes, because if the note was 'Removed'
      # then it will appear in the sapply output as NULL
      valid_notes <-
        which(sapply(notes_list, function(x)
          length(x) != 0))
      notes_list <- notes_list[valid_notes]

      # Append the formatted notes strings to the corresponding question's
      # qtNotes
      questions[[qid_index]][['qtNotes']] <-
        c(questions[[qid_index]][['qtNotes']], notes_list)
    }
  }

  return(questions)
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
  trash <- Filter(function(x)
    x[['Type']] == "Trash", blocks)

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
  questions <-
    Filter(Negate(function(x)
      is.null(unlist(x))), questions)
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
          if (blocks[[i]][['BlockElements']][[j]][['Type']] != "Question") {
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
#' questions and question-parts in order to analyze them. One of two methods is employed, depending
#' on whether or not the original_first_rows are from Insights or Legacy data.
#'
#' If Insights data is used, each question is looped through and the QuestionIDs are used to
#' match response columns to a question.
#'
#' Otherwise, a much more complicated process is used:
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
#' @param original_first_rows The original header rows to the CSV response set
#'
#' @return The updated list of questions, each including its relevant response columns
#' as a data frame stored in [['Responses']].
link_responses_to_questions <-
  function (questions,
            responses,
            original_first_rows) {
    if (!missing(original_first_rows) &&
        nrow(original_first_rows) >= 2) {
      for (i in 1:length(questions)) {
        question_id <- questions[[i]][['Payload']][['QuestionID']]
        matching_responses <-
          which(grepl(
            paste0(question_id, "$", "|", question_id, "[#_-]+.*$"),
            original_first_rows[2, ],
            perl = TRUE
          ))
        if (length(matching_responses) > 0) {
          matching_responses_names <-
            colnames(original_first_rows)[matching_responses]
          matching_responses <-
            as.data.frame(as.data.frame(responses)[, matching_responses_names])
          colnames(matching_responses) <- matching_responses_names
          questions[[i]][['Responses']] <- matching_responses
        }
      }
    } else if (missing(original_first_rows) ||
               !missing(original_first_rows) && nrow(original_first_rows) < 2) {
      responses <- as.data.frame(responses)
      for (i in 1:length(questions)) {
        # create a string with the data export tag and an underscore
        # create a string with the data export tag and a period
        export_tag_with_underscore <-
          paste0(questions[[i]][['Payload']][['DataExportTag']], "_")
        export_tag_with_period <-
          paste0(questions[[i]][['Payload']][['DataExportTag']], ".")
        export_tag_with_hashtag <-
          paste0(questions[[i]][['Payload']][['DataExportTag']], "#")

        # there's also the possibility that a response column starts
        # with a choice data export tag from a question.
        # this unlists the choice data export tags and creates
        # a list of response columns that start with a choice data export tag.
        starts_with_choice_export_tags <- vector('integer')
        if ("ChoiceDataExportTags" %in% names(questions[[i]][['Payload']])) {
          choice_export_tags <-
            unlist(questions[[i]][['Payload']][['ChoiceDataExportTags']])
          choice_export_tags <-
            sapply(choice_export_tags, function(x)
              gsub("-", "_", x))
          for (j in choice_export_tags) {
            starts_with_choice_export_tags <- c(starts_with_choice_export_tags,
                                                which(gdata::startsWith(names(
                                                  responses
                                                ), j)))
          }
        }

        # the response columns that match a question are the ones that:
        # - start with a data export tag followed by a underscore,
        # - start with a data export tag followed by a period,
        # - match the data export tag exactly,
        # - start with a choice data export tag.
        # take those matching response columns and join them to the question under [['Responses']]
        matching_responses <-
          c(
            which(gdata::startsWith(
              names(responses), export_tag_with_underscore
            )),
            which(gdata::startsWith(
              names(responses), export_tag_with_period
            )),
            which(gdata::startsWith(
              names(responses), export_tag_with_hashtag
            )),
            which(names(responses) == questions[[i]][['Payload']][['DataExportTag']]),
            starts_with_choice_export_tags
          )
        questions[[i]][['Responses']] <-
          as.data.frame(responses[unique(matching_responses)])
      }
    }
    return(questions)
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
                                          function(x)
                                            isTRUE(x[['Payload']][['QuestionID']] ==
                                                     blocks[[i]][['BlockElements']][[j]][['QuestionID']]) ||
                                            isTRUE(x[['Payload']][['QuestionID']] ==
                                                     blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionID']])))
        # if matching_question is a list of length 1 then we've matched the
        # question uniquely and can replace the BlockElement with the actual question
        if (length(matching_question) == 1) {
          if ("SkipLogic" %in% names(blocks[[i]][['BlockElements']][[j]])) {
            questions[[matching_question]][['Payload']][['SkipLogic']] <-
              blocks[[i]][['BlockElements']][[j]][['SkipLogic']]
          }
          blocks[[i]][['BlockElements']][[j]] <-
            questions[[matching_question]]
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

  for (i in 1:length(questions)) {
    questions[[i]][['Payload']][['QuestionTextClean']] <-
      clean_html(questions[[i]][['Payload']][['QuestionText']])
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
  # Clean HTML Tags and HTML Entitie
  text <- gsub("<.*?>|&[# a-z 0-9]*;", " ", text)
  # Remove leading or trailing whitespace
  text <- gsub("^\\s+|\\s+$", "", text)
  # Remove extra whitespace
  text <- gsub("\\s+", " ", text)
  return(text)
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
    qtype <- which(c(
      is_multiple_answer(q),
      is_single_answer(q),
      is_rank_order(q),
      is_text_entry(q)
    ))
    if (length(qtype) == 0)
      qtype <- 0
    human_qtype <- switch(qtype,
                          "Multiple Answer",
                          "Single Answer",
                          "Rank Order",
                          "Text Entry")
    if (is.null(human_qtype))
      human_qtype <- ""
    return(human_qtype)
  }

  for (i in 1:length(questions)) {
    questions[[i]][['Payload']][['QuestionTypeHuman']] <-
      create_qtype(questions[[i]])
  }

  return(questions)
}


#' Create a Question Dictionary
#'
#' @param blocks The blocks provided to this function must include questions inserted into
#' the BlockElements. Create the list of blocks from a survey with blocks_from_survey(),
#' and with questions on hand, insert them into the blocks with questions_into_blocks().
#' @return A data frame with a row for each question describing the question's details.
create_question_dictionary <- function(blocks, flow) {

  # Determine the ordering of the block indices that we will use to
  # iterate through the blocks.
  if (!missing(flow)) {
    # If flow was specified, use it to order the blocks and store the
    # blocks' ordering in the block_ordering list of indices.
    block_ordering <- list()
    for (h in flow) {
      # For each flow element, try and match it to a block.
      matched_block <- sapply(blocks, function(x) {
        if ('ID' %in% names(x)) {
          return(x[['ID']] == h)
        } else
          return(FALSE)
      })
      if (table(matched_block)['TRUE'] == 1) {
        block_ordering <- c(block_ordering, which(matched_block))
      }
    }
  } else {
    # If no flow is provided, go in order through the blocks.
    block_ordering <- 1:length(blocks)
  }

  # create_entry creates the row for any individual
  # response with the following elements in it:
  # - The data export tag,
  # - "QuestionTextClean", the question text stripped of any HTML strings/entities,
  # - "QuestionTypeHuman", the human readable question type,
  # - "QuestionType", the qualtrics supplied question type,
  # - "Selector", the qualtrics defined question selector
  create_entry <- function(i, j) {
    if (!"SubSelector" %in% names(blocks[[i]][['BlockElements']][[j]][['Payload']])) {
      blocks[[i]][['BlockElements']][[j]][['Payload']][['SubSelector']] <-
        ""
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
  for (i in block_ordering) {
    if ('BlockElements' %in% names(blocks[[i]]) &&
        length(blocks[[i]][['BlockElements']]) != 0) {
      for (j in 1:length(blocks[[i]][['BlockElements']])) {
        e <- e + 1
        entries[[e]] <- create_entry(i, j)
      }
    }
  }

  # This function takes a list of rows, all with the same length, and
  # turns them into a data frame. This is almost exactly the same as
  # rbind, except for the fact that this works effectively on a single
  # row whereas rbind does not.
  list_of_rows_to_df <- function(data) {
    nCol <- max(vapply(data, length, 0))
    data <-
      lapply(data, function(row)
        c(row, rep(NA, nCol - length(row))))
    data <-
      matrix(
        unlist(data),
        nrow = length(data),
        ncol = nCol,
        byrow = TRUE
      )
    data.frame(data)
  }

  if (length(entries) > 0) {
    question_dictionary <- list_of_rows_to_df(entries)
    colnames(question_dictionary) <-
      c(
        "Question Export Tag",
        "Question Text",
        "Question Type 1",
        "Question Type 2",
        "Question Type 3",
        "Response Type"
      )
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
              all(grepl("TEXT", colnames(blocks[[i]][['BlockElements']][[j]][['Responses']])))) {
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


#' Create a Lookup Table for Response Variables
#'
#' For each variable response in the responses to a question,
#' this function creates a row which contains the variable,
#' its recode value if the question uses recode values, and the
#' corresponding text for the given variable response. The exact
#' format of the output lookup table (dataframe) is dictated
#' by the question type depending on whether it is a
#' multiple answer question, a multiple choice single answer question,
#' or a matrix single answer question. Note that this function
#' only creates a lookup table for each response which appears in the responses.
#'
#' @param question A list of questions extracted from a Qualtrics QSF file with
#' responses inserted into them.
#' @return A dataframe with columns "var", "recode_value", and "text", where each
#' row corresponds to a unique variable response to the question.
create_response_lookup_table <-
  function(question) {
    # Get the list of unique non-text responses
    not_text_columns <- !(grepl("TEXT", colnames(question[['Responses']])))
    relevant_responses <- question[['Responses']][, not_text_columns]
    relevant_responses <- unlist(relevant_responses, use.names=FALSE)
    unique_responses <- unique(relevant_responses)

    # Remove -99 and "" from the list of unique responses
    valid_responses <- as.logical(sapply(unique_responses, function(x) !(x %in% c("-99", ""))))
    unique_responses <- unique_responses[which(valid_responses)]

    # The lookup_table starts as a list, to which we will insert another list for each
    # variable response in the question.
    lookup_table <- list()
    if (is_multiple_answer(question)) {
      # If the question is multiple answer, then set "1" and "" to correspond to
      # Selected and Not Selected.
      lookup_table[['0']] <- list()
      lookup_table[['0']][['var']] <- ""
      lookup_table[['0']][['text']] <- "Not Selected"
      lookup_table[['1']] <- list()
      lookup_table[['1']][['var']] <- "1"
      lookup_table[['1']][['text']] <- "Selected"
    } else if (is_mc_single_answer(question)) {
      # If the question is a multiple choice single answer question:
      has_recode_values <- any("RecodeValues" == names(question[['Payload']]))
      for (r in unique_responses) {
        # Insert a new list for the given variable response r
        i <- length(lookup_table) + 1
        lookup_table[[i]] <- list()
        lookup_table[[i]][['var']] <- r
        # If the question has recode values, lookup r as a recode value,
        # include its recoded value, and replace r with its recoded_value
        if (has_recode_values) {
          recode_value_index <- which(question[['Payload']][['RecodeValues']] == r)
          if (length(recode_value_index) != 0) {
            recoded_value <- names(question[['Payload']][['RecodeValues']])[[as.integer(recode_value_index[[1]])]]
            lookup_table[[i]][['recode_value']] <- recoded_value
            r <- recoded_value
          }
        }
        # Insert the choice text that corresponds to r
        lookup_table[[i]][['text']] <- clean_html(question[['Payload']][['Choices']][[r]][[1]])
      }
    } else if (is_matrix_single_answer(question)) {
      has_recode_values <- any("RecodeValues" == names(question[['Payload']]))
      for (r in unique_responses) {
        # Insert a new list for the given variable response r
        i <- length(lookup_table) + 1
        lookup_table[[i]] <- list()
        lookup_table[[i]][['var']] <- r
        if (has_recode_values) {
          # If the question has recode values, lookup r as a recode value,
          # include its recoded value, and replace r with its recoded_value
          recode_value_index <- which(question[['Payload']][['RecodeValues']] == r)
          if (length(recode_value_index) != 0) {
            recoded_value <- names(question[['Payload']][['RecodeValues']])[[as.integer(recode_value_index[[1]])]]
            lookup_table[[i]][['recode_value']] <- recoded_value
            r <- recoded_value
          }
        }
        # Matrix questions use "Answers" instead of "Choices" -- look up the text corresponding
        # to r and insert it as r's corresponding "text".
        lookup_table[[i]][['text']] <- clean_html(question[['Payload']][['Answers']][[r]][[1]])
      }
    }
    # Convert the lookup table from a list to a Dataframe:
    # sapply(lookup_table, c) creates a dataframe with the rows
    # as "var", "recode_value", and "text" which needs to be transposed.
    lookup_table <- data.frame(t(sapply(lookup_table, c)))
    return(lookup_table)
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
#' @param include_text_entry A parameter which defaults to FALSE indicating whether or not
#' open ended text responses should be included in the dictionary of lean responses.
#' @return a data frame with each row detailing an individual survey response.
lean_responses <- function(question_blocks, survey_responses, include_text_entry = FALSE) {
  requireNamespace("dplyr")
  requireNamespace("plyr")
  # get the blocks, responses, and original_first_row from the global environment
  if (missing(question_blocks)) {
    blocks <- get("blocks", envir = 1)
  } else {
    blocks <- question_blocks
  }
  if (missing(survey_responses)) {
    responses <- get("responses", envir = 1)
  } else {
    responses <- survey_responses
  }

  # This creates an individual entry corresponding to a
  # variable response: it is a list with the respondent ID,
  # question response column name, and the raw variable response.
  create_entry <-
    function(question,
             responses,
             response_column,
             response_row) {
      return(c(
        # Respondent ID:
        as.character(responses[, 1][[response_row]]),
        # Question Response Column:
        names(question[['Responses']])[[response_column]],
        # Raw Response:
        toString(question[['Responses']][[response_column]][[response_row]])
      ))
    }



  # Create a dictionary_list to store the dataframes for each question in.
  dictionary_list <- list()
  e <- 0
  for (b in 1:length(blocks)) {
    if ('BlockElements' %in% names(blocks[[b]])) {
      for (be in 1:length(blocks[[b]][['BlockElements']])) {
        question <- blocks[[b]][['BlockElements']][[be]]
        # If the question is not text entry, or include_text_entry is set to true,
        # and the question is not a descriptive box, proceed to check for its responses.
        if ((! is_text_entry(question) || include_text_entry) &&
            question[['Payload']][['QuestionType']] != "DB") {
          if ("Responses" %in% names(question)) {
            coln <- ncol(question[['Responses']])
            rown <-
              nrow(question[['Responses']])
            if (coln > 0 && rown > 0) {
              # Dictionary will be first a list which contains entries for each
              # variable response, each made by create_entry.
              dictionary <- list()
              f <- 0
              for (c in 1:coln) {
                if (!grepl("TEXT", colnames(question[['Responses']])[[c]])) {
                  for (r in 1:rown) {
                    f <- f+1
                    # if a block element has responses,
                    # for each response increment the dictionary index e once,
                    # and try to add to the dictionary the entry for that
                    # question. If creating the entry fails, return to the
                    # console a message saying

                    dictionary[[f]] <-
                      tryCatch(
                        create_entry(
                          question = question,
                          responses = responses,
                          response_column = c,
                          response_row = r
                        ),
                        error = function(e) {
                          cat(
                            paste0(
                              "\nCreating an entry for the following question failed. \nDataExportTag: ",
                              question[['Payload']][['DataExportTag']],
                              "\nResponse Column: ",
                              c,
                              "\nResponse Row: ",
                              r
                            )
                          )
                          return(NULL)
                        }
                      )

                  }
                }
              }
              if (length(dictionary) != 0) {
                e <- e + 1
                # Turn dictionary into a dataframe and name its columns
                df <- data.frame(t(sapply(dictionary, c)))
                colnames(df) <- c("Respondent ID",
                                  "Question Response Column",
                                  "Raw Response")
                # Use dplyr to filter out blank responses
                df <- dplyr::filter(df, `Raw Response` != "")
                # Create a response lookup table
                response_lookup <-
                  create_response_lookup_table(question)
                # Format and merge the response lookup table into the dictionary
                if (length(response_lookup) != 0) {
                  # Select only the "var" and "text" columns from the response_lookup dataframe
                  response_lookup <- response_lookup[, c("var", "text")]
                  # Rename the "text" column to "Coded Responses" so that it appears next to
                  # "Raw Response" with the correct name in the dataframe for this question
                  colnames(response_lookup)[2] <- "Coded Response"
                  # Make sure that the columns which we're trying to merge based on are of the
                  # same structure, otherwise dplyr will complain.
                  response_lookup[[1]] <- as.character(response_lookup[[1]])
                  df[['Raw Response']] <- as.character(df[['Raw Response']])
                  # Left join, so that we keep anything from df and we insert columns from
                  # response_lookup on the right.
                  df <- dplyr::left_join(df, response_lookup, by=c("Raw Response" = "var"))
                }
                # Save this dataframe with the others
                dictionary_list[[e]] <- df
              }
            }
          }
        }
      }
    }
  }

  # plyr's (NOT DPLYR) ldply function splits a list, applies the given function,
  # and then returns the results in a dataframe.
  df <- plyr::ldply(dictionary_list, data.frame)
  # Rename the columns again, because ldply turns "Raw Response" -> "Raw.Response"
  colnames(df) <- c(
    "Response ID",
    "Question Response Column",
    "Raw Response",
    "Coded Response")
  # Remove all NULL values in the "Coded Response" column.
  df[['Coded Response']] <- lapply(df[['Coded Response']], function(x) ifelse( is.null(x), "", x))
  return(df)
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
answers_from_response_column <-
  function(response_column,
           responses,
           lean_responses,
           question_dict) {
    # if the lean_responses are included as an argument, and the response_column appears
    # in the "Question Response Column" -- use the lean_responses
    if (!missing(question_dict) &&
        !missing(lean_responses) &&
        response_column %in% lean_responses[[2]]) {
      # select the respondent ID, raw response, and coded response
      # from the lean_responses data frame
      selected_df <-
        lean_responses[lean_responses[[2]] == response_column,
                       c(1, 3, 4)]

      # paste the question stem and question choice together from the
      # question dictionary to create the column names
      names(selected_df) <- c(
        "Respondent ID",
        paste0("Raw Response: ",
               question_dict[question_dict[[2]] == response_column, 3][[1]],
               question_dict[question_dict[[2]] == response_column, 4][[1]]),
        paste0("Coded Response: ",
               question_dict[question_dict[[2]] == response_column, 3][[1]],
               question_dict[question_dict[[2]] == response_column, 4][[1]])
      )

      # otherwise, use the responses data frame directly
    } else {
      selected_df <-
        responses[c(1, which(names(responses) == response_column))]
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
        split_questions[[j]][['Payload']] <-
          questions[[i]][['Payload']][['AdditionalQuestions']][[as.character(j)]]

        # question text will include the SBS question's original question text and the
        # specific question component's question text.
        split_questions[[j]][['Payload']][['QuestionText']] <-
          paste0(clean_html(questions[[i]][['Payload']][['QuestionText']]),
                 "-",
                 clean_html(questions[[i]][['Payload']][['AdditionalQuestions']][[as.character(j)]][['QuestionText']]))

        # append a qtNote to split side-by-side questions
        split_questions[[j]][['qtNotes']] <- list()
        if ('qtNotes' %in% names(questions[[i]]))
          split_questions[[j]][['qtNotes']] <- questions[[i]][['qtNotes']]
        split_questions[[j]][['qtNotes']] <-
          c(split_questions[[j]][['qtNotes']],
            'This question was split from a side-by-side question.')
      }

      # use the SBS question's QuestionID to look up the question in the blocks
      # and replace the original with the split question's QuestionIDs
      orig_question_id <-
        questions[[i]][['Payload']][['QuestionID']]
      split_question_ids <-
        lapply(split_questions, function(x)
          x[['Payload']][['QuestionID']])
      split_block_elements <-
        lapply(split_question_ids, function(x)
          list("Type" = "Question", "QuestionID" = x))
      for (k in 1:length(blocks)) {
        if ('BlockElements' %in% names(blocks[[k]])) {
          for (j in 1:length(blocks[[k]][['BlockElements']])) {
            block_elmt_question_id <-
              blocks[[k]][['BlockElements']][[j]][['QuestionID']]
            if (block_elmt_question_id == orig_question_id) {
              blocks[[k]][['BlockElements']][[j]] <- NULL
              blocks[[k]][['BlockElements']] <-
                append(blocks[[k]][['BlockElements']], split_block_elements, after = (j -
                                                                                        1))
              break
            }
          }
        }
      }

      questions[[i]] <- NULL
      questions <-
        append(questions, value = split_questions, after = (i - 1))
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
    e <- e + 1
    dl_indices_1 <-
      suppressWarnings(which(!is.na(as.numeric(
        names(question[['Payload']][['DisplayLogic']])
      ))))
    for (i in dl_indices_1) {
      dl_indices_2 <-
        suppressWarnings(which(!is.na(as.numeric(
          names(question[['Payload']][['DisplayLogic']][[i]])
        ))))
      for (j in dl_indices_2) {
        if ("Description" %in% names(question[['Payload']][['DisplayLogic']][[i]][[j]])) {
          display_logic[[e]] <-
            clean_html(question[['Payload']][['DisplayLogic']][[i]][[j]][['Description']])
          e <- e + 1
        }
      }
    }
  }

  # we do the same process for the
  # choices, but including a message before each display logic describing which
  # choice it corresponds to.
  if ("Choices" %in% names(question[['Payload']])) {
    choices_with_logic <-
      sapply(question[['Payload']][['Choices']], function(x)
        "DisplayLogic" %in% names(x))
    has_choice_logic <- any(choices_with_logic)
    if (has_choice_logic) {
      choices_with_logic <- which(choices_with_logic)
      e <- e + 1
      for (i in choices_with_logic) {
        display_logic[[e]] <-
          paste0("Choice Display Logic for ", question[['Payload']][['Choices']][[i]][['Display']], ":")
        e <- e + 1
        dl_indices_1 <-
          suppressWarnings(which(!is.na(as.numeric(
            names(question[['Payload']][['Choices']][[i]][['DisplayLogic']])
          ))))
        for (j in dl_indices_1) {
          dl_indices_2 <-
            suppressWarnings(which(!is.na(as.numeric(
              names(question[['Payload']][['Choices']][[i]][['DisplayLogic']][[j]])
            ))))
          for (k in dl_indices_2) {
            if ("Description" %in% names(question[['Payload']][['Choices']][[i]][['DisplayLogic']][[j]][[k]])) {
              display_logic[[e]] <-
                clean_html(question[['Payload']][['Choices']][[i]][['DisplayLogic']][[j]][[k]][['Description']])
              e <- e + 1
            }
          }
        }
      }
    }
  }

  # for the answers, we do the exact same as the choices.
  if ("Answers" %in% names(question[['Payload']])) {
    answers_with_logic <-
      sapply(question[['Payload']][['Answers']], function(x)
        "DisplayLogic" %in% names(x))
    has_answer_logic <- any(answers_with_logic)
    answers_with_logic <- which(answers_with_logic)
    if (has_answer_logic) {
      e <- e + 1
      for (i in answers_with_logic) {
        display_logic[[e]] <-
          paste0("Choice Display Logic for ", question[['Payload']][['Answers']][[i]][['Display']], ":")
        e <- e + 1
        dl_indices_1 <-
          suppressWarnings(which(!is.na(as.numeric(
            names(question[['Payload']][['Answers']][[i]][['DisplayLogic']])
          ))))
        for (j in dl_indices_1) {
          dl_indices_2 <-
            suppressWarnings(which(!is.na(as.numeric(
              names(question[['Payload']][['Answers']][[i]][['DisplayLogic']][[j]])
            ))))
          for (k in dl_indices_2) {
            if ("Description" %in% names(question[['Payload']][['Answers']][[i]][['DisplayLogic']][[j]][[k]])) {
              display_logic[[e]] <-
                clean_html(question[['Payload']][['Answers']][[i]][['DisplayLogic']][[j]][[k]][['Description']])
              e <- e + 1
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
split_respondents <-
  function(response_column,
           responses,
           survey,
           blocks,
           questions,
           headerrows,
           already_loaded,
           original_first_rows) {
    if (missing(headerrows)) {
      headerrows <- 3
    }
    if (missing(already_loaded)) {
      already_loaded <- FALSE
    }

    if (already_loaded != TRUE) {
      if (missing(responses)) {
        try(responses <<- ask_for_csv(headerrows = headerrows))
      }
      if (missing(survey)) {
        try(survey <<- ask_for_qsf())
      }
    }

    if (already_loaded == TRUE) {
      if (!exists("survey", where = globalenv())) {
        survey <- sample_survey
      } else {
        survey <- get(x = "survey", envir = globalenv())
      }

      if (!exists("responses", where = globalenv())) {
        responses <- sample_responses
      } else {
        responses <- get("responses", envir = globalenv())
      }

      if (!exists("blocks", where = globalenv())) {
        # process the blocks and questions as per usual
        blocks <- blocks_from_survey(survey)
        questions <- questions_from_survey(survey)
        questions <- remove_trash_questions(questions, blocks)
        questions <- clean_question_text(questions)
        questions <- human_readable_qtype(questions)
        blocks <- remove_trash_blocks(blocks)
        notes <- notes_from_survey(survey)
        questions <- insert_notes_into_questions(questions, notes)
      } else {
        blocks <- get(x = "blocks", envir = globalenv())
        questions <- get(x = "questions", envir = globalenv())
      }
    }

    # For each question, check if its notes contain the "Denominator Used:"
    # substring. If a note on a question does include this substring,
    # remove it, because the "Denominators Used" will be recalculated when
    # the split blocks' questions are processed.
    for (i in 1:length(questions)) {
      if ('qtNotes' %in% names(questions[[i]])) {
        j = 1
        while (j <= length(questions[[i]][['qtNotes']])) {
          if (grepl('Denominator Used:', questions[[i]][['qtNotes']][[j]])) {
            questions[[i]][['qtNotes']] <- questions[[i]][['qtNotes']][-j]
            j <- j - 1
          }
          j <- j + 1
        }
      }
    }

    # split the respondents by their responses to in the response_column
    split_responses <-
      split(responses, responses[response_column], drop = TRUE)


    # insert the header into the blocks
    blocks[['header']] <- c(
      paste0("Survey Name: ",
             survey[['SurveyEntry']][['SurveyName']]),
      paste0("Total Number of Original Respondents: ",
             nrow(responses))
    )

    # duplicate the blocks and questions once for every respondent group
    split_blocks <- rep(list(blocks), times = length(split_responses))
    split_questions <-
      rep(list(questions), times = length(split_responses))

    # for each of the respondent groups, insert the responses into the
    # questions for that respondent group, generate the question's results,
    # and then insert the questions into the blocks for that respondent group.
    for (i in 1:length(split_responses)) {
      split_questions[[i]] <-
        link_responses_to_questions(split_questions[[i]], split_responses[[i]], original_first_rows)
      split_questions[[i]] <-
        generate_results(split_questions[[i]], original_first_rows)
      split_blocks[[i]] <-
        questions_into_blocks(split_questions[[i]], split_blocks[[i]])
      split_blocks[[i]][['header']] <-
        c(
          split_blocks[[i]][['header']],
          paste0(
            "Respondents with ",
            names(split_responses)[[i]],
            " in the ",
            response_column,
            " column"
          ),
          paste0("Size of Respondent Group: ",
                 nrow(split_responses[[i]]))
        )
      split_blocks[[i]][['split_group']] <-
        names(split_responses)[[i]]
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
create_response_column_dictionary <-
  function(question_blocks, flow, orig_first_row) {
    # get the blocks, responses, and original_first_row from the global environment
    if (missing(question_blocks)) {
      blocks <- get("blocks", envir = 1)
    } else {
      blocks <- question_blocks
    }
    if (missing(orig_first_row)) {
      original_first_row <- get("original_first_rows", envir = 1)
    } else {
      original_first_row <- orig_first_row
    }

    # Determine the ordering of the block indices that we will use to
    # iterate through the blocks.
    if (!missing(flow)) {
      # If flow was specified, use it to order the blocks and store the
      # blocks' ordering in the block_ordering list of indices.
      block_ordering <- list()
      for (h in flow) {
        # For each flow element, try and match it to a block.
        matched_block <- sapply(blocks, function(x) {
          if ('ID' %in% names(x)) {
            return(x[['ID']] == h)
          } else
            return(FALSE)
        })
        if (table(matched_block)['TRUE'] == 1) {
          block_ordering <- c(block_ordering, which(matched_block))
        }
      }
    } else {
      # If no flow is provided, go in order through the blocks.
      block_ordering <- 1:length(blocks)
    }

    # this create_entry function returns an entry (a row)
    # to be used in the lean_responses output.
    create_entry <-
      function(question,
               response_column,
               original_first_row) {
        # make sure that the subselector either exists or is set to "", so that
        # including it in an entry doesn't error
        if (!("SubSelector" %in% names(question[['Payload']]))) {
          question[['Payload']][['SubSelector']] <- ""
        }

        # get the choice text and append it to the question text based on the
        # response column and the original_first_row entry in that column
        rcol <- names(question[['Responses']])[[response_column]]
        choice_text <-
          choice_text_from_response_column(rcol, original_first_row, blocks)

        return(
          c(
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
          )
        )
      }

    # create a dictionary as a list to store row-entries in.
    # for each block element, try to create an entry and add it
    # to the dictionary.
    # TODO: does this fail well?
    dictionary <- list()
    e <- 0
    for (b in block_ordering) {
      if ('BlockElements' %in% names(blocks[[b]])) {
        for (be in 1:length(blocks[[b]][['BlockElements']])) {
          if ("Responses" %in% names(blocks[[b]][['BlockElements']][[be]]) &&
              !is.null(blocks[[b]][['BlockElements']][[be]][['Responses']])) {
            coln <- ncol(blocks[[b]][['BlockElements']][[be]][['Responses']])
            rown <-
              nrow(blocks[[b]][['BlockElements']][[be]][['Responses']])
            if (coln > 0) {
              for (c in 1:coln) {
                # if a block element has responses,
                # for each response column increment the dictionary index e once,
                # and try to add to the dictionary the entry for that
                # response column. If creating the entry fails, return to the
                # console a message saying so.
                e <- e + 1
                dictionary[[e]] <-
                  tryCatch(
                    create_entry(
                      question = blocks[[b]][['BlockElements']][[be]],
                      response_column = c,
                      original_first_row = original_first_row
                    ),
                    error = function(e) {
                      cat(
                        paste0(
                          "\nCreating an entry for the following question failed. \nDataExportTag: "
                          ,
                          blocks[[b]][['BlockElements']][[be]][['Payload']][['DataExportTag']]
                          ,
                          "\nResponse Column: "
                          ,
                          c
                        )
                      )
                      return(NULL)
                    }
                  )
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
create_panel_data <-
  function(panel_columns,
           survey_responses,
           lean_responses,
           question_dict) {
    # if the user doesn't provide a response set, try to grab 'responses' from the global scope
    if (missing(survey_responses)) {
      responses <- get("responses", envir = 1)
    } else {
      responses <- survey_responses
    }

    # if the user didn't provide panel_columns,
    # let's not waste anybody's time.
    if (missing(panel_columns))
      return(NULL)
    if (length(panel_columns) == 0)
      return(NULL)

    # initialize a list for us to store data frames in, panel_df.
    panel_df <- list()

    # for each panel column specified by the user, use answers_from_response_column() to generate
    # a data frame with the respondent IDs and response column(s) for that panel column.
    for (i in 1:length(panel_columns)) {
      if (!missing(question_dict) &&
          !missing(lean_responses)) {
        # if the user included a question_dict and a lean_responses dataframe, use them
        # to get a "Raw Response" and "Coded Response" column in the output of answers_from_response_column()
        panel_df[[i]] <-
          answers_from_response_column(panel_columns[[i]],
                                       responses,
                                       lean_responses,
                                       question_dict)
      } else {
        # otherwise, just use the responses data frame
        panel_df[[i]] <-
          answers_from_response_column(panel_columns[[i]], responses)
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


#' Add a Custom Response Column with Merged Contents
#'
#' The create_merged_response_column() function allows a user to
#' create a response set with a column reflecting the combined
#' contents of several different columns. If the user specifies
#' response columns that are part of the survey question responses,
#' then the responses are coded and the coded values are used
#' instead of the variable response values. The column is appended to
#' the response set and the response set with the additional
#' new column is returned to the user.
#'
#' @param response_columns a list of names of response columns
#' @param col_name the desired name for the added output column
#' @param question_blocks a list of blocks with questions inserted
#' in place of the BlockElements.
#' @param survey_responses the data frame of responses as imported by
#' ask_for_csv()
#' @return a response data frame with a single new column which
#' represents the contents of the specified response_columns.
create_merged_response_column <- function(response_columns,
                                          col_name,
                                          question_blocks,
                                          survey_responses) {
  # if the user doesn't include the question_blocks parameter,
  # grab the "blocks" from global scope
  if (missing(question_blocks) && exists("blocks", where = -1)) {
    blocks <- get("blocks", envir = -1)
  } else if (!missing(question_blocks)) {
    blocks <- question_blocks
  }

  # if the user doens't include the survey_responses parameter,
  # grab the "responses" from the global scope
  if (missing(survey_responses) && exists("responses", where = -1)) {
    responses <- get("responses", envir = -1)
  } else if (!missing(survey_responses)) {
    responses <- survey_responses
  }

  # remove any response columns that don't make sense
  for (i in length(response_columns):1) {
    if (!response_columns[[i]] %in% names(responses)) {
      cat(paste0(
        response_columns[[i]],
        " doesn't appear in the
        response columns' names"
      ))
      response_columns[[i]] <- NULL
    }
  }

  # if there aren't any response columns to merge, exit
  if (length(response_columns) == 0)
    return(NULL)

  # for each of the names in "response_columns" get a
  # response column.
  # to do so, check if the response column can be corresponded
  # to a question. if so, use the question to get the coded response
  # column. if it can't be corresponded with a question, use the
  # response column directly.
  to_be_merged <- list()
  for (i in 1:length(response_columns)) {
    merge_col_name <- response_columns[[i]]
    question <-
      question_from_response_column(blocks, merge_col_name)
    response_col <- responses[[merge_col_name]]

    if (!is.null(question)) {
      question <-
        blocks[[question[[1]]]][['BlockElements']][[question[[2]]]]
      should_convert <- !is_text_entry(question)
      converted <-
        lapply(response_col, function(x)
          choice_text_from_question(question, x))
      should_convert <- should_convert && !all(converted == "")
    } else
      should_convert <- FALSE


    if (should_convert) {
      question <-
        blocks[[question[[1]]]][['BlockElements']][[question[[2]]]]
      to_be_merged[[i]] <- converted
    } else {
      to_be_merged[[i]] <- response_col
    }
  }

  # for each row of the responses,
  # get a list of the responses across the response columns selected.
  # then take that list and collapse it to a single string, and save that
  # as the merged response
  merged_col <- list()
  for (i in 1:length(response_col)) {
    to_merge <-
      lapply(1:length(to_be_merged), function(x)
        to_be_merged[[x]][[i]])
    merged_col[[i]] <- paste(to_merge, collapse = " + ")
  }

  # save the original rownames of the responses,
  # cbind the new column in,
  # if there was an output column name provided, use it to name the added column
  # and replace the rownames with the original rownames
  orig_rownames <- rownames(responses)
  responses <- cbind(responses, t(as.data.frame(merged_col)))
  if (!missing(col_name))
    names(responses)[length(names(responses))] <- col_name
  rownames(responses) <- orig_rownames

  return(responses)
  }
