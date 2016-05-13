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
get_questions_and_blocks <- function(survey, responses) {
  blocks <- blocks_from_survey(survey)
  questions <- questions_from_survey(survey)
  questions_without_trash <- remove_trash_questions(questions, blocks)
  questions <- clean_question_text(questions)
  questions <- human_readable_qtype(questions)
  blocks_without_trash <- remove_trash_blocks(blocks)
  questions_with_responses <- link_responses_to_questions(questions_without_trash, responses)
  questions_with_results <- generate_results(questions_with_responses)
  blocks_with_questions <- questions_into_blocks(questions_with_results, blocks_without_trash)
  questions_and_blocks <- list()
  questions_and_blocks[['questions']] <- questions_with_results
  questions_and_blocks[['blocks']] <- blocks_with_questions
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
    blocks <- Filter(function(x) x$Element == "BL", survey$SurveyElements)
    blocks <- blocks[[1]]$Payload
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
    questions <- survey$SurveyElements
    for (i in length(questions):1) {
        if (questions[[i]]$Element != "SQ") {
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
    trash <- Filter(function(x) x$Type == "Trash", blocks)
    trash_questions <- list()
    for (i in trash[[1]]$BlockElements) {
        trash_questions <- c(i$QuestionID, trash_questions)
    }

    delete_if_in_trash <- function(x) {
        if (x$Payload$QuestionID %in% trash_questions) {
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
#' This function finds among the trash blocks which has its $Type value
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
    blocks[which(sapply(blocks, function(x) x$Type == "Trash"))] = NULL
    for (i in 1:length(blocks)) {
      if (length(blocks[[i]]$BlockElements) != 0) {
        for (j in length(blocks[[i]]$BlockElements):1) {
          if(blocks[[i]]$BlockElements[[j]]$Type != "Question") {
            blocks[[i]]$BlockElements[[j]] <- NULL
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
#' contains in its $Payload the $ChoiceDataExportTags list, which contains
#' these user defined variables. This function goes through each question,
#' determines the DataExportTag and ChoiceDataExportTags, and uses each to select
#' the matching columns of the responses. Those are then inserted into that specific
#' question under $Responses, and the whole questions list is returned. One small
#' caveat is that if the column name starts with an integer, then R
#' will prepend it with an "X", so there is a helper function included here to include
#' those columns with prepended "X"s as well.
#'
#' @param questions A list of questions selected from a Qualtrics survey
#' @param responses A data frame of responses from a Qualtrics survey
#'
#' @return The updated list of questions, each including its relevant response columns
#' as a data frame stored in $Responses.
link_responses_to_questions <- function (questions, responses) {
    for (i in 1:length(questions)) {
        export_tag_with_underscore <- paste0( questions[[i]]$Payload$DataExportTag, "_" )
        export_tag_with_period <- paste0( questions[[i]]$Payload$DataExportTag, "." )

        starts_with_choice_export_tags <- vector('integer')
        if ("ChoiceDataExportTags" %in% names(questions[[i]]$Payload)) {
          choice_export_tags <- unlist(questions[[i]]$Payload$ChoiceDataExportTags)
          for (j in choice_export_tags) {
            starts_with_choice_export_tags <- c(starts_with_choice_export_tags,
                                                which(gdata::startsWith(names(responses), j)))
          }
        }
        matching_responses <- c(which(gdata::startsWith(names(responses), export_tag_with_underscore)),
                                which(gdata::startsWith(names(responses), export_tag_with_period)),
                              which(names(responses) == questions[[i]]$Payload$DataExportTag),
                              starts_with_choice_export_tags)
        questions[[i]]$Responses <- as.data.frame(responses[unique(matching_responses)])
    }
    questions
}


#' Organize the Questions into their Survey Blocks
#'
#' This function takes a blocks list (create this with blocks_from_survey and
#' then remove_trash_blocks), inserts the questions from a survey
#' appropriately into the blocks element, and then returns the
#' blocks list including the questions as elements in the blocks[[i]]$BlockElements.
#'
#' @inheritParams remove_trash_questions
#' @return a list of blocks including questions under blocks[[i]]$BlockElements
#' for each index i.
questions_into_blocks <- function(questions, blocks) {
  for (i in 1:length(blocks)) {
    if (length(blocks[[i]]$BlockElements) != 0) {
      for (j in 1:length(blocks[[i]]$BlockElements)) {
        matching_question <- which(sapply(questions,
                                    function(x) isTRUE(x$Payload$QuestionID ==
                                      blocks[[i]]$BlockElements[[j]]$QuestionID)))
        if (length(matching_question) == 1) {
          blocks[[i]]$BlockElements[[j]] <- questions[[matching_question]]
        }
      }
    }
  }
  return(blocks)
}


#' Clean QuestionText of HTML Tags
clean_question_text <- function(questions) {
  clean_html_tags <- function(x) gsub("(&[a-z]*;|<.*?>)", " ", x)
  clean_extra_whitespace <- function(x) gsub("\\s+", " ", x)
  clean_leading_whitespace <- function (x) gsub("^\\s+|\\s+$", "", x)

  for (i in 1:length(questions)) {
    questions[[i]][['Payload']][['QuestionTextClean']] <-
      clean_leading_whitespace(clean_extra_whitespace(
        clean_html_tags(
          questions[[i]][['Payload']][['QuestionText']])))
  }

  return(questions)
}

human_readable_qtype <- function(questions) {
  create_qtype <- function(q) {
    qtype <- which(c(is_multiple_choice(q),
                     is_single_answer(q),
                     is_rank_order(q),
                     is_text_entry(q)))
    if (length(qtype) == 0) qtype <- 0
    human_qtype <- switch(qtype,
                          "Check All",
                          "Single Answer",
                          "Rank Order",
                          "Text Entry")
    if (is.null(human_qtype)) human_qtype <- ""
    return(human_qtype)
  }

  for (i in 1:length(questions)) {
    questions[[i]]$Payload$QuestionTypeHuman <- create_qtype(questions[[i]])
  }

  return(questions)
}



#' Create a Question Dictionary
create_question_dictionary <- function(blocks) {

  list_of_rows_to_df <- function(data) {
    nCol <- max(vapply(data, length, 0))
    data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
    data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
    data.frame(data)
  }

  # create_entry creates the row for any individual
  # response with the following elements in it:
  # "DataExportTag",
  # "QuestionText",
  # "QuestionType",
  # "QuestionType2",
  # "QuestionType3",
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
