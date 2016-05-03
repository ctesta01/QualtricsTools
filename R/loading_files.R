#' Set Response Data to Sample Data or User Data
#'
#' load_csv_data returns the sample response set or the user's response set depending
#' on whether or not the user has uploaded data.
#'
#' @param file1 This should be a CSV received from a file upload made in the Shiny UI,
#' which includes a file1$datapath where the data can be located.
#'
#'
#' @return The return value is the responses data frame
load_csv_data <- function(file1) {
    if (is.null(file1)) {
        responses <- sample_responses
    } else {
        responses <- ask_user_for_csv(file1$datapath)
    }
    return(responses)
}

#' Set Survey to Sample Survey or User Survey
#'
#' load_qsf_data returns the sample survey or the user's survey depending
#' on whether or not the user has uploaded data.
#'
#' @param file2 This should be a QSF file received from a file upload made in the Shiny UI,
#' which includes a file1$datapath where the data can be located.
#'
#' @return The return value is the survey list object

load_qsf_data <- function(file2) {
    if (is.null(file2)) {
        survey <- sample_survey
    } else {
        survey <- ask_user_for_qsf(file2$datapath)
    }
}


#' Ask the user for the Qualtrics Survey file
#'
#' This function can be provided the path to a Qualtrics survey as its parameter, or
#' the function will prompt the user to specify the path to the file.
#'
#' @param The file path to a Qualtrics survey file
#'
#' @return The survey file the user uploads, as a list
ask_user_for_qsf <- function(surveyfile) {
    if (missing(surveyfile)) {
        print("Select Qualtrics Survey File:")
        surveyfile = file.choose()
    }
    survey = fromJSON(file=surveyfile)

    return(survey)
}

#' Ask the user for the Qualtrics Response Set
#'
#' This function automatically turns the entries in the
#' first row into a "text" attribute of the column. Additionally,
#' it removes the first row after turning its entries into attributes.
#'
#' @return The csv file the user uploads, as a data frame
ask_user_for_csv <- function(responsesfile) {
    if (missing(responsesfile)) {
        print("Select CSV Response File:")
        responsesfile = file.choose()
    }
    responses = read.csv(responsesfile)


    for (i in 1:length(colnames(responses))) {
        column <- colnames(responses)[i]
        attr(responses[[column]], "text") <- responses[1, i]
    }

    responses <- responses[-1, ]
    return(responses)
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
#' @inheritParam blocks_from_survey
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
#' set to "Trash" and then removes it from the blocks list.
#'
#' @inheritParams remove_trash_questions
#' @return The list of blocks is returned without any Trash blocks
remove_trash_blocks <- function(blocks) {
    blocks[which(sapply(blocks, function(x) x$Type == "Trash"))] = NULL
    return(blocks)
}

#' Validate Data Export Tag Uniqueness
#'
#' It is crucial for parts of this program to work that the DataExportTags be unique
#' to one another. This program checks for duplicates among the DataExportTags, and
#' returns a boolean statement representing whether or not the DataExportTags validated.
#' validate_export_tags returns TRUE if the DataExportTags are not duplicated, and are valid,
#' and FALSE if the DataExportTags did not validate, and are duplicated.
#'
#' @param questions A list of questions from a Qualtrics survey
validate_data_export_tags <- function(questions) {
    dataexporttags <- sapply(questions, function(x) x$Payload$DataExportTag)
    if (any(duplicated(dataexporttags))) {
        FALSE
    } else {
        TRUE
    }
}

#' Validate Response Column Uniqueness
#'
#' It is crucial for parts of this program to work that the data response columns be unique
#' to one another. This program checks for duplicates among the response column names
#' of the questions and returns FALSE if there are any duplicates, and TRUE if there are no
#' duplicates. Equivalently, FALSE represents FALSE validation, and TRUE represents TRUE validation.
#'
#' @param responses A data frame of responses from a Qualtrics survey
validate_response_columns <- function(responses) {
    if (any(duplicated(names(responses)))) {
        FALSE
    } else {
        TRUE
    }
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
    prepend_x <- function(x) {
        if (length(x) == 0) {
            x
        } else if (substring(x, 1, 1) %in%
                   c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
            paste0("X", x)
        } else {
            x
        }
    }

    for (i in 1:length(questions)) {
        export_tag_with_underscore <- paste0( questions[[i]]$Payload$DataExportTag, "_" )
        export_tag_with_period <- paste0( questions[[i]]$Payload$DataExportTag, "." )
        matching_responses <- c(which(gdata::startsWith(names(responses), export_tag_with_underscore)),
                                which(gdata::startsWith(names(responses), export_tag_with_period)),
                              which(names(responses) == questions[[i]]$Payload$DataExportTag),
                              which(names(responses) %in% sapply(questions[[i]]$Payload$ChoiceDataExportTags, prepend_x)))
        questions[[i]]$Responses <- as.data.frame(responses[matching_responses])
    }
    questions
}
