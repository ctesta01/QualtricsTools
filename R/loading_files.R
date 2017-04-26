#' Set Response Data to Sample Data or User Data
#'
#' load_csv_data returns the sample response set or the user's response set depending
#' on whether or not the user has uploaded data.
#'
#' @param file1 This should be a CSV received from a file upload made in the Shiny UI,
#' which includes a file1[['datapath']] where the data can be located.
#'
#'
#' @return The return value is the responses data frame
load_csv_data <- function(file2, file1, headerrows) {
    if (is.null(file2) && is.null(file1)) {
      responses <- list(sample_responses, sample_original_first_rows)
    } else if (is.null(file2)) {
      responses <- NULL
    } else {
        responses <- ask_for_csv(file2[['datapath']], headerrows)
    }
    return(responses)
}

#' Set Survey to Sample Survey or User Survey
#'
#' load_qsf_data returns the sample survey or the user's survey depending
#' on whether or not the user has uploaded data.
#'
#' @param file2 This should be a QSF file received from a file upload made in the Shiny UI,
#' which includes a file1[['datapath']] where the data can be located.
#'
#' @return The return value is the survey list object

load_qsf_data <- function(file1) {
    if (is.null(file1)) {
        survey <- sample_survey
    } else {
        survey <- ask_for_qsf(file1[['datapath']])
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
ask_for_qsf <- function(surveyfile) {
    if (missing(surveyfile)) {
        print("Select Qualtrics Survey File:")
        surveyfile = file.choose()
    }
    survey = fromJSON(file=surveyfile)

    return(survey)
}

#' Ask the user for the Qualtrics Response Set
#'
#' ask_for_csv() inputs a file and a number of headerrows and returns a
#' list containing a responses data frame, and a 1-2 row data frame of the
#' responses' headers. Both input parameters are optional in the sense that
#' they do not have to be provided inline to the function, and that the
#' function will ask the user to choose a file for the responsesfile if one is
#' not provided inline, and it will default to using 3 headerrows.
#'
#' @param responsesfile this is the file path of a CSV response set to a Qualtrics survey
#' @param headerrows the number of rows before responses begin in the CSV data
#' @return a list of two elements: the responses data frame, and the original_first_rows data frame
ask_for_csv <- function(responsesfile, headerrows) {
    if (missing(responsesfile)) {
        print("Select CSV Response File:")
        responsesfile = file.choose()
    }
    if (missing(headerrows)) {
      headerrows <- 3
    }
    responses = read.csv(responsesfile, check.names = FALSE, stringsAsFactors = FALSE)
    responses[which(colnames(responses) == "")] <- NULL
    original_first_rows <- responses[1:(headerrows-1),]

    if (headerrows == 3) {
      original_first_rows[2,] <- lapply(original_first_rows[2, ], function(x) {
        x <- gsub("^\\{'ImportId': '", "", x, perl=TRUE)
        x <- gsub("'\\}$", "", x, perl=TRUE)
      })
    }

    responses <- responses[headerrows:nrow(responses),]
    responses <- responses[apply(responses, 1, function(x) any(x != "")),]
    responses <- apply(responses, 2, trimws)
    return(list(responses, original_first_rows))
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
    dataexporttags <- sapply(questions, function(x) x[['Payload']][['DataExportTag']])
    if (any(duplicated(dataexporttags))) {
        FALSE
    } else {
        TRUE
    }
}
