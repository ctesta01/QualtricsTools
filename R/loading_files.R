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
load_csv_data <- function(file2) {
    if (is.null(file2)) {
        responses <- sample_responses
    } else {
        responses <- ask_for_csv(file2$datapath)
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

load_qsf_data <- function(file1) {
    if (is.null(file1)) {
        survey <- sample_survey
    } else {
        survey <- ask_for_qsf(file1$datapath)
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
#' This function automatically turns the entries in the
#' first row into a "text" attribute of the column. Additionally,
#' it removes the first row after turning its entries into attributes.
#'
#' @return The csv file the user uploads, as a data frame
ask_for_csv <- function(responsesfile) {
    if (missing(responsesfile)) {
        print("Select CSV Response File:")
        responsesfile = file.choose()
    }
    responses = read.csv(responsesfile, check.names=F)

    responses[which(colnames(responses) == "")] <- NULL

    for (i in 1:length(colnames(responses))) {
        column <- colnames(responses)[i]
        attr(responses[[column]], "text") <- responses[1, i]
    }

    responses <- responses[-1, ]
    return(responses)
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
