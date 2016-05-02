#' Format Percents
#'
#' percent0 is a function for formatting a decimal number into a string representing a percentage with
#' a specific number of decimal places and with a "%" appended.
#' The function uses formatC from the R base package and
#' defaults to formatting the number with 1 digit and with the "f" format
#' in the formatC argument.

percent0 <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


#' Create the Results Table for a Multiple Choice Single Answer Question
#'
#' The mc_single_answer_results function uses the definition of the choices in the QSF file
#' and their potentially recoded values to determine how to table the results paired to that question
#'
#'
#' @param question The question must have a paired response column placed into the question
#' under `$Responses`
#' @return a table with an N, Percent, and choice column, detailing the number of responses for each
#' choice.
mc_single_answer_results <- function(question) {
    # the factors are the variable codes that users could choose between
    # if a question has been recoded, the variable names are in $Payload$RecodeValues
    # and if not, they are in $Payload$Choices
    if ("RecodeValues" %in% names(question$Payload)) {
        factors <- unlist(question$Payload$RecodeValues)
    } else {
        factors <- names(questions[[3]]$Payload$Choices)
    }

    # the responses to single answer questions are only one column, so they are always the first column.
    # take the responses and sort them by the factors to get responses_tabled.
    # the second column of the responses_tabled are the numbers of responses for each factor, our Ns.
    # total number of responses to a question is counted by all the answers that aren't -99
    # use Ns and respondent_count to calculate the percents for each factor.
    responses <- question$Responses[[1]]
    responses_tabled <- as.data.frame(table(factor(responses, levels=factors)))
    N <- responses_tabled[,2]
    respondent_count <- length(question$Responses[[1]] != -99)
    Percent <- percent0(N / respondent_count)

    # if the choice variables have been recoded, first the factors are retrieved from the responses_tabled,
    # then they are turned into the list of corresponding indexes in the RecodeValues list,
    # which are then used to recover the original choice text from the Choices list,
    # and then the choices are flattened to a cleaner list.
    # if the choice variables are not recoded, then they can be retrieved directly from the responses_table
    if ("RecodeValues" %in% names(question$Payload)) {
        choices_recoded <- responses_tabled[,1]
        choices_uncoded <- sapply(choices_recoded, function(x) which(question$Payload$RecodeValues == x))
        choices <- sapply(choices_uncoded, function(x) question$Payload$Choices[[x]])
        choices <- unlist(choices, use.names = FALSE)
    } else {
        choices <- responses_tabled[,1]
    }

    # construct the results table with a column for N, Percent, and choices,
    # but make sure that the choices column doesn't have a header when it prints.
    results_table <- data.frame(N, Percent, choices)
    colnames(results_table)[3] <- ""
    results_table
}
