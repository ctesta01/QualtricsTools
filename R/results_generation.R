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



mc_single_answer_results <- function(question) {

    if ("RecodeValues" %in% names(question$Payload)) {
        factors <- unlist(question$Payload$RecodeValues)
    } else {
        factors <- names(questions[[3]]$Payload$Choices)
    }

    responses <- question$Responses[[1]]
    responses_tabled <- as.data.frame(table(factor(responses, levels=factors)))
    Ns <- responses_tabled[,2]


    if ("RecodeValues" %in% names(question$Payload)) {
        choices_recoded <- responses_tabled[,1]
        choices_uncoded <- sapply(Choices, function(x) which(questions[[3]]$Payload$RecodeValues == x))
        choices <- sapply(Choices, function(x) question$Payload$Choices[[x]])
    } else {
        choices <- responses_tabled[,2]
    }


}
