#' A Shiny app to format Qualtrics survey data and generate reports
#'
#' This function launches the Shiny interface for the Qualtrics
#' package from the files in the install or 'inst' directory.
#'
#' @usage qualtrics::app()
app <- function() {
  shiny::runApp(system.file('shiny', package = 'qualtrics'))
}

#' Setup the Global Environment for a Survey
#'
#' This function sets up the .GlobalEnv to include survey, responses, questions
#' (without trash questions, with clean question text,
#' with a human readable question type, with responses, and with results if
#' automatically generateable), and blocks (without trash blocks, with questions
#' inserted in place of the BlockElements representing them).
#'
#' @param headerrows An optional parameter for specifying the number of
#' headerrows in the response csv.
get_setup <- function(headerrows) {
  if (missing(headerrows)) {
    headerrows <- 3
  }
  try(survey <<- ask_for_qsf())
  try(responses <<- ask_for_csv(headerrows = headerrows))
  try(blocks <<- blocks_from_survey(survey))
  try(questions <<- questions_from_survey(survey))
  try(questions <<- remove_trash_questions(questions, blocks))
  try(questions <<- clean_question_text(questions))
  try(questions <<- human_readable_qtype(questions))
  try(blocks <<- remove_trash_blocks(blocks))
  try(questions <<- link_responses_to_questions(questions, responses))
  try(questions <<- generate_results(questions))
  try(blocks <<- questions_into_blocks(questions, blocks))

  if ( exists("survey", 1) &&
       exists("responses", 1) &&
       exists("questions", 1) &&
       exists("blocks", 1)
  ) {
    cat("survey, responses, questions, and blocks have all been made
        globally available in your R session.")
  }
}


#' Find Question from DataExportTag
#'
#' This function takes a list of questions and an export tag and
#' looks for the matching question. It will try to select
#' the question uniquely.
find_question <- function(questions, exporttag) {
  matched_question_index <- which(sapply(questions, function(x) x$Payload$DataExportTag == exporttag))
  return(questions[[matched_question_index]])
}


#' Find Question Index from DataExportTag
#'
#' This function takes a list of questions and an export tag and
#' looks for the matching question. It returns the index(es) of
#' the questions with that Question Data Export Tag.
find_question_index <- function(questions, exporttag) {
  matched_question_index <- which(sapply(questions, function(x) x$Payload$DataExportTag == exporttag))
  return(matched_question_index)
}
