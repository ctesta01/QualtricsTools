#' Create a List of HTML Versions of the Results Tables
#'
#' @param questions A list of questions with the relevant results tables
#' stored as data frames under the questions[[i]]$Table element. Create
#' such a list of questions by using generate_results function
#'
#' @return A list of HTML results tables for each question
tabelize_blocks <- function(blocks) {
  # all the html tables will be saved into the tables list.
  tables <- list()
  options(stringsAsFactors = FALSE)

  # loop through every block element that has a results table,
  # and for each add to the tables list the results table,
  # with a caption containing the Question Data Export Tag,
  # and the question text. The results tables will be
  # processed with xtable and converted to HTML so they can
  # be displayed in the UI. After each results table and its
  # caption, add a <br> (an html break) to separate the next
  # results table.
  for (i in 1:length(blocks)) {
    if (length(blocks[[i]]$BlockElements) != 0) {
      for (j in 1:length(blocks[[i]]$BlockElements)) {
        if (is.null(blocks[[i]]$BlockElements[[j]]$Table) == FALSE) {

          tables = c(tables,
                     capture.output(
                       print(xtable::xtable(
                               blocks[[i]]$BlockElements[[j]]$Table,
                               caption=paste0(
                                 "Question ",
                                 blocks[[i]]$BlockElements[[j]]$Payload$DataExportTag,
                                 ": ",
                                 blocks[[i]]$BlockElements[[j]]$Payload$QuestionTextClean)
                               ),
                             type="html",
                             html.table.attributes='class="data table table-bordered table-condensed"',
                             caption.placement="top",
                             include.rownames=FALSE)))
          tables = c(tables, "<br>")

        } else if ("Payload" %in% names(blocks[[i]]$BlockElements[[j]])) {
          tables = c(tables, HTML(paste0("<br><p style='color: #777;'>The results table for Question ",
                                   blocks[[i]]$BlockElements[[j]]$Payload$DataExportTag,
                                   " could not be automatically processed.")))
          tables = c(tables, "<br>")
        }
      }
    }
  }
  return(unlist(lapply(tables, paste)))
}




#' Create HTML Tables for the Text Entry Questions
#'
#' This function creates an HTML string with tables
#' for each of the text entry questions and their responses.
#' The appendix_lettering function inside this function
#' is to create lettering for the tables in the style
#' of "A, B, ..., Z, AA, ..., ZZ" and so forth. The html
#' tables are created by looping through the blocks
#' and their contained question block elements. For each
#' question, if the question is a Text Entry question or
#' any of the response columns contain "TEXT" then a table
#' is created for those responses.
#'
#' @param blocks A list of blocks with block elements replaced
#' by the question with its paired responses.
#'
#' @return an html string containing a title,
#' question text, and the text responses for each
#' text appendix.
text_appendices_table <- function(blocks) {

  # appendix_lettering takes a number
  # and returns the corresponding lettered index.
  # examples:
  # 1 -> A
  # 2 -> B
  # 27 -> AA
  # 29 -> AC
  # ... and so forth.
  appendix_lettering <- function(number) {
    if (number %in% 1:26) {
      return(LETTERS[[number]])
    } else if (number %in% 27:702) {
      first_digit <- (floor((number - 1) / 26))
      second_digit <- ((number - 1) %% 26) + 1
      first_letter <- LETTERS[[first_digit]]
      second_letter <- LETTERS[[second_digit]]
      return(paste0(first_letter, second_letter))
    }
  }

  # start with an empty list and an index at 0
  # the index e is for creating the appendix names,
  # by use of the above appendix_lettering function.
  # tables is for storing the HTML for all of the
  # text appendices tables.
  tables <- list()
  e <- 0

  # loop through every response column that is EITHER
  # 1) the only response column to a TextEntry question, or
  # 2) a response column containing the string "TEXT".
  for (i in 1:length(blocks)) {
    if (length(blocks[[i]]$BlockElements) != 0) {
      for (j in 1:length(blocks[[i]]$BlockElements)) {
        if (!(is.null(blocks[[i]]$BlockElements[[j]]$Responses))) {

          # save the indexes of the response columns which contain
          # the string "TEXT"
          text_columns <- which(sapply(colnames(blocks[[i]]$BlockElements[[j]]$Responses),
                                       function(x) grepl("TEXT", x)))

          # if the question is a TextEntry question,
          # remove all the empty and -99 responses,
          # add a title for the appendix (ex "Appendix AA") to tables,
          # use xtable to create an HTML table of the responses,
          # and caption the table with the question text and the
          # number of responses,
          # and last add a <br> (html line break) to separate the next
          # text appendix
          if (blocks[[i]]$BlockElements[[j]]$Payload$QuestionType == "TE") {
            responses <- blocks[[i]]$BlockElements[[j]]$Responses
            responses <- as.data.frame(responses[!apply(responses, 1, function(x) any(x=="")),])
            responses <- as.data.frame(responses[!apply(responses, 1, function(x) any(x==-99)),])
            colnames(responses) <- colnames(blocks[[i]]$BlockElements[[j]]$Responses)
            if (length(as.list(responses)) > 0) {
              e <- e+1
              tables <- c(tables, capture.output(print(xtable::xtable(
                rbind(
                  paste0("Appendix ", appendix_lettering(e)),
                  blocks[[i]]$BlockElements[[j]]$Payload$QuestionTextClean,
		  "Verbatim responses -- these have not been edited in any way.",
                  "",
                  paste0("Responses: (",
                         nrow(responses),
                         ")"),
                  responses)
              ),
              type="html",
              html.table.attributes='class="text_appendices data table table-bordered table-condensed"',
              include.rownames=FALSE)))


              tables <- c(tables, "<br>")
            }

            # if the question isn't a TextEntry question, but does
            # have some response columns which contain "TEXT",
            # then for each of those response columns grab the responses,
            # remove the empty and -99 responses,
            # check that the responses aren't empty,
            # give the appendix a title,
            # use xtable to print an html table for each of the
            # text response columns,
            # give the xtable html table a caption with
            # the question text and number of responses,
            # and last add a <br> (html line break) to separate
            # the next text appendix.
          } else if (length(text_columns) > 0) {
            for (k in 1:length(text_columns)) {
              responses <- blocks[[i]]$BlockElements[[j]]$Responses[text_columns[[k]]]
              responses <- as.data.frame(responses[!apply(responses, 1, function(x) any(x=="")),])
              responses <- as.data.frame(responses[!apply(responses, 1, function(x) any(x==-99)),])
              colnames(responses) <- colnames(blocks[[i]]$BlockElements[[j]]$Responses[text_columns[[k]]])
              if (length(as.list(responses)) > 0) {
                e <- e+1

                tables <- c(tables, capture.output(print(xtable::xtable(
                  rbind(
                    paste0("Appendix ", appendix_lettering(e)),
                    blocks[[i]]$BlockElements[[j]]$Payload$QuestionTextClean,
		    "Verbatim responses -- these have not been edited in any way.",
                    "",
                    paste0("Responses: (",
                           nrow(responses),
                           ")"),
                    responses)
                ),
                type="html",
                html.table.attributes='class="text_appendices data table table-bordered table-condensed"',
                include.rownames=FALSE)))

                tables <- c(tables, "<br>")
              }

            }
          }
        }
      }
    }
  }
  return(unlist(lapply(tables, paste)))
}



#' Create a Message Stating Which Questions Weren't Automatically Tabled
#'
#' This is function is used in the Shiny app to tell users which questions weren't
#' automatically coded. This may be changed later to be more informative, or
#' to include this information elsewhere.
#'
#' @inheritParams tabelize_blocks
#' @return A message stating for which questions could not have
#' results automatically generated.
uncodeable_questions_message <- function(questions) {
  # get all the questions that don't have tables, and
  # aren't a text entry question or a descriptive box.
  uncodeable_questions <- which(sapply(questions, function(x)
    !("Table" %in% names(x)) &&
      (x$Payload$QuestionType != "TE") &&
      (x$Payload$QuestionType != "DB")))

  # get the data export tags of the uncodeable questions
  uncodeable_questions <- sapply(uncodeable_questions, function(x)
    questions[[x]]$Payload$DataExportTag)

  # write the message (ex. "The following questions could not be
  # automatically coded: Q1, Q2, ...")
  uncodeable_message <- ""
  if (length(uncodeable_questions) > 0) {
    uncodeable_questions <- paste(uncodeable_questions, collapse=", ")
    uncodeable_message <- sprintf("The following questions could not be automatically processed: %s",
                                  uncodeable_questions)
  } else {
    uncodeable_message <- "All questions were successfully processed!"
  }
  return(uncodeable_message)
}


