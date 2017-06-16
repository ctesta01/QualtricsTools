#' Repath Windows Paths with "\" to ones with "/"
#' This function is originally from a StackOverflow comment here:
#' https://stackoverflow.com/questions/1189759/expert-r-users-whats-in-your-rprofile/12703931#12703931
repath <- function() {
  cat('Paste windows file path and hit RETURN twice')
  path <- scan(what = "")
  fixed_path <- gsub('\\\\', '/', path)
  writeClipboard(paste(fixed_path, collapse=" "))
  cat('Here\'s your de-windowsified path. (It\'s also on the clipboard.)\n', fixed_path, '\n')
}

#' Get the Index of the Question Corresponding to a Response Column
#'
#' Use this function to get the location of a question
#' in the blocks list. Give it a response column name, and it will
#' try to find the question it corresponds to. Otherwise, it will
#' respond NULL. The blocks have two layers of indexing, one for the individual
#' blocks, and then another for the BlockElements. This function will return a
#' pair of indices, (i, j) where blocks[[i]][['BlockElements']][[j]] specifies the
#' location of the question which has the response_name column among its linked responses.
#' @param blocks A list of the survey blocks, with the questions included in them.
#' @param response_name The string name of a column from a Qualtrics response dataset.
#' @return A pair, (i, j) which specifies a question as blocks[[i]][['BlockElements']][[j]].
question_from_response_column <- function(blocks, response_name) {
  # construct a list, with keys as the response column names, and
  # values as pairs of block and blockelement indexes.
  responses_to_indexes <- list()
  for (i in 1:number_of_blocks(blocks)) {
    if ('BlockElements' %in% names(blocks[[i]])) {
      for (j in 1:length(blocks[[i]][['BlockElements']])) {
        if ("Responses" %in% names(blocks[[i]][['BlockElements']][[j]])) {
          for (k in names(blocks[[i]][['BlockElements']][[j]][['Responses']])) {
            responses_to_indexes[[k]] <- c(i, j)
          }
        }
      }
    }
  }
  return(responses_to_indexes[[response_name]])
}


#' Get the Choice Text based on the Choice from a Question
#'
#' Input a question and a variable corresponding to a choice,
#' and this function returns the choice text. The function
#' works by determining the question type and some question properties
#' and then using a combination of the question's list of choices
#' and related values. The text is then cleaned of any HTML before
#' returned.
#' @param question This is a list object representing an individual question
#' from a Qualtrics Survey File. The question must have a paired
#' response column placed into the question
#' under [['Responses']]. The insertion of the responses into questions is
#' handled by link_responses_to_questions.
#' @param choice A numeric value representing the choice made in a response
#' to the question provided.
choice_text_from_question <- function(question, choice) {
  original <- choice
  choice <- as.character(choice)

  # if the question is a multiple answer question,
  # meaning some form of "check all that apply",
  # then the answers are boolean valued -- either they
  # checked it or they didn't. Return TRUE, FALSE, or
  # "Seen, but Unanswered" depending.
  if (is_multiple_choice(question)) {
    if (choice %in% c(1, "1")) {
      choice <- "Selected"
    } else {
      choice <- "Not Selected"
    }

    # if the question is a single answer multiple choice
    # question, then it either has recode values, or
    # the choice given is directly correspondent with
    # the index of the choice in the [['Payload']][['Choices']]
    # list. if the choice given doesn't match any
    # of the recode values, try getting it directly from
    # the choices.
  } else if (is_mc_single_answer(question)) {
    if ("RecodeValues" %in% names(question[['Payload']]) &&
        choice %in% question[['Payload']][['RecodeValues']]) {
      recoded_value <-
        which(question[['Payload']][['RecodeValues']] == choice)
      recoded_value <-
        names(question[['Payload']][['RecodeValues']])[[as.integer(recoded_value)]]
      if (length(recoded_value) != 0)
        choice <- recoded_value
      if (choice %in% names(question[['Payload']][['Choices']]))
        choice <- question[['Payload']][['Choices']][[choice]][[1]]
    } else {
      if (choice %in% names(question[['Payload']][['Choices']]))
        choice <- question[['Payload']][['Choices']][[choice]][[1]]
    }


    # if the question is a single answer matrix question,
    # the question will either have recode values, or not.
    # if the question has recode values, attempt to use the
    # [['Payload']][['RecodeValues']] list to retrieve the recoded_value.
    # If that doesn't work, just use the original choice given.
  } else if (is_matrix_single_answer(question)) {
    if ("RecodeValues" %in% names(question[['Payload']]) &&
        length(question[['Payload']][['RecodeValues']]) > 0) {
      recoded_value <-
        which(question[['Payload']][['RecodeValues']] == choice)
      if (length(recoded_value) != 0) {
        choice <-
          names(question[['Payload']][['RecodeValues']])[[recoded_value]]
      }
      if (choice %in% names(question[['Payload']][['Answers']]))
        choice <- question[['Payload']][['Answers']][[choice]][[1]]
    } else {
      if (choice %in% names(question[['Payload']][['Answers']]))
        choice <- question[['Payload']][['Answers']][[choice]][[1]]
    }
  }

  if (original %in% c(-99, "-99"))
    choice <- "Seen, but Unanswered"
  if (is.na(choice) || identical(choice, original))
    choice <- ""
  choice <- clean_html(choice)
  return(choice)
}



#' A Shiny app to format Qualtrics survey data and generate reports
#'
#' This function launches the Shiny interface for the Qualtrics
#' package from the files in the install or 'inst' directory.
app <- function() {
  shiny::runApp(system.file('shiny', package = 'QualtricsTools'),
                launch.browser = TRUE)
}

#' Setup the Global Environment for a Survey
#'
#' This function sets the user up with the survey, responses, questions,
#' blocks, questions, original_first_rows, and flow. By default, these are
#' returned to the global scope (referred to by .GlobalEnv or globalenv()).
#' If return_data is passed as TRUE, then the data is returned from
#' the function as a list. The blocks and questions are redundant as they
#' are already included in the survey, but they are often useful to
#' have already pulled out of the survey. Among the many processing steps this
#' function (directly and as subsequent nested function calls), the question
#' text is cleaned and stripped of HTML and any unwanted characters,
#' the trash questions and blocks are removed, response columns are matched
#' and inserted into the corresponding questions, and results tables detailing
#' the frequencies and breakdowns of respondents among each question's choices
#' are automatically inserted into each applicable question. This function can
#' be called in multiple ways, specifying the parameters explicitly, or by
#' specifying them interactively. For example, calling get_setup() with no
#' parameters will result in a prompt asking for the number of headerrows in
#' your response data and two subsequent dialogue boxes asking the user
#' to choose the corresponding QSF and CSV files. If already_loaded=TRUE is
#' passed, then the get_setup function pulls the survey list and responses
#' dataframe from the global environment. If none are found, but
#' already_loaded=TRUE was passed, then a sample survey is loaded.
#'
#' @param headerrows An optional parameter for specifying the number of
#' headerrows in the response csv.
#' @param already_loaded can be set to TRUE to indicate that get_setup should
#' try to get the survey, responses, and original_first_rows from the global scope
#' instead of asking the user for them. If they aren't there, then the function
#' will uses sample data included in the package.
#' @param qsf_path is the string location of the survey as a .QSF (Qualtrics Survey File)
#' @param csv_path is the string location of the survey's responses, downloaded from Qualtrics
#' @param return_data is an optional boolean parameter which dictates whether the processed
#' survey data should be returned to the global scope if return_data=FALSE or is missing,
#' or if the processed should be returned as a list in the order
#' c(survey, responses, questions, blocks, original_first_rows, flow) if return_data=TRUE.
#'
#' @examples
#' # An Interactive Example
#'
#' > get_setup()
#' Enter the number of response data header rows [Default: 3]:
#' Defaulting to headerrows=3
#' [1] "Select Qualtrics Survey File:"
#' [1] "Select CSV Response File:"
#'
#' survey, responses, questions, blocks, original_first_rows,
#' and flow are now global variables.
#'
#' # An Explicit Example
#'
#' > get_setup(
#'     qsf_path = "C:/Example/Path/to/QSF/File.qsf",
#'     csv_path = "C:/Example/Path/to/CSV/File.csv",
#'     headerrows = 3)
#'
#' survey, responses, questions, blocks, original_first_rows,
#' and flow are now global variables.
#'
#' # An Example with return_data=TRUE
#'
#' > qualtricstools_values = get_setup(
#'     qsf_path = "C:/Example/Path/to/QSF/File.qsf",
#'     csv_path = "C:/Example/Path/to/CSV/File.csv",
#'     headerrows = 3,
#'     return_data=TRUE)
#'
#' > varnames = c(
#'     'survey', 'responses', 'questions', 'blocks',
#'     'original_first_rows', 'flow')
#' > for (i in 1:length(varnames))
#'     assign(varnames[[i]], qualtricstools_values[[i]])
#' > rm(qualtricstools_values, varnames, i)
#' > ls()
#'
#' [1] "blocks"              "flow"                "original_first_rows"
#' [5] "questions"           "responses"           "survey"
#'
#' # Loading a Sample Survey
#'
#' > rm(list=ls()) # Make sure your environment is empty first
#' > get_setup(already_loaded=TRUE)
#'
#' survey, responses, questions, blocks, original_first_rows,
#' and flow are now global variables.
get_setup <- function(headerrows,
                      already_loaded,
                      qsf_path,
                      csv_path,
                      return_data = FALSE,
                      sample_data = FALSE) {
  # default to already_loaded = FALSE
  if (missing(already_loaded)) {
    already_loaded <- FALSE
  }

  # ask the user for the CSV and the QSF if the
  if (already_loaded == FALSE) {
    # default to headerrows = 3
    if (missing(headerrows)) {
      headerrows <-
        readline(prompt = "Enter the number of response data header rows [Default: 3]: ")
      if (!grepl("^[0-9]+$", headerrows)) {
        cat('Defaulting to headerrows = 3\n')
        headerrows = 3
      } else
        headerrows <- as.integer(headerrows)
    }
    if (missing(qsf_path)) {
      survey <- ask_for_qsf()
    } else {
      survey <- ask_for_qsf(qsf_path)
    }
    if (missing(csv_path)) {
      responses <- ask_for_csv(headerrows = headerrows)
    } else {
      responses <- ask_for_csv(csv_path, headerrows = headerrows)
    }
    original_first_rows <- as.data.frame(responses[[2]])
    responses <- as.data.frame(responses[[1]])
  }

  if (already_loaded == TRUE) {
    if (!exists("survey", where = -1)) {
      survey <- sample_survey
    } else {
      survey <- get("survey", envir = -1)
    }

    if (!exists("responses", where = -1) ||
        !exists("original_first_rows", where = -1)) {
      responses <- sample_responses
      original_first_rows <<- sample_original_first_rows
    } else {
      responses <- get("responses", envir = -1)
      original_first_rows <- get("original_first_rows", envir = -1)
    }
  }

  questions_and_blocks <-
    get_coded_questions_and_blocks(survey, responses, original_first_rows)
  questions <- questions_and_blocks[[1]]
  blocks <- questions_and_blocks[[2]]

  # insert a header into the blocks
  blocks[['header']] <- c(paste0("Survey Name: ",
                                 survey[['SurveyEntry']][['SurveyName']]),
                          paste0("Number of Respondents: ",
                                 nrow(responses)))
  survey <- survey
  responses <- responses
  questions <- questions
  blocks <- blocks
  original_first_rows <- original_first_rows
  flow <- flow_from_survey(survey)

  if (return_data) {
    return_vals = list(
      "survey" = survey,
      "responses" = responses,
      "questions" = questions,
      "blocks" = blocks,
      "original_first_rows" = original_first_rows,
      "flow" = flow
    )
    return(return_vals)
  } else {
    survey <<- survey
    responses <<- responses
    questions <<- questions
    blocks <<- blocks
    original_first_rows <<- original_first_rows
    flow <<- flow_from_survey(survey)

    if (exists("survey", 1) &&
        exists("responses", 1) &&
        exists("questions", 1) &&
        exists("blocks", 1) &&
        exists("original_first_rows")) {
      cat(
        "survey, responses, questions, blocks, original_first_rows,
        and flow are now global variables.\n"
      )
    }
    }
  }

#' Find Question from DataExportTag
#'
#' This function takes a list of questions and an export tag and
#' looks for the matching question. It will try to select
#' the question uniquely.
#' @param questions A list of questions from a Qualtrics survey.
#' @param exporttag A string data export tag to identify the
#' desired question.
#' @return The question list object, such that
#' find_question(...)[['Payload']][['DataExportTag']] == exporttag
find_question <- function(questions, exporttag) {
  if (missing(questions))
    questions <- get('questions', envir = globalenv())
  matched_question_index <-
    which(sapply(questions, function(x)
      x[['Payload']][['DataExportTag']] == exporttag))
  return(questions[[matched_question_index]])
}


#' Find Question Index from DataExportTag
#'
#' Similar to find_question and find_question, this function
#' takes a list of questions and an export tag and
#' looks for the matching question. Differently from find_question,
#' this function returns the index of
#' the questions with that Question Data Export Tag rather than
#' the question itself.
#' @inheritParams find_question
#' @return A numeric list with entries such that
#' questions[[i]][['Payload']][['DataExportTag]] == exporttag
#' for any i in the returned list.
find_question_index <- function(questions, exporttag) {
  if (missing(questions))
    questions <- get('questions', envir = globalenv())
  matched_question_index <-
    which(sapply(questions, function(x)
      x[['Payload']][['DataExportTag']] == exporttag))
  return(matched_question_index)
}

#' Find a Question by its QuestionID
#'
#' This function takes a list of questions and a Question ID and
#' looks for the question with a matching Question ID. The function
#' returns the index of the matching question.
#' @inheritParams find_question_index
#' @param qid A string QuestionID to match
#' @return A numeric list with entries such that
#' questions[[i]][['Payload']][['DataExportTag']] == qid
find_question_index_by_qid <- function(questions, qid) {
  if (missing(questions))
    questions <- get('questions', envir = globalenv())
  matched_question_index <-
    which(sapply(questions, function(x)
      x[['Payload']][['QuestionID']] == qid))
  return(matched_question_index)
}


#' Get the Choice Text from the First Row of the Responses
#'
#' This function uses the first row of the response data from Qualtrics
#' to determine the choice text a response column corresponds to.
#'
#' @param response_column The string name of a response column from the response set.
#' @param original_first_row A dataframe contianing the header information
#' for each column of response data. This dataframe should include a row for the DataExportTag based
#' response column names, another for the Question Text stem and choice text (although
#' truncated), and a row with QID based column names.
#' @param blocks A list of the survey's blocks, with the questions included in them
#' @return The choice text corresponding to a response column
choice_text_from_response_column <-
  function(response_column,
           original_first_row,
           blocks) {
    # get the question's place in the blocks from the response column,
    # save the indices needed to refer to the question in the blocks list,
    # save the raw question text,
    # and clean it of HTML tags
    question_indices <-
      question_from_response_column(blocks, response_column)
    if (is.null(question_indices))
      return("")
    i <- question_indices[[1]]
    j <- question_indices[[2]]
    question_text <-
      blocks[[i]][['BlockElements']][[j]][['Payload']][['QuestionText']]
    question_text <- clean_html(question_text)

    # get the first-row-entry from the responses for the given response column,
    # count the number of dashes in the cleaned question text,
    # and count the number of dashes in the first-row-entry.
    # NOTE: counting the dashes in the question text is limited to the first 99
    # characters, since the question is cut off in the first row after 99
    # characters.
    if (!response_column %in% colnames(original_first_row))
      return("")
    first_row_entry <-
      enc2native(as.character(original_first_row[response_column][1, ]))
    stem_dashes <- gregexpr("-", substr(question_text, 1, 99))[[1]]
    stem_dash_n <- length(which(stem_dashes > 0))
    first_row_dashes <- gregexpr("-", first_row_entry)[[1]]
    first_row_dash_n <- length(which(first_row_dashes > 0))

    # if the number of dashes in the first-row-entry is the same as
    # the number of dashes in the question stem, then the choice text
    # for the response column can be set to blank.
    # if the number of dashes in the first-row-entry is greater
    # than the number of dashes in the question stem, then
    # the choice text for that response column should be set to
    # the text of the first-row-entry after the appropriate
    # number of dashes
    if (first_row_dash_n > stem_dash_n) {
      choice_text <-
        substr(first_row_entry,
               first_row_dashes[[stem_dash_n + 1]] + 1,
               nchar(first_row_entry))
      choice_text <- clean_html(choice_text)

    } else {
      choice_text <- ""
    }

    return(choice_text)
  }


#' Block's Header to HTML
#'
#' Get an HTML Header for a list of survey blocks. The header
#' is created by either get_coded_questions_and_blocks or
#' by split_respondents.
#'
#' @param blocks A list of blocks with a 'header' inserted by either the
#' get_coded_questions_and_blocks or split_respondents functions.
#' @return An HTML string that can be added as a section header in survey reports.
blocks_header_to_html <- function(blocks) {
  header <- c("<h4>",
              paste(blocks[['header']][1:2], collapse = "<br>"))
  if (length(blocks[['header']]) > 2) {
    header <- c(header,
                "<br><br>",
                paste(blocks[['header']][3:length(blocks[['header']])], collapse =
                        "<br>"),
                "</h4><br>")
  }
  header <- c(header,
              "</h4></br>")
  return(header)
}


#' Count the Number of Blocks
#'
#' Since the blocks list is used to transport some additional information
#' beyond the contents of the survey questions, this function is here to
#' help in counting how many valid question blocks there are.
#' Any real question blocks will be enumerated (aka numbered) in R, as opposed to the
#' content that's been added which will be named (with a string). This means that when
#' looking at the names of the blocks list, the integer values or the values which
#' have no name are the question blocks, and the values which have names are the
#' information added by the QualtricsTools functions. This function counts up the former.
#'
#' @param blocks A list of blocks
#' @return the number of question blocks
number_of_blocks <- function(blocks) {
  if (is.null(names(blocks))) {
    return(length(blocks))
  } else {
    as_ints <- sapply(names(blocks), function(x) {
      (!is.na(suppressWarnings(as.integer(x)))) || (x == "")
    })
    block_length <- length(which(as_ints))
    return(block_length)
  }
}

#' Generate a List of Questions from Those Contained in the Blocks
#'
#' This function iterates through the blocks and anything that has a DataExportTag
#' is added to a list of questions, and it returns that list of questions from
#' the blocks.
#' @inheritParams question_from_response_column
questions_from_blocks <- function(blocks) {
  questions <- list()
  e <- 1
  for (i in 1:length(blocks)) {
    if ('BlockElements' %in% names(blocks[[i]])) {
      for (j in 1:length(blocks[[i]][['BlockElements']])) {
        if ('Payload' %in% names(blocks[[i]][['BlockElements']][[j]]) &&
            'DataExportTag' %in% names(blocks[[i]][['BlockElements']][[j]][['Payload']])) {
          questions[[e]] <- blocks[[i]][['BlockElements']][[j]]
          e <- e + 1
        }
      }
    }
  }
  return(questions)
}

#' Get the Flow out of the Survey
#'
#' The 'Flow' is a list of Block IDs in the order that they are presented
#' in the survey as it is taken by a respondent. The flow list that is returned
#' from this function is used by functions like text_appendices_table and
#' tabelize_blocks to get the ordering of the survey in the preview correct.
#' @param survey A qualtrics survey list object,
#' uploaded from a Qualtrics Survey File (QSF). Use
#' ask_for_qsf() to create such a survey list object from a QSF file.
#' @return A list of strings identifying the blocks in the order that they appear
#' within the survey.
flow_from_survey <- function(survey) {
  flow <-
    which(sapply(survey[['SurveyElements']], function(x)
      x[['Element']] == "FL"))
  flow <-
    sapply(survey[['SurveyElements']][[flow]][['Payload']][['Flow']], function(x)
      if ('ID' %in% names(x)) {
        x[['ID']]
      } else if ('Flow' %in% names(x)) {
        sapply(x[['Flow']], function(y)
          if ('ID' %in% names(y))
            y[['ID']])
      })
  flow <- unlist(flow)
  return(flow)
}



#' Export a file containing the results tables
#'
#' `make_results_tables` uses `get_setup` and `html_2_pandoc` to process a
#' survey and then save its results into a file. If the `qsf_path,` and `csv_path`
#' are included as parameters, then they will be passed to `get_setup` along with a
#' `return_data=TRUE` parameter in order to return the survey, responses,
#' questions, blocks, original_first_rows, and flow as variables local to the function
#' scope. If they are not passed, they will be retrieved as needed from the global scope.
#' The function then uses the blocks, original_first_rows, and flow with `html_2_pandoc`
#' to produce the desired output file.
#'
#' @param qsf_path (optional) is the string path location of the .qsf file to be processed.
#' @param csv_path (optional) is the string path location of the .csv file to be processed.
#' @param headerrows (optional) specifies the number of header rows in the CSV data.
#' @param output_dir specifies the path of the directory to save the output file in.
#' @param filename specifies the name of the output file.
make_results_tables <-
  function(qsf_path,
           csv_path,
           headerrows,
           output_dir,
           filename = 'Results Tables.docx') {
    # Either use the passed parameters or interactively get setup with the survey data.
    if (!any(c(missing(qsf_path), missing(csv_path)))) {
      qt_vals = get_setup(
        qsf_path = qsf_path,
        csv_path = csv_path,
        headerrows = headerrows,
        return_data = TRUE
      )
    } else {
      if (exists('survey', 'responses', envir=globalenv()))
        qt_vals = get_setup(already_loaded=TRUE, return_data=TRUE)
      else qt_vals = get_setup(return_data=TRUE)
    }
    # We used return_data=TRUE, so the data came back as a single
    # list which needs to be processed into individual variables.
    varnames = c('survey',
                 'responses',
                 'questions',
                 'blocks',
                 'original_first_rows',
                 'flow')
    for (i in 1:length(varnames)) {
      assign(varnames[[i]], qt_vals[[i]])
    }
    original_first_rows = as.data.frame(original_first_rows)
    responses = as.data.frame(responses)

    # Now we render the HTML into a report.
    html_2_pandoc(
      html = c(
        blocks_header_to_html(blocks),
        tabelize_blocks(blocks, flow)
      ),
      file_name = filename,
      output_dir = output_dir
    )
  }


#' Export a file containing the text appendices
#'
#' make_text_appendices uses get_setup and html_2_pandoc to process a
#' survey and then save its results into a file. If the qsf_path, and csv_path
#' are included as parameters, then they will be passed to get_setup along with a
#' return_data=TRUE parameter in order to return the survey, responses,
#' questions, blocks, original_first_rows, and flow as variables local to the function
#' scope. If they are not passed, they will be retrieved as needed from the global scope.
#' The function then uses the blocks, original_first_rows, and flow with html_2_pandoc
#' to produce the desired output file.
#'
#' @inheritParams make_results_tables
make_text_appendices <-
  function(qsf_path,
           csv_path,
           headerrows,
           output_dir,
           filename = 'Text Appendices.docx') {
    # Either use the passed parameters or interactively get setup with the survey data.
    if (!any(c(missing(qsf_path), missing(csv_path)))) {
      qt_vals = get_setup(
        qsf_path = qsf_path,
        csv_path = csv_path,
        headerrows = headerrows,
        return_data = TRUE
      )
    } else {
      if (exists('survey', 'responses', envir=globalenv()))
        qt_vals = get_setup(already_loaded=TRUE, return_data=TRUE)
      else qt_vals = get_setup(return_data=TRUE)
    }
    # We used return_data=TRUE, so the data came back as a single
    # list which needs to be processed into individual variables.
    varnames = c('survey',
                 'responses',
                 'questions',
                 'blocks',
                 'original_first_rows',
                 'flow')
    for (i in 1:length(varnames)) {
      assign(varnames[[i]], qt_vals[[i]])
    }
    original_first_rows = as.data.frame(original_first_rows)
    responses = as.data.frame(responses)

    # Now we render the HTML into a report.
    html_2_pandoc(
      html = c(
        blocks_header_to_html(blocks),
        text_appendices_table(blocks, original_first_rows, flow)
      ),
      file_name = filename,
      output_dir = output_dir
    )
  }


#' Create Text Appendices including Coded Comments
#'
#' Using `get_setup`, `directory_get_coded_comment_sheets`, `format_coded_comment_sheets`,
#' `insert_coded_comments`, and `html_2_pandoc`, this function renders
#' text appendices with coded comments included from CSV or XLSX files
#' from the specified `sheets_dir` parameter.
#'
#' @inheritParams make_results_tables
#' @param sheets_dir is the string path location of the directory which contains Excel documents
#' with a "Coded" sheet formatted as specified on the wiki:
#' https://github.com/ctesta01/QualtricsTools/wiki/Comment-Coding
#' @param n_threshold is the number of verbatim comments which will appear before being truncated.
make_coded_comments <-
  function(qsf_path,
           csv_path,
           headerrows,
           sheets_dir,
           output_dir,
           filename = 'text appendices.docx',
           n_threshold = 15
  ) {
    # Declares paths for the qsf and csv files
    if (!any(c(missing(qsf_path), missing(csv_path)))) {
      qt_vals = get_setup(
        qsf_path = qsf_path,
        csv_path = csv_path,
        headerrows = headerrows,
        return_data = TRUE
      )
      varnames = c('survey',
                   'responses',
                   'questions',
                   'blocks',
                   'original_first_rows',
                   'flow')
      for (i in 1:length(varnames)) {
        assign(varnames[[i]], qt_vals[[i]])
      }
      original_first_rows = as.data.frame(original_first_rows)
      responses = as.data.frame(responses)
    }

    coded_sheets <- directory_get_coded_comment_sheets(sheets_dir)

    if (is.null(coded_sheets)) {
      stop("Please fix errors before attempting again")
    }

    comment_tables <-
      format_coded_comment_sheets(coded_comment_sheets = coded_sheets)
    blocks <-
      insert_coded_comments(
        blocks = blocks,
        original_first_rows = original_first_rows,
        coded_comments = comment_tables
      )

    # Used with html_2_pandoc below to keeps the flow of the survey consistent with the output
    flow = flow_from_survey(survey)

    html_2_pandoc(
      html = c(
        blocks_header_to_html(blocks),
        text_appendices_table(
          blocks = blocks,
          original_first_row = original_first_rows,
          flow = flow,
          n_threshold = n_threshold
        )
      ),
      file_name = filename,
      output_dir = output_dir
    )
  }


#' Generate Results Tables Reports Split (or Grouped By) their entries in a Response Column
#'
#' The make_split_results_table function works by constructing and inserting an additional
#' column into the responses data frame from which the responses are split. Once the responses
#' are split, they are inserted into distinct lists of blocks (one list for each split group of
#' responses) and then results tables reports are rendered from these split blocks. The function
#' renders these reports by looping over the list of split blocks, naming each report according to
#' its split respondent group, and saving each file to the specified output_dir. At the simplest
#' level, this function is about running get_setup, create_merged_response_column,
#' split_respondents, and html_2_pandoc in the right way to produce split reports.
#'
#' @inheritParams make_results_tables
#' @param split_by is a list which specifies which columns should be used to split the respondents.
make_split_results_tables <-
  function(qsf_path,
           csv_path,
           output_dir,
           split_by,
           headerrows = 3) {
    # Load the Survey Data
    if (!any(c(missing(qsf_path), missing(csv_path)))) {
      qt_vals = get_setup(
        qsf_path = qsf_path,
        csv_path = csv_path,
        headerrows = headerrows,
        return_data = TRUE
      )
    } else {
      if (exists('survey', 'responses', envir=globalenv()))
        qt_vals = get_setup(already_loaded=TRUE, return_data=TRUE)
      else qt_vals = get_setup(already_loaded=FALSE, return_data=TRUE)
    }

    varnames = c('survey',
                 'responses',
                 'questions',
                 'blocks',
                 'original_first_rows',
                 'flow')
    for (i in 1:length(varnames)) {
      assign(varnames[[i]], qt_vals[[i]])
    }
    original_first_rows = as.data.frame(original_first_rows)
    responses = as.data.frame(responses)

    # This turns the split_by list into a name for the column
    # which will contain the concatenation of the entries of responses
    # which are being split over. That is if split_by = c('column1', 'column2', 'column3'),
    # then this constructs split_string = 'column1-column2-column3'
    split_string <- c(split_by, "split")
    split_string <- toString(paste(split_string, "-"))
    split_string <- gsub(' ', '', split_string)
    split_string <- gsub(',', '', split_string)
    split_string <- substr(split_string, 1, nchar(split_string) - 1)

    # Merges the selected columns into one name
    # In this case School, DegType, and Porgram merged into school-degtype-program
    responses <-
      create_merged_response_column(split_by, split_string, blocks, responses)

    split_blocks <-
      split_respondents(
        response_column = split_string,
        responses = responses,
        survey = survey,
        blocks = blocks,
        questions = questions,
        headerrows = headerrows,
        already_loaded = FALSE,
        original_first_rows
      )

    # Used with html_2_pandoc below to keeps the flow of the survey consistent with the output
    flow = flow_from_survey(survey)

    # Appends .docx to the file names collected by splitting the data to output them as Word Documents
    filenames <- sapply(split_blocks, function(x)
      x$split_group)
    filenames <- sapply(filenames, function(x)
      paste0(x, '.docx'))

    # Outputs the data to word documents using html_2_pandoc
    return_list <- c()
    for (i in 1:length(filenames)) {
      outpath <- html_2_pandoc(
        html = c(
          blocks_header_to_html(split_blocks[[i]]),
          tabelize_blocks(
            blocks = split_blocks[[i]],
            flow = flow
          )
        ),
        file_name = filenames[[i]],
        output_dir = output_dir
      )
      return_list <- c(return_list, outpath)
    }
    return(return_list)
  }

#' Generate Results Tables Reports Split (or Grouped By) their entries in a Response Column
#'
#' The make_split_text_appendices function works by constructing and inserting an additional
#' column into the responses data frame from which the responses are split. Once the responses
#' are split, they are inserted into distinct lists of blocks (one list for each split group of
#' responses) and then text appendices are rendered from these split blocks. The function
#' renders these reports by looping over the list of split blocks, naming each report according to
#' its split respondent group, and saving each file to the specified output_dir. At the simplest
#' level, this function is about running get_setup, create_merged_response_column,
#' split_respondents, and html_2_pandoc in the right way to produce split reports.
#' @inheritParams make_split_results_tables
make_split_text_appendices <-
  function(qsf_path,
           csv_path,
           output_dir,
           split_by,
           n_threshold = 15,
           headerrows = 3) {
    # Load the Survey Data
    if (!any(c(missing(qsf_path), missing(csv_path)))) {
      qt_vals = get_setup(
        qsf_path = qsf_path,
        csv_path = csv_path,
        headerrows = headerrows,
        return_data = TRUE
      )
    } else {
      if (exists('survey', 'responses', envir=globalenv()))
        qt_vals = get_setup(already_loaded=TRUE, return_data=TRUE)
      else qt_vals = get_setup(already_loaded=FALSE, return_data=TRUE)
    }

    varnames = c('survey',
                 'responses',
                 'questions',
                 'blocks',
                 'original_first_rows',
                 'flow')
    for (i in 1:length(varnames)) {
      assign(varnames[[i]], qt_vals[[i]])
    }
    original_first_rows = as.data.frame(original_first_rows)
    responses = as.data.frame(responses)

    # This turns the split_by list into a name for the column
    # which will contain the concatenation of the entries of responses
    # which are being split over. That is if split_by = c('column1', 'column2', 'column3'),
    # then this constructs split_string = 'column1-column2-column3'
    split_string <- c(split_by, "split")
    split_string <- toString(paste(split_string, "-"))
    split_string <- gsub(' ', '', split_string)
    split_string <- gsub(',', '', split_string)
    split_string <- substr(split_string, 1, nchar(split_string) - 1)

    # Merges the selected columns into one name
    # In this case School, DegType, and Porgram merged into school-degtype-program
    responses <-
      create_merged_response_column(split_by, split_string, blocks, responses)

    split_blocks <-
      split_respondents(
        response_column = split_string,
        responses = responses,
        survey = survey,
        blocks = blocks,
        questions = questions,
        headerrows = headerrows,
        already_loaded = FALSE,
        original_first_rows
      )

    # Used with html_2_pandoc below to keeps the flow of the survey consistent with the output
    flow = flow_from_survey(survey)

    # Appends .docx to the file names collected by splitting the data to output them as Word Documents
    filenames <- sapply(split_blocks, function(x)
      x$split_group)
    filenames <- sapply(filenames, function(x)
      paste0(x, '.docx'))

    # Outputs the data to word documents using html_2_pandoc
    return_list <- c()
    for (i in 1:length(filenames)) {
      outpath <- html_2_pandoc(
        html = c(
          blocks_header_to_html(split_blocks[[i]]),
          text_appendices_table(
            blocks = split_blocks[[i]],
            original_first_row = original_first_rows,
            flow = flow,
            n_threshold = n_threshold
          )
        ),
        file_name = filenames[[i]],
        output_dir = output_dir
      )
      return_list <- c(return_list, outpath)
    }
    return(return_list)
  }



#' Split a Survey's Split Coded Comment Appendices
#'
#' This question automates the entire process of splitting a
#' survey's text appendices by specific response columns. The QSF
#' and CSV file are passed as string arguments,
#' the sheets_dir specifies where the coded comments excel or csv
#' data is stored, and the output_dir specifies where the split
#' coded comment appendices should be saved. The n_threshold
#' specifies how many coded comments there must be before the coded
#' comment appendices are included, and headerrows is an argument
#' necessary to process the survey results correctly.
#' @inheritParams make_coded_comments
#' @inheritParams make_split_results_tables
make_split_coded_comments <-
  function(qsf_path,
           csv_path,
           sheets_dir,
           output_dir,
           split_by,
           n_threshold = 15,
           headerrows) {
    # This turns the split_by list into a name for the column
    # which will contain the concatenation of the entries of responses
    # which are being split over. That is if split_by = c('column1', 'column2', 'column3'),
    # then this constructs split_string = 'column1-column2-column3'
    split_string <- c(split_by, "split")
    split_string <- toString(paste(split_string, "-"))
    split_string <- gsub(' ', '', split_string)
    split_string <- gsub(',', '', split_string)
    split_string <- substr(split_string, 1, nchar(split_string) - 1)

    # Declares paths for the qsf and csv files
    if (!any(c(missing(qsf_path), missing(csv_path)))) {
      qt_vals = get_setup(
        qsf_path = qsf_path,
        csv_path = csv_path,
        headerrows = headerrows,
        return_data = TRUE
      )
    } else {
      if (exists('survey', 'responses', envir=globalenv()))
        qt_vals = get_setup(already_loaded=TRUE, return_data=TRUE)
      else qt_vals = get_setup(return_data=TRUE)
    }
    varnames = c('survey',
                 'responses',
                 'questions',
                 'blocks',
                 'original_first_rows',
                 'flow')
    for (i in 1:length(varnames)) {
      assign(varnames[[i]], qt_vals[[i]])
    }
    original_first_rows = as.data.frame(original_first_rows)
    responses = as.data.frame(responses)


    # Merges the selected columns into one name
    # In this case School, DegType, and Porgram merged into school-degtype-program
    responses <-
      create_merged_response_column(split_by, split_string, blocks, responses)

    coded_sheets <- directory_get_coded_comment_sheets(sheets_dir)

    if (is.null(coded_sheets)) {
      stop("Please fix errors before attempting again")
    }

    split_comment_tables <-
      format_and_split_comment_sheets(coded_sheets, responses, split_string)

    split_blocks <-
      split_respondents(
        response_column = split_string,
        responses = responses,
        survey = survey,
        blocks = blocks,
        questions = questions,
        headerrows = headerrows,
        already_loaded = FALSE,
        original_first_rows
      )

    split_blocks <-
      insert_split_survey_comments(split_blocks,
                                   split_comment_tables,
                                   split_string,
                                   original_first_rows)

    #Used with html_2_pandoc below to keeps the flow of the survey consistent with the output
    flow = flow_from_survey(survey)

    #Appends .docx to the file names collected by splitting the data to output them as Word Documents
    filenames <- sapply(split_blocks, function(x)
      x$split_group)
    filenames <- sapply(filenames, function(x)
      paste0(x, '.docx'))

    #Outputs the data to word documents using html_2_pandoc
    return_list <- c()
    for (i in 1:length(filenames)) {
      outpath <- html_2_pandoc(
        html = c(
          blocks_header_to_html(split_blocks[[i]]),
          text_appendices_table(
            blocks = split_blocks[[i]],
            original_first_row = original_first_rows,
            flow = flow,
            n_threshold = n_threshold
          )
        ),
        file_name = filenames[[i]],
        output_dir = output_dir
      )
      return_list <- c(return_list, outpath)
    }
    return(return_list)
  }
