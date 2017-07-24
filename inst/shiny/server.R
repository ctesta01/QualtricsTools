options(shiny.maxRequestSize = 30 * 1024 ^ 2)

shinyServer(function(input, output) {

  # reactiveValues are values which, similar to the input values,
  # cause any reactive block which depends on them to recalculate its output.
  # Here we are constructing the values[['unselected_questions']] list initially
  # as an empty list. This list is updated in an observe block below whenever
  # users specify questions to be included and excluded.
  values <- reactiveValues(unselected_questions = c())

  # The survey_and_responses reactive block reads the input files
  # and loads them as the survey and responses. It validates that there
  # are no duplicate data export tags in the survey, and it returns a
  # list with three elements:
  # 1. the processed survey,
  # 2. the responses, and
  # 3. the original_first_rows.
  survey_and_responses <- reactive({

    survey <- load_qsf_data(input[['file1']])

    # If there are questions which are unselected, meaning they've been set to
    # be excluded, go through the survey and mark these questions with the
    # qtSkip flag such that the reports do not include them.
    if (!is.null(values[['unselected_questions']])) {
      for (i in 1:length(survey[['SurveyElements']])) {
        if ('DataExportTag' %in% names(survey[['SurveyElements']][[i]][['Payload']]) &&
            survey[['SurveyElements']][[i]][['Payload']][['DataExportTag']] %in% values[['unselected_questions']]) {
          survey[['SurveyElements']][[i]][['qtSkip']] <- TRUE
        }
      }
    }

    # Check that the uploaded survey contains no duplicate questions.
    questions <- questions_from_survey(survey)
    blocks <- blocks_from_survey(survey)
    questions <- remove_trash_questions(questions, blocks)
    duplicates <-
      questions[which(duplicated(sapply(questions, function(x)
        x$Payload$DataExportTag)))]
    duplicate_tags <-
      sapply(duplicates, function(x)
        x$Payload$DataExportTag)
    validate(need(
      validate_data_export_tags(questions),
      paste0(
        "Please submit a survey with no duplicate question IDs.
        The following questions were duplicated: ",
        paste(duplicate_tags, collapse = ", ")
      )
    ))

    # Use the input checkbox to set the number of headerrows appropriately.
    if (input[['insights_or_not']] == TRUE)
      headerrows <- 3
    if (input[['insights_or_not']] == FALSE)
      headerrows <- 2

    # load_csv_data returns a pair of two elements, the responses and
    # the original_first_rows.
    responses <- load_csv_data(input$file2, input$file1, headerrows)
    original_first_rows <- responses[[2]]
    responses <- responses[[1]]

    # Construct the output and return it.
    list_survey_and_responses <- list()
    list_survey_and_responses[[1]] <- survey
    list_survey_and_responses[[2]] <- responses
    list_survey_and_responses[[3]] <- original_first_rows
    return(list_survey_and_responses)
  })

  # This is a reactive block wrapped around the get_coded_questions_and_blocks
  # function. The get_coded_questions_and_blocks function cleans up the questions and
  # blocks from the QSF data, adds the response data into the questions, and the
  # questions into the blocks, as well as removing trash questions and adding a
  # human readable question type.
  processed_questions_and_blocks <- reactive({
    if (length(survey_and_responses()) >= 3) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      original_first_rows <- survey_and_responses()[[3]]
      questions_and_blocks <-
        get_coded_questions_and_blocks(survey, responses, original_first_rows)
    }
  })

  # create the responses with a merged response column for splitting respondents
  # This block creates a column over which to split the respondents into distinct
  # factors of the column. The column is constructed from a list of column names,
  # given by the input[['split_response_columns']], and the entries of each are
  # merged by the create_merged_response_column into a new column with entries
  # of the format "Column 1 Data + Column 2 Data + ..." for each selected column.
  split_col_responses <- reactive({
    validate(need(
      length(survey_and_responses()) >= 3,
      "Please upload the survey and responses"
    ))
    if (length(survey_and_responses()) >= 3) {
      if ('split_response_columns' %in% names(input) &&
          !is.null(input[['split_response_columns']])) {
        responses <- survey_and_responses()[[2]]
        blocks <- processed_questions_and_blocks()[[2]]
        split_cols <- input[['split_response_columns']]
        responses <- create_merged_response_column(
          response_columns = split_cols,
          col_name = paste0(c("split", split_cols), collapse = " "),
          survey_responses = responses,
          question_blocks = blocks
        )
      } else
        return(survey_and_responses()[[2]])
    }
  })

  # This block uses the split column constructed by the split_col_responses
  # reactive block in order to split the respondents into distinct
  # reports. Each distinct group of respondents for a given factor
  # in the splitting column generated by split_col_responses is taken as
  # its own set of responses, the original survey blocks are duplicated
  # as many times as there are factors in the splitting column, and then
  # each split response set is inserted into the duplicated survey blocks,
  # creating a list of blocks each of which represents different subsets
  # of respondents to the survey. This process is done by the split_respondents
  # function.
  split_blocks <- reactive({
    validate(need(
      length(survey_and_responses()) >= 3,
      "Please upload the survey and responses"
    ))
    if ('split_response_columns' %in% names(input) &&
        !is.null(input[['split_response_columns']])) {
      survey <- survey_and_responses()[[1]]
      original_first_rows <- survey_and_responses()[[3]]
      questions <- processed_questions_and_blocks()[[1]]
      blocks <- processed_questions_and_blocks()[[2]]
      responses <- split_col_responses()
      split_cols <- input[['split_response_columns']]
      if (input[['insights_or_not']] == TRUE)
        headerrows <- 3
      if (input[['insights_or_not']] == FALSE)
        headerrows <- 2
      split_respondents(
        response_column = paste0(c("split", split_cols), collapse = " "),
        survey = survey,
        responses = responses,
        blocks = blocks,
        questions = questions,
        headerrows = headerrows,
        original_first_rows = original_first_rows
      )
    } else
      return(NULL)
  })

  # Once a user has specified that they would like to split the respondents
  # into subgroups based on the factors of a splitting column, they must
  # select one of the factors to view in the app. The information describing
  # which subset of respondents a given split block corresponds to is inserted
  # into the header of each split block by the split_respondents function,
  # and so this function reads this data and strips it for the relevant
  # factor. When the user makes the selection of what block they would like to
  # view the reports for, they are updating the input[['split_respondents_group']]
  # value, and so we check the block headers against this value to find the
  # split_blocks to use to create the reports the user has chosen to view.
  choose_split_block <- reactive({
    validate(need(
      length(survey_and_responses()) >= 3,
      "Please upload the survey and responses"
    ))
    if ('split_respondents_group' %in% names(input) &&
        input[['split_respondents_group']] != "" &&
        !is.null(split_blocks())) {
      block_respondent_groups <- sapply(split_blocks(), function(x) {
        header <- x[['header']][[3]]
        header <-
          gsub("^Respondents with\\s", "", header, perl = TRUE)
        split_cols <- input[['split_response_columns']]
        header_second_half <-
          paste0("\\sin the ", paste0(c("split", split_cols), collapse = " "), " column")
        header <- gsub(header_second_half, "", header, perl = TRUE)
      })

      matching_blocks <-
        which(block_respondent_groups == input[['split_respondents_group']])
      if (length(matching_blocks) == 1) {
        return(split_blocks()[[matching_blocks[[1]]]])
      } else
        return(NULL)
    } else
      return(NULL)
  })


  # The uncodeable_message reactive block reacts to the survey_and_responses() block
  # with a message indicating which, if any, questions were not properly processed.
  uncodeable_message <- reactive({
    validate(need(
      length(survey_and_responses()) >= 3,
      "Please upload the survey and responses"
    ))
    if (length(survey_and_responses()) >= 3) {
      questions <- processed_questions_and_blocks()[[1]]
      uncodeable_questions_message(questions)
    }
  })

  # The results_tables reactive block reacts to the survey_and_responses output
  # by processing the survey and responses into blocks with results tables inserted,
  # and then converting the results tables to HTML tables.
  results_tables <- reactive({
    validate(need(length(survey_and_responses()) >= 3, ""))
    if (length(survey_and_responses()) >= 3) {
      survey <- survey_and_responses()[[1]]
      flow <- flow_from_survey(survey)
      blocks <- processed_questions_and_blocks()[[2]]
      if (!is.null(choose_split_block()))
        blocks <- choose_split_block()
      if (input[['ignoreflow']] == FALSE) {
        return(c(
          blocks_header_to_html(blocks),
          create_html_results_tables(blocks, flow)
        ))
      } else {
        return(c(
          blocks_header_to_html(blocks),
          create_html_results_tables(blocks)
        ))
      }
    }
  })

  # The question_dictionary block uses the survey from the survey_and_responses output
  # to create a data frame detailing each survey question. This depends on two following
  # reactive code-blocks, which are the complete_question_dictionary and uncodeable_question_dictionary.
  # If the user selects the checkbox which allows them to look at the questions which were
  # not automatically processed, then they get the uncodeable_question_dictionary.
  question_dictionary <- reactive({
    if (input[['uncodeable-only']] == TRUE) {
      return(unprocessed_question_dictionary())
    } else {
      return(complete_question_dictionary())
    }
  })

  # The complete_question_dictionary reactive block uses the create_response_column_dictionary
  # function to create a data frame which will be rendered in the UI using DataTables.js
  # This reactive block is also used to create the Include/Exclude page's datatable.
  complete_question_dictionary <- reactive({
    validate(need(length(survey_and_responses()) >= 3, ""))
    if (length(survey_and_responses()) >= 3) {
      flow <- flow_from_survey(survey_and_responses()[[1]])
      original_first_row <- survey_and_responses()[[3]][1,]
      blocks <- processed_questions_and_blocks()[[2]]
      return(create_response_column_dictionary(blocks, flow, original_first_row))
    }
  })

  # The unprocessed_question_dictionary either creates a message that all questions were
  # successfully processed, or creates a dataframe similar to the complete_question_dictionary
  # except with only information for questions which were not successfully processed.
  unprocessed_question_dictionary <- reactive({
    validate(need(length(survey_and_responses()) >= 3, ""))
    if (length(survey_and_responses()) >= 3) {
      original_first_row <- survey_and_responses()[[3]][1,]
      blocks <- processed_questions_and_blocks()[[2]]
      uncode_qdict <- uncodeable_question_dictionary(blocks)
      if (is.null(uncode_qdict)) {
        success_message <-
          data.frame("All questions were successfully processed!")
        colnames(success_message)[1] <- " "
        return(success_message)
      } else {
        return(uncode_qdict)
      }
    }
  })

  # This reactive block renders the HTML for the text appendices panel in the processed results
  # page. The bulk of the hard work is done in the text_appendices_table function, and the
  # blocks_header_to_html is what creates the header at the top of the document.
  text_appendices <- reactive({
    validate(need(
      length(survey_and_responses()) >= 3,
      "Please upload the survey and responses"
    ))
    if (length(survey_and_responses()) >= 3) {
      original_first_rows <- survey_and_responses()[[3]]
      blocks <- processed_questions_and_blocks()[[2]]
      original_first_row <- original_first_rows[1, ]
      survey <- survey_and_responses()[[1]]
      flow <- flow_from_survey(survey)
      if (!is.null(choose_split_block()))
        blocks <- choose_split_block()
      if (input[['ignoreflow']] == FALSE) {
        return(c(
          blocks_header_to_html(blocks),
          text_appendices_table(blocks, original_first_row, flow)
        ))
      } else {
        return(c(
          blocks_header_to_html(blocks),
          text_appendices_table(blocks, original_first_row)
        ))
      }
    }
  })

  # This reactive block generates HTML tables detailing the display logic of each
  # question.
  display_logic <- reactive({
    validate(need(length(survey_and_responses()) >= 1, "Please upload a survey"))
    if (length(survey_and_responses()) >= 1) {
      survey <- survey_and_responses()[[1]]
      flow <- flow_from_survey(survey)
      blocks <- blocks_from_survey(survey)
      questions <- questions_from_survey(survey)
      questions <- remove_trash_questions(questions, blocks)
      questions <- clean_question_text(questions)
      blocks <- remove_trash_blocks(blocks)
      blocks <- questions_into_blocks(questions, blocks)
      tabelize_display_logic(blocks, flow)
    }
  })

  # The include_exclude_dict constructs a dataframe with some HTML in its leftmost column
  # to add checkboxes to each row. The complete_question_dictionary is filtered for the columns
  # which uniquely represent a question (whereas the complete_question_dictionary itself
  # contains rows which represent response columns). If a question appears in the
  # values[['unselected_questions']] list then it is set to be already unselected, and
  # otherwise it is set to be checked for inclusion in the reports.
  include_exclude_dict <- reactive({
    validate(need(
      length(survey_and_responses()) >= 3,
      "Please upload the survey and responses"
    ))
    qdict <- unique(complete_question_dictionary()[c(1, 3, 5, 6, 7)])
    check_list <-
      lapply(qdict[[1]], function(x)
        ifelse(x %in% values[['unselected_questions']], "", " checked "))
    addCheckboxButtons <-
      paste0(
        '<input type="checkbox" name="unselected_questions_',
        qdict[[1]],
        '" value="',
        qdict[[1]],
        '"',
        check_list,
        '>',
        ""
      )
    #Display table with checkbox buttons
    cbind(Include = addCheckboxButtons, qdict)
  })

  # When a user clicks the submit button in the Include/Exclude Questions page,
  # there is a JavaScript function which updates the input[['unselected_questions']]
  # and input[['selected_questions']]. However, since it is JavaScript, it only has
  # access to elements which exist on the page -- which means only the checkboxes
  # displayed on the page that the user is viewing in the DataTable of questions.
  # If there are multiple pages in the DataTable of questions to include or exclude,
  # then there will be questions which do not appear in either
  # input[['selected_questions']] or input[['unselected_questions']].
  # To ensure that the questions which had previously been excluded on another page
  # remain excluded, we go through the input[['unselected_questions']] and add
  # each of these to the values[['unselected_questions']] if it's not already there,
  # and for each of the input[['selected_questions']] we remove them from the
  # values[['unselected_questions']] if they appear there.
  observeEvent(input$submit, {
    for (q in input[['unselected_questions']]) {
      if (! q %in% values[['unselected_questions']]) {
        values[['unselected_questions']] <- c(values[['unselected_questions']], q)
      }
    }
    for (q in input[['selected_questions']]) {
      if (q %in% values[['unselected_questions']]) {
        index <- which(values[['unselected_questions']] == q)
        values[['unselected_questions']] <- values[['unselected_questions']][-index]
      }
    }
  })

  # Output each tabpanels' corresponding HTML contents generated above
  # in reactive_blocks.
  output[['uncodeable_message']] <-
    renderUI(HTML(uncodeable_message()))

  output[['results_tables']] <-
    renderUI(div(HTML(results_tables()), class = "shiny-html-output"))

  output[['question_dictionary']] <-
    renderDataTable(question_dictionary(),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 10,
                      autoWidth = TRUE
                    ))

  output[['text_appendices']] <-
    renderUI(div(HTML(text_appendices()), class = "shiny-html-output"))

  output[['display_logic']] <-
    renderUI(div(HTML(display_logic()), class = "shiny-html-output"))

  output[['select_qdict']] = renderDataTable({
    include_exclude_dict()
  }, options =
    list(
      orderClasses = TRUE,
      lengthMenu = c(5, 25, 50, 100),
      pageLength = 25
    ), escape = FALSE)


  # selectize response columns for splitting respondents
  output[['select_response_columns']] <- renderUI({
    selectInput(
      'split_response_columns',
      'Response Columns',
      colnames(survey_and_responses()[[2]]),
      multiple = TRUE,
      selectize = TRUE
    )
  })

  # select respondent group to view
  output[['select_respondent_group']] <- renderUI({
    split_cols <- input[['split_response_columns']]
    qt_split_col <- paste0(c("split", split_cols), collapse = " ")
    if (qt_split_col %in% colnames(split_col_responses())) {
      choices <- unique(split_col_responses()[[qt_split_col]])
    } else
      choices <- c('')
    selectInput('split_respondents_group',
                'Split Respondents Group',
                c("", choices))
  })

  # output the breakdown of the respondent groups
  output[['table_respondent_groups']] <- renderTable({
    split_cols <- input[['split_response_columns']]
    qt_split_col <- paste0(c("split", split_cols), collapse = " ")
    if (qt_split_col %in% colnames(split_col_responses())) {
      factor_table <- table(factor(split_col_responses()[[qt_split_col]]))
      factor_table <- as.data.frame(factor_table)
      if (ncol(factor_table) >= 2) {
        colnames(factor_table) <- c("Respondent Group", "N")
        factor_table
      }
    }
  }, include.rownames = FALSE)


  # Download Buttons
  # The next several blocks are entirely dedicated to creating filenames
  # for output files and creating the downloadHandlers for each file to download.

  download_names <- reactive({
    dnames <- list()
    dnames['results_tables'] <-
      paste0("results_tables.", input[['rt_format']])
    dnames['qdict'] <-
      paste0('question_dictionary.', input[['qd_format']])
    dnames['text_appendices'] <-
      paste0('text_appendices.', input[['ta_format']])
    dnames['display_logic'] <-
      paste0('display_logic.', input[['dl_format']])
    return(dnames)
  })

  # Download Results Tables
  output[['downloadResultsTables']] <- downloadHandler(
    filename = function() {
      download_names()[['results_tables']]
    },
    content = function(file) {
      pandoc_output = html_2_pandoc(
        html = results_tables(),
        file_name = as.character(download_names()['results_tables']),
        format = gsub(".*\\.", "", download_names()['results_tables'], perl =
                        TRUE)
      )
      file.copy(pandoc_output, file)
    }
  )

  # Download Question Dictionary
  output[['downloadQuestionDictionary']] <- downloadHandler(
    filename = function() {
      download_names()[['qdict']]
    },
    content = function(file) {
      write.csv(question_dictionary(), file, row.names = FALSE)
    }
  )

  # Download Text Appendices
  output[['downloadTextAppendices']] <- downloadHandler(
    filename = function() {
      download_names()[['text_appendices']]
    },
    content = function(file) {
      pandoc_output = html_2_pandoc(
        html = text_appendices(),
        file_name = as.character(download_names()['text_appendices']),
        format = gsub(".*\\.", "", download_names()['text_appendices'], perl =
                        TRUE)
      )
      file.copy(pandoc_output, file)
    }
  )

  # Download Display Logic
  output[['downloadDisplayLogic']] <- downloadHandler(
    filename = function() {
      download_names()[['display_logic']]
    },
    content = function(file) {
      pandoc_output = html_2_pandoc(
        html = display_logic(),
        file_name = as.character(download_names()['display_logic']),
        format = gsub(".*\\.", "", download_names()['display_logic'], perl =
                        TRUE)
      )
      file.copy(pandoc_output, file)
    }
  )

  # Download Zip Button
  output[['downloadZip']] <- downloadHandler(
    filename = function() {
      paste("QT Survey Output", "zip", sep = ".")
    },
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      rt_docx <- html_2_pandoc(
        html = results_tables(),
        file_name = as.character(download_names()['results_tables']),
        format = gsub(".*\\.", "", download_names()['results_tables'], perl =
                        TRUE)
      )
      write.csv(
        question_dictionary(),
        row.names = FALSE,
        file = file.path(tmpdir, "question_dictionary.csv")
      )
      qd_csv <- file.path(tmpdir, "question_dictionary.csv")
      dl_docx <- html_2_pandoc(
        html = display_logic(),
        file_name = as.character(download_names()['display_logic']),
        format = gsub(".*\\.", "", download_names()['display_logic'], perl =
                        TRUE)
      )
      ta_docx <- html_2_pandoc(
        html = text_appendices(),
        file_name = as.character(download_names()['text_appendices']),
        format = gsub(".*\\.", "", download_names()['text_appendices'], perl =
                        TRUE)
      )

      # repath the CSV in case it needs it for a Windows path
      # https://www.r-bloggers.com/stop-fiddling-around-with-copied-paths-in-windows-r/
      qd_csv <- gsub('\\\\', '/', qd_csv)

      fs <- c(fs, file = rt_docx)
      fs <- c(fs, file = qd_csv)
      fs <- c(fs, file = dl_docx)
      fs <- c(fs, file = ta_docx)
      if (file.exists(paste0(fname, ".zip")))
        file.rename(paste0(fname, ".zip"), fname)
      zip(zipfile = fname,
          files = fs,
          flags = "-j")
    },
    contentType = "application/zip"
  )

  # Download Split Reports and Text Appendices
  output[['downloadSplit']] <- downloadHandler(
    filename = function() {
      paste("QT Split Reports and Appendices", "zip", sep = ".")
    },
    content = function(fname) {
      fs <- c()
      split_blocks <- split_blocks()
      survey <- survey_and_responses()[[1]]
      flow <- flow_from_survey(survey)
      original_first_rows <- survey_and_responses()[[3]]
      original_first_row <- original_first_rows[1, ]
      for (i in 1:length(split_blocks)) {
        fs <-
          c(fs,
            html_2_pandoc(
              html = c(
                blocks_header_to_html(split_blocks[[i]]),
                create_html_results_tables(split_blocks[[i]], flow)
              ),
              file_name = paste0(
                "results_tables_",
                i,
                ".",
                gsub(".*\\.", "", download_names()['results_tables'], perl = TRUE)
              ),
              format = gsub(".*\\.", "", download_names()['results_tables'], perl =
                              TRUE)
            ))


        fs <-
          c(fs,
            html_2_pandoc(
              html = c(
                blocks_header_to_html(split_blocks[[i]]),
                text_appendices_table(split_blocks[[i]], original_first_row, flow)
              ),
              file_name = paste0(
                "text_appendices_",
                i,
                ".",
                gsub(".*\\.", "", download_names()['text_appendices'], perl = TRUE)
              ),
              format = gsub(".*\\.", "", download_names()['text_appendices'], perl =
                              TRUE)
            ))
      }
      zip(zipfile = fname,
          files = fs,
          flags = "-j")
    },
    contentType = "application/zip"
  )


  ########## Stop Button
  observe({
    # If input$quit is unset (NULL) do nothing; if it's anything else, quit
    # and return input$n
    if (input$quit == 0)
      return()
    else
      stopApp("Have a great day!")
  })
})
