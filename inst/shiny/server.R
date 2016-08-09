options(shiny.maxRequestSize=30*1024^2)

shinyServer(
  function(input, output) {
  # the survey_and_responses reactive block reads the input files
  # and loads them as the survey and responses. It validates that there
  # are no duplicate data export tags in the survey, and it returns a
  # list with three elements -- the processed survey, the responses,
  # and the original_first_rows from the response set.
  survey_and_responses <- reactive({
    survey <- try(load_qsf_data(input[['file1']]))
    if (!is.null(input[['unselected_questions']])) {
      remove_these_survey_elements <- function(x) {
            "DataExportTag" %in% names(x[['Payload']]) && x[['Payload']][['DataExportTag']] %in% input[['unselected_questions']]
        }
      for (i in 1:length(survey[['SurveyElements']])) {
        if ('DataExportTag' %in% names(survey[['SurveyElements']][[i]][['Payload']]) &&
            survey[['SurveyElements']][[i]][['Payload']][['DataExportTag']] %in% input[['unselected_questions']]) {
          survey[['SurveyElements']][[i]][['qtSkip']] <- TRUE
        }
      }
    }
    questions <- try(questions_from_survey(survey))
    blocks <- try(blocks_from_survey(survey))
    questions <- remove_trash_questions(questions, blocks)
    duplicates <- questions[which(duplicated(sapply(questions, function(x) x$Payload$DataExportTag)))]
    duplicate_tags <- sapply(duplicates, function(x) x$Payload$DataExportTag)
    validate(
      need(validate_data_export_tags(questions),
           paste0("Please submit a survey with no duplicate question IDs.
           The following questions were duplicated: ",
                  paste(duplicate_tags, collapse=", ")))
      )
    if (input[['insights_or_not']] == TRUE) headerrows <- 3
    if (input[['insights_or_not']] == FALSE) headerrows <- 2
    responses <- load_csv_data(input$file2, input$file1, headerrows)
    original_first_rows <- responses[[2]]
    responses <- responses[[1]]
    list_survey_and_responses <- list()
    list_survey_and_responses[[1]] <- survey
    list_survey_and_responses[[2]] <- responses
    list_survey_and_responses[[3]] <- original_first_rows
    return(list_survey_and_responses)
    })

  # the uncodeable_message reactive block reacts to the survey_and_responses() block
  # with a message indicating which, if any, questions were not properly processed.
  uncodeable_message <- reactive({
    validate(need(length(survey_and_responses()) >= 3, "Please upload the survey and responses"))
    if (length(survey_and_responses()) >= 3) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      original_first_rows <- survey_and_responses()[[3]]
      questions <- get_coded_questions_and_blocks(survey, responses, original_first_rows)[[1]]
      uncodeable_questions_message(questions)
    }
  })

  # the results_tables reactive block reacts to the survey_and_responses output
  # by processing the survey and responses into blocks with results tables inserted,
  # and then converting the results tables to HTML tables.
  results_tables <- reactive({
    validate(need(length(survey_and_responses()) >= 3, ""))
    if (length(survey_and_responses()) >= 3) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      original_first_rows <- survey_and_responses()[[3]]
      blocks <- get_coded_questions_and_blocks(survey, responses, original_first_rows)[[2]]
      c(blocks_header_to_html(blocks),
        tabelize_blocks(blocks))
    }
  })

  # the question_dictionary block uses the survey from the survey_and_responses output
  # to create a data frame detailing each survey question.
  question_dictionary <- reactive({
    validate(need(length(survey_and_responses()) >= 3, ""))
    if (length(survey_and_responses()) >= 3) {
      survey <- survey_and_responses()[[1]]
      original_first_row <- survey_and_responses()[[3]][1, ]
      responses <- survey_and_responses()[[2]]
      original_first_rows <- survey_and_responses()[[3]]
      blocks <- get_coded_questions_and_blocks(survey, responses, original_first_rows)[[2]]
      if (input[['uncodeable-only']] == TRUE) {
        uncode_qdict <- uncodeable_question_dictionary(blocks)
        if (is.null(uncode_qdict)) {
          success_message <- data.frame("All questions were successfully processed!")
          colnames(success_message)[1] <- " "
          return(success_message)
        } else {
          return(uncode_qdict)
        }
      } else {
        return(create_response_column_dictionary(blocks, original_first_row))
      }
    }
  })

  text_appendices <- reactive({
    validate(need(length(survey_and_responses()) >= 3, "Please upload the survey and responses"))
    if (length(survey_and_responses()) >= 3) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      original_first_rows <- survey_and_responses()[[3]]
      blocks <- get_coded_questions_and_blocks(survey, responses, original_first_rows)[[2]]
      original_first_row <- original_first_rows[1,]
      c(blocks_header_to_html(blocks),
        text_appendices_table(blocks, original_first_row))
    }
  })

  display_logic <- reactive({
    validate(need(length(survey_and_responses()) >= 1, "Please upload a survey"))
    if (length(survey_and_responses()) >= 1) {
      survey <- survey_and_responses()[[1]]
      blocks <- blocks_from_survey(survey)
      questions <- questions_from_survey(survey)
      questions <- remove_trash_questions(questions, blocks)
      questions <- clean_question_text(questions)
      blocks <- remove_trash_blocks(blocks)
      blocks <- questions_into_blocks(questions, blocks)
      tabelize_display_logic(blocks)
    }
  })

  include_exclude_dict <- reactive({
    validate(need(length(survey_and_responses()) >= 3, "Please upload the survey and responses"))
    qdict <- unique(question_dictionary()[c(1,3,5,6,7)])
    check_list <- lapply(qdict[[1]], function(x) ifelse(x %in% input[['unselected_questions']], "", " checked "))
    addCheckboxButtons <- paste0('<input type="checkbox" name="unselected_questions_', qdict[[1]], '" value="', qdict[[1]], '"', check_list, '>',"")
    #Display table with checkbox buttons
    cbind(Include=addCheckboxButtons, qdict)
  })

  # output each tabpanels' contents
  output[['uncodeable_message']] <- renderUI(HTML(uncodeable_message()))
  output[['results_tables']] <- renderUI(div(HTML(results_tables()), class="shiny-html-output"))
  output[['question_dictionary']] <- renderDataTable(question_dictionary(),
                                                options = list(scrollX = TRUE,
                                                               pageLength = 10,
                                                               autoWidth = TRUE
                                                ))
  output[['text_appendices']] <- renderUI(div(HTML(text_appendices()), class="shiny-html-output"))
  output[['display_logic']] <- renderUI(div(HTML(display_logic()), class="shiny-html-output"))


  # Include/Exclude Questions
  output[['select_qdict']] = renderDataTable({
    include_exclude_dict()
    }, options = list(orderClasses = TRUE,
                    lengthMenu = c(5, 25, 50),
                    pageLength = 25)
  , escape = FALSE)


  ########## Download Buttons
  download_names <- reactive({
    dnames <- list()

    dnames['results_tables'] <- paste0("results_tables.", input[['rt_format']])
    dnames['qdict'] <- paste0('question_dictionary.', input[['qd_format']])
    dnames['text_appendices'] <- paste0('text_appendices.', input[['ta_format']])
    dnames['display_logic'] <- paste0('display_logic.', input[['dl_format']])
    return(dnames)
  })

  # download results tables
  output[['downloadResultsTables']] <- downloadHandler(
    filename = function() { download_names()[['results_tables']] },
    content = function(file) {
      pandoc_output = html_2_pandoc(html = results_tables(),
                                    file_name = as.character(download_names()['results_tables']),
                                    format = gsub(".*\\.", "", download_names()['results_tables'], perl=TRUE))
      file.copy(pandoc_output, file)
    }
  )

  # download question dictionary
  output[['downloadQuestionDictionary']] <- downloadHandler(
    filename = function() { download_names()[['qdict']] },
    content = function(file) {
      write.csv(question_dictionary(), file, row.names=FALSE)
    }
  )

  # download text appendices
  output[['downloadTextAppendices']] <- downloadHandler(
    filename = function() { download_names()[['text_appendices']] },
    content = function(file) {
      pandoc_output = html_2_pandoc(html = text_appendices(),
                                    file_name = as.character(download_names()['text_appendices']),
                                    format = gsub(".*\\.", "", download_names()['text_appendices'], perl=TRUE))
      file.copy(pandoc_output, file)
    }
  )

  # download display logic
  output[['downloadDisplayLogic']] <- downloadHandler(
    filename = function() { download_names()[['display_logic']] },
    content = function(file) {
      pandoc_output = html_2_pandoc(html = display_logic(),
                                    file_name = as.character(download_names()['display_logic']),
                                    format = gsub(".*\\.", "", download_names()['display_logic'], perl=TRUE))
      file.copy(pandoc_output, file)
    }
  )

  # Download Zip Button
  output[['downloadZip']] <- downloadHandler(
    filename = function() {
      paste("QT Survey Output", "zip", sep=".")
    },
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      rt_docx <- html_to_docx(results_tables(), "results_tables.docx")
      write.csv(question_dictionary(), row.names=FALSE, file=file.path(tmpdir, "question_dictionary.csv"))
      qd_csv <- file.path(tmpdir, "question_dictionary.csv")
      dl_docx <- html_to_docx(display_logic(), "display_logic.docx")
      ta_docx <- html_to_docx(text_appendices(), "text_appendices.docx")

      # repath the CSV in case it needs it for a Windows path
      # https://www.r-bloggers.com/stop-fiddling-around-with-copied-paths-in-windows-r/
      qd_csv <- gsub('\\\\', '/', qd_csv)

      fs <- c(fs, file=rt_docx)
      fs <- c(fs, file=qd_csv)
      fs <- c(fs, file=dl_docx)
      fs <- c(fs, file=ta_docx)
      if (file.exists(paste0(fname, ".zip")))
        file.rename(paste0(fname, ".zip"), fname)
      zip(zipfile=fname, files=fs, flags="-j")
    },
    contentType = "application/zip"
  )


  ########## Stop Button
  observe({
    # If input$quit is unset (NULL) do nothing; if it's anything else, quit
    # and return input$n
    if (input$quit == 0) return()
    else stopApp("Have a great day!")
  })
  }
)
