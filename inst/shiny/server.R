options(shiny.maxRequestSize=30*1024^2)

shinyServer(
  function(input, output) {

  # the survey_and_responses reactive block reads the input files
  # and loads them as the survey and responses. It validates that there
  # are no duplicate data export tags in the survey, and it returns a
  # list with three elements -- the processed survey, the responses,
  # and the original_first_row from the response set.
  survey_and_responses <- reactive({
    survey <- try(load_qsf_data(input$file1))
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
    responses <- load_csv_data(input$file2, input$file1, input$headerrows)
    original_first_row <- NULL
    if (!is.null(input$file2)) original_first_row <- read.csv(input$file2$datapath, check.names=FALSE)[1,]
    if (is.null(input$file2)) original_first_row <- sample_original_first_row
    list_survey_and_responses <- list()
    list_survey_and_responses[[1]] <- survey
    list_survey_and_responses[[2]] <- responses
    list_survey_and_responses[[3]] <- original_first_row
    return(list_survey_and_responses)
    })

  # the uncodeable_message reactive block reacts to the survey_and_responses() block
  # with a message indicating which, if any, questions were not properly processed.
  uncodeable_message <- reactive({
    validate(need(length(survey_and_responses()) >= 2, "Please upload survey responses"))
    if (length(survey_and_responses()) >= 2) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      questions <- get_coded_questions_and_blocks(survey, responses)[[1]]
      uncodeable_questions_message(questions)
    }
  })

  # the results_tables reactive block reacts to the survey_and_responses output
  # by processing the survey and responses into blocks with results tables inserted,
  # and then converting the results tables to HTML tables.
  results_tables <- reactive({
    validate(need(length(survey_and_responses()) >= 2, ""))
    if (length(survey_and_responses()) >= 2) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      blocks <- get_coded_questions_and_blocks(survey, responses)[[2]]
      c(blocks_header_to_html(blocks),
        tabelize_blocks(blocks))
    }
  })

  # the question_dictionary block uses the survey from the survey_and_responses output
  # to create a data frame detailing each survey question.
  question_dictionary <- reactive({
    validate(need(length(survey_and_responses()) >= 3, "Please upload survey responses"))
    survey <- survey_and_responses()[[1]]
    original_first_row <- survey_and_responses()[[3]]
    responses <- survey_and_responses()[[2]]
    blocks <- get_coded_questions_and_blocks(survey, responses)[[2]]
    create_response_column_dictionary(blocks, original_first_row)

  })

  text_appendices <- reactive({
    validate(need(length(survey_and_responses()) >= 2, "Please upload survey responses"))
    if (length(survey_and_responses()) >= 2) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      original_first_row <- NULL
      if (length(survey_and_responses()) >= 3) original_first_row <- survey_and_responses()[[3]]
      blocks <- get_coded_questions_and_blocks(survey, responses)[[2]]
      c(blocks_header_to_html(blocks),
        text_appendices_table(blocks, original_first_row))
    }
  })

  display_logic <- reactive({
    validate(need(length(survey_and_responses()) >= 1, "Please upload survey responses"))
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

  # output tabpanels' contents
  output$uncodeable_message <- renderUI(HTML(uncodeable_message()))
  output$results_tables <- renderUI(div(HTML(results_tables()), class="shiny-html-output"))
  output$question_dictionary <- renderDataTable(question_dictionary(),
                                                options = list(scrollX = TRUE,
                                                               pageLength = 10,
                                                               autoWidth = TRUE
                                                ))
  output$text_appendices <- renderUI(div(HTML(text_appendices()), class="shiny-html-output"))
  output$display_logic <- renderUI(div(HTML(display_logic()), class="shiny-html-output"))


  # Download Buttons

  # download results tables
  output$downloadResultsTables <- downloadHandler(
    filename = 'results-tables.docx',
    content = function(file) {
      file.copy(html_to_docx(results_tables()), file)
    }
  )

  # download question dictionary
  output$downloadQuestionDictionary <- downloadHandler(
    filename = 'question-dictionary.csv',
    content = function(file) {
      write.csv(question_dictionary(), file, row.names=F)
    }
  )

  # download text appendices
  output$downloadTextAppendices <- downloadHandler(
    filename = 'appendices.docx',
    content = function(file) {
      file.copy(html_to_docx(text_appendices()), file)
    }
  )

  # download display logic
  output$downloadDisplayLogic <- downloadHandler(
    filename = 'display-logic.docx',
    content = function(file) {
      file.copy(html_to_docx(display_logic()), file)
    }
  )

  # Stop App button
  observe({
    # If input$quit is unset (NULL) do nothing; if it's anything else, quit
    # and return input$n
    if (input$quit == 0) return()
    else stopApp("Have a great day!")
  })
  }
)
