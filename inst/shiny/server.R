options(shiny.maxRequestSize=30*1024^2)

shinyServer(
  function(input, output) {

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
    list_survey_and_responses <- list()
    list_survey_and_responses[[1]] <- survey
    list_survey_and_responses[[2]] <- responses
    return(list_survey_and_responses)
    })

  uncodeable_message <- reactive({
    validate(need(length(survey_and_responses()) == 2, "Please upload survey responses"))
    if (length(survey_and_responses()) == 2) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      questions <- get_coded_questions_and_blocks(survey, responses)[[1]]
      uncodeable_questions_message(questions)
    }
  })

  results_tables <- reactive({
    if (length(survey_and_responses()) == 2) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      blocks <- get_coded_questions_and_blocks(survey, responses)[[2]]
      tabelize_blocks(blocks)
    }
  })

  question_dictionary <- reactive({
    survey <- survey_and_responses()[[1]]
    blocks <- blocks_from_survey(survey)
    questions <- questions_from_survey(survey)
    questions <- remove_trash_questions(questions, blocks)
    questions <- clean_question_text(questions)
    questions <- human_readable_qtype(questions)
    blocks <- remove_trash_blocks(blocks)
    blocks <- questions_into_blocks(questions, blocks)
    create_question_dictionary(blocks)

  })

  text_appendices <- reactive({
    validate(need(length(survey_and_responses()) == 2, "Please upload survey responses"))
    if (length(survey_and_responses()) == 2) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      blocks <- get_coded_questions_and_blocks(survey, responses)[[2]]
      text_appendices_table(blocks)
    }
  })

  # output tabpanels' contents
  output$uncodeable_message <- renderText(uncodeable_message())
  output$results_tables <- renderUI(div(HTML(results_tables()), class="shiny-html-output"))
  output$question_dictionary <- renderDataTable(question_dictionary())
  output$text_appendices <- renderUI(div(HTML(text_appendices()), class="shiny-html-output"))

  # download buttons
  output$downloadResultsTables <- downloadHandler(
    filename = 'results_tables.docx',
    content = function(file) {
      file.copy(html_to_docx(results_tables()), file)
    }
  )

  output$downloadQuestionDictionary <- downloadHandler(
    filename = 'question_dictionary.csv',
    content = function(file) {
      write.csv(question_dictionary(), file, row.names=F)
    }
  )

  output$downloadTextAppendices <- downloadHandler(
    filename = 'appendices.docx',
    content = function(file) {
      file.copy(html_to_docx(text_appendices()), file)
    }
  )

  output$something_else <- renderMenu({
    # response_columns <- names(load_csv_data(input$file2, input$file1, input$headerrows))
    sidebarMenu(
      menuItem("testing",
               tabName = "booglah",
               icon = icon("calendar"),
               # selectInput('in6', 'Options', response_columns, multiple=TRUE, selectize=TRUE)
               selectInput("inputTest", label = h5("Input Test"), choices = c("a", "b", "c", "d"), multiple=TRUE, selectize=TRUE, width = '95%')
    )
    )
  })

  long_and_lean <- reactive({
    validate(need(length(survey_and_responses()) == 2, "Please upload survey responses"))
    if (length(survey_and_responses()) == 2) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      blocks <- get_coded_questions_and_blocks(survey, responses)[[2]]
      lean_responses(blocks, responses)
    }
  })

  output$long_and_lean <- renderDataTable(long_and_lean())

  output$panel_data_input <- renderMenu({
    response_columns <- names(load_csv_data(input$file2, input$file1, input$headerrows))
    sidebarMenu(
      menuItem("Reshaping Responses", tabName = "reshaping", icon = icon("calendar"),
               menuSubItem("View Lean Responses", tabName="reshape", icon=icon("leanpub")),
               selectInput('selected_response_cols',
                           "Choose response columns for inclusion as panel data:",
                           response_columns,
                           multiple=TRUE,
                           selectize=TRUE)
               )
      )
  })

  output$test <- reactive({
    input$selected_response_cols
  })

  }
)
