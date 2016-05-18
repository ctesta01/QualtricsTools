shinyServer(
  function(input, output) {

  survey_and_responses <- reactive({
    validate(
      need(validate_data_export_tags(
           try(questions_from_survey(
           load_qsf_data(input$file1)
           ))),
           "Please submit a survey with no duplicate question IDs")
      )
    survey <- load_qsf_data(input$file1)
    responses <- load_csv_data(input$file2, input$file1, input$headerrows)
    list_survey_and_responses <- list()
    list_survey_and_responses[[1]] <- survey
    list_survey_and_responses[[2]] <- responses
    return(list_survey_and_responses)
    })

  results_tables <- reactive({
    if (length(survey_and_responses()) == 2) {
      survey <- survey_and_responses()[[1]]
      responses <- survey_and_responses()[[2]]
      blocks <- get_coded_questions_and_blocks(survey, responses)[[2]]
      tabelize_blocks(blocks)
    }
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

  output$results_tables <- renderUI(div(HTML(results_tables()), class="shiny-html-output"))
  output$uncodeable_message <- renderText(uncodeable_message())
  output$question_dictionary <- renderDataTable(question_dictionary())
  output$text_appendices <- renderUI(div(HTML(text_appendices()), class="shiny-html-output"))

  output$downloadResultsTables <- downloadHandler(
    filename = 'results_tables.xls',
    content = function(file) {
      write(unlist(results_tables()), file)
    }
  )

  output$downloadQuestionDictionary <- downloadHandler(
    filename = 'question_dictionary.csv',
    content = function(file) {
      write.csv(question_dictionary(), file, row.names=F)
    }
  )

  output$downloadTextAppendices <- downloadHandler(
    filename = 'appendices.xls',
    content = function(file) {
      write(text_appendices(), file)
    }
  )


  }
)
