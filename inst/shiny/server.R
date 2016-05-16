shinyServer(
  function(input, output) {

  main <- reactive({
    validate(
      need(validate_data_export_tags(
           try(questions_from_survey(
           load_qsf_data(input$file1)
           ))),
           "Please submit a survey with no duplicate question IDs")
      )


    survey <- load_qsf_data(input$file1)
    responses <- load_csv_data(input$file2, input$file1, input$headerrows)
    survey_and_responses <- list()
    survey_and_responses[[1]] <- survey
    survey_and_responses[[2]] <- responses
    return(survey_and_responses)
    })


  output$results_tables <- renderUI({
    if (length(main()) == 2) {
    survey <- main()[[1]]
    responses <- main()[[2]]
    blocks <- get_coded_questions_and_blocks(survey, responses)[[2]]
    div(HTML(html_tabelize(blocks)), class="shiny-html-output")
    }
  })

  output$uncodeable_message <- renderText({
    validate(need(length(main()) == 2, "Please upload survey responses"))
    if (length(main()) == 2) {
    survey <- main()[[1]]
    responses <- main()[[2]]
    questions <- get_coded_questions_and_blocks(survey, responses)[[1]]
    uncodeable_questions_message(questions)
    }
  })

  output$question_dictionary <- renderDataTable({
    survey <- main()[[1]]
    blocks <- blocks_from_survey(survey)
    questions <- questions_from_survey(survey)
    questions <- remove_trash_questions(questions, blocks)
    questions <- clean_question_text(questions)
    questions <- human_readable_qtype(questions)
    blocks <- remove_trash_blocks(blocks)
    blocks <- questions_into_blocks(questions, blocks)
    create_question_dictionary(blocks)

  })

  output$downloadResults <- downloadHandler(
    filename = 'tables.xls',
    content = function(file) {
      write(html_tabelize(main()[['blocks']]), file)
    }
  )


  }
)
