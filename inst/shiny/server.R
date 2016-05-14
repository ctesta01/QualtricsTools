shinyServer(
  function(input, output) {

  main <- reactive({
    validate(
      need(validate_data_export_tags(
           try(questions_from_survey(
           load_qsf_data(input$file1)
           ))),
           "Please submit a survey with no duplicate question IDs"))

    survey <- load_qsf_data(input$file1)
    responses <- load_csv_data(input$file2)
    get_questions_and_blocks(survey, responses)
    })

  output$results_tables <- renderUI({
    blocks <- main()[['blocks']]
    div(HTML(html_tabelize(blocks)), class="shiny-html-output")
  })

  output$uncodeable_message <- renderText({
    questions <- main()[['questions']]
    uncodeable_questions_message(questions)
  })

  output$question_dictionary <- renderDataTable({
    blocks <- main()[['blocks']]
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
