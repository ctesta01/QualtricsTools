shinyServer(
  function(input, output) {

  main <- reactive({
    validate(
      need(validate_data_export_tags(questions_from_survey(load_qsf_data(input$file2))),
           "Please submit a survey with no duplicate question IDs"))

    responses <- load_csv_data(input$file1)
    survey <- load_qsf_data(input$file2)
    blocks <- blocks_from_survey(survey)
    questions <- questions_from_survey(survey)
    questions_without_trash <- remove_trash_questions(questions, blocks)
    blocks_without_trash <- remove_trash_blocks(blocks)
    questions_with_responses <- link_responses_to_questions(questions_without_trash, responses)
    questions_with_results <- generate_results(questions_with_responses)
    blocks_with_questions <- questions_into_blocks(questions_with_results, blocks_without_trash)
    questions_and_blocks <- list()
    questions_and_blocks[['questions']] <- questions_with_results
    questions_and_blocks[['blocks']] <- blocks_with_questions
    return(questions_and_blocks)
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
