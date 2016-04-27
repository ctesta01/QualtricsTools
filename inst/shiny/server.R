shinyServer(
  function(input, output) {

  main_process <- reactive({
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
  })


  output$table <- renderText({
    unlist(main_process())
      })

  }
)
