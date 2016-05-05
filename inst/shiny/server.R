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
    questions_with_results <- generate_results(questions_with_responses)





    results_table_html <- html_tabelize(results_tables, questions_with_results)

    uncodeable_questions <- which(sapply(questions_with_results, function(x) !("Table" %in% names(x))))
    uncodeable_questions <- sapply(uncodeable_questions, function(x)
      questions_with_results[[x]]$Payload$DataExportTag)
    uncodeable_message <- ""
    if (length(uncodeable_questions) > 0) {
      uncodeable_message <- sprintf("The following questions could not be automatically
                                   coded: %s", paste(uncodeable_questions, collapse=", "))
    }

    paste0(uncodeable_message, results_table_html)

    })

  output$results_tables <- renderUI({
    div(HTML(main_process()), class="shiny-html-output")
  })

  }
)
