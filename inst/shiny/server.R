library(shiny)
library(knitr)
library(rjson)

shinyServer(
  function(input, output) {


   responses <- reactive({ load_csv_data(input$file1) })
   survey <- reactive({
    validate(
      need(validate_data_export_tags(questions_from_survey(load_qsf_data(input$file2))),
           "Please submit a survey with no duplicate question IDs"))
    load_qsf_data(input$file2) })

    output$contents <- renderText({ sapply(questions_from_survey(survey()), function(x) x$Payload$QuestionText) })

  }
)
