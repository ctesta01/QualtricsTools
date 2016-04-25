library(shinydashboard)
library(rjson)
library(qualtrics)

shinyServer(
  function(input, output) {

  # Set responses to either user submitted responses or sample data
  responses <- reactive({ load_csv_data(input$file1) })

  # Set survey to either user submitted survey or sample survey
  # Validate that the survey has no duplicate DataExportTags
  survey <- reactive({ validate(
    need(validate_data_export_tags(questions_from_survey(load_qsf_data(input$file2))),
     "Please submit a survey with no duplicate question IDs"))
    load_qsf_data(input$file2) })

  output$contents <- renderText({ "test" })
  output$responses <- renderTable(responses())

  }
)
