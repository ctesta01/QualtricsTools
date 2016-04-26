library(shinydashboard)
library(rjson)
library(qualtrics)

shinyServer(
  function(input, output) {

  # Set responses to either user submitted responses or sample data
  # Validate that the responses have no duplicate column names
  responses <- reactive({ validate(
    need(validate_response_columns(load_csv_data(input$file1)),
    "Please submit a response set with no duplicate column names"))
    load_csv_data(input$file1)
    })

  # Set survey to either user submitted survey or sample survey
  # Validate that the survey has no duplicate DataExportTags
  survey <- reactive({ validate(
    need(validate_data_export_tags(questions_from_survey(load_qsf_data(input$file2))),
     "Please submit a survey with no duplicate question IDs"))
    load_qsf_data(input$file2) })

  blocks <- reactive({ blocks_from_survey(survey()) })
  questions <- reactive({ questions_from_survey(survey()) })
  questions <- reactive({ remove_trash_questions(questions(), blocks() )})
  blocks <- reactive({ remove_trash_blocks(blocks()) })
  questions <- reactive({ link_responses_to_questions(questions(), responses()) })
  
  
  # output$blocks <- renderText()

  }
)
