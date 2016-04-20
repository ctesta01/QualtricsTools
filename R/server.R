library(shiny)
library(knitr)

shinyServer(
  function(input, output) {

    load_csv_data <- function(file1, sampledata) {
      if (sampledata == TRUE) {
        responses <- sample_responses
      } else {
        responses <- ask_user_for_csv(file1)
      }
      return(responses)
    }

    responses <- reactive({ load_csv_data(input$file1, input$sampledata) })

    output$contents <- renderText({"Sample Text"})

  }
)
