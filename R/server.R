library(shiny)
library(knitr)

shinyServer(
  function(input, output) {


   responses <- reactive({ load_csv_data(input$file1) })
   survey <- reactive({ load_qsf_data(input$file2) })

    output$contents <- renderText({ sapply(blocks_from_survey(survey()), function(x) x$Description) })

  }
)
