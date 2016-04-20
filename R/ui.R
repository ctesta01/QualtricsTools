library(shiny)

shinyUI(fluidPage(
  headerPanel(title = "Qualtrics Data Reshaping"),
  sidebarLayout(

    sidebarPanel(
      fileInput('file1', 'Choose CSV Response Set File', accept=c('text/csv',
                                                     'text/comma-separated-values,text/plain',
                                                     '.csv')),
      fileInput('file2', 'Choose QSF Survey File', accept=c('text/qsf',
                                                                  'text/plain',
                                                                  '.qsf'))
      ),

    mainPanel(textOutput("contents"))
  )
))
