shinyUI(fluidPage(
  headerPanel(title = "Qualtrics Data Reshaping"),
  sidebarLayout(

    sidebarPanel(
      checkboxInput('sampledata', 'Use Sample Data', TRUE),
      fileInput('file1', 'Choose CSV File', accept=c('text/csv',
                                                     'text/comma-separated-values,text/plain',
                                                     '.csv'))
      ),

    mainPanel("This is the main panel")
  )
))
