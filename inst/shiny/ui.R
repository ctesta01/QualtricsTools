library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
    menuItem("Report Generator", tabName="report", icon=icon("leanpub"), selected=TRUE),
    menuItem("Long and Lean Formatter", tabName = "tableau", icon=icon("database"))
    )
)

body <- dashboardBody(
  tabItem(tabName = "submitting a survey",
    fluidRow(column(width = 4, tabBox( width = NULL,
      tabPanel(h5("upload files"),
        fileInput('file1',
                  'Choose QSF Survey File',
                  accept=c('text/qsf', 'text/plain', '.qsf')
        ),
        fileInput('file2',
                  'Choose CSV Response Set File',
                  accept=c('text/csv', 'text/comma-separated-values', '.csv')
        ),
        numericInput("headerrows", "How many header rows are there in the responses?", 2, min = 1),
        downloadButton('downloadResults', 'Download Results Table')
      )
    )),
    column(width = 8,
      tabBox( width = NULL,
              tabPanel(h5("results tables"),
                       textOutput("uncodeable_message"),
                       uiOutput("results_tables")
              ),
              tabPanel(h5("question dictionary"),
                       dataTableOutput("question_dictionary"))
      )

    ))
  )
)

dashboardPage(
  dashboardHeader(title = "Qualtrics Automation"),
  sidebar,
  body
)
