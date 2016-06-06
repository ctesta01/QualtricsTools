library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
    menuItem("File Uploading",
             icon=icon("upload"),
             selected=TRUE,
             fileInput('file1',
                       'Choose QSF Survey File',
                       accept=c('text/qsf', 'text/plain', '.qsf')
             ),
             fileInput('file2',
                       'Choose CSV Response Set File',
                       accept=c('text/csv', 'text/comma-separated-values', '.csv')
             ),
             numericInput("headerrows", "How many header rows are there in the responses?", 2, min = 1)
             ),
    menuItem("Report Generator",
             tabName="report-gen",
             icon=icon("tasks"),
             menuSubItem("View Processed Results", tabName="report", icon=icon("leanpub")),
             h5(""),
             downloadButton('downloadResultsTables', 'Results Table', class="btn-primary"),
             h5(""),
             downloadButton('downloadQuestionDictionary', 'Question Dictionary', class="btn-primary"),
             h5(""),
             downloadButton('downloadTextAppendices', 'Text Appendices', class="btn-primary")
             ),
    menuItemOutput("panel_data_input")
    )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "custom.js")
  ),

  tabItems(
  tabItem(tabName = "report",
    fluidRow(
    column(width = 12,
      tabBox( width = NULL,
              tabPanel(h5("results tables"),
                       textOutput("uncodeable_message"),
                       uiOutput("results_tables")
                      ),
              tabPanel(h5("question dictionary"),
                       dataTableOutput("question_dictionary")
                       ),
              tabPanel(h5("text appendices"),
                       uiOutput("text_appendices")
                       )
              )
      )
    )
  ),

  tabItem(tabName = "reshape",
          fluidRow(column(width=12,
                          tabPanel(h5("long and lean responses"),
                                   verbatimTextOutput("test"),
                                   dataTableOutput("long_and_lean")
                                   )
                          )
                   )
          )
  )
)

dashboardPage(
  dashboardHeader(title = "Qualtrics Automation"),
  sidebar,
  body
)
