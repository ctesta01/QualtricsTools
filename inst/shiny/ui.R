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
                  'Choose CSV Response Set File',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
        ),
        fileInput('file2',
                  'Choose QSF Survey File',
                  accept=c('text/qsf', 'text/plain', '.qsf')
        )
      )
    )),
    column(width = 8,
      tabBox( width = NULL,
              tabPanel(h5("results tables"),
                         uiOutput("results_tables")
              ),
              tabPanel(h5("question dictionary"))
      )

    ))
  )
)

dashboardPage(
  dashboardHeader(title = "Qualtrics Automation"),
  sidebar,
  body
)
