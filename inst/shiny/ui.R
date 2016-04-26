library(shinydashboard)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
    menuItem("Report Generator", tabName="report", icon=icon("leanpub"), selected=TRUE),
    menuItem("Long and Lean Formatter", tabName = "tableau", icon=icon("database"))
    )
)

body <- dashboardBody(
  tabItem(tabName = "plot",
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
      ),
      tabPanel(h5("settings"),
        sliderInput("tfd", "test:", value=0, min=0, max = 20, step=1),
        sliderInput("nd", "test:", value=1, min=0, max = 10, step=1),
        sliderInput("ii", "test:", value = 9, min = 0.5, max = 15, step=0.5),
        sliderInput("amt", "test:", value = 5, min = 0, max = 20, step=1)
      )
    )),
    column(width = 8,
      box(width = NULL,
        textOutput('blocks'),
        collapsible = TRUE,
        title = "Output",
        status = "primary",
        solidHeader = TRUE)
    ))
  )
)

dashboardPage(
  dashboardHeader(title = "Qualtrics Reshaping"),
  sidebar,
  body
)
