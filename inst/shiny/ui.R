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
             div(class="sidebar-text",
                 HTML("Was the CSV exported using the <a href='https://github.com/ctesta01/QualtricsTools/wiki/Usage-Requirements#legacy-and-insights-data'>Insights format</a>?")),
             checkboxInput("insights_or_not", "Insights?", value = TRUE, width = NULL)
             ),
             menuItem("Processed Results", tabName="report", icon=icon("tasks")),
             menuItem("Include/Exclude Responses", tabName="include_exclude", icon=icon("toggle-on")),
             menuItem("More Options", tabName="more-options", icon=icon("dashboard")),


             # empty h5 headers below are for spacing
             h5(""),
             downloadButton('downloadAllAsZip', 'Download All As Zip', class="btn-primary"),
             h5(""),
             actionButton("quit", "Stop App")
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
                       uiOutput("uncodeable_message"),
                       uiOutput("results_tables")
                      ),
              tabPanel(h5("question dictionary"),
                       checkboxInput("uncodeable-only", "Only Uncodeable Questions", value = FALSE, width = NULL),
                       dataTableOutput("question_dictionary")
                       ),
              tabPanel(h5("text appendices"),
                       uiOutput("text_appendices")
                       ),
              tabPanel(h5("display logic"),
                       uiOutput("display_logic")
                       )
              )
      )
    )
  ),
  tabItem(tabName = "include_exclude",
          fluidRow(
            column(width=12,
            h2('Include or Exclude Specific Questions'),
            actionButton("selectAll", "Unselect/Select All"),
            actionButton("submit", "Apply"),
              dataTableOutput("select_qdict")
              )
  )))
)

dashboardPage(
  dashboardHeader(title = "Qualtrics Tools"),
  sidebar,
  body
)
