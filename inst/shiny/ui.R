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
             menuItem("Processed Results", tabName="report", icon=icon("leanpub")),
             menuItem("Include/Exclude Responses", tabName="include_exclude", icon=icon("toggle-on")),
             menuItem("More Options", tabName="more_options", icon=icon("dashboard")),

             # empty h5 headers below are for spacing
             h5(""),
             downloadButton('downloadZip', 'Download Zip', class="btn-primary"),
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
            actionButton("submit", "Apply", class="btn-success"),
            HTML("<br><br>"),
              dataTableOutput("select_qdict")
              )
  )),
  tabItem(tabName = "more_options",
          fluidPage(
            # titlePanel('Downloads'),
            sidebarLayout(
              sidebarPanel(
                h1('Downloads'),
                HTML("<br><table style='width: 100%;'> <tr> <td>"),
                selectInput("rt_format", "Format for Results Tables:",
                            choices = c("docx", "html", "md", "pdf", "xls"),
                            width='90%'),
                HTML("</td> <td>"),
                downloadButton('downloadResultsTables', '', class="btn-primary"),
                HTML("</td> </tr> <tr> <td>"),
                selectInput("qd_format", "Format for Question Dictionary:",
                            choices = c("csv"),
                            width='90%'),
                HTML("</td> <td>"),
                downloadButton('downloadQuestionDictionary', '', class="btn-primary"),
                HTML("</td> </tr> <tr> <td>"),
                selectInput("ta_format", "Format for Text Appendices:",
                            choices = c("docx", "html", "md", "pdf", "xls"),
                            width='90%'),
                HTML("</td> <td>"),
                downloadButton('downloadTextAppendices', '', class="btn-primary"),
                HTML("</td> </tr> <tr> <td>"),
                selectInput("dl_format", "Format for Display Logic:",
                            choices = c("docx", "html", "md", "pdf", "xls"),
                            width='90%'),
                HTML("</td> <td>"),
                downloadButton('downloadDisplayLogic', '', class="btn-primary"),
                HTML("</td> </tr> </table>")
              ),
              sidebarPanel(width=8,
                h1('Splitting Respondents'),
                HTML("Select the columns for which you'd like to split the respondents
                     into unique respondent groups"),
                uiOutput("select_response_columns"),
                HTML("Select for which answer (combinations) you'd like to restrict the respondents to"),
                uiOutput('select_respondent_group'),
                tableOutput('table_respondent_groups'),
                downloadButton('downloadSplit', 'Download All Split Reports and Appendices', class="btn-primary")
              )
            ),
            sidebarPanel(
                h3('Ignore Survey Flow'),
                checkboxInput("ignoreflow", "Check this box if you would like the report to render without reordering
                              the questions according to the survey's ordering.", FALSE)
            )
          )
   ))
)

dashboardPage(
  dashboardHeader(title = "Qualtrics Tools"),
  sidebar,
  body
)
