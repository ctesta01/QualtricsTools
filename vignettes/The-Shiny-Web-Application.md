This page contains explanations for each of the views that are in 
the Shiny application, as well as explanations for the structure of the 
code for the server and user-interface (UI) below. 

- [Explaining the Shiny App Components](#explaining-the-shiny-app-components)
  * [Sidebar](#sidebar)
  * [Processed Results](#processed-results)
    + [Results Tables](#results-tables)
    + [Question Dictionary](#question-dictionary)
    + [Text Appendices](#text-appendices)
    + [Display Logic](#display-logic)
  * [Include/Exclude Responses](#include-exclude-responses)
  * [More Options](#more-options)
    + [Downloads](#downloads)
    + [Ignore Survey Flow](#ignore-survey-flow)
    + [Splitting Respondents](#splitting-respondents)
- [Understanding the Code](#understanding-the-code)
  * [The Shiny Server](#the-shiny-server)
  * [The Shiny UI](#the-shiny-ui)

The code for the Shiny app is in [/inst/shiny/](https://github.com/ctesta01/QualtricsTools/tree/master/inst/shiny). 

# Explaining the Shiny App Components

![Gif of All App Views](https://github.com/ctesta01/QualtricsTools/blob/master/pics/animation.gif?raw=true)

The QualtricsTools app is composed primarily of four components: 
the sidebar, the processed results, the include/exclude responses
page, and the more options view. The sidebar allows
users to upload their Qualtrics data and download the reports
as a zip. In processed results, reports of the different survey 
components are presented in four tabs. In the include/exclude responses
page, users can control which questions are processed and included
in the processed results. Under more options, users can download
their data in a variety of formats, split respondents across response
fields to view and download reports on subsets of the respondents,
and also indicate if the survey's flow should be ignored (for reasons
such as the survey using randomized or particularly convoluted flow).

## Sidebar

![Sidebar](http://i.imgur.com/vNy6GJY.png)

The sidebar contains a file uploader for the QSF and CSV file, 
a checkbox to indicate the Qualtrics version used to create the data, 
three tabs for "Processed Results," "Include/Exclude Responses," and "More
Options," a download button to download docx versions of the results tables, 
question dictionary, text appendices, and display logic, and a stop button 
to close the app. 

## Processed Results

The Processed Results panel includes results tables, question dictionaries, 
text appendices, and display logic. 

### Results Tables

![Results Tables](https://github.com/ctesta01/QualtricsTools/blob/master/pics/results%20tables.png?raw=true)

The results tables list for each question how the respondents are broken down across each type of response to the question. 

The results tables panel starts with a message indicating which, if any, questions that could not be processed. The results generation functions currently only handle multiple choice single answer, multiple choice multiple answer (check all), matrix single answer, matrix multiple answer, and some side-by-side questions. 

Next, the results tables panel displays the header of the survey, which includes the name of the survey and the number of respondents. 

The results tables are divided into the blocks they were organized into in the Qualtrics survey, and headers for each question block are inserted above each section of questions. 

Each results table lists the choices that were available to survey respondents, the number of respondents who responded to each part of the question, and what percentage of the respondents chose each option. Further, if a question includes options which are coded with negative values, the results generation functions handle these as "not-applicable" type responses. With questions that have negative coded options, it separates the respondents who chose the "not-applicable" type options from those who responded with other choices. 

### Question Dictionary

![Question Dictionary](https://github.com/ctesta01/QualtricsTools/blob/master/pics/question%20dictionary.png?raw=true)

The question dictionary lists each response column, the data export tag of the question it corresponds to, the question stem and question choice, the question types, and the response type. 

The question stem refers to the question text field in the Qualtrics survey. The question choice is a column used only when a question has multiple components, and indicates which component of the question that a response column corresponds to. 

The response type describes how the respondent interacts with the question, and is more broad than the question types columns.  

### Text Appendices

![Text Appendices](https://github.com/ctesta01/QualtricsTools/blob/master/pics/text%20appendices.png?raw=true)

Each text appendix is accompanied by a header that provides it an alphabetic enumeration, the question text, a note indicating that the responses are unedited, and the number of respondents. 


### Display Logic

![Display Logic](https://github.com/ctesta01/QualtricsTools/blob/master/pics/display%20logic.png?raw=true)

For each question, if it has display logic, this panel details how the display logic affects how a respondent views the survey or parts of the survey. 

## Include/Exclude Responses

![Include/Exclude Responses](https://github.com/ctesta01/QualtricsTools/blob/master/pics/include%20exclude%20questions.png?raw=true)

Users are able to specify which exact questions they would like to appear in the processed results, including the output results tables, text appendices, 
etc. This is useful for excluding questions which are not intended for reporting purposes, like gathered browser information, or sensitive data not
meant to be included in anonymized reports. After selecting and unselecting the appropriate checkboxes, be sure to click "Apply" to have the changes
made take effect.

## More Options

![More Options](https://github.com/ctesta01/QualtricsTools/blob/master/pics/more%20options.png?raw=true)

### Downloads

In the More Options panel, an interface for users to select the download filetype is provided to allow users
to download their reports in a multitude of formats. The interface is supported with Pandoc, a programmatic 
document converter. 

### Ignore Survey Flow

Users may also choose to specify that QualtricsTools should not use the Survey Flow data included in the QSF 
to reorganize the processed results. Usually the Survey Flow is a good indicator of how the report should be 
formatted, and it is used by default in QualtricsTools. However, there are cases, such as when a survey's 
logic is overly complicated or its flow involves randomized blocks, when QualtricsTools creates a better report
without using the Survey Flow.  

### Splitting Respondents

The splitting respondents panel allows users to select columns of the response data 
that was uploaded and split the respondents into respondent groups. Intended applications of this functionality 
include examples such as splitting across continuing or graduating students in different programs or departments. 
Similarly, other student demographics surveyed can be used to study simple comparative frequencies across student
demographics on campus.

# Understanding the Code

The web application for QualtricsTools, built on top of the [Shiny framework](https://shiny.rstudio.com), is made up 
of a user interface and a server which together handle the input, processing, and display of user submitted data. In addition to the server and UI components, there are small amounts of CSS and JavaScript included in the package. All code for the Shiny application is stored in the `/inst/shiny/` directory and the CSS and JavaScript files are in `/inst/shiny/www/`. 

## The Shiny Server 

A server is a protocol for responding to requests. All kinds of servers are built on the request-response model, including the server that runs the QualtricsTools web app. The Shiny server processes the input data with reactive code blocks, which update their output values whenever the input data is updated, and then uses the reactive blocks' returned values to form output data to deliver to the user. 

The reactive blocks included in the server are as follows.

- `survey_and_responses` is the block which handles the reactive loading of the survey data and the csv data. It returns a list with three elements: the survey, the responses, and the original first rows. 
- `processed_question_and_blocks` uses `get_coded_questions_and_blocks` to insert response data into each question as well as to insert the questions into each block.
- `split_col_responses` creates an additional column over which the application may split respondents if the user chooses to split the reports by response data. 
- `split_blocks` uses the `split_col_responses` output to create separate blocks lists where each contains split responses. 
- `choose_split_block` processes the input choice for which set of split responses to examine in the Shiny app. 
- `uncodeable_message` creates an output message listing which questions could not be automatically processed. 
- `results_tables` produces a list of HTML tables with question descriptions and their frequency breakdowns. 
- `question_dictionary` outputs a dataframe which is processed by DataTables.js to present several columns of information for each response column, including the Question DataExportTag, Question Stem, choice text, question type, and response type information. 
- `text_appendices` are HTML tables in a list with question descriptions and text appendices for each free response question in the survey. 
- `display_logic` tables the display logic for each question and block.
- `include_exclude_dict` converts input checkbox information to a list of questions which will be the only questions tabled in the results tables. 
- `download_names` creates the correct filename for each downloadable file based on the filetype dropdown choice made by the user. 

The output data, often constructed from the output of the reactive code blocks, is saved in the object `output` as named elements. In Shiny, the `input` and `output` objects are used to convey user input information and the output response information from the server function. Here, the output information takes the form of raw HTML, HTML data tables, and download buttons. The output list elements are as follows. 

- `uncodeable_message`, `results_tables`, `text_appendices`, and `display_logic` all render raw HTML from the `uncodeable_message`, `results_tables`, `text_appendices`, and `display_logic` reactive blocks' output.
- question_dictionary` renders a datatable (using DataTables.js behind the scenes, included in the Shiny framework) to render the dataframe it retrieves from the `question_dictionary` reactive block.
- `select_qdict` uses the output from the `include_exclude_qdict` reactive block in order to render a datatable which includes checkboxes for each question to either include or exclude it. 
- `select_response_columns` uses the `selectInput` method included in Shiny to allow the user to choose which response columns they would (optionally) like to use to split the respondents into distinct split reports. This input allows for the user to select multiple response columns over which to split the reports. 
- `select_respondent_group` uses `selectInput` the user to choose of the split respondent groups which they would like to preview in the application or download. 
- `table_respondent_group` renders a table (not using DataTables.js) to show the breakdown of respondents in each of the respondent groups when splitting the survey by response columns. 
- `downloadResultsTables,` `downloadQuestionDictionary,` `downloadTextAppendices,` and `downloadDisplayLogic` each use `html_2_pandoc` to first render the results tables, question dictionary, text appendices, and display logic to a file. The `downloadHandler` then copies the produced file from the temporary file produced to the user's desired download location. 
- `downloadZip` and `downloadSplit` write the results tables, question dictionary, text appendices, and display logic to temporary files, zip them together, and then they use `downloadHandler` to deliver the zipped file to the user in the desired download location. The `downloadSplit` contains an extra for loop which produces each of the reports for each of the blocks before zipping them together. 


## The Shiny UI

The user interface is built using the [shinydashboard package](https://rstudio.github.io/shinydashboard/). The dashboard built for QualtricsTools contains two primary components: the sidebar and the body. 

The sidebar contains file input items for the QSF and CSV, a checkbox input for "Insights?", tabs for the "Processed Results," "Include/Exclude Responses," and "More Options," and two buttons for downloading all the results as a zip and for quitting the app. The sidebar is constructed using the `sidebarMenu` method with `fileInput,` `checkboxInput`, `menuItem`, `downloadButton` and `actionButton` menu items. Reference material for the sidebar can be found in the [ShinyDashboard documentation pages](https://rstudio.github.io/shinydashboard/structure.html#sidebar). 

The body is built using the `dashboardBody` method, with body contents specified in `tabItem` code blocks for each of the "Processed Results," "Include/Exclude Responses," and "More Options" pages. The contents of each of these is a combination of UI elements which either process user input data or data output from the server. The ShinyDashboard body documentation can be found [here](https://rstudio.github.io/shinydashboard/structure.html#body). 

After constructing the `dashboardBody` and `dashboardSidebar` we are able to use the following function call to construct the UI of the shiny dashboard. 

```R
dashboardPage(
  dashboardHeader(title = "Qualtrics Tools"),
  sidebar,
  body
)
```
