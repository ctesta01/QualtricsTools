% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{get_setup}
\alias{get_setup}
\title{Setup the Global Environment for a Survey}
\usage{
get_setup(qsf_path, csv_path, headerrows, already_loaded,
  return_data_as_list = FALSE, sample_data = FALSE)
}
\arguments{
\item{qsf_path}{The string location of the survey as a .QSF (Qualtrics Survey File)}

\item{csv_path}{The string location of the survey's responses, downloaded from Qualtrics}

\item{headerrows}{An optional parameter for specifying the number of
headerrows in the response csv. While the headerrows parameter can functionally
be set to any number, users predominantly should expect to use headerrows equal to
2 or 3 depending on whether or not their response data was downloaded from Qualtrics
before or after the rollout of the new Qualtrics Insights platform. In the
Qualtrics Insights platform, there are 3 headerrows, while before this update there
were only 2.}

\item{already_loaded}{already_loaded=TRUE indicates that get_setup should
get the survey, responses, and original_first_rows from the global scope
instead of asking the user for them. This parameter is optional and defaults to FALSE.}

\item{return_data_as_list}{An optional boolean parameter which dictates whether the processed
survey data should be returned to the global scope if return_data_as_list=FALSE or is missing,
or if the processed should be returned as a list in the order
c(survey, responses, questions, blocks, original_first_rows, flow) if return_data_as_list=TRUE.}

\item{sample_data}{An optional boolean parameter which when true makes get_setup load the
sample survey data included with the QualtricsTools package. The sample data stored with
the package is stored in the data/sample_*.rda files. The sample data stored there is
Insights formatted data with 3 headerrows.}
}
\description{
This function sets the user up with the survey, responses, questions,
blocks, questions, original_first_rows, and flow. By default, these are
returned to the global scope (referred to by .GlobalEnv or globalenv()).
If return_data_as_list is passed as TRUE, then the data is returned from
the function as a list. The blocks and questions are redundant as they
are already included in the survey, but they are often useful to
have already pulled out of the survey. Among the many processing steps this
function (made both directly and as subsequent nested function calls), the question
text is cleaned and stripped of HTML and any unwanted characters,
the trash questions and blocks are removed, response columns are matched
and inserted into the corresponding questions, and results tables detailing
the frequencies and breakdowns of respondents among each question's choices
are automatically inserted into each applicable question. This function can
be called in multiple ways, specifying the parameters explicitly, or by
specifying them interactively. For example, calling get_setup() with no
parameters will result in a prompt asking for the number of headerrows in
your response data and two subsequent dialogue boxes asking the user
to choose the corresponding QSF and CSV files. If already_loaded=TRUE is
passed, then the get_setup function pulls the survey list and responses
dataframe from the global environment. If
sample_data=TRUE is passed, then a sample survey is loaded.
}
\examples{
# An Interactive Example

> get_setup()
Enter the number of response data header rows [Default: 3]:
Defaulting to headerrows=3
[1] "Select Qualtrics Survey File:"
[1] "Select CSV Response File:"

survey, responses, questions, blocks, original_first_rows,
and flow are now global variables.

# An Explicit Example

> get_setup(
    qsf_path = "C:/Example/Path/to/QSF/File.qsf",
    csv_path = "C:/Example/Path/to/CSV/File.csv",
    headerrows = 3)

survey, responses, questions, blocks, original_first_rows,
and flow are now global variables.

# An Example with return_data_as_list=TRUE

> qualtricstools_values = get_setup(
    qsf_path = "C:/Example/Path/to/QSF/File.qsf",
    csv_path = "C:/Example/Path/to/CSV/File.csv",
    headerrows = 3,
    return_data_as_list=TRUE)

> varnames = c(
    'survey', 'responses', 'questions', 'blocks',
    'original_first_rows', 'flow')
> for (i in 1:length(varnames))
    assign(varnames[[i]], qualtricstools_values[[i]])
> rm(qualtricstools_values, varnames, i)
> ls()

[1] "blocks"              "flow"                "original_first_rows"
[5] "questions"           "responses"           "survey"

# Loading a Sample Survey

> get_setup(sample_data=TRUE)

survey, responses, questions, blocks, original_first_rows,
and flow are now global variables.
}
