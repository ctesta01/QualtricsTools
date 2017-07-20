By breaking down the Qualtrics survey responses and survey file and reintegrating them, while we have the data available in R we are able to merge in externally provided or constructed data to supplement our reports. For the OIRE the supplemental information included in our reports usually includes categorization and frequency breakdowns among these categories of text responses to a given question. The "coded comments" data comes in as CSV files which have the following standardized structure:

![Coded Comments Example](http://i.imgur.com/Oahr8tk.png)

Extra columns can come to the left of the "varname" column, but the Boolean categorization information (the 1s columns) must start in the columns 2 to the right of the "varname" column. Further, the "varname" must remain constant for each file. The tab in which the comments are coded must be named (capitalization arbitrary) "Coded". The response IDs must be in the first column of the coded comments files. Given a directory full of such comment coding files (csv, xls, or xlsx), we can automatically process a survey which includes them as follows. 

```R
library(QualtricsTools)

make_coded_comments(
  qsf_path = "C:/Example/Path/to/SurveyFile.qsf",
  csv_path = "C:/Example/Path/to/ResponseFile.csv",
  headerrows = 3,
  sheets_dir = "C:/Example/Directory/For/Coded/Comments",
  output_dir = "C:/Example/Path/Output Testing/",
  filename = "Testing Comment Coding.docx",
  n_threshold = 15)
```

If adjustments to the survey are necessary before exporting the coded comments, 
the survey data can be loaded through `get_setup`, edited and adjusted, 
and then processed with coded comments through `make_coded_comments`. 

```R
# Load and process the survey
get_setup(qsf_path = "C:/Example/Path/to/SurveyFile.qsf",
          csv_path = "C:/Example/Path/to/ResponseFile.csv",
          headerrows = 3)

# Exclude a particular question and reinsert it into the blocks
questions[[1]][['qtSkip']] = TRUE
blocks <- questions_into_blocks(questions, blocks)

# Insert coded comments and render text appendices
make_coded_comments(
    sheets_dir = "C:/Example/Directory/For/Coded/Comments",
    output_dir = "C:/Example/Path/Output Testing/",
    filename = "Testing Comment Coding.docx",
    n_threshold = 15)

```