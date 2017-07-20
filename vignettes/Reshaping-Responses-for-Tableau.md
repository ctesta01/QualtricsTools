Tableau significantly prefers long and lean data to bulky wide data. To translate the data that Qualtrics exports to data better suited for insights, there are three functions in the QualtricsTools package here to help. 

(If you're using Insights, use `headerrows=3`. If you're using legacy data, use `headerrows=2`). 
Here we demonstrate two different ways to write the output, into CSV files and into a single XLS file using respectively the data.table package's fwrite function and the WriteXLS package.

```R
library(QualtricsTools)
get_setup(headerrows=3)
lean_dict <- lean_responses(blocks, responses)
qdict <- create_response_column_dictionary(blocks, flow_from_survey(survey), original_first_rows[1,])
panel_df <- create_panel_data(c("V10", "q1_favorite"), responses, lean_dict, qdict)

# Here we use fwrite over write.csv because fwrite takes advantage of a 
# computer having multiple cores. The vast majority of modern computers do have
# multiple cores, and so fwrite often performs significantly better than write.csv.
# fwrite is from the data.table package which may need to be installed.

if(!require('data.table')) { install.packages('data.table') } # Ensure data.table is installed.
library('data.table') # Load data.table package for the fwrite function.
fwrite(x = lean_dict, file = "C:/Users/username/Desktop/lean_dictionary.csv", row.names=FALSE)
fwrite(x = qdict, file = "C:/Users/username/Desktop/question_dictionary.csv", row.names=FALSE)
fwrite(x = panel_df, file = "C:/Users/username/Desktop/panel_data.csv", row.names=FALSE)

# While we can use WriteXLS, fwrite and write.csv are more reliable options because 
# Excel files can only support up to a finite number of rows where as CSVs are much less
# limited in length. Here is an example of how to write an Excel file with three sheets containing 
# each of these documents.
if(!require('WriteXLS')) { install.packages('WriteXLS') } # Ensure WriteXLS is installed.    
library(WriteXLS)    
WriteXLS(c("lean_dict", "qdict", "panel_df"), ExcelFileName = "C:/Users/oirera01/Documents/example_long_and_lean.xls")
```


This outputs a workbook with three sheets:

![lean_dict worksheet](http://i.imgur.com/jx9guPP.png)
![qdict worksheet](http://i.imgur.com/sP35DJv.png)
![panel_df worksheet](http://i.imgur.com/AIEkDGn.png)

The three functions designed specifically to help with this reshaping process are `lean_responses()`, `create_response_column_dictionary()`, and `create_panel_data()`. 

`lean_responses()` requires two parameters, `question_blocks` and `survey_responses`. 
The `question_blocks` parameter is a list of blocks with questions inserted in place of the block elements' names. This is most easily created by using the `get_setup()` function and using the `blocks` variable that it outputs to the global scope. The `survey_responses` parameter is more straightforward -- it's just the `responses` data frame as imported by `ask_for_csv()`. The output of the function is a data frame with a single response in each row. Each row contains a respondent ID, the response column, the raw response, and the coded response. 

![lean_responses autocompleted parameters](http://i.imgur.com/I7gLGIj.png)

`create_response_column_dictionary()` requires two parameters, `question_blocks` and `orig_first_row`. The `question_blocks` refers to the same parameter that was given to `lean_responses()` above. The `orig_first_row` refers to a data frame with only one row: the first row of the response CSV with headers. The `get_setup()` function provides both a `blocks` variable and an `original_first_row` variable to the global scope that can be used as the `question_blocks` and `orig_first_row` parameters respectively. The output of the function is a data frame with each response column in a unique row. Each row contains the data export tag of the question the response column corresponds to, the question stem and question choice, the question types, and the response type. 

![create_response_column_dictionary autocompleted parameters](http://i.imgur.com/uiYGXdd.png)

`create_panel_data()` requires one parameter and optionally takes an additional three. The first (required) parameter is a list of response column names to be included in the panel data. The next three are the `survey_responses`, the `lean_responses` and the `question_dict`. The `survey_responses` are the responses as a data frame as imported by the `ask_for_csv()` function, or as provided by the `get_setup()` function. The `lean_responses` parameter is a data frame created using the `lean_responses()` function. The `question_dict` parameter is a question dictionary created using the `create_response_column_dictionary()` function. The output is a data frame with each respondent in a row, and each specified panel data column included either verbatim from the response set, or as raw and coded responses included from the `lean_responses` data frame. 

![create_panel_data autocompleted parameters](http://i.imgur.com/asC4Whd.png)

The data is split into three different outputs to decrease the amount of repetition. Of course, you can create only one, or two, or all three of these data frames, depending on your needs. 

I hope this helps. 