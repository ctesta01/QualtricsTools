In this page there are several functions which are described and walked-through here. The functions chosen are those that are the most critical, for reasons varying from their frequency of use to their subtlety and detail. 

Short summaries: 

- `get_setup` loads survey and response data into R.
- `link_responses_to_questions` inserts response data into questions. 
- `get_coded_questions_and_blocks` inserts questions into blocks. 
- `tabelize_blocks` creates the html results tables of the questions.
- `text_appendices_table` creates the html text appendices. 

## `get_setup`

Arguments:


-  `qsf_path,`
-  `csv_path,`
-  `headerrows,`
-  `already_loaded,`
-  `return_data`
-  `sample_data`

`get_setup` serves multiple purposes: to interactively load Qualtrics survey data, to load survey data from specified paths, or to process survey data already loaded into the R environment. If qsf_path, csv_path, and headerrows are all passed directly as arguments, the files will be read in from those paths. If none of those are passed directly, the function will prompt the user to enter the number of headerrows and provide dialogue boxes for selecting the QSF and CSV files. If already_loaded=TRUE is passed to get_setup, then the `survey, `responses,` and `original_first_rows` will be read from the global scope. If `sample_data=TRUE` is passed to `get_setup`, 
then sample survey data will be loaded. 

The `get_setup` function then processes the survey and responses by running the following.

```R 
questions_and_blocks <- get_coded_questions_and_blocks(survey, responses, original_first_rows).
```
This call to `get_coded_questions_and_blocks` does all the heavy lifting with respect to organizing the Qualtrics survey data into a format that can be processed by QualtricsTools. 

If `return_data=FALSE` is passed to `get_setup`, or `return_data` is simply not specified, then the survey, questions, blocks, responses, original_first_rows, and flow will all be added to the global scope rather than returned directly. Conversely, if `return_data=TRUE` is passed to `get_setup` then the function will return a list constructed as follows. 

To learn more about global variables, scopes, and environments, check out either [this intro to R Environment and Scope](https://www.programiz.com/r-programming/environment-scope) or for the more technical and advanced usage, check out the [section on Environments in Hadley Wickham's Advanced R](http://adv-r.had.co.nz/Environments.html).

```R
    return_vals = list(
      "survey" = survey,
      "responses" = responses,
      "questions" = questions,
      "blocks" = blocks,
      "original_first_rows" = original_first_rows,
      "flow" = flow
    )
```
## `link_responses_to_questions`

Arguments:

- `questions,`
- `responses,`
- `original_first_rows`

The `link_responses_to_questions` function is one which is called before nearly all others both in the web-app and in the `get_setup` or `get_coded_questions_and_blocks` functions. This function takes as arguments two data frames (`responses` and `original_first_rows`) and a list of questions. The questions are pulled out from the original QSF survey data using `questions_from_survey`, and trash questions are excluded, but they are not originally paired with the response data when the QSF data is imported as JSON into R. This function goes through the questions and pairs the relevant response columns to each question. 

Depending on whether or not the original_first_rows indicate Legacy or Insights Qualtrics response data, either the 
original first rows' contents is used (in the Insights case) or the response column names are used directly. 
The `original_first_rows` are a data frame which contains the rows which are not response data included in Qualtrics CSVs. If there are three rows of them, and the data is from an Insights survey, then the `original_first_rows` contains QIDs which are used to match questions exactly. Otherwise, for a given question, if a column name starts with the question's Data Export Tag (`question[['Payload']][['DataExportTag']]`) (potentially followed by an underscore, dash, or hashtag) or with one of the question's ChoiceDataExportTags, then the response column corresponding to the column name is matched to that question and inserted into `question[['Responses']]`. 

## `get_coded_questions_and_blocks`

Arguments:

- `survey`
- `responses`
- `original_first_rows`

`get_coded_questions_and_blocks` is one of the first functions called by `get_setup()` because it combines the information that is contained separately in the csv data and the qsf data into questions and blocks which contain 
the relevant information from each. The blocks start out in the qsf as lists of BlockElements, which reference QuestionIDs in the question BlockElements. This function inserts the actual questions from elsewhere in the QSF into this organized list of the blocks. Further, it takes the response data and through `link_responses_to_questions` it 
inserts the relevant csv response columns into the correct questions. Throughout this process it is cleaning out trash questions and removing unsafe strings from the question text (like HTML), removing trash questions/blocks, and generally tidying up the data. Finally, it returns this list of blocks with questions and responses included in them, and it returns a list of questions including responses without any block structure. 


## `tabelize_blocks`

Arguments: 

- `blocks`
- `flow`
- `include_block_headers`

This function creates the results tables as a body of HTML text from the contents of the blocks. Most fundamentally, it is looping through the questions and calling the `question_description` function on each and concatenating all the question's HTML descriptions and results tables. The HTML is first stored in a list called `tables`, to which the descriptions and results tables for each question are appended. The survey's `flow` (an ordered list of block IDs), extracted from the survey using `flow_from_survey`, is optionally used to ensure that the questions are presented in the HTML in the same ordering as in the survey. If the flow is not passed to this question, the questions are presented in the HTML in the order they are in the blocks inside the QSF. In `tabelize_blocks` there are two nested loops: one looping over the blocks, and within each, one looping over the BlockElements. If `include_block_headers` is TRUE (by default or explicitly), then for each block a block header including the block's description is printed before that block's questions. Within these loops, questions [marked for skipping](https://github.com/ctesta01/QualtricsTools/wiki/Including-Excluding-a-Specific-Question) and Descriptive Box questions are skipped. For those questions not skipped, the results of `question_description(blocks[[i]][['BlockElements']][[j]])` are appended to the list of HTML. After both loops finish, the HTML is unlisted and returned as a single HTML body of text. 

## `text_appendices_table` 

Arguments: 

- `blocks`
- `original_first_row`
- `flow`
- `n_threshold` 

This function creates HTML text appendices, tabling each text entry component of free response questions as individual text appendices. This function relies heavily on the following functions:
- `appendix_lettering` which turns a positive integer into the corresponding alphabetic numbering (i.e. 1 -> A, 26 -> Z, 27 -> AA). 
- `table_text_entry` creates text appendices for text entry type questions. 
- `table_no_respondents` creates the text appendix for a text entry question to which there were no responses. 
- `table_non_text_entry` creates the text appendix for a text entry component of a question.
- `choice_text_from_response_column` in this context returns the choice text which corresponds to the text entry component of a specific question. 
- `table_html_coded_comments` creates the text appendix corresponding to categorical breakdowns of a text entry response column, inserted to a question as "coded_comments". 

Using the above functions, the `text_appendices_table` function loops through the blocks and questions, selecting the text entry columns of the responses to each question, and then uses the appropriate tabling function to create and save each text appendix. 