
# Top Level Files

These are the source code files which are contained in the [R/](https://github.com/ctesta01/QualtricsTools/tree/master/R) folder. Each of the files and its contents are described below.

  - [`comment_coding.R`](#comment_codingr)
  - [`data.R`](#datar)
  - [`helper_functions.R`](#helper_functionsr)
  - [`html_results.R`](#html_resultsr)
  - [`loading_results.R`](#loading_resultsr)
  - [`pandoc_conversion.R`](#pandoc_conversionr)
  - [`question_type_checking.R`](#question_type_checkingr)
  - [`reorganizing_survey_data.R`](#reorganizing_survey_datar)
  - [`results_generation.R`](#results_generationr)

## comment_coding.R

The functions contained in comment_coding.R are written for users to import 
CSV and XLSX sheets which categorize the raw text comments of a survey
and include them as "Coded Comments" tables in the Text Appendices portion
of the generated report.  

  - `directory_get_coded_comment_sheets`
  - `get_coded_comment_sheets`
  - `format_coded_comments`
  - `format_coded_comment_sheets`
  - `merge_split_column_into_comment_sheet`
  - `format_and_split_comment_sheets`
  - `insert_coded_comments`
  - `insert_split_survey_comments`
  - `generate_split_coded_comments`

## data.R

This file does not contain any functions, but rather provides the names 
of the data files which should be imported from the [data/](https://github.com/ctesta01/QualtricsTools/tree/master/data) folder. This includes some sample data
for testing.

## helper_functions.R

The helper functions contained in this file are functions which are relied
upon by many functions found in different parts of the application. The 
functions `app` and `get_setup` are two functions which users will regularly
encounter. Functions such as `question_from_response_column`, `find_question`,
`find_question_index`, `find_question_index_by_qid`, and others are functions
which lookup values or indices corresponding to their inputs and are relied
upon in the application to find where data should be sourced from or inserted into. 

  - `list_of_rows_to_df`
  - `question_from_response_column`
  - `choice_text_from_question`
  - `app`
  - `get_setup`
  - `find_question`
  - `find_question_index`
  - `find_question_index_by_qid`
  - `choice_text_from_response_column`
  - `blocks_header_to_html`
  - `number_of_blocks`
  - `questions_from_blocks`
  - `flow_from_survey`


## html_results.R

The QualtricsTools application builds reports in HTML before rendering them 
into other formats with Pandoc. The functions contained in this file are those
which help in rendering the HTML reports seen in the Shiny app. `tabelize_blocks`, `text_appendices_table`, and `tabelize_display_logic` are the most critical
functions in this file, and they produce the Results Tables, Text Appendices, and Survey Logic reports. 

  - `tabelize_blocks`
  - `question_description`
  - `has_display_logic`
  - `text_appendices_table`
  - `uncodeable_questions_message`
  - `tabelize_display_logic`
  - `appendix_lettering`
  - `table_html_coded_comments`
  - `table_no_respondents`
  - `table_text_entry`
  - `table_non_text_entry`

## loading_results.R

These are functions which are used to import Qualtrics survey data in a standardized way, as well as to provide a graphical interface to choose the data to be loaded. `validate_data_export_tags` ensures that the Qualtrics Survey File uploaded does not have any duplicate data export tags. 

  - `load_csv_data`
  - `load_qsf_data`
  - `ask_for_qsf`
  - `ask_for_csv`
  - `validate_data_export_tags`

## pandoc_conversion.R

The data which is produced in `html_results.R` is passed to this single
function, with parameters including the desired output extension and
desired output file name. `html_2_pandoc` then uses system calls to Pandoc
to convert the file (in temporary memory) and then moves it to the desired
output location. 

  - `html_2_pandoc`

## question_type_checking.R

An integral part of the results generation is the tabling of frequencies of 
responses and choices to a given question. Depending on the question's format, 
these frequencies need to be tabled differently. In order to reliably detect
what format a given question is, the following functions are used, which each
return boolean values indicating the type of a question provided as an argument. 

  - `is_mc_multiple_answer`
  - `is_matrix_multiple_answer`
  - `is_mc_single_answer`
  - `is_matrix_single_answer`
  - `is_matrix_bipolar`
  - `is_multiple_choice`
  - `is_single_answer`
  - `is_rank_order`
  - `is_text_entry`
  - `is_matrix_question`

## reorganizing_survey_data.R

Qualtrics provides its survey and response data separately, as a QSF and CSV file which 
can be downloaded. However, the QualtricsTools application often needs the context of 
the data which is included in both and therefore the following functions have been 
built to reorganize and combine survey data into structures more usable in R. 
Firstly, the response data is linked to its corresponding question with 
`link_responses_to_questions`. Additionally, Qualtrics lists questions and 
blocks as top level elements in the `SurveyElements` list in the QSF, and 
then lists within the blocks the IDs of questions included in each block. 
`blocks_from_survey`, `questions_from_survey`, `questions_into_blocks`,
and others handle extracting this data and integrating it into single lists 
which contain, for example, all blocks with all questions inserted into them,
or all questions in a list with nothing else. 

  - `get_coded_questions_and_blocks`
  - `blocks_from_survey`
  - `notes_from_survey`
  - `insert_notes_into_questions`
  - `questions_from_survey`
  - `remove_trash_questions`
    - `delete_if_in_trash`
  - `remove_trash_blocks`
  - `link_responses_to_questions`
  - `questions_into_blocks`
  - `clean_question_text`
    - `remove_css_style`
  - `clean_html`
    - `clean_html_tags`
    - `clean_html_entities`
    - `clean_extra_whitespace`
    - `clean_leading_whitespace`
  - `human_readable_qtype`
    - `create_qtype`
  - `create_question_dictionary`
    - `create_entry`
  - `uncodeable_question_dictionary`
  - `lean_responses`
    - `create_entry`
  - `answers_from_response_column`
  - `split_side_by_sides`
  - `display_logic_from_question`
  - `split_respondents`
  - `create_response_column_dictionary`
    - `create_entry`
  - `create_panel_data`
  - `create_merged_response_column`

## results_generation.R

These functions do the frequency tabling for the given question types. Included 
in these functions is functionality to look up the "choice text" which corresponds
to a response's variable value (depending on things like "RecodedValues," "Choices", "Answers", and so forth),
as well as logic which dictates how the frequencies for each response should be tabulated. 
Finally, the results of gathering this data are then tabled in a data frame which is returned.

  - `percent0`
  - `mc_single_answer_results`
  - `mc_multiple_answer_results`
  - `matrix_single_answer_results`
  - `matrix_multiple_answer_results`
  - `rename_choices`
  - `generate_results`


