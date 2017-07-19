# Results Tables on the Command Line

If you would like to write a script, or simply use R rather than the QualtricsTools app, 
then you can use the functions which are built into the QualtricsTools package to process
a Qualtrics Survey File and its response data automatically. The following code snippets show 
different ways to process results tables. Depending on the level of interactivity necessary, 
different methods will be better for each individual's needs. 

The last example makes use of the Survey Flow. If you would like to avoid using it, simply 
do not define `flow`, and do not pass it as an argument to `text_appendices_table`. 

## Generating with `make_results_tables`
```R
# If not passed arguments, get_setup prompts the user for them, 
# and make_results will automatically pull them from the global scope.
get_setup()
make_results_tables(output_dir='C:/Example/Path/Here/')
```
```R
# Alternatively every argument can be made explicit.
make_results_tables(qsf_path = 'C:/Example/Path/to/QSF_file.qsf',
                    csv_path = 'C:/Example/path/to/CSV_file.csv',
                    headerrows = 3, 
                    output_dir = 'C:/Users/me/Desktop/',
                    filename = 'Surveyname Results Tables.docx')
```

If `make_results_tables` is not passed a qsf_path, csv_path, and headerrows, it automatically pulls them from the global scope. This means that you may use `get_setup()` to import a survey, edit its questions and other details, and then use 
`make_results_tables` after editing to render edited results tables. 

If you use `make_results_tables` before loading a survey, it will call `get_setup`, and you can select your survey and response files.

## Generating Results Tables with `get_setup`
```R
# Import Survey Data
get_setup(
  qsf_path='C:/Users/oirera01/Desktop/Sample_Survey.qsf',
  csv_path='C:/Users/oirera01/Desktop/Sample_Survey.csv',
  headerrows=3
)

# Output Formatted Report
output_dir = 'C:/Users/oirera01/Desktop/'
html_2_pandoc(
  html=c(blocks_header_to_html(blocks), tabelize_blocks(blocks, flow)), 
  file_name='sample survey results tables.docx',
  output_dir=output_dir
)
```

## Generating Results Tables without `get_setup`
```R
qsf_path = 'C:/Users/oirera01/Desktop/Sample_Survey.qsf'
csv_path = 'C:/Users/oirera01/Desktop/Sample_Survey.csv'
headerrows = 3 

survey <- ask_for_qsf(qsf_path)
responses <- ask_for_csv(csv_path, headerrows=headerrows)
original_first_rows <- as.data.frame(responses[[2]])
responses <- as.data.frame(responses[[1]])
blocks <- blocks_from_survey(survey)
questions <- questions_from_survey(survey)
questions <- remove_trash_questions(questions, blocks)
blocks <- remove_trash_blocks(blocks)
questions_and_blocks <- split_side_by_sides(questions, blocks)
questions <- questions_and_blocks[[1]]
blocks <- questions_and_blocks[[2]]
questions <- clean_question_text(questions)
questions <- human_readable_qtype(questions)
questions <- link_responses_to_questions(questions, responses, original_first_rows)
questions <- generate_results(questions, original_first_rows)
blocks <- questions_into_blocks(questions, blocks)
blocks[['header']] <- c(paste0("Survey Name: ",
                               survey[['SurveyEntry']][['SurveyName']]),
                        paste0("Number of Respondents: ",
                               nrow(responses)))
flow <- flow_from_survey(survey)

output_dir = 'C:/Users/oirera01/Desktop/'
html_2_pandoc(
  html=c(blocks_header_to_html(blocks), tabelize_blocks(blocks, flow)), 
  file_name='sample survey results tables.docx',
  output_dir=output_dir
)
```

