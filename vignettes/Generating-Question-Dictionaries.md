```R
get_setup(
  qsf_path='C:/Users/oirera01/Desktop/Sample_Survey.qsf',
  csv_path='C:/Users/oirera01/Desktop/Sample_Survey.csv',
  headerrows=3
)

question_dictionary <- create_response_column_dictionary(blocks, flow_from_survey(survey), original_first_rows)
View(question_dictionary)
```