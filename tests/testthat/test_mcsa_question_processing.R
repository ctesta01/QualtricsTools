# Test mc_single_answer_results

context("Testing mc_single_answer_results")
rm(list=ls()) # clear the environment
# surveysdir is the sample surveys filepath
surveysdir = file.path(path.package('QualtricsTools'), 'data/Sample Surveys/')

test_that("Test that mc_single_answer_results is correct for q2_favorite in the Better Sample Survey", {
  survey_dir = file.path(surveysdir, "Better Sample Survey")
  qsf = file.path(survey_dir, "Better_Sample_Survey.qsf")
  csv = file.path(survey_dir, "Better_Sample_Survey.csv")
  # Use capture.output to suppress printed messages in testing.
  # Syntax from https://stackoverflow.com/questions/2723034/suppress-one-commands-output-in-r
  capture.output(get_setup(headerrows=3, qsf_path=qsf, csv_path=csv), file='NUL')
  # the only things which should be in the environment are now
  # survey, responses, flow, blocks, questions, original_first_rows,
  # surveysdir, and survey_dir.

  index1 = find_question_index(questions, 'q2_favorite')
  df = as.data.frame(questions[[index1]][['Table']])
  expect_equal(df[['N']], c(4,5,1))
  expect_equal(as.character(df[["Percent"]]), c('40.0%', '50.0%', '10.0%'))
  expect_equal(as.character(df[[3]]), c('Canteloupe', 'Kiwi', 'Pomagranite'))
})

test_that("Test that mc_single_answer_results is correct for ", {
  survey_dir = file.path(surveysdir, "Long Exhaustive Sample Survey")
  qsf = file.path(survey_dir, "Long_Exhaustive_Sample_Survey.qsf")
  csv = file.path(survey_dir, "Long_Exhaustive_Sample_Survey.csv")
  # Use capture.output to suppress printed messages from get_setup in testing.
  # Syntax from https://stackoverflow.com/questions/2723034/suppress-one-commands-output-in-r
  capture.output(get_setup(headerrows=3, qsf_path=qsf, csv_path=csv), file='NUL')

  index1 = find_question_index(questions,'Q2')
})
