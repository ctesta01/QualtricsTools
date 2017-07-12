library(QualtricsTools)

context("Generating generate_text_appendices on Sample Surveys")

surveysdir = file.path(path.package('QualtricsTools'), 'data/Sample Surveys/')

test_that("Test that generate_results_tables creates a file when run on the Better Sample Survey", {
  survey_dir = file.path(surveysdir, "Better Sample Survey")
  qsf = file.path(survey_dir, "Better_Sample_Survey.qsf")
  csv = file.path(survey_dir, "Better_Sample_Survey.csv")
  results_output_file = make_text_appendices(headerrows=3, qsf_path = qsf, csv_path = csv)
  status = file.exists(results_output_file)
  expect_true(status)
})

test_that("Test that generate_results_tables creates a file when run on the Long Exhaustive Sample Survey", {
  survey_dir = file.path(surveysdir, "Long Exhaustive Sample Survey")
  qsf = file.path(survey_dir, "Long_Exhaustive_Sample_Survey.qsf")
  csv = file.path(survey_dir, "Long_Exhaustive_Sample_Survey.csv")
  results_output_file = make_text_appendices(headerrows=3, qsf_path = qsf, csv_path = csv)
  status = file.exists(results_output_file)
  expect_true(status)
})

test_that("Test that generate_results_tables creates a file when run on the No Basis for Evaluation Survey", {
  survey_dir = file.path(surveysdir, "No Basis for Evaluation")
  qsf = file.path(survey_dir, "No_Basis_for_Evaluation.qsf")
  csv = file.path(survey_dir, "No_Basis_for_Evaluation (Insights).csv")
  results_output_file = make_text_appendices(headerrows=3, qsf_path = qsf, csv_path = csv)
  status = file.exists(results_output_file)
  expect_true(status)
})

test_that("Test that generate_results_tables creates a file when run on the Survey Logic Survey", {
  survey_dir = file.path(surveysdir, "Survey Logic")
  qsf = file.path(survey_dir, "Survey_Logic_EVERYWHERE.qsf")
  csv = file.path(survey_dir, "Survey_Logic_EVERYWHERE.csv")
  results_output_file = make_text_appendices(headerrows=3, qsf_path = qsf, csv_path = csv)
  status = file.exists(results_output_file)
  expect_true(status)
})

test_that("Test that generate_results_tables creates a file when run on the User Notes Survey", {
  survey_dir = file.path(surveysdir, "User Notes Survey")
  qsf = file.path(survey_dir, "Notes_Survey.qsf")
  csv = file.path(survey_dir, "Notes_Survey.csv")
  results_output_file = make_text_appendices(headerrows=3, qsf_path = qsf, csv_path = csv)
  status = file.exists(results_output_file)
  expect_true(status)
})
