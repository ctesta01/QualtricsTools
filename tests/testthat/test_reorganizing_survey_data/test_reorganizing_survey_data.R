library(QualtricsTools)

context("Generating functions in /R/reorganizing_survey_data.R")

surveysdir = file.path(path.package('QualtricsTools'), 'data/Sample Surveys/')

test_that("Test that blocks_from_survey returns a list with elements which each have a Type, Description, and ID", {
  qsf_path = file.path(surveysdir, "/Better Sample Survey/Better_Sample_Survey.qsf")
  headerrows <- 3
  survey <- ask_for_qsf(qsf_path)
  blocks <- blocks_from_survey(survey)
  all_blocks_contain_type <- all(sapply(blocks, function(x) "Type" %in% names(x)))
  all_blocks_contain_ID <- all(sapply(blocks, function(x) "ID" %in% names(x)))
  all_blocks_contain_description <- all(sapply(blocks, function(x) "Description" %in% names(x)))
  expect_true(all(c(all_blocks_contain_ID, all_blocks_contain_type, all_blocks_contain_description)))
  })
