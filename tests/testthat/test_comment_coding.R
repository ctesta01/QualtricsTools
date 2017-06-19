library(QualtricsTools)

context("Generating text appendices with inserted coded comments")

surveysdir = file.path(path.package('QualtricsTools'), 'data/Sample Surveys/')

test_that("Test that make_coded_comments includes the categories defined in the coded comments.", {
  qsf = file.path(surveysdir, "/Better Sample Survey/Better_Sample_Survey.qsf")
  csv = file.path(surveysdir, "/Better Sample Survey/Better_Sample_Survey.csv")
  sheets_dir = file.path(surveysdir, "/Comment Coding/Coded Comments/")
  output_file <- make_coded_comments(
    qsf_path = qsf,
    csv_path = csv,
    headerrows = 3,
    sheets_dir = sheets_dir,
    n_threshold = 0)
  expect_true(file.exists(output_file))
})
