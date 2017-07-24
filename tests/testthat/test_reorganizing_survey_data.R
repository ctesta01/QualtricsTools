library(QualtricsTools)

context("Testing for correct structure in the output of functions from reorganizing_survey_data.R")

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


test_that("Notes are inserted into questions by insert_notes_into_questions", {
  qsf_path <- file.path(surveysdir, "User Notes Survey/Notes_Survey.qsf")
  survey <- ask_for_qsf(qsf_path)
  blocks <- blocks_from_survey(survey)
  # insert_notes_into_questions is performed by get_coded_questions_and_blocks
  questions_and_blocks <- get_coded_questions_and_blocks(survey=survey, responses=data.frame(), original_first_rows=data.frame())
  questions <- questions_and_blocks[[1]]
  # Test that there exist qtNotes with "User Note" inserted into them.
  user_notes_exist_in_questions <-
    length(which(sapply(questions, function(x) {
      'qtNotes' %in% names(x) &&
        grepl("User Note", x[['qtNotes']])
    }))) > 0
  expect_true(user_notes_exist_in_questions)
})


