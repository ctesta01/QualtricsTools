library(QualtricsTools)
context("Results Generation - Multiple Choice Single Answer")

# testing multiple choice single answer (radio button) questions
test_that("plain mcsa results are processed correctly", {
  plain_mcsa <- readRDS(system.file("testdata", "plain_mcsa.RDS", package="QualtricsTools"))
  results_table <- mc_single_answer_results(plain_mcsa)
  expect_equal(results_table[['N']], c(0, 1, 4))
  expect_equal(as.character(results_table[['Percent']]), c("0.0%", "20.0%", "80.0%"))
  expect_equal(as.character(results_table[[3]]), c("Click to write Statement 1",
                                                   "Click to write Statement 2",
                                                   "Click to write Statement 3"))
})

# testing multiple choice single answer (radio button) questions with recoded values
test_that("recoded mcsa results are processed correctly", {
  recode_mcsa <- readRDS(system.file("testdata", "recode_mcsa.RDS", package="QualtricsTools"))
  results_table <- mc_single_answer_results(recode_mcsa)
  expect_equal(results_table[['N']],
               c(1, 0, 0, 1, 0, 2, 1))
  expect_equal(as.character(results_table[['Percent']]),
               c("20.0%", "0.0%",  "0.0%",  "20.0%", "0.0%",  "40.0%", "20.0%"))
  expect_equal(as.character(results_table[[3]]),
               c("Click to write Statement 1",
                 "Click to write Statement 2",
                 "Click to write Statement 3",
                 "Click to write Choice 4",
                 "Click to write Choice 5",
                 "Click to write Choice 6",
                 "Click to write Choice 7"))
})

# testing multiple choice multiple answer (check all) questions
test_that("plain mcma results are processed correctly", {
  plain_mcma <- readRDS(system.file("testdata", "plain_mcma.RDS", package="QualtricsTools"))
  results_table <- mc_multiple_answer_results(plain_mcma)
  expect_equal(results_table[['N']],
               c(2, 1, 2, 8))
  expect_equal(as.character(results_table[['Percent']]),
               c("20.0%", "10.0%",  "20.0%",  "80.0%"))
  expect_equal(as.character(results_table[[3]]),
               c("Cantaloupe",
                 "Kiwi",
                 "Pomegranate",
                 "None of the above"))
})

# testing multiple choice multiple answer (check all) questions
test_that("recoded mcma results are processed correctly", {
  recode_mcma <- readRDS(system.file("testdata", "recode_mcma.RDS", package="QualtricsTools"))
  results_table <- mc_multiple_answer_results(recode_mcma)
  expect_equal(results_table[['N']],
               c(1,0,1,5))
  expect_equal(as.character(results_table[['Percent']]),
               c("16.7%", "0.0%", "16.7%", "83.3%"))
  expect_equal(as.character(results_table[[3]]),
               c("Cantaloupe",
                 "Kiwi",
                 "Pomegranate",
                 "None of the above"))
})
