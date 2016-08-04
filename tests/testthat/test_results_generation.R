library(QualtricsTools)
context("Results Generation: mcsa, mcma, matrix-sa")

# testing a multiple choice single answer (radio button) question
test_that("plain mcsa results are processed correctly", {
  plain_mcsa <- readRDS(system.file("testdata", "plain_mcsa.RDS", package="QualtricsTools"))
  results_table <- mc_single_answer_results(plain_mcsa)
  expect_equal(results_table[['N']], c(0, 1, 4))
  expect_equal(as.character(results_table[['Percent']]), c("0.0%", "20.0%", "80.0%"))
  expect_equal(as.character(results_table[[3]]), c("Click to write Statement 1",
                                                   "Click to write Statement 2",
                                                   "Click to write Statement 3"))
})

# testing a multiple choice single answer (radio button) question with recoded values
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

# testing a multiple choice multiple answer (check all) question
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

# testing a multiple choice multiple answer (check all) question
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

# testing matrix single answer with recode values and an NA option
test_that("matrix single answer results with recoded values and NA-choice(s) are processed correctly",{
  recode_na_mx_sa <- readRDS(system.file("testdata", "recode_na_matrix_sa.RDS", package="QualtricsTools"))
  results_table <- matrix_single_answer_results(recode_na_mx_sa)
  expect_equal(as.character(results_table[[1]]),
               c("Cantaloupe", "Kiwi", "Pomegranate"))
  expect_equal(results_table[['N']],
               c(5,5,3))
  expect_equal(as.character(results_table[['Enjoy']]),
               c("20.0%", "0.0%", "33.3%"))
  expect_equal(as.character(results_table[['Ambivalent']]),
               c("60.0%", "60.0%", "33.3%"))
  expect_equal(as.character(results_table[['Dislike']]),
               c("20.0%", "40.0%", "33.3%"))
  expect_equal(results_table[['total_N']],
               c(6,6,6))
  expect_equal(as.character(results_table[['Never Tried It']]),
               c("16.7%", "16.7%", "50.0%"))
})

# testing a plain matrix multiple answer (checkbox) question
test_that("matrix multiple answer results are processed correctly", {
  matrix_ma <- readRDS(system.file("testdata", "plain_matrix_ma.RDS", package="QualtricsTools"))
  results_table <- as.data.frame(matrix_multiple_answer_results(matrix_ma))
  expect_equal(colnames(results_table),
               c(" ", "N", "In the last week", "In the last month", "In the last year"))
  expect_equal(as.character(results_table[,1]),
               c("Canteloupe", "Kiwi", "Pomegranate"))
  expect_equal(as.numeric(results_table[['N']]),
               c(10,10,10))
  expect_equal(as.character(results_table[['In the last week']]),
               c("70.0%", "80.0%", "70.0%"))
  expect_equal(as.character(results_table[['In the last month']]),
               c("80.0%", "100.0%", "60.0%"))
  expect_equal(as.character(results_table[['In the last year']]),
               c("70.0%", "80.0%", "80.0%"))
})
