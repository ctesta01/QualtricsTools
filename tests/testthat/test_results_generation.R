library(QualtricsTools)
context("Results Generation - Multiple Choice Single Answer")

test_that("plain mcsa results are processed correctly", {
  plain_mcsa <- readRDS(system.file("testdata", "plain_mcsa.RDS", package="QualtricsTools"))
  expect_equal(mc_single_answer_results(plain_mcsa)[['N']], c(0, 1, 4))
  expect_equal(as.character(mc_single_answer_results(plain_mcsa)[['Percent']]), c("0.0%", "20.0%", "80.0%"))
  expect_equal(as.character(mc_single_answer_results(plain_mcsa)[[3]]), c("Click to write Statement 1",
                                                                          "Click to write Statement 2",
                                                                          "Click to write Statement 3"))
})

test_that("recoded mcsa results are processed correctly", {
  recode_mcsa <- readRDS(system.file("testdata", "recode_mcsa.RDS", package="QualtricsTools"))
  expect_equal(mc_single_answer_results(recode_mcsa)[['N']],
               c(1, 0, 0, 1, 0, 2, 1))
  expect_equal(as.character(mc_single_answer_results(recode_mcsa)[['Percent']]),
               c("20.0%", "0.0%",  "0.0%",  "20.0%", "0.0%",  "40.0%", "20.0%"))
  expect_equal(as.character(mc_single_answer_results(recode_mcsa)[[3]]),
               c("Click to write Statement 1",
                 "Click to write Statement 2",
                 "Click to write Statement 3",
                 "Click to write Choice 4",
                 "Click to write Choice 5",
                 "Click to write Choice 6",
                 "Click to write Choice 7"))
})

test_that("plain mcma results are processed correctly", {
  plain_mcma <- readRDS(system.file("testdata", "plain_mcma.RDS", package="QualtricsTools"))
  expect_equal(mc_multiple_answer_results(plain_mcma)[['N']],
               c(2, 1, 2, 8))
  expect_equal(as.character(mc_multiple_answer_results(plain_mcma)[['Percent']]),
               c("20.0%", "10.0%",  "20.0%",  "80.0%"))
  expect_equal(as.character(mc_single_answer_results(plain_mcma)[[3]]),
               c("Cantaloupe",
                 "Kiwi",
                 "Pomegranate",
                 "None of the above"))
})

