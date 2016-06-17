library(QualtricsTools)
context("Results Generation - Multiple Choice Single Answer")

test_that("plain mcsa results are processed correctly", {
  plain_mcsa <- readRDS(system.file("testdata", "plain_mcsa.RDS", package="QualtricsTools"))
  expect_equal(mc_single_answer_results(plain_mcsa)[['N']][[1]], 0)
  expect_equal(mc_single_answer_results(plain_mcsa)[['N']][[2]], 1)
  expect_equal(mc_single_answer_results(plain_mcsa)[['N']][[3]], 4)
  expect_equal(as.character(mc_single_answer_results(plain_mcsa)[['Percent']][[1]]), "0.0%")
  expect_equal(as.character(mc_single_answer_results(plain_mcsa)[['Percent']][[2]]), "20.0%")
  expect_equal(as.character(mc_single_answer_results(plain_mcsa)[['Percent']][[3]]), "80.0%")
  expect_equal(as.character(mc_single_answer_results(plain_mcsa)[[3]][[1]]), "Click to write Statement 1")
  expect_equal(as.character(mc_single_answer_results(plain_mcsa)[[3]][[2]]), "Click to write Statement 2")
  expect_equal(as.character(mc_single_answer_results(plain_mcsa)[[3]][[3]]), "Click to write Statement 3")
})

