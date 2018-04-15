library(testthat)

context("Utils testing")


test_that("Correct functioning of 'options' function", {
  expect_error(options("clean_noise", "AENN"), NA)
  expect_error(options("clean_noise", "hybrid"), NA)
  expect_error(options("clean_noise"), NA)
  expect_error(options("feature_selection", "chi_squared"), NA)
  expect_error(options("feature_selection", "hybrid"))
})
