library(testthat)

context("Utils testing")


test_that("Correct functioning of 'options' function", {
  expect_error(which_options("clean_outliers"), NA)
  expect_error(which_options("clean_noise", "AENN"), NA)
  expect_error(which_options("clean_noise", "hybrid"), NA)
  expect_error(which_options("clean_noise"), NA)
  expect_error(which_options("impute_missing"), NA)
  expect_error(which_options("feature_selection", "chi_squared"), NA)
  expect_error(which_options("feature_selection", "hybrid"))
  expect_error(which_options("instance_selection"), NA)
  expect_error(which_options("discretize"), NA)
  expect_error(which_options("normalize"), NA)
  expect_error(which_options("oversample"), NA)
  expect_error(which_options("space_transformation"), NA)
})
