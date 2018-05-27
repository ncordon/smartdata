library(testthat)

context("Outliers testing")

test_that("Correct outliers treatment", {
  expect_error(clean_outliers(iris, method = "multivariate", type = "adj"), NA)
  expect_error(clean_outliers(iris, method = "multivariate", type = "quan"), NA)
  expect_error(clean_outliers(iris, method = "univariate", type = "z", prob = 0.9, fill = "mean"), NA)
  expect_error(clean_outliers(iris, method = "univariate", type = "t", prob = 0.9, fill = "median"), NA)
  expect_error(clean_outliers(iris, method = "univariate", type = "chisq", fill = "median"), NA)
  expect_error(clean_outliers(iris, method = "univariate", type = "iqr", fill = "median"), NA)
  expect_error(clean_outliers(iris, method = "univariate", type = "mad", fill = "median"), NA)
})
