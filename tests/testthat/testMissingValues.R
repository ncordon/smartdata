library(testthat)
data(nhanes, package = "mice")

context("Missing values testing")

nhanes[, "Foo"] <- "Bar"
nhanes <- nhanes[, 5:1]

test_that("Correct imputation of missing values", {
  expect_error(impute_missing(nhanes, "gibbs_sampling"), NA)
  expect_equal(names(impute_missing(nhanes, "gibbs_sampling")), names(nhanes))
})
