library(testthat)
data(nhanes, package = "mice")
data(africa, package = "Amelia")

context("Missing values testing")

nhanes[, "Foo"] <- "Bar"
nhanes <- nhanes[, 5:1]

test_that("Correct imputation of missing values", {
  expect_error(impute_missing(nhanes, "gibbs_sampling"), NA)
  expect_equal(names(impute_missing(nhanes, "gibbs_sampling")), names(nhanes))
  expect_error(impute_missing(africa, "expect_maximization", exclude = "country"), NA)
  expect_error(impute_missing(africa, "expect_maximization", exclude = "country", ts_class = "year"), NA)
  # exclude vars are not present in ozone
  expect_error(impute_missing(ozone, "FAMD_imputation", num_dimensions = 5, imputation = "EM", exclude = c("foo", "bar")))
})
