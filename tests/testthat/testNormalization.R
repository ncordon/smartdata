library(testthat)
library(imbalance)
# Loads data
data(ecoli1)
data(glass0)
data(haberman)
data(iris0)
data(newthyroid1)
data(wisconsin)
data(yeast4)

context("Normalization testing")

test_that("Correct normalization", {
  expect_error(normalize(iris0, "min_max"), NA)
  expect_error(normalize(iris, "min_max", by = "row"), NA)
  expect_error(normalize(iris0, "pos_standardization"), NA)
  expect_error(normalize(iris0, "unitization"), NA)
  expect_error(normalize(iris0, "pos_unitization"), NA)
  expect_error(normalize(iris0, "rnorm"), NA)
  expect_error(normalize(iris0, "rpnorm"), NA)
  expect_error(normalize(iris0, "sd_quotient"), NA)
  expect_error(normalize(iris0, "mad_quotient"), NA)
  expect_error(normalize(iris0, "range_quotient"), NA)
  expect_error(normalize(iris0, "max_quotient"), NA)
  expect_error(normalize(iris0, "mean_quotient"), NA)
  expect_error(normalize(iris0, "median_quotient"), NA)
  expect_error(normalize(iris0, "sum_quotient"), NA)
  expect_error(normalize(iris0, "ssq_quotient"), NA)
  expect_error(normalize(iris0, "norm"), NA)
  expect_error(normalize(iris0, "pnorm"), NA)
  expect_error(normalize(iris0, "znorm"), NA)
  expect_error(normalize(glass0, "z_score"), NA)
  expect_error(normalize(glass0, "z_score", by = "column"), NA)
  # expect_error(normalize(glass0, "z-score", by = "row"), NA)
  # expect_error(normalize(haberman, "sigmoidal"), NA)
  # expect_error(normalize(newthyroid1, method = "softmax"), NA)
  # expect_error(normalize(iris0, method = "decimal_scaling"), NA)
  # expect_error(normalize(ecoli1, method = "decimal_scaling"), NA)
  # expect_error(normalize(ecoli1, method = "decimal_scaling"), NA)
})
