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

# We change the class attribute in a dataset
modecoli <- ecoli1
names(modecoli)[ncol(modecoli)] <- "my_class"

test_that("Correct normalization", {
  expect_error(normalization(iris0, "min_max"), NA)
  expect_error(normalization(iris0, "pos_standardization"), NA)
  expect_error(normalization(iris0, "unitization"), NA)
  expect_error(normalization(iris0, "pos_unitization"), NA)
  expect_error(normalization(iris0, "rnorm"), NA)
  expect_error(normalization(iris0, "rpnorm"), NA)
  expect_error(normalization(iris0, "sd_quotient"), NA)
  expect_error(normalization(iris0, "mad_quotient"), NA)
  expect_error(normalization(iris0, "range_quotient"), NA)
  expect_error(normalization(iris0, "max_quotient"), NA)
  expect_error(normalization(iris0, "mean_quotient"), NA)
  expect_error(normalization(iris0, "median_quotient"), NA)
  expect_error(normalization(iris0, "sum_quotient"), NA)
  expect_error(normalization(iris0, "ssq_quotient"), NA)
  expect_error(normalization(iris0, "norm"), NA)
  expect_error(normalization(iris0, "pnorm"), NA)
  expect_error(normalization(iris0, "znorm"), NA)
  expect_error(normalization(glass0, "z_score"), NA)
  expect_error(normalization(glass0, "z_score", by = "column"), NA)
  #expect_error(normalization(glass0, "z-score", normalization = "row"), NA)
  expect_error(normalization(haberman, "sigmoidal", exclude = "Class"), NA)
  expect_error(normalization(newthyroid1, method = "softmax"), NA)
  expect_error(normalization(iris0, method = "decimal_scaling"), NA)
  expect_error(normalization(modecoli, method = "decimal_scaling"), NA)
  expect_error(normalization(modecoli, method = "decimal_scaling", exclude = "my_class"), NA)
})
