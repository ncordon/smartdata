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
  expect_error(normalization(iris0, "min-max"), NA)
  expect_error(normalization(iris0, "pos-standardization"), NA)
  expect_error(normalization(iris0, "unitization"), NA)
  expect_error(normalization(iris0, "pos-unitization"), NA)
  expect_error(normalization(iris0, "rnorm"), NA)
  expect_error(normalization(iris0, "rpnorm"), NA)
  expect_error(normalization(iris0, "sd-quotient"), NA)
  expect_error(normalization(iris0, "mad-quotient"), NA)
  expect_error(normalization(iris0, "range-quotient"), NA)
  expect_error(normalization(iris0, "max-quotient"), NA)
  expect_error(normalization(iris0, "mean-quotient"), NA)
  expect_error(normalization(iris0, "median-quotient"), NA)
  expect_error(normalization(iris0, "sum-quotient"), NA)
  expect_error(normalization(iris0, "ssq-quotient"), NA)
  expect_error(normalization(iris0, "norm"), NA)
  expect_error(normalization(iris0, "pnorm"), NA)
  expect_error(normalization(iris0, "znorm"), NA)
  expect_error(normalization(glass0, "z-score"), NA)
  expect_error(normalization(glass0, "z-score", by = "column"), NA)
  #expect_error(normalization(glass0, "z-score", normalization = "row"), NA)
  expect_error(normalization(haberman, "sigmoidal", class_attr = "Class"), NA)
  expect_error(normalization(newthyroid1, method = "softmax"), NA)
  expect_error(normalization(iris0, method = "decimal-scaling"), NA)
  expect_error(normalization(modecoli, method = "decimal-scaling"))
  expect_error(normalization(modecoli, method = "decimal-scaling", class_attr = "my_class"), NA)
})
