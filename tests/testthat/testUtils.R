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

context("Utils testing")

test_that("Integers are correctly checked", {
  expect_equal(argCheck("integer", min = 1)$check(1), TRUE)
  expect_equal(argCheck("integer", min = 1)$check(-3), FALSE)
  expect_equal(argCheck("integer", min = 1, minIncluded = FALSE)$check(1), FALSE)
  # Check that required parameter works perfectly
  expect_equal(argCheck("integer", min = 1, minIncluded = FALSE, required = FALSE)$check(NULL), TRUE)
  expect_equal(argCheck("integer", min = -1, max = 1, minIncluded = FALSE, maxIncluded = 1)$check(1), TRUE)
  expect_equal(argCheck("integer", min = -1, max = 1, minIncluded = FALSE, maxIncluded = 1)$check(-1), FALSE)
  expect_equal(argCheck("integer", min = -1, max = 1, minIncluded = TRUE, maxIncluded = 1)$check(-1), TRUE)
  expect_equal(argCheck("integer")$check("test"), FALSE)
  expect_equal(argCheck("integer")$check(TRUE), FALSE)
})


test_that("Reals are correctly checked", {
  expect_equal(argCheck("real", min = 1)$check(1.0), TRUE)
  expect_equal(argCheck("real", min = 1)$check(-3.0), FALSE)
  expect_equal(argCheck("real", min = 1, minIncluded = FALSE)$check(1.0), FALSE)
  # Check that required parameter works perfectly
  expect_equal(argCheck("real", min = 1, minIncluded = FALSE, required = FALSE)$check(NULL), TRUE)
  expect_equal(argCheck("real", min = -1, max = 1, minIncluded = FALSE, maxIncluded = 1)$check(1.0), TRUE)
  expect_equal(argCheck("real", min = -1, max = 1, minIncluded = FALSE, maxIncluded = 1)$check(-1.0), FALSE)
  expect_equal(argCheck("real", min = -1, max = 1, minIncluded = TRUE, maxIncluded = 1)$check(-1.0), TRUE)
  expect_equal(argCheck("real")$check(-Inf), TRUE)
  expect_equal(argCheck("real")$check("test"), FALSE)
  expect_equal(argCheck("real")$check(TRUE), FALSE)
})


test_that("Booleans are correctly checked", {
  expect_equal(argCheck("boolean")$check(1.0), FALSE)
  expect_equal(argCheck("boolean")$check("test"), FALSE)
  expect_equal(argCheck("boolean")$check(TRUE), TRUE)
  expect_equal(argCheck("boolean")$check(FALSE), TRUE)
  expect_equal(argCheck("boolean", required = FALSE)$check(NULL), TRUE)
})


test_that("Discrete values are correctly checked", {
  expect_equal(argCheck("discrete", values = c("a", "b", "c", "d"))$check("e"), FALSE)
  expect_equal(argCheck("discrete", values = c("a", "b", "c", "d"))$check(1), FALSE)
  expect_equal(argCheck("discrete", values = c("a", "b", "c", "d"))$check(TRUE), FALSE)
  expect_equal(argCheck("discrete", values = c("a", "b", "c", "d"))$check("a"), TRUE)
  expect_equal(argCheck("discrete", values = c("a", "b", "c", "d"), required = FALSE)$check(NULL), TRUE)
  expect_equal(argCheck("discrete", values = c("a", "b", "c", "d"), required = FALSE)$check(FALSE), FALSE)
})
