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

context("Feature selection testing")

test_that("Correct feature selection", {
  # class attribute missing
  expect_error(feature_selection(iris, "Boruta"))
  expect_error(feature_selection(iris0, "Boruta", class_attr = "Class"), NA)
  expect_error(feature_selection(iris0, "Boruta", class_attr = "Class", num_iterations = 200), NA)
  expect_error(feature_selection(iris, "chi_squared"))
  expect_error(feature_selection(iris, "chi_squared", class_attr = "Species", exclude = c("Petal.Width", "Sepal.Width")), NA)
  # exclude argument contains things not present in the dataset
  expect_error(feature_selection(ecoli1, "information_gain", class_attr = "Class", exclude = c("Petal.Width", "Sepal.Width")))
  expect_error(feature_selection(ecoli1, "information_gain", class_attr = "Class", exclude = c("Alm1", "Alm2")), NA)
  expect_error(feature_selection(ecoli1, "sym_uncertainty", class_attr = "Class", exclude = c("Alm1", "Alm2")), NA)
  expect_error(feature_selection(ecoli1, "oneR", class_attr = "Class", exclude = c("Alm1", "Alm2"), num_attrs = 3), NA)
  expect_error(feature_selection(ecoli1, "RF_importance", class_attr = "Class", num_attrs = 3), NA)
  expect_error(feature_selection(newthyroid1, "cfs", class_attr = "Class"), NA)
})
