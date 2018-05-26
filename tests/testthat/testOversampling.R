library(testthat)
# Loads data
data(ecoli1, package = "imbalance")
data(iris0, package = "imbalance")

context("Oversampling testing")

test_that("Correct oversampling", {
  #expect_error(oversample(iris0, "RACOG"), NA)
  expect_error(oversample(iris0, "wRACOG", wrapper = "KNN"), NA)
  expect_error(oversample(iris0, "wRACOG", wrapper = "C5.0"), NA)
  expect_error(oversample(iris0, "PDFOS"), NA)
  expect_error(oversample(ecoli1, "RWO"), NA)
  expect_error(oversample(iris0, "ANSMOTE"), NA)
  expect_error(oversample(iris0, "SMOTE", filtering = TRUE), NA)
  expect_error(oversample(iris0, "MWMOTE", ratio = 0.6), NA)
  #expect_error(oversample(iris0, "DBSMOTE"), NA)
  expect_error(oversample(iris0, "SLMOTE"), NA)
})
