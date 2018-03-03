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

context("Discretization testing")

# We change the class attribute in a dataset
modecoli <- ecoli1
names(modecoli)[ncol(modecoli)] <- "my_class"

test_that("Correct discretization", {
  expect_error(discretize(iris0, method = "chi2", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "chi-merge", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "extended-chi2", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "mod-chi2", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "CAIM", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "CACC", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "ameva", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "mdlp", class_attr = "Class"), NA)
  expect_error(discretize(glass0, method = "equalwidth", class_attr = "Class"), NA)
  expect_error(discretize(glass0, method = "equalwidth", class_attr = "Class", nbins = 3), NA)
  # Wrong argument passed to discretized with equalwidth, correct-> nbins, used -> nbin
  expect_error(discretize(glass0, method = "equalfreq", class_attr = "Class", nbin = 3))
})
