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

# Change class column in iris to be at second place
mod_iris <- iris[, c(1,5,2,3,4)]

test_that("Correct discretization", {
  expect_error(discretize(iris0, method = "chi2", class_attr = "Class"), NA)
  expect_error(discretize(mod_iris, method = "chi2", class_attr = "Species", delta = 0.3), NA)
  expect_error(discretize(iris,  method = "chi_merge", class_attr = "Species",
                          alpha = 0.6), NA)
  expect_error(discretize(iris0, method = "chi_merge", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "extended_chi2", class_attr = "Class"), NA)
  expect_error(discretize(ecoli1, method = "extended_chi2", class_attr = "Class", alpha = 0.6), NA)
  expect_error(discretize(mod_iris, method = "mod_chi2", class_attr = "Species"), NA)
  expect_equal(names(discretize(mod_iris, method = "mod_chi2", class_attr = "Species")), names(mod_iris))
  expect_error(discretize(iris0, method = "CAIM"))
  expect_error(discretize(iris0, method = "CAIM", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "CACC", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "ameva", class_attr = "Class"), NA)
  expect_error(discretize(iris0, method = "mdlp", class_attr = "Class"), NA)
  expect_error(discretize(glass0, method = "equalwidth",
                          num_bins = ceiling(nrow(glass0) / 2)), NA)
  expect_error(discretize(glass0, method = "equalwidth",
                          num_bins = ceiling(nrow(glass0) / 3)), NA)
  # Wrong argument passed to discretized with equalwidth, correct -> num_bins, used -> nbin
  expect_error(discretize(glass0, method = "equalfreq", nbins = 3))
})
