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
  expect_error(discretize(iris0, method = "chi2", exclude = "Class"), NA)
  expect_error(discretize(iris0, method = "chi_merge", exclude = "Class"), NA)
  expect_error(discretize(iris0, method = "extended_chi2", exclude = "Class"), NA)
  expect_error(discretize(iris0, method = "mod_chi2", exclude = "Class"), NA)
  expect_error(discretize(iris0, method = "CAIM", exclude = "Class"), NA)
  expect_error(discretize(iris0, method = "CACC", exclude = "Class"), NA)
  expect_error(discretize(iris0, method = "ameva", exclude = "Class"), NA)
  expect_error(discretize(iris0, method = "mdlp", exclude = "Class"), NA)
  expect_error(discretize(glass0, method = "equalwidth", exclude = "Class",
                          num_bins = ceiling(nrow(glass0) / 2)), NA)
  expect_error(discretize(glass0, method = "equalwidth", exclude = "Class",
                          num_bins = ceiling(nrow(glass0) / 3)), NA)
  # Wrong argument passed to discretized with equalwidth, correct -> num_bins, used -> nbin
  expect_error(discretize(glass0, method = "equalfreq", exclude = "Class", nbins = 3))
})
