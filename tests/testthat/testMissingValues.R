library(testthat)
data(nhanes, package = "mice")
data(africa, package = "Amelia")
data(ozone,  package = "missMDA")
data(sleep,  package = "VIM")

context("Missing values testing")

nhanes[, "Foo"] <- "Bar"
nhanes <- nhanes[, 5:1]
# Modify africa dataset to have ordinals and nominals NAs
mod_africa <- africa
mod_africa[1, c("country", "gdp_pc")] <- NA


test_that("Correct imputation of missing values", {
  expect_error(impute_missing(nhanes, "gibbs_sampling"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "pmm"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "midastouch"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "sample"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "cart"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "rf"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "mean"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "norm"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "norm_nob"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "norm_boot"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "norm_predict"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "quadratic", num_iterations = 10), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "ri"), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "logreg"), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "logreg_boot"), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "polr"), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "polyreg"), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "lda"), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "2l_norm"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "2l_lmer"), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "2l_pan"), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "2lonly_mean"), NA)
  #expect_error(impute_missing(nhanes, "gibbs_sampling", imputation = "2lonly_pmm"), NA)
  expect_error(impute_missing(nhanes, "gibbs_sampling"), NA)
  expect_equal(names(impute_missing(nhanes, "gibbs_sampling")), names(nhanes))
  expect_error(impute_missing(africa, "expect_maximization", exclude = "country"), NA)
  expect_error(impute_missing(africa, "expect_maximization", exclude = "country", ts_class = "year"), NA)
  # Correct imputation of ordinals and nominals
  expect_error(impute_missing(mod_africa, "expect_maximization", ts_class = "year", ordinals = "gdp_pc", nominals = "country"), NA)
  expect_error(impute_missing(africa, "knn_imputation", k = 5, imputation = "median"), NA)
  expect_error(impute_missing(africa, "rf_imputation", num_iterations = 4, num_trees = 40), NA)
  # exclude vars are not present in ozone
  expect_error(impute_missing(ozone, "FAMD_imputation", num_dimensions = 5, imputation = "EM", exclude = c("foo", "bar")))
  expect_error(impute_missing(ozone, "FAMD_imputation", num_dimensions = 5, imputation = "EM"), NA)
  expect_error(impute_missing(sleep, "regression_imputation", formula = Dream+NonD~BodyWgt+BrainWgt), NA)
  expect_error(impute_missing(ozone, "ATN", sigma = 2.2, lambda = 0.025, gamma = 2.5, tune = "SURE"), NA)
})
