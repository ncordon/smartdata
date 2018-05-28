library(testthat)
data(ecoli1, package = "imbalance")
data(AntibioticSmall, package = "adaptiveGPCA")
antibiotics <- data.frame(AntibioticSmall$X)

context("Space transformation testing")

test_that("Correct space transformation", {
  expect_error(space_transformation(ecoli1, "lle_knn", regularization = 3, k = 3, num_features = 2,
                                    exclude = c("Mcg", "Alm1")), NA)
  expect_error(space_transformation(antibiotics, "adaptative_gpca",
                                    similarity = AntibioticSmall$Q, num_features = 2), NA)
})
