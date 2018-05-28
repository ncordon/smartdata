library(testthat)
data(iris0, package = "imbalance")

context("Noise cleaning testing")

test_that("Correct noise treatment", {
  expect_error(clean_noise(iris, "AENN", class_attr = "Species", k = 3), NA)
  expect_error(clean_noise(iris, "ENN", class_attr = "Species", k = 3), NA)
  expect_error(clean_noise(iris, "BBNR", class_attr = "Species", k = 3), NA)
  #expect_error(clean_noise(iris, "DROP1", class_attr = "Species", k = 3), NA)
  #expect_error(clean_noise(iris, "DROP2", class_attr = "Species", k = 3), NA)
  #expect_error(clean_noise(iris, "DROP3", class_attr = "Species", k = 3), NA)
  expect_error(clean_noise(iris, "EF", class_attr = "Species", consensus = TRUE, num_folds = 5), NA)
  expect_error(clean_noise(iris, "C45robust", class_attr = "Species", num_folds = 5), NA)
  expect_error(clean_noise(iris, "C45voting", class_attr = "Species", num_folds = 5), NA)
  expect_error(clean_noise(iris, "C45iteratedVoting", class_attr = "Species", num_folds = 5), NA)
  expect_error(clean_noise(iris, "CVCF", class_attr = "Species", num_folds = 5), NA)
  expect_error(clean_noise(iris, "EF", class_attr = "Species", num_folds = 5), NA)
  #expect_error(clean_noise(iris, "ENG", class_attr = "Species", graph = "GG"), NA)
  #expect_error(clean_noise(iris, "ENG", class_attr = "Species", graph = "RNG"), NA)
  expect_error(clean_noise(iris, "HARF", class_attr = "Species", num_folds = 3), NA)
  expect_error(clean_noise(iris, "edgeBoost", class_attr = "Species",
                           threshold = 0.01, percent = 0.1, num_boosting = 7), NA)
  expect_error(clean_noise(iris, "ORBoost", class_attr = "Species",
                           num_boosting = 20, threshold = 11, num_adaboost = 20), NA)
  expect_error(clean_noise(iris, "hybrid", class_attr = "Species", action = "repair"), NA)
  expect_error(clean_noise(iris, "hybrid", class_attr = "Species", action = "hybrid"), NA)
  #expect_error(clean_noise(iris, "edgeWeight", class_attr = "Species",
  #                         threshold = 0.01, action = "remove"), NA)
  #expect_error(clean_noise(iris, "edgeWeight", class_attr = "Species",
  #                         threshold = 0.01, action = "hybrid"), NA)
  expect_error(clean_noise(iris, "GE", class_attr = "Species", k = 3, relabel_th = 3), NA)
  expect_error(clean_noise(iris, "INFFC", class_attr = "Species", consensus = TRUE,
                           prob_noisy = 0.2, num_iterations = 2, k = 3, threshold = -0.4), NA)
  expect_error(clean_noise(iris, "IPF", class_attr = "Species", consensus = FALSE,
                           num_folds = 3, prob_noisy = 0.2, prob_good = 0.5, num_iterations = 3), NA)
  #expect_error(clean_noise(iris, "Mode", class_attr = "Species", type = "iterative",
  #                         action = "remove", epsilon = 0.05, num_iterations = 1, alpha = 1, beta = 1), NA)
  #expect_error(clean_noise(iris, "Mode", class_attr = "Species", type = "classical",
  #                         action = "remove", epsilon = 0.05, num_iterations = 1, alpha = 1, beta = 1), NA)
  #expect_error(clean_noise(iris, "Mode", class_attr = "Species", type = "weighted",
  #                         action = "remove", epsilon = 0.05, num_iterations = 1, alpha = 1, beta = 1), NA)
  expect_error(clean_noise(iris, "PF", class_attr = "Species", prob_noisy = 0.01,
                           num_iterations = 5, prob_good = 0.5, theta = 0.8), NA)
  expect_error(clean_noise(iris, "PRISM", class_attr = "Species"), NA)
  expect_error(clean_noise(iris, "RNN", class_attr = "Species"), NA)
  expect_error(clean_noise(iris0, "TomekLinks"), NA)
  expect_error(clean_noise(iris0, method = "dynamic", num_folds = 5, consensus = FALSE), NA)
  #expect_error(clean_noise(iris0, method = "saturation", noise_threshold = 0.25), NA)
  #expect_error(clean_noise(iris0, method = "consensusSF", noise_threshold = 0.25, consensus_level = 4), NA)
  #expect_error(clean_noise(iris0, method = "classificationSF"), NA)
})
