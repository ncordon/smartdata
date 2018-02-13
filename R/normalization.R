normalizationPackages <- list("z-score" = "clusterSim",
                              "pos-standardization" = "clusterSim",
                              "unitization" = "clusterSim",
                              "pos-unitization" = "clusterSim",
                              "min-max" = "clusterSim",
                              "rnorm" = "clusterSim",
                              "rpnorm" = "clusterSim",
                              "sd-quotient" = "clusterSim",
                              "mad-quotient" = "clusterSim",
                              "range-quotient" = "clusterSim",
                              "max-quotient" = "clusterSim",
                              "mean-quotient" = "clusterSim",
                              "median-quotient" = "clusterSim",
                              "sum-quotient" = "clusterSim",
                              "ssq-quotient" = "clusterSim",
                              "norm" = "clusterSim",
                              "pnorm" = "clusterSim",
                              "znorm" = "clusterSim",
                              "decimal-scaling" = "dprep",
                              "sigmoidal" = "dprep",
                              "softmax" = "dprep")

normalizationMethods <- names(normalizationPackages)

doNormalization <- function(task){
  UseMethod("doNormalization")
}

doNormalization.clusterSim <- function(task){
  possibleArgs <- list(normalization = argCheck("discrete", c("column", "row")))
  checkListArguments(task$args, possibleArgs)

  type <- switch(task$method,
                 "z-score" = "n1",
                 "pos-standardization" = "n2",
                 "unitization" = "n3",
                 "pos-unitization" = "n3a",
                 "min-max" = "n4",
                 "rnorm" = "n5",
                 "rpnorm" = "n5a",
                 "sd-quotient" = "n6",
                 "mad-quotient" = "n6a",
                 "range-quotient" = "n7",
                 "max-quotient" = "n8",
                 "mean-quotient" = "n9",
                 "median-quotient" = "n9a",
                 "sum-quotient" = "n10",
                 "ssq-quotient" = "n11",
                 "norm" = "n12",
                 "pnorm" = "n12a",
                 "znorm" = "n13")

  callArgs <- append(list(x = task$dataset, type = type), task$args)
  do.call(clusterSim::data.Normalization, callArgs)
}


doNormalization.dprep <- function(task){
  possibleArgs <- list()
  checkListArguments(task$args, possibleArgs)

  if(task$method == "decimal-scaling"){
    dprep::decscale(task$dataset)
  } else if(task$method == "sigmoidal"){
    dprep::signorm(task$dataset)
  } else if(task$method == "softmax"){
    dprep::softmaxnorm(task$dataset)
  }
}


#' Normalization wrapper
#'
#' @param dataset we want to perform normalization on
#' @param method selected method of normalization
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param ... Further arguments for \code{method}
#'
#' @return The normalized dataset
#' @export
#'
#' @examples
#' library("amendr")
#' library("imbalance")
#' library("magrittr")
#' data(iris0)
#'
#' super_iris <- iris0 %>% normalization(method = "min-max", class_attr = "Class")
#' super_iris <- iris0 %>% normalization(method = "min-max", normalization = "row")
#'
normalization <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)
  dataset <- toNumeric(dataset, exclude = classAttr)
  checkAllColumnsNumeric(dataset, exclude = classAttr)
  method <- match.arg(method, normalizationMethods)
  classIndex <- which(names(dataset) %in% classAttr)
  # Strip dataset from class attribute
  datasetClass <- dataset[, classIndex]
  dataset <- dataset[, -classIndex]

  # Perform normalization
  task <- preprocessingTask(dataset, "normalization", method, classAttr, ...)
  dataset <- preprocess(task)

  # Join class attribute again
  dataset[, classIndex] <- datasetClass
  names(dataset)[classIndex] <- classAttr

  dataset
}
