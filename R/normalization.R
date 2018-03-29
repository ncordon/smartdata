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

args.clusterSim <- list(
  by = list(check = Curry(expect_choice, choices = c("column", "row"), label = "by"),
            default = "column")
)

args.dprep <- list()

doNormalization.clusterSim <- function(task){
  task$args <- checkListArguments(task$args, args.clusterSim)

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

  callArgs <- list(x = task$dataset, type = type,
                   normalization = task$args[["by"]])
  do.call(clusterSim::data.Normalization, callArgs)
}

doNormalization.dprep <- function(task){
  task$args <- checkListArguments(task$args, args.dprep)

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
#'
#' super_iris <- normalization(iris, method = "min-max", class_attr = "Species", by = "column")
#' # Use default parameter by = "row"
#' super_iris <- normalization(iris, method = "min-max", class_attr = "Species")
#' super_iris <- normalization(iris, method = "min-max", class_attr = "Species", by = "row")
#' super_iris <- normalization(iris, method = "z-score", class_attr = "Species", by = "row")
#' super_iris <- normalization(iris, method = "sd-quotient", class_attr = "Species", by = "row")
#' super_iris <- normalization(iris, method = "decimal-scaling", class_attr = "Species")
#'
normalization <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  orig_dataset <- dataset
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)

  method <- matchArg(method, normalizationMethods)
  colnames <- names(dataset)
  dataset <- dataset[, -which(colnames %in% classAttr)]
  nonNumericAttrs <- classAttr
  coltypes <- colTypes(dataset)
  nonNumeric <- which(! coltypes %in% c("numeric", "integer"))
  numericNonClass <- names(dataset)[nonNumeric]

  if(length(nonNumeric) > 0){
    dataset <- dataset[, -nonNumeric]
    nonNumericAttrs <- c(numericNonClass, nonNumericAttrs)
  }

  # Perform normalization
  task <- preprocessingTask(dataset, "normalization", method, NULL, ...)
  dataset <- preprocess(task)

  # Join non numeric attrs and class attribute again
  result <- sapply(names(orig_dataset), function(name){
    if(name %in% nonNumericAttrs){
      orig_dataset[, name]
    } else{
      dataset[, name]
    }
  }, USE.NAMES = TRUE, simplify = FALSE)

  result <- as.data.frame(result)

  result
}
