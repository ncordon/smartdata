#' amendr: A package to ease data preprocessing tasks
#'
#' Provides a pipe interface that integrates a collection of the most used data
#' preprocessing libraries, providing oversampling, instance and feature
#' selection, normalization, discretization, space transformation and
#' outliers/noise/missing values treatment
#'
#' @section Method to do oversampling
#' @section Method to do instance selection \code{\link{instance_selection}}
#' @section Method to do feature selection
#' @section Method to do normalization \code{\link{normalization}}
#' @section Method to do discretization
#' @section Method to do space transformation
#' @section Method to treat outliers
#' @section Method to treat missing values
#' @section Method to treat noise
#'
#' @docType package
#' @name amendr
NULL


#' @import imbalance
#' @import magrittr
#' @import unbalanced
#' @import discretization
#' @importFrom functional Curry
#' @importFrom checkmate expect_choice
#' @importFrom checkmate matchArg
#' @importFrom checkmate qexpect
NULL

preprocessingTask <- function(dataset, what, method, classAttr = NULL, ...){
  if(is.null(classAttr)){
    classIndex <- NULL
  } else{
    classIndex <- which(names(dataset) %in% classAttr)
  }

  task <- list(dataset = dataset,
               method = method,
               classAttr = classAttr,
               classIndex = classIndex,
               args = list(...))
  class(task) <- what
  task
}

preprocess <- function(task){
  UseMethod("preprocess")
}

preprocess.normalization <- function(task){
  class(task) <- normalizationPackages[[task$method]]

  doNormalization(task)
}

preprocess.instanceSelection <- function(task){
  class(task) <- instSelectionPackages[[task$method]]

  doInstSelection(task)
}

preprocess.discretization <- function(task){
  class(task) <- discretizationPackages[[task$method]]

  doDiscretization(task)
}

preprocess.oversampling <- function(task){
  class(task) <- oversamplingPackages[[task$method]]

  doOversampling(task)
}

preprocess.outliers <- function(task){
  class(task) <- outliersPackages[[task$method]]

  doOutliersClean(task)
}

preprocess.noise <- function(task){
  class(task) <- noisePackages[[task$method]]

  doNoiseClean(task)
}

