discretizationPackages <- list("chi2" = "discretization",
                              "chi-merge" = "discretization",
                              "extended-chi2" = "discretization",
                              "mod-chi2" = "discretization",
                              "CAIM" = "discretization",
                              "CACC" = "discretization",
                              "ameva" = "discretization",
                              "mdlp" = "discretization",
                              "equalfreq" = "infotheo",
                              "equalwidth" = "infotheo",
                              "globalequalwidth" = "infotheo")

discretizationMethods <- names(discretizationPackages)

doDiscretization <- function(task){
  UseMethod("doDiscretization")
}

doDiscretization.discretization <- function(task){
  type = NA
  possibleArgs <- list()

  if(task$method == "chi-merge"){
    possibleArgs <- list(alpha = argCheck("real", min = 0, max = 1))
    method <- "chiM"
  } else if(task$method == "chi2"){
    possibleArgs <- list(alp = argCheck("real", min = 0, max = 1),
                         del = argCheck("real", min = 0, max = 1))
    method <- "chi2"
  } else if(task$method == "extended-chi2"){
    possibleArgs <- list(alp = argCheck("real", min = 0, max = 1))
    method <- "extendChi2"
  } else if(task$method == "mod-chi2"){
    possibleArgs <- list(alp = argCheck("real", min = 0, max = 1))
    method <- "modChi2"
  } else if(task$method == "CAIM"){
    type <- 1
    method <- "disc.Topdown"
  } else if(task$method == "CACC"){
    type <- 2
    method <- "disc.Topdown"
  } else if(task$method == "ameva"){
    type <- 3
    method <- "disc.Topdown"
  } else if(task$method == "mdlp"){
    method <- "mdlp"
  }

  checkListArguments(task$args, possibleArgs)
  method <- eval(parse(text = paste("discretization::", method, sep = "")))

  callArgs <- append(list(task$dataset), task$args)
  result <- do.call(method, callArgs)
  result$Disc.data
}


doDiscretization.infotheo <- function(task) {
  type = NA
  possibleArgs <- list(nbins = argCheck("integer", min = 2, max = nrow(task$dataset)))

  checkListArguments(task$args, possibleArgs)
  callArgs <- append(list(X = task$dataset, disc = task$method), task$args)
  result <- do.call(infotheo::discretize, callArgs)
}


#' Discretization wrapper
#'
#' @param dataset we want to perform discretization on
#' @param method selected method of discretization
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param ... Further arguments for \code{method}
#'
#' @return The discretized dataset
#' @export
#'
#' @examples
#' library("amendr")
#' library("magrittr")
#' data(iris0, package = "imbalance")
#'
#' super_iris <- iris0 %>% discretize(method = "chi2", class_attr = "Class")
#' super_iris <- iris0 %>% discretize(method = "ameva", class_attr = "Class")
#' super_iris <- iris0 %>% discretize(method = "equalwidth", class_attr = "Class")
#'
discretize <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)
  #dataset <- toNumeric(dataset, exclude = classAttr)
  #checkAllColumnsNumeric(dataset, exclude = classAttr)
  method <- match.arg(method, discretizationMethods)
  classIndex <- which(names(dataset) %in% classAttr)
  # Strip dataset from class attribute
  datasetClass <- dataset[, classIndex]
  dataset <- dataset[, -classIndex]

  # Perform discretization
  task <- preprocessingTask(dataset, "discretization", method, classAttr, ...)
  dataset <- preprocess(task)

  # Join class attribute again
  dataset[, classIndex] <- datasetClass
  names(dataset)[classIndex] <- classAttr

  dataset
}
