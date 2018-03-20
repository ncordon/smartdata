oversamplingPackages <- list("RACOG" = "imbalance",
                             "wRACOG" = "imbalance",
                             "PDFOS" = "imbalance",
                             "RWO" = "imbalance",
                             "ADASYN" = "imbalance",
                             "ANSMOTE" = "imbalance",
                             "SMOTE" = "imbalance",
                             "MWMOTE" = "imbalance",
                             "BLSMOTE" = "imbalance",
                             "DBSMOTE" = "imbalance",
                             "SLMOTE" = "imbalance",
                             "RSLSMOTE" = "imbalance")

oversamplingMethods <- names(oversamplingPackages)

doOversampling <- function(task){
  UseMethod("doOversampling")
}

doOversampling.imbalance <- function(task){
  possibleArgs <- list(ratio = argCheck("real", min = 0, max = 1),
                       filtering = argCheck("boolean"),
                       wrapper = argCheck("discrete", values = c("C5.0", "KNN")) )
  # Check correction of arguments passed to the method
  checkListArguments(task$args, possibleArgs)

  # Prepare list of arguments for the methods
  callArgs <- append(list(dataset = task$dataset,
                          method = task$method,
                          classAttr = task$classAttr),
                     task$args)
  result <- do.call(imbalance::oversample, callArgs)
  result
}

#' Oversampling wrapper
#'
#' @param dataset we want to perform oversampling on
#' @param method selected method of oversampling
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param ... Further arguments for \code{method}
#'
#' @return An oversampled dataset
#' @export
#'
#' @examples
#' library("amendr")
#' library("magrittr")
#' data(iris0, package = "imbalance")
#'
#' super_iris <- iris0 %>% oversample(method = "MWMOTE", class_attr = "Class",
#'                                    ratio = 0.8, filtering = TRUE)
#' super_iris <- iris0 %>% oversample(method = "SMOTE", class_attr = "Class", ratio = 0.6)
#' super_iris <- iris0 %>% oversample(method = "PDFOS", class_attr = "Class", ratio = 0.6)
#'
oversample <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)
  #dataset <- toNumeric(dataset, exclude = classAttr)
  #checkAllColumnsNumeric(dataset, exclude = classAttr)
  method <- match.arg(method, oversamplingMethods)

  # Perform discretization
  task <- preprocessingTask(dataset, "oversampling", method, classAttr, ...)
  dataset <- preprocess(task)

  dataset
}
