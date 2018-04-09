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

args.imbalance <- list(
  ratio = list(
    check = Curry(qexpect, rules = "N1[0,1]"),
    default = 0.8
  ),
  filtering = list(
    check = Curry(qexpect, rules = "B1"),
    default = FALSE
  ),
  wrapper = list(
    check = Curry(expect_choice, choices = c("C5.0", "KNN")),
    default = "KNN"
  )
)

doOversampling.imbalance <- function(task){
  # Check correction of arguments passed to the method
  task$args <- checkListArguments(task$args, args.imbalance)

  # Prepare list of arguments for the methods
  callArgs <- c(list(dataset = task$dataset,
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
#' data(iris0, package = "imbalance")
#'
#' super_iris <- oversample(iris0, method = "MWMOTE", class_attr = "Class",
#'                          ratio = 0.8, filtering = TRUE)
#' super_iris <- oversample(iris0, method = "SMOTE", class_attr = "Class", ratio = 0.6)
#' super_iris <- oversample(iris0, method = "PDFOS", class_attr = "Class", ratio = 0.6)
#' super_iris <- oversample(iris0, method = "RWO", class_attr = "Class", ratio = 0.8)
#' super_iris <- oversample(iris0, method = "SLMOTE", class_attr = "Class")
#'
oversample <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)

  method <- matchArg(method, oversamplingMethods)

  # Perform discretization
  task <- preprocessingTask(dataset, "oversampling", method, classAttr, ...)
  dataset <- preprocess(task)

  dataset
}
