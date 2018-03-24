outliersPackages <- list("multivariate" = "MVN",
                         "univariate" = "outliers")

outliersMethods <- names(outliersPackages)

doOutliers <- function(task){
  UseMethod("doOutliers")
}

doOutliers.MVN <- function(task){
  coltypes <- colTypes(task$dataset)
  numericIndexes <- which(coltypes %in% c("numeric", "integer"))
  # Strip non numeric columns from dataset
  # dataset <- dataset[, numericIndexes]
  possibleArgs <- list(type = argCheck("discrete", values = c("z", "t", "chisq", "iqr", "mad"), required = TRUE),
                       prob = argCheck("real", min = 0, max = 1, required = TRUE))
  checkListArguments(task$args, possibleArgs)


}


doOutliers.outliers <- function(task) {

}


#' Outliers cleaning wrapper
#'
#' @param dataset we want to clean outliers on
#' @param method selected method to clean outliers
#' @param ... Further arguments for \code{method}
#'
#' @return The dataset without outliers
#' @export
#'
#' @examples
#'
outliers_clean <- function(dataset, method, ...){
  checkDataset(dataset)
  method <- match.arg(method, outliersMethods)

  # Clean outliers
  task <- preprocessingTask(dataset, "outliers", method, ...)
  dataset <- preprocess(task)

  dataset
}
