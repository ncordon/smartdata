outliersPackages <- list("multivariate" = "MVN",
                         "univariate" = "outliers")

outliersMethods <- names(outliersPackages)

doOutliers <- function(task){
  UseMethod("doOutliers")
}

doOutliers.outliers <- function(task){
  dataset <- task$dataset
  coltypes <- colTypes(dataset)
  numericIndexes <- which(coltypes %in% c("numeric", "integer"))
  # Strip non numeric columns from dataset
  dataset <- dataset[, numericIndexes]
  possibleArgs <- list(type = argCheck("discrete", values = c("z", "t", "chisq", "iqr", "mad"), required = TRUE),
                       prob = argCheck("real", min = 0, max = 1, required = TRUE, maxIncluded = FALSE),
                       mean = argCheck("boolean", values = c("TRUE", "FALSE"), required = TRUE))
  checkListArguments(task$args, possibleArgs)
  scoresArguments <- task$args[names(task$args) != "mean"]

  # Compute which are the outliers per column
  whichOutliers <- do.call(outliers::scores, append(list(x = dataset), scoresArguments))

  dataset <- task$dataset

  # Replace outliers with mean (if mean = TRUE in the task arguments) or median
  result <- sapply(names(dataset), function(col){
    current <- dataset[, col]

    if(col %in% names(whichOutliers)){
      newVal <- ifelse(task$args[["mean"]], mean(current), median(current))
      current[whichOutliers[, col]] <- newVal
      current
    } else{
      current
    }
  }, USE.NAMES = TRUE, simplify = FALSE)

  result <- as.data.frame(result)

  result
}


doOutliers.MVN <- function(task){

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
