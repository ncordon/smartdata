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
  if("type" %in% names(task$args)){
    if(task$args[["type"]] != "iqr"){
      possibleArgs <- list(type = argCheck("discrete", values = c("z", "t", "chisq", "iqr", "mad"), required = TRUE),
                           prob = argCheck("real", min = 0, max = 1, required = TRUE, maxIncluded = FALSE),
                           mean = argCheck("boolean", values = c("TRUE", "FALSE"), required = TRUE))
    } else{
      possibleArgs <- list(type = argCheck("discrete", values = c("z", "t", "chisq", "iqr", "mad"), required = TRUE),
                           lim = argCheck("real"),
                           mean = argCheck("boolean", values = c("TRUE", "FALSE"), required = TRUE))
    }

    checkListArguments(task$args, possibleArgs)
  } else{
    stop("type argument must be present")
  }

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
    }

    current
  }, USE.NAMES = TRUE, simplify = FALSE)

  result <- as.data.frame(result)

  result
}


doOutliers.MVN <- function(task){
  dataset <- task$dataset
  coltypes <- colTypes(dataset)
  numericIndexes <- which(coltypes %in% c("numeric", "integer"))
  # Strip non numeric columns from dataset
  dataset <- dataset[, numericIndexes]

  # Check list of arguments
  possibleArgs <- list(multivariateOutlierMethod = argCheck("discrete", values = c("adj", "quan"), required = TRUE))
  checkListArguments(task$args, possibleArgs)

  # Compute outliers, disabling graphical effects from the functions that computes them
  pdf(file = NULL)
  outliers <- do.call(MVN::mvn, append(list(data = dataset, showOutliers = TRUE), task$args))
  dummy <- capture.output(dev.off())

  whichOutliers <- as.integer(as.character(outliers$multivariateOutliers$Observation))

  # Remove outliers from original dataset
  result <- task$dataset[-whichOutliers, ]

  # Reconfigure id of observations
  rownames(result) <- c()

  result
}


#' Outliers cleaning wrapper
#'
#' @param dataset we want to clean outliers on
#' @param method selected method to clean outliers
#' @param ... Further arguments for \code{method}
#'
#' @return The dataset without outliers
#' @importFrom stats median
#' @importFrom grDevices dev.off pdf
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' library("amendr")
#' data(ecoli1, package = "imbalance")
#' data(iris0, package = "imbalance")
#'
#' super_ecoli <- outliers_clean(ecoli1, method = "multivariate", multivariateOutlierMethod = "adj")
#' super_ecoli <- outliers_clean(ecoli1, method = "multivariate", multivariateOutlierMethod = "quan")
#'
#' # Use mean as method to substitute outliers
#' outliers_clean(iris0, method = "univariate", type = "z", prob = 0.9, mean = TRUE)
#' # Use median as method to substitute outliers
#' outliers_clean(iris0, method = "univariate", type = "z", prob = 0.9, mean = FALSE)
#' # Use chi-sq instead of z p-values
#' outliers_clean(iris0, method = "univariate", type = "chisq", prob = 0.9, mean = FALSE)
#' # Use interquartilic range instead (lim argument is mandatory when using it)
#' outliers_clean(iris0, method = "univariate", type = "iqr", lim = 0.9, mean = FALSE)
#'
outliers_clean <- function(dataset, method, ...){
  checkDataset(dataset)
  method <- match.arg(method, outliersMethods)

  # Clean outliers
  task <- preprocessingTask(dataset, "outliers", method, ...)
  dataset <- preprocess(task)

  dataset
}
