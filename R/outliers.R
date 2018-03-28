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
                           fill = argCheck("discrete", values = c("median", "mean"), required = TRUE))
    } else{
      possibleArgs <- list(type = argCheck("discrete", values = c("z", "t", "chisq", "iqr", "mad"), required = TRUE),
                           lim = argCheck("real"),
                           fill = argCheck("discrete", values = c("median", "mean"), required = TRUE))
    }

    checkListArguments(task$args, possibleArgs)
  } else{
    stop("type argument must be present")
  }

  scoresArguments <- task$args[names(task$args) != "fill"]

  # Compute which are the outliers per column
  whichOutliers <- do.call(outliers::scores, append(list(x = dataset), scoresArguments))

  dataset <- task$dataset

  # Replace outliers with mean (if mean = TRUE in the task arguments) or median
  result <- sapply(names(dataset), function(col){
    current <- dataset[, col]

    if(col %in% names(whichOutliers)){
      newVal <- ifelse(task$args[["fill"]] == "mean", mean(current), median(current))
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
  possibleArgs <- list(type = argCheck("discrete", values = c("adj", "quan"), required = TRUE))
  checkListArguments(task$args, possibleArgs)

  # Compute outliers, disabling graphical effects from the functions that computes them
  pdf(file = NULL)
  outliers <- do.call(MVN::mvn, append(list(data = dataset, showOutliers = TRUE),
                                       list(multivariateOutlierMethod = task$args[["type"]])))
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
#' @param method selected method to clean outliers. Possbilities are:
#'  \itemize{
#'   \item{"univariate"}{detects outliers column by column (an outlier will be an
#'    abnormal value inside a column) and fills them with mean or median of the
#'    corresponding column}
#'   \item{"multivariate"}{detects outliers using a multicolumn approach, so that
#'    an outlier will be a whole observation (row). And deletes those
#'    observations}
#'  }
#' @param ... further arguments for the method
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
#' super_ecoli <- outliers_clean(ecoli1, method = "multivariate", type = "adj")
#' super_ecoli <- outliers_clean(ecoli1, method = "multivariate", type = "quan")
#'
#' # Use mean as method to substitute outliers
#' outliers_clean(iris0, method = "univariate", type = "z", prob = 0.9, fill = "mean")
#' # Use median as method to substitute outliers
#' outliers_clean(iris0, method = "univariate", type = "z", prob = 0.9, fill = "median")
#' # Use chi-sq instead of z p-values
#' outliers_clean(iris0, method = "univariate", type = "chisq", prob = 0.9, fill = "median")
#' # Use interquartilic range instead (lim argument is mandatory when using it)
#' outliers_clean(iris0, method = "univariate", type = "iqr", lim = 0.9, fill = "median")
#'
outliers_clean <- function(dataset, method, ...){
  checkDataset(dataset)
  method <- match.arg(method, outliersMethods)

  # Clean outliers
  task <- preprocessingTask(dataset, "outliers", method, ...)
  dataset <- preprocess(task)

  dataset
}
