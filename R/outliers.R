outliersPackages <- list("multivariate" = "MVN",
                         "univariate" = "outliers")

outliersMethods <- names(outliersPackages)

doOutliersClean <- function(task){
  UseMethod("doOutliersClean")
}

args.outliers <- list(
  type = list(
    check = Curry(expect_choice, choices = c("z", "t", "chisq", "iqr", "mad"),
                  label = "type")
  ),
  prob = list(
    check = Curry(qexpect, rules = "N1(0,1)", label = "prob")
  ),
  fill = list(
    check = Curry(expect_choice, choices = c("median", "mean"), label = "fill")
  ),
  lim  = list(
    check = Curry(qexpect, rules = "N1", label = "lim")
  )
)

args.MVN <- list(
  type = list(
    check = Curry(expect_choice, choices = c("adj", "quan"), label = "type")
  )
)

doOutliersClean.outliers <- function(task){
  dataset <- task$dataset
  coltypes <- colTypes(dataset)
  numericIndexes <- which(coltypes %in% c("numeric", "integer"))
  # Strip non numeric columns from dataset
  dataset <- dataset[, numericIndexes]

  if("type" %in% names(task$args)){
    if(task$args[["type"]] != "iqr"){
      possibleArgs <- args.outliers[c("type", "prob", "fill")]
    } else{
      possibleArgs <- args.outliers[c("type", "lim", "fill")]
    }

    task$args <- checkListArguments(task$args, possibleArgs)
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


doOutliersClean.MVN <- function(task){
  dataset <- task$dataset
  coltypes <- colTypes(dataset)
  numericIndexes <- which(coltypes %in% c("numeric", "integer"))
  # Strip non numeric columns from dataset
  dataset <- dataset[, numericIndexes]

  # Check list of arguments
  task$args <- checkListArguments(task$args, args.MVN)

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
#' @return The treated dataset (either with outliers replaced or erased)
#' @importFrom stats median
#' @importFrom grDevices dev.off pdf
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' library("amendr")
#'
#' clean_outliers(iris, method = "multivariate", type = "adj")
#' clean_outliers(iris, method = "multivariate", type = "quan")
#'
#' # Use mean as method to substitute outliers
#' clean_outliers(iris, method = "univariate", type = "z", prob = 0.9, fill = "mean")
#' # Use median as method to substitute outliers
#' clean_outliers(iris, method = "univariate", type = "z", prob = 0.9, fill = "median")
#' # Use chi-sq instead of z p-values
#' clean_outliers(iris, method = "univariate", type = "chisq", prob = 0.9, fill = "median")
#' # Use interquartilic range instead (lim argument is mandatory when using it)
#' clean_outliers(iris, method = "univariate", type = "iqr", lim = 0.9, fill = "median")
#'
clean_outliers <- function(dataset, method, ...){
  checkDataset(dataset)
  method <- matchArg(method, outliersMethods)

  # Clean outliers
  task <- preprocessingTask(dataset, "outliers", method, ...)
  dataset <- preprocess(task)

  dataset
}
