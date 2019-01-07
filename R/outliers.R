outliersPackages <- list(
  "multivariate" = list(
    pkg = "MVN",
    map = "mvn"
  ),
  "univariate"  = list(
    pkg = "outliers",
    map = "scores"
  )
)

outliersMethods <- names(outliersPackages)

doOutliersClean <- function(task){
  UseMethod("doOutliersClean")
}

args.univariate <- list(
  type      = list(
    check   = Curry(expect_choice, choices = c("z", "t", "chisq", "iqr", "mad"),
                  label = "type"),
    info    = c("Method to compute outliers: ",
                "* 'z' calculates normal scores",
                "* 't' calculates t-Student scores",
                "* 'chisq' gives chi-squared scores",
                paste(
                  "* For the 'iqr' type, all values lower than first and greater than third quartile are considered",
                  "and difference between them and nearest quartile divided by IQR are calculated"
                ),
                "* 'mad' gives differences between each value and median, divided by median absolute deviation")
  ),
  prob      = list(
    check   = Curry(qexpect, rules = "N1(0,1)", label = "prob"),
    info    = "Noise are values exceeding this specified probability",
    default = 0.75
  ),
  fill      = list(
    check   = Curry(expect_choice, choices = c("median", "mean"), label = "fill"),
    info    = paste("Possible values are: 'median' or 'mean', indicating which value is going to be used to fill",
                   "the outliers. 'median' and 'mean' are calculated with respect to each column")
  ),
  lim       = list(
    check   = Curry(qexpect, rules = "N1", label = "lim"),
    info    = "This value can be set for 'iqr' type of scores, to form logical vector, which values has this limit exceeded",
    default = 1
  )
)

args.multivariate <- list(
  type      = list(
    check   = Curry(expect_choice, choices = c("adj", "quan"), label = "type"),
    info    = paste("Multivariate outlier detection method: 'quan' quantile method based on Mahalanobis distance",
                    "and 'adj' adjusted quantile method based on Mahalanobis distance"),
    default = "adj",
    map     = "multivariateOutlierMethod"
  )
)

doOutliersClean.outliers <- function(task){
  dataset <- task$dataset
  coltypes <- colTypes(dataset)
  numericIndexes <- which(coltypes %in% c("numeric", "integer"))
  # Strip non numeric columns from dataset
  dataset <- dataset[, numericIndexes]

  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  method   <- mapMethod(outliersPackages, task$method)

  if("type" %in% names(task$args)){
    if(task$args[["type"]] != "iqr"){
      callArgs <- callArgs[c("type", "prob", "fill")]
    } else{
      callArgs <- callArgs[c("type", "lim", "fill")]
    }
  } else{
    stop("type argument must be present")
  }

  args <- callArgs[names(callArgs) != "fill"]

  # Compute which are the outliers per column
  whichOutliers <- do.call(method, c(list(x = dataset), args))

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

  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  dataset  <- dataset[, numericIndexes]
  method   <- mapMethod(outliersPackages, task$method)

  # Compute outliers, disabling graphical effects from the functions that computes them
  pdf(file = NULL)
  outliers <- do.call(method, c(list(data = dataset, showOutliers = TRUE), callArgs))

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
#' @param dataset we want to clean outliers of
#' @param method selected method to clean outliers. Possibilities are: \itemize{
#'   \item{"univariate"}{ detects outliers column by column (an outlier will be
#'   an abnormal value inside a column) and fills them with mean or median of
#'   the corresponding column} \item{"multivariate"}{ detects outliers using a
#'   multicolumn approach, so that an outlier will be a whole observation (row).
#'   And deletes those observations} }
#' @param ... further arguments for the method
#'
#' @return The treated dataset (either with outliers replaced or erased)
#' @importFrom stats median
#' @importFrom grDevices dev.off pdf
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' library("smartdata")
#'
#' super_iris <- clean_outliers(iris, method = "multivariate", type = "adj")
#' super_iris <- clean_outliers(iris, method = "multivariate", type = "quan")
#'
#' # Use mean as method to substitute outliers
#' super_iris <- clean_outliers(iris, method = "univariate", type = "z", prob = 0.9, fill = "mean")
#' # Use median as method to substitute outliers
#' super_iris <- clean_outliers(iris, method = "univariate", type = "z", prob = 0.9, fill = "median")
#' # Use chi-sq instead of z p-values
#' super_iris <- clean_outliers(iris, method = "univariate", type = "chisq",
#'                              prob = 0.9, fill = "median")
#' # Use interquartilic range instead (lim argument is mandatory when using it)
#' super_iris <- clean_outliers(iris, method = "univariate", type = "iqr", lim = 0.9, fill = "median")
#'
clean_outliers <- function(dataset, method, ...){
  checkDataset(dataset)
  method <- matchArg(method, outliersMethods)

  # Clean outliers
  task <- preprocessingTask(dataset, "outliers", method, ...)
  dataset <- preprocess(task)

  dataset
}
