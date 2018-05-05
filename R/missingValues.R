missingValuesPackages <- list(
  "gibbs_sampling"  = list(
    pkg = "mice",
    map = "mice"
  ),
  "expect_maximization" = list(
    pkg = "Amelia",
    map = "amelia"
  ),
  "central_imputation"  = list(
    pkg = "DMwR",
    map = "centralImputation"
  ),
  "knn_imputation"      = list(
    pkg = "DMwR",
    map = "knnImputation"
  )
)

missingValuesMethods <- names(missingValuesPackages)

doMissingValues <- function(task){
  UseMethod("doMissingValues")
}

args.gibbs_sampling <- list(
  imputation   = list(
    # Needs to be adjusted the imputation method of every single column
    check      = Curry(expect_choice, choices =
                     c(
                        "pmm",
                        "midastouch",
                        "sample",
                        "cart",
                        "rf",
                        "mean",
                        "norm",
                        "norm_nob",
                        "norm_boot",
                        "norm_predict",
                        "quadratic",
                        "ri",
                        "logreg",
                        "logreg_boot",
                        "polr",
                        "polyreg",
                        "lda",
                        "2l_norm",
                        "2l_lmer",
                        "2l_pan",
                        "2lonly_mean",
                        "2lonly_pmm"
                      ), label = "imputation"),
    info       = c( "Imputation method (one of the above or a vector containing as much as number of columns in dataset):",
                         "pmm: Predictive mean matching",
                         "midastouch: Weighted predictive mean matching",
                         "sample: Random sample from observed values",
                         "cart: Classification and regression trees",
                         "rf: Random forest imputations",
                         "mean: Unconditional mean imputation",
                         "norm: Bayesian linear regression",
                         "norm_nob: Linear regression ignoring model error",
                         "norm_boot: Linear regression using bootstrap",
                         "norm_predict: Linear regression, predicted values",
                         "quadratic: Imputation of quadratic terms",
                         "ri: Random indicator for nonignorable data",
                         "logreg: Logistic regression",
                         "logreg_boot: Logistic regression with bootstrap",
                         "polr: Proportional odds model",
                         "polyreg: Polytomous logistic regression",
                         "lda: Linear discriminant analysis",
                         "2l_norm: Level-1 normal heteroskedastic",
                         "2l_lmer: Level-1 normal homoscedastic, lmer",
                         "2l_pan: Level-1 normal homoscedastic, pan",
                         "2lonly_mean: Level-2 class mean",
                         "2lonly_norm: Level-2 class normal",
                         "2lonly_pmm: Level-2 class predictive mean matching"
                   ),
    default  = "pmm",
    map      = "method"
  ),
  num_iterations = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_iterations"),
    info    = "Desired number of iterations in the Gibbs sampling",
    default = 5,
    map     = "maxit"
  )
)

args.expect_maximization <- list(
  ts_class  = list(
    check   = NA,
    info    = paste("Attribute that represents time series class",
                    "Should be passed when variables vary smoothly with respect to this argument"),
    map     = "ts"
  ),
  ordinals  = list(
    check   = NA,
    info    = paste("Ordinal attributes in the dataset.",
                    "If not included, continuous imputations could happen"),
    map     = "ords"
  ),
  nominals  = list(
    check   = NA,
    info    = paste("Nominal attributes in the dataset.",
                    "If not included, continuous imputations could happen"),
    map     = "noms"
  ),
  exclude   = list(
    check   = NA,
    info    = paste("Attributes we need to retain but will not be used in the imputation model"),
    map     = "idvars"
  )
)

args.central_imputation <- list()

args.knn_imputation <- list(
  k  = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "k"),
    info    = "Number of nearest neighbours to use",
    default = 10
  ),
  imputation  = list(
    check   = Curry(expect_choice, choices = c("weighAvg", "median"), label = "imputation"),
    info    = c("Imputation method to use:",
                  "weighAvg: weighted average of the values of the neighbours",
                  "median: use median for numeric variables or the most frequent value for factors"),
    map     = "meth",
    default = "weightAvg"
  )
)


doMissingValues.mice <- function(task){
  callArgs   <- eval(parse(text = paste("args.", task$method, sep = "")))
  # Adjust check function to test the imputation method for all the columns,
  # in case more than one is passed to the function
  checkfn <- callArgs$imputation$check
  callArgs$imputation$check <- function(x){
    for(arg in x){
      checkfn(arg)
    }
  }
  callArgs   <- mapArguments(task$args, callArgs)
  callArgs$m <- 1
  method     <- mapMethod(missingValuesPackages, task$method)
  # Do not print anything to screen
  callArgs$printFlag <- FALSE

  # Substitute _ by . in method to use
  callArgs$method <- sub("_", "\\.", callArgs$method)

  dataset    <- task$dataset

  # Method needs dataset and class attr to be filled separately
  callArgs <- c(list(data = dataset), callArgs)
  result   <- do.call(mice::mice, callArgs)
  result   <- mice::complete(result, 1)

  result
}

doMissingValues.amelia <- function(task){
  callArgs     <- eval(parse(text = paste("args.", task$method, sep = "")))
  # Adjust check functions for arguments
  for(argName in c("ts_class", "ordinals", "nominals", "exclude")){
    callArgs[[argName]]$check <- function(x){
      if(!x %in% colnames)
        stop(paste(argName, "should be present as a variable(s) in the dataset"))
    }
  }

  callArgs     <- mapArguments(task$args, callArgs)
  callArgs$m   <- 1
  callArgs$p2s <- 0
  method       <- mapMethod(missingValuesPackages, task$method)

  # Method needs dataset and class attr to be filled separately
  callArgs <- c(list(data = task$dataset), callArgs)
  result <- do.call(method, callArgs)

  result
}


doMissingValues.DMwR <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  method   <- mapMethod(missingValuesPackages, task$method)

  # Method needs dataset and class attr to be filled separately
  callArgs <- c(list(data = task$dataset), callArgs)
  result <- do.call(method, callArgs)

  result
}


#' Missing values imputation wrapper
#'
#' @param dataset we want to impute missing values on
#' @param method selected method of missing values imputation
#' @param ... Further arguments for \code{method}
#'
#' @return The treated dataset (either with noisy instances replaced or erased)
#' @export
#' @examples
#' library("amendr")
#' data(africa, package = "Amelia")
#' data(nhanes, package = "mice")
#'
#' super_nhanes <- impute_missing(nhanes, "gibbs_sampling")
#' # Use a different method for every column
#' impute_methods <- c("pmm", "midastouch", "norm_nob", "norm_boot")
#' super_nhanes <- impute_missing(nhanes, "gibbs_sampling", imputation = impute_methods)
#' super_nhanes <- impute_missing(nhanes, "central_imputation")
#' super_africa <- impute_missing(africa, "knn_imputation")
#' # Execute knn imputation with non default value for k
#' super_africa <- impute_missing(africa, "knn_imputation", k = 5)
#'
impute_missing <- function(dataset, method, ...){
  checkDataset(dataset)

  method <- matchArg(method, missingValuesMethods)

  # Impute missing values
  task <- preprocessingTask(dataset, "missingValues", method, ...)
  dataset <- preprocess(task)

  dataset
}
