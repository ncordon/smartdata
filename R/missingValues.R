missingValuesPackages <- list(
  "gibbs_sampling"      = list(
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
  ),
  "rf_imputation"       = list(
    pkg = "missForest",
    map = "missForest"
  ),
  "PCA_imputation"      = list(
    pkg = "missMDA",
    map = "imputePCA"
  ),
  "MCA_imputation"      = list(
    pkg = "missMDA",
    map = "imputeMCA"
  ),
  "FAMD_imputation"     = list(
    pkg = "missMDA",
    map = "imputeFAMD"
  ),
  "hotdeck"  = list(
    pkg = "VIM",
    map = "hotdeck"
  ),
  "iterative_robust"    = list(
    pkg = "VIM",
    map = "irmi"
  ),
  "regression_imputation" = list(
    pkg = "VIM",
    map = "regressionImp"
  ),
  "ATN" = list(
    pkg   = "denoiseR",
    map   = "imputeada"
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
    map     = "ts",
    default = NULL
  ),
  ordinals  = list(
    check   = NA,
    info    = paste("Ordinal attributes in the dataset.",
                    "If not included, continuous imputations could happen"),
    map     = "ords",
    default = NULL
  ),
  nominals  = list(
    check   = NA,
    info    = paste("Nominal attributes in the dataset.",
                    "If not included, continuous imputations could happen"),
    map     = "noms",
    default = NULL
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

args.rf_imputation <- list(
  num_iterations = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_iterations"),
    info    = "Desired number of iterations",
    default = 10,
    map     = "maxiter"
  ),
  num_trees = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_trees"),
    info    = "Number of trees to grow in each forest",
    default = 100,
    map     = "ntree"
  ),
  bootstrap = list(
    check   = Curry(qexpect, rules = "B1", label = "bootstrap"),
    info    = "Use either bootstrap sampling or subsampling",
    default = TRUE,
    map     = "replace"
  )
)

args.PCA_imputation <- list(
  num_dimensions = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_dimensions"),
    info    = "Number of dimensions used to predict missing values",
    default = 2,
    map     = "ncp"
  ),
  imputation  = list(
    check   = Curry(expect_choice, choices = c("Regularized", "EM"), label = "imputation"),
    info    = "Imputation method to use: Regularized or EM",
    map     = "method",
    default = "Regularized"
  ),
  random_init = list(
    check   = Curry(qexpect, rules = "B1", label = "random_init"),
    info    = "Random initializatoin of missing values",
    map     = "seed",
    default = FALSE
  )
)

args.MCA_imputation <- args.PCA_imputation

args.FAMD_imputation <- args.PCA_imputation

args.hotdeck <- list()

args.iterative_robust <- list(
  num_iterations = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_iterations"),
    info    = "Maximum number of iterations",
    default = 100,
    map     = "maxit"
  ),
  initialization = list(
    check   = Curry(expect_choice, choices = c("kNN", "median"), label = "initialization"),
    info    = "Initialization with 'median' or 'kNN'",
    map     = "init.method",
    default = "kNN"
  )
)

args.regression_imputation <- list(
  formula = list(
    check   = function(x) {
      if(!class(x) == "formula")
        stop("formula should be of class 'formula'")
    },
    info    = "Formula to use for imputation",
    map     = "formula"
  )
)

args.ATN <- list(
  lambda = list(
    check   = Curry(qexpect, rules = "N1(0,Inf)", label = "lambda"),
    info    = "Real value to be used in the iterative ATN algorithm",
    default = 0.025
  ),
  gamma = list(
    check   = Curry(qexpect, rules = "N1(0,Inf)", label = "gamma"),
    info    = "Real value to be used in the iterative ATN algorithm",
    default = 2.5
  ),
  sigma = list(
    check   = Curry(qexpect, rules = "N1(0,Inf)", label = "sigma"),
    info    = "Real value that represents standard deviation of the Gaussian noise",
    default = NA
  ),
  tune = list(
    check   = Curry(expect_choice, choices = c("GSURE", "SURE"), label = "tune"),
    info    = "Method to select the two tunning parameters lambda and gamma",
    default = "GSURE",
    map     = "method"
  ),
  num_iterations = list(
    check   = Curry(qexpect, rules = "N1[1,Inf)", label = "num_iterations"),
    info    = "Maximum number of iterations of the algorithm",
    default = 1000,
    map     = "maxiter"
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

  callArgs <- c(list(data = dataset), callArgs)
  result   <- do.call(mice::mice, callArgs)
  result   <- mice::complete(result, 1)

  result
}

doMissingValues.Amelia <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  colnames <- names(task$dataset)

  # Adjust check functions for arguments
  for(argName in c("ts_class", "ordinals", "nominals")){
    callArgs[[argName]]$check <- function(x){
      if(!is.null(x)){
        if(!x %in% colnames){
          stop(paste(argName, "should be present as a variable(s) in the dataset"))
        }
      }
    }
  }

  callArgs     <- mapArguments(task$args, callArgs)
  callArgs$m   <- 1
  callArgs$p2s <- 0
  method       <- mapMethod(missingValuesPackages, task$method)

  callArgs <- c(list(x = task$dataset), callArgs)
  result <- do.call(method, callArgs)
  result <- result$imputations[[1]]

  result
}


doMissingValues.DMwR <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  method   <- mapMethod(missingValuesPackages, task$method)

  callArgs <- c(list(data = task$dataset), callArgs)
  result <- do.call(method, callArgs)

  result
}

doMissingValues.missForest <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  method   <- mapMethod(missingValuesPackages, task$method)

  callArgs <- c(list(xmis = task$dataset), callArgs)
  result <- do.call(method, callArgs)
  result <- result$ximp

  result
}

doMissingValues.missMDA <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  method   <- mapMethod(missingValuesPackages, task$method)

  if(callArgs$seed == FALSE){
    callArgs$seed <- NULL
  } else{
    callArgs$seed <- 1234567
  }

  callArgs <- c(list(task$dataset), callArgs)
  result <- do.call(method, callArgs)
  result <- result$completeObs
  result <- data.frame(result)

  result
}

doMissingValues.VIM <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  method   <- mapMethod(missingValuesPackages, task$method)

  if(task$method %in% c("hotdeck", "regression_imputation")){
    callArgs$imp_var = FALSE
  }

  if(task$method == "regression_imputation"){
    callArgs <- c(list(data = task$dataset), callArgs)
  } else{
    callArgs <- c(list(task$dataset), callArgs)
  }

  result <- do.call(method, callArgs)

  result
}

doMissingValues.denoiseR <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  method <- mapMethod(missingValuesPackages, task$method)

  dataset    <- task$dataset
  colnames   <- names(dataset)
  coltypes   <- colTypes(dataset)
  nonNumeric <- which(! coltypes == "numeric")
  nonNumericAttrs <- names(dataset)[nonNumeric]

  # Strip dataset from non numeric attributes
  dataset <- dataset[, -nonNumeric]

  callArgs <- c(list(X = dataset), callArgs)
  result <- do.call(method, callArgs)$completeObs

  result <- mergeDatasets(task$dataset, result, nonNumericAttrs)

  result
}

#' Missing values imputation wrapper
#'
#' @param dataset we want to impute missing values on
#' @param method selected method of missing values imputation
#' @param exclude \code{character}. Vector of attributes to exclude from the
#'   missing values treatment
#' @param ... Further arguments for \code{method}
#'
#' @return The treated dataset (either with noisy instances replaced or erased)
#' @export
#' @examples
#' library("smartdata")
#' data(africa, package = "Amelia")
#' data(nhanes, package = "mice")
#' data(ozone,  package = "missMDA")
#' data(vnf,    package = "missMDA")
#' data(orange, package = "missMDA")
#' data(sleep,  package = "VIM")
#'
#' super_nhanes <- impute_missing(nhanes, "gibbs_sampling")
#' super_nhanes <- impute_missing(nhanes, "gibbs_sampling", exclude = "chl")
#' # Use a different method for every column
#' impute_methods <- c("pmm", "midastouch", "norm_nob", "norm_boot")
#' super_nhanes <- impute_missing(nhanes, "gibbs_sampling", imputation = impute_methods)
#' super_nhanes <- impute_missing(nhanes, "central_imputation")
#' super_africa <- impute_missing(africa, "knn_imputation")
#' # Execute knn imputation with non default value for k
#' super_africa <- impute_missing(africa, "knn_imputation", k = 5)
#' super_africa <- impute_missing(africa, "expect_maximization", exclude = "country")
#' super_africa <- impute_missing(africa, "rf_imputation", num_iterations = 15,
#'                                num_trees = 200, bootstrap = FALSE)
#' # Examples of calls to 'PCA imputation' with wholly numeric datasets
#' \donttest{
#' super_orange <- impute_missing(orange, "PCA_imputation", num_dimensions = 5,
#'                                imputation = "EM")
#' super_orange <- impute_missing(orange, "PCA_imputation", num_dimensions = 5,
#'                                imputation = "Regularized")
#' }
#' super_orange <- impute_missing(orange, "PCA_imputation", num_dimensions = 5,
#'                                imputation = "Regularized", random_init = TRUE)
#' # Examples of calls to 'MCA imputation' with wholly categorical datasets
#' \donttest{
#' super_vnf    <- impute_missing(vnf, "MCA_imputation", num_dimensions = 5,
#'                                imputation = "EM")
#' super_vnf    <- impute_missing(vnf, "MCA_imputation", num_dimensions = 5,
#'                                imputation = "Regularized")
#' }
#' super_vnf    <- impute_missing(vnf, "MCA_imputation", num_dimensions = 5,
#'                                imputation = "Regularized", random_init = TRUE)
#' # Examples of calls to 'FAMD imputation' with hybrid datasets
#' \donttest{
#' super_ozone  <- impute_missing(ozone, "FAMD_imputation", num_dimensions = 5,
#'                                imputation = "EM", exclude = c("Ne12", "Vx15"))
#' super_ozone  <- impute_missing(ozone, "FAMD_imputation", num_dimensions = 5,
#'                                imputation = "Regularized")
#' }
#' super_ozone  <- impute_missing(ozone, "FAMD_imputation", num_dimensions = 5,
#'                                imputation = "Regularized", random_init = TRUE)
#'
#' # Examples of hotdeck, iterative robust and reggresion imputations
#' super_sleep <- impute_missing(sleep, "hotdeck")
#' super_sleep <- impute_missing(sleep, "iterative_robust", initialization = "median",
#'                               num_iterations = 1000)
#' super_sleep <- impute_missing(sleep, "regression_imputation",
#'                               formula = Dream+NonD~BodyWgt+BrainWgt)
#'
#' # Examples of adaptative shrinkage imputation
#' super_ozone <- impute_missing(ozone, "ATN", sigma = 2.2)
#' super_ozone <- impute_missing(ozone, "ATN", lambda = 0.025, gamma = 2.5)
#' super_ozone <- impute_missing(ozone, "ATN", tune = "SURE")
#'
impute_missing <- function(dataset, method, exclude = NULL, ...){
  orig_dataset <- dataset
  checkDataset(dataset)
  checkInDataset(dataset, exclude)

  method <- matchArg(method, missingValuesMethods)

  colnames <- names(dataset)
  coltypes <- colTypes(dataset)

  if(length(exclude) > 0){
    dataset <- dataset[, -which(colnames %in% exclude)]
  }

  # Impute missing values
  task <- preprocessingTask(dataset, "missingValues", method, ...)
  result <- preprocess(task)

  orig_dataset <- orig_dataset[, colnames %in% c(names(result), exclude)]

  # Join excluded attrs again
  result <- mergeDatasets(orig_dataset, result, exclude)

  result
}
