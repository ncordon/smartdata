missingValuesPackages <- list(
  "gibbs-sampling"  = list(
    pkg = "mice",
    map = "mice"
  )
)

missingValuesMethods <- names(missingValuesPackages)

doMissingValues <- function(task){
  UseMethod("doMissingValues")
}

args.mice    <- list(
  imputation = list(
    check    = Curry(expect_choice, choices =
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
    info     = paste("Imputation method:",
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
                        "2lonly_pmm: Level-2 class predictive mean matching",
                     sep = "\n"),
    default  = "",
    map      = "method"
  ),
  num_iterations = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_iterations"),
    info    = "Desired number of iterations in the Gibbs sampling",
    map     = "maxit"
  )
)


doMissingValues.mice <- function(task){
  callArgs   <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs   <- mapArguments(task$args, callArgs)
  callArgs$m <- 1
  method <- mapMethod(spaceTransformationPackages, task$method)

  # Substitute _ by . in method to use
  callArgs$method <- sub("_", "\\.", callArgs$method)

  classAttr  <- task$classAttr
  dataset    <- task$dataset
  colnames   <- names(dataset)
  classIndex <- which(colnames %in% classAttr)
  dataset    <- dataset[, -classIndex]

  # Method needs dataset and class attr to be filled separately
  callArgs <- c(list(X = dataset), callArgs)
  result <- do.call(method, callArgs)
  result <- mice::complete(result, 1)

  result <- cbind(result, task$dataset[, classAttr])

  result
}


#' Missing values imputation wrapper
#'
#' @param dataset we want to impute missing values on
#' @param method selected method of missing values imputation
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param ... Further arguments for \code{method}
#'
#' @return The treated dataset (either with noisy instances replaced or erased)
#' @export
#'
space_transformation <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)

  method <- matchArg(method, spaceTransformationMethods)

  # Do feature selection
  task <- preprocessingTask(dataset, "spaceTransformation", method, classAttr, ...)
  dataset <- preprocess(task)

  dataset
}
