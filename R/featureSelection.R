featSelectionPackages <- list(
  "Boruta"  = list(
    pkg = "Boruta"
  ),
  "chi_squared" = list(
    pkg = "FSelector",
    map = "chi.squared"
  ),
  "information_gain" = list(
    pkg = "FSelector",
    map = "information.gain"
  ),
  "gain_ratio" = list(
    pkg = "FSelector",
    map = "gain.ratio"
  ),
  "sym_uncertainty" = list(
    pkg = "FSelector",
    map = "symmetrical.uncertainty"
  ),
  "oneR" = list(
    pkg = "FSelector"
  ),
  "RF_importance" = list(
    pkg = "FSelector",
    map = "random.forest.importance"
  ),
  "best_first_search" = list(
    pkg = "FSelector",
    map = "best.first.search"
  ),
  "forward_search"  = list(
    pkg = "FSelector",
    map = "forward.search"
  ),
  "backward_search" = list(
    pkg = "FSelector",
    map = "backward.search"
  ),
  "hill_climbing" = list(
    pkg = "FSelector",
    map = "hill.climbing.search"
  ),
  "cfs" = list(
    pkg = "FSelector"
  ),
  "consistency" = list(
    pkg = "FSelector"
  )
)

featSelectionMethods <- names(featSelectionPackages)

doFeatSelection <- function(task){
  UseMethod("doFeatSelection")
}

args.Boruta <- list(
  num_iterations = list(
    check   = Curry(qexpect, rules = "X1[100,Inf)", label = "num_iterations"),
    info    = paste("Maximal  number  of  importance  source  runs.",
                    "By default, the algorithm will pick attributes it is sure about",
                    "and possible important ones. Increase this parameter to be more",
                    "accurate on the attributes we are picking. Default value: 100"),
    default = 100,
    map     = "maxRuns"
  )
)

args.chi_squared <- list(
  num_features = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_features"),
    info    = paste("Number of attributes to pick (apart from class_attr, if supplied).",
                    "Should be lower than number of features in the dataset"),
    default = 1
  )
)

args.information_gain <- args.chi_squared
args.gain_ratio <- args.chi_squared
args.sym_uncertainty <- args.chi_squared
args.oneR <- args.chi_squared

args.RF_importance <- list(
  num_features = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_features"),
    info    = paste("Number of attributes to pick. Should be lower than number of",
                    "features in the dataset"),
    default = 1
  ),
  type      = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "type"),
    info    = paste("Type of importance measure (1 = mean decrease in accuracy,",
                    "2 = mean decrease in node impurity)"),
    default = 1,
    map     = "importance.type"
  )
)

args.best_first_search <- list(
  eval_fun = list(
    check  = function(x) { TRUE },
    info   = paste("Function taking as first parameter a character vector of all",
                   "attributes and returning a numeric indicating how important a",
                   "given subset is"),
    map    = "eval.fun"
  )
)

args.forward_search  <- args.best_first_search
args.backward_search <- args.best_first_search
args.hill_climbing   <- args.best_first_search
args.cfs             <- list()
args.consistency     <- list()

doFeatSelection.Boruta <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  classIndex <- task$classIndex
  classAttr  <- task$classAttr
  dataset <- task$dataset
  original_attrs <- names(dataset)

  # Check that class attr has been passed to method and exists in the dataset
  if(is.null(classAttr)){
    stop("Boruta needs non NULL class_attr")
  }
  checkDatasetClass(dataset, classAttr)

  method <- mapMethod(featSelectionPackages, task$method)

  # Method needs dataset and class attr to be filled separately
  callArgs <- c(list(x = dataset[, -classIndex],
                     y = dataset[, classIndex]),
                callArgs)
  attrs <- do.call(method, callArgs)$finalDecision
  attrs <- names(attrs)[attrs %in% c("Confirmed", "Tentative")]
  attrs <- c(attrs, task$classAttr)
  attrs <- original_attrs[original_attrs %in% attrs]
  result <- dataset[, attrs]

  result
}

doFeatSelection.FSelector <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  classAttr <- task$classAttr
  original_attrs <- names(task$dataset)
  dataset <- task$dataset

  method <- mapMethod(featSelectionPackages, task$method)

  # If method is based on information gains, we keep non numerical attributes,
  # and apply method only on continuous variables
  if(task$method %in% c("information_gain", "gain_ratio", "sym_uncertainty")){
    pick_attrs <- which(sapply(dataset, class) == "numeric")
    pick_attrs <- c(original_attrs[pick_attrs], classAttr)
    whichNonNumeric <- which(! original_attrs %in% pick_attrs)
    nonNumeric <- original_attrs[whichNonNumeric]
    # Strip dataset from non continuous attributes, except from class
    if (length(whichNonNumeric) > 0)
      dataset <- dataset[, -whichNonNumeric]
  } else{
    nonNumeric <- c()
  }

  num_features <- callArgs$num_features
  callArgs <- callArgs[names(callArgs) != "num_features"]

  if(task$method %in% c("best_first_search", "backward_search",
                         "forward_search", "hill_climbing")){
    args  <- c(list(attributes = original_attrs), callArgs)
    attrs <- do.call(method, args)
  } else{
    # Check that class attr has been passed to method and exists in the dataset
    if(is.null(classAttr)){
      stop(task$method, " needs non NULL class_attr")
    }
    checkDatasetClass(dataset, classAttr)

    args <- c(list(formula = as.formula(paste(classAttr, "~.")),
                   data    = dataset),
              callArgs)
    if(task$method %in% c("cfs", "consistency")){
      attrs <- do.call(method, args)
    } else{
      weights <- do.call(method, args)
      attrs <- FSelector::cutoff.k(weights, num_features)
    }
  }

  attrs <- c(attrs, classAttr, nonNumeric)
  attrs <- original_attrs[original_attrs %in% attrs]
  result <- task$dataset[, attrs]

  result
}

#' Feature selection wrapper
#'
#' @param dataset we want to do feature selection on
#' @param method selected method of feature selection
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param exclude \code{character}. Vector of attributes to exclude from the
#'   feature selection process
#' @param ... Further arguments for \code{method}
#'
#' @return The treated dataset (either with noisy instances replaced or erased)
#' @export
#' @importFrom stats as.formula
#' @examples
#' library("smartdata")
#' library("rpart")
#' data(ecoli1, package = "imbalance")
#' data(HouseVotes84, package = "mlbench")
#'
#' # Extracted from FSelector::best.first.search documentation
#' evaluator <- function(subset) {
#'   k <- 5
#'   splits <- runif(nrow(iris))
#'   results = sapply(1:k, function(i) {
#'     test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
#'     train.idx <- !test.idx
#'     test <- iris[test.idx, , drop=FALSE]
#'     train <- iris[train.idx, , drop=FALSE]
#'     tree <- rpart(FSelector::as.simple.formula(subset, "Species"), train)
#'     error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
#'     return(1 - error.rate)
#'   })
#'   print(subset)
#'   print(mean(results))
#'   return(mean(results))
#' }
#'
#'
#'
#' super_iris <- feature_selection(iris, "Boruta", class_attr = "Species")
#' super_iris <- feature_selection(iris, "chi_squared",
#'                                 class_attr = "Species", num_features = 3)
#' # Pick 3 attributes from the continuous ones
#' super_ecoli <- feature_selection(ecoli1, "information_gain",
#'                                  class_attr = "Class", num_features = 3)
#' super_ecoli <- feature_selection(ecoli1, "gain_ratio",
#'                                  class_attr = "Class", num_features = 3)
#' super_ecoli <- feature_selection(ecoli1, "sym_uncertainty",
#'                                  class_attr = "Class", num_features = 3)
#' super_votes <- feature_selection(HouseVotes84, "oneR", exclude = c("V1", "V2"),
#'                                  class_attr = "Class", num_features = 3)
#' super_votes <- feature_selection(iris, "RF_importance", class_attr = "Species",
#'                                  num_features = 3, type = 2)
#' \donttest{
#' super_iris  <- feature_selection(iris, "best_first_search", exclude = "Species",
#'                                  eval_fun = evaluator)
#' super_iris  <- feature_selection(iris, "forward_search", exclude = "Species",
#'                                  eval_fun = evaluator)
#' super_iris  <- feature_selection(iris, "backward_search", exclude = "Species",
#'                                  eval_fun = evaluator)
#' }
#' super_iris  <- feature_selection(iris, "cfs", class_attr = "Species")
#' super_iris  <- feature_selection(iris, "consistency", class_attr = "Species")
#'
feature_selection <- function(dataset, method, class_attr = NULL,
                              exclude = NULL, ...){
  classAttr <- class_attr
  orig_dataset <- dataset
  checkDataset(dataset)
  checkInDataset(dataset, exclude)

  method   <- matchArg(method, featSelectionMethods)
  colnames <- names(dataset)

  if(length(exclude) > 0){
    dataset <- dataset[, -which(colnames %in% exclude)]
  }

  # Do feature selection
  task   <- preprocessingTask(dataset, "featureSelection", method, classAttr, ...)
  result <- preprocess(task)

  orig_dataset <- orig_dataset[, colnames %in% c(names(result), exclude)]

  # Join excluded attrs again
  result <- mergeDatasets(orig_dataset, result, exclude)

  result
}
