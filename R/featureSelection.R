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
  )
)

featSelectionMethods <- names(featSelectionPackages)

doFeatSelection <- function(task){
  UseMethod("doFeatSelection")
}

args.Boruta <- list(
  num_iterations = list(
    check   = Curry(qexpect, rules = "X1[100,Inf)", label = "k"),
    info    = paste("Maximal  number  of  importance  source  runs.",
                    "By default, the algorithm will pick attributes it is sure about",
                    "and possible important ones. Increase this parameter to be more",
                    "accurate on the attributes we are picking. Default value: 100"),
    default = 100,
    map     = "maxRuns"
  )
)

args.chi_squared <- list(
  num_attrs = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_attrs"),
    info    = paste("Number of attributes to pick. Should be lower than number of",
                    "features in the dataset"),
    default = 1
  )
)

args.information_gain <- args.chi_squared
args.gain_ratio <- args.chi_squared
args.sym_uncertainty <- args.chi_squared


doFeatSelection.Boruta <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  classColumn <- task$classIndex
  original_attrs <- names(task$dataset)

  method <- mapMethod(featSelectionPackages, task$method)

    # Method needs dataset and class attr to be filled separately
  callArgs <- c(list(x = task$dataset[,-classColumn],
                     y = task$dataset[,classColumn]),
                callArgs)
  attrs <- do.call(method, callArgs)$finalDecision
  attrs <- names(attrs)[attrs %in% c("Confirmed", "Tentative")]
  attrs <- c(attrs, task$classAttr)
  attrs <- original_attrs[original_attrs %in% attrs]
  result <- task$dataset[, attrs]

  result
}

doFeatSelection.FSelector <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  classAttr <- task$classAttr
  original_attrs <- names(task$dataset)

  method <- mapMethod(featSelectionPackages, task$method)

  # If method is based on information gains, we keep non numerical attributes,
  # and apply method only on continuous variables
  if(task$method %in% c("information_gain", "gain_ratio", "sym_uncertainty")){
    pick_attrs <- which(sapply(task$dataset, class) == "numeric")
    pick_attrs <- c(original_attrs[pick_attrs], task$classAttr)
    whichNonNumeric <- which(! original_attrs %in% pick_attrs)
    nonNumeric <- original_attrs[whichNonNumeric]
    # Strip dataset from non continuous attributes, except from class
    dataset <- task$dataset[, -whichNonNumeric]
  } else{
    nonNumeric <- c()
    dataset <- task$dataset
  }

  # Method needs dataset and class attr to be filled separately
  args <- list(formula = as.formula(paste(classAttr, "~.")),
               data    = dataset)
  weights <- do.call(method, args)
  attrs <- FSelector::cutoff.k(weights, callArgs$num_attrs)

  attrs <- c(attrs, task$classAttr, nonNumeric)
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
#' @param ... Further arguments for \code{method}
#'
#' @return The treated dataset (either with noisy instances replaced or erased)
#' @export
#' @importFrom stats as.formula
#' @examples
#' library("amendr")
#' data(ecoli1, package = "imbalance")
#'
#' super_iris <- feature_selection(iris, "Boruta", class_attr = "Species")
#' super_iris <- feature_selection(iris, "chi_squared",
#'                                 class_attr = "Species", num_attrs = 3)
#' # Pick 3 attributes from the continuous ones
#' super_iris <- feature_selection(ecoli1, "information_gain",
#'                                 class_attr = "Class", num_attrs = 3)
#' super_iris <- feature_selection(ecoli1, "gain_ratio",
#'                                 class_attr = "Class", num_attrs = 3)
#' super_iris <- feature_selection(ecoli1, "sym_uncertainty",
#'                                 class_attr = "Class", num_attrs = 3)
#'
feature_selection <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)

  method <- matchArg(method, featSelectionMethods)

  # Do feature selection
  task <- preprocessingTask(dataset, "featureSelection", method, classAttr, ...)
  dataset <- preprocess(task)

  dataset
}
