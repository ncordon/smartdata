featSelectionPackages <- list(
  "Boruta"  = list(
    pkg   = "Boruta"
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
#'
#' @examples
#' library("amendr")
#'
#' super_iris <- feature_selection(iris, "Boruta", class_attr = "Species")
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
