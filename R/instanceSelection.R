instSelectionPackages <- list("CNN" = "unbalanced",
                              "ENN" = "unbalanced",
                              "multiedit" = "class",
                              "FRIS" = "RoughSets")

instSelectionMethods <- names(instSelectionPackages)

doInstSelection <- function(task){
  UseMethod("doInstSelection")
}

args.unbalanced <- list(
  k = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "k",
                  info = "Number of nearest neighbors to perform CNN or ENN"),
    default = 1
  )
)

args.class <- list(
  k = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "k",
                  info = "Number of neighbors used in KNN"),
    default = 1
  ),
  num_folds = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds",
                  info = "Number of partitions the train set is split in"),
    default = 3
  ),
  null_passes = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "null_passes",
                  info = "Number of null passes to use in the algorithm"),
    default = 5
  )
)

args.Roughsets <- list(
  threshold = list(
    check = Curry(qexpect, rules = "N1(0,1)", label = "threshold",
                  info = "Threshold between 0 and 1 determining wether an object can be removed or not"),
    default = 0.95
  ),
  alpha = list(
    check = Curry(qexpect, rules = "N1(0,Inf)", label = "alpha",
                  info = "Granularity of fuzzy similarity measure"),
    default = 1
  ),
  implicator_type = list(
    check = Curry(expect_choice, choices = c("kleene_dienes", "lukasiewicz", "zadeh",
                                             "gaines", "godel", "kleene_dienes_lukasiewicz",
                                             "mizumoto", "dubois_prade"),
                  label = "implicator_type",
                  info = "Implicator function type"),
    default = "lukasiewicz"
  )
)

doInstSelection.unbalanced <- function(task){
  method <- task$method
  classAttr <- task$classAttr
  classIndex <- task$classIndex
  dataset <- task$dataset
  task$args <- checkListArguments(task$args, args.unbalanced)

  # CNN and ENN need minority class as 1, and majority one as 0
  minorityClass <- whichMinorityClass(dataset, classAttr)
  minority <- whichMinority(dataset, classAttr)
  old_levels <- levels(dataset[, classIndex])
  new_levels <- old_levels
  new_levels[old_levels == minorityClass] <- 1
  new_levels[old_levels != minorityClass] <- 0
  levels(dataset[, classIndex]) <- as.numeric(new_levels)

  selectedMethod <- eval(parse(text = paste("unbalanced::ub", method, sep = "")))
  callArgs <- append(list(X = dataset[, -classIndex], Y = dataset[, classIndex],
                          verbose = FALSE), task$args)
  result <- do.call(selectedMethod, callArgs)
  result <- cbind(result$X, result$Y)
  # Assign original classAttr name to class column
  names(result)[classIndex] <- classAttr
  # Retrieve original levels for class
  levels(result[, classIndex]) <- old_levels
  # Reset rownames
  rownames(result) <- c()

  result
}


doInstSelection.class <- function(task){
  method <- task$method
  classAttr <- task$classAttr
  classIndex <- task$classIndex
  dataset <- task$dataset
  result <- NULL

  if(method == "multiedit"){
    task$args <- checkListArguments(task$args, args.class)
    callArgs <- list(x = dataset[, -classIndex],
                     class = dataset[, classIndex],
                     trace = FALSE,
                     k = task$args$k,
                     V = task$args$num_folds,
                     I = task$args$null_passes)
    rowsToKeep <- do.call(class::multiedit, callArgs)
    result <- dataset[rowsToKeep, ]
  }

  # Reset rownames
  rownames(result) <- c()
  result
}


doInstSelection.RoughSets <- function(task){
  method <- task$method
  classAttr <- task$classAttr
  classIndex <- task$classIndex
  dataset <- task$dataset
  decisionTable <- RoughSets::SF.asDecisionTable(dataset, decision.attr = classIndex)
  result <- NULL

  if(method == "FRIS"){
    # Improve type checking
    task$args <- checkListArguments(task$args, args.Roughsets)
    callArgs <- list(decision.table = decisionTable,
                     control = list(threshold.tau = task$args$threshold,
                                    alpha = task$args$alpha,
                                    t.implicator = task$args$implicator_type))
    rowsToKeep <- (do.call(RoughSets::IS.FRIS.FRST, callArgs))$indx.objects
    result <- dataset[rowsToKeep, ]
  }

  # Reset rownames
  rownames(result) <- c()
  result
}



#' Instance selection wrapper
#'
#' @param dataset we want to perform an instance selection on
#' @param method selected method of instance selection
#' @param class_attr \code{character}. Indicates the class attribute from
#'   \code{dataset}. Must exist in it
#' @param ... Further arguments for \code{method}
#'
#' @return A filtered dataset with same shape as the input to the function
#' @export
#'
#' @examples
#' library("amendr")
#'
#' super_iris <- instance_selection(iris, method = "CNN", class_attr = "Species")
#' # Use k = 2 instead of default k
#' super_iris <- instance_selection(iris, method = "CNN", class_attr = "Species", k = 2)
#' # Use Edited Nearest Neighbor as method to select observations
#' super_iris <- instance_selection(iris, method = "ENN", class_attr = "Species", k = 3)
#' super_iris <- instance_selection(iris, method = "multiedit", class_attr = "Species",
#'                    k = 3, num_folds = 5, null_passes = 8)
#' # Use default arguments for multiedit
#' super_iris <- instance_selection(iris, method = "multiedit", class_attr = "Species")
#' super_iris <- instance_selection(iris, method = "FRIS", class_attr = "Species")
#' # FRIS method with fuzzy granularity of 2
#' super_iris <- instance_selection(iris, method = "FRIS", class_attr = "Species", alpha = 2)
#' # FRIS method with Dubois Prade implicator
#' super_iris <- instance_selection(iris, method = "FRIS", "Species", implicator_type = "dubois_prade")
#' # FRIS method with lower threshold (that is, less observations are removed)
#' super_iris <- instance_selection(iris, method = "FRIS", class_attr = "Species", threshold = 0.6)
#'
instance_selection <- function(dataset, method, class_attr = "Class", ...){
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)

  method <- matchArg(method, instSelectionMethods)

  # Perform instance selection
  task <- preprocessingTask(dataset, "instanceSelection", method, classAttr, ...)
  dataset <- preprocess(task)

  dataset
}
