instSelectionPackages <- list(
  "CNN" = list(
    pkg = "unbalanced",
    map = "ubCNN"
  ),
  "ENN" = list(
    pkg = "unbalanced",
    map = "ubENN"
  ),
  "multiedit" = list(
    pkg       = "class"
  ),
  "FRIS" = list(
    pkg  = "RoughSets",
    map  = "IS.FRIS.FRST"
  )
)

instSelectionMethods <- names(instSelectionPackages)

doInstSelection <- function(task){
  UseMethod("doInstSelection")
}

args.CNN <- list(
  k = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "k"),
    info = "Number of nearest neighbors to perform CNN or ENN",
    default = 1
  )
)

args.ENN <- args.CNN

args.multiedit <- list(
  k = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "k"),
    info = "Number of neighbors used in KNN",
    default = 1
  ),
  num_folds = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds"),
    info = "Number of partitions the train set is split in",
    default = 3,
    map = "V"
  ),
  null_passes = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "null_passes"),
    info = "Number of null passes to use in the algorithm",
    default = 5,
    map = "I"
  )
)

args.FRIS <- list(
  threshold = list(
    check = Curry(qexpect, rules = "N1(0,1)", label = "threshold"),
    info = "Threshold (real between 0 and 1) determining wether an object can be removed or not",
    default = 0.95,
    map = "threshold.tau"
  ),
  alpha = list(
    check = Curry(qexpect, rules = "N1(0,Inf)", label = "alpha"),
    info = "Granularity of fuzzy similarity measure (real greater than 0)",
    default = 1
  ),
  implicator_type = list(
    check = Curry(expect_choice, choices = c("kleene_dienes", "lukasiewicz", "zadeh",
                                             "gaines", "godel", "kleene_dienes_lukasiewicz",
                                             "mizumoto", "dubois_prade"),
                  label = "implicator_type"),
    info = c("Implicator function type:",
             "'kleene_dienes'",
             "'lukasiewicz'",
             "'zadeh'",
             "'gaines'",
             "'godel'",
             "'kleene_dienes_lukasiewicz'",
             "'mizumoto'",
             "'dubois_prade'"),
    default = "lukasiewicz",
    map = "t.implicator"
  )
)

doInstSelection.unbalanced <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  classAttr <- task$classAttr
  classIndex <- task$classIndex
  dataset <- task$dataset

  method <- mapMethod(instSelectionPackages, task$method)

  # CNN and ENN need minority class as 1, and majority one as 0
  minorityClass <- whichMinorityClass(dataset, classAttr)
  minority <- whichMinority(dataset, classAttr)
  old_levels <- levels(dataset[, classIndex])
  new_levels <- old_levels
  new_levels[old_levels == minorityClass] <- 1
  new_levels[old_levels != minorityClass] <- 0
  levels(dataset[, classIndex]) <- as.numeric(new_levels)

  callArgs <- c(list(X = dataset[, -classIndex], Y = dataset[, classIndex], verbose = FALSE),
                callArgs)
  result <- do.call(method, callArgs)
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
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  classAttr <- task$classAttr
  classIndex <- task$classIndex
  dataset <- task$dataset

  method <- mapMethod(instSelectionPackages, task$method)

  callArgs <- c(list(x = dataset[, -classIndex],
                    class = dataset[, classIndex],
                    trace = FALSE),
                callArgs)
  rowsToKeep <- do.call(method, callArgs)
  result <- dataset[rowsToKeep, ]

  # Reset rownames
  rownames(result) <- c()
  result
}


doInstSelection.RoughSets <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  classAttr <- task$classAttr
  classIndex <- task$classIndex
  dataset <- task$dataset

  method <- mapMethod(instSelectionPackages, task$method)
  decisionTable <- RoughSets::SF.asDecisionTable(dataset, decision.attr = classIndex)


  callArgs <- list(decision.table = decisionTable,
                   control = callArgs)
  rowsToKeep <- (do.call(method, callArgs))$indx.objects
  result <- dataset[rowsToKeep, ]

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
#' library("smartdata")
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
