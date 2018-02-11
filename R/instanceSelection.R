instSelectionPackages <- list("CNN" = "unbalanced",
                              "ENN" = "unbalanced",
                              "multiedit" = "class",
                              "FRIS" = "RoughSets")


doInstSelection <- function(task){
  UseMethod("doInstSelection")
}


doInstSelection.unbalanced <- function(task){
  method <- task$method
  classAttr <- task$classAttr
  classIndex <- task$classIndex
  dataset <- task$dataset
  possibleArgs <- list(k = "natural")
  checkListArguments(task$args, possibleArgs)

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
    possibleArgs <- list(k = "natural",
                         V = "natural",
                         I = "natural")
    checkListArguments(task$args, possibleArgs)
    callArgs <- append(list(x = dataset[, -classIndex],
                            class = dataset[, classIndex],
                            trace = FALSE), task$args)
    rowsToKeep <- do.call(class::multiedit, callArgs)
    result <- dataset[rowsToKeep, ]
  }

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
    possibleArgs <- list(threshold.tau = "real",
                         alpha = "real",
                         type.aggregation = "real",
                         t.implicator = "real")
    checkListArguments(task$args, possibleArgs)
    callArgs <- list(decision.table = decisionTable, control = task$args)
    rowsToKeep <- (do.call(RoughSets::IS.FRIS.FRST, callArgs))$indx.objects
    result <- dataset[rowsToKeep, ]
  }

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
#' library("imbalance")
#' library("magrittr")
#' data(iris0)
#'
#' super_iris <- iris0 %>% instance_selection(method = "CNN")
instance_selection <- function(dataset,
                               method = c("CNN",
                                          "ENN",
                                          "multiedit",
                                          "FRIS"),
                               class_attr = "Class", ...){
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)
  dataset <- toNumeric(dataset, exclude = classAttr)
  checkAllColumnsNumeric(dataset, exclude = classAttr)
  method <- match.arg(method)

  # Perform instance selection
  task <- preprocessingTask(dataset, "instanceSelection", method, classAttr, ...)
  dataset <- preprocess(task)

  dataset
}
