instSelectionMethods <- c("CNN", "ENN", "multiedit", "FRIS")


#' Instance selection wrapper
#'
#' @param dataset we want to perform an instance selection on
#' @param method selected method of instance selection
#' @param classAttr \code{character}. Indicates the class attribute from
#'   \code{dataset}. Must exist in it.
#'
#' @return A filtered dataset with same shape as the input to the function
#' @export
#'
#' @examples
#' library("amendr")
#' data(iris0)
#'
#' super_iris <- iris0 %>% instance_selection(method = "CNN")
instance_selection <- function(dataset, method = instSelectionMethods, classAttr = "Class"){
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)
  #originalShape <- datasetStructure(dataset, classAttr)
  checkAllColumnsNumeric(dataset, exclude = classAttr)
  classIndex <- which(names(dataset) == classAttr)


  if(method %in% c("CNN", "ENN")){
    # CNN and ENN need minority class as 1, and majority one as 0
    minorityClass <- whichMinorityClass(dataset, classAttr)
    minority <- whichMinority(dataset, classAttr)
    old_levels <- levels(dataset[, classIndex])
    new_levels <- old_levels
    new_levels[old_levels == minorityClass] <- 1
    new_levels[old_levels != minorityClass] <- 0
    levels(dataset[, classIndex]) <- as.numeric(new_levels)

    selectedMethod <- eval(parse(text = paste("unbalanced::ub", method, sep = "")))
    result <- selectedMethod(dataset[, -classIndex], dataset[, classIndex], verbose = F)
    result <- cbind(result$X, result$Y)
    # Assign original classAttr name to class column
    names(result)[classIndex] <- classAttr
    # Retrieve original levels for class
    levels(result[, classIndex]) <- old_levels
    # Reset rownames
    rownames(result) <- c()
  }
  else if(method == "multiedit"){
    rowsToKeep <- class::multiedit(dataset[, -classIndex],
                                   dataset[, classIndex], trace = F)
    result <- dataset[rowsToKeep, ]
  }
  else if(method == "FRIS"){
    decisionTable <- RoughSets::SF.asDecisionTable(dataset, decision.attr = classIndex)
    rowsToKeep <- RoughSets::IS.FRIS.FRST(decisionTable)$indx.objects
    result <- dataset[rowsToKeep, ]
  }


  result
}
