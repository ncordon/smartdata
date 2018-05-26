checkDataset = getFromNamespace("checkDataset", "imbalance")
checkDatasetClass = getFromNamespace("checkDatasetClass", "imbalance")
checkAllColumnsNumeric = getFromNamespace("checkAllColumnsNumeric", "imbalance")
whichMinorityClass = getFromNamespace("whichMinorityClass", "imbalance")
whichMinority = getFromNamespace("whichMinority", "imbalance")
toNumeric <- getFromNamespace("toNumeric", "imbalance")
colTypes <- getFromNamespace("colTypes", "imbalance")
#datasetStructure = getFromNamespace("datasetStructure", "imbalance")
#normalizeNewSamples = getFromNamespace("normalizeNewSamples", "imbalance")


checkInDataset <- function(dataset, vars){
  attrs <- names(dataset)
  missingAttrs <- vars[!vars %in% attrs]

  if(length(missingAttrs) > 0){
    stop("Variables ", paste(missingAttrs, collapse = ", "), " not present in dataset")
  }
}

checkListArguments <- function(args, checks){
  # Check that no non-existent argument has been passed to the function
  if(any(! names(args) %in% names(checks))){
    if(length(names(checks)) == 0){
      validArgs <- "no arguments"
    } else{
      validArgs <- paste(names(checks), collapse = ", ")
    }
    stop(paste("Wrong arg for selected method. Valid args are:", validArgs))
  }

  # Name of arguments that are correctly passed to the function
  # wildcard <- names(args)[names(args) %in% names(checks)]
  sapply(names(checks), function(argName){
    arg <- args[[argName]]
    # If argument is not present and default value for the argument exists,
    # substitute argument by default value
    if(is.null(arg) && !is.null(checks[[argName]]$default))
      arg <- checks[[argName]]$default

    checks[[argName]]$check(arg)

    arg
  }, simplify = FALSE, USE.NAMES = TRUE)
}

mapArguments <- function(args, checks){
  # Check that no non-existent argument has been passed to the function
  if(any(! names(args) %in% names(checks))){
    if(length(names(checks)) == 0){
      validArgs <- "no arguments"
    } else{
      validArgs <- paste(names(checks), collapse = ", ")
    }
    stop(paste("Wrong arg for selected method. Valid args are:", validArgs))
  }

  mappedNames <- sapply(names(checks), function(argName){
    mapped <- checks[[argName]]$map
    result <- argName

    if(!is.null(mapped))
      result <- mapped

    result
  })
  # Name of arguments that are correctly passed to the function
  # wildcard <- names(args)[names(args) %in% names(checks)]
  result <- sapply(names(checks), function(argName){
    arg <- args[[argName]]
    # If argument is not present and default value for the argument exists,
    # substitute argument by default value
    if(is.null(arg) && !is.null(checks[[argName]]$default)){
      arg <- checks[[argName]]$default
    } else{
      checks[[argName]]$check(arg)
    }

    arg
  }, simplify = FALSE)

  names(result) <- mappedNames

  result
}

mapMethod <- function(methodsInfo, method){
  result <- method
  pkg    <- methodsInfo[[method]]$pkg
  mapped <- methodsInfo[[method]]$map

  if(!is.null(mapped))
    result <- mapped

  eval(parse(text = paste(pkg, "::", result , sep = "")))
}

mergeDatasets <- function(orig, new, excluded){
  result <- sapply(names(orig), function(name){
    if(name %in% excluded){
      orig[, name]
    } else{
      new[, name]
    }
  }, USE.NAMES = TRUE, simplify = FALSE)

  data.frame(result)
}
