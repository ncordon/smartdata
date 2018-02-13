checkDataset = getFromNamespace("checkDataset", "imbalance")
checkDatasetClass = getFromNamespace("checkDatasetClass", "imbalance")
checkAllColumnsNumeric = getFromNamespace("checkAllColumnsNumeric", "imbalance")
whichMinorityClass = getFromNamespace("whichMinorityClass", "imbalance")
whichMinority = getFromNamespace("whichMinority", "imbalance")
toNumeric <- getFromNamespace("toNumeric", "imbalance")
#datasetStructure = getFromNamespace("datasetStructure", "imbalance")
#normalizeNewSamples = getFromNamespace("normalizeNewSamples", "imbalance")


checkListArguments <- function(args, checks){
  # Check that no non-existent argument has been passed to the function
  if(any(! names(args) %in% names(checks)))
    stop(paste("Wrong arg for selected method. Valid args are:", names(checks)))

  # Name of arguments that are correctly passed to the function
  wildcard <- names(args)[names(args) %in% names(checks)]

  sapply(wildcard, function(argName){
    goodArg <- checks[[argName]]$check
    if(!goodArg(args[[argName]]))
      stop(paste("Valid values for", argName, "are:",
                 paste(checks[[argName]]$values, collapse = ", ")))
  })
}



argCheck <- function(category, values, min = -Inf, max = Inf){
  if(category == "other"){
    result <- list(check = function(){ TRUE }, values = c())
  } else if(!category %in% c("discrete")){
    mycheck <- function(arg){
    is.numeric(arg) && (arg >= min) && (arg <= max)
  }

    result <- list(check = mycheck, values = paste(category, " in ",
                                                   "]", min, ",", max, "[",
                                                   sep = ""))
  } else{
    mycheck <- function(arg){
      arg %in% values
    }

    result <- list(check = mycheck, values = values)
  }

  result
}

