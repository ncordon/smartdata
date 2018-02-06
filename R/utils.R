checkDataset = getFromNamespace("checkDataset", "imbalance")
checkDatasetClass = getFromNamespace("checkDatasetClass", "imbalance")
checkAllColumnsNumeric = getFromNamespace("checkAllColumnsNumeric", "imbalance")
whichMinorityClass = getFromNamespace("whichMinorityClass", "imbalance")
whichMinority = getFromNamespace("whichMinority", "imbalance")
#datasetStructure = getFromNamespace("datasetStructure", "imbalance")
#normalizeNewSamples = getFromNamespace("normalizeNewSamples", "imbalance")


checkListArguments <- function(args, possibilities){
  # Check that no non-existent argument has been passed to the function
  if(any(! names(args) %in% names(possibilities)))
    stop(paste("Wrong arg for selected method. Valid args are:", names(possibilities)))

  # Name of arguments that are correctly passed to the function
  wildcard <- names(args)[names(args) %in% names(possibilities)]

  sapply(wildcard, function(argName){
    if(!args[[argName]] %in% possibilities[[argName]])
      stop(paste(args[[argName]], "must be one of ", possibilities[argName]))
  })
}
