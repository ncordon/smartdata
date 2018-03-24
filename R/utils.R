checkDataset = getFromNamespace("checkDataset", "imbalance")
checkDatasetClass = getFromNamespace("checkDatasetClass", "imbalance")
checkAllColumnsNumeric = getFromNamespace("checkAllColumnsNumeric", "imbalance")
whichMinorityClass = getFromNamespace("whichMinorityClass", "imbalance")
whichMinority = getFromNamespace("whichMinority", "imbalance")
toNumeric <- getFromNamespace("toNumeric", "imbalance")
colTypes <- getFromNamespace("colTypes", "imbalance")
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

# TODO: improve types and available checks
# Categories used so far in argCheck:
# discrete, real, integer, natural, boolean

argCheck <- function(category, values, min = -Inf, max = Inf, required = FALSE){
  mycheck <- function(arg) { TRUE }
  myvalues <- c()

  # Numerical check
  if(category %in% c("real", "integer", "natural")){
    mycheck <- function(arg){
      is.numeric(arg) && (arg >= min) && (arg <= max)
    }

    myvalues <- paste(category, " in ", "]", min, ",", max, "[", sep = "")
  # Discrete values check (argument has to be among values)
  } else if(category == "discrete"){
    mycheck <- function(arg){
      arg %in% values
    }

    myvalues <- values
  # Boolean category check (argument has to be either True or False)
  } else if(category == "boolean"){
    mycheck <- function(arg){
      class(arg) == "logical"
    }

    myvalues <- c("TRUE", "FALSE")
  }

  # If argument is required, do check that it matches values provided.
  # Otherwise, match it against provided values only if it is present
  result <- list(check = function(arg){
      if(required){
        if(! is.null(arg)){
          mycheck(arg)
        }else{
          FALSE
        }
      } else{
        if(! is.null(arg)){
          mycheck(arg)
        } else{
          TRUE
        }
      }
    }, values = myvalues)

  result
}

