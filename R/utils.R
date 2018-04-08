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

# TODO: improve types and available checks
# Categories used so far in argCheck:
# discrete, real, integer, boolean
#
# argCheck <- function(category, values, min = -Inf, max = Inf, required = FALSE,
#                      minIncluded = TRUE, maxIncluded = TRUE){
#   mycheck <- function(arg) { TRUE }
#   myvalues <- c()
#
#   # Numerical check
#   if(category %in% c("real", "integer")){
#     mycheck <- function(arg){
#       # If we have indicated that value can be maximum (maximum = TRUE),
#       # match argument against close interval, otherwise check that argument
#       # is strictly lower than maximum
#       minComparison <- ifelse(minIncluded, ">=", ">")
#       maxComparison <- ifelse(maxIncluded, "<=", "<")
#       minCondition <- parse(text = paste(arg, minComparison, min))
#       maxCondition <- parse(text = paste(arg, maxComparison, max))
#       is.numeric(arg) && eval(minCondition) && eval(maxCondition)
#     }
#
#     myvalues <- paste(category, " in ", "]", min, ",", max, "[", sep = "")
#   # Discrete values check (argument has to be among values)
#   } else if(category == "discrete"){
#     mycheck <- function(arg){
#       arg %in% values
#     }
#
#     myvalues <- values
#   # Boolean category check (argument has to be either True or False)
#   } else if(category == "boolean"){
#     mycheck <- function(arg){
#       class(arg) == "logical"
#     }
#
#     myvalues <- c("TRUE", "FALSE")
#   }
#
#   # If argument is required, do check that it matches values provided.
#   # Otherwise, match it against provided values only if it is present
#   result <- list(check = function(arg){
#       if(required){
#         if(! is.null(arg)){
#           mycheck(arg)
#         }else{
#           FALSE
#         }
#       } else{
#         if(! is.null(arg)){
#           mycheck(arg)
#         } else{
#           TRUE
#         }
#       }
#     }, values = myvalues)
#
#   result
# }
#
