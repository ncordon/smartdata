discretizationPackages <- list(
  "chi2"      = list(
    pkg       = "discretization",
    map       = "chi2"
  ),
  "chi_merge" = list(
    pkg       = "discretization",
    map       = "chiM"
  ),
  "extended_chi2" = list(
    pkg           = "discretization",
    map           = "extendChi2"
  ),
  "mod_chi2"  = list(
    pkg       = "discretization",
    map       = "modChi2"
  ),
  "CAIM"      = list(
    pkg       = "discretization",
    map       = "disc.Topdown"
  ),
  "CACC"      = list(
    pkg       = "discretization",
    map       = "disc.Topdown"
  ),
  "ameva"     = list(
    pkg       = "discretization",
    map       = "disc.Topdown"
  ),
  "mdlp"      = list(
    pkg       = "discretization",
    map       = "mdlp"
  ),
  "equalfreq" = list(
    pkg       = "infotheo",
    map       = "discretize"
  ),
  "equalwidth" = list(
    pkg        = "infotheo",
    map        = "discretize"
  ),
  "globalequalwidth" = list(
    pkg              = "infotheo",
    map              = "discretize"
  )
)

discretizationMethods <- names(discretizationPackages)

doDiscretization <- function(task){
  UseMethod("doDiscretization")
}

args.chi_merge <- list(
  alpha        = list(
    check      = Curry(qexpect, rules = "N1[0,1]", label = "alpha"),
    info       = "Significance level between 0 and 1",
    default    = 0.5
  )
)

args.extended_chi2 <- list(
  alpha            = list(
    check          = Curry(qexpect, rules = "N1[0,1]", label = "alpha"),
    info           = "Significance level between 0 and 1",
    default        = 0.5,
    map            = "alp"
  )
)

args.mod_chi2 <- args.extended_chi2

args.chi2     <- list(
  alpha       = list(
    check     = Curry(qexpect, rules = "N1[0,1]", label = "alpha"),
    info      = "Significance level between 0 and 1",
    default   = 0.5,
    map       = "alp"
  ),
  delta       = list(
    check     = Curry(qexpect, rules = "N1[0,1]", label = "delta"),
    info      = "Inconsistency level. Algorithm is performed until we exceed this level",
    default   = 0.05,
    map       = "del"
  )
)

args.mdlp  <- list()
args.CAIM  <- list()
args.CACC  <- list()
args.ameva <- list()

args.equalfreq <- list(
  num_bins    = list(
    check     = Curry(qexpect, rules = "X1[2,Inf)", label = "num_bins"),
    info      = paste("Number of bins used, must be lower than rows of dataset",
                      "eg. Half of the number of rows"),
    default   = 2,
    map       = "nbins"
  )
)

args.equalwidth       <- args.equalfreq
args.globalequalwidth <- args.equalfreq

doDiscretization.discretization <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  callArgs <- c(list(task$dataset), callArgs)
  method   <- mapMethod(discretizationPackages, task$method)


  if(task$method == "CAIM"){
    callArgs <- c(callArgs, method = 1)
  } else if(task$method == "CACC"){
    callArgs <- c(callArgs, method = 2)
  } else if(task$method == "ameva"){
    callArgs <- c(callArgs, method = 3)
  }

  result <- do.call(method, callArgs)
  result$Disc.data
}


doDiscretization.infotheo <- function(task) {
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  callArgs <- c(list(X = task$dataset, disc = task$method), callArgs)
  result   <- do.call(infotheo::discretize, callArgs)

  result
}


#' Discretization wrapper
#'
#' @param dataset we want to perform discretization on
#' @param method selected method of discretization
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param ... Further arguments for \code{method}
#'
#' @return The discretized dataset
#' @export
#'
#' @examples
#' library("amendr")
#'
#' super_iris <- discretize(iris, method = "chi_merge", class_attr = "Species")
#' super_iris <- discretize(iris, method = "chi_merge", class_attr = "Species", alpha = 0.7)
#' super_iris <- discretize(iris, method = "chi2", "Species", alpha = 0.7, delta = 0.1)
#' super_iris <- discretize(iris, method = "chi2", class_attr = "Species")
#' super_iris <- discretize(iris, method = "extended_chi2", class_attr = "Species")
#' super_iris <- discretize(iris, method = "ameva", class_attr = "Species")
#' super_iris <- discretize(iris, method = "CAIM", class_attr = "Species")
#' super_iris <- discretize(iris, method = "CACC", class_attr = "Species")
#' super_iris <- discretize(iris, method = "equalwidth", "Species", num_bins = nrow(iris) / 2)
#' super_iris <- discretize(iris, method = "equalfreq", "Species", num_bins = nrow(iris) / 2)
#'
discretize <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)

  method <- matchArg(method, discretizationMethods)
  classIndex <- which(names(dataset) %in% classAttr)
  # Strip dataset from class attribute
  datasetClass <- dataset[, classIndex]
  dataset <- dataset[, -classIndex]

  # Perform discretization
  task <- preprocessingTask(dataset, "discretization", method, classAttr, ...)
  dataset <- preprocess(task)

  # Join class attribute again
  dataset[, classIndex] <- datasetClass
  names(dataset)[classIndex] <- classAttr

  dataset
}
