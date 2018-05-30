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
    default    = 0.05
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
  callArgs   <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs   <- mapArguments(task$args, callArgs)
  dataset    <- task$dataset
  classAttr  <- task$classAttr
  classIndex <- task$classIndex
  method     <- mapMethod(discretizationPackages, task$method)

  if(is.null(classAttr)){
    stop(task$method, " needs non NULL class_attr")
  }

  checkDatasetClass(dataset, classAttr)

  # Adjust class column to be last one
  classCol <- dataset[, classIndex]
  dataset  <- dataset[, -classIndex]
  dataset[, classAttr] <- classCol

  callArgs   <- c(list(dataset), callArgs)

  if(task$method == "CAIM"){
    callArgs <- c(callArgs, method = 1)
  } else if(task$method == "CACC"){
    callArgs <- c(callArgs, method = 2)
  } else if(task$method == "ameva"){
    callArgs <- c(callArgs, method = 3)
  }

  result <- do.call(method, callArgs)
  result <- result$Disc.data

  result
}


doDiscretization.infotheo <- function(task) {
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  callArgs <- c(list(X = task$dataset, disc = task$method), callArgs)

  if(!is.null(task$classAttr))
    stop(task$method, " must be called without class_attr")

  result   <- do.call(infotheo::discretize, callArgs)

  result
}


#' Discretization wrapper
#'
#' @param dataset we want to perform discretization on
#' @param method selected method of discretization
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param exclude \code{character}. Vector of attributes to exclude from the
#'   discretization
#' @param ... Further arguments for \code{method}
#'
#' @return The discretized dataset
#' @export
#'
#' @examples
#' library("smartdata")
#'
#' super_iris <- discretize(iris, method = "chi_merge",
#'                          class_attr = "Species", exclude = "Sepal.Length")
#' super_iris <- discretize(iris, method = "chi_merge",
#'                          class_attr = "Species", alpha = 0.7)
#' super_iris <- discretize(iris, method = "chi2", "Species",
#'                          alpha = 0.7, delta = 0.1)
#' super_iris <- discretize(iris, method = "chi2", class_attr = "Species")
#' super_iris <- discretize(iris, method = "extended_chi2", class_attr = "Species")
#' super_iris <- discretize(iris, method = "ameva", class_attr = "Species")
#' super_iris <- discretize(iris, method = "CAIM", class_attr = "Species")
#' super_iris <- discretize(iris, method = "CACC", class_attr = "Species")
#' super_iris <- discretize(iris, method = "equalwidth", num_bins = nrow(iris) / 2)
#' super_iris <- discretize(iris, method = "equalfreq", num_bins = nrow(iris) / 2)
#'
discretize <- function(dataset, method, class_attr = NULL,
                       exclude = NULL, ...){
  classAttr <- class_attr
  orig_dataset <- dataset
  checkDataset(dataset)
  checkInDataset(dataset, exclude)

  method   <- matchArg(method, discretizationMethods)
  colnames <- names(dataset)
  coltypes <- colTypes(dataset)
  nonNumeric <- names(dataset)[! coltypes %in% c("numeric", "integer")]
  nonNumeric <- nonNumeric[nonNumeric != classAttr]

  exclude <- unique(c(exclude, nonNumeric))

  if(length(exclude) > 0){
    dataset <- dataset[, -which(colnames %in% exclude)]
  }

  # Perform discretization
  task   <- preprocessingTask(dataset, "discretization", method, classAttr, ...)
  result <- preprocess(task)

  # Join excluded attrs again
  result <- mergeDatasets(orig_dataset, result, exclude)

  result
}
