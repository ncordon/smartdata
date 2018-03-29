discretizationPackages <- list("chi2" = "discretization",
                               "chi-merge" = "discretization",
                               "extended-chi2" = "discretization",
                               "mod-chi2" = "discretization",
                               "CAIM" = "discretization",
                               "CACC" = "discretization",
                               "ameva" = "discretization",
                               "mdlp" = "discretization",
                               "equalfreq" = "infotheo",
                               "equalwidth" = "infotheo",
                               "globalequalwidth" = "infotheo")

discretizationMethods <- names(discretizationPackages)

doDiscretization <- function(task){
  UseMethod("doDiscretization")
}

args.discretization <- list(
  alpha = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "alpha",
                  info = "Significance level between 0 and 1"),
    default = 0.5
  ),
  delta = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "delta",
                  info = paste("Inconsistency level.",
                               "Algorithm is performed until we exceed this level")),
    default = 0.05
  )
)

args.infotheo <- list(
  num_bins = list(
    check = Curry(qexpect, rules = "X1[2,Inf)", label = "num_bins",
                  info = paste("Number of bin used, must be lower than rows of dataset",
                               "eg. Half of the number of rows")),
    default = 2
  )
)

doDiscretization.discretization <- function(task){
  callArgs <- list()

  if(task$method == "chi-merge"){
    args.discretization <- args.discretization["alpha"]
    callArgs <- checkListArguments(task$args, args.discretization)
    method <- "chiM"
  } else if(task$method == "chi2"){
    args.discretization <- args.discretization[c("alpha", "delta")]
    task$args <- checkListArguments(task$args, args.discretization)
    callArgs <- list(alp = task$args$alpha, del = task$args$del)
    method <- "chi2"
  } else if(task$method == "extended-chi2"){
    args.discretization <- args.discretization["alpha"]
    task$args <- checkListArguments(task$args, args.discretization)
    callArgs <- list(alp = task$args$alpha)
    method <- "extendChi2"
  } else if(task$method == "mod-chi2"){
    args.discretization <- args.discretization["alpha"]
    task$args <- checkListArguments(task$args, args.discretization)
    callArgs <- list(alp = task$args$alpha)
    method <- "modChi2"
  } else if(task$method == "CAIM"){
    task$args <- checkListArguments(task$args, args.discretization)
    callArgs <- list(method = 1)
    method <- "disc.Topdown"
  } else if(task$method == "CACC"){
    task$args <- checkListArguments(task$args, args.discretization)
    callArgs <- list(method = 2)
    method <- "disc.Topdown"
  } else if(task$method == "ameva"){
    task$args <- checkListArguments(task$args, args.discretization)
    callArgs <- list(method = 3)
    method <- "disc.Topdown"
  } else if(task$method == "mdlp"){
    task$args <- checkListArguments(task$args, args.discretization)
    method <- "mdlp"
  }

  method <- eval(parse(text = paste("discretization::", method, sep = "")))

  callArgs <- c(list(task$dataset), callArgs)
  result <- do.call(method, callArgs)
  result$Disc.data
}


doDiscretization.infotheo <- function(task) {
  args.infotheo <- checkListArguments(task$args, args.infotheo)
  callArgs <- list(X = task$dataset,
                   disc = task$method,
                   nbins = task$args$num_bins)
  result <- do.call(infotheo::discretize, callArgs)

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
#' discretize(iris, method = "chi-merge", class_attr = "Species")
#' discretize(iris, method = "chi-merge", class_attr = "Species", alpha = 0.7)
#' discretize(iris, method = "chi2", class_attr = "Species", alpha = 0.7, delta = 0.1)
#' discretize(iris, method = "chi2", class_attr = "Species")
#' discretize(iris, method = "extended-chi2", class_attr = "Species")
#' discretize(iris, method = "ameva", class_attr = "Species")
#' discretize(iris, method = "CAIM", class_attr = "Species")
#' discretize(iris, method = "CACC", class_attr = "Species")
#' discretize(iris, method = "equalwidth", class_attr = "Species", num_bins = nrow(iris) / 2)
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
