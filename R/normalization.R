normalizationPackages <- list(
  "z_score" = list(
    pkg     = "clusterSim",
    map     = "data.Normalization"
  ),
  "pos_standardization" = list(
    pkg                 = "clusterSim",
    map                 = "data.Normalization"
  ),
  "unitization" = list(
    pkg         = "clusterSim",
    map         = "data.Normalization"
  ),
  "pos_unitization"     = list(
    pkg                 = "clusterSim",
    map                 = "data.Normalization"
  ),
  "min_max" = list(
    pkg     = "clusterSim",
    map     = "data.Normalization"
  ),
  "rnorm"   = list(
    pkg     = "clusterSim",
    map     = "data.Normalization"
  ),
  "rpnorm"  = list(
    pkg     = "clusterSim",
    map     = "data.Normalization"
  ),
  "sd_quotient" = list(
    pkg         = "clusterSim",
    map         = "data.Normalization"
  ),
  "mad_quotient" = list(
    pkg          = "clusterSim",
    map          = "data.Normalization"
  ),
  "range_quotient" = list(
    pkg            = "clusterSim",
    map            = "data.Normalization"
  ),
  "max_quotient"   = list(
    pkg            = "clusterSim",
    map            = "data.Normalization"
  ),
  "mean_quotient"  = list(
    pkg            = "clusterSim",
    map            = "data.Normalization"
  ),
  "median_quotient" = list(
    pkg             = "clusterSim",
    map             = "data.Normalization"
  ),
  "sum_quotient"    = list(
    pkg             = "clusterSim",
    map             = "data.Normalization"
  ),
  "ssq_quotient"    = list(
    pkg             = "clusterSim",
    map             = "data.Normalization"
  ),
  "norm" = list(
    pkg  = "clusterSim",
    map  = "data.Normalization"
  ),
  "pnorm" = list(
    pkg  = "clusterSim",
    map  = "data.Normalization"
  ),
  "znorm" = list(
    pkg  = "clusterSim",
    map  = "data.Normalization"
  )
  # "decimal_scaling" = list(
  #   pkg             = "dprep",
  #   map             = "decscale"
  # ),
  # "sigmoidal" = list(
  #   pkg       = "dprep",
  #   map       = "signorm"
  # ),
  # "softmax"   = list(
  #   pkg       = "dprep",
  #   map       = "softmaxnorm"
  # )
)

normalizationMethods <- names(normalizationPackages)

doNormalization <- function(task){
  UseMethod("doNormalization")
}

args.z_score <- list(
  by = list(
    check   = Curry(expect_choice, choices = c("column", "row"), label = "by"),
    info    = "Normalization type: 'column' or 'row'",
    default = "column",
    map     = "normalization"
  )
)

args.pos_standardization <- args.z_score
args.unitization         <- args.z_score
args.pos_unitization     <- args.z_score
args.min_max <- args.z_score
args.rnorm   <- args.z_score
args.rpnorm  <- args.z_score
args.sd_quotient     <- args.z_score
args.mad_quotient    <- args.z_score
args.range_quotient  <- args.z_score
args.max_quotient    <- args.z_score
args.mean_quotient   <- args.z_score
args.median_quotient <- args.z_score
args.sum_quotient    <- args.z_score
args.ssq_quotient    <- args.z_score
args.norm            <- args.z_score
args.pnorm           <- args.z_score
args.znorm           <- args.z_score
args.decimal_scaling <- list()
args.sigmoidal       <- list()
args.softmax         <- list()


doNormalization.clusterSim <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  method   <- mapMethod(normalizationPackages, task$method)

  type <- switch(task$method,
                 "z_score" = "n1",
                 "pos_standardization" = "n2",
                 "unitization" = "n3",
                 "pos_unitization" = "n3a",
                 "min_max" = "n4",
                 "rnorm" = "n5",
                 "rpnorm" = "n5a",
                 "sd_quotient" = "n6",
                 "mad_quotient" = "n6a",
                 "range_quotient" = "n7",
                 "max_quotient" = "n8",
                 "mean_quotient" = "n9",
                 "median_quotient" = "n9a",
                 "sum_quotient" = "n10",
                 "ssq_quotient" = "n11",
                 "norm" = "n12",
                 "pnorm" = "n12a",
                 "znorm" = "n13")

  callArgs <- c(list(x = task$dataset, type = type), callArgs)
  result <- do.call(method, callArgs)
  result
}

doNormalization.dprep <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)
  method   <- mapMethod(normalizationPackages, task$method)

  callArgs <- c(list(task$dataset), callArgs)
  result <- do.call(method, callArgs)
  result
}


#' Normalization wrapper
#'
#' @param dataset we want to perform normalization on
#' @param method selected method of normalization
#' @param exclude \code{character}. Vector of attributes to exclude from the
#'   normalization
#' @param ... Further arguments for \code{method}
#'
#' @return The normalized dataset
#' @export
#'
#' @examples
#' library("smartdata")
#'
#' super_iris <- normalize(iris, method = "min_max", exclude = "Species", by = "column")
#' # Use default parameter by = "row"
#' super_iris <- normalize(iris, method = "min_max", exclude = c("Sepal.Length", "Species"))
#' super_iris <- normalize(iris, method = "min_max", exclude = "Species", by = "row")
#' super_iris <- normalize(iris, method = "z_score", exclude = "Species", by = "row")
#' super_iris <- normalize(iris, method = "sd_quotient", exclude = "Species", by = "row")
#'
normalize <- function(dataset, method, exclude = NULL, ...){
  orig_dataset <- dataset
  checkDataset(dataset)
  checkInDataset(dataset, exclude)

  method   <- matchArg(method, normalizationMethods)
  colnames <- names(dataset)
  coltypes <- colTypes(dataset)
  nonNumeric <- names(dataset)[! coltypes %in% c("numeric", "integer")]

  exclude <- unique(c(exclude, nonNumeric))

  if(length(exclude) > 0){
    dataset <- dataset[, -which(colnames %in% exclude)]
  }

  # Perform normalization
  task    <- preprocessingTask(dataset, "normalization", method, NULL, ...)
  result  <- preprocess(task)

  # Join excluded attrs again
  result <- mergeDatasets(orig_dataset, result, exclude)

  result
}
