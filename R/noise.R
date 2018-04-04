noisePackages <- list("AENN"  = "NoiseFiltersR",
                      "BBNR"  = "NoiseFiltersR",
                      "DROP1" = "NoiseFiltersR",
                      "DROP2" = "NoiseFiltersR",
                      "DROP3" = "NoiseFiltersR",
                      "EF"    = "NoiseFiltersR",
                      "ENG"   = "NoiseFiltersR",
                      "edgeBoost"  = "NoiseFiltersR",
                      "ensemble"   = "NoiseFiltersR",
                      "edgeWeight" = "NoiseFiltersR",
                      "TomekLinks" = "NoiseFiltersR",
                      "C45ensembles"  = "NoiseFiltersR",
                      "dynamic"       = "NoiseFiltersR")

noiseMethods <- names(noisePackages)

doNoiseClean <- function(task){
  UseMethod("doNoiseClean")
}

args.NoiseFiltersR <- list(
  k = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "k",
                    info = "Number of nearest neighbors for K-Nearest-Neighbors"),
    default = 5
  ),
  num_folds = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds",
                  info = "Number of partitions the dataset is split in"),
    default = 5
  ),
  consensus = list(
    check = Curry(qexpect, rules = "B1", label = "consensus",
                  info = "Use consensus vote. Else, use majority vote"),
    default = TRUE
  ),
  num_ensambles = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_ensambles",
                  info = "Number of classifiers for the ensamble"),
    default = 3
  ),
  percent = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "percent",
                  info = "Number between 0 and 1 which sets the percentage of instances to erase"),
    default = 0.05
  ),
  threshold = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "threshold",
                  info = "Number between 0 and 1 which sets boundary between good and bad/suspicious instances"),
    default = 0.25
  ),
  num_iterations = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_iterations",
                  info = "Number of iterations for the boosting algorithm"),
    default = 15
  ),
  graph = list(
    check = Curry(expect_choice, choices = c("GG", "RNG"), label = "graph"),
    default = "GG"
  ),
  action = list(
    check = Curry(expect_choice, choices = c("remove", "hybrid"), label = "action",
                  info = paste("Strategy to treat noise: remove all or remove only some of them",
                               "depending on a proper measure", sep = "")),
    default = "remove"
  )
)

doNoiseClean.NoiseFiltersR <- function(task){
  callArgs <- list()

  classColumn <- task$classIndex
  method <- task$method

  if(task$method %in% c("C45ensembles", "TomekLinks")){
    args.NoiseFiltersR <- list()
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
  } else if(task$method %in% c("CVCF", "EF")){
    args.NoiseFiltersR <- args.NoiseFiltersR[c("num_folds", "consensus")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(nfolds = callArgs$num_folds, consensus = callArgs$consensus)
  } else if(task$method %in% c("AENN", "BBNR", "DROP1", "DROP2", "DROP3")){
    args.NoiseFiltersR <- args.NoiseFiltersR["k"]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
  } else if(task$method == "dynamic"){
    args.NoiseFiltersR <- args.NoiseFiltersR[c("num_folds", "consensus", "num_ensambles")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(nfolds    = callArgs$num_folds,
                     consensus = callArgs$consensus,
                     m         = callArgs$num_ensambles)
    method <- "dynamicCF"
  } else if(task$method == "edgeBoost"){
    args.NoiseFiltersR <- args.NoiseFiltersR[c("percent", "threshold", "m")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(percent   = callArgs$percent,
                     threshold = callArgs$threshold,
                     m         = callArgs$num_iterations)
    method <- "edgeBoostFilter"
  } else if(task$method == "ENG"){
    args.NoiseFiltersR <- args.NoiseFiltersR["graph"]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
  } else if(task$method == "edgeWeight"){
    args.NoiseFiltersR <- args.NoiseFiltersR[c("action", "threshold")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(noiseAction = callArgs$action,
                     threshol    = callArgs$threshold)
    method <- "EWF"
  }

  method <- eval(parse(text = paste("NoiseFiltersR::", method, sep = "")))

  callArgs <- c(list(x = task$dataset, classColumn = classColumn), callArgs)
  result <- do.call(method, callArgs)
  result$cleanData
}

#' Noise cleaning wrapper
#'
#' @param dataset we want to clean noisy instances on
#' @param method selected method of noise cleaning
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param ... Further arguments for \code{method}
#'
#' @return The treated dataset (either with noisy instances replaced or erased)
#' @export
#'
#' @examples
#' library("amendr")
#'
#' clean_noise(iris, method = "AENN", class_attr = "Species", k = 3)
#' clean_noise(iris, method = "AENN", class_attr = "Species")
#'
clean_noise <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)

  method <- matchArg(method, noiseMethods)

  # Clean noise
  task <- preprocessingTask(dataset, "noise", method, classAttr, ...)
  dataset <- preprocess(task)

  dataset
}
