noisePackages <- list("AENN"  = "NoiseFiltersR",
                      "BBNR"  = "NoiseFiltersR",
                      "DROP1" = "NoiseFiltersR",
                      "DROP2" = "NoiseFiltersR",
                      "DROP3" = "NoiseFiltersR",
                      "EF"    = "NoiseFiltersR",
                      "ENG"   = "NoiseFiltersR",
                      "IPF"   = "NoiseFiltersR",
                      "HARF"  = "NoiseFiltersR",
                      "INFFC" = "NoiseFiltersR",
                      "GE"    = "NoiseFiltersR",
                      "edgeBoost"  = "NoiseFiltersR",
                      "ensemble"   = "NoiseFiltersR",
                      "edgeWeight" = "NoiseFiltersR",
                      "TomekLinks" = "NoiseFiltersR",
                      "C45ensembles"  = "NoiseFiltersR",
                      "dynamic"       = "NoiseFiltersR",
                      "hybrid"        = "NoiseFiltersR")

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
    check = Curry(expect_choice, choices = c("remove", "repair", "hybrid"), label = "action",
                  info = paste("Strategy to treat noise: remove all or remove only some of them",
                               "depending on a proper measure")),
    default = "remove"
  ),
  relabel_th = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "relabel_th",
                  info = paste("Number lower or equal than k (number of neigbors of KNN)",
                               "of observations of majority class in neighborhood to relabel instance"))
  ),
  agree_level = list(
    check = Curry(qexpect, rules = "N1[0.5,1]", label = "agree_level",
                  info = paste("Number between 0.5 and 1. An instance is",
                               "considered noisy when confidences of belonging to other class",
                               "provided by the random forest, add to a number greater than this parameter")),
    default = 0.7
  ),
  num_trees = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_trees",
                  info = paste("Number of trees for the random forest")),
    default = 500
  )
)

doNoiseClean.NoiseFiltersR <- function(task){
  callArgs <- list()

  classColumn <- task$classIndex
  method <- task$method

  if(method %in% c("C45ensembles", "TomekLinks")){
    args.NoiseFiltersR <- list()
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
  } else if(method %in% c("CVCF", "EF")){
    args.NoiseFiltersR <- args.NoiseFiltersR[c("num_folds", "consensus")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(nfolds = callArgs$num_folds, consensus = callArgs$consensus)
  } else if(method %in% c("AENN", "BBNR", "DROP1", "DROP2", "DROP3")){
    args.NoiseFiltersR <- args.NoiseFiltersR["k"]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
  } else if(method == "dynamic"){
    args.NoiseFiltersR <- args.NoiseFiltersR[c("num_folds", "consensus", "num_ensambles")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(nfolds    = callArgs$num_folds,
                     consensus = callArgs$consensus,
                     m         = callArgs$num_ensambles)
    method <- "dynamicCF"
  } else if(method == "edgeBoost"){
    args.NoiseFiltersR <- args.NoiseFiltersR[c("percent", "threshold", "m")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(percent   = callArgs$percent,
                     threshold = callArgs$threshold,
                     m         = callArgs$num_iterations)
    method <- "edgeBoostFilter"
  } else if(method == "ENG"){
    args.NoiseFiltersR <- args.NoiseFiltersR["graph"]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
  } else if(method == "edgeWeight"){
    args.NoiseFiltersR <- args.NoiseFiltersR[c("action", "threshold")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(noiseAction = callArgs$action,
                     threshol    = callArgs$threshold)
    method <- "EWF"
  } else if(method == "GE"){
    args.NoiseFiltersR <- args.NoiseFiltersR[c("k", "relabel_th")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(k  = callArgs$k,
                     kk = callArgs$relabel_th)
  } else if(method == "HARF"){
    args.NoiseFiltersR <-
      args.NoiseFiltersR[c("num_folds", "agree_level", "num_trees")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(nfolds  = callArgs$num_folds,
                     agreementLevel = callArgs$agree_level,
                     ntrees  = callArgs$ntrees)
  } else if(method == "hybrid"){
    args.NoiseFiltersR <-
      args.NoiseFiltersR[c("action", "consensus")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(consensus   = callArgs$consensus,
                     noiseAction = callArgs$action)
    method <- "hybridRepairFilter"
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
#' clean_noise(iris, "AENN", class_attr = "Species")
#' clean_noise(iris, "GE", class_attr = "Species", k = 5, relabel_th = 2)
#' clean_noise(iris, "HARF", class_attr = "Species", num_folds = 10, agree_level = 0.7, num_trees = 5)
#' clean_noise(iris, "hybrid", class_attr = "Species", consensus = FALSE, action = "repair")
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
