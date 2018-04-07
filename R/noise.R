noisePackages <- list("AENN"  = "NoiseFiltersR",
                      "BBNR"  = "NoiseFiltersR",
                      "DROP1" = "NoiseFiltersR",
                      "DROP2" = "NoiseFiltersR",
                      "DROP3" = "NoiseFiltersR",
                      "EF"    = "NoiseFiltersR",
                      "ENG"   = "NoiseFiltersR",
                      "HARF"  = "NoiseFiltersR",
                      "GE"    = "NoiseFiltersR",
                      "INFFC" = "NoiseFiltersR",
                      "IPF"   = "NoiseFiltersR",
                      "Mode"  = "NoiseFiltersR",
                      "PF"    = "NoiseFiltersR",
                      "PRISM" = "NoiseFiltersR",
                      "RNN"   = "NoiseFiltersR",
                      "ORBoost"    = "NoiseFiltersR",
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
    default = FALSE
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
                  info = "Number between 0 and 1. Minimum value required to erase an instance"),
    default = 0
  ),
  num_boosting = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_boosting",
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

args.hybrid <- list(
  consensus = list(
    check = Curry(qexpect, rules = "B1", label = "consensus"),
    info = "Use consensus vote. Else, use majority vote",
    default = FALSE
  ),
  action = list(
    check = Curry(expect_choice, choices = c("remove", "repair", "hybrid"), label = "action"),
    info = paste("Strategy to treat noise: remove all or remove only some of them",
                 "depending on a proper measure"),
    default = "remove"
  )
)

args.edgeWeight <- list(
  threshold = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "threshold"),
    info = "Number between 0 and 1. Limit between good and suspicious instances",
    default = 0.25
  ),
  action = list(
    check = Curry(expect_choice, choices = c("remove", "hybrid"), label = "action"),
    info = paste("Strategy to treat noise: remove all or remove only some of them",
                 "depending on a proper measure"),
    default = "remove"
  )
)

args.INFFC <- list(
  consensus = list(
    check = Curry(qexpect, rules = "B1", label = "consensus"),
    info = "Use consensus vote. Else, use majority vote",
    default = FALSE
  ),
  prob_noisy = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "prob_noisy"),
    info = paste("Real between 0 and 1. Minimum proportion of original instances which",
                 "must be tagged as noisy in order to go for another iteration"),
    default = 0.01
  ),
  stop_iterations = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "stop_iterations"),
    info = "The filter stops after stop_iterations iterations with not enough noisy instances removed",
    default = 3
  ),
  k = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "k"),
    info = "Number of nearest neighbors for KNN used for the noisy score",
    default = 5
  ),
  threshold = list(
    check = Curry(qexpect, rules = "N1[-1,1]", label = "threshold"),
    info = "Number between -1 and 1. Noise score value above which an instance is removed",
    default = 0
  )
)

args.IPF <- list(
  prob_noisy = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "prob_noisy"),
    info = paste("Real between 0 and 1. Minimum proportion of original instances which",
                 "must be tagged as noisy in order to go for another iteration"),
    default = 0.01
  ),
  prob_good = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "prob_good"),
    info = "Real between 0 and 1. Proportion of good instances which must be stored in each iteration",
    default = 0.01
  ),
  stop_iterations = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "stop_iterations"),
    info = "The filter stops after stop_iterations iterations with not enough noisy instances removed",
    default = 3
  ),
  num_folds = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds",
                  info = "Number of partitions the dataset is split in each iteration"),
    default = 5
  ),
  consensus = list(
    check = Curry(qexpect, rules = "B1", label = "consensus"),
    info = "Use consensus vote. Else, use majority vote",
    default = FALSE
  )
)

args.Mode <- list(
  action = list(
    check = Curry(expect_choice, choices = c("remove", "repair"), label = "action"),
    info = "Strategy to treat noise: remove instance or repair it",
    default = "remove"
  ),
  type = list(
    check = Curry(expect_choice, choices = c("classical", "iterative", "weighted"), label = "type"),
    info = "Character indicating the scheme to be used. It can be ’classical’, ’iterative’ or ’weighted’",
    default = "classical"
  ),
  epsilon = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "epsilon"),
    info = paste("If ’iterative’ type is used, the loop will be stopped if the proportion of modified",
                 "instances is less or equal than this threshold"),
    default = 0.05
  ),
  max_iterations = list(
    check = Curry(qexpect, rules = "N1[1,Inf)", label = "max_iterations"),
    info = "Maximum number of iterations in 'iterative' type",
    default = 1
  ),
  alpha = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "alpha"),
    info = "Parameter used in the computation of the similarity between two instances",
    default = 1
  ),
  beta = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "beta"),
    info = paste("Influence of the similarity metric in the estimation of",
                 "a new label for an instance"),
  default = 1
  )
)

args.ORBoost <- list(
  num_boosting = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_boosting"),
    info = "Number of iterations for the boosting algorithm",
    default = 20
  ),
  threshold = list(
    check = Curry(qexpect, rules = "X1[3,20]", label = "threshold"),
    info = "Threshold for removing noisy instances",
    default = 11
  ),
  num_adaboost = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "threshold"),
    info = "Number of boosting iterations for AdaBoost when computing the optimal threshold",
    default = 20
  ),
  use_decision_stump = list(
    check = Curry(qexpect, rules = "B1", label = "use_decision_stump"),
    info = paste("If TRUE, uses a decision stump as weak classifier,",
                 "otherwise a naive-Bayes classifier"),
    default = FALSE
  )
)

args.PF <- list(
  prob_noisy = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "prob_noisy"),
    info = paste("Real between 0 and 1. Minimum proportion of original instances which",
                 "must be tagged as noisy in order to go for another iteration"),
    default = 0.01
  ),
  prob_good = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "prob_good"),
    info = "Real between 0 and 1. Proportion of good instances which must be stored in each iteration",
    default = 0.01
  ),
  stop_iterations = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "stop_iterations"),
    info = "The filter stops after stop_iterations iterations with not enough noisy instances removed",
    default = 3
  ),
  num_folds = list(
    check = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds",
                  info = "Number of partitions the dataset is split in each iteration"),
    default = 5
  ),
  consensus = list(
    check = Curry(qexpect, rules = "B1", label = "consensus"),
    info = "Use consensus vote. Else, use majority vote",
    default = FALSE
  ),
  theta = list(
    check = Curry(qexpect, rules = "N1[0,1]", label = "theta"),
    info = "Real between 0 and 1. Proportion of ’good rules’ to be selected",
    theta = 0.7
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
    args.NoiseFiltersR <- args.NoiseFiltersR[c("percent", "threshold", "num_boosting")]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
    callArgs <- list(percent   = callArgs$percent,
                     threshold = callArgs$threshold,
                     m         = callArgs$num_boosting)
    method <- "edgeBoostFilter"
  } else if(method == "ENG"){
    args.NoiseFiltersR <- args.NoiseFiltersR["graph"]
    callArgs <- checkListArguments(task$args, args.NoiseFiltersR)
  } else if(method == "edgeWeight"){
    callArgs <- checkListArguments(task$args, args.edgeWeight)
    callArgs <- list(noiseAction = callArgs$action,
                     threshold   = callArgs$threshold)
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
    callArgs <- checkListArguments(task$args, args.hybrid)
    callArgs <- list(consensus   = callArgs$consensus,
                     noiseAction = callArgs$action)
    method <- "hybridRepairFilter"
  } else if(method == "INFFC"){
    callArgs <- checkListArguments(task$args, args.INFFC)
    callArgs <- list(p = callArgs$prob_noisy,
                     s = callArgs$stop_iterations,
                     k = callArgs$k,
                     consensus = callArgs$consensus,
                     threshold = callArgs$threshold)
  } else if(method == "IPF"){
    callArgs <- checkListArguments(task$args, args.IPF)
    callArgs <- list(p = callArgs$prob_noisy,
                     y = callArgs$prob_good,
                     s = callArgs$stop_iterations,
                     k = callArgs$k,
                     consensus = callArgs$consensus,
                     nfolds = callArgs$num_folds)
  } else if(method == "Mode"){
    callArgs <- checkListArguments(task$args, args.Mode)
    callArgs <- list(type = callArgs$type,
                     noiseAction = callArgs$action,
                     epsilon = callArgs$epsilon,
                     maxIter = callArgs$max_iterations,
                     alpha = callArgs$alpha,
                     beta = callArgs$beta)
    method <- "ModeFilter"
  } else if(method == "ORBoost"){
    callArgs <- checkListArguments(task$args, args.ORBoost)
    callArgs <- list(N = callArgs$num_boosting,
                     d = callArgs$threshold,
                     Naux = callArgs$num_adaboost,
                     useDecisionStump = callArgs$use_decision_stump)
    method <- "ORBoostFilter"
  } else if(method == "PF"){
    callArgs <- checkListArguments(task$args, args.PF)
    callArgs <- list(nfolds = callArgs$num_folds,
                     consensus = callArgs$consensus,
                     p = callArgs$prob_noisy,
                     s = callArgs$stop_iterations,
                     y = callArgs$prob_good,
                     theta = callArgs$theta)
    method <- "ORBoostFilter"
  }

  method <- eval(parse(text = paste("NoiseFiltersR::", method, sep = "")))

  callArgs <- c(list(x = task$dataset, classColumn = classColumn), callArgs)
  result <- do.call(method, callArgs)$cleanData
  rownames(result) <- c()
  result
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
#' clean_noise(iris, "INFFC", class_attr = "Species", consensus = FALSE,
#'             prob_noisy = 0.2, stop_iterations = 3, k = 5, threshold = 0)
#' clean_noise(iris, "IPF", class_attr = "Species", consensus = FALSE, num_folds = 3,
#'             prob_noisy = 0.2, prob_good = 0.5, stop_iterations = 3)
#' clean_noise(iris, "Mode", class_attr = "Species", type = "iterative", action = "repair",
#'             epsilon = 0.05, max_iterations = 200, alpha = 1, beta = 1)
#' clean_noise(iris, "Mode", class_attr = "Species", type = "iterative", action = "repair",
#'             epsilon = 0.05, max_iterations = 200, alpha = 1, beta = 1)
#' clean_noise(iris, "ORBoost", class_attr = "Species", num_boosting = 20,
#'             threshold = 11, num_adaboost = 20)
#' clean_noise(iris, "PF", class_attr = "Species", prob_noisy = 0.01,
#'             stop_iterations = 5, prob_good = 0.5, theta = 0.8)
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
