noisePackages <- list(
  "AENN"  = list(
    pkg   = "NoiseFiltersR"
  ),
  "ENN"  = list(
    pkg   = "NoiseFiltersR"
  ),
  "BBNR"  = list(
    pkg   = "NoiseFiltersR"
  ),
  "DROP1" = list(
    pkg   = "NoiseFiltersR"
  ),
  "DROP2" = list(
    pkg   = "NoiseFiltersR"
  ),
  "DROP3" = list(
    pkg   = "NoiseFiltersR"
  ),
  "EF"    = list(
    pkg   = "NoiseFiltersR"
  ),
  "ENG"   = list(
    pkg   = "NoiseFiltersR"
  ),
  "HARF"  = list(
    pkg   = "NoiseFiltersR"
  ),
  "GE"    = list(
    pkg   = "NoiseFiltersR"
  ),
  "INFFC" = list(
    pkg   = "NoiseFiltersR"
  ),
  "IPF"   = list(
    pkg   = "NoiseFiltersR"
  ),
  "Mode"  = list(
    pkg   = "NoiseFiltersR",
    map   = "ModeFilter"
  ),
  "PF"    = list(
    pkg   = "NoiseFiltersR"
  ),
  "PRISM" = list(
    pkg   = "NoiseFiltersR"
  ),
  "RNN"   = list(
    pkg   = "NoiseFiltersR"
  ),
  "ORBoost" = list(
    pkg   = "NoiseFiltersR",
    map   = "ORBoostFilter"
  ),
  "edgeBoost" = list(
    pkg   = "NoiseFiltersR",
    map   = "edgeBoostFilter"
  ),
  "edgeWeight" = list(
    pkg   = "NoiseFiltersR",
    map   = "EWF"
  ),
  "TomekLinks" = list(
    pkg   = "NoiseFiltersR"
  ),
  "dynamic" = list(
    pkg   = "NoiseFiltersR",
    map   = "dynamicCF"
  ),
  "hybrid"  = list(
    pkg   = "NoiseFiltersR",
    map   = "hybridRepairFilter"
  ),
  "saturation" = list(
    pkg   = "NoiseFiltersR",
    map   = "saturationFilter"
  ),
  "consensusSF" = list(
    pkg   = "NoiseFiltersR"
  ),
  "classificationSF" = list(
    pkg   = "NoiseFiltersR",
    map   = "classifSF"
  ),
  "C45robust" = list(
    pkg   = "NoiseFiltersR",
    map   = "C45robustFilter"
  ),
  "C45voting" = list(
    pkg   = "NoiseFiltersR",
    map   = "C45votingFilter"
  ),
  "C45iteratedVoting" = list(
    pkg   = "NoiseFiltersR",
    map   = "C45iteratedVotingFilter"
  ),
  "CVCF" = list(
    pkg   = "NoiseFiltersR",
    map   = "C45iteratedVotingFilter"
  )
)

noiseMethods <- names(noisePackages)

doNoiseClean <- function(task){
  UseMethod("doNoiseClean")
}

args.AENN <- list(
  k = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "k"),
    info    = "Number of nearest neighbors for KNN",
    default = 5
  )
)

args.BBNR <- args.AENN
args.DROP1 <- args.AENN
args.DROP2 <- args.AENN
args.DROP3 <- args.AENN
args.ENN  <- args.AENN

args.dynamic <- list(
  num_folds = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds"),
    info    = "Number of partitions the dataset is split in",
    default = 5,
    map     = "nfolds"
  ),
  consensus = list(
    check   = Curry(qexpect, rules = "B1", label = "consensus"),
    info    = "Use consensus vote if TRUE. Else, use majority vote",
    default = FALSE
  ),
  num_ensemble = list(
    check   = Curry(qexpect, rules = "X1[1,9]", label = "num_ensemble"),
    info    = "Number from 1 to 9. Number of classifiers to make up the ensemble",
    default = 3,
    map     = "m"
  )
)

args.C45robust <- list(
  num_folds = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds"),
    info    = "Number of partitions the dataset is split in",
    default = 10,
    map     = "nfolds"
  ),
  consensus = list(
    check   = Curry(qexpect, rules = "B1", label = "consensus"),
    info    = "Use consensus vote if TRUE. Else, use majority vote",
    default = FALSE
  )
)

args.C45voting <- args.C45robust
args.C45iteratedVoting <- args.C45robust
args.CVCF <- args.C45robust
args.EF <- args.C45robust

args.edgeBoost <- list(
  percent   = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "percent"),
    info    = "Number between 0 and 1 which sets the percentage of instances to erase",
    default = 0.05
  ),
  threshold = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "threshold"),
    info    = "Number between 0 and 1. Minimum value required to erase an instance",
    default = 0.05
  ),
  num_boosting = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_boosting"),
    info    = "Number of iterations for the boosting algorithm",
    default = 15,
    map     = "m"
  )
)

args.hybrid <- list(
  consensus = list(
    check   = Curry(qexpect, rules = "B1", label = "consensus"),
    info    = "Use consensus vote if TRUE. Else, use majority vote",
    default = FALSE
  ),
  action    = list(
    check   = Curry(expect_choice, choices = c("remove", "repair", "hybrid"), label = "action"),
    info    = "Strategy to treat noisy instances: 'remove', 'repair', 'hybrid'",
    default = "remove"
  )
)

args.edgeWeight <- list(
  threshold = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "threshold"),
    info    = "Number between 0 and 1. Limit between good and suspicious instances",
    default = 0.25
  ),
  action    = list(
    check   = Curry(expect_choice, choices = c("remove", "hybrid"), label = "action"),
    info    = "Strategy to treat noisy instances: 'remove', 'hybrid'",
    default = "remove"
  )
)

args.ENG <- list(
  graph     = list(
    check   = Curry(expect_choice, choices = c("GG", "RNG"), label = "graph"),
    info    = paste("Character indicating the type of graph to be constructed. It can be",
                    "chosen between 'GG' (Gabriel Graph) and 'RNG' (Relative Neighborhood Graph"),
    default = "GG"
  )
)

args.GE <- list(
  k = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "k"),
    info    = "Number of nearest neighbors for K-Nearest-Neighbors",
    default = 5
  ),
  relabel_th = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "relabel_th"),
    info    = paste("Number lower or equal than k (number of neigbors of KNN) of",
                    "observations of majority class in neighborhood to relabel instance"),
    map     = "kk"
  )
)

args.HARF <- list(
  num_folds = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds"),
    info    = "Number of partitions the dataset is split in",
    default = 5,
    map     = "nfolds"
  ),
  agree_level = list(
    check   = Curry(qexpect, rules = "N1[0.5,1]", label = "agree_level"),
    info    = paste("Number between 0.5 and 1. An instance is considered noisy",
                    "when confidences of belonging to other class provided by",
                    "the random forest, add to a number greater than this parameter"),
    default = 0.7,
    map     = "agreementLevel"
  ),
  num_trees = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_trees"),
    info    = "Number of trees for Random Forest",
    default = 500,
    map     = "ntrees"
  )
)

args.INFFC <- list(
  consensus = list(
    check   = Curry(qexpect, rules = "B1", label = "consensus"),
    info    = "Use consensus vote if TRUE. Else, use majority vote",
    default = FALSE
  ),
  prob_noisy = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "prob_noisy"),
    info    = paste("Real between 0 and 1. Minimum proportion of original instances which",
                 "must be tagged as noisy in order to go for another iteration"),
    default = 0.01,
    map     = "p"
  ),
  num_iterations = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_iterations"),
    info    = "The filter stops after num_iterations iterations with not enough noisy instances removed",
    default = 3,
    map     = "s"
  ),
  k = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "k"),
    info    = "Number of nearest neighbors for KNN used for the noisy score",
    default = 5
  ),
  threshold = list(
    check   = Curry(qexpect, rules = "N1[-1,1]", label = "threshold"),
    info    = "Real between -1 and 1. Noise score value above which an instance is removed",
    default = 0
  )
)

args.IPF <- list(
  prob_noisy = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "prob_noisy"),
    info    = paste("Real between 0 and 1. Minimum proportion of original instances which",
                    "must be tagged as noisy in order to go for another iteration"),
    default = 0.01,
    map     = "p"
  ),
  prob_good = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "prob_good"),
    info    = "Real between 0 and 1. Proportion of good instances which must be stored in each iteration",
    default = 0.01,
    map     = "y"
  ),
  num_iterations = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_iterations"),
    info    = "The filter stops after num_iterations iterations with not enough noisy instances removed",
    default = 3,
    map     = "s"
  ),
  num_folds = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds"),
    info    = "Number of partitions the dataset is split in each iteration",
    default = 5,
    map     = "nfolds"
  ),
  consensus = list(
    check   = Curry(qexpect, rules = "B1", label = "consensus"),
    info    = "Use consensus vote if TRUE. Else, use majority vote",
    default = FALSE
  )
)

args.Mode <- list(
  action    = list(
    check   = Curry(expect_choice, choices = c("remove", "repair"), label = "action"),
    info    = "Strategy to treat noise: 'remove' or 'repair' it",
    default = "remove",
    map     = "noiseAction"
  ),
  type      = list(
    check   = Curry(expect_choice, choices = c("classical", "iterative", "weighted"), label = "type"),
    info    = "Character indicating the scheme to be used. It can be 'classical', 'iterative' or 'weighted'",
    default = "classical"
  ),
  epsilon   = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "epsilon"),
    info    = paste("Real between 0 and 1. If 'iterative' type is used, the loop will be stopped if",
                    "the proportion of modified instances is less or equal than this threshold"),
    default = 0.05
  ),
  num_iterations = list(
    check   = Curry(qexpect, rules = "N1[1,Inf)", label = "num_iterations"),
    info    = "Maximum number of iterations in 'iterative' type",
    default = 200,
    map     = "maxIter"
  ),
  alpha     = list(
    check   = Curry(qexpect, rules = "N1[0,Inf)", label = "alpha"),
    info    = paste("Real positive number. Parameter used in the computation of the",
                    "similarity between two instances"),
    default = 1
  ),
  beta      = list(
    check   = Curry(qexpect, rules = "N1[0,Inf)", label = "beta"),
    info    = paste("Real positive number. Influence of the similarity metric in the",
                    "estimation of a new label for an instance"),
    default = 1
  )
)

args.ORBoost <- list(
  num_boosting = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_boosting"),
    info    = "Number of iterations for the boosting algorithm",
    default = 20,
    map     = "N"
  ),
  threshold = list(
    check   = Curry(qexpect, rules = "X1[3,20]", label = "threshold"),
    info    = "Threshold for removing noisy instances",
    default = 11,
    map     = "d"
  ),
  num_adaboost = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_adaboost"),
    info    = paste("Number of boosting iterations for AdaBoost when",
                    "computing the optimal threshold"),
    default = 20,
    map     = "Naux"
  ),
  use_decision_stump = list(
    check   = Curry(qexpect, rules = "B1", label = "use_decision_stump"),
    info    = paste("If TRUE, uses a decision stump as weak classifier,",
                    "otherwise a naive-Bayes classifier"),
    default = FALSE,
    map     = "useDecisionStump"
  )
)

args.PF <- list(
  prob_noisy = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "prob_noisy"),
    info    = paste("Real between 0 and 1. Minimum proportion of original instances which",
                    "must be tagged as noisy in order to go for another iteration"),
    default = 0.01,
    map     = "p"
  ),
  prob_good = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "prob_good"),
    info    = "Real between 0 and 1. Proportion of good instances which must be stored in each iteration",
    default = 0.01,
    map     = "y"
  ),
  num_iterations = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_iterations"),
    info    = "The filter stops after num_iterations iterations with not enough noisy instances removed",
    default = 3,
    map     = "s"
  ),
  num_folds = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds"),
    info    = "Number of partitions the dataset is split in each iteration",
    default = 5,
    map     = "nfolds"
  ),
  consensus = list(
    check   = Curry(qexpect, rules = "B1", label = "consensus"),
    info    = "Use consensus vote if TRUE. Else, use majority vote",
    default = FALSE
  ),
  theta     = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "theta"),
    info    = "Real between 0 and 1. Proportion of 'good rules' to be selected",
    default = 0.7
  )
)

args.saturation <- list(
  noise_threshold = list(
    check   = Curry(qexpect, rules = "N1[0.25,2]", label = "noise_threshold"),
    info    = "Real between 0.25 and 2. Threshold for removing noisy instances in the saturation filter.",
    default = 0.25,
    map     = "NoiseThreshold"
  )
)

args.consensusSF <- list(
  noise_threshold = list(
    check   = Curry(qexpect, rules = "N1[0.25,2]", label = "noise_threshold"),
    info    = "Real between 0.25 and 2. Threshold for removing noisy instances in the saturation filter.",
    default = 0.25,
    map     = "NoiseThreshold"
  ),
  num_folds = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds"),
    info    = "Number of partitions the dataset is split in",
    default = 5,
    map     = "nfolds"
  ),
  consensus_level = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "consensus_level"),
    info    = "Number of 'noisy votes' an instance must get in order to be removed. Suggestion: nfolds - 1",
    map     = "consensusLevel"
  )
)


args.classificationSF <- list(
  noise_threshold = list(
    check   = Curry(qexpect, rules = "N1[0.25,2]", label = "noise_threshold"),
    info    = "Real between 0.25 and 2. Threshold for removing noisy instances in the saturation filter.",
    default = 0.25,
    map     = "NoiseThreshold"
  ),
  num_folds = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_folds"),
    info    = "Number of partitions the dataset is split in",
    default = 5,
    map     = "nfolds"
  )
)


args.PRISM <- list()

args.RNN <- list()

args.TomekLinks <- list()

doNoiseClean.NoiseFiltersR <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  classColumn <- task$classIndex
  callArgs <- mapArguments(task$args, callArgs)

  method <- mapMethod(noisePackages, task$method)

  # Add dataset and classColumn to callArgs
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
#' library("smartdata")
#' data(iris0, package = "imbalance")
#'
#' super_iris <- clean_noise(iris, method = "AENN", class_attr = "Species", k = 3)
#' super_iris <- clean_noise(iris, "GE", class_attr = "Species", k = 5, relabel_th = 2)
#' super_iris <- clean_noise(iris, "HARF", class_attr = "Species",
#'                           num_folds = 10, agree_level = 0.7, num_trees = 5)
#'
#' \donttest{
#' super_iris <- clean_noise(iris0, "TomekLinks")
#' super_iris <- clean_noise(iris, "hybrid", class_attr = "Species",
#'                           consensus = FALSE, action = "repair")
#' super_iris <- clean_noise(iris, "Mode", class_attr = "Species", type = "iterative",
#'                           action = "repair", epsilon = 0.05,
#'                           num_iterations = 200, alpha = 1, beta = 1)
#' super_iris <- clean_noise(iris, "INFFC", class_attr = "Species", consensus = FALSE,
#'                           prob_noisy = 0.2, num_iterations = 3, k = 5, threshold = 0)
#' super_iris <- clean_noise(iris, "IPF", class_attr = "Species", consensus = FALSE,
#'                           num_folds = 3, prob_noisy = 0.2,
#'                           prob_good = 0.5, num_iterations = 3)
#' super_iris <- clean_noise(iris, "ORBoost", class_attr = "Species",
#'                           num_boosting = 20, threshold = 11, num_adaboost = 20)
#' super_iris <- clean_noise(iris, "PF", class_attr = "Species", prob_noisy = 0.01,
#'                           num_iterations = 5, prob_good = 0.5, theta = 0.8)
#' super_iris <- clean_noise(iris, "C45robust", class_attr = "Species", num_folds = 5)
#' }
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
