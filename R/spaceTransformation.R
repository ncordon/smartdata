spaceTransformationPackages <- list(
  "lle_knn"  = list(
    pkg = "lle",
    map = "lle"
  ),
  "lle_epsilon" = list(
    pkg = "lle",
    map = "lle"
  ),
  "adaptative_gpca" = list(
    pkg = "adaptiveGPCA",
    map = "adaptivegpca"
  )
)

spaceTransformationMethods <- names(spaceTransformationPackages)

doSpaceTransformation <- function(task){
  UseMethod("doSpaceTransformation")
}

args.lle_knn <- list(
  k         = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "k"),
    info    = "Number of neighbors used in KNN",
    default = 1
  ),
  regularization = list(
    check   = Curry(expect_choice, choices = 1:3, label = "method"),
    info    = "Regularization method: 1, 2 or 3, by default 2",
    default = 2,
    map     = "reg"
  ),
  num_features = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_features"),
    info    = "Desired number of numeric features to return (non numeric will be binded to the numeric)",
    map     = "m"
  )
)

args.lle_epsilon <- list(
  epsilon   = list(
    check   = Curry(qexpect, rules = "N1(0,1)", label = "epsilon"),
    info    = "Epsilon radius",
    default = 0.99,
    map     = "eps"
  ),
  regularization = list(
    check   = Curry(expect_choice, choices = 1:3, label = "method"),
    info    = "Regularization method: 1, 2 or 3, by default 2",
    default = 2,
    map     = "reg"
  ),
  num_features = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_features"),
    info    = "Desired number of numeric features to return",
    map     = "m"
  )
)

args.adaptative_gpca <- list(
  similarity = list(
    check   = function(x){
      if(class(x) != "matrix")
        stop("similarity parameter needs a matrix of similarities of the dataset variabless")
    },
    info    = "A p \times p similarity matrix on the variables defining an inner product on the rows of X",
    map     = "Q"
  ),
  num_features = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_features"),
    info    = "Desired number of numeric features to return",
    map     = "k"
  )
)

doSpaceTransformation.lle <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)

  method <- mapMethod(spaceTransformationPackages, task$method)

  # Adjust use of KNN
  if(task$method == "lle_knn"){
    callArgs <- c(callArgs, list(nnk = TRUE))
  } else{
    callArgs <- c(callArgs, list(nnk = FALSE))
  }

  dataset    <- task$dataset

  callArgs <- c(list(X = dataset), callArgs)
  result <- do.call(method, callArgs)$Y

  result
}

doSpaceTransformation.adaptiveGPCA <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)

  method <- mapMethod(spaceTransformationPackages, task$method)

  dataset    <- task$dataset

  callArgs <- c(list(X = as.matrix(dataset)), callArgs)
  result <- do.call(method, callArgs)$U
  result <- data.frame(result)

  result
}

#' Space transformation wrapper
#'
#' @param dataset we want to do space transformation on
#' @param method selected method for space transformation
#' @param exclude \code{character}. Vector of attributes to exclude from the
#'   space transformation process
#' @param ... Further arguments for \code{method}
#'
#' @return The transformed dataset
#' @export
#' @examples
#' library("smartdata")
#' data(ecoli1, package = "imbalance")
#' data(AntibioticSmall, package = "adaptiveGPCA")
#' antibiotics <- data.frame(AntibioticSmall$X)
#'
#' super_ecoli <- space_transformation(ecoli1, "lle_knn", k = 3, num_features = 2,
#'                                    regularization = 1, exclude = c("Mcg", "Alm1"))
#' \donttest{
#' super_ecoli <- space_transformation(ecoli1, "lle_epsilon", epsilon = 0.99, num_features = 3)
#'
#' super_antibiotics <- space_transformation(antibiotics, "adaptative_gpca",
#'                                          similarity = AntibioticSmall$Q,
#'                                          num_features = 2)
#' }
#'
space_transformation <- function(dataset, method, exclude = NULL, ...){
  orig_dataset <- dataset
  checkDataset(dataset)
  checkInDataset(dataset, exclude)

  method <- matchArg(method, spaceTransformationMethods)

  colnames <- names(dataset)
  coltypes <- colTypes(dataset)
  nonNumeric <- colnames[which(! coltypes %in% c("numeric", "integer"))]
  exclude  <- unique(c(exclude, nonNumeric))

  if(length(exclude) > 0){
    dataset <- dataset[, -which(colnames %in% exclude)]
  }

  # Do feature selection
  task   <- preprocessingTask(dataset, "spaceTransformation", method, NULL, ...)
  result <- preprocess(task)

  result <- cbind(result, orig_dataset[, exclude])

  result
}
