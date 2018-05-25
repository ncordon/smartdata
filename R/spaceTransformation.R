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
  method    = list(
    check   = Curry(expect_choice, choices = 1:3, label = "method"),
    info    = "Regularisation method: 1, 2 or 3, by default 2",
    default = 2,
    map     = "reg"
  ),
  num_features = list(
    check   = Curry(qexpect, rules = "X1[1,Inf)", label = "num_features"),
    info    = "Desired number of numeric features to return",
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
  method    = list(
    check   = Curry(expect_choice, choices = 1:3, label = "method"),
    info    = "Regularisation method: 1, 2 or 3, by default 2",
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

  classAttr  <- task$classAttr
  dataset    <- task$dataset
  colnames   <- names(dataset)
  dataset    <- dataset[, -which(colnames %in% classAttr)]
  coltypes   <- colTypes(dataset)
  nonNumeric <- which(! coltypes %in% c("numeric", "integer"))
  nonNumericAttrs <- c(names(dataset)[nonNumeric], classAttr)

  if(length(nonNumeric) > 0){
    dataset <- dataset[, -nonNumeric]
  }

    # Method needs dataset and class attr to be filled separately
  callArgs <- c(list(X = dataset), callArgs)
  result <- do.call(method, callArgs)$Y

  result <- cbind(result, task$dataset[, nonNumericAttrs])

  result
}

doSpaceTransformation.adaptative_gpca <- function(task){
  callArgs <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs <- mapArguments(task$args, callArgs)

  method <- mapMethod(spaceTransformationPackages, task$method)

  classAttr  <- task$classAttr
  dataset    <- task$dataset
  colnames   <- names(dataset)
  dataset    <- dataset[, -which(colnames %in% classAttr)]
  coltypes   <- colTypes(dataset)
  nonNumeric <- which(! coltypes %in% c("numeric", "integer"))
  nonNumericAttrs <- c(names(dataset)[nonNumeric], classAttr)

  if(length(nonNumeric) > 0){
    dataset <- dataset[, -nonNumeric]
  }

  # Method needs dataset and class attr to be filled separately
  callArgs <- c(list(X = as.matrix(dataset)), callArgs)
  result <- do.call(method, callArgs)$U
  result <- data.frame(result)
  result <- cbind(result, task$dataset[, nonNumericAttrs])

  result
}

#' Space transformation wrapper
#'
#' @param dataset we want to do space transformation on
#' @param method selected method of space transformation
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param ... Further arguments for \code{method}
#'
#' @return The transformed dataset
#' @export
#' @examples
#' library("amendr")
#' data(ecoli1, package = "imbalance")
#'
#'
#' super_iris <- space_transformation(ecoli1, "lle_knn", k = 3, num_features = 3)
#' \dontrun{
#' super_iris <- space_transformation(ecoli1, "lle_epsilon", epsilon = 0.99, num_features = 3)
#' }
#'
space_transformation <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)

  method <- matchArg(method, spaceTransformationMethods)

  # Do feature selection
  task <- preprocessingTask(dataset, "spaceTransformation", method, classAttr, ...)
  dataset <- preprocess(task)

  dataset
}
