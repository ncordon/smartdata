oversamplingPackages <- list(
  "RACOG" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "wRACOG" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "PDFOS" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "RWO" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "ADASYN" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "ANSMOTE" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "SMOTE" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "MWMOTE" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "BLSMOTE" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "DBSMOTE" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "SLMOTE" = list(
    pkg = "imbalance",
    map = "oversample"
  ),
  "RSLSMOTE" = list(
    pkg = "imbalance",
    map = "oversample"
  )
)

oversamplingMethods <- names(oversamplingPackages)

doOversampling <- function(task){
  UseMethod("doOversampling")
}

args.RACOG <- list(
  ratio     = list(
    check   = Curry(qexpect, rules = "N1[0,1]", label = "ratio"),
    info    = "Number between 0 and 1 indicating the desired ratio between minority examples and majority ones",
    default = 0.8
  ),
  filtering = list(
    check   = Curry(qexpect, rules = "B1", label = "filtering"),
    info    = "Logical (TRUE or FALSE) indicating wheter to apply filtering of oversampled instances with neater algorithm",
    default = FALSE
  ),
  wrapper   = list(
    check   = Curry(expect_choice, choices = c("C5.0", "KNN"), label = "wrapper"),
    info    = "A character corresponding to wrapper to apply if selected method is wracog. Possibilities are: 'C5.0' and 'KNN'",
    default = "KNN"
  )
)

args.wRACOG   <- args.RACOG
args.PDFOS    <- args.RACOG
args.RWO      <- args.RACOG
args.ADASYN   <- args.RACOG
args.ANSMOTE  <- args.RACOG
args.SMOTE    <- args.RACOG
args.MWMOTE   <- args.RACOG
args.BLSMOTE  <- args.RACOG
args.DBSMOTE  <- args.RACOG
args.SLMOTE   <- args.RACOG
args.RSLSMOTE <- args.RACOG

doOversampling.imbalance <- function(task){
  callArgs   <- eval(parse(text = paste("args.", task$method, sep = "")))
  callArgs   <- mapArguments(task$args, callArgs)
  dataset    <- task$dataset
  classAttr  <- task$classAttr
  method     <- mapMethod(oversamplingPackages, task$method)

  # Prepare list of arguments for the methods
  callArgs <- c(list(dataset = task$dataset,
                     method = task$method,
                     classAttr = classAttr),
                callArgs)

  result <- do.call(method, callArgs)
  result
}

#' Oversampling wrapper
#'
#' @param dataset we want to perform oversampling on
#' @param method selected method of oversampling
#' @param class_attr \code{character}. Indicates the class attribute or
#'   attributes from \code{dataset}. Must exist in it.
#' @param ... Further arguments for \code{method}
#'
#' @return An oversampled dataset
#' @export
#'
#' @examples
#' library("smartdata")
#' data(iris0, package = "imbalance")
#'
#' super_iris <- oversample(iris0, method = "MWMOTE", class_attr = "Class",
#'                          ratio = 0.8, filtering = TRUE)
#' super_iris <- oversample(iris0, method = "SMOTE", class_attr = "Class", ratio = 0.6)
#' super_iris <- oversample(iris0, method = "PDFOS", class_attr = "Class", ratio = 0.6)
#' super_iris <- oversample(iris0, method = "RWO", class_attr = "Class", ratio = 0.8)
#' super_iris <- oversample(iris0, method = "SLMOTE", class_attr = "Class")
#'
oversample <- function(dataset, method, class_attr = "Class", ...){
  # Convert all not camelCase arguments to camelCase
  classAttr <- class_attr
  checkDataset(dataset)
  checkDatasetClass(dataset, classAttr)

  method <- matchArg(method, oversamplingMethods)

  # Perform discretization
  task <- preprocessingTask(dataset, "oversampling", method, classAttr, ...)
  dataset <- preprocess(task)

  dataset
}
