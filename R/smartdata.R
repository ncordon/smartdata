#' smartdata: A package to ease data preprocessing tasks
#'
#' Provides a pipe interface that integrates a collection of the most used data
#' preprocessing libraries, providing oversampling, instance and feature
#' selection, normalization, discretization, space transformation and
#' outliers/noise/missing values treatment
#'
#' @section Method to do oversampling \code{\link{oversample}}
#' @section Method to do instance selection \code{\link{instance_selection}}
#' @section Method to do feature selection \code{\link{feature_selection}}
#' @section Method to do normalization \code{\link{normalize}}
#' @section Method to do discretization \code{\link{discretize}}
#' @section Method to do space transformation \code{\link{space_transformation}}
#' @section Method to treat outliers \code{\link{clean_outliers}}
#' @section Method to treat missing values \code{\link{impute_missing}}
#' @section Method to treat noise \code{\link{clean_noise}}
#'
#' @docType package
#' @name smartdata
NULL

#' @import mice
#' @importFrom functional Curry
#' @importFrom checkmate expect_choice
#' @importFrom checkmate matchArg
#' @importFrom checkmate qexpect
NULL


#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' Prints options for a wrapper or a certain preprocessing method
#'
#' @param preprocess Possible preprocessing: \code{'oversample'},
#'   \code{'clean_noise'}, \code{'instance_selection'},
#'   \code{'feature_selection'}, \code{'normalize'}, \code{'discretize'},
#'   \code{'space_transformation'}, \code{'clean_outliers'},
#'   \code{'impute_missing'}, \code{'clean_noise'}
#' @param method For the preprocessing method
#'
#' @return Prints valid preprocessings for the selected wrapper or options for a
#'   given preprocessing
#' @export
#'
#' @examples
#'
#' which_options("oversample")
#' which_options("clean_noise", method = "edgeWeight")
#' which_options("clean_noise", method = "ENG")
#' which_options("impute_missing", method = "gibbs_sampling")
#'
which_options <- function(preprocess, method = NULL){
  pkgs <- list(
    "clean_outliers"       = "outliersPackages",
    "clean_noise"          = "noisePackages",
    "impute_missing"       = "missingValuesPackages",
    "feature_selection"    = "featSelectionPackages",
    "instance_selection"   = "instSelectionPackages",
    "discretize"           = "discretizationPackages",
    "normalize"            = "normalizationPackages",
    "oversample"           = "oversamplingPackages",
    "space_transformation" = "spaceTransformationPackages"
  )

  if(!preprocess %in% names(pkgs))
    stop(paste("Selected preprocessing doesn't exist. Valid preprocessings are: ",
               paste(names(pkgs), collapse = ", ")))

  pkgs <- eval(parse(text = pkgs[[preprocess]]))

  if(is.null(method)){
    pkgsNames <- sapply(names(pkgs), function(p){
      paste("\'", p, "\'", sep = "")
    })
    cat("Possible methods are:", paste(pkgsNames, collapse = ", "), "\n")
  } else{
    map_method <- pkgs[[method]]$map
    map_method <- paste(pkgs[[method]]$pkg, "::",
                        ifelse(is.null(map_method), method, map_method), sep = "")

    methodArgs <- paste("args.", method, sep = "")

    if(is.null(pkgs[[method]]))
      stop(paste("Wrong method for", preprocess))

    args <- eval(parse(text = methodArgs))
    argNames <- names(args)

    cat("For more information do:", paste("?", map_method, sep = ""), "\n")
    cat("Parameters for", method, "are: \n")

    for(argName in argNames){
      mysep <- "  * "
      innersep <- ": "
      cat(mysep, argName, innersep, sep = "")
      # map <- args[[argName]]$map
      # map <- ifelse(is.null(map), argName, map)
      info   <- args[[argName]]$info
      exdent <- nchar(argName) + nchar(mysep) + nchar(innersep)

      # Indicate default value if there is one
      if(!is.null(args[[argName]]$default)){
        info <- c(info, paste("Default value:", args[[argName]]$default))
      }

      for(n in 1:length(info)){
        if (n == 1){
          indent <- 0
        } else{
          indent <- exdent
        }

        for(line in strwrap(info[n], indent = indent, exdent = exdent)){
          cat(line, "\n")
        }
      }
    }
  }

  invisible()
}

preprocessingTask <- function(dataset, what, method, classAttr = NULL, ...){
  if(is.null(classAttr)){
    classIndex <- NULL
  } else{
    classIndex <- which(names(dataset) %in% classAttr)
  }

  task <- list(dataset = dataset,
               method = method,
               classAttr = classAttr,
               classIndex = classIndex,
               args = list(...))
  class(task) <- what
  task
}

preprocess <- function(task){
  UseMethod("preprocess")
}

preprocess.normalization <- function(task){
  class(task) <- normalizationPackages[[task$method]]$pkg

  doNormalization(task)
}

preprocess.instanceSelection <- function(task){
  class(task) <- instSelectionPackages[[task$method]]$pkg

  doInstSelection(task)
}

preprocess.discretization <- function(task){
  class(task) <- discretizationPackages[[task$method]]$pkg

  doDiscretization(task)
}

preprocess.oversampling <- function(task){
  class(task) <- oversamplingPackages[[task$method]]$pkg

  doOversampling(task)
}

preprocess.outliers <- function(task){
  class(task) <- outliersPackages[[task$method]]$pkg

  doOutliersClean(task)
}

preprocess.noise <- function(task){
  class(task) <- noisePackages[[task$method]]$pkg

  doNoiseClean(task)
}

preprocess.featureSelection <- function(task){
  class(task) <- featSelectionPackages[[task$method]]$pkg

  doFeatSelection(task)
}

preprocess.spaceTransformation <- function(task){
  class(task) <- spaceTransformationPackages[[task$method]]$pkg

  doSpaceTransformation(task)
}

preprocess.missingValues <- function(task){
  class(task) <- missingValuesPackages[[task$method]]$pkg

  doMissingValues(task)
}

