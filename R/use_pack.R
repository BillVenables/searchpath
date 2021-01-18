#' @importFrom pingr is_online
#' @importFrom utils install.packages setRepositories
#' @export is_online
NULL


#' Add packages to the search path
#'
#' Make multiple packages available on the search path,
#' optionally installing any which are not yet installed,
#' (provided they are available on the default repository).
#'
#' @param ... names of packages required as unquoted character strings
#' @param list package names as a character string vector (or list)
#' @param quietly logical: should the package startup messages be stifled?
#' @param show_data logical: should non-lazy-loaded data sets be exposed?
#'
#' @return A character string vector of the package names, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' modelling <- cq(lme4, splines, mgcv, randomForest, rpart)
#' use_packages(tidyverse, readxl, list = modelling, quietly = TRUE)
#' }
use_packages <- function (..., list = NULL, quietly = TRUE, show_data = TRUE) {
  dots <- match.call(expand.dots = FALSE)$...
  pkgs <- c(lapply(dots, deparse), as.list(list))
  if(is.null(getOption("repos"))) {
    # options(repos = c(CRAN = "https://cloud.r-project.org",
    #                   BioCsoft = "https://bioconductor.org/packages/3.11/bioc"))
    utils::setRepositories(graphics = FALSE, ind = 1:2)
    on.exit(options(repos = NULL))
  }
  req <- if(show_data) requireData else if(quietly) {
    function(...) {
      suppressPackageStartupMessages(require(...))
    }
  } else base::require
  for (pkg in pkgs) {
    if (!req(pkg, character.only = TRUE, quietly = quietly)) {
      if(is_online()) {
        install.packages(pkg)
      } else {
        stop("You appear to be offline, and package ",
             sQuote(pkg), " is not yet installed.")
      }
      req(pkg, character.only = TRUE, quietly = quietly)
    }
  }
  # }
  invisible(pkgs)
}

#' Unquoted String Concatenation
#'
#' A convenience function.  Allows a character string
#' vector to be generated without typing the quotation
#' marks.  Very similar to Hmisc::Cs
#'
#' @param ... Unqouted strings
#'
#' @return A character string vector
#' @export
#'
#' @examples
#' packages <- cq(tidyverse, readxl, lme4, mgcv, splines, randomForest, rpart)
#' packages
cq <- function (...) {  ## same idea as Hmisc::Cs
  sapply(match.call(expand.dots = FALSE)$..., deparse)
}


