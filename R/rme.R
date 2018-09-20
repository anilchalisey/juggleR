#' @title Remove Objects Except
#'
#' @description \code{rme} removes all objects in the global environemnt except the ones specified
#'
#' @details Acts as a wrapper around \code{rm} to keep the specified objects.
#'
#' @param except Character vector of the names of the objects to keep
#'
#' @return None.
#'
#' @export
rme <- function(except = NULL) {
  if (!is.character(except)) stop("except must be a character vector")
  rm(list = setdiff(ls(envir = .GlobalEnv), except), envir = .GlobalEnv)
}
