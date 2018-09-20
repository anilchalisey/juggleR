#' Create directories
#'
#' Wrapper around the base function \code{dir.create} that allows creation
#'  of multiple directories simultaneously in a recursive fashion without
#'  warning messages
#'
#' @param dir.names character vector of the directory paths to be created
#'
#' @return
#' returns invisibly a logical vector indicating if the
#'   operation succeeded for each of the files attempted. Using a missing
#'   value for a path name will always be regarded as a failure.
#'   \code{createDir} will will fail if the directory already exists.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' createDir("testdir")
#' }

createDir <- function(dir.names) {
  if (length(dir.names) == 1) {
    dir.create(dir.names, showWarnings = FALSE, recursive = TRUE)
  } else {
    lapply(dir.names, function(x) {
      dir.create(x, showWarnings = FALSE, recursive = TRUE)
    })
  }
}
