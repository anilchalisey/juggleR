#' Search and replace strings in a file
#'
#' Search for and replace strings in a file
#'
#' \code{findString} searches for a string in a file, and \code{replaceString}
#'   replaces a string in a file.
#'
#' @param string String to search for or replace
#' @param replacement Replacement string
#' @param file The file to search
#'
#' @return
#' \code{findString} returns the lines in the file containing the string, and
#' \code{replaceString} invisibly replaces the string in place
#'
#' @name stringSearch
NULL

#' @export
#' @rdname stringSearch

findString <- function(string = NULL, file = NULL) {
  file.content <- suppressWarnings(readLines(file))
  file.lines <- file.content[grep(string, file.content)]
  return(file.lines)
}

#' @export
#' @rdname stringSearch

replaceString <- function(string = NULL, replacement = NULL, file = NULL) {
  file.content <- suppressWarnings(readLines(file))
  file.new <- gsub(string, replacement, file.content)
  cat(file.new, file = file, sep = "\n")
}
