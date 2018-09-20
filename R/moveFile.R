#' @title Move a file
#'
#' @description Move a file from one location to another and create destination directories
#' as necessary
#'
#' @param from path to file to move
#' @param to path to destination
#'
#' @return Logical vector indicating if operation successful
#'
#' @export

file.move <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from, to = to)
  file.exists(to)
}
