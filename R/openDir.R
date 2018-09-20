#' Open a directory in file browser
#'
#' \code{openDir} opens the specified directory in the
#'   default file browser.  When no directory is specified
#'   the current working directory is opened.
#'
#' @note This has only been tested on Windows
#'
#' @param dir The absolute or relative path to the directory to be opened
#'
#' @return
#' Invisible NULL.  The functions puts up a window and returns immediately.
#'   The window can be closed via its controls or menus.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' openDir()
#' openDir("..")
#' }

openDir <- function(dir = getwd()) {
  suppressWarnings(shell(paste("explorer",  gsub('/', '\\\\', dir))))
}
