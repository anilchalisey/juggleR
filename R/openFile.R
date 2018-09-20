#' @title Open a file
#'
#' @description Open a file using the default program.
#'
#' @param file Path to file to be opened
#'
#' @return
#' Invisble NULL.  The function opens the file using its default program and
#'  returns immediately.  The window cam be closed via its controls or menus.
#'
#' @export
#'
#' @examples
#' \dontrun{openFile("myplot.pdf")}

openFile <- function(file) {
  path <- normalizePath(file)

  platform <- .Platform$OS.type

  switch(
    platform,
    "windows" = {
      cmd <- paste0('explorer.exe "', path, '"')
      suppressWarnings(system(cmd))
    },
    "unix" = {
      cmd <- paste0('xdg-open "', path, '"')
      suppressWarnings(system(cmd))
    },
    "Darwin" = {
      cmd <- paste0('open "', path, '"')
      suppressWarnings(system(cmd))
    }
  )
}
