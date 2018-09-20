#' Write a data.frame to file with sensible defaults
#'
#' @description This is simply a wrapper around write.table with the
#' settings I use most often: quote = FALSE, sep = "\t", row.names = FALSE,
#' and col.names = TRUE
#'
#' @inheritParams utils::write.table
#'
#'
#' @seealso \code{\link{utils::write.table}}
#'
#' @export

write.df <- function(x, file = "", quote = FALSE, sep = "\t",
                     row.names = FALSE, col.names = TRUE) {
  write.table(x = x, file = file, quote = quote, sep = sep,
              row.names = row.names, col.names = col.names)
}
