#' @title Convert a vector into a single character string
#'
#' @description Convert a vector into a single character string with the items
#' separated by commas and an "and".
#'
#' @param x A vector
#'
#' @export
#'
#' @return A character string
#'
#' @examples
#' stringify(letters[1:2])
#' stringify(letters[1:4])

stringify <- function(x) {
    n <- length(x)
    if (n == 0) return("")
    if (n == 1) return(paste(x))
    if (n == 2) return(paste(x, collapse = " and "))

    paste(paste(x[-n], collapse = ", "), x[n], sep = ", and ")
}
