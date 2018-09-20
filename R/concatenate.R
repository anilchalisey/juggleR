#' @title Concatenate operator
#'
#' @description The \code{\%p\%} operator is a shortcut for \code{\link[base]{paste0}}.
#'
#' @param x a first vector to paste
#' @param y a second vector to paste
#'
#' @examples
#' "abc" %p% "def"
#' "v_" %p% 1:3
#' c("A", "B", "C") %p% 1:3
#'
#' @name concatenate
NULL

#' @export
#' @rdname concatenate
`%p%` <- function(x, y) {
  concatenate(x, y)
}

#' @export
#' @rdname concatenate
concatenate <- function(x, y) {
  paste0(x, y)
}
