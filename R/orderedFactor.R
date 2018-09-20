#' @title Preserve factor order
#'
#' @description \code{orderedFactor()} is a convenience wrapper for
#'   \code{factor()} that orders the levels as they appear in the data if the
#'   \code{levels} argument is not specified.  This is valuable for making
#'   plots with ggplot2, where the axis labels are usually arranged in
#'   alphabetical order rather than the order in which they appear.
#'
#' @param x A vector of data, usually taking a small number of distinct values.
#' @param ... Other arguments passed on to \code{base::factor()}.
#'
#' @return A factor.
#'
#' @examples
#' orderedFactor(3:1)
#' orderedFactor(9:12, exclude=11)
#'
#' @export
orderedFactor <- function(x = character(), ...) {
  x <- as.character(x)
  args <- list(...)
  if (!("levels" %in% names(args))) {
    args <- c(list(levels=unique(x)), args)
  }
  args <- c(list(x=x), args)
  do.call(factor, args)
}
